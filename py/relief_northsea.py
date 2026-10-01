"""
North Sea oil relief, 2023-2025 (forge3d)
=========================================

Path-traced 3D "oil relief" in the style of Milos Popovic's forge3d population maps:
the sea surface is raised and coloured by how much oil has been detected on it.

What the surface measures
-------------------------
Every slick polygon is rasterised onto a 2 km grid, keeping the share of each cell
it covers. Adding these up over time gives, for every cell, the cumulative slick
area per unit of sea: km2 of slick per 1,000 km2. It is the report's measure
(before dividing by 3 years), mapped continuously instead of per MPA.
The "exact" version shows the 2 km cells as they are; the "density" version smooths
them over ~6 km so concentrations read as a relief (peaks are not slick sizes).

Height and colour both show that value. Marine protected areas are drawn as
outlines; the header counts oil inside them, as in the report.

Inputs   raw/region_slicks.gpkg, raw/region_mpas.gpkg   (prep/extract_region.R)
         raw/ne_10m_land.geojson                         (downloaded once)
Cache    raw/relief_cache.npz                            (delete to rebuild)
Output   figures/preview_relief_<version>.png   with --preview
         figures/oil_relief_<version>.mp4       otherwise (1080 x 1350, LinkedIn 4:5)
         <version> = exact (default) or density, see VERSIONS below

Setup, once, from the project root:
    python3 -m venv .venv
    .venv/bin/pip install forge3d geopandas rasterio matplotlib imageio-ffmpeg
Run from the project root:
    .venv/bin/python py/relief_northsea.py --preview
    .venv/bin/python py/relief_northsea.py
"""

from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
import time
import urllib.request
from datetime import date, timedelta
from pathlib import Path

import numpy as np

# ---- Settings ------------------------------------------------------------------
BBOX = (-4.5, 50.5, 12.5, 61.5)       # lon/lat: North Sea + eastern Channel
CRS = 3035                            # ETRS89 LAEA Europe (equal area)
RES = 2000                            # grid cell (m)
SUPER = 4                             # sub-cells per side when measuring slick cover
# Two versions, chosen with --version:
#   exact    each 2 km cell rises by its own cumulative slick area (no smoothing)
#   density  the same values smoothed over ~SMOOTH_KM: where slicks concentrate
VERSIONS = {
    "exact": dict(
        smooth_km=0,
        subtitle="Each column: oil slick area detected by satellite in a 2 km cell, "
                 "accumulating since Jan 2023.\nOutlines: marine protected areas."),
    "density": dict(
        smooth_km=6,
        subtitle="Density of oil slicks detected by satellite since Jan 2023, smoothed over "
                 "~6 km.\nPeaks show where slicks concentrate, not their size. "
                 "Outlines: marine protected areas."),
}
VERSION = "exact"
SMOOTH_KM = 0                         # set from VERSIONS (0 = raw cells)
SUBTITLE = VERSIONS["exact"]["subtitle"]
RELIEF_MAX = 0.12                     # highest peak, as a share of the map width
HEIGHT_GAMMA = 1.0                    # 1 = height proportional to oil (<1 lifts low values)
START, END = date(2023, 1, 1), date(2025, 12, 31)
RAMP = 10                             # days a new slick takes to reach full height
STEP_DAYS = 3                         # days between frames (1 = daily, slower)
FPS = 24
HOLD_S = 4                            # seconds holding the last frame
W, H = 1080, 1350                     # output size (px)
FRAMES_PER_RENDER = 24                # path-tracing passes per frame (more = less noise)
TILE = 540                            # render tile size (px): lower it if you get a memory error

VIEW_ELEV = 38                        # camera height above the horizon (degrees)
ORBIT_FROM, ORBIT_TO = -8, 8          # slow camera drift over the three years
SUN_AZ, SUN_ELEV = 300, 25            # low sun from the west-northwest
SUN_INT, ENV_INT = 3.2, 0.55
ZOOM = 0.60                           # orthographic half-height, in map widths
LOOK_SHIFT = -0.25                    # >0 moves the map up the frame, <0 down

# Colours: the ramp starts at the sea colour, so clean water stays flat and dark
SEA, LAND, MPA_LINE = "#052832", "#2c3d43", "#5fb3b3"
RAMP_STOPS = [(0.00, "#052832"), (0.06, "#4a1a3c"), (0.30, "#a3201c"),
              (0.55, "#e04a24"), (0.80, "#f6a06a"), (1.00, "#fff1e0")]
INK, INK2, MUTED, ACCENT = "#ffffff", "#9cb7c9", "#6f8a99", "#ff6633"

ROOT = Path.cwd()
RAW, FIG = ROOT / "raw", ROOT / "figures"
FRAMES = ROOT / "frames" / "relief"
CACHE = RAW / "relief_cache.npz"
LAND_URL = ("https://raw.githubusercontent.com/nvkelso/natural-earth-vector/"
            "master/geojson/ne_10m_land.geojson")


# ---- Data preparation (cached) --------------------------------------------------
def download(url: str, dest: Path) -> None:
    """Download with certifi's certificates (python.org builds on macOS ship none)."""
    import ssl
    try:
        import certifi
        ctx = ssl.create_default_context(cafile=certifi.where())
    except ImportError:
        ctx = ssl.create_default_context()
    with urllib.request.urlopen(url, context=ctx) as r, open(dest, "wb") as f:
        shutil.copyfileobj(r, f)


def build_cache() -> None:
    import geopandas as gpd
    from rasterio import features
    from rasterio.transform import from_origin
    from shapely.geometry import box as shp_box

    print("Preparing data (first run only) ...")
    frame = gpd.GeoSeries([shp_box(*BBOX)], crs=4326).to_crs(CRS)
    xmin, ymin, xmax, ymax = frame.total_bounds
    xmin, ymin = np.floor(xmin / RES) * RES, np.floor(ymin / RES) * RES
    ncol = int(np.ceil((xmax - xmin) / RES))
    nrow = int(np.ceil((ymax - ymin) / RES))
    xmax, ymax = xmin + ncol * RES, ymin + nrow * RES
    rect = shp_box(xmin, ymin, xmax, ymax)
    transform = from_origin(xmin, ymax, RES, RES)     # row 0 = north

    land_file = RAW / "ne_10m_land.geojson"
    if not land_file.exists():
        print("  downloading Natural Earth 10m land ...")
        download(LAND_URL, land_file)
    land = gpd.read_file(land_file, bbox=(BBOX[0] - 3, BBOX[1] - 3, BBOX[2] + 3, BBOX[3] + 3))
    land = land.to_crs(CRS).clip(rect.buffer(50_000))
    land_union = land.union_all()
    land_mask = features.rasterize([(land_union, 1)], out_shape=(nrow, ncol),
                                   transform=transform, fill=0, dtype="uint8")

    # MPAs: sea part only, one feature per WDPAID
    mpa = gpd.read_file(RAW / "region_mpas.gpkg").to_crs(CRS)
    mpa["geometry"] = mpa.geometry.make_valid()
    mpa = mpa.clip(rect)
    mpa["geometry"] = mpa.geometry.difference(land_union)
    mpa = mpa[~mpa.geometry.is_empty]
    mpa = mpa.dissolve(by="WDPAID", aggfunc="first").reset_index()
    outline = features.rasterize([(g, 1) for g in mpa.geometry.boundary if not g.is_empty],
                                 out_shape=(nrow, ncol), transform=transform, fill=0,
                                 dtype="uint8", all_touched=True)

    # Slicks
    sl = gpd.read_file(RAW / "region_slicks.gpkg").to_crs(CRS)
    sl["geometry"] = sl.geometry.make_valid()
    sl["day"] = sl["slick_timestamp"].str.slice(0, 10)
    sl = sl[(sl["day"] >= START.isoformat()) & (sl["day"] <= END.isoformat())]
    sl = sl[sl.intersects(rect)].reset_index(drop=True)
    sl["dnum"] = [(date.fromisoformat(d) - START).days for d in sl["day"]]

    # Share of each 2 km cell covered by each slick (rasterised on a finer grid)
    sub = RES / SUPER
    cov_sid, cov_idx, cov_frac = [], [], []
    for i, g in enumerate(sl.geometry):
        bx0, by0, bx1, by1 = g.bounds
        c0 = max(int((bx0 - xmin) // RES), 0); c1 = min(int((bx1 - xmin) // RES) + 1, ncol)
        r0 = max(int((ymax - by1) // RES), 0); r1 = min(int((ymax - by0) // RES) + 1, nrow)
        if c1 <= c0 or r1 <= r0:
            continue
        hr, wc = r1 - r0, c1 - c0
        fine = features.rasterize(
            [(g, 1)], out_shape=(hr * SUPER, wc * SUPER), fill=0, dtype="uint8",
            transform=from_origin(xmin + c0 * RES, ymax - r0 * RES, sub, sub),
            all_touched=False)
        frac = fine.reshape(hr, SUPER, wc, SUPER).mean(axis=(1, 3))
        if frac.sum() == 0:             # slick thinner than a sub-cell: put it at its centroid
            p = g.representative_point()
            rr = min(int((ymax - p.y) // RES), nrow - 1) - r0
            cc = min(int((p.x - xmin) // RES), ncol - 1) - c0
            frac = np.zeros((hr, wc)); frac[max(rr, 0), max(cc, 0)] = 1
        # rescale so the cells add up to the slick's real area
        frac *= (g.area / RES ** 2) / frac.sum()
        rr, cc = np.nonzero(frac)
        cov_sid.append(np.full(rr.size, i))
        cov_idx.append((rr + r0) * ncol + (cc + c0))
        cov_frac.append(frac[rr, cc])

    # Oil inside each MPA: slick polygons clipped to the MPA (as in the report)
    pairs = gpd.overlay(sl[["dnum", "geometry"]], mpa[["geometry"]].assign(mid=mpa.index),
                        how="intersection", keep_geom_type=True)

    np.savez_compressed(
        CACHE, nrow=nrow, ncol=ncol, land=land_mask, outline=outline, n_mpa=len(mpa),
        sl_dnum=sl["dnum"].to_numpy(),
        cov_sid=np.concatenate(cov_sid), cov_idx=np.concatenate(cov_idx),
        cov_frac=np.concatenate(cov_frac).astype(np.float32),
        pr_mid=pairs["mid"].to_numpy(), pr_dnum=pairs["dnum"].to_numpy(),
        pr_km2=(pairs.geometry.area / 1e6).to_numpy(),
    )
    print(f"  {len(sl)} slicks, {len(mpa)} MPAs, grid {nrow} x {ncol} cells of {RES / 1000:g} km")


# ---- Helpers ------------------------------------------------------------------------
def lin(hex_col: str) -> np.ndarray:
    """sRGB hex -> linear RGB (the path tracer works in linear light)."""
    c = np.array([int(hex_col[i:i + 2], 16) for i in (1, 3, 5)], dtype=np.float32) / 255
    return np.where(c <= 0.04045, c / 12.92, ((c + 0.055) / 1.055) ** 2.4).astype(np.float32)


def ramp(dnum_today: float, dnum: np.ndarray) -> np.ndarray:
    return np.clip((dnum_today - dnum) / RAMP, 0, 1)


def box_blur(a: np.ndarray, r: int) -> np.ndarray:
    """Mean filter of radius r (in cells) along both axes, via cumulative sums."""
    if r < 1:
        return a
    for axis in (0, 1):
        p = np.pad(a, [(r + 1, r) if ax == axis else (0, 0) for ax in (0, 1)], mode="edge")
        c = np.cumsum(p, axis=axis)
        hi = np.take(c, np.arange(2 * r + 1, c.shape[axis]), axis=axis)
        lo = np.take(c, np.arange(0, c.shape[axis] - 2 * r - 1), axis=axis)
        a = (hi - lo) / (2 * r + 1)
    return a


def smooth(a: np.ndarray) -> np.ndarray:
    """Near-Gaussian smoothing (three box passes), radius SMOOTH_KM."""
    r = int(round(SMOOTH_KM * 1000 / RES / 1.7))
    for _ in range(3):
        a = box_blur(a, r)
    return a


def colour_ramp(t: np.ndarray) -> np.ndarray:
    """t in [0, 1] -> linear RGB, interpolating RAMP_STOPS in sRGB."""
    pos = np.array([p for p, _ in RAMP_STOPS])
    cols = np.array([[int(h[i:i + 2], 16) / 255 for i in (1, 3, 5)] for _, h in RAMP_STOPS])
    srgb = np.stack([np.interp(t, pos, cols[:, k]) for k in range(3)], axis=-1)
    return np.where(srgb <= 0.04045, srgb / 12.92, ((srgb + 0.055) / 1.055) ** 2.4)


class Scene:
    def __init__(self, c):
        self.c = c
        self.nrow, self.ncol = int(c["nrow"]), int(c["ncol"])
        self.sea = c["land"] == 0
        # Fixed scale: the end state, so heights and colours are comparable across frames
        final = self.density(10 ** 6)
        self.vmax = float(np.quantile(final[final > 0], 0.999))
        self.k = None

    def density(self, today: int) -> np.ndarray:
        """Cumulative km2 of slick per 1,000 km2 of sea, smoothed."""
        c = self.c
        w = ramp(today, c["sl_dnum"])[c["cov_sid"]] * c["cov_frac"]
        v = np.bincount(c["cov_idx"], weights=w, minlength=self.nrow * self.ncol)
        v = v.reshape(self.nrow, self.ncol) * 1000
        v = smooth(v)
        return np.where(self.sea, v, 0)

    def state(self, today: int):
        c = self.c
        v = self.density(today)
        t = np.clip(v / self.vmax, 0, 1)
        dem = (t ** HEIGHT_GAMMA) * RELIEF_MAX * self.ncol
        dem[~self.sea] = 0.8                       # land: a low flat plateau

        albedo = np.empty((self.nrow, self.ncol, 4), np.float32)
        albedo[..., 3] = 1
        albedo[..., :3] = colour_ramp(t ** HEIGHT_GAMMA)
        line = (c["outline"] == 1) & self.sea & (t < 0.04)   # MPA outlines on calm water only
        albedo[line, :3] = lin(MPA_LINE)
        albedo[~self.sea, :3] = lin(LAND)

        oil = np.bincount(c["pr_mid"], weights=c["pr_km2"] * ramp(today, c["pr_dnum"]),
                          minlength=int(c["n_mpa"]))
        stats = dict(km2_in=float(oil.sum()), n_mpa=int((oil > 0).sum()),
                     n_slicks=int((c["sl_dnum"] <= today).sum()))
        return dem.astype(np.float32), albedo, stats


def _flat_radiance() -> float:
    """Radiance of a flat, white, sunlit surface: used to map render -> colours, so
    flat ground shows exactly its palette colour and only relief adds light/shadow."""
    from forge3d.path_tracing import hybrid_render_terrain_reference
    cam = {"model": "orthographic", "origin": (0.0, 100.0, 60.0), "look_at": (0.0, 0.0, 0.0),
           "up": (0.0, 0.5, -0.866), "half_height": 4.0}
    res = hybrid_render_terrain_reference(
        np.zeros((16, 16), np.float32), 16, 16, cam, albedo=(1.0, 1.0, 1.0),
        sun_azimuth_deg=SUN_AZ, sun_elevation_deg=SUN_ELEV, sun_intensity=SUN_INT,
        env_intensity=ENV_INT, min_frames=8, max_frames=8, variance_threshold=1e9)
    return float(np.asarray(res["radiance"])[8, 8].mean())


def render(scene: Scene, dem, albedo, orbit_deg: float, width=W, height=H) -> np.ndarray:
    from forge3d.path_tracing import hybrid_render_terrain_reference

    if scene.k is None:
        scene.k = _flat_radiance()
    n = scene.ncol
    el, az = np.deg2rad(VIEW_ELEV), np.deg2rad(orbit_deg)
    dist = 3.0 * max(scene.nrow, scene.ncol)
    origin = (dist * np.cos(el) * np.sin(az), dist * np.sin(el), dist * np.cos(el) * np.cos(az))
    up = (-np.sin(el) * np.sin(az), np.cos(el), -np.sin(el) * np.cos(az))
    look = (LOOK_SHIFT * scene.nrow * np.sin(az), 0.0, LOOK_SHIFT * scene.nrow * np.cos(az))
    cam = {"model": "orthographic", "origin": tuple(o + l for o, l in zip(origin, look)),
           "look_at": look, "up": up, "half_height": ZOOM * n}
    # Render in tiles of a shared virtual sensor (forge3d caps GPU memory per call;
    # tiles are identical to one big render)
    dem_c = np.ascontiguousarray(dem, np.float32)
    alb_c = np.ascontiguousarray(albedo)
    radiance = np.empty((height, width, 3), np.float32)
    depth = np.empty((height, width), np.float32)
    for y0 in range(0, height, TILE):
        for x0 in range(0, width, TILE):
            tw, th = min(TILE, width - x0), min(TILE, height - y0)
            res = hybrid_render_terrain_reference(
                dem_c, tw, th, cam, spacing=(1.0, 1.0), exaggeration=1.0,
                albedo_map=alb_c, albedo_sampling="bilinear",
                sensor_rect=(x0 / width, y0 / height, (x0 + tw) / width, (y0 + th) / height),
                full_width=width, full_height=height, pixel_offset=(x0, y0),
                sun_azimuth_deg=SUN_AZ, sun_elevation_deg=SUN_ELEV, sun_intensity=SUN_INT,
                env_intensity=ENV_INT, spp=1, min_frames=FRAMES_PER_RENDER,
                max_frames=FRAMES_PER_RENDER, variance_threshold=1e9, seed=7)
            radiance[y0:y0 + th, x0:x0 + tw] = np.asarray(res["radiance"], np.float32)[..., :3]
            depth[y0:y0 + th, x0:x0 + tw] = np.asarray(res["depth"], np.float32)
    res = {"radiance": radiance, "depth": depth}
    lin_rgb = np.asarray(res["radiance"], np.float32) / scene.k
    miss = ~np.isfinite(np.asarray(res["depth"]))
    lin_rgb[miss] = lin(SEA)
    lin_rgb = np.nan_to_num(np.clip(lin_rgb, 0, 1))
    srgb = np.where(lin_rgb <= 0.0031308, 12.92 * lin_rgb, 1.055 * lin_rgb ** (1 / 2.4) - 0.055)
    return (srgb * 255 + 0.5).astype(np.uint8)


def compose(rgb: np.ndarray, d: date, stats: dict, scene: Scene, out: Path,
            outro: bool = False) -> None:
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    dpi = 144
    fig = plt.figure(figsize=(W / dpi, H / dpi), dpi=dpi)
    ax = fig.add_axes([0, 0, 1, 1]); ax.axis("off")
    ax.imshow(rgb, extent=(0, 1, 0, 1), aspect="auto", interpolation="lanczos")
    ax.set_xlim(0, 1); ax.set_ylim(0, 1)
    t = lambda x, y, s, **k: ax.text(x, y, s, transform=ax.transAxes, **k)
    t(0.06, 0.955, "Three years of oil slicks\nin the North Sea", color=INK, fontsize=19,
      fontweight="bold", va="top", linespacing=1.05)
    t(0.94, 0.955, d.strftime("%b %Y"), color=ACCENT, fontsize=20, fontweight="bold",
      va="top", ha="right")
    t(0.06, 0.865, SUBTITLE, color=INK2, fontsize=8.4,
      va="top", linespacing=1.35)
    # continuous legend, same scale (and same height curve) as the relief
    x0, x1, y0, y1 = 0.06, 0.46, 0.808, 0.820
    tt = np.linspace(0, 1, 256)
    bar = colour_ramp(tt ** HEIGHT_GAMMA)
    bar = np.where(bar <= 0.0031308, 12.92 * bar, 1.055 * bar ** (1 / 2.4) - 0.055)
    ax.imshow(bar[None, :, :], extent=(x0, x1, y0, y1), aspect="auto",
              transform=ax.transAxes)
    for f in (0, 0.25, 0.5, 0.75, 1):
        lab = f"{f * scene.vmax:,.0f}" + ("+" if f == 1 else "")
        t(x0 + f * (x1 - x0), y0 - 0.006, lab, color=INK2, fontsize=7, ha="center", va="top")
    t(x1 + 0.02, (y0 + y1) / 2, "km² of slick per 1,000 km² of sea\n(cumulative)",
      color=MUTED, fontsize=7, va="center", linespacing=1.25)
    t(0.06, 0.775, f"{stats['km2_in']:,.0f} km²", color=ACCENT, fontsize=15,
      fontweight="bold", va="top")
    t(0.06, 0.742, "cumulative slick area inside MPAs", color=INK2, fontsize=7.8, va="top")
    t(0.50, 0.775, f"{stats['n_mpa']:,}", color=INK, fontsize=15, fontweight="bold", va="top")
    t(0.50, 0.742, f"of {int(scene.c['n_mpa']):,} MPAs reached", color=INK2, fontsize=7.8,
      va="top")
    if outro:
        t(0.06, 0.06, f"{stats['n_slicks']:,} slicks in three years. "
          f"{stats['n_mpa']:,} MPAs reached.", color=INK, fontsize=11, fontweight="bold",
          bbox=dict(facecolor=SEA, edgecolor="none", pad=6))
    t(0.98, 0.012, "More oil from April 2025 partly reflects a second Sentinel-1 satellite (1C)\n"
      "Data: SkyTruth Cerulean · WDPA (UNEP-WCMC & IUCN) · Théophile Mouton · "
      "DataSphere Analytics", color=MUTED, fontsize=5.6, ha="right", va="bottom",
      linespacing=1.3)
    fig.savefig(out, dpi=dpi, facecolor=SEA)
    plt.close(fig)


# ---- Main -----------------------------------------------------------------------------
def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--preview", action="store_true", help="render one still only")
    ap.add_argument("--day", default="2025-12-31", help="day shown by --preview")
    ap.add_argument("--version", choices=list(VERSIONS), default="exact",
                    help="exact (raw 2 km cells) or density (smoothed)")
    ap.add_argument("--draft", action="store_true",
                    help="quick video: weekly frames, 12 fps, fewer render passes")
    ap.add_argument("--scale", type=float, default=1.0, help="render size factor (preview)")
    args = ap.parse_args()
    global VERSION, SMOOTH_KM, SUBTITLE, FRAMES
    VERSION = args.version
    SMOOTH_KM = VERSIONS[VERSION]["smooth_km"]
    SUBTITLE = VERSIONS[VERSION]["subtitle"]
    FRAMES = ROOT / "frames" / f"relief_{VERSION}"
    global STEP_DAYS, FPS, FRAMES_PER_RENDER
    tag = ""
    if args.draft:
        STEP_DAYS, FPS, FRAMES_PER_RENDER = 7, 12, 8
        tag = "_draft"
        FRAMES = ROOT / "frames" / f"relief_{VERSION}_draft"

    if not (RAW / "region_slicks.gpkg").exists():
        sys.exit("Run from the project root (raw/region_slicks.gpkg not found).")
    if not CACHE.exists():
        build_cache()
    scene = Scene(dict(np.load(CACHE)))
    FIG.mkdir(exist_ok=True)
    total_days = (END - START).days
    print(f"Colour/height scale: 0 to {scene.vmax:,.0f} km² of slick per 1,000 km² (cumulative)")

    def orbit(dn):
        return ORBIT_FROM + (ORBIT_TO - ORBIT_FROM) * dn / total_days

    if args.preview:
        d = date.fromisoformat(args.day)
        dn = (d - START).days
        t0 = time.time()
        dem, albedo, stats = scene.state(dn)
        rgb = render(scene, dem, albedo, orbit(dn),
                     int(W * args.scale) // 2 * 2, int(H * args.scale) // 2 * 2)
        out = FIG / f"preview_relief_{VERSION}.png"
        compose(rgb, d, stats, scene, out, outro=d == END)
        print(f"Preview written to {out} ({time.time() - t0:.0f} s)")
        return 0

    import imageio_ffmpeg
    shutil.rmtree(FRAMES, ignore_errors=True); FRAMES.mkdir(parents=True)
    days = list(range(0, total_days + 1, STEP_DAYS))
    if days[-1] != total_days:
        days.append(total_days)
    t0 = time.time()
    for i, dn in enumerate(days):
        d = START + timedelta(days=dn)
        dem, albedo, stats = scene.state(dn)
        rgb = render(scene, dem, albedo, orbit(dn))
        compose(rgb, d, stats, scene, FRAMES / f"f{i:05d}.png", outro=dn == total_days)
        el = time.time() - t0
        print(f"\r  frame {i + 1}/{len(days)}  ({el / (i + 1):.1f} s/frame, "
              f"~{el / (i + 1) * (len(days) - i - 1) / 60:.0f} min left)", end="", flush=True)
    print()
    last = FRAMES / f"f{len(days) - 1:05d}.png"
    for k in range(HOLD_S * FPS):
        shutil.copy(last, FRAMES / f"f{len(days) + k:05d}.png")
    out = FIG / f"oil_relief_{VERSION}{tag}.mp4"
    subprocess.run([imageio_ffmpeg.get_ffmpeg_exe(), "-y", "-loglevel", "error",
                    "-framerate", str(FPS), "-i", str(FRAMES / "f%05d.png"),
                    "-c:v", "libx264", "-pix_fmt", "yuv420p", "-crf", "18", str(out)],
                   check=True)
    print(f"Done: {out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
