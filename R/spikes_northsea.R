# =============================================================================
# North Sea oil "pinboard", 2023-2025
#
# A tilted (pseudo-3D) map with one thin pin per 20 km grid cell. Each pin rises
# as oil slicks are detected in that cell, so shipping lanes and oil fields grow
# into a forest of pins. Underneath, every marine protected area fills in with
# colour as oil accumulates inside it, using the same measure and colour classes
# as the report's MPA table (km2 of slick per 1,000 km2 of MPA per year).
#
# Inputs  raw/region_slicks.gpkg, raw/region_mpas.gpkg   (prep/extract_region.R)
# Output  frames/spikes/ (PNG, gitignored)  ->  figures/oil_spikes_northsea.mp4
#         1080 x 1350 px (LinkedIn 4:5), 30 fps, ~40 s
#
# Quick look first:   Sys.setenv(PREVIEW = "1"); source("R/spikes_northsea.R")
# Full render:        Sys.setenv(PREVIEW = "");  source("R/spikes_northsea.R")
# =============================================================================

Sys.unsetenv(c("PROJ_LIB", "PROJ_DATA", "GDAL_DATA"))   # avoid conda's old PROJ
need <- c("sf", "ggplot2", "ragg", "png", "av", "rnaturalearth", "rnaturalearthdata")
new  <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({ library(sf); library(ggplot2) })
sf_use_s2(FALSE)

# ---- Settings -----------------------------------------------------------------
BBOX   <- c(xmin = -4.5, ymin = 50.5, xmax = 12.5, ymax = 61.5)  # North Sea + Channel
CRS    <- 3035              # ETRS89 LAEA Europe (equal area)
CELL   <- 20000             # pin spacing (m)
TILT   <- 0.55              # vertical squash of the map plane (1 = seen from above)
PIN_MAX <- 0.30             # tallest pin, as a share of the map width
PIN_LW  <- 0.35             # pin stem width (mm)
START  <- as.Date("2023-01-01"); END <- as.Date("2025-12-31")
RAMP   <- 10                # days a new slick takes to grow into its pin
FPS    <- 30                # 1 frame = 1 day
INTRO  <- 30; OUTRO <- 120  # frames before day 1 / holding the end
W <- 1080; H <- 1350; DPI <- 144
FRAMES <- "frames/spikes"; OUT <- "figures/oil_spikes_northsea.mp4"
CORES  <- max(1, (if (is.na(parallel::detectCores(logical = FALSE)))
                   parallel::detectCores() else parallel::detectCores(logical = FALSE)) - 1)

# MPA colour classes: km2 of slick per 1,000 km2 of MPA per year. Same breaks as
# the report (quartiles of exposed MPAs, rounded) and the same pale -> deep red
# ramp as the country grid: darker = more oil.
BREAKS     <- c(0.1, 0.6, 2)
BREAK_LABS <- c("< 0.1", "0.1-0.6", "0.6-2", "> 2")
POLL_FILL  <- c("#fcc5a0", "#f6874f", "#e04a24", "#c01f14")

# Report palette
SEA <- "#052832"; LAND <- "#24343a"; COAST <- "#3a4c52"
MPA_FILL <- "#0c4450"; MPA_LINE <- "#3f8c90"
PIN_STEM <- "#e8dcd3"; PIN_HEAD <- "#ffffff"
INK <- "#ffffff"; INK2 <- "#9cb7c9"; MUTED <- "#6f8a99"; ACCENT <- "#ff6633"

dir.create(FRAMES, recursive = TRUE, showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

# Full renders started from Positron/RStudio run in a separate Rscript process
# (see render_parallel() below), so hand over before loading any data.
if (interactive() && !nzchar(Sys.getenv("PREVIEW"))) {
  message("Interactive session: rendering in a separate Rscript process ...")
  status <- system2(file.path(R.home("bin"), "Rscript"), "R/spikes_northsea.R")
  if (status != 0) stop("Rendering failed (see messages above)")
  # The child process already printed "Done"; end source() here, without an error
  invokeRestart("abort")
}

# ---- Data ---------------------------------------------------------------------
# Rectangular frame in the projected CRS (a lon/lat box would be skewed)
box <- st_as_sfc(st_bbox(st_transform(st_as_sfc(st_bbox(BBOX, crs = 4326)), CRS)))
bb  <- st_bbox(box)
x0 <- bb[["xmin"]]; y0 <- bb[["ymin"]]
mapw <- bb[["xmax"]] - x0

# Tilt: shift to the box origin, squash y. Works on sf geometries and on x/y.
tilt_sf <- function(g) {
  g <- st_geometry(g)
  g <- (g - c(x0, y0)) * matrix(c(1, 0, 0, TILT), 2, 2)
  st_set_crs(g, NA)
}
tilt_y <- function(y) (y - y0) * TILT

# 1:10m coastline (detailed fjords and estuaries); downloaded once by rnaturalearth
land <- rnaturalearth::ne_download(scale = 10, type = "land", category = "physical",
                                   returnclass = "sf") |>
  st_make_valid() |> st_transform(CRS) |> st_crop(st_buffer(box, 1e5)) |>
  st_union() |> st_intersection(box)

# MPAs: sea part only (coastal sites include land in the WDPA), one row per site
mpa <- st_read("raw/region_mpas.gpkg", quiet = TRUE) |> st_transform(CRS) |>
  st_make_valid() |> st_intersection(box)
mpa <- st_difference(mpa, land)
mpa <- st_collection_extract(mpa[!st_is_empty(mpa), ], "POLYGON")
mpa <- aggregate(mpa[c("marine_km2")], by = list(WDPAID = mpa$WDPAID), FUN = function(x) x[1])
mpa$area_km2 <- ifelse(is.na(mpa$marine_km2) | mpa$marine_km2 <= 0,
                       as.numeric(st_area(mpa)) / 1e6, mpa$marine_km2)
mpa$mid <- seq_len(nrow(mpa))

sl <- st_read("raw/region_slicks.gpkg", quiet = TRUE) |> st_transform(CRS) |> st_make_valid()
sl$day <- as.Date(substr(sl$slick_timestamp, 1, 10))
sl <- sl[!is.na(sl$day) & sl$day >= START & sl$day <= END, ]
sl <- sl[st_intersects(sl, box, sparse = FALSE)[, 1], ]
sl$sid <- seq_len(nrow(sl))
pt <- st_coordinates(st_point_on_surface(st_geometry(sl)))

# Oil inside each MPA: slick polygons clipped to the MPA (as in the report)
hits  <- st_intersects(sl, mpa)
pairs <- data.frame(s = rep(seq_along(hits), lengths(hits)), m = unlist(hits))
pairs$km2 <- mapply(function(s, m) {
  a <- tryCatch(sum(as.numeric(st_area(st_intersection(st_geometry(sl)[s],
                                                       st_geometry(mpa)[m])))) / 1e6,
                error = function(e) NA_real_)
  if (is.na(a)) sl$area_km2[s] else a
}, pairs$s, pairs$m)
pairs$day <- sl$day[pairs$s]
n_years <- as.numeric(END - START + 1) / 365.25

# Pin cells: one per 20 km grid cell with any slick
sl$cx <- (floor((pt[, 1] - x0) / CELL) + 0.5) * CELL + x0
sl$cy <- (floor((pt[, 2] - y0) / CELL) + 0.5) * CELL + y0
sl$cell <- paste(sl$cx, sl$cy)
cells <- unique(as.data.frame(sl)[, c("cell", "cx", "cy")])
final <- tapply(sl$area_km2, sl$cell, sum)
hscale <- PIN_MAX * mapw / sqrt(max(final))
message(sprintf("%d slicks, %d pins, %d MPAs, %d reached",
                nrow(sl), nrow(cells), nrow(mpa), length(unique(pairs$m))))

# Reached MPAs as tilted polygon data frames (redrawn every frame with their colour)
to_df <- function(g, id) {
  cc <- st_coordinates(st_cast(g, "MULTIPOLYGON"))
  data.frame(x = cc[, 1], y = cc[, 2], m = id[cc[, "L3"]],
             grp = paste(id[cc[, "L3"]], cc[, "L2"]),   # one polygon
             sub = cc[, "L1"])                           # its rings (holes stay empty)
}
reached <- sort(unique(pairs$m))
mpa_df  <- to_df(tilt_sf(mpa[reached, ]), reached)

# ---- Layout and static background (tilted land + MPA outlines) ----------------
headroom <- PIN_MAX * mapw * 1.05
plane_h  <- (bb[["ymax"]] - y0) * TILT
header   <- mapw * H / W - plane_h - headroom
xlim <- c(0, mapw); ylim <- c(-0.02 * mapw, plane_h + headroom + header)

theme_frame <- theme_void() +
  theme(plot.background = element_rect(fill = SEA, colour = NA),
        panel.background = element_rect(fill = SEA, colour = NA),
        plot.margin = margin(0, 0, 0, 0))

bg_plot <- ggplot() +
  geom_sf(data = tilt_sf(land), fill = LAND, colour = COAST, linewidth = 0.25) +
  geom_sf(data = tilt_sf(mpa), fill = MPA_FILL, colour = MPA_LINE, linewidth = 0.15) +
  coord_sf(xlim = c(0, mapw), ylim = c(0, plane_h), expand = FALSE, datum = NA) +
  theme_frame
bg_file <- file.path(FRAMES, "_background.png")
ragg::agg_png(bg_file, width = W, height = round(W * plane_h / mapw), res = DPI,
              background = SEA)
print(bg_plot); invisible(dev.off())
bg <- png::readPNG(bg_file)

# ---- State for one day -----------------------------------------------------------
pins_for <- function(today) {
  w <- pmin(pmax(as.numeric(today - sl$day) / RAMP, 0), 1)     # growth ramp
  v <- tapply(sl$area_km2 * w, sl$cell, sum)
  d <- cells[cells$cell %in% names(v)[v > 0], ]
  if (!nrow(d)) return(NULL)
  d$h  <- sqrt(v[d$cell]) * hscale
  d$bx <- d$cx - x0
  d$by <- tilt_y(d$cy)
  d[order(-d$by), ]                          # back (north) to front (south)
}

# MPA colour class to date: oil inside / MPA area / years elapsed. On the last
# day this equals the report's "Oil per 1,000 km2 per yr".
mpa_levels <- function(today) {
  w <- pmin(pmax(as.numeric(today - pairs$day) / RAMP, 0), 1)
  v <- tapply(pairs$km2 * w, pairs$m, sum)
  v <- v[v > 0]
  if (!length(v)) return(NULL)
  dens <- v / mpa$area_km2[as.integer(names(v))] / n_years * 1000
  data.frame(m = as.integer(names(v)), lvl = findInterval(dens, BREAKS) + 1)
}

fmt <- function(n) format(round(n), big.mark = ",")
hy  <- function(f) plane_h + headroom + f * header      # y inside the header band

draw_frame <- function(today, file, outro = FALSE) {
  pins <- pins_for(today)
  lv   <- mpa_levels(today)
  km2_in <- sum(pairs$km2[pairs$day <= today])
  n_mpa  <- length(unique(pairs$m[pairs$day <= today]))

  p <- ggplot() +
    annotation_raster(bg, xmin = 0, xmax = mapw, ymin = 0, ymax = plane_h)
  if (!is.null(lv)) {
    poly <- mpa_df[mpa_df$m %in% lv$m, ]
    poly$fill <- POLL_FILL[lv$lvl[match(poly$m, lv$m)]]
    p <- p + geom_polygon(data = poly, aes(x, y, group = grp, subgroup = sub, fill = fill),
                          colour = NA, alpha = 0.92) +
      scale_fill_identity()
  }
  if (!is.null(pins)) {
    p <- p +
      geom_segment(data = pins, aes(x = bx, xend = bx, y = by, yend = by + h),
                   colour = PIN_STEM, linewidth = PIN_LW, alpha = 0.85, lineend = "round") +
      geom_point(data = pins, aes(x = bx, y = by + h), colour = PIN_HEAD,
                 size = 0.9, stroke = 0)
  }
  # Legend for MPA colours
  lx <- 0.06 * mapw + (0:3) * 0.13 * mapw
  p <- p +
    annotate("text", x = 0.06 * mapw, y = hy(0.92), hjust = 0, vjust = 1, colour = INK,
             size = 7, fontface = "bold", lineheight = 0.95,
             label = "Three years of oil slicks\nin the North Sea") +
    annotate("text", x = 0.94 * mapw, y = hy(0.92), hjust = 1, vjust = 1, colour = ACCENT,
             size = 8, fontface = "bold", label = format(today, "%b %Y")) +
    annotate("text", x = 0.06 * mapw, y = hy(0.66), hjust = 0, vjust = 1, colour = INK2,
             size = 3.2, lineheight = 1.1,
             label = paste0("Each pin: satellite-detected oil slicks in a 20 km cell, piling up.\n",
                            "Marine protected areas fill in as oil accumulates inside them:")) +
    annotate("rect", xmin = lx, xmax = lx + 0.03 * mapw, ymin = hy(0.43), ymax = hy(0.49),
             fill = POLL_FILL) +
    annotate("text", x = lx + 0.04 * mapw, y = hy(0.46), hjust = 0, vjust = 0.5,
             colour = INK2, size = 2.9, label = BREAK_LABS) +
    annotate("text", x = 0.06 * mapw + 4 * 0.13 * mapw, y = hy(0.46), hjust = 0, vjust = 0.5,
             colour = MUTED, size = 2.6, label = "km² of slick per 1,000 km² per year") +
    annotate("text", x = 0.06 * mapw, y = hy(0.30), hjust = 0, vjust = 1, colour = ACCENT,
             size = 5.4, fontface = "bold", label = sprintf("%s km²", fmt(km2_in))) +
    annotate("text", x = 0.06 * mapw, y = hy(0.20), hjust = 0, vjust = 1, colour = INK2,
             size = 3, label = "cumulative slick area inside MPAs") +
    annotate("text", x = 0.50 * mapw, y = hy(0.30), hjust = 0, vjust = 1, colour = INK,
             size = 5.4, fontface = "bold", label = fmt(n_mpa)) +
    annotate("text", x = 0.50 * mapw, y = hy(0.20), hjust = 0, vjust = 1, colour = INK2,
             size = 3, label = sprintf("of %s MPAs reached", fmt(nrow(mpa)))) +
    annotate("text", x = 0.98 * mapw, y = ylim[1] + 0.005 * mapw, hjust = 1, vjust = 0,
             colour = MUTED, size = 2.2, lineheight = 0.95,
             label = paste0("More pins from April 2025 partly reflect a second Sentinel-1 satellite (1C)\n",
                            "Data: SkyTruth Cerulean · WDPA (UNEP-WCMC & IUCN) · ",
                            "Théophile Mouton · DataSphere Analytics")) +
    coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_frame
  if (outro) {
    p <- p + annotate("label", x = 0.06 * mapw, y = hy(0.04), hjust = 0,
                      label = sprintf("%s slicks in three years. %s MPAs reached.",
                                      fmt(nrow(sl)), fmt(n_mpa)),
                      colour = INK, fill = SEA, size = 3.6, fontface = "bold",
                      label.size = 0, label.padding = unit(0.5, "lines"))
  }
  ragg::agg_png(file, width = W, height = H, res = DPI, background = SEA)
  print(p); invisible(dev.off())
}

# Parallel rendering with fresh R worker processes (PSOCK). Forking is avoided:
# Positron/RStudio forbid it, and on macOS forked workers crash once the graphics
# libraries are loaded. This always runs inside a clean `Rscript` process (full
# renders from Positron are handed over to one at the top of the script), so the
# global environment holds only this script's objects and is safe to copy over.
render_parallel <- function(idx, fun) {
  cl <- parallel::makePSOCKcluster(CORES)
  on.exit(parallel::stopCluster(cl))
  parallel::clusterEvalQ(cl, {
    suppressPackageStartupMessages({ library(sf); library(ggplot2) })
    sf_use_s2(FALSE)
  })
  parallel::clusterExport(cl, ls(globalenv()), envir = globalenv())
  invisible(parallel::parLapplyLB(cl, idx, fun))
}

# ---- Preview or render --------------------------------------------------------
if (nzchar(Sys.getenv("PREVIEW"))) {
  for (d in c("2023-06-30", "2024-09-30", "2025-12-31")) {
    draw_frame(as.Date(d), sprintf("figures/preview_spikes_%s.png", d),
               outro = d == "2025-12-31")
  }
  message("Previews written to figures/preview_spikes_*.png")
} else {
  unlink(list.files(FRAMES, pattern = "^f.*png$", full.names = TRUE))
  days <- seq(START, END, by = "day")
  plan <- data.frame(today = c(rep(START - 1, INTRO), days, rep(END, OUTRO)),
                     outro = c(rep(FALSE, INTRO + length(days)), rep(TRUE, OUTRO)))
  plan$file <- file.path(FRAMES, sprintf("f%05d.png", seq_len(nrow(plan))))
  uniq <- c(1, (INTRO + 1):(INTRO + length(days)), INTRO + length(days) + 1)
  message(sprintf("Rendering %d frames on %d cores ...", length(uniq), CORES))
  render_parallel(uniq, function(i) draw_frame(plan$today[i], plan$file[i], plan$outro[i]))
  for (i in setdiff(seq_len(nrow(plan)), uniq)) {
    src <- if (i <= INTRO) plan$file[1] else plan$file[INTRO + length(days) + 1]
    file.copy(src, plan$file[i], overwrite = TRUE)
  }
  av::av_encode_video(plan$file, output = OUT, framerate = FPS,
                      vfilter = "format=yuv420p", verbose = FALSE)
  message("Done: ", OUT)
}
