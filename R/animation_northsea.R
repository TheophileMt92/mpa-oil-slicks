# =============================================================================
# Oil building up in North Sea marine protected areas, 2023-2025
#
# One frame per day. Each satellite-detected slick flashes on its detection
# date, then cools to a faint ember that stays on the map, so three years of
# chronic pollution accumulate. MPAs light up the first time a slick touches
# them and stay red. Counters track slicks and MPAs reached.
#
# Inputs  raw/region_slicks.gpkg, raw/region_mpas.gpkg  (prep/extract_region.R)
# Output  frames/ (PNG, gitignored)  ->  figures/oil_northsea.mp4
#         1080 x 1350 px (LinkedIn portrait 4:5), 30 fps, about 40 s
#
# Run from the project root:  Rscript R/animation_northsea.R
# Rendering ~1,150 frames takes 5-15 min depending on the machine.
# =============================================================================

Sys.unsetenv(c("PROJ_LIB", "PROJ_DATA", "GDAL_DATA"))   # avoid conda's old PROJ
need <- c("sf", "ggplot2", "ragg", "png", "av", "rnaturalearth", "rnaturalearthdata")
new  <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({ library(sf); library(ggplot2) })
sf_use_s2(FALSE)

# ---- Settings -----------------------------------------------------------------
BBOX     <- c(xmin = -6, ymin = 48.5, xmax = 15, ymax = 61.5)
CRS      <- 3035                       # ETRS89 LAEA Europe (equal area)
START    <- as.Date("2023-01-01"); END <- as.Date("2025-12-31")
FPS      <- 30                         # 1 frame = 1 day  ->  ~36 s of timeline
INTRO    <- 30                         # frames of empty map before day 1
OUTRO    <- 120                        # frames holding the final state
FLASH    <- 12                         # days a new slick stays bright
W <- 1080; H <- 1350; DPI <- 144
FRAMES   <- "frames/northsea"
OUT      <- "figures/oil_northsea.mp4"
CORES    <- max(1, (if (is.na(parallel::detectCores(logical = FALSE)))
                   parallel::detectCores() else parallel::detectCores(logical = FALSE)) - 1)  # physical cores

# Report palette (template/custom.css)
SEA   <- "#052832"; LAND <- "#24343a"; COAST <- "#3a4c52"
MPA_LINE <- "#3f7f86"; MPA_FILL <- "#0a3a44"
HIT_FILL <- "#c01f14"; HIT_LINE <- "#ff6633"
FLASH_COL <- "#fff1e6"; EMBER <- "#ff6633"
INK <- "#ffffff"; INK2 <- "#9cb7c9"; MUTED <- "#6f8a99"

dir.create(FRAMES, recursive = TRUE, showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
unlink(list.files(FRAMES, full.names = TRUE))

# Full renders started from Positron/RStudio run in a separate Rscript process
# (see render_parallel() below), so hand over before loading any data.
if (interactive() && !nzchar(Sys.getenv("PREVIEW"))) {
  message("Interactive session: rendering in a separate Rscript process ...")
  status <- system2(file.path(R.home("bin"), "Rscript"), "R/animation_northsea.R")
  if (status != 0) stop("Rendering failed (see messages above)")
  # The child process already printed "Done"; end source() here, without an error
  invokeRestart("abort")
}

# ---- Data ---------------------------------------------------------------------
box <- st_transform(st_as_sfc(st_bbox(BBOX, crs = 4326)), CRS)
bb  <- st_bbox(box)
xlim <- c(bb["xmin"], bb["xmax"]); ylim0 <- c(bb["ymin"], bb["ymax"])
mapw <- diff(xlim); maph <- diff(ylim0)
# Extra space above the map for title and counters, sized for a 4:5 canvas
yhead <- mapw * H / W - maph
ylim  <- c(ylim0[1], ylim0[2] + yhead)

# 1:10m coastline (detailed fjords and estuaries); downloaded once by rnaturalearth
land <- rnaturalearth::ne_download(scale = 10, type = "land", category = "physical",
                                   returnclass = "sf") |>
  st_make_valid() |> st_transform(CRS) |> st_crop(st_buffer(box, 2e5)) |>
  st_union()                                              # union: no country borders

# Coastal MPAs include land and intertidal areas in the WDPA: show only their
# sea part, so no MPA appears to sit on land
mpa <- st_read("raw/region_mpas.gpkg", quiet = TRUE) |> st_transform(CRS) |> st_make_valid()
mpa <- st_difference(mpa, land)
mpa <- mpa[!st_is_empty(mpa), ]
mpa <- st_collection_extract(mpa, "POLYGON")
mpa <- aggregate(mpa["WDPAID"], by = list(WDPAID = mpa$WDPAID), FUN = function(x) x[1])
mpa$mid <- seq_len(nrow(mpa))

sl <- st_read("raw/region_slicks.gpkg", quiet = TRUE) |> st_transform(CRS) |> st_make_valid()
sl$day <- as.Date(substr(sl$slick_timestamp, 1, 10))
sl <- sl[!is.na(sl$day) & sl$day >= START & sl$day <= END, ]
sl <- sl[st_intersects(sl, box, sparse = FALSE)[, 1], ]
pt <- st_coordinates(st_point_on_surface(st_geometry(sl)))
sl$x <- pt[, 1]; sl$y <- pt[, 2]

# First day each MPA is touched by a slick
hits <- st_intersects(sl, mpa)
first_hit <- rep(as.Date(NA), nrow(mpa))
for (i in order(sl$day)) for (m in hits[[i]]) if (is.na(first_hit[m])) first_hit[m] <- sl$day[i]
mpa$first_hit <- first_hit
sl$in_mpa <- lengths(hits) > 0
message(sprintf("%d slicks, %d MPAs, %d reached", nrow(sl), nrow(mpa), sum(!is.na(first_hit))))

# Polygons as plain data frames (fast to draw every frame)
to_df <- function(g, id) {
  cc <- st_coordinates(st_cast(st_geometry(g), "MULTIPOLYGON"))
  data.frame(x = cc[, 1], y = cc[, 2], id = id[cc[, "L3"]],
             grp = paste(id[cc[, "L3"]], cc[, "L2"], cc[, "L1"]))
}
hit_mpa <- st_intersection(mpa[!is.na(mpa$first_hit), ], box)   # keep inside the map
hit_mpa <- hit_mpa[!st_is_empty(hit_mpa), ]
hit_mpa <- st_collection_extract(hit_mpa, "POLYGON")
hit_df <- to_df(hit_mpa, hit_mpa$mid)
hit_df$first_hit <- mpa$first_hit[match(hit_df$id, mpa$mid)]

# ---- Static background, rendered once -----------------------------------------
theme_frame <- theme_void() +
  theme(plot.background = element_rect(fill = SEA, colour = NA),
        panel.background = element_rect(fill = SEA, colour = NA),
        plot.margin = margin(0, 0, 0, 0))

bg_plot <- ggplot() +
  geom_sf(data = land, fill = LAND, colour = COAST, linewidth = 0.25) +
  geom_sf(data = mpa, fill = MPA_FILL, colour = MPA_LINE, linewidth = 0.18, alpha = 0.9) +
  coord_sf(crs = CRS, xlim = xlim, ylim = ylim0, expand = FALSE, datum = NA) +
  theme_frame
bg_file <- file.path(FRAMES, "_background.png")
ragg::agg_png(bg_file, width = W, height = round(W * maph / mapw), res = DPI, background = SEA)
print(bg_plot); invisible(dev.off())
bg <- png::readPNG(bg_file)

# ---- Frame builder ------------------------------------------------------------
days  <- seq(START, END, by = "day")
total_mpa <- nrow(mpa)
fmt <- function(n) format(n, big.mark = ",")
tx <- function(f) xlim[1] + f * mapw                     # x as a fraction of width
ty <- function(f) ylim0[2] + f * yhead                    # y within the header band

# Timeline strip along the bottom of the header: monthly slick counts
months  <- seq(START, END, by = "month")
mcount  <- tabulate(match(format(sl$day, "%Y-%m"), format(months, "%Y-%m")), length(months))
tl_x0 <- 0.06; tl_x1 <- 0.94; tl_y0 <- 0.08; tl_h <- 0.16
bar_w <- (tl_x1 - tl_x0) / length(months)
bars <- data.frame(m = months, n = mcount, row.names = NULL,
                   x0 = tx(tl_x0 + (seq_along(months) - 1) * bar_w + bar_w * 0.15),
                   x1 = tx(tl_x0 + seq_along(months) * bar_w - bar_w * 0.15),
                   y0 = ty(tl_y0), y1 = ty(tl_y0) + yhead * tl_h * mcount / max(mcount))

draw_frame <- function(k, today, file, outro = FALSE) {
  past  <- sl[sl$day <= today, ]
  age   <- as.numeric(today - past$day)
  fresh <- past[age < FLASH, ]
  fage  <- age[age < FLASH]
  hitd  <- hit_df[hit_df$first_hit <= today, ]
  hage  <- as.numeric(today - hitd$first_hit)
  n_hit <- sum(!is.na(mpa$first_hit) & mpa$first_hit <= today)

  p <- ggplot() +
    annotation_raster(bg, xmin = xlim[1], xmax = xlim[2], ymin = ylim0[1], ymax = ylim0[2]) +
    # MPAs already reached: red fill, bright outline that settles after ~3 weeks
    geom_polygon(data = hitd, aes(x, y, group = grp), fill = HIT_FILL, alpha = 0.45,
                 colour = NA) +
    geom_polygon(data = hitd, aes(x, y, group = grp, alpha = pmax(0.35, 1 - hage / 20)),
                 fill = NA, colour = HIT_LINE, linewidth = 0.3) +
    # Embers: every slick seen so far, small and dim
    geom_point(data = past, aes(x, y), colour = EMBER, alpha = 0.4, size = 0.8,
               stroke = 0) +
    # Flashes: slicks detected in the last FLASH days, big and bright, fading
    geom_point(data = fresh, aes(x, y, size = sqrt(area_km2) * (1 + 2 * (1 - fage / FLASH)),
                                 alpha = (1 - fage / FLASH)^1.5),
               colour = HIT_LINE, stroke = 0) +
    geom_point(data = fresh, aes(x, y, size = sqrt(area_km2) * 0.9,
                                 alpha = (1 - fage / FLASH)), colour = FLASH_COL, stroke = 0) +
    scale_size_identity() + scale_alpha_identity() +
    # Header
    annotate("text", x = tx(0.06), y = ty(0.93), hjust = 0, vjust = 1, colour = INK,
             size = 7.2, fontface = "bold", lineheight = 0.95,
             label = "Oil building up in North Sea\nmarine protected areas") +
    annotate("text", x = tx(0.06), y = ty(0.585), hjust = 0, vjust = 1, colour = INK2,
             size = 3.4, label = "Every satellite-detected oil slick, day by day") +
    annotate("text", x = tx(0.94), y = ty(0.93), hjust = 1, vjust = 1, colour = HIT_LINE,
             size = 8.5, fontface = "bold", label = format(today, "%b %Y")) +
    annotate("text", x = tx(0.06), y = ty(0.44), hjust = 0, vjust = 1, colour = INK,
             size = 5.6, fontface = "bold", label = fmt(nrow(past))) +
    annotate("text", x = tx(0.06), y = ty(0.31), hjust = 0, vjust = 1, colour = INK2,
             size = 3.1, label = "oil slicks detected") +
    annotate("text", x = tx(0.40), y = ty(0.44), hjust = 0, vjust = 1, colour = HIT_LINE,
             size = 5.6, fontface = "bold", label = fmt(n_hit)) +
    annotate("text", x = tx(0.40), y = ty(0.31), hjust = 0, vjust = 1, colour = INK2,
             size = 3.1, label = sprintf("of %s MPAs reached", fmt(total_mpa))) +
    # Timeline: monthly counts, filled up to today
    geom_rect(data = bars, aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1),
              fill = ifelse(bars$m <= today, EMBER, COAST)) +
    annotate("text", x = tx(tl_x0 + bar_w * c(0, 12, 24)), y = ty(tl_y0) - yhead * 0.03,
             label = c("2023", "2024", "2025"), hjust = 0, vjust = 1, colour = MUTED,
             size = 2.6) +
    annotate("text", x = tx(tl_x1), y = ty(tl_y0) - yhead * 0.03, hjust = 1, vjust = 1,
             colour = MUTED, size = 2.6, label = "Slicks detected per month") +
    # Credit
    annotate("text", x = tx(0.98), y = ylim0[1] + maph * 0.012, hjust = 1, vjust = 0,
             colour = MUTED, size = 2.3, lineheight = 0.95,
             label = paste0("More detections from April 2025 partly reflect a second Sentinel-1 satellite (1C)\n",
                            "Data: SkyTruth Cerulean (Sentinel-1) \u00b7 WDPA (UNEP-WCMC & IUCN)\n",
                            "Théophile Mouton · DataSphere Analytics")) +
    coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_frame

  if (outro) {
    p <- p + annotate("label", x = tx(0.73), y = ylim0[1] + maph * 0.12, hjust = 0.5,
                      label = sprintf("%s slicks in three years.\n%s of %s MPAs reached.",
                                      fmt(nrow(past)), fmt(n_hit), fmt(total_mpa)),
                      colour = INK, fill = SEA, size = 4.4, fontface = "bold",
                      label.size = 0, label.padding = unit(0.6, "lines"), lineheight = 1.1)
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

# ---- Render -------------------------------------------------------------------
plan <- data.frame(
  today = c(rep(START - 1, INTRO), days, rep(END, OUTRO)),
  outro = c(rep(FALSE, INTRO + length(days)), rep(TRUE, OUTRO)))
plan$k <- seq_len(nrow(plan))
plan$file <- file.path(FRAMES, sprintf("f%05d.png", plan$k))

# Quick look: PREVIEW=1 renders only a few frames to figures/preview_*.png
if (nzchar(Sys.getenv("PREVIEW"))) {
  for (d in c("2023-03-15", "2024-06-15", "2025-12-31")) {
    draw_frame(0, as.Date(d), sprintf("figures/preview_%s.png", d), outro = d == "2025-12-31")
  }
  quit(save = "no")
}

# Identical frames (intro, outro) are rendered once and copied
render_one <- function(i) draw_frame(plan$k[i], plan$today[i], plan$file[i], plan$outro[i])
uniq <- c(1, (INTRO + 1):(INTRO + length(days)), INTRO + length(days) + 1)
message(sprintf("Rendering %d frames on %d cores ...", length(uniq), CORES))
render_parallel(uniq, render_one)
for (i in setdiff(seq_len(nrow(plan)), uniq)) {
  src <- if (i <= INTRO) plan$file[1] else plan$file[INTRO + length(days) + 1]
  file.copy(src, plan$file[i], overwrite = TRUE)
}

av::av_encode_video(plan$file, output = OUT, framerate = FPS, vfilter = "format=yuv420p",
                    verbose = FALSE)
message("Done: ", OUT)
