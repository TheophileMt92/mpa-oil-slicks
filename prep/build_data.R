# =============================================================================
# mpa-oil-slicks: data pipeline
#
# Downloads satellite-detected oil slicks (SkyTruth Cerulean) and marine
# protected areas (WDPA), then writes small, report-ready tables to data/.
#
#   data/mpa_exposure.rds   one row per MPA: name, country, protection class,
#                           marine area, slicks and oil area detected inside
#   data/class_summary.rds  slick density inside each protection class vs the
#                           unprotected ocean within RING_KM, with bootstrap CIs
#   data/slick_points.rds   one row per slick: lon, lat, area, month, class
#   data/run_info.rds       parameters and counts, for inline text in the report
#
# Run from the project root:   Rscript prep/build_data.R
# First run: roughly 30-60 min (WDPA ~1-2 GB, 36 monthly API calls).
# Re-runs reuse the caches in raw/ (gitignored).
# =============================================================================

# ---- 0. Packages -------------------------------------------------------------
pkgs <- c("httr2", "sf", "terra", "dplyr", "tidyr", "purrr", "readr",
          "wdpar", "rnaturalearth", "countrycode", "chromote")
new <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(new)) install.packages(new)
suppressPackageStartupMessages({
  library(httr2); library(sf); library(terra); library(dplyr)
  library(tidyr); library(purrr); library(readr)
})
sf_use_s2(FALSE)

# ---- 1. Parameters -----------------------------------------------------------
API          <- "https://api.cerulean.skytruth.org"
START        <- as.Date("2023-01-01")   # same window as SkyTruth's global estimate
END          <- as.Date("2025-12-31")
# SkyTruth recommends max_source_collated_score > 0 as "credible oil".
# Sensitivity alternative: "slick_confidence GTE 0.8"
SLICK_FILTER <- "max_source_collated_score GT 0"
PAGE_SIZE    <- 2000                    # API max is 9999; smaller = fewer timeouts
RES_KM       <- 5                       # equal-area analysis grid resolution (km)
RING_KM      <- 25                      # width of the comparison ring
LAT_RANGE    <- c(-78, 84)
N_BOOT       <- 2000
WDPA_PATH    <- NULL   # path to a manually downloaded WDPA .gdb/.zip, to skip wdpar's download
DIR_RAW      <- "raw"
DIR_DATA     <- "data"
set.seed(42)
dir.create(file.path(DIR_RAW, "slicks"), recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_DATA, showWarnings = FALSE)

# Protection classes (IUCN management categories)
#   1  No-take          IUCN Ia, Ib, II, III
#   2  Multiple-use     IUCN IV, V, VI
#   3  Not reported     Not Reported / Not Assigned / Not Applicable
CLASS_LABELS <- c("1" = "No-take (IUCN I-III)",
                  "2" = "Multiple-use (IUCN IV-VI)",
                  "3" = "Category not reported")
iucn_to_class <- function(cat) {
  cat <- trimws(cat)
  case_when(cat %in% c("Ia", "Ib", "II", "III") ~ 1L,
            cat %in% c("IV", "V", "VI")         ~ 2L,
            TRUE                                ~ 3L)
}

# ---- 2. Oil slicks from the Cerulean API -------------------------------------
# OGC API Features (tipg) with CQL2 filters; see SkyTruth's "Cerulean API Guide"
# notebook in github.com/SkyTruth/cerulean-cloud for field definitions.
read_geojson_text <- function(txt) {
  tryCatch(st_read(txt, quiet = TRUE), error = function(e) NULL)
}

# Consistent column types + geometry name, so pages and cached months bind cleanly
normalise_slicks <- function(x) {
  if (is.null(x) || nrow(x) == 0) return(NULL)
  d <- st_drop_geometry(x)
  st_sf(
    id                        = as.integer(d$id),
    slick_timestamp           = as.character(d$slick_timestamp),
    area                      = as.numeric(d$area),
    machine_confidence        = as.numeric(d$machine_confidence),
    max_source_collated_score = as.numeric(d$max_source_collated_score),
    hitl_cls                  = suppressWarnings(as.integer(d$hitl_cls)),
    geometry                  = st_geometry(x),
    crs                       = 4326
  )
}

fetch_slicks_month <- function(m_start) {
  cache <- file.path(DIR_RAW, "slicks", sprintf("slicks_%s.gpkg", format(m_start, "%Y_%m")))
  if (file.exists(cache)) return(normalise_slicks(st_read(cache, quiet = TRUE)))
  m_end  <- seq(m_start, by = "month", length.out = 2)[2] - 1
  offset <- 0
  pages  <- list()
  repeat {
    resp <- request(API) |>
      req_url_path_append("collections", "public.slick_plus", "items") |>
      req_url_query(
        limit      = PAGE_SIZE,
        offset     = offset,
        datetime   = sprintf("%sT00:00:00Z/%sT23:59:59Z", m_start, m_end),
        filter     = SLICK_FILTER,
        sortby     = "id",                 # stable order => safe pagination
        properties = "id,slick_timestamp,area,machine_confidence,max_source_collated_score,hitl_cls",
        f          = "geojson"
      ) |>
      req_timeout(420) |>
      req_retry(max_tries = 6, backoff = function(i) 15 * i,
                is_transient = function(r) resp_status(r) %in% c(429, 500, 502, 503, 504)) |>
      req_perform()
    pg <- normalise_slicks(read_geojson_text(resp_body_string(resp)))
    n  <- if (is.null(pg)) 0 else nrow(pg)
    message(sprintf("  %s  offset %6d  -> %d slicks", format(m_start, "%Y-%m"), offset, n))
    if (n > 0) pages[[length(pages) + 1]] <- pg
    if (n < PAGE_SIZE) break
    offset <- offset + PAGE_SIZE
  }
  out <- if (length(pages)) do.call(rbind, pages) else NULL
  if (!is.null(out)) st_write(out, cache, quiet = TRUE, delete_dsn = TRUE)
  out
}

message("Downloading slicks (cached per month in ", DIR_RAW, "/slicks) ...")
months <- seq(START, END, by = "month")
slicks <- do.call(rbind, compact(map(months, fetch_slicks_month)))
slicks <- slicks[!duplicated(slicks$id), ] |>
  st_make_valid() |>
  mutate(area_km2 = as.numeric(area) / 1e6,
         month    = substr(slick_timestamp, 1, 7))
message(sprintf("Slicks retained: %s (%.0f km2)",
                format(nrow(slicks), big.mark = ","), sum(slicks$area_km2)))

# One point per slick (point on surface, not centroid: slicks are often curved)
slick_pts <- st_point_on_surface(st_geometry(slicks))

# ---- 3. MPAs from the WDPA ---------------------------------------------------
mpa_cache <- file.path(DIR_RAW, "mpa_marine.gpkg")
if (file.exists(mpa_cache)) {
  mpa <- st_read(mpa_cache, quiet = TRUE)
} else {
  message("Loading WDPA ...")
  wdpa <- if (!is.null(WDPA_PATH)) {
    wdpar::wdpa_read(WDPA_PATH)
  } else {
    wdpar::wdpa_fetch("global", wait = TRUE,
                      download_dir = file.path(DIR_RAW, "wdpa"), verbose = TRUE)
  }
  wdpa <- st_sf(setNames(st_drop_geometry(wdpa), toupper(names(st_drop_geometry(wdpa)))),
                geometry = st_geometry(wdpa))

  is_marine <- if ("MARINE" %in% names(wdpa)) {
    as.character(wdpa$MARINE) %in% c("1", "2")          # 1 = coastal, 2 = marine
  } else if ("REALM" %in% names(wdpa)) {
    grepl("marine|coastal", wdpa$REALM, ignore.case = TRUE)
  } else stop("Cannot find a MARINE or REALM field in the WDPA: check names(wdpa)")
  is_poly <- as.character(st_geometry_type(wdpa)) %in% c("POLYGON", "MULTIPOLYGON")

  area_col <- intersect(c("GIS_M_AREA", "REP_M_AREA"), names(wdpa))[1]
  mpa <- wdpa[is_marine & is_poly, ] |>
    filter(STATUS %in% c("Designated", "Inscribed", "Established"),
           !grepl("UNESCO-MAB", DESIG_ENG, fixed = TRUE)) |>   # biosphere reserves: not MPAs
    transmute(WDPAID, NAME, DESIG_ENG, ISO3, IUCN_CAT = trimws(IUCN_CAT),
              STATUS_YR, marine_km2 = suppressWarnings(as.numeric(.data[[area_col]]))) |>
    st_make_valid()
  rm(wdpa); invisible(gc())
  st_write(mpa, mpa_cache, quiet = TRUE, delete_dsn = TRUE)
}
mpa$prot_class <- iucn_to_class(mpa$IUCN_CAT)
message(sprintf("Marine protected area polygons: %s", format(nrow(mpa), big.mark = ",")))
print(table(CLASS_LABELS[as.character(mpa$prot_class)]))

# ---- 4. Per-MPA exposure -----------------------------------------------------
# Oil area INSIDE each MPA = area of the slick polygon clipped to the MPA,
# computed in an equal-area CRS. A slick inside overlapping MPAs counts for each.
message("Per-MPA exposure ...")
EA    <- "EPSG:8857"          # Equal Earth
# WDPA and Cerulean geometries are already in -180..180; wrapping is a safety net
to_ea <- function(x) {
  w <- tryCatch(suppressWarnings(st_wrap_dateline(x, options = c("WRAPDATELINE=YES"))),
                error = function(e) x)
  if (any(st_is_empty(w))) w <- x
  st_transform(w, EA)
}

mpa_ea    <- to_ea(mpa)
slicks_ea <- to_ea(slicks[, c("id", "month", "area_km2")])
hits  <- st_intersects(slicks_ea, mpa_ea)
pairs <- tibble(s = rep(seq_along(hits), lengths(hits)), m = unlist(hits))
message(sprintf("  %s slick-MPA overlaps", format(nrow(pairs), big.mark = ",")))

sg <- st_geometry(slicks_ea); mg <- st_geometry(mpa_ea)
# One GEOS call per MPA (all its slicks at once), much faster than per pair
oil_by_mpa <- pairs |>
  group_by(m) |>
  summarise(s = list(s), .groups = "drop") |>
  mutate(oil_km2 = map2_dbl(s, m, function(s, m) {
    tryCatch(sum(as.numeric(st_area(st_intersection(sg[s], mg[m])))) / 1e6,
             # fallback where GEOS fails on a messy polygon: whole slick areas
             error = function(e) sum(slicks_ea$area_km2[s]))
  })) |>
  select(m, oil_km2)
pairs$month <- slicks_ea$month[pairs$s]

n_years <- length(unique(slicks$month)) / 12
mpa_centroid <- suppressWarnings(st_coordinates(st_point_on_surface(st_geometry(mpa))))

# Fallback marine area where the WDPA field is missing: polygon area
geom_km2 <- as.numeric(st_area(mpa_ea)) / 1e6
mpa$marine_km2 <- ifelse(is.na(mpa$marine_km2) | mpa$marine_km2 <= 0, geom_km2, mpa$marine_km2)

first_iso <- sub(";.*", "", mpa$ISO3)
mpa_tbl <- st_drop_geometry(mpa) |>
  mutate(row = row_number(),
         country = countrycode::countrycode(first_iso, "iso3c", "country.name",
                                            warn = FALSE),
         country = coalesce(country, ISO3),
         class_label = CLASS_LABELS[as.character(prot_class)],
         lon = mpa_centroid[, 1], lat = mpa_centroid[, 2])

mpa_exposure <- pairs |>
  group_by(m) |>
  summarise(n_slicks = n(), n_months = n_distinct(month), .groups = "drop") |>
  left_join(oil_by_mpa, by = "m") |>
  rename(row = m) |>
  right_join(mpa_tbl, by = "row") |>
  mutate(n_slicks = coalesce(n_slicks, 0L),
         n_months = coalesce(n_months, 0L),
         oil_km2  = coalesce(oil_km2, 0),
         # km2 of slick per 1,000 km2 of MPA per year
         oil_per_1000km2_yr = oil_km2 / marine_km2 / n_years * 1000) |>
  select(WDPAID, NAME, DESIG_ENG, country, ISO3, IUCN_CAT, prot_class, class_label,
         STATUS_YR, marine_km2, n_slicks, n_months, oil_km2, oil_per_1000km2_yr, lon, lat)
saveRDS(mpa_exposure, file.path(DIR_DATA, "mpa_exposure.rds"))
message(sprintf("  MPAs with >= 1 slick: %d of %d",
                sum(mpa_exposure$n_slicks > 0), nrow(mpa_exposure)))

# ---- 5. Class-level comparison: inside vs surrounding ocean ------------------
# Neighbouring waters share the same Sentinel-1 revisit pattern, shipping context
# and sea state, so the inside/ring ratio controls for uneven satellite coverage.
terraOptions(memfrac = 0.5, progress = 0)   # lower memfrac if R runs out of memory
message("Rasterising ...")
grid <- rast(ext(-17250000, 17250000, -8400000, 8400000), res = RES_KM * 1000, crs = EA)
cell_km2 <- RES_KM^2

lon <- seq(-180, 180, by = 0.5); lat <- seq(LAT_RANGE[1], LAT_RANGE[2], by = 0.5)
dom <- st_sfc(st_polygon(list(rbind(
  cbind(lon, LAT_RANGE[1]), cbind(180, lat), cbind(rev(lon), LAT_RANGE[2]),
  cbind(-180, rev(lat))))), crs = 4326) |> st_transform(EA)
domain <- !is.na(rasterize(vect(dom), grid, field = 1))

land  <- rnaturalearth::ne_download(scale = 10, type = "land", category = "physical",
                                    returnclass = "sf")
ocean <- domain & is.na(rasterize(vect(st_transform(st_make_valid(land), EA)), grid, field = 1))

# Strongest protection wins where MPAs overlap (lowest class number)
prot <- rasterize(vect(mpa_ea[, "prot_class"]), grid, field = "prot_class", fun = "min")
prot <- mask(prot, ocean, maskvalues = c(FALSE, NA))
unprot_ocean <- ocean & is.na(prot)

# Rings: buffer simplified MPA polygons by RING_KM, keep unprotected ocean only
mpa_buf <- mpa_ea[, "prot_class"] |> st_simplify(dTolerance = 500) |> st_buffer(RING_KM * 1000)
near_to <- function(classes) {
  !is.na(rasterize(vect(mpa_buf[mpa_buf$prot_class %in% classes, ]), grid, field = 1))
}

zones <- list()           # kept as a list: stacking all layers can exhaust memory
for (k in 1:3) {
  inside <- prot == k
  inside[is.na(inside)] <- FALSE
  zones[[paste0("in_", k)]]   <- inside
  zones[[paste0("ring_", k)]] <- near_to(k) & unprot_ocean
}
zones[["in_all"]]      <- ocean & !is.na(prot)
zones[["ring_all"]]    <- near_to(1:3) & unprot_ocean
zones[["unprotected"]] <- unprot_ocean   # global reference (NOT coverage-controlled)

zone_area <- tibble(
  zone      = names(zones),
  ocean_km2 = map_dbl(zones, ~ global(.x, "sum", na.rm = TRUE)[[1]] * cell_km2)
)

cells <- cellFromXY(grid, st_coordinates(st_transform(slick_pts, EA)))
ok    <- !is.na(cells)
zv    <- as.data.frame(map(zones, ~ as.logical(terra::extract(.x, cells[ok])[[1]])))
zv[is.na(zv)] <- FALSE

slick_zone <- bind_cols(tibble(month = slicks$month[ok], area_km2 = slicks$area_km2[ok]), zv) |>
  pivot_longer(-c(month, area_km2), names_to = "zone", values_to = "hit") |>
  filter(hit) |>
  group_by(zone, month) |>
  summarise(n = n(), oil_km2 = sum(area_km2), .groups = "drop") |>
  complete(zone = zone_area$zone, month = unique(slicks$month),
           fill = list(n = 0L, oil_km2 = 0))

PER <- 1e4    # km2 of slick per 10,000 km2 of ocean per year
density_of <- function(d) {
  d |> group_by(zone) |>
    summarise(oil_km2 = sum(oil_km2), n = sum(n), .groups = "drop") |>
    left_join(zone_area, by = "zone") |>
    mutate(dens = oil_km2 / ocean_km2 / n_years * PER)
}
point <- density_of(slick_zone)

all_months <- unique(slick_zone$month)
boot <- map_dfr(seq_len(N_BOOT), function(b) {
  s <- tibble(month = sample(all_months, replace = TRUE))
  slick_zone |> inner_join(s, by = "month", relationship = "many-to-many") |>
    density_of() |> select(zone, dens) |> mutate(b = b)
})
boot_w <- boot |> pivot_wider(names_from = zone, values_from = dens)

ci <- function(x) quantile(x, c(.025, .975), na.rm = TRUE)
class_summary <- map_dfr(c(as.character(1:3), "all"), function(k) {
  i <- paste0("in_", k); r <- paste0("ring_", k)
  pi <- point |> filter(zone == i); pr <- point |> filter(zone == r)
  ratio_b <- boot_w[[i]] / boot_w[[r]]
  in_k <- if (k == "all") mpa_exposure else filter(mpa_exposure, prot_class == as.integer(k))
  tibble(
    class        = k,
    label        = if (k == "all") "All MPAs" else CLASS_LABELS[k],
    n_mpas       = nrow(in_k),
    n_mpas_oiled = sum(in_k$n_slicks > 0),
    ocean_in_km2 = pi$ocean_km2,  ocean_ring_km2 = pr$ocean_km2,
    slicks_in    = pi$n,          slicks_ring    = pr$n,
    dens_in      = pi$dens,       dens_ring      = pr$dens,
    dens_in_lo   = ci(boot_w[[i]])[1], dens_in_hi   = ci(boot_w[[i]])[2],
    dens_ring_lo = ci(boot_w[[r]])[1], dens_ring_hi = ci(boot_w[[r]])[2],
    ratio        = pi$dens / pr$dens,
    ratio_lo     = ci(ratio_b)[1], ratio_hi = ci(ratio_b)[2]
  )
})
saveRDS(class_summary, file.path(DIR_DATA, "class_summary.rds"))
print(class_summary |> select(label, n_mpas, n_mpas_oiled, dens_in, dens_ring, ratio, ratio_lo, ratio_hi))

# ---- 6. Slick points (for maps) and run info ---------------------------------
prot_at_slick <- rep(NA_integer_, length(cells))
prot_at_slick[ok] <- terra::extract(prot, cells[ok])[[1]]
xy <- st_coordinates(slick_pts)
saveRDS(tibble(id = slicks$id, lon = xy[, 1], lat = xy[, 2],
               area_km2 = slicks$area_km2, month = slicks$month,
               prot_class = prot_at_slick),
        file.path(DIR_DATA, "slick_points.rds"))

saveRDS(list(
  run_date      = Sys.Date(),
  start = START, end = END, n_months = length(all_months), n_years = n_years,
  slick_filter  = SLICK_FILTER,
  n_slicks      = nrow(slicks), slick_km2 = sum(slicks$area_km2),
  n_mpas        = nrow(mpa), res_km = RES_KM, ring_km = RING_KM, n_boot = N_BOOT,
  dens_unprotected = point$dens[point$zone == "unprotected"],
  class_labels  = CLASS_LABELS
), file.path(DIR_DATA, "run_info.rds"))

message("Done. Tables written to ", DIR_DATA, "/")
