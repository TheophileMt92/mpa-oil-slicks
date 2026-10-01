# =============================================================================
# Extract one region (default: North Sea & NW Europe) for the animation:
#   raw/region_mpas.gpkg     MPA polygons intersecting the region (simplified)
#   raw/region_slicks.gpkg   every slick in the region, with its timestamp
# Both stay in raw/ (gitignored): WDPA terms do not allow redistributing
# its geometries.
#
# Needs the caches written by prep/build_data.R (raw/mpa_marine.gpkg and
# raw/slicks/*.gpkg). Run from the project root:  Rscript prep/extract_region.R
# Takes a minute or two.
# =============================================================================

Sys.unsetenv(c("PROJ_LIB", "PROJ_DATA", "GDAL_DATA"))   # avoid conda's old PROJ
suppressPackageStartupMessages({ library(sf); library(dplyr) })
sf_use_s2(FALSE)

BBOX <- c(xmin = -6, ymin = 48, xmax = 16, ymax = 62)  # Channel, North Sea, Skagerrak
LAEA <- 3035                                            # equal-area Europe

wkt <- st_as_text(st_as_sfc(st_bbox(BBOX, crs = 4326)))

# ---- MPAs ---------------------------------------------------------------------
mpa <- st_read("raw/mpa_marine.gpkg", wkt_filter = wkt, quiet = TRUE)
mpa <- mpa |>
  st_make_valid() |>
  st_transform(LAEA) |>
  st_simplify(dTolerance = 150, preserveTopology = TRUE) |>
  st_make_valid()
# Same site listed under several designations: keep one (as dedupe_sites())
key <- paste(tolower(trimws(mpa$NAME)), signif(as.numeric(st_area(mpa)) / 1e6, 2))
mpa <- mpa[!duplicated(key), c("WDPAID", "NAME", "IUCN_CAT", "ISO3", "marine_km2")]
st_write(st_transform(mpa, 4326), "raw/region_mpas.gpkg", delete_dsn = TRUE, quiet = TRUE)
message(sprintf("MPAs: %d", nrow(mpa)))

# ---- Slicks -------------------------------------------------------------------
files  <- sort(list.files("raw/slicks", pattern = "\\.gpkg$", full.names = TRUE))
slicks <- do.call(rbind, lapply(files, function(f) {
  x <- st_read(f, wkt_filter = wkt, quiet = TRUE)
  if (nrow(x) == 0) return(NULL)
  st_sf(id = as.integer(x$id), slick_timestamp = as.character(x$slick_timestamp),
        area_km2 = as.numeric(x$area) / 1e6, geometry = st_geometry(x))
}))
slicks <- slicks[!duplicated(slicks$id), ]
st_write(slicks, "raw/region_slicks.gpkg", delete_dsn = TRUE, quiet = TRUE)
message(sprintf("Slicks: %d", nrow(slicks)))
