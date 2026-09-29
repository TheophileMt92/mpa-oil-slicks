# LinkedIn images, rendered from the tables in data/ (run prep/build_data.R first).
# Output: outputs/ (gitignored)
#   fig_most_exposed_mpas.png   main post image: beeswarm of exposed MPAs (2000 x 1600 px)
#   fig_oil_map.png             dark map of slick density + top MPAs (2000 x 1240 px)
#   fig_country_grid.png        one block per country, one square per MPA (top N_COUNTRIES)
#   fig_country_ranking.png     total oil inside MPAs, top 20 countries (1600 x 1600 px)
#   fig_protection_classes.png  protection level vs surrounding ocean (1600 x 1500 px)

need <- c("ggplot2", "ggrepel", "sf", "scales", "rnaturalearth", "rnaturalearthdata")
new  <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")

source("R/plots.R")
dir.create("outputs", showWarnings = FALSE)

exposure <- readRDS("data/mpa_exposure.rds")
cls      <- readRDS("data/class_summary.rds")
info     <- readRDS("data/run_info.rds")

pts      <- readRDS("data/slick_points.rds")

ggsave("outputs/fig_most_exposed_mpas.png", plot_swarm_mpas(exposure, info),
       width = 10, height = 8, dpi = 200, bg = SURF)
ggsave("outputs/fig_oil_map.png", plot_oil_map(pts, exposure, info),
       width = 10, height = 6.2, dpi = 200, bg = "#161616")
N_COUNTRIES <- 8           # fits a portrait LinkedIn image; report shows 20
RANK_BY     <- "oil_km2"   # "oil_km2" (total slick area), "oiled" (n MPAs with oil), "share"
MIN_KM2     <- 1           # hide MPAs smaller than this (km2)
ggsave("outputs/fig_country_grid.png",
       plot_country_grid(exposure, info, N_COUNTRIES, min_km2 = MIN_KM2, rank_by = RANK_BY),
       width = 10, height = country_grid_height(exposure, N_COUNTRIES, min_km2 = MIN_KM2,
                                                rank_by = RANK_BY),
       dpi = 200, bg = SURF)
ggsave("outputs/fig_country_ranking.png", plot_country_ranking(exposure, info, 20),
       width = 8, height = 8, dpi = 200, bg = SURF)
ggsave("outputs/fig_protection_classes.png", plot_classes(cls, info),
       width = 8, height = 7.5, dpi = 200, bg = SURF)

# Numbers for the post text
top <- top_mpas(exposure)
cat(sprintf("\nMPAs with >= 1 slick: %d of %d (%.1f%%)\n",
            sum(exposure$n_slicks > 0), nrow(exposure),
            100 * mean(exposure$n_slicks > 0)))
print(cls[, c("label", "n_mpas", "n_mpas_oiled", "ratio", "ratio_lo", "ratio_hi")])
# Where the country ranking comes from: biggest contributors to each country's total
cat("\nLargest oil totals inside single MPAs:\n")
print(head(exposure[order(-exposure$oil_km2),
                    c("NAME", "country", "DESIG_ENG", "marine_km2", "n_slicks", "oil_km2")], 10))
print(head(top[, c("NAME", "country", "class_label", "n_slicks", "oil_km2",
                   "oil_per_1000km2_yr")], 10))
