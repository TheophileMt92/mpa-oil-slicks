# Figures for the README and LinkedIn, in the same dark style as the report.
# Rendered from the tables in data/ (run prep/build_data.R first).
# Output: figures/ (committed, shown in the README)
#   fig_country_grid.png      LinkedIn image: one block per country, one square per MPA
#                             (top N_COUNTRIES, portrait, 2000 px wide)
#   fig_country_ranking.png   total oil-slick area inside MPAs, top 20 countries

need <- c("ggplot2", "sf", "scales")
new  <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")

source("R/plots.R")
use_dark_theme()                 # same look as the report
dir.create("figures", showWarnings = FALSE)

exposure <- dedupe_sites(readRDS("data/mpa_exposure.rds"))
cls      <- readRDS("data/class_summary.rds")
info     <- readRDS("data/run_info.rds")

N_COUNTRIES <- 10          # fits a LinkedIn image (about square); the report shows 20
RANK_BY     <- "oil_km2"   # "oil_km2" (total slick area), "oiled" (n MPAs with oil), "share"
MIN_KM2     <- 1           # hide MPAs smaller than this (km2)

ggsave("figures/fig_country_grid.png",
       plot_country_grid(exposure, info, N_COUNTRIES, min_km2 = MIN_KM2, rank_by = RANK_BY),
       width = 10, height = country_grid_height(exposure, N_COUNTRIES, min_km2 = MIN_KM2,
                                                rank_by = RANK_BY),
       dpi = 200, bg = SURF)
ggsave("figures/fig_country_ranking.png", plot_country_ranking(exposure, info, 20),
       width = 8, height = 8, dpi = 200, bg = SURF)

# ---- Numbers for the post text ------------------------------------------------
d   <- exposure[exposure$marine_km2 >= MIN_KM2 & !is.na(exposure$country), ]
tot <- aggregate(cbind(oil_km2, oiled = n_slicks > 0, n = 1) ~ country, data = d, FUN = sum)
tot <- tot[order(-tot$oil_km2), ]
cat(sprintf("\nMPAs with >= 1 slick: %d of %d (%.1f%%)\n",
            sum(exposure$n_slicks > 0), nrow(exposure), 100 * mean(exposure$n_slicks > 0)))
cat(sprintf("Top %d countries: %d of %d MPAs with oil (%.1f%%)\n", N_COUNTRIES,
            sum(tot$oiled[1:N_COUNTRIES]), sum(tot$n[1:N_COUNTRIES]),
            100 * sum(tot$oiled[1:N_COUNTRIES]) / sum(tot$n[1:N_COUNTRIES])))
cat("\nCountries:\n");  print(head(tot, 20))
cat("\nProtection levels (inside / around ratio):\n")
print(cls[, c("label", "n_mpas", "n_mpas_oiled", "ratio", "ratio_lo", "ratio_hi")])
cat("\nLargest oil totals inside single MPAs:\n")
print(head(exposure[order(-exposure$oil_km2),
                    c("NAME", "country", "DESIG_ENG", "marine_km2", "n_slicks", "oil_km2")], 10))
