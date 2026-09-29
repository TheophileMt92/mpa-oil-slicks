# LinkedIn images, rendered from the tables in data/ (run prep/build_data.R first).
# Output: outputs/ (gitignored)
#   fig_most_exposed_mpas.png   main post image (1600 x 1800 px, portrait)
#   fig_protection_classes.png  second image / carousel slide (1600 x 1500 px)

source("R/plots.R")
dir.create("outputs", showWarnings = FALSE)

exposure <- readRDS("data/mpa_exposure.rds")
cls      <- readRDS("data/class_summary.rds")
info     <- readRDS("data/run_info.rds")

ggsave("outputs/fig_most_exposed_mpas.png", plot_top_mpas(exposure, info),
       width = 8, height = 9, dpi = 200, bg = SURF)
ggsave("outputs/fig_protection_classes.png", plot_classes(cls, info),
       width = 8, height = 7.5, dpi = 200, bg = SURF)

# Numbers for the post text
top <- top_mpas(exposure)
cat(sprintf("\nMPAs with >= 1 slick: %d of %d (%.1f%%)\n",
            sum(exposure$n_slicks > 0), nrow(exposure),
            100 * mean(exposure$n_slicks > 0)))
print(cls[, c("label", "n_mpas", "n_mpas_oiled", "ratio", "ratio_lo", "ratio_hi")])
print(head(top[, c("NAME", "country", "class_label", "n_slicks", "oil_km2",
                   "oil_per_1000km2_yr")], 10))
