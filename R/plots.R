# Shared plotting helpers: used by index.qmd and linkedin_figures.R
# so the report and the LinkedIn images stay identical.

library(ggplot2)

INK   <- "#0b0b0b"; INK2 <- "#52514e"; MUTED <- "#898781"
GRIDC <- "#e1e0d9"; SURF <- "#fcfcfb"

# Protection classes: two blues (darker = stronger protection) + grey for unknown
CLASS_COLS <- c("1" = "#1c5cab", "2" = "#6da7ec", "3" = "#a8a69f", "all" = "#52514e")

CREDIT <- paste0(
  "Data: SkyTruth Cerulean (Sentinel-1 SAR) · WDPA (UNEP-WCMC & IUCN) · Natural Earth\n",
  "Analysis: Théophile Mouton · DataSphere Analytics")

theme_oil <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background    = element_rect(fill = SURF, colour = NA),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = GRIDC, linewidth = 0.3),
      axis.text.y        = element_text(colour = INK, hjust = 0, lineheight = 1.1),
      axis.text.x        = element_text(colour = MUTED),
      axis.title.x       = element_text(colour = INK2, size = rel(0.85), hjust = 0,
                                        margin = ggplot2::margin(t = 8)),
      legend.position    = "top",
      legend.justification = "left",
      legend.text        = element_text(colour = INK2, size = rel(0.85)),
      legend.title       = element_blank(),
      legend.margin      = ggplot2::margin(0, 0, 4, 0),
      plot.title         = element_text(colour = INK, face = "bold", size = rel(1.45),
                                        lineheight = 1.1),
      plot.subtitle      = element_text(colour = INK2, size = rel(0.88), lineheight = 1.2,
                                        margin = ggplot2::margin(t = 4, b = 10)),
      plot.caption       = element_text(colour = MUTED, size = rel(0.66), hjust = 0,
                                        lineheight = 1.2, margin = ggplot2::margin(t = 14)),
      plot.title.position = "plot", plot.caption.position = "plot",
      plot.margin        = ggplot2::margin(24, 24, 16, 24)
    ) +
    legend_left()
}

# ggplot2 >= 3.5 can align the legend with the title instead of the panel
NEW_GG <- utils::packageVersion("ggplot2") >= "3.5.0"
legend_left <- function() if (NEW_GG) theme(legend.location = "plot") else theme()

wrap_title <- function(x, width = 52) paste(strwrap(x, width), collapse = "\n")

# ---- Most exposed MPAs --------------------------------------------------------
# exposure: data/mpa_exposure.rds ; info: data/run_info.rds
top_mpas <- function(exposure, n = 20, min_km2 = 100, min_slicks = 3) {
  d <- exposure[exposure$marine_km2 >= min_km2 & exposure$n_slicks >= min_slicks, ]
  d <- d[order(-d$oil_per_1000km2_yr), ][seq_len(min(n, nrow(d))), ]
  nm <- ifelse(nchar(d$NAME) > 42, paste0(substr(d$NAME, 1, 40), "…"), d$NAME)
  d$y_lab <- sprintf("%s  (%s)", nm, d$country)
  d$y_lab <- factor(d$y_lab, levels = rev(unique(d$y_lab)))
  d
}

plot_top_mpas <- function(exposure, info, n = 20, min_km2 = 100, min_slicks = 3,
                          title = NULL) {
  d <- top_mpas(exposure, n, min_km2, min_slicks)
  n_oiled <- sum(exposure$n_slicks > 0)
  if (is.null(title)) {
    title <- sprintf("Oil slicks were detected inside %s of the world's %s marine protected areas",
                     format(n_oiled, big.mark = ","), format(nrow(exposure), big.mark = ","))
  }
  x_max <- max(d$oil_per_1000km2_yr)
  ggplot(d, aes(y = y_lab)) +
    geom_segment(aes(x = 0, xend = oil_per_1000km2_yr, yend = y_lab,
                     colour = as.character(prot_class)), linewidth = 1.1,
                 lineend = "round", show.legend = FALSE) +
    geom_point(aes(x = oil_per_1000km2_yr, fill = as.character(prot_class)),
               shape = 21, size = 3.6, stroke = 1, colour = SURF) +
    geom_text(aes(x = oil_per_1000km2_yr, label = sprintf("%d slicks", n_slicks)),
              hjust = -0.25, size = 3, colour = INK2) +
    scale_colour_manual(values = CLASS_COLS, labels = info$class_labels,
                        breaks = names(info$class_labels), drop = FALSE,
                        aesthetics = c("colour", "fill")) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.16))) +
    coord_cartesian(clip = "off") +
    labs(
      title = wrap_title(title),
      subtitle = sprintf(paste0(
        "The %d most exposed MPAs of at least %s km², ranked by oil-slick area detected inside\n",
        "them per 1,000 km² per year, %s-%s."),
        nrow(d), format(min_km2, big.mark = ","), format(info$start, "%Y"), format(info$end, "%Y")),
      x = "Oil-slick area (km² per 1,000 km² of MPA per year)", y = NULL,
      caption = paste0(
        "Satellite-detected slicks matched to a nearby vessel or platform (Cerulean collated score > 0). ",
        "Minimum ", min_slicks, " slicks per MPA.\n",
        "Detection depends on Sentinel-1 coverage: absence of slicks is not evidence of absence of oil.\n",
        CREDIT)
    ) +
    guides(colour = guide_legend(override.aes = list(size = 4),
                                 nrow = if (NEW_GG) 1 else 3)) +
    theme_oil() +
    theme(axis.text.y = element_text(size = 9.5),
          legend.position = if (NEW_GG) "top" else "bottom")
}

# ---- Protection classes vs surrounding ocean ----------------------------------
fmt_ratio <- function(r, lo, hi) {
  lab <- if (lo > 1 & hi > 1) sprintf("%.1fx more oil", r)
         else if (hi < 1)     sprintf("%.0f%% less oil", (1 - r) * 100)
         else                 "no clear difference"
  sprintf("%s\nratio %.2f [%.2f-%.2f]", lab, r, lo, hi)
}

plot_classes <- function(cls, info, title = NULL) {
  cls$y_lab <- sprintf("%s\n%s MPAs · %s with slicks", cls$label,
                       trimws(format(cls$n_mpas, big.mark = ",")),
                       trimws(format(cls$n_mpas_oiled, big.mark = ",")))
  cls$y_lab <- factor(cls$y_lab, levels = rev(cls$y_lab))
  cls$ratio_txt <- mapply(fmt_ratio, cls$ratio, cls$ratio_lo, cls$ratio_hi)
  ring_lab <- sprintf("Unprotected ocean within %d km", info$ring_km)
  pts <- rbind(
    data.frame(y = as.numeric(cls$y_lab) + 0.13, class = cls$class, series = "Inside the MPAs",
               x = cls$dens_in, lo = cls$dens_in_lo, hi = cls$dens_in_hi),
    data.frame(y = as.numeric(cls$y_lab) - 0.13, class = "ring", series = ring_lab,
               x = cls$dens_ring, lo = cls$dens_ring_lo, hi = cls$dens_ring_hi))
  pts$series <- factor(pts$series, levels = c("Inside the MPAs", ring_lab))
  x_max <- max(pts$hi, na.rm = TRUE)
  if (is.null(title)) {
    a <- cls[cls$class == "all", ]
    title <- if (a$ratio_lo > 1) {
      sprintf("Marine protected areas see %.1fx more oil than the waters around them", a$ratio)
    } else if (a$ratio_hi < 1) {
      sprintf("Protection cuts oil exposure by %.0f%%, but does not keep it out", (1 - a$ratio) * 100)
    } else "Protection status makes no clear difference to oil exposure"
  }
  cols <- c(CLASS_COLS, ring = SURF)
  ggplot(pts) +
    geom_linerange(aes(y = y, xmin = lo, xmax = hi,
                       colour = ifelse(class == "ring", "ringline", class)), linewidth = 0.7) +
    geom_point(aes(y = y, x = x, shape = series, fill = class,
                   colour = ifelse(class == "ring", "ringline", "halo")),
               size = 4.2, stroke = 1.1) +
    geom_text(data = cls, aes(y = as.numeric(y_lab), x = x_max * 1.08, label = ratio_txt),
              hjust = 0, size = 3.2, lineheight = 1.05, colour = INK2) +
    scale_shape_manual(values = c(21, 21)) +
    scale_fill_manual(values = cols, guide = "none") +
    scale_colour_manual(values = c(CLASS_COLS, ringline = MUTED, halo = SURF), guide = "none") +
    guides(shape = guide_legend(override.aes = list(
      fill = c(CLASS_COLS[["all"]], SURF), colour = c(SURF, MUTED), size = 4))) +
    scale_y_continuous(breaks = seq_along(levels(cls$y_lab)), labels = levels(cls$y_lab),
                       expand = expansion(add = 0.5)) +
    scale_x_continuous(breaks = pretty(c(0, x_max), 5), expand = expansion(mult = c(0, 0))) +
    coord_cartesian(xlim = c(0, x_max * 1.75), clip = "off") +
    labs(
      title = wrap_title(title),
      subtitle = sprintf(paste0(
        "Oil-slick area per 10,000 km² of ocean per year, %s-%s. Each class is compared with\n",
        "the unprotected ocean around it, which shares its satellite coverage and shipping context."),
        format(info$start, "%Y"), format(info$end, "%Y")),
      x = "Oil-slick density (km² per 10,000 km² per year)", y = NULL,
      caption = paste0(
        "Lines and [brackets]: 95% CI (bootstrap over months). Ratio = inside / outside. ",
        "Overlapping MPAs take their strongest class.\n", CREDIT)
    ) +
    theme_oil()
}
