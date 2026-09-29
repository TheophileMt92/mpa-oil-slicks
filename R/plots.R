# Shared plotting helpers: used by index.qmd and linkedin_figures.R
# so the report and the LinkedIn images stay identical.

library(ggplot2)

INK   <- "#0b0b0b"; INK2 <- "#52514e"; MUTED <- "#898781"
GRIDC <- "#e1e0d9"; SURF <- "#fcfcfb"

# Protection classes: two blues (darker = stronger protection) + grey for unknown
CLASS_COLS <- c("1" = "#1c5cab", "2" = "#6da7ec", "3" = "#a8a69f", "all" = "#52514e")

# IUCN management categories: blues for I-III (no-take), greens for IV-VI
# (multiple-use), grey where no category is reported. Darker = stricter.
IUCN_LEVELS <- c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Not reported")
IUCN_COLS <- c(Ia = "#0d366b", Ib = "#1c5cab", II = "#3987e5", III = "#86b6ef",
               IV = "#0c6048", V = "#1b9a6c", VI = "#4cbf92",
               "Not reported" = "#b9b7ae")
iucn_group <- function(cat) {
  cat <- trimws(as.character(cat))
  factor(ifelse(cat %in% IUCN_LEVELS[1:7], cat, "Not reported"), levels = IUCN_LEVELS)
}

# Pollution levels (oil-slick km2 per 1,000 km2 of MPA per year)
POLL_COLS <- c("#e4e2db", "#f5a066", "#e5692e", "#b8420f", "#7a2408")
POLL_COLS_LIGHT <- POLL_COLS   # tables are always light, even in the dark report
LOLLI     <- c(seg = "#f5a066", pt = "#b8420f")
DARK      <- FALSE

# Dark "deep ocean" look matching the report stylesheet (template/custom.css).
# Call once before plotting; the LinkedIn script keeps the light look by default.
use_dark_theme <- function() {
  INK   <<- "#ffffff"; INK2 <<- "#9cb7c9"; MUTED <<- "#7f9aab"
  GRIDC <<- "#1c4a57"; SURF <<- "#052832"
  # on dark, more oil = brighter (validated ordinal ramp vs #052832)
  POLL_COLS <<- c("#16434f", "#a8380f", "#e0531f", "#ff8a52", "#ffc4a3")
  LOLLI <<- c(seg = "#a8380f", pt = "#ff6633")
  DARK  <<- TRUE
  invisible(TRUE)
}

# Text colour that stays readable on a pollution-level fill (level = 1..5)
heat_text <- function(level) {
  level <- as.integer(level)
  if (DARK) ifelse(level >= 4, "#00131f", "#ffffff") else ifelse(level >= 4, "#ffffff", INK)
}

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

# Tables stay light in both looks, so they stand out from the dark report page.
# Header: a pale tint of the report's teal.
TABLE_BG   <- "#fcfcfb"
TABLE_INK  <- "#0b0b0b"
TABLE_RULE <- "#dde6e9"
table_theme <- function() {
  reactable::reactableTheme(
    color = TABLE_INK, backgroundColor = TABLE_BG, borderColor = TABLE_RULE,
    stripedColor = "#f1f0ec", highlightColor = "#e3ecef",
    headerStyle = list(background = "#cfe0e6", color = "#052832", fontWeight = 700,
                       borderColor = "#b7cdd5", textTransform = "uppercase",
                       fontSize = "0.72rem", letterSpacing = "0.05em"),
    searchInputStyle = list(width = "100%", borderColor = TABLE_RULE,
                            backgroundColor = "white", color = TABLE_INK),
    paginationStyle = list(color = "#52514e", borderTop = paste("1px solid", TABLE_RULE)),
    pageButtonHoverStyle  = list(backgroundColor = "#e3ecef"),
    pageButtonActiveStyle = list(backgroundColor = "#cfe0e6"),
    style = list(fontSize = "0.88rem", fontFamily = "Inter, Arial, sans-serif",
                 borderRadius = "4px"))
}

# The WDPA often lists one site several times under different designations
# (e.g. a UK Marine Protected Area that is also an Emerald Network site), which
# would double-count its oil. Keep one row per country + name + area (2 s.f.).
dedupe_sites <- function(exposure) {
  key <- paste(exposure$country, tolower(trimws(exposure$NAME)),
               signif(exposure$marine_km2, 2), sep = "|")
  o <- order(key, -exposure$oil_km2, exposure$prot_class)   # keep the most exposed record
  keep <- o[!duplicated(key[o])]
  exposure[sort(keep), ]
}

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

# ---- Beeswarm of exposed MPAs --------------------------------------------------
# Every exposed MPA as a circle: x = oil density (log scale), size = MPA area.
# Circles are packed so they never overlap (sized beeswarm), which ggplot's
# point geoms cannot guarantee, so they are drawn as polygons in data units.

swarm_layout <- function(x, r) {
  # x, r in the same units. Greedy: place each circle (in x order) at the
  # smallest |y| that does not overlap anything already placed.
  o <- order(x); y <- rep(NA_real_, length(x)); placed <- integer(0)
  for (i in o) {
    cand <- 0
    near <- placed[abs(x[placed] - x[i]) < r[placed] + r[i]]
    if (length(near)) {
      dx <- x[near] - x[i]
      h  <- sqrt(pmax((r[near] + r[i])^2 - dx^2, 0))
      cand <- c(0, y[near] + h, y[near] - h)
      ok <- vapply(cand, function(cy) all((x[near] - x[i])^2 + (y[near] - cy)^2 >=
                                             (r[near] + r[i])^2 - 1e-9), logical(1))
      cand <- cand[ok]
    }
    y[i] <- cand[which.min(abs(cand))]
    placed <- c(placed, i)
  }
  y
}

circle_polys <- function(x, y, r, id, n = 40) {
  a <- seq(0, 2 * pi, length.out = n)
  data.frame(id = rep(id, each = n),
             px = rep(x, each = n) + rep(r, each = n) * cos(a),
             py = rep(y, each = n) + rep(r, each = n) * sin(a))
}

plot_swarm_mpas <- function(exposure, info, min_km2 = 100, min_slicks = 3,
                            n_label = 10, title = NULL,
                            width = 100, aspect = 0.62, fill_frac = 0.30) {
  d <- exposure[exposure$marine_km2 >= min_km2 & exposure$n_slicks >= min_slicks, ]
  d <- d[order(-d$oil_per_1000km2_yr), ]
  d$rank <- seq_len(nrow(d))
  d$hl   <- d$rank <= n_label

  # x on a log scale, stretched to `width` plot units so radii share the unit
  lx  <- log10(d$oil_per_1000km2_yr)
  rng <- range(lx)
  rng <- c(floor(rng[1] * 2) / 2, ceiling(rng[2] * 2) / 2)
  sx  <- function(v) (v - rng[1]) / diff(rng) * width
  d$x <- sx(lx)

  # radius ~ sqrt(area) (circle area proportional to MPA area, capped at the
  # 98th percentile), rescaled so all circles cover ~fill_frac of the swarm band
  height <- width * aspect
  a  <- pmin(d$marine_km2, quantile(d$marine_km2, 0.98))
  r0 <- pmax(sqrt(a / max(a)), 0.25)
  k  <- sqrt(fill_frac * width * height / sum(pi * r0^2))
  d$r <- r0 * k
  d$y <- swarm_layout(d$x, d$r)

  polys <- circle_polys(d$x, d$y, d$r, d$rank)
  d$iucn <- iucn_group(d$IUCN_CAT)
  polys <- merge(polys, d[, c("rank", "hl", "iucn")], by.x = "id", by.y = "rank")
  polys <- polys[order(polys$hl, polys$id), ]           # named MPAs drawn on top

  # Named MPAs: a tidy label column to the right, ordered by vertical position
  # so leader lines never cross
  ylim <- max(height / 2, max(abs(d$y) + d$r) * 1.05)
  lab  <- d[d$hl, ]
  lab  <- lab[order(-lab$y), ]
  nm   <- ifelse(nchar(lab$NAME) > 38, paste0(substr(lab$NAME, 1, 36), "…"), lab$NAME)
  lab$txt <- sprintf("%d. %s, %s", lab$rank, nm, lab$country)
  lab$ly  <- seq(ylim * 0.9, -ylim * 0.9, length.out = nrow(lab))
  lab_x   <- width + 6

  med_x <- sx(log10(median(d$oil_per_1000km2_yr)))
  brk   <- 10^seq(ceiling(rng[1]), floor(rng[2]))
  brk_lab <- prettyNum(as.character(brk), big.mark = ",")

  if (is.null(title)) {
    title <- sprintf("Oil slicks were detected inside %s of the world's %s marine protected areas",
                     format(sum(exposure$n_slicks > 0), big.mark = ","),
                     format(nrow(exposure), big.mark = ","))
  }
  ggplot() +
    geom_vline(xintercept = med_x, colour = MUTED, linewidth = 0.3, linetype = "22") +
    annotate("text", x = med_x, y = ylim, label = "Median MPA", colour = MUTED,
             size = 2.9, vjust = -0.3) +
    geom_polygon(data = polys[!polys$hl, ], aes(px, py, group = id, fill = iucn),
                 colour = SURF, linewidth = 0.2) +
    geom_polygon(data = polys[polys$hl, ], aes(px, py, group = id, fill = iucn),
                 colour = INK, linewidth = 0.45) +
    geom_segment(data = lab, aes(x = x + r, y = y, xend = lab_x - 1, yend = ly),
                 colour = INK2, linewidth = 0.25) +
    geom_text(data = lab, aes(x = lab_x, y = ly, label = txt),
              hjust = 0, size = 2.9, colour = INK) +
    scale_fill_manual(values = IUCN_COLS, drop = FALSE, name = "IUCN category") +
    scale_x_continuous(breaks = sx(log10(brk)), labels = brk_lab,
                       expand = expansion(0)) +
    scale_y_continuous(expand = expansion(0)) +
    coord_fixed(xlim = c(-2, width * 1.62), ylim = c(-ylim, ylim * 1.08), clip = "off") +
    guides(fill = guide_legend(nrow = 1, title.position = "left",
                               override.aes = list(colour = NA))) +
    labs(
      title = wrap_title(title),
      subtitle = sprintf(paste0(
        "Each circle is an MPA with oil detected inside it, %s-%s, sized by its area and\n",
        "coloured by IUCN category (blues: no-take I-III, greens: multiple-use IV-VI).\n",
        "The %d most exposed are outlined and named."),
        format(info$start, "%Y"), format(info$end, "%Y"), n_label),
      x = "Oil-slick area detected inside the MPA (km² per 1,000 km² per year, log scale)",
      y = NULL,
      caption = paste0(
        "MPAs of at least ", min_km2, " km² with ", min_slicks, "+ slicks matched to a nearby ",
        "vessel or platform (Cerulean collated score > 0).\n",
        "Detection depends on Sentinel-1 coverage: absence of slicks is not evidence of absence of oil.\n",
        CREDIT)
    ) +
    theme_oil() +
    theme(axis.text.y = element_blank(), panel.grid.major.x = element_blank(),
          axis.title.x = element_text(hjust = 0),
          legend.title = element_text(colour = INK2, size = rel(0.85)),
          legend.key.size = unit(0.9, "lines"),
          axis.ticks.x = element_line(colour = GRIDC, linewidth = 0.4))
}

# ---- Country dot grids ("calendar heatmap" style) -----------------------------
# One block per country, one square per MPA, coloured by pollution level.
# Countries ranked by total oil-slick area detected inside their MPAs.
poll_bins <- function(v) {
  pos <- v[v > 0]
  br  <- unique(signif(quantile(pos, c(0.25, 0.5, 0.75), names = FALSE), 1))
  cut(v, c(-Inf, 0, br, Inf), right = TRUE,
      labels = c("No slick detected",
                 paste0("< ", br[1]),
                 if (length(br) > 1) paste0(br[-length(br)], "-", br[-1]),
                 paste0("> ", br[length(br)])))
}

country_grid_data <- function(exposure, n_countries, ncol, min_km2, gap = 1.6,
                              max_rows = 3, note_cells = 18, rank_by = "oil_km2") {
  d <- exposure[exposure$marine_km2 >= min_km2 & !is.na(exposure$country), ]
  d$level <- poll_bins(d$oil_per_1000km2_yr)
  tot <- aggregate(cbind(oil_km2, oiled = n_slicks > 0, n = 1) ~ country, data = d, FUN = sum)
  tot$share <- tot$oiled / tot$n
  tot <- tot[order(-tot[[rank_by]], -tot$oil_km2), ][seq_len(min(n_countries, nrow(tot))), ]
  d <- d[d$country %in% tot$country, ]
  # Within a country: most polluted first, reading left to right, top to bottom
  d <- d[order(match(d$country, tot$country), -d$oil_per_1000km2_yr, -d$marine_km2), ]
  d$i <- ave(seq_len(nrow(d)), d$country, FUN = seq_along) - 1

  # Show at most `max_rows` rows per country. When a country has more MPAs than
  # fit, the end of the last row is replaced by a "+ N more" note.
  cap <- max_rows * ncol
  tot$hidden <- pmax(tot$n - cap, 0)
  tot$hidden[tot$hidden > 0] <- tot$n[tot$hidden > 0] - (cap - note_cells)
  shown <- ifelse(tot$hidden > 0, cap - note_cells, tot$n)
  d <- d[d$i < shown[match(d$country, tot$country)], ]
  shown_oiled  <- aggregate(list(k = d$n_slicks > 0), by = list(country = d$country), FUN = sum)
  hidden_oiled <- tot$oiled - shown_oiled$k[match(tot$country, shown_oiled$country)]
  tot$note <- ifelse(tot$hidden == 0, NA_character_,
                ifelse(hidden_oiled > 0,
                       sprintf("+ %s more MPAs (%s with oil)", fmt_int(tot$hidden),
                               fmt_int(hidden_oiled)),
                       sprintf("+ %s more MPAs, no slick detected", fmt_int(tot$hidden))))

  # Every block is `max_rows` tall so the three label lines never collide
  tot$rows <- max_rows
  tot$top  <- -c(0, cumsum(tot$rows + gap)[-nrow(tot)])   # y of each block's first row
  tot$note_x <- ncol - note_cells - 0.1
  tot$note_y <- tot$top - (max_rows - 1)
  d$x <- d$i %% ncol
  d$y <- tot$top[match(d$country, tot$country)] - d$i %/% ncol
  list(d = d, tot = tot, bottom = min(tot$top - tot$rows + 1))
}

fmt_int <- function(x) vapply(x, function(v) format(v, big.mark = ",", scientific = FALSE), character(1))

LABEL_W <- 16   # grid units reserved on the left for country labels

# rank_by: "oil_km2" (total slick area inside the country's MPAs),
#          "oiled" (number of MPAs with oil) or "share" (fraction of MPAs with oil)
plot_country_grid <- function(exposure, info, n_countries = 10, ncol = 50,
                              min_km2 = 1, rank_by = "oil_km2", title = NULL) {
  g <- country_grid_data(exposure, n_countries, ncol, min_km2, rank_by = rank_by)
  d <- g$d; tot <- g$tot
  tot$lab1 <- tot$country
  tot$lab2 <- sprintf("%s of %s MPAs with oil\n%s km² of slicks inside",
                      fmt_int(tot$oiled), fmt_int(tot$n), fmt_int(round(tot$oil_km2)))
  if (is.null(title)) title <- "Oil slicks inside marine protected areas, country by country"
  rank_txt <- switch(rank_by, oiled = "the most MPAs with oil detected inside",
                     share = "the highest share of MPAs with oil detected inside",
                     "the most oil-slick area detected inside their marine protected areas")
  cols <- setNames(POLL_COLS[seq_along(levels(d$level))], levels(d$level))

  ggplot(d) +
    geom_tile(aes(x, y, fill = level), width = 0.78, height = 0.78) +
    geom_text(data = tot, aes(x = -1.2, y = top + 0.4, label = lab1), hjust = 1, vjust = 1,
              fontface = "bold", size = 3.3, colour = INK) +
    geom_text(data = tot, aes(x = -1.2, y = top - 0.9, label = lab2), hjust = 1, vjust = 1,
              size = 2.6, colour = INK2, lineheight = 1.05) +
    geom_text(data = tot[!is.na(tot$note), ], aes(x = note_x, y = note_y, label = note),
              hjust = 0, vjust = 0.5, size = 2.6, colour = INK2, fontface = "italic") +
    scale_fill_manual(values = cols, drop = FALSE,
                      name = "Oil-slick km² per 1,000 km² of MPA per year") +
    coord_fixed(xlim = c(-LABEL_W, ncol - 0.5), ylim = c(g$bottom - 0.6, 0.6),
                expand = FALSE, clip = "off") +
    guides(fill = guide_legend(nrow = 1, title.position = "top")) +
    labs(
      title = wrap_title(title, 60),
      subtitle = paste0(
        wrap_title(sprintf("The %d countries with %s, %s-%s.", nrow(tot), rank_txt,
                           format(info$start, "%Y"), format(info$end, "%Y")), 115),
        "\nEach square is one MPA, coloured by how much oil was detected inside it, most exposed first."),
      x = NULL, y = NULL,
      caption = paste0(
        "Country: first ISO3 code of the MPA in the WDPA. MPAs smaller than ", min_km2,
        " km² not shown. Slicks matched to a nearby vessel or platform.\n",
        "Detection depends on Sentinel-1 coverage: absence of slicks is not evidence of absence of oil.\n",
        CREDIT)
    ) +
    theme_oil() +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.ticks = element_blank(), panel.grid = element_blank(),
          panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
          legend.title = element_text(colour = INK2, size = rel(0.8)),
          legend.key.size = unit(0.9, "lines"))
}

# Figure height (inches) that fits the grid at a given width without dead space
country_grid_height <- function(exposure, n_countries = 10, ncol = 50, min_km2 = 1,
                                rank_by = "oil_km2", width = 10) {
  g <- country_grid_data(exposure, n_countries, ncol, min_km2, rank_by = rank_by)
  panel_w <- width - 0.7                            # minus plot margins
  panel_h <- panel_w * (abs(g$bottom) + 1.2) / (ncol - 0.5 + LABEL_W)
  panel_h + 3.4                                     # title, legend and caption
}

# ---- Country ranking (dot plot) ------------------------------------------------
# Total oil-slick area detected inside each country's MPAs. Log scale, because
# one or two countries can sit an order of magnitude above the rest.
plot_country_ranking <- function(exposure, info, n_countries = 20, min_km2 = 1,
                                 title = NULL) {
  d <- exposure[exposure$marine_km2 >= min_km2 & !is.na(exposure$country), ]
  tot <- aggregate(cbind(oil_km2, oiled = n_slicks > 0, n = 1) ~ country, data = d, FUN = sum)
  tot <- tot[tot$oil_km2 > 0, ]
  tot <- tot[order(-tot$oil_km2), ][seq_len(min(n_countries, nrow(tot))), ]
  tot$country <- factor(tot$country, levels = rev(tot$country))
  tot$val <- ifelse(tot$oil_km2 >= 10, fmt_int(round(tot$oil_km2)),
                    format(signif(tot$oil_km2, 2), drop0trailing = TRUE))
  tot$mpa_txt <- sprintf("%s of %s MPAs", fmt_int(tot$oiled), fmt_int(tot$n))

  lo <- 10^floor(log10(min(tot$oil_km2)))
  hi <- 10^ceiling(log10(max(tot$oil_km2)))
  brk <- 10^seq(log10(lo), log10(hi))
  if (sum(brk <= max(tot$oil_km2)) < 3)             # narrow range: add 2x and 5x ticks
    brk <- sort(as.vector(outer(c(1, 2, 5), brk)))
  brk <- brk[brk >= lo & brk <= max(tot$oil_km2) * 1.5]  # keep room for the MPA column
  if (is.null(title)) title <- "Oil slicks detected inside marine protected areas, by country"

  ggplot(tot, aes(y = country)) +
    geom_segment(aes(x = lo, xend = oil_km2, yend = country), colour = LOLLI[["seg"]],
                 linewidth = 0.9, lineend = "round") +
    geom_point(aes(x = oil_km2), shape = 21, size = 3.6, stroke = 1, fill = LOLLI[["pt"]],
               colour = SURF) +
    geom_text(aes(x = oil_km2, label = val), hjust = 0, nudge_x = 0.08, size = 3, colour = INK) +
    geom_text(aes(x = hi * 4, label = mpa_txt), hjust = 1, size = 2.8, colour = INK2) +
    annotate("text", x = hi * 4, y = nrow(tot) + 0.9, label = "MPAs with oil",
             hjust = 1, size = 2.8, colour = MUTED, fontface = "bold") +
    scale_x_log10(breaks = brk, labels = fmt_int(brk), expand = expansion(0)) +
    coord_cartesian(xlim = c(lo, hi * 4.2), ylim = c(0.5, nrow(tot) + 1.2), clip = "off") +
    labs(
      title = wrap_title(title),
      subtitle = sprintf(paste0(
        "Total area of satellite-detected oil slicks inside each country's marine protected\n",
        "areas, km² summed over all detections %s-%s (log scale). The %d highest countries."),
        format(info$start, "%Y"), format(info$end, "%Y"), nrow(tot)),
      x = "Oil-slick area inside MPAs (km², log scale)", y = NULL,
      caption = paste0(
        "Slick area clipped to MPA boundaries; slicks matched to a nearby vessel or platform. ",
        "Country: first ISO3 code of the MPA in the WDPA.\n",
        "Detection depends on Sentinel-1 coverage: absence of slicks is not evidence of absence of oil.\n",
        CREDIT)
    ) +
    theme_oil() +
    theme(axis.text.y = element_text(size = 10))
}

# ---- Dark "glow" map of slick density (SkyTruth-style) --------------------------
plot_oil_map <- function(pts, exposure, info, n_label = 10, min_km2 = 100,
                         min_slicks = 3, cell_km = 50) {
  EA <- "EPSG:8857"
  BG <- "#161616"; LAND <- "#2f2f2f"
  world <- sf::st_transform(rnaturalearth::ne_countries(scale = 50, returnclass = "sf"), EA)
  world <- sf::st_make_valid(world)

  # Slick area summed on an equal-area grid (cell_km x cell_km)
  xy <- sf::st_coordinates(sf::st_transform(
          sf::st_as_sf(pts, coords = c("lon", "lat"), crs = 4326), EA))
  cs <- cell_km * 1000
  g  <- aggregate(list(oil = pts$area_km2),
                  by = list(x = (floor(xy[, 1] / cs) + 0.5) * cs,
                            y = (floor(xy[, 2] / cs) + 0.5) * cs), FUN = sum)
  g  <- g[order(g$oil), ]                      # brightest cells drawn last

  # Most exposed MPAs, numbered as in the beeswarm
  top <- exposure[exposure$marine_km2 >= min_km2 & exposure$n_slicks >= min_slicks, ]
  top <- top[order(-top$oil_per_1000km2_yr), ][seq_len(min(n_label, nrow(top))), ]
  top$rank <- seq_len(nrow(top))
  top_sf <- sf::st_transform(sf::st_as_sf(top, coords = c("lon", "lat"), crs = 4326), EA)
  top_xy <- sf::st_coordinates(top_sf)
  top$x <- top_xy[, 1]; top$y <- top_xy[, 2]

  ggplot() +
    geom_sf(data = world, fill = LAND, colour = NA) +
    geom_tile(data = g, aes(x, y, fill = oil), width = cs, height = cs) +
    geom_point(data = top, aes(x, y), shape = 21, size = 5, stroke = 0.6,
               colour = "white", fill = NA) +
    ggrepel::geom_text_repel(data = top, aes(x, y, label = rank), colour = "white",
                             size = 3, fontface = "bold", point.size = 5,
                             box.padding = 0.35, min.segment.length = 0.3,
                             segment.colour = "white", segment.size = 0.25, seed = 1) +
    scale_fill_gradientn(
      colours = c("#4a1500", "#9a3412", "#ea580c", "#fb923c", "#fed7aa", "#fffbeb"),
      trans = "log10",
      labels = function(x) format(x, big.mark = ",", drop0trailing = TRUE, scientific = FALSE, trim = TRUE),
      name = sprintf("Oil-slick area per %d × %d km cell (km², %s-%s)",
                     cell_km, cell_km, format(info$start, "%Y"), format(info$end, "%Y")),
      guide = guide_colourbar(title.position = "top", barwidth = unit(16, "lines"),
                              barheight = unit(0.45, "lines"))) +
    coord_sf(crs = EA, datum = NA, expand = FALSE,
             ylim = c(-6.2e6, 8.2e6)) +
    labs(
      title = "Where the oil is",
      subtitle = sprintf(paste0(
        "Satellite-detected oil slicks, %s-%s. Numbered rings: the %d most exposed marine\n",
        "protected areas."), format(info$start, "%Y"), format(info$end, "%Y"), nrow(top)),
      caption = CREDIT) +
    theme_void(base_size = 12) +
    theme(
      plot.background  = element_rect(fill = BG, colour = NA),
      panel.background = element_rect(fill = BG, colour = NA),
      plot.title    = element_text(colour = "white", face = "bold", size = rel(1.45)),
      plot.subtitle = element_text(colour = "#bdbdbd", size = rel(0.88), lineheight = 1.2,
                                   margin = ggplot2::margin(t = 4, b = 10)),
      plot.caption  = element_text(colour = "#8a8a8a", size = rel(0.66), hjust = 0,
                                   lineheight = 1.2, margin = ggplot2::margin(t = 10)),
      plot.title.position = "plot", plot.caption.position = "plot",
      legend.position = "bottom", legend.justification = "left",
      legend.title = element_text(colour = "#bdbdbd", size = rel(0.75)),
      legend.text  = element_text(colour = "#8a8a8a", size = rel(0.7)),
      plot.margin  = ggplot2::margin(20, 20, 14, 20)
    )
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
