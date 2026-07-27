## script to contain functions for visualizing data




## prefered graphing theme
my.theme = theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(),
                 axis.line = element_line(colour = "black"),
                 axis.title.x=element_text(size=15),
                 axis.title.y=element_text(size=15),
                 axis.text=element_text(size=15),
                 plot.title = element_text(size=15),
                 legend.title=element_text(size=15),
                 legend.text=element_text(size=15))


## function to visualize diffs between ROV - diver abundance data
visualize.abundance.pairs <- function(x_axis, y_axis, colname, axis_limit = 10,
                            x_label = deparse(substitute(x_axis)),
                            y_label = deparse(substitute(y_axis))) {

  combined_df <- data.frame(
    x = x_axis[[colname]],
    y = y_axis[[colname]]
  )

  ggplot(combined_df, aes(x = x, y = y)) +
    geom_point(color = "black", size=2) +
    geom_abline(slope = 1, intercept = 0, color = "gray40", linetype = "solid") +
    coord_fixed(ratio = 1, xlim = c(0, axis_limit), ylim = c(0, axis_limit)) +
    labs(x = x_label, y = y_label, title = colname) +
    my.theme
}


## build the long-form ROV-diver head-to-head comparison data ~~~~~~~~~~~~~~~~~
## `pairs` is a data frame with one row per head-to-head comparison (see
## head_to_head_pairs in data_visualization.R), giving the display category
## label, the matching ROV column name, and the matching diver column name.
## Each pair is joined on site/transect/season independently (rather than one
## big join across all pairs at once) so that columns which happen to share a
## name across the ROV and diver dataframes -- e.g. both have their own
## "combined_red_algae" -- never collide or get silently suffixed.
## `rov_scale` rescales the ROV values (stored as 0-1 proportions) up to the
## same 0-100 percentage scale as the diver UPC data, so the two axes and the
## 1:1 reference line are directly comparable.
build.head.to.head.data <- function(rov_df, diver_df, pairs, rov_scale = 100) {
  purrr::map_dfr(seq_len(nrow(pairs)), function(i) {
    rov_slim <- rov_df %>%
      select(site, transect, season, y = all_of(pairs$rov_col[i])) %>%
      mutate(y = y * rov_scale)
    diver_slim <- diver_df %>%
      select(site, transect, season, x = all_of(pairs$diver_col[i]))

    inner_join(diver_slim, rov_slim, by = c("site", "transect", "season")) %>%
      mutate(category = pairs$category[i], .before = 1)
  })
}


## read Zooniverse labelset colors, keyed by short_label_code, as a numeric
## RGB matrix (0-255)
get.zooniverse.rgb <- function(json_path) {
  labelset <- jsonlite::fromJSON(json_path)
  rgb_mat <- do.call(rbind, labelset$color)[, 1:3, drop = FALSE]
  rownames(rgb_mat) <- labelset$short_label_code
  rgb_mat
}


## build a hex color lookup for our head-to-head categories from the
## Zooniverse labelset. Categories that map onto a single Zooniverse code
## (`code_map`, a named vector of category -> short_label_code) get that
## code's color directly. Categories built by combining several ROV columns
## with no single Zooniverse code of their own (`combo_map`, a named list of
## category -> character vector of short_label_codes) get the average RGB of
## their constituent codes -- a best guess at a representative color, not an
## authoritative one.
get.category.colors <- function(json_path, code_map, combo_map = list()) {
  rgb_mat <- get.zooniverse.rgb(json_path)

  direct <- setNames(
    rgb(rgb_mat[code_map, 1], rgb_mat[code_map, 2], rgb_mat[code_map, 3],
        maxColorValue = 255),
    names(code_map)
  )

  combined <- vapply(combo_map, function(codes) {
    avg <- colMeans(rgb_mat[codes, , drop = FALSE])
    rgb(avg[1], avg[2], avg[3], maxColorValue = 255)
  }, character(1))

  c(direct, combined)
}


## visualize ROV vs. diver head-to-head percent-cover comparisons (one point
## per site/transect/season, faceted by category when more than one category
## is present in `data`)
visualize.head.to.head <- function(data, colors, axis_limit = 100,
                                   x_label = "diver UPC percent-cover",
                                   y_label = "ROV percent-cover") {
  p <- ggplot(data, aes(x = x, y = y, color = category)) +
    geom_point(size = 2) +
    geom_abline(slope = 1, intercept = 0, color = "black") +
    coord_fixed(ratio = 1, xlim = c(0, axis_limit), ylim = c(0, axis_limit)) +
    scale_color_manual(values = colors) +
    labs(x = x_label, y = y_label, color = "category") +
    my.theme

  if (length(unique(data$category)) > 1) {
    p <- p + facet_wrap(~ category) + guides(color = "none")
  }

  p
}


## compute each photo's approximate distance (m) from its transect's first
## captured photo, using a local equirectangular approximation of GPS
## coordinates (adequate given transects span only ~30m; not appropriate at
## larger scales). Two ROV passes are run per 30m transect -- an outbound
## pass down one side of the meter tape and a return pass down the other --
## so ordering all photos (both passes) by this distance naturally
## interleaves them by physical position along the tape (roughly two points
## per meter mark) rather than needing to explicitly stitch the two passes
## together.
add.transect.distance <- function(df, group_cols = c("site", "transect", "season")) {
  df %>%
    group_by(across(all_of(group_cols))) %>%
    arrange(Time, .by_group = TRUE) %>%
    mutate(
      lat0 = dplyr::first(Latitude),
      lon0 = dplyr::first(Longitude),
      dx = (Longitude - lon0) * 111320 * cos(lat0 * pi / 180),
      dy = (Latitude - lat0) * 111320,
      distance_m = sqrt(dx^2 + dy^2)
    ) %>%
    ungroup() %>%
    select(-lat0, -lon0, -dx, -dy)
}


## visualize a single percent-cover category across photos/space (both ROV
## passes interleaved by distance along the transect) for one site x season,
## faceted by transect in the given order
visualize.photo.level <- function(data, category, transect_order,
                                  color = "black", ncol = 3,
                                  x_label = "distance along transect (m)",
                                  y_label = category) {
  plot_data <- data %>%
    mutate(transect = factor(transect, levels = transect_order)) %>%
    arrange(transect, distance_m)

  ggplot(plot_data, aes(x = distance_m, y = .data[[category]])) +
    geom_line(color = color) +
    geom_point(color = color, size = 2) +
    facet_wrap(~ transect, ncol = ncol) +
    labs(x = x_label, y = y_label) +
    my.theme
}


## kernel density of a single percent-cover category's per-photo values,
## overlaid by transect (one curve per transect, all photos -- both ROV
## passes -- from a single site x season). Unlike visualize.photo.level()
## above, this isn't about *where* along the tape cover occurs, just the
## overall distribution of values within each transect. Density is bounded to
## [0, 1] (the only possible range for a proportion) via geom_density()'s
## `bounds` argument, so the KDE doesn't leak probability mass past the edges
## of the data -- these categories are typically heavily zero-inflated, so
## expect a sharp peak at/near 0 regardless.
visualize.category.density.by.transect <- function(data, category, colors,
                                                    transect_order = 1:6,
                                                    x_label = "proportion cover",
                                                    y_label = "density") {
  plot_data <- data %>%
    mutate(transect = factor(transect, levels = transect_order))

  ggplot(plot_data, aes(x = .data[[category]], color = transect, fill = transect)) +
    geom_density(alpha = 0.15, linewidth = 1, bounds = c(0, 1)) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    coord_cartesian(xlim = c(0, 1)) +
    labs(x = x_label, y = y_label, color = "transect", fill = "transect") +
    my.theme
}


## same as visualize.category.density.by.transect() above, but pooling the
## three deep (transects 1-3) and three shallow (4-6) transects into two
## curves instead of six -- uses the existing `depth` column (added upstream
## by add.depth()) rather than re-deriving it from transect number
visualize.category.density.by.depth <- function(data, category, colors,
                                                 x_label = "proportion cover",
                                                 y_label = "density") {
  plot_data <- data %>%
    mutate(depth = factor(depth, levels = c("deep", "shallow")))

  ggplot(plot_data, aes(x = .data[[category]], color = depth, fill = depth)) +
    geom_density(alpha = 0.15, linewidth = 1, bounds = c(0, 1)) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    coord_cartesian(xlim = c(0, 1)) +
    labs(x = x_label, y = y_label, color = "depth", fill = "depth") +
    my.theme
}


## distribution of cover magnitude *given presence* (proportion > 0 only) for
## a single category, per transect: violin (density shape) + a narrow inset
## boxplot (median/IQR reference) + jittered points, all colored by transect.
## Each transect's prevalence (% of all photos, zero included, with any
## cover) is printed as large bold black text directly above its violin,
## with a smaller header line naming what those numbers mean -- in-panel text
## in place of a separate prevalence bar-chart panel.
##
## Loosely modeled after Fig. 4 of Randell et al. 2022 (PNAS Kelp-forest
## dynamics controlled by substrate complexity) -- a beeswarm + median line
## per group -- but swaps their flat median line for a violin, since our
## categories are proportions (bounded [0, 1], often multimodal) rather than
## the roughly unimodal urchin-abundance counts in that figure. Density is
## bounded to [0, 1] via geom_violin()'s `bounds` argument, same reasoning as
## visualize.category.density.by.transect() above.
##
## Zero-cover photos are excluded from the violin/box/points (that's the
## point -- see the % labels for how common they are); this is stated
## explicitly in the subtitle so the exclusion isn't silent.
visualize.category.violin.with.prevalence <- function(data, category, colors,
                                                       transect_order = 1:6,
                                                       species_label = category,
                                                       title = category,
                                                       y_label = "proportion cover (given present)",
                                                       label_y = 1.12,
                                                       header_y = 1.24) {
  plot_data <- data %>%
    mutate(transect = factor(transect, levels = transect_order))

  prevalence <- plot_data %>%
    group_by(transect) %>%
    summarise(pct = round(100 * mean(.data[[category]] > 0, na.rm = TRUE)),
             .groups = "drop")

  nonzero_data <- filter(plot_data, .data[[category]] > 0)
  n_total <- nrow(plot_data)
  n_nonzero <- nrow(nonzero_data)
  subtitle <- paste0(
    "shown below: the ", n_nonzero, "/", n_total, " photos (",
    round(100 * n_nonzero / n_total, 1),
    "%) with any cover; zero-cover photos excluded (see % above)"
  )

  header_data <- tibble::tibble(
    x = mean(seq_along(transect_order)), y = header_y,
    label = paste0("% of photos with ", species_label, " present")
  )

  ggplot(nonzero_data, aes(x = transect, y = .data[[category]])) +
    geom_violin(aes(fill = transect, color = transect), alpha = 0.25,
               bounds = c(0, 1), linewidth = 0.8) +
    geom_boxplot(aes(color = transect), width = 0.12, fill = "white",
                alpha = 0.8, outlier.shape = NA, linewidth = 0.6) +
    geom_jitter(aes(color = transect), width = 0.15, alpha = 0.5, size = 1.5) +
    geom_text(data = prevalence, aes(x = transect, y = label_y, label = paste0(pct, "%")),
              inherit.aes = FALSE, color = "black", fontface = "bold", size = 6) +
    ggtext::geom_richtext(data = header_data, aes(x = x, y = y, label = label),
                          inherit.aes = FALSE, fill = NA, label.color = NA,
                          color = "black", size = 4.2) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    scale_y_continuous(breaks = seq(0, 1, 0.25)) +
    coord_cartesian(ylim = c(0, header_y + 0.06)) +
    labs(x = "transect", y = y_label, title = title, subtitle = subtitle) +
    guides(fill = "none", color = "none") +
    my.theme +
    theme(plot.title = ggtext::element_markdown(size = 15),
          plot.subtitle = element_text(size = 10.5))
}


## build one row per transect (site x transect x season) comparing diver
## density to ROV percent-cover for a single algae/kelp species. These are
## NOT the same units -- diver density is a count-based index (individuals
## per transect, extrapolated/standardized), ROV cover is the proportion of
## photo points classified as that species -- and there's no defensible way
## to convert one into the other without calibration data (e.g. average
## canopy footprint per individual) that we don't have. The three
## visualize.kelp.*() functions below compare relative pattern/concordance
## across transects instead of raw magnitude.
build.kelp.comparison.data <- function(diver_density_df, rov_cover_df, species_col) {
  diver_slim <- diver_density_df %>%
    select(site, transect, season, depth, diver_value = all_of(species_col))
  rov_slim <- rov_cover_df %>%
    select(site, transect, season, depth, rov_value = all_of(species_col))

  inner_join(diver_slim, rov_slim, by = c("site", "transect", "season", "depth")) %>%
    mutate(transect_label = paste0(site, "_T", transect, "_", season)) %>%
    arrange(site, transect, season)
}


## z-score both series (mean 0, sd 1) so they're visually comparable on one
## shared axis despite being on fundamentally different scales; supports a
## relative/pattern comparison across transects, not an absolute-magnitude one
visualize.kelp.standardized.overlay <- function(data, colors,
                                                x_label = "transect",
                                                y_label = "standardized value (z-score)") {
  plot_data <- data %>%
    mutate(
      diver = as.numeric(scale(diver_value)),
      ROV = as.numeric(scale(rov_value)),
      transect_label = factor(transect_label, levels = transect_label)
    ) %>%
    select(transect_label, diver, ROV) %>%
    pivot_longer(cols = c(diver, ROV), names_to = "method", values_to = "value")

  ggplot(plot_data, aes(x = transect_label, y = value, color = method, group = method)) +
    geom_line() +
    geom_point(size = 2) +
    scale_color_manual(values = colors) +
    labs(x = x_label, y = y_label, color = "method") +
    my.theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}


## shared data-prep for visualize.kelp.standardized.overlay.stack(): z-scores
## both methods and builds a "T#.season.site" nested x category (in that
## order -- transect innermost/fastest-varying, season next, site outermost)
## for use with legendry::guide_axis_nested(), which draws the season/site
## groupings as bracket labels beneath the transect ticks.
prep.kelp.standardized.data <- function(data, site_order, season_order) {
  data %>%
    mutate(
      site_f = factor(site, levels = site_order),
      season_f = factor(season, levels = season_order),
      site_display = factor(gsub("_", " ", as.character(site_f)),
                            levels = gsub("_", " ", site_order)),
      transect_short = factor(paste0("T", transect),
                              levels = paste0("T", sort(unique(transect)))),
      diver = as.numeric(scale(diver_value)),
      ROV = as.numeric(scale(rov_value))
    ) %>%
    arrange(site_f, season_f, transect) %>%
    mutate(x_nested = interaction(transect_short, season_f, site_display,
                                  sep = ".", lex.order = FALSE)) %>%
    select(x_nested, diver, ROV) %>%
    pivot_longer(cols = c(diver, ROV), names_to = "method", values_to = "value")
}


## combine two standardized (z-score) overlay plots -- e.g. two kelp species --
## into a single one-column, two-row figure: a legend inset into the top
## panel's upper right, a single shared y-axis title, and a shared nested
## x-axis (transect > season > site) shown only on the bottom row (the top
## row keeps its tick marks but no text, so the two rows still line up).
## `title_top`/`title_bottom` are rendered as markdown (via ggtext), so pass
## e.g. "*Saccharina latissima* (sugar kelp)" for italicized scientific names.
## Requires legendry (nested axis guide), patchwork (stacking + collecting
## the shared axis title), and ggtext (markdown/italic titles).
visualize.kelp.standardized.overlay.stack <- function(data_top, data_bottom,
                                                       title_top, title_bottom,
                                                       colors,
                                                       site_order = c("Centennial_Park", "Elliott_Bay_Marina"),
                                                       season_order = c("summer", "winter"),
                                                       y_label = "standardized z-score",
                                                       axis_text_size = 15,
                                                       subtitle_text_size = 13,
                                                       legend_position_inside = c(0.93, 0.90)) {
  plot_top <- ggplot(
    prep.kelp.standardized.data(data_top, site_order, season_order),
    aes(x = x_nested, y = value, color = method, group = method)
  ) +
    geom_line() +
    geom_point(size = 2) +
    scale_color_manual(values = colors) +
    labs(x = NULL, y = y_label, color = "method", title = title_top) +
    my.theme +
    theme(axis.text.x = element_blank(),
          plot.title = ggtext::element_markdown(size = 15),
          legend.position = "inside",
          legend.position.inside = legend_position_inside,
          legend.background = element_blank())

  plot_bottom <- ggplot(
    prep.kelp.standardized.data(data_bottom, site_order, season_order),
    aes(x = x_nested, y = value, color = method, group = method)
  ) +
    geom_line() +
    geom_point(size = 2) +
    scale_color_manual(values = colors) +
    guides(x = legendry::guide_axis_nested(key = legendry::key_range_auto(sep = "\\."))) +
    labs(x = NULL, y = y_label, color = "method", title = title_bottom) +
    my.theme +
    theme(axis.text.x = element_text(size = axis_text_size),
          legendry.axis.subtitle = element_text(size = subtitle_text_size),
          plot.title = ggtext::element_markdown(size = 15),
          legend.position = "none")

  (plot_top / plot_bottom) +
    patchwork::plot_layout(axes = "collect", axis_titles = "collect")
}


## combine sugar and sieve kelp standardized overlays into a single row/panel
## (4 lines: diver x {sugar, sieve}, ROV x {sugar, sieve}) rather than the
## two-row stack in visualize.kelp.standardized.overlay.stack(). Reuses
## prep.kelp.standardized.data() for the per-species z-scoring/nested x-axis
## category, then combines both species into one long dataframe with a
## 4-level method x species "series" column.
## `colors` must be a named vector keyed by "diver_sugar", "diver_sieve",
## "ROV_sugar", "ROV_sieve"; `series_labels` (same keys) controls the legend
## text shown for each.
visualize.kelp.standardized.overlay.combined <- function(data_sugar, data_sieve,
                                                          title, colors,
                                                          site_order = c("Centennial_Park", "Elliott_Bay_Marina"),
                                                          season_order = c("summer", "winter"),
                                                          y_label = "standardized z-score",
                                                          axis_text_size = 15,
                                                          subtitle_text_size = 13,
                                                          series_labels = c(
                                                            diver_sugar = "diver – sugar kelp",
                                                            diver_sieve = "diver – sieve kelp",
                                                            ROV_sugar   = "ROV – sugar kelp",
                                                            ROV_sieve   = "ROV – sieve kelp"
                                                          )) {
  sugar_data <- prep.kelp.standardized.data(data_sugar, site_order, season_order) %>%
    mutate(series = paste(method, "sugar", sep = "_"))
  sieve_data <- prep.kelp.standardized.data(data_sieve, site_order, season_order) %>%
    mutate(series = paste(method, "sieve", sep = "_"))

  plot_data <- bind_rows(sugar_data, sieve_data) %>%
    mutate(series = factor(series, levels = names(series_labels)))

  ggplot(plot_data, aes(x = x_nested, y = value, color = series, group = series)) +
    geom_line() +
    geom_point(size = 2) +
    scale_color_manual(values = colors, labels = series_labels, breaks = names(series_labels)) +
    guides(x = legendry::guide_axis_nested(key = legendry::key_range_auto(sep = "\\."))) +
    labs(x = NULL, y = y_label, color = NULL, title = title) +
    my.theme +
    theme(axis.text.x = element_text(size = axis_text_size),
          legendry.axis.subtitle = element_text(size = subtitle_text_size),
          plot.title = ggtext::element_markdown(size = 15))
}


## rank transects separately within each method (1 = highest value) and draw
## a two-column slope/bump graph connecting each transect's diver-rank to its
## ROV-rank -- visualizes relative agreement ("did the methods agree on which
## transects had more kelp") without needing shared units
visualize.kelp.bump.chart <- function(data, rank_color_by = "site") {
  plot_data <- data %>%
    mutate(
      diver = rank(-diver_value, ties.method = "min"),
      ROV = rank(-rov_value, ties.method = "min")
    ) %>%
    select(transect_label, diver, ROV, all_of(rank_color_by)) %>%
    pivot_longer(cols = c(diver, ROV), names_to = "method", values_to = "rank") %>%
    mutate(method = factor(method, levels = c("diver", "ROV")))

  ggplot(plot_data, aes(x = method, y = rank, group = transect_label,
                        color = .data[[rank_color_by]])) +
    geom_line(alpha = 0.6) +
    geom_point(size = 2) +
    scale_y_reverse(breaks = seq_len(max(plot_data$rank))) +
    labs(x = NULL, y = "rank (1 = highest)", color = rank_color_by) +
    my.theme
}


## scatter of diver density vs. ROV cover per transect -- no 1:1 line (there
## is no meaningful "equal" value across these units), just a look at the
## strength/shape of the association, with a loess trend for reference
visualize.kelp.scatter <- function(data, color_by = "site",
                                   x_label = "diver kelp density",
                                   y_label = "ROV kelp percent-cover") {
  ggplot(data, aes(x = diver_value, y = rov_value, color = .data[[color_by]])) +
    geom_point(size = 2) +
    geom_smooth(method = "loess", se = TRUE, color = "black", linewidth = 0.6) +
    labs(x = x_label, y = y_label, color = color_by) +
    my.theme
}
