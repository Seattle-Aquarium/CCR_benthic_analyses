## script to contain functions for analyzing / visualizing data




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
## head_to_head_pairs in analyze.R), giving the display category label, the
## matching ROV column name, and the matching diver column name. Each pair is
## joined on site/transect/season independently (rather than one big join
## across all pairs at once) so that columns which happen to share a name
## across the ROV and diver dataframes -- e.g. both have their own
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
