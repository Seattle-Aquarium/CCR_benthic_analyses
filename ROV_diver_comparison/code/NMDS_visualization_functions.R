## script to contain functions for visualizing the photo-level NMDS ordination




## preferred graphing theme (matches data_visualization_functions.R)
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


## display names for site/season, and the 2-color site palette used by the
## site-only (+ category correlation) figure
site_display_names <- c(Centennial_Park = "Centennial Park",
                        Elliott_Bay_Marina = "Elliott Bay Marina")
season_display_names <- c(summer = "Summer", winter = "Winter")

## blue = Elliott Bay Marina, orange = Centennial Park -- same hue convention
## as transect_density_colors in data_visualization.R and
## site_depth_season_colors below (mid-tones, since this figure shows site
## alone rather than the full site x depth x season breakdown)
site_colors <- c("Centennial Park" = "#E6550D", "Elliott Bay Marina" = "#3182BD")


## site x depth x season palette for the single-panel, most-granular figure --
## site sets the hue family (Elliott Bay Marina = blue, matching
## transect_density_colors' "deep" family in data_visualization.R;
## Centennial Park = orange, matching that palette's "shallow" family), with
## 4 shades per family (dark -> light) encoding depth x season together
## (deep/summer darkest through shallow/winter lightest). This keeps every
## one of the 8 site x depth x season groups visually distinct -- and still
## instantly sortable into "blue = EBM" / "orange = Centennial" -- in one
## panel with no faceting.
site_depth_season_colors <- c(
  "Elliott Bay Marina, Deep, Summer"    = "#08306B",
  "Elliott Bay Marina, Deep, Winter"    = "#3182BD",
  "Elliott Bay Marina, Shallow, Summer" = "#6BAED6",
  "Elliott Bay Marina, Shallow, Winter" = "#BDD7E7",
  "Centennial Park, Deep, Summer"       = "#7F2704",
  "Centennial Park, Deep, Winter"       = "#D94801",
  "Centennial Park, Shallow, Summer"    = "#FD8D3C",
  "Centennial Park, Shallow, Winter"    = "#FDD0A2"
)


## recode raw site/season/depth codes to display-ready factors, once, so
## every plotting function below can assume clean labels/level order. Also
## adds site_depth_season, the single 8-level column used for both color and
## ellipse/point grouping in the most-granular figure.
prep.nmds.data <- function(data){
  data %>%
    mutate(
      site = factor(site, levels = names(site_display_names), labels = site_display_names),
      season = factor(season, levels = names(season_display_names), labels = season_display_names),
      depth = factor(depth, levels = c("deep", "shallow"), labels = c("Deep", "Shallow")),
      site_depth_season = factor(paste(site, depth, season, sep = ", "),
                                levels = names(site_depth_season_colors))
    )
}


## base NMDS scatter + 95% ellipses, colored by `color_by` -- used for the
## site x depth x season figure. `group_by` defaults to `color_by` but can be
## set finer -- e.g. site_depth_season -- so multiple groups sharing one
## color (site not being color-coded there) still get separate ellipses
visualize.nmds <- function(data, color_by, colors, group_by = color_by,
                          point_alpha = 0.35, point_size = 1.5,
                          ellipse_linewidth = 1, title = NULL, legend_name = color_by){
  p <- ggplot(data, aes(x = MDS1, y = MDS2, color = .data[[color_by]])) +
    geom_point(size = point_size, alpha = point_alpha) +
    stat_ellipse(aes(group = .data[[group_by]]), linewidth = ellipse_linewidth, level = 0.95) +
    scale_color_manual(values = colors, name = legend_name) +
    coord_fixed() +
    xlab("NMDS1") + ylab("NMDS2") +
    my.theme

  if (!is.null(title)) p <- p + ggtitle(title)

  p
}


## overlay percent-cover category correlation vectors (arrows + labels, from
## the spp_scores saved alongside the ordination in NMDS.R) on top of a
## faded, site-colored NMDS scatter -- shows which categories pull ordination
## space in which direction. `categories`, if given, subsets spp_scores down
## to just those category names (the full 30-category set gets crowded --
## see NMDS_spp_scores_photo-level.csv for the complete, unfiltered set).
## Labels are placed via ggrepel::geom_label_repel rather than plain
## geom_label so they nudge apart instead of stacking illegibly on top of
## each other -- `seed` is
## fixed so repositioning is reproducible across re-runs. `ellipses = TRUE`
## adds the same per-site 95% ellipse as visualize.nmds(), letting this
## double as an annotated version of the site-only figure. `legend_position`,
## if given (a c(x, y) pair in normalized panel coordinates), moves the
## legend inside the panel -- e.g. c(0.95, 0.05) with the default
## legend.justification below for a bottom-right inset.
visualize.nmds.categories <- function(data, spp_scores, colors, categories = NULL,
                                      point_alpha = 0.15, point_size = 1.2,
                                      ellipses = FALSE, ellipse_linewidth = 1,
                                      title = NULL, legend_name = "site",
                                      legend_position = NULL, legend_justification = c(1, 0),
                                      label_size = 3.2, seed = 42){
  if (!is.null(categories)) spp_scores <- dplyr::filter(spp_scores, category %in% categories)

  p <- ggplot(data, aes(x = MDS1, y = MDS2)) +
    geom_point(aes(color = site), size = point_size, alpha = point_alpha) +
    scale_color_manual(values = colors, name = legend_name)

  if (ellipses) {
    p <- p + stat_ellipse(aes(color = site, group = site), linewidth = ellipse_linewidth, level = 0.95)
  }

  p <- p +
    geom_segment(data = spp_scores, linewidth = 0.8, color = "black",
                aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                arrow = arrow(length = unit(0.2, "cm"))) +
    ggrepel::geom_label_repel(data = spp_scores, size = label_size, seed = seed,
              fontface = "bold", label.size = 0.4, fill = "white",
              max.overlaps = Inf, min.segment.length = 0, segment.color = "gray40",
              aes(x = NMDS1, y = NMDS2, label = category)) +
    coord_fixed() +
    xlab("NMDS1") + ylab("NMDS2") +
    my.theme

  if (!is.null(title)) p <- p + ggtitle(title)

  if (!is.null(legend_position)) {
    p <- p + theme(legend.position = legend_position,
                   legend.justification = legend_justification,
                   legend.background = element_rect(fill = scales::alpha("white", 0.7), color = NA))
  }

  p
}


## manually draw a compact title + color-key/label block directly on the
## panel at data coordinates (x, y_top) -- used to split the 8-entry
## site_depth_season legend into two site-specific mini-legends positioned
## under each site's own point cluster (see NMDS_visualization.R), instead of
## one shared ggplot legend wide enough to double the figure's overall width
## once placed next to the category-correlation figure in the report. Not a
## real ggplot legend/guide -- just annotate() layers -- so it works with
## legend.position = "none" and is free to sit on top of a few data points,
## which is expected/acceptable here.
add.legend.block <- function(p, x, y_top, title, labels, colors,
                            line_height = 0.12, segment_length = 0.16,
                            text_size = 3.6, title_size = 4, gap = 0.05){
  p <- p + annotate("text", x = x, y = y_top, label = title, hjust = 0,
                    fontface = "bold", size = title_size, color = "black")

  for (i in seq_along(labels)) {
    y <- y_top - gap - i * line_height
    p <- p +
      annotate("segment", x = x, xend = x + segment_length, y = y, yend = y,
              color = colors[i], linewidth = 1.3) +
      annotate("text", x = x + segment_length + 0.04, y = y, label = labels[i],
              hjust = 0, size = text_size, color = "black")
  }

  p
}


## crop a PDF's surrounding whitespace down to its content bounding box via
## pdfcrop (ships with MiKTeX/TeX Live). ggsave's PDF canvas is the full
## requested width x height, but coord_fixed() -- used by every NMDS figure
## here -- often leaves uneven blank strips on the sides (or top/bottom) once
## the panel's forced 1:1 data aspect doesn't exactly fill that canvas;
## pdfcrop trims all of that down to just the actual ink (axis titles
## included), plus a small `margins` buffer (points) so text isn't clipped
## right at the edge -- this matters when placing figures side-by-side in a
## LaTeX report. Silently skipped, with a message, if pdfcrop isn't on the
## PATH (e.g. no TeX distribution installed).
crop.pdf <- function(path, margins = 3){
  if (!nzchar(Sys.which("pdfcrop"))) {
    message("pdfcrop not found on PATH -- left ", path, " uncropped")
    return(invisible(NULL))
  }
  tmp <- paste0(path, ".cropped.pdf")
  system2("pdfcrop", args = c("--margins", margins, shQuote(path), shQuote(tmp)),
         stdout = FALSE, stderr = FALSE)
  if (file.exists(tmp)) file.rename(tmp, path)
  invisible(NULL)
}


## save a plot as both PNG (quick previews) and PDF (vector, for the
## technical report -- cropped tight via crop.pdf() above) with one call --
## mirrors save.plot() in community_analyses/code/visualization_functions.R
save.plot <- function(plot, path, filename, width, height, dpi = 300){
  ggsave(file.path(path, paste0(filename, ".png")), plot, width = width, height = height, dpi = dpi)

  pdf_path <- file.path(path, paste0(filename, ".pdf"))
  ggsave(pdf_path, plot, width = width, height = height)
  crop.pdf(pdf_path)
}




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
