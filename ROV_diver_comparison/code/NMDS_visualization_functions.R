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
## site-only and site x depth figures (site mapped to color; depth handled
## via facets there -- see visualize.nmds()'s `facets` argument)
site_display_names <- c(Centennial_Park = "Centennial Park",
                        Elliott_Bay_Marina = "Elliott Bay Marina")
season_display_names <- c(summer = "Summer", winter = "Winter")

site_colors <- c("Centennial Park" = "#1B9E77", "Elliott Bay Marina" = "#7570B3")


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


## base NMDS scatter + 95% ellipses, colored by `color_by`, optionally
## faceted via `facets` (a facet_wrap()/facet_grid() layer, e.g.
## facet_wrap(~ depth) or facet_grid(season ~ depth)) -- the shared building
## block behind every grouped NMDS figure, since the site-only, site x depth,
## and single-panel site x depth x season figures differ only in which
## columns are mapped to color/ellipse-grouping vs. facets. `group_by`
## defaults to `color_by` (one ellipse per color, as in the first 2 figures)
## but can be set finer -- e.g. site_depth_season -- so multiple groups
## sharing one color (site not being color-coded) still get separate ellipses
visualize.nmds <- function(data, color_by, colors, group_by = color_by, facets = NULL,
                          point_alpha = 0.35, point_size = 1.5,
                          ellipse_linewidth = 1, title = NULL, legend_name = color_by){
  p <- ggplot(data, aes(x = MDS1, y = MDS2, color = .data[[color_by]])) +
    geom_point(size = point_size, alpha = point_alpha) +
    stat_ellipse(aes(group = .data[[group_by]]), linewidth = ellipse_linewidth, level = 0.95) +
    scale_color_manual(values = colors, name = legend_name) +
    coord_fixed() +
    xlab("NMDS1") + ylab("NMDS2") +
    my.theme

  if (!is.null(facets)) p <- p + facets
  if (!is.null(title)) p <- p + ggtitle(title)

  p
}


## overlay percent-cover category correlation vectors (arrows + labels, from
## the spp_scores saved alongside the ordination in NMDS.R) on top of a
## faded, site-colored NMDS scatter -- shows which categories pull ordination
## space in which direction, independent of the site/depth/season groupings
## shown in the other 3 figures. With 30 categories this gets crowded; the
## saved NMDS_spp_scores_photo-level.csv is the more precise reference.
visualize.nmds.categories <- function(data, spp_scores, colors, title = NULL,
                                      label_size = 3.2){
  p <- ggplot(data, aes(x = MDS1, y = MDS2)) +
    geom_point(aes(color = site), size = 1.2, alpha = 0.15) +
    scale_color_manual(values = colors, name = "site") +
    geom_segment(data = spp_scores, linewidth = 0.8, color = "black",
                aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                arrow = arrow(length = unit(0.2, "cm"))) +
    geom_label(data = spp_scores, hjust = 0.5, size = label_size,
              fontface = "bold", label.size = 0.4, fill = "white",
              aes(x = NMDS1, y = NMDS2, label = category,
                  vjust = ifelse(NMDS2 >= 0, -0.4, 1.3))) +
    coord_fixed() +
    xlab("NMDS1") + ylab("NMDS2") +
    my.theme

  if (!is.null(title)) p <- p + ggtitle(title)

  p
}




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
