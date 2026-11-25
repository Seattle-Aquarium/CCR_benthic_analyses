## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## functions for visualizing perpendicular-transect percent cover ~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Every figure is sized for a 16:9 slide (13.333 x 7.5 in) unless noted.




## colors ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## one color per cover group (names match cover_groups in wrangle_functions.R),
## following the ROV_diver_comparison conventions: red/green reserved for red/
## green algae, earth tones for substrate (darkest = coarsest), browns for
## kelp. Checked with the dataviz skill's palette validator in this stacking
## order: every adjacent pair is separable under protan/deutan colorblindness
## (worst Delta E 11.9, red/green under deutan; target >= 8) and with normal
## vision (worst 17.8; floor 15). The three substrate colors are deliberately
## low-chroma (they read as sediment), which the validator flags as "grayish"
## -- intended -- and silt/sand are light, so every figure carries a legend.
cover_colors <- c(
  "Silt"              = "#A39E93",
  "Sand & shell"      = "#EBC867",
  "Hard substrate"    = "#6E6259",
  "Sugar kelp"        = "#C2862B",
  "Sieve kelp"        = "#5E3F1E",
  "Other brown algae" = "#9C9640",
  "Red algae"         = "#A6323C",
  "Green algae"       = "#4C9A2A",
  "Other"             = "#5B6C9A"
)

## non-data colors: seafloor cross-section and water column
seafloor_fill <- "#DAD3C6"
seafloor_line <- "#4A4237"
water_fill    <- "#E6F0F5"
ink           <- "grey15"
ink_muted     <- "grey40"


## text on a colored block: near-black on light fills, white on dark ones
text.on <- function(fill) {
  rgb <- grDevices::col2rgb(fill) / 255
  lin <- ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  lum <- 0.2126 * lin[1, ] + 0.7152 * lin[2, ] + 0.0722 * lin[3, ]
  ifelse(lum > 0.25, ink, "white")
}


## slide theme ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## same look as my.theme in ROV_diver_comparison (no grid, black axis lines),
## with text sized for a projected 13.333 x 7.5 in slide
slide.theme <- function(base = 14) {
  theme_classic(base_size = base) +
    theme(axis.line = element_line(colour = "black", linewidth = 0.5),
          axis.ticks = element_line(colour = "black", linewidth = 0.5),
          axis.text = element_text(colour = ink, size = base),
          axis.title = element_text(colour = ink, size = base + 2),
          plot.title = element_text(colour = ink, size = base + 8, face = "bold"),
          plot.subtitle = element_text(colour = ink_muted, size = base + 1),
          plot.caption = element_text(colour = ink_muted, size = base - 3, hjust = 0),
          legend.title = element_blank(),
          legend.text = element_text(colour = ink, size = base),
          legend.key.size = unit(0.22, "in"),
          strip.background = element_blank(),
          strip.text = element_text(colour = ink, size = base + 2, face = "bold"))
}


## shared legend scale: always all nine groups, in stacking order, so every
## figure's legend is identical whether or not a group occurs in it
scale.cover.fill <- function(...) {
  scale_fill_manual(values = cover_colors, limits = names(cover_colors),
                    breaks = names(cover_colors), drop = FALSE, ...)
}


## long format: one row per photo x cover group (proportion 0-1)
cover.long <- function(photos) {
  photos %>%
    select(transect, Name, photo_index, cumulative_distance_m, depth_mllw_m,
           all_of(names(cover_groups))) %>%
    pivot_longer(all_of(names(cover_groups)), names_to = "group", values_to = "p") %>%
    mutate(group = factor(group, levels = names(cover_colors)))
}




## thumbnails ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## write a `width`-px-wide copy of every photo into thumbs_dir/T<n>/ (skipping
## ones already there) and return the photos table with a thumb_path column.
## The ~3.5 MB, 4606 x 4030 px originals are far more than a 0.2 in filmstrip
## frame needs, and re-reading 123 of them on every run is slow.
build.thumbnails <- function(photos, flights_root, thumbs_dir, width = 800) {
  photos <- photos %>%
    mutate(src_path = file.path(flights_root, paste0("T", transect), "edited", Name),
           thumb_path = file.path(thumbs_dir, paste0("T", transect), Name))
  for (d in unique(dirname(photos$thumb_path))) dir.create(d, showWarnings = FALSE, recursive = TRUE)

  todo <- which(!file.exists(photos$thumb_path))
  for (i in todo) {
    magick::image_read(photos$src_path[i]) %>%
      magick::image_scale(as.character(width)) %>%
      magick::image_write(photos$thumb_path[i], format = "jpg", quality = 88)
  }
  if (length(todo)) cat("  wrote", length(todo), "thumbnails to", thumbs_dir, "\n")
  photos
}


## read an image as a raster for annotation_raster()
read.raster <- function(path, width = NULL) {
  img <- magick::image_read(path)
  if (!is.null(width)) img <- magick::image_scale(img, as.character(width))
  as.raster(img)
}




## method figure: photo -> 50 points -> percent cover ~~~~~~~~~~~~~~~~~~~~~~~~~
## one photo with each of its 50 annotated 224 x 224 px patches outlined in
## its cover group's color (over a thin white halo so every outline reads
## against the photo), and directly beneath it that photo's composition as a
## single 100% bar: one block per cover group present (stacking order, left ->
## right), its share printed inside when the block is wide enough. Photo and
## bar share one coordinate system (the original image's pixels), so the bar
## is always exactly as wide as the photo. The photo itself is embedded
## downsampled (`embed_width` px) to keep file size sane.
visualize.point.overlay <- function(photo_path, pts, photo_row, title = NULL, subtitle = NULL,
                                    img_w = 4606, img_h = 4030, patch = 224,
                                    embed_width = 1200, box_lw = 0.9,
                                    bar_h = 420, bar_gap = 90, min_label = 0.08) {
  half <- patch / 2
  boxes <- pts %>%
    mutate(group = factor(group, levels = names(cover_colors)),
           xmin = col - half, xmax = col + half,
           ymin = -(row + half), ymax = -(row - half))

  seg_gap <- 0.004 * img_w
  segs <- photo_row %>%
    select(all_of(names(cover_groups))) %>%
    pivot_longer(everything(), names_to = "group", values_to = "p") %>%
    filter(p > 0) %>%
    mutate(group = factor(group, levels = names(cover_colors))) %>%
    arrange(group) %>%
    mutate(xmax = cumsum(p) * img_w, xmin = xmax - p * img_w,
           label = ifelse(p >= min_label, paste0(round(100 * p), "%"), ""),
           label_colour = text.on(cover_colors[as.character(group)]))
  bar_top <- -img_h - bar_gap
  bar_bottom <- bar_top - bar_h

  ggplot() +
    annotation_raster(read.raster(photo_path, embed_width),
                      xmin = 0, xmax = img_w, ymin = -img_h, ymax = 0, interpolate = TRUE) +
    geom_rect(data = boxes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = NA, colour = "white", linewidth = box_lw + 0.9) +
    geom_rect(data = boxes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, colour = group),
              fill = NA, linewidth = box_lw) +
    geom_rect(data = segs, aes(xmin = pmax(xmin + seg_gap, 0), xmax = pmin(xmax - seg_gap, img_w),
                               ymin = bar_bottom, ymax = bar_top, fill = group)) +
    geom_text(data = segs, aes(x = (xmin + xmax) / 2, y = (bar_top + bar_bottom) / 2,
                               label = label, colour = I(label_colour)),
              size = 4.8, fontface = "bold") +
    scale_colour_manual(values = cover_colors, limits = names(cover_colors), guide = "none") +
    scale.cover.fill(guide = "none") +
    coord_fixed(xlim = c(0, img_w), ylim = c(bar_bottom, 0), expand = FALSE) +
    labs(title = title, subtitle = subtitle) +
    theme_void(base_size = 14) +
    theme(plot.title = element_text(colour = ink, size = 18, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(colour = ink_muted, size = 13, hjust = 0.5,
                                       margin = margin(t = 2, b = 6)),
          plot.margin = margin(4, 14, 4, 14))
}


## three photos (e.g. deep, mid, shallow), each with its point overlay above
## its composition bar, plus one shared legend -- the "how a photo becomes a
## bar" explainer that precedes the transect-strip figures
visualize.method.figure <- function(exemplars, points, title, subtitle, caption) {
  cols <- purrr::map(seq_len(nrow(exemplars)), function(i) {
    ex <- exemplars[i, ]
    pts <- points %>% filter(transect == ex$transect, Name == ex$Name)
    visualize.point.overlay(ex$src_path, pts, ex, title = ex$label,
                            subtitle = sprintf("%.1f m below MLLW", ex$depth_mllw_m))
  })

  ## a legend-only panel: a ggplot whose only visible element is the legend
  legend_plot <- ggplot(tibble(group = factor(names(cover_colors), levels = names(cover_colors)),
                               x = 1, y = 1),
                        aes(x, y, fill = group)) +
    geom_col(width = 0) +
    scale.cover.fill() +
    guides(fill = guide_legend(nrow = 1)) +
    theme_void(base_size = 14) +
    theme(legend.position = "inside", legend.position.inside = c(0.5, 0.5),
          legend.title = element_blank(),
          legend.text = element_text(colour = ink, size = 13),
          legend.key.size = unit(0.22, "in"))

  (wrap_plots(cols, nrow = 1) / legend_plot) +
    plot_layout(heights = unit(c(1, 0.45), c("null", "in"))) +
    plot_annotation(title = title, subtitle = subtitle, caption = caption,
                    theme = slide.theme() + theme(plot.margin = margin(12, 16, 8, 16)))
}




## transect strip: cover bars over the photos over the seafloor profile ~~~~~~~~
## x is photo order (1..n), so each bar sits exactly over the photo that
## produced it and over that photo's spot on the depth profile. Photo spacing
## is close to even (~1 m), and the bottom axis is labelled in true cumulative
## distance (ticks placed by interpolating distance -> photo order), so the
## axis stays honest where spacing varies.
distance.breaks <- function(df, step = 10) {
  at <- seq(0, floor(max(df$cumulative_distance_m) / step) * step, by = step)
  list(breaks = approx(df$cumulative_distance_m, df$photo_index, xout = at, ties = "ordered")$y,
       labels = as.character(at))
}


## one raster holding every photo side by side, each centred in an equal-width
## cell with white gaps between -- drawn as a single annotation_raster(). (Drawing
## each frame as its own small raster corrupted some frames on the Windows PNG
## device: half-blank, shifted tiles.) Each cell is `tile_w` px wide.
build.filmstrip <- function(paths, frame_frac = 0.9, tile_w = 180, photo_aspect = 4606 / 4030) {
  frame_w <- round(tile_w * frame_frac)
  frame_h <- round(frame_w / photo_aspect)
  pad <- tile_w - frame_w
  frames <- purrr::map(paths, function(p) {
    magick::image_read(p) %>%
      magick::image_resize(paste0(frame_w, "x", frame_h, "!")) %>%
      magick::image_border("white", paste0(floor(pad / 2), "x0")) %>%
      magick::image_crop(paste0(tile_w, "x", frame_h))
  })
  as.raster(magick::image_append(do.call(c, frames)))
}


## shared colour scale for the line version (same colours/order as the fills)
scale.cover.colour <- function(...) {
  scale_colour_manual(values = cover_colors, limits = names(cover_colors),
                      breaks = names(cover_colors), drop = FALSE, ...)
}


## centred running mean of each group's cover over `window` consecutive
## photos (shrinking to what's available at the transect's two ends)
running.mean <- function(long, window = 5) {
  half <- (window - 1) %/% 2
  long %>%
    group_by(transect, group) %>%
    arrange(photo_index, .by_group = TRUE) %>%
    mutate(p = purrr::map_dbl(seq_along(p), function(i) {
      mean(p[max(1, i - half):min(length(p), i + half)])
    })) %>%
    ungroup()
}


## `style` picks the top panel: "bars" (one stacked bar per photo) or "lines"
## (one line per cover group, a point per photo, on the same 0-100% axis);
## `smooth_window` (lines only) draws each line as a running mean over that
## many photos instead of the raw per-photo values.
## `zones` (optional) is a data frame of boundaries (column `boundary_index`,
## the fractional photo position between the two photos the boundary falls
## between) and zone labels (`zones_labels`: label, x_mid) drawn as dashed
## lines through all three panels with names along the top.
## `panel_width_in` should be the top panel's rendered width; it sets the
## filmstrip's height so each frame keeps the photos' 4606:4030 shape.
visualize.transect.strip <- function(df, title, subtitle, caption = NULL,
                                     style = c("bars", "lines"), smooth_window = NULL,
                                     boundaries = NULL, zone_labels = NULL,
                                     panel_width_in = 11.85, frame_frac = 0.9,
                                     profile_height_in = 1.45, max_depth = 16,
                                     photo_aspect = 4606 / 4030, show_legend = TRUE) {
  style <- match.arg(style)
  n <- nrow(df)
  x_range <- c(0.5, n + 0.5)
  long <- cover.long(df)
  dist_axis <- distance.breaks(df)

  boundary_layer <- function(y_from = -Inf, y_to = Inf) {
    if (is.null(boundaries) || !nrow(boundaries)) return(NULL)
    geom_segment(data = boundaries, inherit.aes = FALSE,
                 aes(x = boundary_index, xend = boundary_index, y = y_from, yend = y_to),
                 colour = ink, linewidth = 0.6, linetype = "22")
  }

  ## top panel: stacked bars, or one line per group
  if (style == "bars") {
    bars <- ggplot(long, aes(x = photo_index, y = 100 * p, fill = group)) +
      geom_col(width = frame_frac, colour = "white", linewidth = 0.2,
               position = position_stack(reverse = TRUE)) +
      scale.cover.fill() +
      guides(fill = guide_legend(nrow = 1))
    y_lim <- c(0, 100)
  } else {
    ## a group's line is drawn only where it is present (plus the 0% points
    ## either side, so it rises from and returns to the baseline); otherwise
    ## every absent group runs along 0% and the last one drawn paints over the rest
    line_data <- (if (is.null(smooth_window)) long else running.mean(long, smooth_window)) %>%
      group_by(group) %>%
      arrange(photo_index, .by_group = TRUE) %>%
      mutate(p = ifelse(p > 0 | lag(p, default = 0) > 0 | lead(p, default = 0) > 0, p, NA)) %>%
      ungroup()
    bars <- ggplot(line_data, aes(x = photo_index, y = 100 * p, colour = group)) +
      geom_line(linewidth = if (is.null(smooth_window)) 0.8 else 1.3, na.rm = TRUE) +
      { if (is.null(smooth_window)) geom_point(data = filter(line_data, p > 0), size = 1.4) } +
      scale.cover.colour() +
      guides(colour = guide_legend(nrow = 1, override.aes = list(linewidth = 1.8, size = 0)))
    ## a little headroom so lines at 0% / 100% aren't cut by the panel edge
    y_lim <- c(-1.5, 101.5)
  }

  bars <- bars +
    boundary_layer() +
    scale_y_continuous(breaks = seq(0, 100, 25)) +
    coord_cartesian(xlim = x_range, ylim = y_lim, expand = FALSE, clip = "off") +
    labs(x = NULL, y = "Percent cover (%)") +
    slide.theme() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          legend.position = if (show_legend) "top" else "none",
          legend.justification = "left",
          legend.text = element_text(colour = ink, size = 13),
          legend.key.spacing.x = unit(0.12, "in"),
          ## zone names sit above the bars, so push the legend clear of them
          ## (and keep its background transparent so it can't paint over them)
          legend.margin = margin(0, 0, if (is.null(zone_labels)) 4 else 62, 0),
          legend.background = element_blank(),
          plot.margin = margin(t = 4, r = 6, b = 2, l = 6))

  ## each zone: a bracket spanning its bars, its name (one group per line) above
  if (!is.null(zone_labels)) {
    bars <- bars +
      geom_segment(data = zone_labels, inherit.aes = FALSE,
                   aes(x = first_photo - frame_frac / 2, xend = last_photo + frame_frac / 2,
                       y = 102, yend = 102), colour = ink, linewidth = 0.6) +
      geom_text(data = zone_labels, inherit.aes = FALSE,
                aes(x = x_mid, y = 104, label = label),
                vjust = 0, size = 4.1, lineheight = 0.88, colour = ink)
  }

  ## filmstrip: one frame per photo, centred on its bar, same width as the bar
  frame_w <- frame_frac * panel_width_in / n
  film_height_in <- frame_w / photo_aspect
  film <- ggplot() +
    annotation_raster(build.filmstrip(df$thumb_path, frame_frac),
                      xmin = x_range[1], xmax = x_range[2], ymin = 0, ymax = 1,
                      interpolate = TRUE) +
    boundary_layer() +
    coord_cartesian(xlim = x_range, ylim = c(0, 1), expand = FALSE, clip = "off") +
    theme_void() +
    theme(plot.margin = margin(t = 3, r = 6, b = 3, l = 6))

  ## seafloor profile (seafloor depth below MLLW at each photo)
  prof <- df %>% arrange(photo_index)
  ## seafloor fill runs edge to edge (held flat past the first/last photo)
  prof_fill <- bind_rows(
    tibble(photo_index = x_range[1], depth_mllw_m = first(prof$depth_mllw_m)),
    select(prof, photo_index, depth_mllw_m),
    tibble(photo_index = x_range[2], depth_mllw_m = last(prof$depth_mllw_m))
  )
  profile <- ggplot(prof, aes(x = photo_index)) +
    annotate("rect", xmin = x_range[1], xmax = x_range[2], ymin = 0, ymax = max_depth,
             fill = water_fill) +
    geom_ribbon(data = prof_fill, aes(ymin = depth_mllw_m, ymax = max_depth), fill = seafloor_fill) +
    geom_hline(yintercept = 0, colour = ink_muted, linewidth = 0.4, linetype = "dashed") +
    geom_line(aes(y = depth_mllw_m), colour = seafloor_line, linewidth = 0.8) +
    geom_point(aes(y = depth_mllw_m), colour = seafloor_line, size = 1.1) +
    boundary_layer(y_from = 0, y_to = max_depth) +
    scale_y_reverse(breaks = c(0, 5, 10, 15), expand = c(0, 0)) +
    scale_x_continuous(breaks = dist_axis$breaks, labels = dist_axis$labels) +
    coord_cartesian(xlim = x_range, ylim = c(max_depth, 0), expand = FALSE, clip = "off") +
    labs(x = "Distance along transect (m)", y = "Depth (m)\nbelow MLLW") +
    slide.theme() +
    theme(plot.margin = margin(t = 2, r = 6, b = 4, l = 6))

  wrap_plots(bars, film, profile, ncol = 1) +
    plot_layout(heights = unit(c(1, film_height_in, profile_height_in), c("null", "in", "in")),
                axis_titles = "collect_y") +
    plot_annotation(title = title, subtitle = subtitle, caption = caption,
                    theme = slide.theme() + theme(plot.margin = margin(12, 16, 8, 12)))
}




## kite diagram: depth zonation of each cover group ~~~~~~~~~~~~~~~~~~~~~~~~~~~
## the classic marine-zonation figure. Depth runs down the page; each cover
## group gets a column, and its "kite" is as wide as that group's mean cover
## in each 1-m depth band (a group covering 100% fills its whole column).
## One panel per transect.
kite.data <- function(photos, bin_width = 1, kite_frac = 0.92) {
  long <- cover.long(photos) %>%
    mutate(bin = floor(depth_mllw_m / bin_width),
           depth_mid = (bin + 0.5) * bin_width)
  binned <- long %>%
    group_by(transect, group, bin, depth_mid) %>%
    summarise(cover = mean(p), n_photos = n(), .groups = "drop")

  ## close each kite at the shallow and deep edges of the sampled depth range
  ## so shapes taper to a point rather than ending in a flat cut
  edges <- binned %>%
    group_by(transect, group) %>%
    summarise(top = min(bin) * bin_width, bottom = (max(bin) + 1) * bin_width, .groups = "drop") %>%
    pivot_longer(c(top, bottom), values_to = "depth_mid", names_to = "edge") %>%
    mutate(cover = 0) %>%
    select(-edge)

  ## runs of empty bands are dropped and each remaining contiguous run becomes
  ## its own kite (`piece`) -- otherwise a group absent over a stretch of
  ## depth still draws a hairline through it. A zero band next to an occupied
  ## one is kept so the kite tapers into it.
  bind_rows(binned, edges) %>%
    arrange(transect, group, depth_mid) %>%
    group_by(transect, group) %>%
    mutate(keep = cover > 0 | lag(cover, default = 0) > 0 | lead(cover, default = 0) > 0,
           piece = cumsum(keep & !lag(keep, default = FALSE))) %>%
    ungroup() %>%
    filter(keep) %>%
    mutate(col = as.integer(group),
           xmin = col - kite_frac * cover / 2,
           xmax = col + kite_frac * cover / 2,
           piece = interaction(transect, group, piece, drop = TRUE)) %>%
    select(-keep)
}


visualize.kite <- function(photos, title, subtitle, caption = NULL, bin_width = 1,
                           kite_frac = 0.92, max_depth = 15,
                           group_labels = c("Silt" = "Silt", "Sand & shell" = "Sand &\nshell",
                                            "Hard substrate" = "Hard\nsubstrate",
                                            "Sugar kelp" = "Sugar\nkelp", "Sieve kelp" = "Sieve\nkelp",
                                            "Other brown algae" = "Other\nbrown",
                                            "Red algae" = "Red\nalgae", "Green algae" = "Green\nalgae",
                                            "Other" = "Other")) {
  kd <- kite.data(photos, bin_width, kite_frac) %>%
    mutate(transect_label = paste("Transect", transect))
  n_groups <- length(cover_colors)

  ## a 50%-cover scale key in the (empty) bottom-right corner of the first panel
  key <- tibble(transect_label = paste("Transect", min(photos$transect)),
                x = n_groups + 0.45 - kite_frac * 0.5, xend = n_groups + 0.45,
                y = max_depth - 0.6)

  ggplot(kd) +
    geom_ribbon(aes(y = depth_mid, xmin = xmin, xmax = xmax, fill = group, group = piece),
                orientation = "y", colour = NA) +
    geom_segment(data = key, aes(x = x, xend = xend, y = y, yend = y),
                 colour = ink, linewidth = 1) +
    geom_text(data = key, aes(x = xend, y = y - 0.35, label = "width of 50% cover"),
              colour = ink, size = 4.2, vjust = 0, hjust = 1) +
    scale.cover.fill() +
    scale_x_continuous(breaks = seq_len(n_groups), labels = group_labels[names(cover_colors)],
                       expand = c(0, 0)) +
    scale_y_reverse(breaks = seq(0, max_depth, 3), expand = c(0, 0)) +
    coord_cartesian(xlim = c(0.5, n_groups + 0.5), ylim = c(max_depth, 0)) +
    facet_wrap(~ transect_label, nrow = 1) +
    labs(x = NULL, y = "Depth (m below MLLW)", title = title, subtitle = subtitle, caption = caption) +
    slide.theme() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 12, lineheight = 0.9, vjust = 1),
          axis.ticks.x = element_blank(),
          panel.spacing = unit(0.5, "in"),
          plot.margin = margin(12, 16, 8, 12))
}




## NMDS path: each transect's photos through the shared photo-level
## ordination (nmds.R), coloured by depth (one hue, light = shallow, dark =
## deep). One panel per transect, with the other transect's photos behind in
## grey for comparison. Two layers of path:
##   - faint: every photo joined to the next, in survey order
##   - bold:  the route of a `smooth_window`-photo moving average (the same
##            ~5 m running mean as the smoothed line strips), with arrowheads
##            showing direction -- the photo-to-photo path zigzags too much to
##            read the overall route from on its own
## A third "compass" panel at the same scale shows the direction each cover
## group increases (envfit vectors, in the group colours), leaving the path
## panels uncluttered; groups the ordination barely explains (r^2 <
## `min_r2`) are left off. NMDS1 is rotated to run deep (left) -> shallow
## (right).
visualize.nmds.path <- function(points, vectors, title, subtitle, caption = NULL,
                                smooth_window = 5, min_r2 = 0.1,
                                depth_limits = c(0, 15),
                                shallow_col = "#8CC0E0", deep_col = "#08306B") {
  panel_name <- function(t) paste("Transect", t)
  depth_scale <- function(aesthetics = c("colour", "fill"), guide) {
    scale_colour_gradient(low = shallow_col, high = deep_col, limits = depth_limits,
                          aesthetics = aesthetics, name = "Depth (m below MLLW)", guide = guide)
  }
  half <- (smooth_window - 1) %/% 2
  roll <- function(v) purrr::map_dbl(seq_along(v), ~ mean(v[max(1, .x - half):min(length(v), .x + half)]))

  fg <- points %>% arrange(transect, photo_index) %>% mutate(panel = panel_name(transect))
  bg <- points %>% mutate(panel = panel_name(3 - transect))
  route <- fg %>%
    group_by(panel) %>%
    mutate(NMDS1 = roll(NMDS1), NMDS2 = roll(NMDS2), depth_mllw_m = roll(depth_mllw_m)) %>%
    ungroup()

  ## arrowheads along each route: on the final step, and on the step nearest
  ## each third of the way along, so direction reads anywhere on the path
  heads <- route %>%
    group_by(panel) %>%
    mutate(step = row_number(), n = n(),
           x = lag(NMDS1), y = lag(NMDS2)) %>%
    filter(step %in% c(round(n / 3), round(2 * n / 3), n)) %>%
    ungroup()

  ends <- route %>%
    group_by(panel) %>%
    filter(photo_index %in% range(photo_index)) %>%
    mutate(start = photo_index == min(photo_index),
           label = ifelse(start, "start", "end")) %>%
    ungroup()

  lims_x <- range(c(points$NMDS1, vectors$xend, 0)) + c(-0.12, 0.12)
  lims_y <- range(c(points$NMDS2, vectors$yend, 0)) + c(-0.12, 0.12)

  paths <- ggplot() +
    geom_point(data = bg, aes(NMDS1, NMDS2), colour = "grey85", size = 1.6) +
    geom_path(data = fg, aes(NMDS1, NMDS2, colour = depth_mllw_m), linewidth = 0.35, alpha = 0.55) +
    geom_point(data = fg, aes(NMDS1, NMDS2, fill = depth_mllw_m),
               shape = 21, colour = "white", stroke = 0.4, size = 2.3) +
    geom_path(data = route, aes(NMDS1, NMDS2), colour = "white", linewidth = 3, lineend = "round") +
    geom_path(data = route, aes(NMDS1, NMDS2, colour = depth_mllw_m), linewidth = 1.6, lineend = "round") +
    geom_segment(data = heads, aes(x = x, y = y, xend = NMDS1, yend = NMDS2, colour = depth_mllw_m),
                 linewidth = 1.6, arrow = arrow(length = unit(0.14, "in"), type = "closed")) +
    ## "start" sits just right of and below the first point (clear of the
    ## y axis); "end" just below the last
    geom_label(data = ends, aes(NMDS1, NMDS2, label = label, hjust = ifelse(start, 0, 0.5)),
               size = 4.2, colour = ink, fill = scales::alpha("white", 0.85), linewidth = 0,
               label.padding = unit(0.06, "in"), fontface = "bold",
               nudge_x = ifelse(ends$start, 0.03, 0), nudge_y = -0.14) +
    depth_scale(guide = "none") +
    facet_wrap(~ panel, nrow = 1) +
    coord_fixed(xlim = lims_x, ylim = lims_y, expand = FALSE, clip = "off") +
    labs(x = "NMDS1  (deep → shallow)", y = "NMDS2") +
    slide.theme() +
    theme(panel.spacing = unit(0.35, "in"))

  ## labels sit beyond each arrow tip, on the side the arrow points to
  vec <- vectors %>%
    filter(r2 >= min_r2) %>%
    mutate(group = factor(variable, levels = names(cover_colors)),
           label_x = xend * 1.1, label_y = yend * 1.1,
           hjust = ifelse(xend >= 0, 0, 1),
           vjust = ifelse(abs(yend) > 2 * abs(xend), ifelse(yend > 0, 0, 1), 0.5))

  ## the compass panel also carries the depth colour bar (an invisible layer
  ## mapped to depth gives it the same scale as the path panels); it is
  ## collected into its own slot beneath the compass. The compass is drawn
  ## to its own extent -- only arrow directions and relative lengths matter.
  compass_lims <- list(x = range(c(0, vec$xend)) * 1.05, y = range(c(0, vec$yend)) * 1.05)
  compass <- ggplot(vec) +
    annotate("point", x = 0, y = 0, colour = ink_muted, size = 1.2) +
    geom_segment(aes(x = 0, y = 0, xend = xend, yend = yend, colour = group),
                 linewidth = 1.1, arrow = arrow(length = unit(0.1, "in"), type = "closed")) +
    geom_text(aes(x = label_x, y = label_y, label = variable, hjust = hjust, vjust = vjust),
              colour = ink, size = 4) +
    geom_point(data = tibble(x = 0, y = 0, depth = depth_limits), aes(x, y, fill = depth),
               size = 0, stroke = 0) +
    scale_colour_manual(values = cover_colors, guide = "none") +
    depth_scale(aesthetics = "fill",
                guide = guide_colourbar(barwidth = unit(2.4, "in"), barheight = unit(0.16, "in"),
                                        title.position = "top", direction = "horizontal")) +
    coord_fixed(xlim = compass_lims$x, ylim = compass_lims$y, clip = "off") +
    labs(title = "Where each group\nincreases") +
    theme_void(base_size = 14) +
    theme(plot.title = element_text(colour = ink, size = 16, face = "bold", hjust = 0.5,
                                    lineheight = 0.95, margin = margin(b = 14)),
          legend.title = element_text(colour = ink, size = 13),
          legend.text = element_text(colour = ink, size = 12),
          plot.margin = margin(4, 70, 4, 70))

  ## paths fill the left, full height; compass over the colour bar on the right
  wrap_plots(A = paths, B = compass, C = guide_area(), design = "AB\nAC") +
    plot_layout(widths = c(3.1, 1), heights = c(1.6, 1), guides = "collect") +
    plot_annotation(title = title, subtitle = subtitle, caption = caption,
                    theme = slide.theme() + theme(plot.margin = margin(12, 16, 8, 12)))
}


## zone-edge sharpness: the split-moving-window z-profile along each
## transect (zonation.R), with the z = 2 threshold and the CONISS zone
## boundaries drawn on the strips. x is cumulative distance at the split
## (midway between the two photos it separates).
visualize.smw.profile <- function(smw, photos, boundaries, title, subtitle, caption = NULL,
                                  z_crit = 2) {
  at.split <- function(df) {
    dist <- photos %>% select(transect, photo_index, cumulative_distance_m)
    df %>%
      left_join(dist, by = c("transect", "k" = "photo_index")) %>%
      left_join(dist %>% mutate(photo_index = photo_index - 1),
                by = c("transect", "k" = "photo_index"), suffix = c("", "_next")) %>%
      mutate(distance_m = (cumulative_distance_m + cumulative_distance_m_next) / 2,
             transect_label = paste("Transect", transect))
  }
  smw_d <- at.split(smw)
  bnd_d <- at.split(boundaries)

  ggplot(smw_d, aes(distance_m, mean_z)) +
    geom_hline(yintercept = 0, colour = "grey85", linewidth = 0.4) +
    geom_hline(yintercept = z_crit, colour = ink_muted, linewidth = 0.5, linetype = "dashed") +
    geom_vline(data = bnd_d, aes(xintercept = distance_m), colour = ink, linewidth = 0.6, linetype = "22") +
    geom_line(colour = "#2F5D8A", linewidth = 0.9) +
    geom_point(data = filter(smw_d, mean_z > z_crit), colour = "#2F5D8A", size = 2.6) +
    annotate("text", x = 0, y = z_crit, label = "abrupt-edge threshold (z = 2)",
             hjust = 0, vjust = -0.5, colour = ink_muted, size = 4) +
    facet_wrap(~ transect_label, ncol = 1) +
    labs(x = "Distance along transect (m)", y = "Edge strength (mean z)",
         title = title, subtitle = subtitle, caption = caption) +
    slide.theme() +
    theme(plot.margin = margin(12, 16, 8, 12), panel.spacing = unit(0.3, "in"))
}


## save a figure as PNG (slides) and PDF (vector) with one call. PNGs go
## through ragg, which draws embedded photos reliably on Windows.
save.figure <- function(plot, filename, width = 13.333, height = 7.5, dpi = 300) {
  ggsave(file.path(figs_dir, paste0(filename, ".png")), plot, width = width, height = height,
         dpi = dpi, device = ragg::agg_png)
  ## cairo_pdf: the default pdf() device can't draw non-Latin-1 glyphs (e.g. the arrow in "deep -> shallow")
  ggsave(file.path(figs_dir, paste0(filename, ".pdf")), plot, width = width, height = height,
         device = grDevices::cairo_pdf)
}




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
