## script to contain functions for analyzing / visualizing data




## prefered graphing theme 
my.theme = theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"),
                 axis.title=element_text(size=16),
                 axis.text=element_text(size=14),
                 plot.title = element_text(size=16))


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


## function to visualize diffs between ROV - diver percent-cover data
visualize.percent.cover.pairs <- function(x_axis, y_axis, colnames, colors,
                                          axis_limit = 10,
                                          x_label = deparse(substitute(x_axis)),
                                          y_label = deparse(substitute(y_axis))) {

  # Create a combined long-form dataframe
  plot_data <- do.call(rbind, lapply(seq_along(colnames), function(i) {
    data.frame(
      x = x_axis[[colnames[i]]],
      y = y_axis[[colnames[i]]],
      group = colnames[i],
      color = colors[i]
    )
  }))
  
  # Plot
  ggplot(plot_data, aes(x = x, y = y, color = group)) +
    geom_point(show.legend = FALSE, size = 2) +
    geom_abline(slope = 1, intercept = 0, color = "gray40") +
    coord_fixed(ratio = 1, xlim = c(0, axis_limit), ylim = c(0, axis_limit)) +
    scale_color_manual(values = unique(plot_data$color)) +
    facet_wrap(~ group, scales = "fixed") +
    labs(x = x_label, y = y_label) +
    my.theme
}
