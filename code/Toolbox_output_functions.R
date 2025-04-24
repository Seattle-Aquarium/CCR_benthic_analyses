## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## functions to wrangle data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## function to bring new column to the [1] position 
front.ofthe.line <- function(data){
  num.col <- ncol(data)
  data <- data[c(num.col, 1:num.col-1)]
  return(data)
}


## function to filter by group 
filter.out.group <- function(df, col, group) {
  df %>% 
    filter(.data[[col]] != group)
}


## function to rename columns
rename.columns <- function(data, old, new) {
  names(data)[names(data) %in% old] <- new
  return(data)
}


## function to add "1" to all the blank cells in a column
replace.with.1 <- function(df, column_name) {
  df[[column_name]][is.na(df[[column_name]])] <- 1
  return(df)
}


## function to add "manual_update" to all the blank cells in a column
add.text.to.cell <- function(df, column_name) {
  df[[column_name]][df[[column_name]] == ""] <- "manual_update"
  return(df)
}


## function to calculate all x4 image patch boundaries
calculate.patch.bounds <- function(df) {
  half_size <- df$patch_size / 2
  df$left_column   <- df$midpoint_column - half_size
  df$right_column  <- df$midpoint_column + half_size
  df$top_row       <- df$midpoint_row - half_size
  df$bottom_row    <- df$midpoint_row + half_size
  return(df)
}


## function to rearrange the order of columns 
reorder.columns <- function(df, column_order) {
  df <- df[, column_order, drop = FALSE]
  return(df)
}


## function to plot image patches based on newly calculated boundings
plot.boxes.on.image <- function(df, image_dir) {

  img_path <- file.path(image_dir, "image_1.jpg")
  img <- readJPEG(img_path)
  img_grob <- rasterGrob(img, width = unit(1,"npc"), height = unit(1,"npc"), interpolate = TRUE)
  
  p <- ggplot() +
    annotation_custom(img_grob, xmin = 0, xmax = ncol(img), ymin = 0, ymax = nrow(img)) +
    coord_fixed(ratio = 1, xlim = c(0, ncol(img)), ylim = c(nrow(img), 0)) +
    theme_void()
  
  p <- p + geom_rect(
    data = df,
    aes(xmin = left_column, xmax = right_column,
        ymin = top_row, ymax = bottom_row),
    color = "white", fill = NA, linewidth = 0.8
  )
  
  print(p)
}


## create an output dataframe that summarizes preditions
summarize.predictions <- function(dat) {
  
  summary_df <- dat |>
    dplyr::group_by(label_code) |>
    dplyr::summarise(
      label_name = dplyr::first(label_name),
      total_count = dplyr::n(),
      correct_prediction = sum(machine_prediction != "manual_update"),
      manually_updated = sum(machine_prediction == "manual_update"),
      proportion_correct = round(correct_prediction / total_count, 2),
      proportion_revised = round(manually_updated / total_count, 2),
      percent_confidence = round(median(confidence[machine_prediction != "manual_update"], na.rm = TRUE), 2),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(total_count)) |>
    dplyr::select(label_name, label_code, total_count, correct_prediction, manually_updated,
                  proportion_correct, proportion_revised, percent_confidence)
  
  return(summary_df)
}


## custom graphing theme
my.theme <- theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  axis.title = element_text(size = 16),
                  axis.text = element_text(size = 14),
                  plot.title = element_text(size = 16))


## plot kernel densities, within min_n specifying minimum # of observations
plot.kernels.by.group <- function(df, col, min_n = 20) {
  # Step 1: Filter and order groups with more than `min_n` observations
  group_counts <- df %>%
    group_by(label_name) %>%
    tally(name = "n") %>%
    filter(n > min_n) %>%
    arrange(desc(n))
  
  # Step 2: Filter data and set factor levels
  df_filtered <- df %>%
    filter(label_name %in% group_counts$label_name) %>%
    mutate(label_name = factor(label_name, levels = group_counts$label_name))
  
  # Step 3: Determine global x-axis lower bound
  x_min <- min(df_filtered[[col]], na.rm = TRUE)
  x_max <- 1.0  # Fixed upper bound
  
  # Step 4: Build each plot
  plots <- lapply(seq_along(levels(df_filtered$label_name)), function(i) {
    label <- levels(df_filtered$label_name)[i]
    df_group <- df_filtered %>% filter(label_name == label)
    count <- nrow(df_group)
    med <- round(median(df_group[[col]], na.rm = TRUE), 2)
    
    row_index <- ceiling(i / 4)
    col_index <- (i - 1) %% 4 + 1
    
    # Build base plot and extract y_max
    p_base <- ggplot(df_group, aes_string(x = col)) +
      geom_density(fill = "#00688B", alpha = 0.6)
    built <- ggplot_build(p_base)
    y_max <- max(built$data[[1]]$y, na.rm = TRUE)
    
    # Final plot
    p <- p_base +
      labs(title = NULL,
           x = ifelse(row_index == 4, col, ""),
           y = ifelse(col_index == 1, "Density", "")) +
      annotate("text", x = -Inf, y = y_max,
               label = paste0(label, "\n", "n = ", count, "\n", "median = ", med),
               hjust = -0.1, vjust = 1.1, size = 4.8, fontface = "plain") +
      coord_cartesian(xlim = c(x_min, x_max), clip = "off") +
      my.theme +
      theme(plot.margin = margin(t = 15, r = 5, b = 5, l = 5))
    
    if (col_index != 1) {
      p <- p + theme(axis.text.y = element_blank(),
                     axis.ticks.y = element_blank())
    }
    if (row_index != 4) {
      p <- p + theme(axis.text.x = element_blank(),
                     axis.ticks.x = element_blank())
    }
    
    return(p)
  })
  
  plot_grid(plotlist = plots, ncol = 4)
}


## plot a frequency histogram function
plot.freq.hist <- function(df, group, count, plot_title = NULL) {
  df %>%
    group_by(.data[[group]]) %>%
    summarise(total = sum(.data[[count]], na.rm = TRUE)) %>%
    arrange(desc(total)) %>%
    mutate({{ group }} := factor(.data[[group]], levels = .data[[group]])) %>%
    ggplot(aes(x = .data[[group]], y = total)) +
    geom_col(fill = "#00688B") +
    labs(x = group, y = "Count", title = plot_title) +
    my.theme +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
      plot.title = element_text(hjust = 0, size = 18, face = "bold")
    )
}
## END functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
