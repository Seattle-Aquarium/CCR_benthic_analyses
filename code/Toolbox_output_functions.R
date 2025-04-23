## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## functions to wrangle data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## function to bring new column to the [1] position 
front.ofthe.line <- function(data){
  num.col <- ncol(data)
  data <- data[c(num.col, 1:num.col-1)]
  return(data)
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
calculate_patch_bounds <- function(df) {
  half_size <- df$patch_size / 2
  
  df$left_column   <- df$midpoint_column - half_size
  df$right_column  <- df$midpoint_column + half_size
  df$top_row       <- df$midpoint_row - half_size
  df$bottom_row    <- df$midpoint_row + half_size
  
  return(df)
}


## function to rearrange the order of columns 
reorder_columns <- function(df, column_order) {
  df <- df[, column_order, drop = FALSE]
  return(df)
}


## function to plot image patches based on newly calculated boundings
plot_boxes_on_image <- function(df, image_dir) {

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


## function to generate summarized output for Toolbox
summarize.predictions <- function(dat) {
  
  summary_df <- dat |>
    dplyr::group_by(label_code) |>
    dplyr::summarise(
      total_count = dplyr::n(),
      correct_prediction = sum(machine_prediction != "manual_update"),
      manually_updated = sum(machine_prediction == "manual_update"),
      proportion_correct = round(correct_prediction / total_count, 2),
      proportion_revised = round(manually_updated / total_count, 2),
      percent_confidence = round(median(confidence[machine_prediction != "manual_update"], na.rm = TRUE), 2),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(total_count)) |>
    dplyr::select(label_code, total_count, correct_prediction, manually_updated,
                  proportion_correct, proportion_revised, percent_confidence)
  
  return(summary_df)
}
## END functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
