# List of category folders
category_list <- c(
  "BR_sarg",
  "KE_5rib",
  "KE_sieve",
  "KE_sugar",
  "MS",
  "SU_bould",
  "SU_cob",
  "SU_peb",
  "SU_sand",
  "SU_shell",
  "SU_silt",
  "unknown",
  "KE_stipe",
  "KE_holdfas",
  "RE_CCA",
  "RE_branch",
  "RE_bush",
  "RE_leaf",
  "GR_ulva",
  "RE_fil",
  "SI_kelpBry"
)


title.names <- c(
  "BR_sarg" = "Sargassum",
  "KE_5rib" = "Five-ribbed kelp",
  "KE_sieve" = "Sieve kelp",
  "KE_sugar" = "Sugar kelp",
  "MS" = "Mobile species",
  "SU_bould" = "Boulder substrate",
  "SU_cob" = "Cobble substrate",
  "SU_peb" = "Pebble substrate",
  "SU_sand" = "Sand substrate",
  "SU_shell" = "Shell debris substrate",
  "SU_silt" = "Silt substrate",
  "unknown" = "Unknown",
  "KE_stipe" = "Kelp stipe",
  "KE_holdfas" = "Kelp holdfast",
  "RE_CCA" = "Crustose coralline algae",
  "RE_branch" = "Flat branching red algae",
  "RE_bush" = "Bushy/cylindrical red algae",
  "RE_leaf" = "Leafy red algae",
  "GR_ulva" = "Ulva",
  "RE_fil" = "Filamentous red algae",
  "SI_kelpBry" = "Colonial kelp bryozoan"
)


# Function to create and save an image grid for a single category with a custom title
single.category.image.grid <- function(category, n = 36, grid_dims = c(6, 6), img_size = "224x224") {
  
  image_folder <- file.path(patches, category)
  folder_name <- basename(image_folder)  
  image_files <- list.files(image_folder, full.names = TRUE, pattern = "\\.(jpg|png|jpeg)$", ignore.case = TRUE)
  
  if (length(image_files) < n) {
    stop(paste("Not enough images in", image_folder, "to sample", n, "images"))
  }
  
  selected_images <- sample(image_files, n)
  images <- lapply(selected_images, image_read)
  
  # Resize all images and add black borders to create gaps
  padded_images <- lapply(images, function(img) {
    img <- image_resize(img, img_size)
    img <- image_border(img, "black", "4x4")  # Adds black padding around each image
    return(img)
  })
  
  # Arrange images into rows
  rows <- split(padded_images, rep(1:grid_dims[1], each = grid_dims[2]))
  row_images <- lapply(rows, function(row) image_append(image_join(row), stack = FALSE))  # Horizontally align
  
  # Stack rows vertically to form a grid
  final_image <- image_append(image_join(row_images), stack = TRUE)  # Stack rows vertically
  
  # Retrieve the correct title name from title.names
  title_text <- ifelse(category %in% names(title.names), title.names[category], category)
  
  # Create a title image with black background and white text
  title_height <- 100
  title_image <- image_blank(width = image_info(final_image)$width, height = title_height, color = "black") %>%
    image_annotate(text = title_text, size = 75, color = "white", gravity = "center")
  
  # Stack the title above the grid image
  final_with_title <- image_append(c(title_image, final_image), stack = TRUE)
  
  # Ensure output directory exists
  if (!dir.exists(output)) dir.create(output, recursive = TRUE)
  
  # Define output filenames inside the output directory
  png_file <- file.path(output, paste0(folder_name, ".png"))
  pdf_file <- file.path(output, paste0(folder_name, ".pdf"))
  
  image_write(final_with_title, path = png_file, format = "png")
  pdf(pdf_file, width = grid_dims[2] * 2, height = (grid_dims[1] * 2) + 1)
  plot(final_with_title)  
  dev.off()
  
  print(paste("Image grid saved as:", png_file, "and", pdf_file))
}




# Function to create and save image grids for multiple categories
create.image.grid <- function(category, n = 36, grid_dims = c(6, 6), img_size = "224x224") {
  
  image_folder <- file.path(patches, category)
  folder_name <- basename(image_folder)  
  image_files <- list.files(image_folder, full.names = TRUE, pattern = "\\.(jpg|png|jpeg)$", ignore.case = TRUE)
  
  if (length(image_files) < n) {
    stop(paste("Not enough images in", image_folder, "to sample", n, "images"))
  }
  
  selected_images <- sample(image_files, n)
  images <- lapply(selected_images, image_read)
  
  # Resize and add black borders to create gaps
  padded_images <- lapply(images, function(img) {
    img <- image_resize(img, img_size)
    img <- image_border(img, "black", "4x4")  # Adds black padding around each image
    return(img)
  })
  
  # Arrange images into rows
  rows <- split(padded_images, rep(1:grid_dims[1], each = grid_dims[2]))
  row_images <- lapply(rows, function(row) image_append(image_join(row), stack = FALSE))  # Horizontally align
  
  # Stack rows vertically to form a grid
  final_image <- image_append(image_join(row_images), stack = TRUE)  # Stack rows vertically
  
  # Retrieve the correct title name from title.names
  title_text <- ifelse(category %in% names(title.names), title.names[category], category)
  
  # Create a title image with black background and white text
  title_height <- 100
  title_image <- image_blank(width = image_info(final_image)$width, height = title_height, color = "black") %>%
    image_annotate(text = title_text, size = 75, color = "white", gravity = "center")
  
  # Stack the title above the grid image
  final_with_title <- image_append(c(title_image, final_image), stack = TRUE)
  
  # Ensure output directory exists
  if (!dir.exists(output)) dir.create(output, recursive = TRUE)
  
  # Define output filenames inside the output directory
  png_file <- file.path(output, paste0(folder_name, ".png"))
  pdf_file <- file.path(output, paste0(folder_name, ".pdf"))
  
  image_write(final_with_title, path = png_file, format = "png")
  pdf(pdf_file, width = grid_dims[2] * 2, height = (grid_dims[1] * 2) + 1)
  plot(final_with_title)  
  dev.off()
  
  print(paste("Image grid saved as:", png_file, "and", pdf_file))
}
















