## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## script to extract patches and arrange in image grid ~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## startup
library(magick)


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
  "GR_ulva",
  "RE_fil",
  "SI_kelpBry"
)


## Function to create and save an image grid for a single category
single.category.image.grid <- function(image_folder, n = 36, grid_dims = c(6, 6)) {
  
  folder_name <- basename(image_folder)  
  image_files <- list.files(image_folder, full.names = TRUE, pattern = "\\.(jpg|png|jpeg)$", ignore.case = TRUE)
  selected_images <- sample(image_files, n)
  images <- lapply(selected_images, image_read)
  
  final_image <- image_montage(image_join(images), 
                               tile = paste(grid_dims[2], "x", grid_dims[1], sep = ""), 
                               geometry = "224x224+2+2")
  
  png_file <- paste0(folder_name, ".png")
  pdf_file <- paste0(folder_name, ".pdf")
  image_write(final_image, path = png_file, format = "png")
  pdf(pdf_file, width = grid_dims[2] * 2, height = grid_dims[1] * 2)  
  plot(final_image)  
  dev.off()
  
  print(paste("Image grid saved as:", png_file, "and", pdf_file))
}


## invoke function
single.category.image.grid(image_folder = "KE_stipe", n = 100, grid_dims = c(8, 12))





## function to create and save an image grid for multiple categories
create.image.grid <- function(image_folder, n = 36, grid_dims = c(6, 6)) {
  
  folder_name <- basename(image_folder)  
  image_files <- list.files(image_folder, full.names = TRUE, pattern = "\\.(jpg|png|jpeg)$", ignore.case = TRUE)
  selected_images <- sample(image_files, n)
  images <- lapply(selected_images, image_read)
  
  final_image <- image_montage(image_join(images), 
                               tile = paste(grid_dims[2], "x", grid_dims[1], sep = ""), 
                               geometry = "224x224+2+2")
  
  png_file <- paste0(folder_name, ".png")
  pdf_file <- paste0(folder_name, ".pdf")
  image_write(final_image, path = png_file, format = "png")
  pdf(pdf_file, width = grid_dims[2] * 2, height = grid_dims[1] * 2)  
  plot(final_image)  
  dev.off()
  
  print(paste("Image grid saved as:", png_file, "and", pdf_file))
}


# Loop through each category in category_list and apply the function
for (category in category_list) {
  create.image.grid(image_folder = category, n = 36, grid_dims = c(6, 6))
}


















