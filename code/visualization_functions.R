## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Data visualizations functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## function to open and record plots 
my.window <- function(x, y){
  windows(x, y, record = TRUE)
}


## function to perform a log10transform across a range of columns
log.transform <- function(data, start_col, end_col) {
  data[, start_col:end_col] <- log10(data[, start_col:end_col] + 1)
  return(data)  
}


## inverse log transformation across specific cols 
inverse.log.transform <- function(data, start_col, end_col){
  data[, start_col:end_col] <- (10^data[, start_col:end_col]) - 1  # Apply transformation only to specified columns
  return(data)
}


## Function to multiple by 100
multiply.100 <- function(data, start_col, end_col) {
  data[, start_col:end_col] <- data[, start_col:end_col] * 100
  return(data)
}


## Function to multiple by 100
divide.100 <- function(data, start_col, end_col) {
  data[, start_col:end_col] <- data[, start_col:end_col] / 100
  return(data)
}



save.plot <- function(plot, filename, width, height) {
  # Save as PDF
  ggsave(filename = paste0(filename, ".pdf"), 
         plot = plot, 
         dpi = 1200, 
         width = width, 
         height = height, 
         units = "in")
  
  # Save as PNG
  ggsave(filename = paste0(filename, ".png"), 
         plot = plot, 
         dpi = 1200, 
         width = width, 
         height = height, 
         units = "in")
  
  message("Saved: ", filename, ".pdf and ", filename, ".png")
}







## plotting params ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my.theme = theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"),
                 axis.title=element_text(size=16),
                 axis.text=element_text(size=14),
                 plot.title = element_text(size=16))


## specify hex codes for custom colors
site.cols <- c(
  "#000000",  #gray; site 1
  "#008080",  #teal; site 2
  "#03396c",  #dark blue; site 5
  "#7BBF6A",  #light green; site 3
  "#FF9955",  #dark green; site 4
  "#CC6677",  #light maroon; site 6 
  "#882255",  #maroon; site 7
  "#7A378B"   #black 
)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## plot NMDS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## plot NMDS ordination with site labels & updated styling
plot.NMDS <- function(data){
  # Define site labels
  site.labs <- c(
    "1" = "Magnolia",
    "2" = "EBM West",
    "3" = "EBM Center",
    "4" = "EBM East",
    "5" = "Grain Elevator",
    "6" = "Sirens Spring",
    "7" = "Pocket Beach",
    "8" = "Coast Guard"
  )
  
  # Convert site column from numbers to factor with site names
  data <- data %>%
    mutate(site = factor(site, levels = names(site.labs), labels = site.labs))
  
  # Generate the NMDS plot
  p1 <- ggplot(data=data, aes(x=MDS1, y=MDS2)) +
    geom_point(size=2, aes(color=site), alpha=0.75) + my.theme +  
    scale_color_manual(values=site.cols, name = "Urban Kelp\nsurvey site") + 
    xlab("Axis 1") + ylab("Axis 2") + 
    ggtitle("2D NMDS ordination of 19 percent-cover categories\nacross 1011 ROV survey images, totaling 96,365 data pts") +
    coord_fixed() +  # Fixes aspect ratio
    theme(
      plot.title = element_text(size = 16, hjust = 0.5),
      legend.position = "right",  # Keeps legend on the right
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      legend.key.size = unit(1, "cm"),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14)
    )
  
  return(p1)
}


## plot NMDS ellipses
plot.NMDS.ellipses <- function(data){
  # Define site labels
  site.labs <- c(
    "1" = "Magnolia",
    "2" = "EBM West",
    "3" = "EBM Center",
    "4" = "EBM East",
    "5" = "Grain Elevator",
    "6" = "Sirens of Spring",
    "7" = "Pocket Beach",
    "8" = "Coast Guard Stn"
  )
  
  # Convert site column from numbers to factor with site names
  data <- data %>%
    mutate(site = factor(site, levels = names(site.labs), labels = site.labs))
  
  # Generate the NMDS plot
  p1 <- ggplot(data=data, aes(x=MDS1, y=MDS2)) +
    geom_point(size=2, alpha=0.15, aes(color=site)) + my.theme + 
    stat_ellipse(linewidth=1.2, aes(group=site, color=site), level=0.95) +
    scale_color_manual(values=site.cols, name = "Urban Kelp\nsurvey site") + 
    xlab("Axis 1") + ylab("Axis 2") + coord_fixed() + 
    ggtitle("2D NMDS ordination of 19 percent-cover categories across\n 1011 ROV survey images, totaling 96,365 data pts") +
    theme(
      legend.text = element_text(size = 14),  # Increases legend text size
      legend.title = element_text(size = 16),  # Increases & bolds legend title
      legend.key.size = unit(1, "cm"),  # Increases legend key size
      axis.title = element_text(size = 16),  # Increases x/y-axis title size
      axis.text = element_text(size = 14)  # Increases tick label size
    )
  
  return(p1)
}


## plot NMDS ordination with species correlation coefficients and outlined labels
plot.NMDS.spp.scores <- function(data){
  # Define site labels
  site.labs <- c(
    "1" = "Magnolia",
    "2" = "EBM West",
    "3" = "EBM Center",
    "4" = "EBM East",
    "5" = "Grain Elevator",
    "6" = "Sirens Spring",
    "7" = "Pocket Beach",
    "8" = "Coast Guard"
  )
  
  # Convert site column from numbers to factor with site names
  data <- data %>%
    mutate(site = factor(site, levels = names(site.labs), labels = site.labs))
  
  # Generate the NMDS plot
  p1 <- ggplot(data=data, aes(x=MDS1, y=MDS2)) +
    geom_point(size=2, alpha=0.15, aes(color=site)) + my.theme + 
    scale_color_manual(values=site.cols, name = "Urban Kelp\nsurvey site") + 
    geom_segment(data=spp_scores, linewidth=1.0,
                 aes(x=0, y=0, xend=NMDS1, yend=NMDS2), 
                 arrow=arrow(length=unit(0.2, "cm")), color="black") +
    
    # Updated: Use geom_label() with black outline
    geom_label(data=spp_scores, 
               aes(x=NMDS1, y=NMDS2, label=species, 
                   vjust=ifelse(NMDS2 >= 0, -0.5, 1.5)),  
               hjust=0.5, 
               size=5, color="black", fontface="bold", 
               fill="white", label.size=0.5, label.r=unit(0.1, "lines")) + 
    
    xlab("Axis 1") + ylab("Axis 2") + 
    ggtitle("NMDS ordination with species correlation coefficients") +
    coord_fixed() +  # Fixes aspect ratio
    theme(
      plot.title = element_text(size = 16, hjust = 0.5),
      legend.position = "right",  # Keeps legend on the right
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      legend.key.size = unit(1, "cm"),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14)
    )
  
  return(p1)
}
## END NMDS plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## Kernel densities ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to plot density for a single site, dynamically choosing the column to visualize
single.category.1.site <- function(data, column, site_number) {
  filtered_data <- data %>% filter(site == site_number)  # Filter for selected site
  p4 <- ggplot(filtered_data, aes(x={{column}}, fill=transect, group=transect)) +  # Use {{}} for dynamic columns
    geom_density(alpha=0.7, position="stack") +  # Stacked density plot
    scale_fill_viridis_d() +  # Automatically assigns colors to transects
    scale_x_continuous(name=paste(as_label(enquo(column)), "seafloor percent coverage"), 
                       breaks=seq(0, 100, by=20)) +  # X-axis label and tick marks every 10
    labs(y="kernel density", fill="Transect") +  # Y-axis label
    my.theme  # Apply custom theme
  return(p4)
}


## plot one label for all 8 sites 
single.category.8.sites <- function(data, column) {

    site.labs <- c(
    "1" = "Magnolia",
    "2" = "EBM Breakwater West",
    "3" = "EBM Breakwater Center",
    "4" = "EBM Breakwater East",
    "5" = "Grain Elevator",
    "6" = "Sirens of Spring",
    "7" = "Pocket Beach",
    "8" = "Coast Guard Station"
  )
  
  data <- data %>%
    mutate(site = factor(site, levels = names(site.labs), labels = site.labs))  # Ensure correct site mapping
  
  p <- ggplot(data, aes(x={{column}}, fill=transect, group=transect)) +  
    geom_density(alpha=0.7, position="stack") +  
    scale_fill_viridis_d() +  
    scale_x_continuous(name=paste(as_label(enquo(column)), "seafloor percent coverage"), breaks=seq(0, 100, by=25)) +  
    labs(y="Kernel Density", fill="Transect") +  
    my.theme +  
    facet_wrap(~ site, nrow=2, ncol=4, scales="free_y") +  # Ensure correct facet labeling
    theme(
      strip.text = element_text(size = 16),#, face = "bold"),  # Facet label text size
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(), # Remove y-axis ticks
      legend.text = element_text(size = 16),  # Legend text size
      legend.title = element_text(size = 16)  # Legend title size
    )
  
  return(p)
}


## single category for all 8 sites, looped through cateogory
all.sites <- function(data, labels, save_path = "figs", width = 10, height = 7) {

  site.labs <- c(
    "1" = "Magnolia",
    "2" = "EBM Breakwater West",
    "3" = "EBM Breakwater Center",
    "4" = "EBM Breakwater East",
    "5" = "Grain Elevator",
    "6" = "Sirens of Spring",
    "7" = "Pocket Beach",
    "8" = "Coast Guard Station"
  )

    data <- data %>%
    mutate(site = factor(site, levels = names(site.labs), labels = site.labs))  
  
  for (col_name in labels) {
    column_sym <- sym(col_name)
    
    pdf_file <- file.path(save_path, paste0("8_sites_kernel_", col_name, ".pdf"))
    png_file <- file.path(save_path, paste0("8_sites_kernel_", col_name, ".png"))
    
    p <- ggplot(data, aes(x = !!column_sym, fill = transect, group = transect)) +  
      geom_density(alpha = 0.7, position = "stack") +  
      scale_fill_viridis_d() +  
      scale_x_continuous(name = paste(col_name, "seafloor percent coverage"), 
                         breaks = seq(0, 100, by = 25)) +  
      labs(y = "Kernel Density", fill = "Transect") +  
      my.theme +  
      facet_wrap(~ site, nrow = 2, ncol = 4, scales = "free_y") +  
      theme(
        strip.text = element_text(size = 16),  
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        legend.text = element_text(size = 16),  
        legend.title = element_text(size = 16)
      )
    
    ggsave(filename = pdf_file, plot = p, width = width, height = height, units = "in", dpi = 1200)
    ggsave(filename = png_file, plot = p, width = width, height = height, units = "in", dpi = 1200)
    message("Saved: ", pdf_file, " and ", png_file)
  }
}

# Run the function to generate PDFs for all categories
# save.all.categories.kernel(log, labels, width = 12, height = 8)





## plot all categories for a single site
all.categories.1.site <- function(data, site_number) {

    reordered_labels <- c(
    "sugar_kelp", "red_algae", "shell_debris",
    "textured_kelp", "green_algae", "pebble",
    "unknown", "coralline_algae", "soft_sediment",
    "filamentous_brown", "kelp_bryozoan", "hard_substrate",
    "sargassum", "mobile_invert", "cobble",
    "other_brown_kelp", "sessile_invert", "anthro_substrate"
  )
  
  category_order <- tibble(
    category = reordered_labels,
    order = 1:length(reordered_labels)  # Assign numeric order for verification
  )
  
  filtered_data <- data %>% filter(site == site_number)
  
  long_data <- filtered_data %>%
    select(site, transect, all_of(reordered_labels)) %>%
    pivot_longer(cols = all_of(reordered_labels), names_to = "category", values_to = "value") %>%
    left_join(category_order, by = "category")  # Merge explicit ordering
  
  long_data$category <- factor(long_data$category, levels = reordered_labels)
  
  p <- ggplot(long_data, aes(x=value, fill=transect, group=transect)) +  
    geom_density(alpha=0.7, position="stack") +  
    scale_fill_viridis_d() +  
    scale_x_continuous(name="Seafloor Percent Coverage", breaks=seq(0, 100, by=25)) +  
    labs(y="Kernel Density", fill="Transect") +  
    my.theme +  
    facet_wrap(~ category, nrow=6, ncol=3, scales="free_y") +  # Uses correct category order
    theme(
      strip.text = element_text(size = 13),  # Facet label text size
      axis.text.y = element_blank(),  # Remove y-axis numbers
      axis.ticks.y = element_blank(), # Remove y-axis ticks
      legend.text = element_text(size = 16),  # Legend text size
      legend.title = element_text(size = 16)  # Legend title size
    )
  
  return(p)
}


## all categories for a single site looped through all sites, pdf and pngs saved
all.categories <- function(data, save_path = "figs", width = 10, height = 7) {

    reordered_labels <- c(
    "sugar_kelp", "red_algae", "shell_debris",
    "textured_kelp", "green_algae", "pebble",
    "unknown", "coralline_algae", "soft_sediment",
    "filamentous_brown", "kelp_bryozoan", "hard_substrate",
    "sargassum", "mobile_invert", "cobble",
    "other_brown_kelp", "sessile_invert", "anthro_substrate"
  )
  
  site.labs <- c(
    "1" = "Magnolia",
    "2" = "EBM Breakwater West",
    "3" = "EBM Breakwater Center",
    "4" = "EBM Breakwater East",
    "5" = "Grain Elevator",
    "6" = "Sirens of Spring",
    "7" = "Pocket Beach",
    "8" = "Coast Guard Station"
  )
  
  for (site_number in 1:8) {
    filtered_data <- data %>% filter(site == site_number)
    
    long_data <- filtered_data %>%
      select(site, transect, all_of(reordered_labels)) %>%
      pivot_longer(cols = all_of(reordered_labels), names_to = "category", values_to = "value") %>%
      mutate(category = factor(category, levels = reordered_labels))
    
    p <- ggplot(long_data, aes(x=value, fill=transect, group=transect)) +  
      geom_density(alpha=0.7, position="stack") +  
      scale_fill_viridis_d() +  
      scale_x_continuous(name="seafloor log10(x+1) percent coverage", breaks=seq(0, 2, by=0.50)) +  
      #scale_x_continuous(name="seafloor percent coverage", breaks=seq(0, 100, by=25)) +  
      labs(y="kernel density", fill="transect") +  
      my.theme +  
      facet_wrap(~ category, nrow=6, ncol=3, scales="free_y") +  
      theme(
        strip.text = element_text(size = 16),  
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        legend.text = element_text(size = 16),  
        legend.title = element_text(size = 16)
      )
    
    pdf_file <- file.path(save_path, paste0("site_", site_number, "_all_categories_kernel_log.pdf"))
    png_file <- file.path(save_path, paste0("site_", site_number, "_all_categories_kernel_log.png"))
    ggsave(filename = pdf_file, plot = p, width = width, height = height, units = "in", dpi = 1200)
    ggsave(filename = png_file, plot = p, width = width, height = height, units = "in", dpi = 1200)
    message("Saved: ", pdf_file, " and ", png_file)
  }
}
## END of plotting kernel densities ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
