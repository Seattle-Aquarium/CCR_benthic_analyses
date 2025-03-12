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


## function to save a plot 
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
## END start up functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





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
      plot.title = element_text(size = 16),
      legend.position = "right",  # Keeps legend on the right
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      legend.key.size = unit(1, "cm"),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14)
    )
  
  return(p1)
}


## plot NMDS ordination with site labels & updated styling, with dynamic size mapping, size range, and title
plot.NMDS.size <- function(data, size_col, min_size = 1, max_size = 5){
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
  
  # Generate dynamic title
  dynamic_title <- paste(size_col, "percent-cover per image")
  
  # Generate the NMDS plot with dynamic size mapping, size range, and title
  p1 <- ggplot(data=data, aes(x=MDS1, y=MDS2, size=.data[[size_col]], color=site)) +
    geom_point(alpha=0.75) + 
    scale_size(range = c(min_size, max_size)) +  # Dynamically adjust point sizes
    my.theme +  
    scale_color_manual(values=site.cols, name = "Urban Kelp\nsurvey site") + 
    xlab("Axis 1") + ylab("Axis 2") + 
    ggtitle(dynamic_title) +  # Set dynamic title
    coord_fixed() +  # Fixes aspect ratio
    theme(
      plot.title = element_text(size = 16),
      legend.position = "right",  # Keeps legend on the right
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      legend.key.size = unit(1, "cm"),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14)
    )
  
  return(p1)
}


## plot NMDS ordination with only the size legend
plot.NMDS.size.legend <- function(data, size_col, min_size = 1, max_size = 5) {
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
  
  # Generate dynamic title
  dynamic_title <- paste(size_col, "percent-cover per image")
  
  # Generate the NMDS plot with only the size legend
  p1 <- ggplot(data = data, aes(x = MDS1, y = MDS2, size = .data[[size_col]])) +
    geom_point(alpha = 0.75, color = "black") +  # Removes color legend by setting all points to gray
    scale_size(range = c(min_size, max_size), name = size_col) +  # Keeps only the size legend
    my.theme +  
    xlab("Axis 1") + ylab("Axis 2") + 
    ggtitle(dynamic_title) +  # Set dynamic title
    coord_fixed() +  # Fixes aspect ratio
    theme(
      plot.title = element_text(size = 16),
      legend.position = "right",  # Keeps only the size legend
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      legend.key.size = unit(1, "cm"),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14)
    ) +
    guides(color = "none")  # Explicitly removes the color legend
  
  return(p1)
}


## plot NMDS ordination with site labels & updated styling, without legend
plot.NMDS.size.noLegend <- function(data, size_col, min_size = 1, max_size = 5){
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
  
  # Generate dynamic title
  dynamic_title <- paste(size_col, "seafloor percent-cover")
  
  # Generate the NMDS plot without legend
  p1 <- ggplot(data=data, aes(x=MDS1, y=MDS2, size=.data[[size_col]], color=site)) +
    geom_point(alpha=0.75) + 
    scale_size(range = c(min_size, max_size)) +  # Dynamically adjust point sizes
    my.theme +  
    scale_color_manual(values=site.cols) +  # Removed legend title
    xlab("Axis 1") + ylab("Axis 2") + 
    ggtitle(dynamic_title) +  # Set dynamic title
    coord_fixed() +  # Fixes aspect ratio
    theme(
      plot.title = element_text(size = 16),  # Title size defined in function
      legend.position = "none",  # Removes the legend
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 15)
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
    #ggtitle("2D NMDS ordination of 19 percent-cover categories across\n 1011 ROV survey images, totaling 96,365 data pts") +
    theme(
      legend.text = element_text(size = 14),  # Increases legend text size
      legend.title = element_text(size = 16),  # Increases & bolds legend title
      legend.key.size = unit(1, "cm"),  # Increases legend key size
      axis.title = element_text(size = 16),  # Increases x/y-axis title size
      axis.text = element_text(size = 14)  # Increases tick label size
    )
  
  return(p1)
}


## plot NMDS ellipses (legend removed)
plot.NMDS.ellipses.noLegend <- function(data){
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
  
  data <- data %>%
    mutate(site = factor(site, levels = names(site.labs), labels = site.labs))

    p1 <- ggplot(data=data, aes(x=MDS1, y=MDS2)) +
    geom_point(size=1, alpha=0.2, aes(color=site)) + my.theme + 
    stat_ellipse(linewidth=0.75, aes(group=site, color=site), level=0.95) +
    scale_color_manual(values=site.cols) + 
    xlab("Axis 1") + ylab("Axis 2") + coord_fixed() + 
    ggtitle("95% ellipses around each site") +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 16),
      axis.title = element_text(size = 16),  # Increases x/y-axis title size
      axis.text = element_text(size = 15)  # Increases tick label size
    )
  
  return(p1)
}


## plot NMDS ellipses with multiple site selection (by number)
plot.select.ellipses <- function(data, highlight_sites){
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
  
  data <- data %>%
    mutate(site = factor(site, levels = names(site.labs), labels = site.labs))
   highlight_site_names <- site.labs[highlight_sites]
  
  if (any(is.na(highlight_site_names))) {
    stop("Invalid site numbers provided. Must be one or more of: ", paste(names(site.labs), collapse = ", "))
  }
  
  site.cols.named <- setNames(site.cols, site.labs)
  data <- data %>%
    mutate(point_color = ifelse(site %in% highlight_site_names, as.character(site), "gray"),
           point_alpha = ifelse(site %in% highlight_site_names, 1, 0.15))  # Lower alpha for other points
  
  site_colors <- site.cols.named  # Copy original colors with site names
  site_colors["gray"] <- "gray"   # Add gray for non-highlighted sites
  
  p1 <- ggplot(data=data, aes(x=MDS1, y=MDS2)) +
    geom_point(size=2, aes(color=point_color, alpha=point_alpha)) + 
    scale_color_manual(values = site_colors, name = "Urban Kelp\nsurvey site") +  # Single scale
    scale_alpha_identity() +  # Use alpha directly from data
    stat_ellipse(data = data %>% filter(site %in% highlight_site_names), 
                 aes(group = site, color = site), linewidth = 1.2, level = 0.95) +
    xlab("Axis 1") + ylab("Axis 2") + coord_fixed() + 
    ggtitle("2D NMDS ordination of 19 percent-cover categories across\n 1011 ROV survey images, totaling 96,365 data pts") +
    my.theme +
    theme(
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      legend.key.size = unit(1, "cm"),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14)
    )
  
  return(p1)
}


## plot NMDS by specific path 
plot.NMDS.by.site <- function(data, save_path = "figs/19_labels/ordinations/sites", width = 8, height = 6, axis_offset = 0.3) {
  # Define site labels (now using numbers)
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
  
  # Ensure site column is a factor with numeric labels
  data <- data %>%
    mutate(site = factor(site, levels = names(site.labs)))
  
  site.cols.named <- setNames(site.cols, names(site.labs))  # Use site numbers as names
  
  # Ensure the save directory exists
  if (!dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
    message("Created directory: ", save_path)
  }
  
  # Define fixed x and y limits across all figures
  xlims <- range(data$MDS1, na.rm = TRUE) + c(-axis_offset, axis_offset)
  ylims <- range(data$MDS2, na.rm = TRUE) + c(-axis_offset, axis_offset)
  
  xbreaks <- seq(floor(xlims[1]), ceiling(xlims[2]), by = 0.5)
  ybreaks <- seq(floor(ylims[1]), ceiling(ylims[2]), by = 0.5)
  
  # Iterate over each site and create separate plots
  for (site_num in names(site.labs)) {
    message("Processing site: ", site_num)
    
    # Filter data for the specific site
    site_data <- data %>% filter(site == site_num)
    
    # Assign colors: Highlighted site retains color, other sites remain gray
    data <- data %>%
      mutate(point_color = ifelse(site == site_num, as.character(site), "gray"),
             point_alpha = ifelse(site == site_num, 1, 0.15))  # Lower alpha for other sites
    
    site_colors <- c(site.cols.named, "gray" = "gray")
    
    p <- ggplot(data, aes(x = MDS1, y = MDS2)) +
      geom_point(size = 2, aes(color = point_color, alpha = point_alpha)) + 
      scale_color_manual(values = site_colors, name = "Site",
                         breaks = names(site.labs),  # Ensure all site numbers are in legend
                         labels = names(site.labs)) +  # Use site numbers instead of names
      scale_alpha_identity() + 
      stat_ellipse(data = site_data, aes(group = site, color = site), linewidth = 1.2, level = 0.95) +
      coord_fixed(ratio = 1, xlim = xlims, ylim = ylims, expand = FALSE) +  # Maintain aspect ratio
      scale_x_continuous(breaks = xbreaks, name = "Axis 1") +
      scale_y_continuous(breaks = ybreaks, name = "Axis 2") +
      ggtitle(paste0("2D NMDS ordination for Site ", site_num)) +
      my.theme +
      theme(
        legend.text = element_text(size = 14),  # Fixed text size
        legend.title = element_text(size = 16),
        legend.key.size = unit(1.5, "cm"),  # Lock legend key size
        legend.spacing.y = unit(0.5, "cm"),  # Fixed vertical spacing
        legend.box.margin = margin(10, 10, 10, 10),  # Fixes legend box size
        legend.position = "right",  # Keep legend in the same position
        legend.box = "vertical",  # Keep legend layout consistent
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 18)
      ) +
      guides(color = guide_legend(ncol = 1))  # Keeps legend in a single-column format
    
    # Save plots
    pdf_path <- file.path(save_path, paste0("NMDS_Site_", site_num, ".pdf"))
    png_path <- file.path(save_path, paste0("NMDS_Site_", site_num, ".png"))
    
    ggsave(filename = pdf_path, plot = p, width = width, height = height)
    ggsave(filename = png_path, plot = p, width = width, height = height, dpi = 300)
    
    message("Saved plots for site: ", site_num)
  }
  
  message("All plots saved successfully.")
}


## Fix NMDS plot consistency across axes and legends, ensuring ellipses fit
plot.NMDS.transects <- function(data, highlight_site){
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
  
  data <- data %>%
    mutate(site = factor(site, levels = names(site.labs), labels = site.labs))
  highlight_site_name <- site.labs[highlight_site]
  unique_transects <- unique(data$transect[data$site == highlight_site_name])
  transect_colors <- if (length(unique_transects) == 2) {
    c("#7c4e88", "#64b2af")  # Purple & Blue
  } else {
    c("#7c4e88", "#64b2af", "#feee67")  # Purple, Blue, Yellow
  }
  transect_colors <- setNames(transect_colors[1:length(unique_transects)], unique_transects)
  data <- data %>%
    mutate(point_color = ifelse(site == highlight_site_name, as.character(transect), "gray"),
           point_alpha = ifelse(site == highlight_site_name, 1, 0.15))  # Lower alpha for other points
  transect_colors["gray"] <- "gray"
  xlims <- range(data$MDS1, na.rm = TRUE) + c(-0.45, 0.3)  # Adds buffer to avoid cutoff
  ylims <- range(data$MDS2, na.rm = TRUE) + c(-0.3, 0.3)  # Adds buffer to avoid cutoff
  xbreaks <- seq(floor(xlims[1]), ceiling(xlims[2]), by = 0.5)
  ybreaks <- seq(floor(ylims[1]), ceiling(ylims[2]), by = 0.5)
  p1 <- ggplot(data=data, aes(x=MDS1, y=MDS2)) +
    geom_point(size=2, aes(color=point_color, alpha=point_alpha)) + 
    scale_color_manual(values = transect_colors, name = "Transects") +  # Use custom transect colors
    scale_alpha_identity() +  # Use alpha directly from data
    stat_ellipse(data = data %>% filter(site == highlight_site_name), 
                 aes(group = transect, color = as.factor(transect)), linewidth = 1.2, level = 0.95) +  # Ellipses per transect
    coord_fixed(ratio = 1, xlim = xlims, ylim = ylims, expand = FALSE) +  # Maintain fixed aspect ratio & size
    scale_x_continuous(breaks = xbreaks, name = "Axis 1") +  # Fix x-axis labels to 0.5 increments
    scale_y_continuous(breaks = ybreaks, name = "Axis 2") +  # Fix y-axis labels to 0.5 increments
    ggtitle(paste0("Community homogeneity at Magnolia", highlight_site_name)) +
    my.theme +
    theme(
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 18),
      legend.key.size = unit(1, "cm"),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 18),
      plot.title = element_text(size = 20),
      legend.position = "right",  # Fix legend position
      legend.box = "vertical",  # Keep a consistent layout
      legend.spacing.y = unit(0.5, "cm")  # Standardize legend spacing
    ) +
    guides(color = guide_legend(ncol = 1))  # Keep legend in a single-column format
  
  return(p1)
}


## Function to generate and save NMDS plots for all 8 sites
plot.NMDS.transects.all <- function(df, transects, width = 7, height = 7) {
  # Ensure input is a data frame
  if (!inherits(df, "data.frame")) {
    stop("Error: 'df' must be a data frame.")
  }
  
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
  
  # Loop through each site and generate plots
  for (site_num in names(site.labs)) {
    # Generate plot
    p <- plot.NMDS.transects(df, highlight_site = site_num)
    
    # Define file paths for saving
    site_name <- gsub(" ", "_", site.labs[site_num])  # Replace spaces with underscores
    pdf_file <- file.path(transects, paste0("NMDS_transects_", site_name, ".pdf"))
    png_file <- file.path(transects, paste0("NMDS_transects_", site_name, ".png"))
    
    # Save as PDF
    ggsave(pdf_file, plot = p, device = "pdf", width = width, height = height, units = "in", dpi = 300)
    
    # Save as PNG
    ggsave(png_file, plot = p, device = "png", width = width, height = height, units = "in", dpi = 300)
    
    # Print confirmation message
    message("Saved: ", pdf_file, " and ", png_file)
  }
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


## plot NMDS ordination with species correlation coefficients and outlined labels (legend removed)
plot.NMDS.spp.scores.noLegend <- function(data){
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
  
  # Generate the NMDS plot (without legend)
  p1 <- ggplot(data=data, aes(x=MDS1, y=MDS2)) +
    geom_point(size=1, alpha=0.2, aes(color=site)) + my.theme + 
    scale_color_manual(values=site.cols) +  # Removed legend title
    geom_segment(data=spp_scores, linewidth=0.75,
                 aes(x=0, y=0, xend=NMDS1, yend=NMDS2), 
                 arrow=arrow(length=unit(0.2, "cm")), color="black") +
    
    # Outlined species labels
    geom_label(data=spp_scores, 
               aes(x=NMDS1, y=NMDS2, label=species, 
                   vjust=ifelse(NMDS2 >= 0, -0.5, 1.5)),  
               hjust=0.5, 
               size=2.5, color="black", fontface="bold", 
               fill="white", label.size=0.5, label.r=unit(0.1, "lines")) + 
    
    xlab("Axis 1") + ylab("Axis 2") + 
    ggtitle("Percent-cover correlation coefficients") +
    coord_fixed() +  # Fixes aspect ratio
    theme(
      plot.title = element_text(size = 16),  # Title size defined in function
      legend.position = "none",  # Removes the legend
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 15)
    )
  
  return(p1)
}


## Updated NMDS plot function for transects, ensuring consistency in formatting and removing the legend
plot.NMDS.transects.noLegend <- function(data, highlight_site) {
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
  
  # Convert site column to factor using site labels
  data <- data %>%
    mutate(site = factor(site, levels = names(site.labs), labels = site.labs))
  
  # Extract site name for title
  highlight_site_name <- site.labs[highlight_site]
  
  # Define unique transects and assign colors
  unique_transects <- unique(data$transect[data$site == highlight_site_name])
  transect_colors <- if (length(unique_transects) == 2) {
    c("#7c4e88", "#64b2af")  # Purple & Blue
  } else {
    c("#7c4e88", "#64b2af", "#feee67")  # Purple, Blue, Yellow
  }
  transect_colors <- setNames(transect_colors[1:length(unique_transects)], unique_transects)
  
  # Adjust data for visualization
  data <- data %>%
    mutate(point_color = ifelse(site == highlight_site_name, as.character(transect), "gray"),
           point_alpha = ifelse(site == highlight_site_name, 1, 0.15))  # Lower alpha for other points
  
  transect_colors["gray"] <- "gray"  # Ensure gray is in the color mapping
  
  # Generate NMDS plot with consistent formatting
  p1 <- ggplot(data=data, aes(x=MDS1, y=MDS2)) +
    geom_point(size=1, aes(color=point_color, alpha=point_alpha)) + 
    scale_color_manual(values = transect_colors, guide = "none") +  # Remove legend
    scale_alpha_identity() +  # Use alpha directly from data
    stat_ellipse(data = data %>% filter(site == highlight_site_name), 
                 aes(group = transect, color = as.factor(transect)), linewidth = 0.75, level = 0.95, show.legend = FALSE) +  # Remove ellipses from legend
    coord_fixed() +  # Fixed aspect ratio
    xlab("Axis 1") + ylab("Axis 2") +  # Axis labels
    ggtitle(paste(highlight_site_name, "transects")) +  # Updated title format
    my.theme +
    theme(
      plot.title = element_text(size = 16),  # Match previous function
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 15),
      legend.position = "none"  # Completely remove the legend
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





## diversity plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## plot one label for all 8 sites 
diversity.8.sites <- function(data, column) {
  
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
  
  # Ensure correct site mapping
  data <- data %>%
    mutate(site = factor(site, levels = names(site.labs), labels = site.labs))
  
  # Calculate median x-values for each transect within each site
  median_values <- data %>%
    group_by(site, transect) %>%
    summarize(median_x = median({{column}}, na.rm = TRUE), .groups = "drop")
  
  # Plot
  p <- ggplot(data, aes(x={{column}}, fill=transect, group=transect)) +  
    geom_density(alpha=0.7, position="stack") +  
    geom_vline(data=median_values, aes(xintercept=median_x, color=transect), linetype="solid", size=1.1) +  # Add vertical lines
    scale_fill_viridis_d() +  
    scale_color_viridis_d(guide="none") +  # Ensure vertical lines match transect colors
    scale_x_continuous(name=paste(as_label(enquo(column)), "seafloor percent coverage"), breaks=seq(0, 2, by=1)) +  
    labs(y="Kernel Density", fill="Transect") +  
    my.theme +  
    facet_wrap(~ site, nrow=2, ncol=4, scales="free_y") +  
    theme(
      strip.text = element_text(size = 16),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 16)
    )
  
  return(p)
}
## END diversity plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END of script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
