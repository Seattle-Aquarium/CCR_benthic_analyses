## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NMDS functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## log base10 transformation, IF required 
log.transform.df <- function(data){
  num.col <- ncol(data)
  out <- log10(data[,1:num.col]+1)
  return(out)
}


log.transform <- function(data, start_col, end_col) {
  data[, start_col:end_col] <- log10(data[, start_col:end_col] + 1)
  return(data)  
}


# Function to reverse the log10 transformation
inverse.log.transform.all <- function(data){
  num.col <- ncol(data)
  out <- (10^data[,1:num.col]) - 1
  return(out)
}


## inverse log transformation across specific cols 
inverse.log.transform <- function(data, start_col, end_col){
  data[, start_col:end_col] <- (10^data[, start_col:end_col]) - 1  # Apply transformation only to specified columns
  return(data)
}



## function to extract NMDS coordinates, bind, and save w/ metadata and comm ~~~
save.points <- function(metadata, points, comm){
  t1 <- as.data.frame(ord$points)
  t2 <- cbind(metadata, t1, comm)
  return(t2)
}


## function to extract spp scores 
save.spp <- function(ord) {
  species_scores <- scores(ord, display = "species") %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "species")
  
  return(species_scores)
}


## function to open and record plots 
my.window <- function(x, y){
  windows(x, y, record = TRUE)
}
## end star-up functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





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
  "#6497b1",  #gray; site 1
  "#008080",  #teal; site 2
  "#03396c",  #dark blue; site 5
  "#7BBF6A",  #light green; site 3
  "#3D8B37",  #dark green; site 4
  "#CC6677",  #light maroon; site 6 
  "#882255",  #maroon; site 7
  "#000000"   #black 
)


## site labels
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


## category labels 
labels <- c(
  "sugar_kelp",
  "textured_kelp",
  "unknown",
  "filamentous_brown",
  "sargassum",
  "other_brown_algae",
  
  "red_algae",
  "green_algae",
  "coralline_algae",
  "kelp_bryozoan",
  "mobile_invert",
  "sessile_invert",
  
  "shell_debris",
  "pebble",
  "soft_sediment",
  "hard_substrate",
  "cobble",
  "anthro_substrate"
)


## plot NMDS 
plot.NMDS <- function(data){
  p1 <- ggplot(data=dat, aes(x=MDS1, y=MDS2)) +
    geom_point(size=2, aes(x=MDS1, y=MDS2, color=site)) + my.theme + 
    scale_color_manual(values=site.cols)
  return(p1)
}


## plot NMDS ellipses
plot.NMDS.ellipses <- function(data){
  p1 <- ggplot(data=dat, aes(x=MDS1, y=MDS2)) +
    geom_point(size=2, alpha=0.35, aes(x=MDS1, y=MDS2, color=site)) + my.theme + 
    stat_ellipse(linewidth=1, aes(group=site, color=site), level=0.95) +
    scale_color_manual(values=site.cols)
  return(p1)
}


## plot NMDS with species correlation coefficients 
plot.NMDS.spp_scores <- function(data){
  
  p1 <- ggplot(data=dat, aes(x=MDS1, y=MDS2)) +
    geom_point(size=2, alpha=0.35, aes(x=MDS1, y=MDS2, color=site)) + my.theme + 
    scale_color_manual(values=site.cols) + 
    
    geom_segment(data=spp_scores, linewidth=1.25,
                 aes(x=0, y=0, xend=NMDS1, yend=NMDS2), 
                 arrow=arrow(length=unit(0.2, "cm")), color="black") +
    
    geom_text(data=spp_scores, 
              aes(x=NMDS1, y=NMDS2, label=species, 
                  vjust=ifelse(NMDS2 >= 0, -0.5, 1.5)),  
              hjust=0.5, 
              size=4, color="black", fontface="bold")
  
  return(p1)
}


## Function to plot density for a single site, dynamically choosing the column to visualize
plot_site_density <- function(data, column, site_number) {
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
plot_all_sites_density <- function(data, column) {
  # Define site labels inside the function
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
    scale_x_continuous(name=paste(as_label(enquo(column)), "seafloor percent coverage"), 
                       breaks=seq(0, 10, by=1)) +  
    labs(y="Kernel Density", fill="Transect") +  
    my.theme +  
    facet_wrap(~ site, nrow=2, ncol=4, scales="free_y") +  # Ensure correct facet labeling
    theme(
      strip.text = element_text(size = 16),#, face = "bold"),  # Facet label text size
      axis.text.y = element_blank(),
      #axis.text.y = element_text(size=16),
      axis.ticks.y = element_blank(), # Remove y-axis ticks
      legend.text = element_text(size = 16),  # Legend text size
      legend.title = element_text(size = 16)  # Legend title size
    )
  
  return(p)
}


## plot all categories for a single site
plot_site_density <- function(data, site_number) {
  # Manually reorder categories to fill column-wise
  reordered_labels <- c(
    # Column 1
    "sugar_kelp", "red_algae", "shell_debris",
    "textured_kelp", "green_algae", "pebble",
    "unknown", "coralline_algae", "soft_sediment",
    "filamentous_brown", "kelp_bryozoan", "hard_substrate",
    "sargassum", "mobile_invert", "cobble",
    "other_brown_kelp", "sessile_invert", "anthro_substrate"
  )
  
  # Create explicit ordering to enforce column-wise filling
  category_order <- tibble(
    category = reordered_labels,
    order = 1:length(reordered_labels)  # Assign numeric order for verification
  )
  
  # Filter data for the selected site
  filtered_data <- data %>% filter(site == site_number)
  
  # Convert data to long format for ggplot
  long_data <- filtered_data %>%
    select(site, transect, all_of(reordered_labels)) %>%
    pivot_longer(cols = all_of(reordered_labels), names_to = "category", values_to = "value") %>%
    left_join(category_order, by = "category")  # Merge explicit ordering
  
  # Convert category to a factor with correct order
  long_data$category <- factor(long_data$category, levels = reordered_labels)
  
  # Plot using facet_wrap(), ensuring order fills by column
  p <- ggplot(long_data, aes(x=value, fill=transect, group=transect)) +  
    geom_density(alpha=0.7, position="stack") +  
    scale_fill_viridis_d() +  
    scale_x_continuous(name="Seafloor Percent Coverage", breaks=seq(0, 100, by=25)) +  
    labs(y="Kernel Density", fill="Transect") +  
    my.theme +  
    facet_wrap(~ category, nrow=6, ncol=3, scales="free_y") +  # Uses correct category order
    theme(
      strip.text = element_text(size = 16),  # Facet label text size
      axis.text.y = element_blank(),  # Remove y-axis numbers
      axis.ticks.y = element_blank(), # Remove y-axis ticks
      legend.text = element_text(size = 16),  # Legend text size
      legend.title = element_text(size = 16)  # Legend title size
    )
  
  return(p)
}


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
