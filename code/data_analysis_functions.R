## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Data visualizations functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## log base10 transformation, IF required 
log.transform <- function(data){
  num.col <- ncol(data)
  out <- log10(data[,1:num.col]+1)
  return(out)
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



# Function to plot density for a single site, dynamically choosing the column to visualize
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
                       breaks=seq(0, 100, by=20)) +  
    labs(y="Kernel Density", fill="Transect") +  
    my.theme +  
    facet_wrap(~ site, nrow=2, ncol=4, scales="free_y") +  # Ensure correct facet labeling
    theme(
      strip.text = element_text(size = 16),#, face = "bold"),  # Facet label text size
      axis.text.y = element_blank(),  # Remove numeric labels on y-axis
      axis.ticks.y = element_blank(), # Remove y-axis ticks
      legend.text = element_text(size = 16),  # Legend text size
      legend.title = element_text(size = 16)  # Legend title size
    )
  
  return(p)
}

# Example usage:
print(plot_all_sites_density(dat, sugar_kelp))












## REVISE or DELETE parallel processing code: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## run NMDS analyses with parallel cores 
nmds_parallel <- function(comm_matrix, 
                          distance = "bray", 
                          k = 2, 
                          min = 100, 
                          trymax = 500, 
                          autotransform = FALSE, 
                          wascores = TRUE, 
                          parallel_runs = num_cores, ...) {
  
  results <- foreach(i = 1:parallel_runs, .packages = "vegan", .combine = list) %dopar% {
    metaMDS(comm = comm_matrix, 
            distance = distance, 
            k = k, 
            min = min, 
            trymax = trymax, 
            autotransform = autotransform, 
            wascores = wascores, 
            ...)
  }
  
  return(results)
}


stopCluster(cl)





## set up parallel processing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)
## END parallel processing set up~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## perform NMDS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Example Invocation (Specify Only the Parameters You Want)
nmds_results <- nmds_parallel(log_comm, min = 1000, trymax = 5000)

# Stop parallel cluster
stopCluster(cl)

# Select the best NMDS solution (lowest stress)
best_nmds <- nmds_results[[which.min(sapply(nmds_results, function(x) x$stress))]]

# Plot the best NMDS result
plot(best_nmds, type = "t", main = "Optimized NMDS Ordination")
## END NMDS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


















## graphing params and functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## set up custom ggplot theme 
my.theme = theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"),
                 axis.title.x=element_text(size=18),
                 axis.title.y=element_text(size=18),
                 axis.text=element_text(size=14),
                 plot.title = element_text(size=13),
                 legend.text = element_text(size=15),
                 legend.title = element_text(size=15)
                 )


## create a custom theme for n pairwise comparisons
mult.theme = theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   axis.title.x=element_text(size=8),
                   axis.title.y=element_text(size=8),
                   axis.text=element_text(size=7),
                   plot.title = element_text(size=4),
                   legend.text = element_text(size=5),
                   legend.title = element_text(size=5)
                   )

## Define custom labels for the facets
site_labels <- c(
  "1" = "Site 1: Magnolia",
  "2" = "Site 2: EBM west",
  "3" = "Site 3: EBM center",
  "4" = "Site 4: EBM east",
  "5" = "Site 5: Grain Elevator",
  "6" = "Site 6: Sirens of Spring",
  "7" = "Site 7: Pocket Beach",
  "8" = "Site 8: Coast Guard Stn."
)


## transect colors
cols <- scale_color_manual(values=c("#B22222", "#009ACD", "#000000"))
fills <- scale_fill_manual(values=c("#B22222", "#009ACD", "#000000"))




## function to create custom graphing window
my.window <- function(width, height){
  graphics.off()
  windows(width, height, record=TRUE)
}


## function for pairwise visualization
pairwise.plot <- function(data, x, y, x.lab, y.lab){
  fig <- ggplot(data=data, aes(x, y)) + my.theme + coord_fixed() + 
    geom_point(size=1.0, aes(alpha=0.5)) +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    xlab(x.lab) + ylab(y.lab) + xlim(0,1) + ylim(0,1) +
    theme(legend.position = "none")
  return(fig)
}



facet.site <- function(data, x, y, x.lab, y.lab, fig.pos.x, fig.pos.y){
 
   fig <- ggplot(data=data, aes(x, y)) + my.theme +  #coord_fixed() +
    geom_point(size=1.75, alpha=0.65, aes(color=transect)) + cols +
    facet_wrap(~site, labeller = labeller(site = site_labels), ncol=4) +
    theme(strip.text = element_text(size = 14)) +
    xlab(x.lab) + ylab(y.lab) + #xlim(0,1) + ylim(0,1) +
    theme(legend.position = c(fig.pos.x, fig.pos.y), 
          legend.justification = c(1, 1)) + 
    theme(panel.spacing = unit(1, "lines")) +
     
     scale_x_continuous(limits = c(0, 1),  # Set limits here
                        breaks = c(0, 0.25, 0.5, 0.75, 1),  
                        labels = c(0, 0.25, 0.5, 0.75, 1),
                        expand = c(.015, .015)) +  # Remove padding to ensure equal aspect ratio
     scale_y_continuous(limits = c(0, 1),  # Set limits here
                        breaks = c(0, 0.25, 0.5, 0.75, 1),  
                        labels = c(0, 0.25, 0.5, 0.75, 1),
                        expand = c(.015, .015))   # Remove padding to ensure equal aspect ratio
    
  return(fig)
}





## function for modified pairwise plot for "n" pairwise comparisons
mult.pairwise.plot <- function(data, x, y, x.lab, y.lab) {
  ggplot(data, aes_string(x = x, y = y)) + 
    mult.theme + coord_fixed() + 
    geom_point(size = 1, alpha = 0.5)+#, aes(color=site)) +
    #geom_smooth(method = "loess", se = FALSE, color = "red") +
    xlab(x.lab) + ylab(y.lab) + xlim(0, 1) + ylim(0, 1) +
    theme(legend.position = "none")
}



## function to save both pdf and png files of each fig 
save.figs <- function(pdf, png, plot.lab, width.x, height.y){
  setwd(figs)
  ggsave(pdf, plot = plot.lab, width = width.x, height = height.y)
  ggsave(png, plot = plot.lab, width = width.x, height = height.y)
}


## sample down to 50 rows (or n rows)
sample.data <- function(data, nrows, replace.logic){
  new.dat <- data %>%
    group_by(key) %>%
    sample_n(size = nrows, replace = replace.logic) %>%
    ungroup()
  return(new.dat)
}


## function to save figs
save.figs <- function(pX, path, title.pdf, title.png, width.x, height.y, png.dpi){
  ggsave(file.path(path, title.pdf), plot = pX, device = "pdf", width = width.x, height = height.y)
  ggsave(file.path(path, title.png), plot = pX, device = "png", width = width.x, height = height.y, dpi = png.dpi)
}
## END graphing params & graphing functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
