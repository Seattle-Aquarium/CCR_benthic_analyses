## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Data visualizations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())


## add libraries
library(tidyverse)
library(vegan)


## set working directory to home folder
setwd("../")
getwd()


## relative file paths
code <- "code"
figs <- "figs"
label_19 <- "data_output/19_labels"


## graphing functions 
source(file.path(code, "NMDS_functions.R"))


## invoke relative file path 
dat <- read.csv(file.path(label_19, "diversity_T3-2_19_labels.csv"))
## END startup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## prep and run NMDS analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## partition metadata and the community matrix
metadata <- dat[, c(1:14)]
community <- dat[, c(15:32)]


## return percentages to natural scale
natural_scale_comm <- community * 100


## perform transformation if desired/required
log_comm <- log.transform(natural_scale_comm) 


## select log transform on a range of columns
log <- log.transform(dat, 9, 27)


## exponentiate back to the natural scale following log transform
dat <- inverse.log.transform(dat, 9, 27)
## END NMDS prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## perform NMDS and save output ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ord <- metaMDS(comm = natural_scale_comm, 
               distance="bray", 
               k=2, 
               min = 1000, 
               trymax=2500, 
               autotransform = F, 
               wascores = TRUE)


## save the ordination 
setwd(label_19)
save(ord, file="ord_T3-2_19_natural_scale.rda")
load(file.path(label_19, "ord_T3-2_19_natural_scale.rda"))


## visualize stress, check ordination, xy coordinates 
## open graphics window
graphics.off()
windows(6,6,record=T)


## plot
plot(ord)
stressplot(ord)
## END ordination ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## NMDS ordination coordinates and spp scores saved as data frame ~~~~~~~~~~~~~~
## save ord point into data frame for plotting, additional analyses
dat <- save.points(metadata, ord$points, natural_scale_comm)


## save species correlation coefficients as separate data.frame and csv 
spp_scores <- save.spp(ord)


## save csv files
write.csv(dat, file.path(label_19, "ord_pts_T3-2_19_natural_scale.csv"), row.names=FALSE)
write.csv(spp_scores, file.path(label_19, "spp_scores_T3-2_19_natural_scale.csv"), row.names=FALSE)
## END extract and save ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





## PERMANOVA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## classify as factor for color plotting
dat$transect <- as.factor(dat$transect)
dat$site <- as.factor(dat$site)
dat$key <- as.factor(dat$key)


## partition metadata and the community matrix
metadata <- dat[, c(1:10)]
community <- dat[, c(11:29)]


## compute a distance matrix
dist_matrix <- vegdist(community, method = "bray")


## run PERMANOVA
permanova <- adonis2(dist_matrix ~ site, data = metadata, permutations = 999)


# Print results
print(permanova)


# Check for homogeneity of dispersion
dispersion_test <- betadisper(dist_matrix, metadata$site)
anova(dispersion_test)  # If p < 0.05, dispersion is significantly different

# Visualize dispersion
plot(dispersion_test)


## pairwise site-site tests
pairwise_permanova_manual <- function(dist_matrix, grouping_factor, data, permutations = 999) {
  # Get unique levels of the factor (e.g., sites)
  levels_list <- unique(data[[grouping_factor]])
  
  # Create empty dataframe to store results
  results <- data.frame(Comparison = character(), R2 = numeric(), F.Model = numeric(), p.value = numeric(), stringsAsFactors = FALSE)
  
  # Loop through all pairs of levels
  for (i in 1:(length(levels_list) - 1)) {
    for (j in (i + 1):length(levels_list)) {
      group1 <- levels_list[i]
      group2 <- levels_list[j]
      
      # Subset metadata and distance matrix for just the two groups
      subset_data <- data[data[[grouping_factor]] %in% c(group1, group2), ]
      subset_dist <- as.matrix(dist_matrix)[rownames(subset_data), rownames(subset_data)]
      
      # Run PERMANOVA with reformulated formula
      test_result <- adonis2(as.dist(subset_dist) ~ reformulate(grouping_factor), data = subset_data, permutations = permutations)
      
      # Store results
      results <- rbind(results, data.frame(
        Comparison = paste(group1, "vs", group2),
        R2 = test_result$R2[1],
        F.Model = test_result$F[1],
        p.value = test_result$`Pr(>F)`[1]
      ))
    }
  }
  
  # Adjust p-values for multiple comparisons (Bonferroni correction)
  results$p.adj <- p.adjust(results$p.value, method = "bonferroni")
  
  return(results)
}

# Run pairwise PERMANOVA by site
pairwise_results <- pairwise_permanova_manual(dist_matrix, "site", metadata)

# View results
print(pairwise_results)



















## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
