# CCR benthic taxa analyses
This repo contains code and data to perform analyses and visualizations of percent-cover data exported from CoralNet. 

### code
- `diversity_metrics.R`: calculate species richness and the Shannon-Weiner diversity metric, Stimpson's diveristy metric, and Peilou's evenness, used in conjunction with the Shannon-Weiner matric. 
- `diversity_metrics_functions.R`: functions necessary to calculate spp diveristy, collate the output, and save a .csv file.
- `NMDS.R`: perform non-metric multidimensional scaling (NMDS) analyses.
- `NMDS_functions.R`: functions necessary to format data, perform NMDS, extract the relevant data, and save the resulting ordination. 
- `revise_categories.R`: modify the percent-cover categories within an exported CoralNet dataset. 
- `revise_categories_functions.R`: functions to modify CoralNet categories
- `visualization.R`: visualize NMDS ordinations, kernel densities, and species diversity metrics.
- `visualization_functions.R`: functions to perform visualizations.
- `wrangle_data.R`: modify CoralNet data and save it prior to further analyses. 
- `wrangle_data_functions.R`: function to wrangle data. 

### data_input
- `2022_multiple_transects.csv` contains the raw output from CoralNet merged with our ROV telemetry file, containing 1479 images and 118,000 annotations in CoralNet.
- `bull_kelp_stipes.csv` contains bull kelp stipe and bundle counts from the forward-facing video.
 
### data_output
- ...
- ...


