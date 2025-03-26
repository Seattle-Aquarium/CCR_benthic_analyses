# CCR benthic analyses

### Overview 
This repository contains code and files necessary to work with our ROV survey imagery and the derived percent-cover and abundance data. Specifically, this repository includes code to complete a variety of tasks, ROV survey image patches used to train computer vision models, and percent-cover and abundance data extracted from the manual annotation of those models. To work with those data we include code for wrangling data, multivariate analyses of community structure, visualization of kernel densities, calculation of species diversity and species evenness metrics, and statistical analyses. 

Most R scripts that involve longer workflows include a separate script that we `source()`. For example, the main `NMDS.R` script we use to perform multivariate analyses invokes `source(NMDS_functions.R)` to run. These `..._functions.R` scripts are not noted separately below, but can be found within `..\code` alongside all other scripts. 

<p float="center">
 <img src="data_output/patches/figs/KE_holdfas.png" width="250" height="270" />&nbsp;&nbsp;&nbsp;
 <img src="data_output/patches/figs/GR_ulva.png" width="250" height="270" />&nbsp;&nbsp;&nbsp;
 <img src="data_output/patches/figs/RE_branch.png" width="250" height="270" />
</p>

### Machine learning (ML) processing of survey imagery  
We are training ML models to generate data from our ROV survey imagery. Specifically, we are generating metrics of percent-cover classification (for aggregate taxa and substrate categories) and object detection (for individually conspicuous species). Both classification and object detection models are being trained using Ultralytics via [CoralNet-Toolbox](https://github.com/Jordan-Pierce/CoralNet-Toolbox). The use of Ultralytics by Toolbox allows numerous augmentations and transformations of the patches and objects (depicted via the figure below), enabling a more robust model due to the variation introduced.

<div align="center">
  <img src="figs/readme_images/schematic.PNG" width="800", height="455">
</div> 



#### Percent-cover classification
* We have categorized **31 percent-cover classes**, including red, green, and brown algae, substrate types, sessile and mobile species. View our label set [here](https://www.dropbox.com/scl/fi/o2oxc0fen94m5o8x5a5el/percent_cover_labelset.xlsx?rlkey=kh8dlx9fpo9pz5wxnn8eaq5e4&dl=0).
* You can find examples of our percent-cover image patches [here](https://github.com/Seattle-Aquarium/CCR_benthic_analyses/tree/main/data_output/patches/labels) in `data_output`.
* See the confusion matrix linked [here](https://github.com/Seattle-Aquarium/CCR_benthic_analyses/blob/main/figs/readme_images/matrix.png) for the current predicted vs real percent-accuracy of our model (with an overall model accuracy = 91.5%).
* You can find the model weights from a trained classifier [here](https://www.dropbox.com/scl/fo/ro11h5n7aaydzvij028y9/AKqxPHgltMklumPAGXxzV24?rlkey=iiuv3rcrt2uh1osuxbdy0n4xz&dl=0) on the Seattle Aquarium's DropBox.
  
<div align="center">
  <img src="figs/readme_images/Toolbox_percentcover.gif" alt="Description of GIF", width="650", height="360">
</div>

#### Object detection
* We are training an object detection model to automate identification of animals in survey imagery. Annotations to create the training the dataset are being created in VIAME thanks to our hardworking "AI Teacher" volunteers. View the species list [here](https://www.dropbox.com/scl/fi/v8k7ndggqiwn6cdxrfnyj/objects_labelset.xlsx?rlkey=p26n0qj0jekl5j0s0cbt2g1bj&dl=0).
<div align="center">
  <img src="figs/readme_images/VIAME_detection.gif" alt="Description of GIF", width="650", height="360">
</div> 

### Other CCR GitHub repositories

```mermaid
graph TD

A["<a href='https://github.com/Seattle-Aquarium/Coastal_Climate_Resilience' target='_blank' style='font-size: 16px; font-weight: bold;'>Coastal_Climate_Resilience</a><br><font color='darkgray' style='text-decoration: none;'> the main landing pad for the CCR research program</font>"]

A --> B["<a href='https://github.com/Seattle-Aquarium/CCR_analytical_resources' target='_blank' style='font-size: 16px; font-weight: bold;'>CCR_ROV_telemetry_processing</a><br><font color='darkgray' style='text-decoration: none;'>contains code, analytical tools, and data</font>"]

A --> C["<a href='https://github.com/Seattle-Aquarium/CCR_benthic_analyses' target='_blank' style='font-size: 16px; font-weight: bold;'>CCR_benthic_analyses</a><br><font color='darkgray' style='text-decoration: none;'> (this page) code to analyze ROV survey data</font>"]

A --> D["<a href='https://github.com/Seattle-Aquarium/CCR_development' target='_blank' style='font-size: 16px; font-weight: bold;'>CCR_development</a><br><font color='darkgray' style='text-decoration: none;'>repo for active software projects and Issues</font>"]

A --> E["<a href='https://github.com/Seattle-Aquarium/CCR_benthic_taxa_simulation' target='_blank' style='font-size: 16px; font-weight: bold;'>CCR_benthic_taxa_simulation</a><br><font color='darkgray' style='text-decoration: none;'>code to simulate ROV survey data</font>"]

style C stroke:#00B2EE,stroke-width:4px
```



### code
- `wrangle_data.R`: modify the raw CoralNet annotation output, merge with ROV telemetry metadata, and save it prior to further processing. 
- `revise_categories.R`: modify the percent-cover categories within an exported CoralNet dataset.  
- `diversity_metrics.R`: calculate species richness, Shannon-Weiner diversity metric, Stimpson's diveristy metric, and Peilou's evenness for percent-cover data or the combination of percent-cover and abundance data.
- `species_density.R`: calculate species density metrics. 
- `NMDS.R`: perform non-metric multidimensional scaling (NMDS) analyses on a community matrix. 
- `visualization.R`: visualize NMDS ordinations, kernel densities, and species diversity metrics.
- `GLMM.R`: compile generalized linear mixed effects models with percent-cover as the response.
- `patch_aggregation.R`: extract image patches and aggregate them in custom specified grids to create figures for patch inspection, plotting, communication, etc.

### data_input
- `bull_kelp_stipes.csv` contains bull kelp stipe and bundle counts from the forward-facing video.
- `CoralNet_2022_annotations.csv` raw CoralNet annotation output. 
- `original_CoralNet_2022_dataset.csv` contains output from CoralNet annotations merged with our ROV telemetry file, containing 1479 images and 118,000 annotations in CoralNet.
- `HSIL_VIAME_2024.csv` contains ROV-diver overlap data from summer 2024 surveys.
- `Port_VIAME_2022.csv` contains summer 2022 percent-cover and abundance data.

### data_output
- `19_labels`: output folder with processed .csv files, ordinations, and other output files for our revised 19 CoralNet categories. 
- `69_labels`: output folder with processed .csv files, ordinations, and other output files for our original 69 CoralNet categories. 
- `patches`: folder containing example image patches extracted from our Toolbox datasets. 

### figs
- output folder for various figures 

<p float="center">
  <img src="figs/readme_images/NMDS_ellipses.PNG" width="400" height="300" />
  <img src="figs/readme_images/NMDS_spp_scores.png" width="400" height="300" />
 </p>

 <p float="center">
  <img src="figs/readme_images/sargassum.PNG" width="550" height="300" />
 </p>

 
