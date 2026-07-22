# ROVs and scuba divers: a 1:1 methods comparison

## Background
For the past several decades, scientific scuba divers have been the primary avenue for collecting abundance and percent-cover data along the seafloor (with percent-cover being a method of quantifying coverage of aggregate taxa that cannot be individually counted, such as red and green algae). 
However, significant limitations to scuba diving exist: it is logistically very challenging, and divers can only cover relatively small areas. ROVs have traditionally been large, expensive, and used to explore deep areas of the ocean, but the latest generation of small ROVs are suited for nearshore, coastal operations. It remains unknown however how ROV-derived data compare to diver-data, thus we are uncertain how best to integrate ROV methods into the broader framework of coastal monitoring. This study was designed to gather both ROV and scuba diver data from the same survey locations on the same day, using a broadly accepted standardized diver methodology---Reef Check’s Kelp Forest Monitoring Program. 

## Sampling design
- We surveyed x2 “sites" in distinct locations of Elliott Bay: one along Centennial Park, another along the Elliott Bay Marina breakwater (see "Elliott Bay Marina" and "Sirens of Spring" below, with "Sirens of Spring" part of Centennial Park, and referred to henceforth as Centennial Park). 
- Each site has x6 “transects” (individual surveys).
- The transects are 30m long by 2m wide, and each transect is delineated by a “transect tape” (a surveyors tape laid along the seafloor, denoting the specific area to be surveyed). 
- All transects are parallel to shore, with x3 laid at a depth of 10m and x3 laid at a depth of 5m. 
- The transects within a depth are laid back-to-back with a 5m spacing between each one (with 5m being required for approximate statistical independence, per the benthic ecology literature). We consider the x3 transects within a depth to be replicates. 

<p align="center">
<img width="575" height="622" alt="updated_map" src="https://github.com/user-attachments/assets/8452c796-6ef0-4bde-94ba-6784af9c8e9b" />
</p>

There are two different benthic environments captured by our two sites: soft sediment (Centennial Park) and boulder structure (Elliott Bay Marina breakwater); we know benthic community structure will differ between the two sites, and within each site based on depth. 

## Analytical priorities
We are most interested in understanding: (**1**) how abundance and percent-cover data differ between ROVs and divers. We would secondarily be interested in seeing how the answer to the former question varies depending upon (**2**) site and (**3**) depth (if possible). (with site and depth as random effects?). 
In terms of inference, we are not trying in trying to comprehensively make statements about the sites themselves, or the broader location (Centennial Park vs the breakwater); rather, we want to prioritize how the ROV vs diver methodologies differ.  

We want to statistically compare the differences/similarities between (1) abundances (individuals: sea stars, crabs, etc.) captured by both platforms, and (2) percent-cover (proportion data). 
Comparing abundances should be reasonably straight forward. 
Comparing percent-cover, however, could be more challenging; the diver gathers several data points at each meter along the 30 meter transect; 
in contrast, the ROV gathers high-resolution photos, and we annotate x50 data points per photo. 
With 1 photo per meter, we have 1500 percent-cover data points. 
So, to facilitate a comparison, we have averaged the ROV percent-cover data (across photos) to a single proportion value per transect. In some cases, we have combined categories of the ROV data (e.g., as the Reef Check protocols only record a single category of "red algae," we combined our x4 distinct categories of red algae into a single, sum total "red algae" category, and averaged those values across the photos within a single transect.   

## Data
### Reef Check diver data
See [wrangle_diver_data.R](https://github.com/Seattle-Aquarium/CCR_benthic_analyses/blob/main/ROV_diver_comparison/code/wrangle_diver_data.R) and [wrangle_diver_functions.R](https://github.com/Seattle-Aquarium/CCR_benthic_analyses/blob/main/ROV_diver_comparison/code/wrangle_data_functions.R) for the code used to process the diver data, producing data with summary values per transect (per row). 

- [diver_algae_abundance.csv](https://github.com/Seattle-Aquarium/CCR_benthic_analyses/blob/main/ROV_diver_comparison/results/diver/diver_algae_abundance.csv)
- [diver_invert_abundance.csv](https://github.com/Seattle-Aquarium/CCR_benthic_analyses/blob/main/ROV_diver_comparison/results/diver/diver_invert_abundance.csv)
- [diver_UPC_percentage.csv](https://github.com/Seattle-Aquarium/CCR_benthic_analyses/blob/main/ROV_diver_comparison/results/diver/diver_UPC_percentage.csv)

### ROV abundance data
See [wrangle_ROV_abundance_data.R](https://github.com/Seattle-Aquarium/CCR_benthic_analyses/blob/main/ROV_diver_comparison/code/wrangle_ROV_abundance_data.R) and [wrangle_data_functions.R](https://github.com/Seattle-Aquarium/CCR_benthic_analyses/blob/main/ROV_diver_comparison/code/wrangle_data_functions.R) for the code used to process VIAME-derived ROV object-detection data, producing:

- [ROV_invert_abundance.csv](https://github.com/Seattle-Aquarium/CCR_benthic_analyses/blob/main/ROV_diver_comparison/results/ROV/ROV_invert_abundance.csv)

(New VIAME abundance data is forthcoming; this script needs to be re-run once it lands.)

### ROV percent-cover data (HSIL export)
See [wrangle_HSIL_percent-cover_data.R](https://github.com/Seattle-Aquarium/CCR_benthic_analyses/blob/main/ROV_diver_comparison/code/wrangle_HSIL_percent-cover_data.R) for the code used to process `HSIL_percent_cover.csv` -- the full photo-level percent-cover export (both sites, all 6 transects, both seasons), which supersedes the single-season `short_percent_t4.csv`/`short_percent_t6.csv` inputs used above. It produces:

- [HSIL_percent-cover_photo-level.csv](https://github.com/Seattle-Aquarium/CCR_benthic_analyses/blob/main/ROV_diver_comparison/results/ROV/HSIL_percent-cover_photo-level.csv) -- cleaned, one row per photo
- [HSIL_percent-cover_long.csv](https://github.com/Seattle-Aquarium/CCR_benthic_analyses/blob/main/ROV_diver_comparison/results/ROV/HSIL_percent-cover_long.csv) -- long format (photo x category)
- [HSIL_percent-cover_transect-averaged.csv](https://github.com/Seattle-Aquarium/CCR_benthic_analyses/blob/main/ROV_diver_comparison/results/ROV/HSIL_percent-cover_transect-averaged.csv) -- mean percent cover per site/transect/depth/season
- [HSIL_points_photo-level.csv](https://github.com/Seattle-Aquarium/CCR_benthic_analyses/blob/main/ROV_diver_comparison/results/ROV/HSIL_points_photo-level.csv), [HSIL_points_transect-sums.csv](https://github.com/Seattle-Aquarium/CCR_benthic_analyses/blob/main/ROV_diver_comparison/results/ROV/HSIL_points_transect-sums.csv), [HSIL_points_transect-average.csv](https://github.com/Seattle-Aquarium/CCR_benthic_analyses/blob/main/ROV_diver_comparison/results/ROV/HSIL_points_transect-average.csv) -- the same data expressed as point counts (each photo's cover is generated from 50 randomly distributed points), which are additive in a way percentages are not

We have a started some scripts to visualize and analyze both ROV and diver data, e.g.,

- [analyze.R](https://github.com/Seattle-Aquarium/CCR_benthic_analyses/blob/main/ROV_diver_comparison/code/analyze.R)
- [analyze_functions.R](https://github.com/Seattle-Aquarium/CCR_benthic_analyses/blob/main/ROV_diver_comparison/code/analyze_functions.R)
