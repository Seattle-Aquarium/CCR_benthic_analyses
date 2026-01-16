# CCR benthic taxa simulation
## Introduction
This repo provides code to simulate data derived from the Seattle Aquarium's remotely operated vehicle (ROV) kelp forest survey program (see [here](https://github.com/zhrandell/Seattle_Aquarium_ROV_development)). Specifically, we simulate data derived from the downward- and forward-facing cameras mounted on the ROV. Our purpose is to provide simulated data with varying underlying patterns to a bull kelp habitat suitability model (linked [here](https://experience.arcgis.com/experience/b11daaa83ff045f1a9d88b2b926e1f75/page/About/)), enabling model evaluation and expansion. Once both simulated and real ROV data have been incorporated into this model, our intent is to generate finer-spatial scale predictors of habitat suitability to further guide bull kelp conservation and restoration.  

The downward-facing camera is used to characterize percent-coverage using CoralNet, an open-source AI annotation program, of aggregated taxa such as fleshy red algae, brown algae, and substrate type (see here for the complete list of categories we record). Imagery are analyzed in CoralNet to generate metrics of percent-coverage. This repo contains code to simulate data derived from CoralNet, where our rows sum to `row_sum = 50`, mirroring the 50 percent-coverage data points classified in every image. With this in mind, every row in our simulated data represent a simulated downward-facing image.  

Imagery from the forward- and downward-facing cameras are used to classify discrete and individually specific "objects" (individuals) encountered by the ROV such as bull kelp stipes, kelp crabs, and fishes. Here, our code provides the means to simulate and append these data to the simulated percent-coverage data. Unlike percent-coverage requiring discretion over `row_sum = 50`, abundance counts may take upon any integer equal to or greater than 0.  

Finally, we simulate these data in a manner that approximates our real-world ROV surveys from the Urban Kelp Research Project with the Port of Seattle. That is to say, our simulated benthic community data have the exact structure (in terms of the number of rows representing sites and transects) as our summer 2023 Urban Kelp ROV surveys. Specifically, in summer of 2023 we surveyed eight sites throughout Elliott Bay, Seattle, each with four transects, spanning 19,552 rows -- 19,552 seconds of ROV telemetry surveys, or 325 minutes of ROV transect survey time. As we extract a still image every 8 seconds as to obtain non-overlapping imagery from the downward-facing camera, our telemetry file is reduced to `total_n <- 2,441` across eight separate ROV sites throughout Elliott Bay. Each row is a simulated image that corresponds with a real extracted image. 

Below we provide the framework to simulate the percent-coverages and abundances that pair with our real Urban Kelp metadata, providing complete control over the probability distribution parameters (mean/variance, and/or shape/scale) of as many percent-coverage and abundance categories and species as desired. Our simulation functions are written to enable mean and variance parameters to vary by site, enabling a wide range of benthic configurations to be simulated. Our purpose is to control the underlying signal within the simulated data to evaluate and better understand the behavior of our habitat suitability model, as well as to better evaluate our own ROV and analytical methodological development.  

In terms of content, we include the following: 
* [code](https://github.com/zhrandell/Seattle_Aquarium_benthic_taxa_simulation/tree/main/code) contains _R_ scripts to combine and clean our real-world ROV telemetry files in preparation to append it to our simulated data ([wrangle_metadata.R](https://github.com/zhrandell/Seattle_Aquarium_benthic_taxa_simulation/blob/main/code/wrangle_metadata.R)), code necessary to simulate percent-coverage and abundance data ([simulate_taxa.R](https://github.com/zhrandell/Seattle_Aquarium_benthic_taxa_simulation/blob/main/code/CoralNet_simulation.R)), and code to visualize these simulations ([visualizations.R](https://github.com/zhrandell/Seattle_Aquarium_benthic_taxa_simulation/blob/main/code/graphing_functions.R)). The prior x3 _R_ scrips contain functions which we `source()` and invoke from [run_me.R](https://github.com/zhrandell/Seattle_Aquarium_benthic_taxa_simulation/blob/main/code/run_me.R).
* [data_input](https://github.com/zhrandell/Seattle_Aquarium_benthic_taxa_simulation/tree/main/data_input) contains our Urban Kelp ROV telemetry files from summer 2023 that we modify slight, clean, and append together. 
* [data_output](https://github.com/zhrandell/Seattle_Aquarium_benthic_taxa_simulation/tree/main/data_output) contains all derived files from our analyses, including the combined [metadata.csv](https://github.com/zhrandell/Seattle_Aquarium_benthic_taxa_simulation/tree/main/data_output/metadata.csv) file we append to our simulated data.
* [figs](https://github.com/zhrandell/Seattle_Aquarium_benthic_taxa_simulation/tree/main/figs) contains figures generated in the course of our simulations. 

Below we lay out a brief overview of our code and how to implement it. 

## CoralNet percent-coverage simulation

Simulating data derived from CoralNet requires diligence to avoid simulated values that exceed `row_sum = 50`, the number of data points processed per image. We achieve this in a two-step process. First, we simulate any given percent-cover category, e.g., red algae: 

```
i1 <- round(rgamma(total_n, shape = shape_S1, scale = 1), 0)
```
Here, `rgamma` randomly draws from a gamma distribution with `shape = shape_S1`, e.g., 5, and `scale = 1`. The `round()` function ensures we have whole numbers (as our real-world CoralNet annotations are integers). We expand this code slight to enable different `shape` parameters for different sites, e.g.,  

```
i1 <- round(rgamma(total_n, shape = c(rep(shape_S1, rows[1,2]),
                                      rep(shape_S2, rows[2,2]), scale = 1)), 0)
```
Here, `shape_S1` and `shape_S2` provide two different parameter values, and the `rgamma()` random draws are repeated `rep()` down rows with lengths defined by `rows[1,2]`, and `rows[2,2]`, with `rows` dataframe of site-specific row lengths that was generated by `wrangle_metadata.R` and can be found [here](https://github.com/zhrandell/Seattle_Aquarium_benthic_taxa_simulation/blob/main/data_output/nrows_site.csv). 

Next, because subsequent CoralNet categories simulated cannot exceed the values within `i1`, we run the following, with `row_sum <- 50`, the number of points we annotate in CoralNet: 

```
b2 <- (row_sum - i1) 
```
creating a new vector that designates the upper bounds that subsequent values can take, i.e., subsequent simulated categories cannot exceed `b2`. 

Now, for the second percent-cover category simulated (e.g., sugar kelp), we switch to a truncated normal distribution (see `rtruncnorm()`), as the upper bound places a hard limit on the distribution, thereby ensuring we do not simulate values greater than `b2` (and `a` below is a minimum bound, in our case `a <- 0`. Specifically, 

```
i2 <- round(rtruncnorm(total_n, a, b2, 
                       mean = c(rep(mean_S1, rows[1,2]), 
                                rep(mean_S2, rows[2,2]),
                                rep(mean_S3, rows[3,2]),
                                rep(mean_S4, rows[4,2]),
                                rep(mean_S5, rows[5,2]), 
                                rep(mean_S6, rows[6,2]),
                                rep(mean_S7, rows[7,2]),
                                rep(mean_S8, rows[8,2])),
                       sd = i2_sd), 0)

b3 <- (col_sum - i1 - i2) 
```

provides separate parameter values for each of the eight Urban Kelp sites, allowing us to control the underlying patterns of data. Now, just as before, we calculate our new upper bound, `b3 <- (col_sum - i1 - i2)`, which sets the upper limit with our next (third) simulated category. 

We proceed in this two-step process: simulate a variable, then calculate the new upper bound. As shown above, the eight Urban Kelp ROV sites can each have a distinct mean for any given category sampled. For example, below we visualize all eight sites, i.e., all `total_n <- 2441` rows of simulated CoralNet images with mean values of `mean = c(25, 5, 10, 15, 1, 5, 10, 15)`, and `sd = 2`.

![sugarkelp_allsites](https://github.com/zhrandell/Seattle_Aquarium_benthic_taxa_simulation/assets/49246458/5df021bc-0466-4a04-81aa-ff3c6529c858)

This process can be repeated for as many percent-coverage categories as desired, and the code below generates six simulated percent-cover categories. 

https://github.com/zhrandell/Seattle_Aquarium_benthic_taxa_simulation/blob/35b7841ca994acee7cc95f534712f76c481084ac/code/simulate_taxa.R#L28-L119

Finally, we need to ensure that each row sums to 50, so we run the following to calculate a 7th CoralNet variable called `remainder`, which rounds the row sum up to 50. In the following we only caluclate a single additional column, though we can feed `num.vars` a vector and randomly allocate values across new columns until the row sum = 50. 

```
simulate.remainder <- function(col_sum, num.vars){
  
  remainder <- (col_sum - df.1[,1] - df.1[,2] - df.1[,3] - df.1[,4] - df.1[,5] - df.1[,6])
  categories <- as.factor(num.vars)
  rows <- map(remainder, ~ sample(categories, ., replace = TRUE))
  df.2 <- lapply(rows, summary)
  df.2 <- as.data.frame(do.call(rbind, df.2))
  
  dat <- cbind(df.1, df.2)
  return(dat)
}

```

To provide an example of the data, we can examine a single transect -- `S1_T1` -- Site 1, Transect 1, from our Urban Kelp surveys. As we extract a still image from every 8 seconds of video, our telemetry `S1_T1` contains 112 rows, akin to 112 simulated images destined for CoralNet analysis. 

![simulated_magnolia](https://github.com/zhrandell/Seattle_Aquarium_benthic_taxa_simulation/assets/49246458/dfaf2ace-762f-43ec-8e26-51ec209e7181)

Here, every simulated image (row) are algined on the x-axis, with the values of the simulated categories on the y-axis. All rows sum to 50. Here we see lots of `sugar_kelp` and `soft_sediment`, akin to our ROV surveys at Magnolia. Likewise, we see very little `boulder_bedrock` or `red_algae`, again reflective of the soft-sediment site. 

## Abundance simulations

Compared to simulating percent-coverages, simulating abundances are relativley straightforward. Simulated values can take upon any integer of 0 or greater. We use a `rpois()` to randomly draw from a Poisson distribution, as it is commonly used to simulate integers equal to or greater than 0. `rpois()` only has a single parameter, lambda, or `λ`, with `λ` representing the mean number of events, or observations, in our case. 

In our case, simulating abundances across the eight sites is easily achieved with the following function: 

```
simulate.abundances <- function(n){

spp <- rpois(n, lambda = c(rep(lam_1, rows[1,2]),
                           rep(lam_2, rows[2,2]), 
                           rep(lam_3, rows[3,2]), 
                           rep(lam_4, rows[4,2]),
                           rep(lam_5, rows[5,2]),
                           rep(lam_6, rows[6,2]),
                           rep(lam_7, rows[7,2]),
                           rep(lam_8, rows[8,2])))

as.data.frame(spp)
return(spp)
}
```

Finally, we use the following function to bind our CoralNet simulations to our abundances as well as our metadata, generating a final dataframe: 

```
bind.data <- function(df, input.file){
  
  setwd(data_output)
  metadata <- read.csv(input.file)
  
  df$SU <- seq(1:nrow(df))
  df <- cbind(df, 
              bull_kelp, kelp_crabs, 
              metadata)

 return(df)
}
```









