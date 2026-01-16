## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## benthic taxa simulation functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
rm(list = ls())

library(truncnorm)
library(tidyverse)
library(reshape2)

## hardcode relative file paths
code <- "../code"
data_input <- "../data_input"
data_output <- "../data_output"
figs <- "../figs"

## pull in row length csv
setwd(data_output)
rows <- read.csv("nrows_site.csv")
## END start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## simulate ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## simulate gamma with shape and scale

simulate.CoralNet <- function(total_n, row_sum){

i1 <- round(rnorm(total_n, mean = c(rep(mean_1_S1, rows[1,2]), 
                                    rep(mean_1_S2, rows[2,2]),
                                    rep(mean_1_S3, rows[3,2]),
                                    rep(mean_1_S4, rows[4,2]),
                                    rep(mean_1_S5, rows[5,2]),
                                    rep(mean_1_S6, rows[6,2]),
                                    rep(mean_1_S7, rows[7,2]),
                                    rep(mean_1_S8, rows[8,2])), 
                     sd = i1_sd), 0)
  
b2 <- (row_sum - i1) 

i2 <- round(rtruncnorm(total_n, a, b2, 
                       mean = c(rep(mean_2_S1, rows[1,2]), 
                                rep(mean_2_S2, rows[2,2]),
                                rep(mean_2_S3, rows[3,2]),
                                rep(mean_2_S4, rows[4,2]),
                                rep(mean_2_S5, rows[5,2]), 
                                rep(mean_2_S6, rows[6,2]),
                                rep(mean_2_S7, rows[7,2]),
                                rep(mean_2_S8, rows[8,2])),
                       sd = i2_sd), 0)

b3 <- (row_sum - i1 - i2) 

i3 <- round(rtruncnorm(total_n, a, b3, 
                       mean = c(rep(mean_3_S1, rows[1,2]), 
                                rep(mean_3_S2, rows[2,2]),
                                rep(mean_3_S3, rows[3,2]),
                                rep(mean_3_S4, rows[4,2]),
                                rep(mean_3_S5, rows[5,2]), 
                                rep(mean_3_S6, rows[6,2]),
                                rep(mean_3_S7, rows[7,2]),
                                rep(mean_3_S8, rows[8,2])),
                       sd = i3_sd), 0)

b4 <- (row_sum - i1 - i2 - i3)

i4 <- round(rtruncnorm(total_n, a, b4, 
                       mean = c(rep(mean_4_S1, rows[1,2]), 
                                rep(mean_4_S2, rows[2,2]),
                                rep(mean_4_S3, rows[3,2]),
                                rep(mean_4_S4, rows[4,2]),
                                rep(mean_4_S5, rows[5,2]), 
                                rep(mean_4_S6, rows[6,2]),
                                rep(mean_4_S7, rows[7,2]),
                                rep(mean_4_S8, rows[8,2])),
                       sd = i4_sd), 0)

b5 <- (row_sum - i1 - i2 - i3 - i4)

i5 <- round(rtruncnorm(total_n, a, b5, 
                       mean = c(rep(mean_5_S1, rows[1,2]), 
                                rep(mean_5_S2, rows[2,2]),
                                rep(mean_5_S3, rows[3,2]),
                                rep(mean_5_S4, rows[4,2]),
                                rep(mean_5_S5, rows[5,2]), 
                                rep(mean_5_S6, rows[6,2]),
                                rep(mean_5_S7, rows[7,2]),
                                rep(mean_5_S8, rows[8,2])),
                       sd = i5_sd), 0)

b6 <- (row_sum - i1 - i2 - i3 - i4 - i5)

i6 <- round(rtruncnorm(total_n, a, b6, 
                       mean = c(rep(mean_6_S1, rows[1,2]), 
                                rep(mean_6_S2, rows[2,2]),
                                rep(mean_6_S3, rows[3,2]),
                                rep(mean_6_S4, rows[4,2]),
                                rep(mean_6_S5, rows[5,2]), 
                                rep(mean_6_S6, rows[6,2]),
                                rep(mean_6_S7, rows[7,2]),
                                rep(mean_6_S8, rows[8,2])),
                       sd = i6_sd), 0)

df.1 <- cbind(i1, i2, i3, i4, i5, i6)
df.1[is.na(df.1)] <- 0

colnames(df.1)[1] <- "red_algae"
colnames(df.1)[2] <- "sugar_kelp"
colnames(df.1)[3] <- "green_algae"
colnames(df.1)[4] <- "soft_sediment"
colnames(df.1)[5] <- "small_rocks"
colnames(df.1)[6] <- "boulder_bedrock"

return(df.1)

}


## function to simulate "unimportant" variables that round each row 
## (each image) up to num.vars i.e. the number of data points
simulate.remainder <- function(row_sum, num.vars){
  
  remainder <- (row_sum - df.1[,1] - df.1[,2] - df.1[,3] - df.1[,4] - df.1[,5] - df.1[,6])
  categories <- as.factor(num.vars)
  rows <- map(remainder, ~ sample(categories, ., replace = TRUE))
  df.2 <- lapply(rows, summary)
  df.2 <- as.data.frame(do.call(rbind, df.2))

  dat <- cbind(df.1, df.2)
  return(dat)
  
}


## function to simulate abundances across eight sites
simulate.abundances <- function(n){
  
  
bull_kelp <- round(rnorm(n, mean = c(rep(mean_1_S1, rows[1,2]), 
                                     rep(mean_1_S2, rows[2,2]),
                                     rep(mean_1_S3, rows[3,2]),
                                     rep(mean_1_S4, rows[4,2]),
                                     rep(mean_1_S5, rows[5,2]),
                                     rep(mean_1_S6, rows[6,2]),
                                     rep(mean_1_S7, rows[7,2]),
                                     rep(mean_1_S8, rows[8,2])), 
                    sd = i1_sd), 0)


kelp_crabs <- round(rnorm(n, mean = c(rep(mean_1_S1, rows[1,2]), 
                                      rep(mean_1_S2, rows[2,2]),
                                      rep(mean_1_S3, rows[3,2]),
                                      rep(mean_1_S4, rows[4,2]),
                                      rep(mean_1_S5, rows[5,2]),
                                      rep(mean_1_S6, rows[6,2]),
                                      rep(mean_1_S7, rows[7,2]),
                                      rep(mean_1_S8, rows[8,2])), 
                         sd = i1_sd), 0)


  
#bull_kelp <- rpois(n, lambda = c(rep(lam_1_S1, rows[1,2]),
#                                 rep(lam_1_S2, rows[2,2]), 
#                                 rep(lam_1_S3, rows[3,2]), 
#                                 rep(lam_1_S4, rows[4,2]),
#                                 rep(lam_1_S5, rows[5,2]),
#                                 rep(lam_1_S6, rows[6,2]),
#                                 rep(lam_1_S7, rows[7,2]),
#                                 rep(lam_1_S8, rows[8,2])))

#kelp_crabs <- rpois(n, lambda = c(rep(lam_2_S1, rows[1,2]),
#                                  rep(lam_2_S2, rows[2,2]), 
#                                  rep(lam_2_S3, rows[3,2]), 
#                                  rep(lam_2_S4, rows[4,2]),
#                                  rep(lam_2_S5, rows[5,2]),
#                                  rep(lam_2_S6, rows[6,2]),
#                                  rep(lam_2_S7, rows[7,2]),
#                                  rep(lam_2_S8, rows[8,2])))

df.2 <- cbind(bull_kelp, kelp_crabs)
df.2 <- as.data.frame(df.2)
df.2$SU <- seq(1:nrow(df.2)) 

return(df.2)

}


## function to bind together percent-coverage, abundances, and metadata
bind.data <- function(df.1, df.2, input.file){
  
  setwd(data_output)
  metadata <- read.csv(input.file)
  
  df <- cbind(df.1, df.2, metadata)
  return(df)
  
}
## END functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
