## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## wrangle data prior to simulation  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
rm(list = ls())


## read in libraries
library(tidyverse)
library(readr)


## check wd is appropriate
getwd()


## hardcode relative file paths
code <- "../code"
data_input <- "../data_input"
data_output <- "../data_output"
figs <- "../figs"
## END start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##





## wrangle data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## function to place the last column [, ncol] in the first column position i.e. [, 1]
## this function is invoked to place the "key" column created below in the first column position 
front.ofthe.line <- function(data){
  num.col <- ncol(data)
  data <- data[c(num.col, 1:num.col-1)]
  return(data)
}


## create a unique site-transect key (for data with multiple transects)
create.key <- function(data){
  data$key <- data$site
  data$key <- with(data, paste0(key, "_", transect))
  data <- front.ofthe.line(data)
  return(data)
}


## function to bind and process multiple csv files 
wrangle.csv <- function(file.name){
  
  setwd(data_input)
  data <- list.files(path=data_input) %>%
    lapply(read_csv) %>%
    bind_rows
  
  data <- arrange(data, site, transect)
  data <- create.key(data)
  
  data <- subset(data, select=c("key", "site", "transect", 
                                "depth", "altitude", "DVLlat", 
                                "DVLlon", "EucDIS", "area", "time", "date"))
  
  setwd(data_output)
  write.csv(data, file.name, row.names = FALSE)
  return(data)
}


## function that returns every nth row to simulate non-overlapping, extracted still images
every.nth.row <- function(file.name, nth, output.file){
  setwd(data_output)
  data <- read.csv(file.name)
  data <- data[seq(1, nrow(data), nth), ]
  setwd(data_output)
  write.csv(data, output.file, row.names = FALSE)
  return(data)
}


## function to create .csv with transect specific row lengths to guide simulation 
nrows.transect <- function(data, file.name){
  nrows <- data %>%
    group_by(key) %>%
    summarise(no_rows = length(key))
  setwd(data_output)
  write.csv(nrows, file.name, row.names = FALSE)
  return(nrows)
}


## function to create .csv with site specific row lengths to guide simulation
nrows.site <- function(data, file.name){
  nrows <- data %>%
    group_by(site) %>%
    summarise(no_rows = length(site))
  setwd(data_output)
  write.csv(nrows, file.name, row.names = FALSE)
  return(nrows)
}
## END functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##






## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
