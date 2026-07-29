## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NMDS functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




## bind the NMDS (x, y) ordination coordinates back onto the metadata +
## community matrix, one row per photo (the sample unit for this ordination)
save.points <- function(metadata, ord, comm){
  coords <- as.data.frame(ord$points)
  cbind(metadata, coords, comm)
}


## extract each percent-cover category's correlation coefficients (its
## position/vector in ordination space) as a data frame, one row per category
save.spp <- function(ord){
  scores(ord, display = "species") %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "category")
}




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## END script ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
