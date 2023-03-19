## code to prepare `CurrentLanduse` dataset goes here

library(terra)

set.seed(2023)
Landuse <- matrix(sample(1:3, 9, replace = T),
                  nrow = 3, ncol = 3, byrow = TRUE)


CurrentLanduse <- rast(Landuse)

LU <- data.frame(id=1:3, Sutiability=c("Agriculture", "Forest", "Urban"))

levels(CurrentLanduse) <- LU

CurrentLanduse <- terra::wrap(CurrentLanduse)

usethis::use_data(CurrentLanduse, overwrite = TRUE)
