############################################################################### 
#                                                                 Summer 2024 #
#                                                          Ana Livia Oliveira #
# Falaise St. Jacques little project                                          #
#                                                                             #
############################################################################### 

# Check directory

getwd ()

###############################################################################
# Observations                                                                #
###############################################################################

library (tidyverse)

library (dplyr)

# Import data from directory
# GBIF.org (11 JULY 2024) https://doi.org/10.15468/dl.6m8t6b

falais_obs <- read.csv ("00_raw_data/falais_gbif.csv", sep = "\t")

# Take off the columns I don't need

falais_obs <- falais_obs [ , -c (1:7, 11:21, 24:50)]

# Rename columns

falais_obs <- falais_obs %>% 
  rename (
    longitude = decimalLongitude,
    latitude = decimalLatitude
  )

###############################################################################
# Observations inside the polygons                                            #
###############################################################################

# Import coordinates from directory
# https://www.keene.edu/campus/maps/tool/

coord_files <- c ("observations-falaise-saint-jacques-coord.txt")

library (sp)

library (mgcv)

# Remove rows with missing values 

falais_obs <- na.omit (falais_obs)

# Convert all character variables to factor variables

falais_obs %>% mutate_if (is.character, as.factor)

# Plot the observations inside the polygon and save new columns with the
# information "FALSE" (if the observations is not inside the polygon) and 
# "TRUE" (if the observations is inside the polygon)

for (x in coord_files) {
  poly_coords <- read.table (file = paste ("C:/Users/anali/OneDrive/Documentos/Concordia University/FUN team/00_raw_data/", 
                                           x, sep = ""), 
                             header = TRUE, sep = ",")
  p1 = Polygon (poly_coords)
  p1b = Polygons (list(p1),1)
  p1bp = SpatialPolygons (list(p1b))
  plot (p1bp)
  falais_obs_in_coord <- cbind (falais_obs$longitude, falais_obs$latitude)
  poly1 <- cbind (poly_coords$x, poly_coords$y)
  falais_obs_in <- in.out (poly1, falais_obs_in_coord)
  points (falais_obs_in_coord, col= as.numeric (falais_obs_in) + 1)
  falais_obs [x] <- falais_obs_in
}

# Rename columns

falais_obs <- falais_obs %>%
  rename (
    falaise_st_jacques = "observations-falaise-saint-jacques-coord.txt"
  )

# New data frame with the observed SPECIES and the new "FALSE" and 
# "TRUE" columns

falais_obs <- data.frame (falais_obs$species,
                          falais_obs$falaise_st_jacques)

# Change the "FALSE" and "TRUE" informations into "0" and "1"

falais_obs [falais_obs == "FALSE"] <- 0
falais_obs [falais_obs == "TRUE"] <- 1

# Sum all observations of the same SPECIES together

falais_obs <- falais_obs %>% 
  group_by (falais_obs.species) %>%
  summarise_each (list(sum))

# Rename columns

falais_obs <- falais_obs %>%
  rename (
    Species = falais_obs.species,
    Abundance = falais_obs.falaise_st_jacques
  )

# Takeoff species that are not inside the polygon

falais_obs <- falais_obs [- 1, ]

falais_obs <- falais_obs %>%
  filter (Abundance != 0)

# Save the data frame as .csv

write.csv (falais_obs, file = "01_tidy_data/falais_obs.csv")