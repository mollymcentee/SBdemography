#'@title sighting check
#'@description outputs a df of observations that occurred before an individual's birth or after an individual's death in order to catch date typos or resurrections.
#'@param sightings df of sightings with Observation.Date and Dolphin.ID as columns - output from sightings function
#'@param demography db download of life history file from MMDD
#'@import dplyr
#'@export

sighting.check <- function(sightings, demography) {
  
  demo <- demography %>%
    select(Dolphin.ID, Birth.Date, Death.Date)

  sighting.check <- sightings %>%
  inner_join(demo, by = c("Dolphin.ID" = "Dolphin.ID"))  %>%
  filter(Observation.Date < Birth.Date |
           Observation.Date > Death.Date)
  
  return(sighting.check)
  
}

