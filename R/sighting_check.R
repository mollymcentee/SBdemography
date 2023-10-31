#'@title sighting check
#'@param sightings df of sighitngs with Observation.Date and Dolphin.ID as columns - output from sightings function
#'@param focals db download of the focal dolphin file from MMDD
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

