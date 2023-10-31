#'@title sighting check
#'@param surveys db download of the survey dolphin file from MMDD
#'@param focals db download of the focal dolphin file from MMDD
#'@param demography db download of life history file from MMDD
#'@import dplyr
#'@export

sighting.check <- function(surveys, focals, demography) {
  
  all.sightings <- agg.sightings(surveys, focals)
  
  demo <- demography %>%
    select(Dolphin.ID, Birth.Date, Death.Date)

  sighting.check <- all.sightings %>%
  inner_join(demo, by = c("Dolphin.ID" = "Dolphin.ID"))  %>%
  filter(Observation.Date < Birth.Date |
           Observation.Date > Death.Date)
  
  return(sighting.check)
  
}

