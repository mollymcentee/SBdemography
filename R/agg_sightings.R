#'@title aggregate sightings
#'@param surveys db download of the survey dolphin file from MMDD
#'@param focals db download of the focal dolphin file from MMDD
#'@export



agg.sightings <- function(surveys, focals) {
  
  ###combine survey and focal sightings, remove low certainty observations, duplicates, and update formats
  surveys <- surveys[c("Observation.Date", "Dolphin.ID", "Dolphin.ID.Certainty")]
  focals <- focals[c("Observation.Date", "Dolphin.ID", "Dolphin.ID.Certainty")]
  all.sightings <- rbind(surveys, focals)
  all.sightings <- subset(all.sightings, Dolphin.ID.Certainty != "LOW")
  all.sightings <- all.sightings[c("Observation.Date", "Dolphin.ID")]
  all.sightings <- unique(all.sightings)
  all.sightings$Observation.Date <- as.Date(all.sightings$Observation.Date)
  all.sightings$Dolphin.ID <- as.factor(all.sightings$Dolphin.ID)
  
  return (all.sightings)
  
}
  