#'@title create sighting df
#'@description outputs a df containing each individual's sighting history
#'@param surveys db download of the survey dolphin file from MMDD
#'@param focals db download of the focal dolphin file from MMDD
#'@param film 
#'@param biopsies 
#'@param google_doc
#'@param misc
#'@export



sightings <- function(surveys, focals, film, biopsies, google_doc, misc) {
  
  ###combine survey and focal sightings, remove low certainty observations, duplicates, and update formats
  surveys <- surveys[c("Observation.Date", "Dolphin.ID", "Dolphin.ID.Certainty")]
  surveys$Observation.Date <- as.Date(surveys$Observation.Date)
  surveys$Observation <- "survey"
  
  
  focals <- focals[c("Observation.Date", "Dolphin.ID", "Dolphin.ID.Certainty")]
  focals$Observation.Date <- as.Date(focals$Observation.Date)
  focals$Observation <- "focal"
  
  
  film <- film[,c("Observation.Date", "Dolphin.ID", "Dolphin.ID.Certainty")]
  film$Observation.Date <- as.Date(film$Observation.Date)
  film$Observation <- "film"
  
  
  biopsies <- biopsies[,c("Observation.Date", "Dolphin.ID", "Dolphin.ID.Certainty")]
  biopsies$Observation.Date <- as.Date(biopsies$Observation.Date)
  biopsies$Observation <- "biopsy"
  
  google_doc <- google_doc[,c("Observation.Date", "Dolphin.ID", "Dolphin.ID.Certainty")]
  google_doc$Observation.Date <- as.Date(google_doc$Observation.Date)
  google_doc$Observation <- "google_doc"
  
  misc <- misc[,c("Observation.Date", "Dolphin.ID", "Dolphin.ID.Certainty")]
  misc$Observation.Date <- as.Date(misc$Observation.Date)
  misc$Observation <- "misc"
  
  all.sightings <- rbind(surveys, focals, film, biopsies, google_doc, misc)
  all.sightings <- subset(all.sightings, Dolphin.ID.Certainty != "LOW")
  all.sightings <- all.sightings[,-3]
  
  all.sightings$Observation <- factor(all.sightings$Observation, 
                                      levels = c("survey", "focal", "film", "biopsy", "misc", "google_doc") )
  all.sightings <- all.sightings[order(all.sightings$Observation),]
  
  all.sightings <- all.sightings[!duplicated(all.sightings[,c("Dolphin.ID", "Observation.Date")]),]
  all.sightings <- all.sightings[complete.cases(all.sightings),]
  

  all.sightings$Observation.Date <- as.Date(all.sightings$Observation.Date)
  all.sightings$Dolphin.ID <- as.factor(all.sightings$Dolphin.ID)
  
  return (all.sightings)
  
}
  