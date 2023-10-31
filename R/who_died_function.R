#'@title who died
#'@description outputs a list of individuals who have exceeded their maximum sighting gap
#'@param sightings df of sighitngs with Observation.Date and Dolphin.ID as columns - output from sightings function
#'@param demography db download of life history file from MMDD
#'@param time.cutoff date we want to use to calculate time since the last sighting
#'@export



who.died <- function(sightings, demography, time.cutoff) {
  
  ###determine number of sightings of each dolphin
  num.sight <- aggregate(sightings$Dolphin.ID, by=list(sightings$Dolphin.ID), FUN=length)
  colnames(num.sight) <- c("Dolphin.ID", "num.sightings")
  
  ###Format demo data, add in number of sightings, and apply sighting cutoff
  demo <- demography[c("Dolphin.ID", "Birth.Date", "Death.Date")]
  demo$Birth.Date <- as.Date(demo$Birth.Date)
  demo$Death.Date <- as.Date(demo$Death.Date)
  
  demo <- merge(demo, num.sight)

  ###get last sighting date and time since last sight for each dolphin
  last.sight <- do.call(rbind, lapply(split(sightings,sightings$Dolphin.ID), function(x) {return(x[which.max(x$Observation.Date),])}))
  last.sight <- last.sight[c("Dolphin.ID", "Observation.Date")]
  colnames(last.sight)[2] <- "last.sight.date"
  last.sight$time.since.last.sight <- as.numeric((as.Date(time.cutoff) - last.sight$last.sight.date)/365.25)
  
  
  
  
  ###calculate maximum absolute gap in sightings for whole sighting history 
  sightings.gap.overall <- sightings[order(sightings$Dolphin.ID, sightings$Observation.Date), , drop = FALSE]
  
  sightings.gap.overall$gap <- unlist(tapply(sightings.gap.overall$Observation.Date, INDEX = sightings.gap.overall$Dolphin.ID,
                                              FUN = function(x) c(NA, diff(as.numeric(x)))))
  
  sightings.gap.overall$gap <- as.numeric(sightings.gap.overall$gap/365.25)
  sightings.gap.overall <- do.call(rbind, lapply(split(sightings.gap.overall, sightings.gap.overall$Dolphin.ID), function(x) {return(x[which.max(x$gap),])}))
  sightings.gap.overall <- sightings.gap.overall[c("Dolphin.ID", "gap")]
  
  ###calculate maximum gap in sightings for whole sighting history by season
  sightings.gap <- sightings[order(sightings$Dolphin.ID, sightings$Observation.Date), , drop = FALSE]
  sightings.gap$obs.year <- format(as.Date(sightings.gap$Observation.Date, format="%d/%m/%Y"),"%Y")
  
  sightings.gap$gap.year <- unlist(tapply(sightings.gap$obs.year, INDEX = sightings.gap$Dolphin.ID,
                                         FUN = function(x) c(NA, diff(as.numeric(x)))))
  
  sightings.gap <- do.call(rbind, lapply(split(sightings.gap, sightings.gap$Dolphin.ID), function(x) {return(x[which.max(x$gap.year),])}))
  sightings.gap <- sightings.gap[c("Dolphin.ID", "gap.year")]
  sightings.gap$gap.year <- ifelse(sightings.gap$gap.year <=2, 1,
                                       sightings.gap$gap.year - 1)
  
  
  ###calculate maximum gap in sightings for post 1995 sightings by season
  sightings.gap.1996 <- subset(sightings, Observation.Date >= as.Date('1996-01-01'))
  sightings.gap.1996 <- sightings.gap.1996[order(sightings.gap.1996$Dolphin.ID, sightings.gap.1996$Observation.Date), , drop = FALSE]
  
  sightings.gap.1996$obs.year <- format(as.Date(sightings.gap.1996$Observation.Date, format="%d/%m/%Y"),"%Y")
  
  sightings.gap.1996$gap.1996.year <- unlist(tapply(sightings.gap.1996$obs.year, INDEX = sightings.gap.1996$Dolphin.ID,
                                               FUN = function(x) c(NA, diff(as.numeric(x)))))
  
  sightings.gap.1996 <- do.call(rbind, lapply(split(sightings.gap.1996,sightings.gap.1996$Dolphin.ID), function(x) {return(x[which.max(x$gap.1996.year),])}))
  sightings.gap.1996 <- sightings.gap.1996[c("Dolphin.ID", "gap.1996.year")]
  sightings.gap.1996$gap.1996.year <- ifelse(sightings.gap.1996$gap.1996.year <=2, 1,
                                             sightings.gap.1996$gap.1996.year - 1)
  ###bring all the sighting info together
  sighting.info <- merge(demo, last.sight,  all.x=TRUE)
  sighting.info <- merge(sighting.info, sightings.gap.overall,  all.x=TRUE)
  sighting.info <- merge(sighting.info, sightings.gap,  all.x=TRUE)
  sighting.info <- merge(sighting.info, sightings.gap.1996,  all.x=TRUE)

  
  ###calculate age at last sighting and which gap we should use (overall or post 1996)
  sighting.info$age.at.last.sight <- as.numeric((sighting.info$last.sight.date - sighting.info$Birth.Date)/365.25)
  
  sighting.info$gap.to.use <- ifelse(is.na(sighting.info$gap.1996.year), sighting.info$gap.year,
                                     ifelse(sighting.info$gap.1996.year <= sighting.info$gap.year, 
                                            sighting.info$gap.1996.year, sighting.info$gap.year))
  

  ###determine if the individual exceeded the sighting cutoff
  sighting.info$exceeded.gap <- ifelse(sighting.info$age.at.last.sight < 30,
                                       ifelse(sighting.info$time.since.last.sight > 3*sighting.info$gap.to.use, 1, 0),
                                       ifelse(sighting.info$time.since.last.sight > 2*sighting.info$gap.to.use, 1, 0))
  
  ###select individuals who have exceeded the cutoff and do not have a death date in demo
  new.deaths <- subset(sighting.info, exceeded.gap == 1 & is.na(Death.Date))
  new.deaths <- new.deaths[c("Dolphin.ID", "Birth.Date", "Death.Date", "num.sightings", 
                             "gap",
                             "last.sight.date", "time.since.last.sight", "age.at.last.sight")]
  
  
  return(new.deaths)
}
