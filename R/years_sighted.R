#'@title years sighted
#'@description Outputs a dataframe of all of the years each individual was sighted, and the gaps between those years. 
#'  Used for assigning death dates.
#'@param sightings df of sightings with Observation.Date and Dolphin.ID as columns - output from sightings function
#'@import lubridate
#'@import dplyr
#'@export



years.sighted <- function(sightings) {
  
  years <- sightings %>%
    mutate(year = year(Observation.Date)) %>%
    select(Dolphin.ID, year) %>%
    distinct() %>%
    group_by(Dolphin.ID) %>%
    arrange(year) %>%
    mutate(diff = year - lag(year))

  return (years)
}
