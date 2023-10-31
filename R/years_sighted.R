#'@title years sighted
#'@param sightings df of sighitngs with Observation.Date and Dolphin.ID as columns - output from sightings function
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
