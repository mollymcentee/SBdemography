#'@title years sighted
#'@param surveys db download of the survey dolphin file from MMDD
#'@param focals db download of the focal dolphin file from MMDD
#'@import lubridate
#'@import dplyr
#'@export



years.sighted <- function(surveys, focals) {
  
  all.sightings <- agg.sightings(surveys, focals)
  
  years <- all.sightings %>%
    mutate(year = year(Observation.Date)) %>%
    select(Dolphin.ID, year) %>%
    distinct() %>%
    group_by(Dolphin.ID) %>%
    arrange(year) %>%
    mutate(diff = year - lag(year))

  return (years)
  
}
