if(!require(devtools)) {
  install.packages("devtools")
  library(devtools)
}

if (!require(StatsBombR)) {
  install_github("statsbomb/StatsBombR")
  library(StatsBombR)
}

if (!require(tidyverse)) {
  install.packages("tidyverse")
  library(tidyverse)
}

## detach("package:statsbombCleaning", unload = TRUE)

if (!require(statsbombCleaning)) {
  install_github("ElliotJoyce09/statsbombCleaning")
  library(statsbombCleaning)
}

competitions <- FreeCompetitions() %>%
  filter(!is.na(match_available_360))

euro2024matches <- FreeMatches(competitions %>%
                                 filter(competition_id == 55 &
                                          season_name == 2024))


euro2024_events <- get.opposingteam(allclean(free_allevents(euro2024matches, Parallel = TRUE)))

euro2024_events <- lowercase_clean_dataframe(euro2024_events)

cleaned_dataframe <- modify_statsbomb_events_dataframe(euro2024_events)

euro2024_360data <- free_allevents_360(euro2024matches, Parallel = TRUE) %>%
  rename(id = event_uuid)

merged_dataframe <- join_360_data(cleaned_dataframe, euro2024_360data)
