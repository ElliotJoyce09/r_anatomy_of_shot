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

if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
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

measure_of_defensive_area_occupied <- function(dataframe) {
  # retrieves the maximum timestamp value of the first half
  max_timestamp <- max(filter(dataframe, period == 1)$timestamp)
  # retrieves the two teams playing in the game
  team_names <- unique(dataframe$team.name)
  # a dataframe of zones
  zones <- data.frame(
    zone_id = 1:13,
    x_min = c(0, 18.8, 39.4, 0, 0, 18.8, 39.4, 18.8, 18.8, 18.8, 60, 78.8, 99.4),
    x_max = c(18.8, 39.4, 60, 18.8, 18.8, 39.4, 60, 60, 60, 60, 78.8, 99.4, 120),
    y_min = c(0, 0, 0, 16.3, 63.6, 63.6, 63.6, 50.8, 16.3, 29.2, 0, 0, 0),
    y_max = c(16.3, 16.3, 16.3, 63.6, 80, 80, 80, 63.6, 29.2, 50.8, 80, 80, 80)
  )
  # a dataframe of the scaling factors for each zones
  scaling_factors <- c("1" = 1/8, "2" = 1/16, "3" = 1/32, "4" = 1, "5" = 1/8, "6" = 1/16, "7" = 1/32, "8" = 1/2, "9" = 1/2, "10" = 1/4, "11" = 1/64, "12" = 1/128, "13" = 1/256)
  # now the second half timestamp's start from the end of the first half, and now each column is given a team value, depending on the team of the player's coordinates. Also only one event for each time
  dataframe_cleaned <- dataframe %>%
    mutate(timestamp = if_else(period == 2, as_hms(as.POSIXct(timestamp, format = "%H:%M:%OS")) + difftime(as_hms(as.POSIXct(max_timestamp, format = "%H:%M:%OS")), as_hms(as.POSIXct("00:00:00", format = "%H:%M:%OS"))), as_hms(as.POSIXct(timestamp, format = "%H:%M:%OS")))) %>%
    mutate(team = ifelse(teammate, team.name, ifelse(team.name == team_names[1], team_names[2], team_names[1]))) %>%
    group_by(timestamp) %>%
    filter(id == first(id)) %>%  
    ungroup() 
  # splits the location into x and y, and makes sure they are between [0, 120] and [0, 80] for x and y respectively. Also turns all of them to be in their defensive area starting at 0
  dataframe_coordinates <- dataframe_cleaned %>%
    mutate(x = map_dbl(location, 1), y = map_dbl(location, 2)) %>%
    mutate(x = pmax(0, pmin(x, 120)), y = pmax(0, pmin(y, 80))) %>%
    mutate(x = if_else(teammate == FALSE, 120 - x, x), y = if_else(teammate == FALSE, 80 - y, y))
  dataframe_zones <- dataframe_coordinates %>%
    rowwise() %>%
    mutate(zone = case_when(
      x >= zones$x_min[1] & x < zones$x_max[1] & y >= zones$y_min[1] & y <= zones$y_max[1] ~ 1,
      x >= zones$x_min[2] & x < zones$x_max[2] & y >= zones$y_min[2] & y <= zones$y_max[2] ~ 2,
      x >= zones$x_min[3] & x < zones$x_max[3] & y >= zones$y_min[3] & y <= zones$y_max[3] ~ 3,
      x >= zones$x_min[4] & x < zones$x_max[4] & y >= zones$y_min[4] & y <= zones$y_max[4] ~ 4,
      x >= zones$x_min[5] & x < zones$x_max[5] & y >= zones$y_min[5] & y <= zones$y_max[5] ~ 5,
      x >= zones$x_min[6] & x < zones$x_max[6] & y >= zones$y_min[6] & y <= zones$y_max[6] ~ 6,
      x >= zones$x_min[7] & x < zones$x_max[7] & y >= zones$y_min[7] & y <= zones$y_max[7] ~ 7,
      x >= zones$x_min[8] & x < zones$x_max[8] & y >= zones$y_min[8] & y <= zones$y_max[8] ~ 8,
      x >= zones$x_min[9] & x < zones$x_max[9] & y >= zones$y_min[9] & y <= zones$y_max[9] ~ 9,
      x >= zones$x_min[10] & x < zones$x_max[10] & y >= zones$y_min[10] & y <= zones$y_max[10] ~ 10,
      x >= zones$x_min[11] & x < zones$x_max[11] & y >= zones$y_min[11] & y <= zones$y_max[11] ~ 11,
      x >= zones$x_min[12] & x < zones$x_max[12] & y >= zones$y_min[12] & y <= zones$y_max[12] ~ 12,
      x >= zones$x_min[13] & x <= zones$x_max[13] & y >= zones$y_min[13] & y <= zones$y_max[13] ~ 13)) %>%
    ungroup()
  dataframe_zones_counts <- dataframe_zones %>%
    group_by(timestamp, team, zone) %>%
    summarise(count = n(), .groups = 'drop') %>%
    pivot_wider(names_from = zone, values_from = count, values_fill = 0)
  dataframe_summarised_zones_counts <- dataframe_zones_counts %>%
    mutate(across(names(scaling_factors), ~ . * scaling_factors[cur_column()])) %>%
    rowwise() %>%
    mutate(row_sum = sum(c_across(names(scaling_factors)))) %>%
    select(-c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))
}

# test3 <- measure_of_defensive_area_occupied(test)
# 
# ggplot(filter(zone_counts, period == 1), aes(x = timestamp, y = row_sum, colour = team)) + 
#   geom_smooth(method = "loess", span = 0.1) +  # Smooth line for running average
#   theme_minimal() +
#   labs(title = "Running Average of Row Sum", x = "Timestamp", y = "Row Sum")
# 
# ggplot(filter(zone_counts, period == 2), aes(x = timestamp, y = row_sum, colour = team)) + 
#   geom_smooth(method = "loess", span = 0.1) +  # Smooth line for running average
#   theme_minimal() +
#   labs(title = "Running Average of Row Sum", x = "Timestamp", y = "Row Sum")
# 
# team_averages <- zone_counts %>%
#   group_by(team) %>%
#   summarize(average_row_sum = mean(row_sum, na.rm = TRUE))
