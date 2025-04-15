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

if (!require(hms)) {
  install.packages("hms")
  library(hms)
}

if (!require(statsbombCleaning)) {
  install_github("ElliotJoyce09/statsbombCleaning")
  library(statsbombCleaning)
}

if (!require(pROC)) {
  install.packages("pROC")
  library(pROC)
}

source("indicator_functions.R")

# finds the competitions where the tracking data is available
competitions <- FreeCompetitions() %>%
  filter(!is.na(match_available_360))

# Euro 2024 has an id of 55 and the season of it is 2024
euro2024matches <- FreeMatches(competitions %>%
                                 filter(competition_id == 55 &
                                          season_name == 2024))

euro2024_events <- get.opposingteam(allclean(free_allevents(euro2024matches, Parallel = TRUE)))

euro2024_events <- lowercase_clean_dataframe(euro2024_events)

# calculating the proportion in each column that an entry is NA
missing_proportions <- colMeans(is.na(euro2024_events))
# if missing_proportions is non-zero, then the column has some NA entries
number_of_non_full_columns <- length(missing_proportions[missing_proportions != 0])
number_of_high_missing_columns <- length(missing_proportions[missing_proportions > 0.9])
highest_missing_columns <- head(sort(missing_proportions, decreasing = TRUE), n = 10)

cleaned_dataframe <- modify_statsbomb_events_dataframe(euro2024_events)

# retrieves the most frequent 20 entries in type.name that contain "pass"
frequent_pass_attempts_dataframe <- cleaned_dataframe %>%
  filter(grepl("pass", type.name, ignore.case = TRUE)) %>%
  count(type.name, sort = TRUE) %>%
  slice_max(n, n = 20)

# retains the tracking data
euro2024_360data <- free_allevents_360(euro2024matches, Parallel = TRUE) %>%
  rename(id = event_uuid)

# takes the polygon for assist for Florian Wirtz's goal against Scotland
polygon_vector <- euro2024_360data %>%
  filter(id == "76b03cff-22f8-4a32-b4a0-35607970ad95")
polygon_vector <- polygon_vector$visible_area[[1]]

# the odd numbers of the vector are the x-coordinates, and the even are the y-coordinates
polygon_dataframe <- data.frame(x = 120 - polygon_vector[seq(1, length(polygon_vector), by = 2)], y = polygon_vector[seq(2, length(polygon_vector), by = 2)])

merged_dataframe <- join_360_data(cleaned_dataframe, euro2024_360data)

# takes the locations and finds which are out of bounds of the pitch
locations_matrix <- do.call(rbind, merged_dataframe$location)
out_of_bounds <- locations_matrix[, 1] < 0 |
  locations_matrix[, 1] > 120 |
  locations_matrix[, 2] < 0 |
  locations_matrix[, 2] > 80

measure_of_defensive_area_occupied_dataframe <- measure_of_defensive_area_occupied(merged_dataframe)
measure_of_defensive_area_occupied_dataframe <- change_timestamp(measure_of_defensive_area_occupied_dataframe)
measure_of_defensive_area_occupied_dataframe_intervals <- measure_of_defensive_area_occupied_dataframe %>%
  mutate(interval = ceiling(timestamp / 300)) %>%
  group_by(match_id, timestamp) %>%
  mutate(other_row_sum = ifelse(row_number() == 1, lead(row_sum), lag(row_sum))) %>%
  ungroup() %>%
  group_by(match_id, interval, team) %>%
  summarise(
    average_MDAO = mean(row_sum, na.rm = TRUE),
    opposition_average_MDAO = mean(other_row_sum, na.rm = TRUE)
  )

mdao_possession_stats <- measure_of_defensive_area_occupied_dataframe %>%
  filter(team == possession_team.name) %>%
  summarise(
    mean_value = mean(row_sum, na.rm = TRUE),
    sd_value = sd(row_sum, na.rm = TRUE)
  )

mdao_non_possession_stats <- measure_of_defensive_area_occupied_dataframe %>%
  filter(team != possession_team.name) %>%
  summarise(
    mean_value = mean(row_sum, na.rm = TRUE),
    sd_value = sd(row_sum, na.rm = TRUE)
  )

germany_scotland_mdao <- measure_of_defensive_area_occupied_dataframe %>%
  filter(match_id == 3930158) %>%
  pivot_wider(names_from = team, values_from = row_sum) %>%
  mutate(difference = -(germany - scotland)) %>%
  filter(!is.na(difference))

loess_fit <- loess(difference ~ as.numeric(timestamp),
                   data = germany_scotland_mdao,
                   span = 0.1)

germany_scotland_mdao <- germany_scotland_mdao %>%
  mutate(smooth = predict(loess_fit))

progressive_actions_dataframe <- progressive_actions(cleaned_dataframe)
progressive_actions_dataframe <- change_timestamp(progressive_actions_dataframe)
progressive_actions_dataframe_intervals <- progressive_actions_dataframe %>%
  mutate(interval = ceiling(timestamp / 300)) %>%
  group_by(match_id, interval, team.name) %>%
  summarise(number_of_progressive_actions = sum(progressive_action)) %>%
  rename(team = team.name)

defenders_removed_dataframe <- defenders_removed(merged_dataframe)
defenders_removed_dataframe <- change_timestamp(defenders_removed_dataframe)
defenders_removed_dataframe_intervals <- defenders_removed_dataframe %>%
  mutate(interval = ceiling(timestamp / 300)) %>%
  group_by(match_id, interval, team.name) %>%
  summarise(
    number_of_defender_removing_actions = n(),
    number_of_defenders_removed_from_actions = sum(number_of_defenders_removed),
    number_of_high_defender_removing_actions = sum(number_of_defenders_removed >= 5)
  ) %>%
  rename(team = team.name)

turnover_dataframe <- turnovers(cleaned_dataframe)
turnover_dataframe <- change_timestamp(turnover_dataframe)
turnover_dataframe_intervals <- turnover_dataframe %>%
  group_by(match_id) %>%
  mutate(team = ifelse(
    team.name == first(team.name),
    last(team.name),
    first(team.name)
  )) %>%
  ungroup() %>%
  mutate(interval = ceiling(timestamp / 300)) %>%
  group_by(match_id, interval, team) %>%
  summarise(
    number_of_opposition_turnovers = n(),
    number_of_opposition_turnovers_in_their_own_half = sum(turnover_distance_to_own_goal <= 60),
    opposition_turnover_closest_to_their_own_goal = min(turnover_distance_to_own_goal)
  )

successful_pressures_dataframe <- defensive_forced_pressures(cleaned_dataframe)
successful_pressures_dataframe <- change_timestamp(successful_pressures_dataframe)
successful_pressures_dataframe_intervals <- successful_pressures_dataframe %>%
  mutate(interval = ceiling(timestamp / 300)) %>%
  group_by(match_id, interval, team.name) %>%
  summarise(
    number_of_successful_pressures = n(),
    number_of_successful_pressures_in_opposition_half = sum(successful_pressure_distance_to_opposition_goal <= 60),
    successful_pressure_closest_to_opposition_goal = min(successful_pressure_distance_to_opposition_goal)
  ) %>%
  rename(team = team.name)

expected_goals_dataframe_intervals <- change_timestamp(cleaned_dataframe) %>%
  mutate(interval = ceiling(timestamp / 300)) %>%
  group_by(match_id, interval, team.name) %>%
  summarise(
    number_of_shots = sum(!is.na(shot.statsbomb_xg)),
    sum_of_expected_goals = sum(shot.statsbomb_xg, na.rm = TRUE),
    sum_of_non_penalty_expected_goals = sum(shot.statsbomb_xg[!grepl("penalty", type.name)], na.rm = TRUE)
  ) %>%
  rename(team = team.name)

modelling_dataframe <- measure_of_defensive_area_occupied_dataframe_intervals %>%
  left_join(progressive_actions_dataframe_intervals,
            by = c("match_id", "interval", "team")) %>%
  left_join(defenders_removed_dataframe_intervals,
            by = c("match_id", "interval", "team")) %>%
  left_join(turnover_dataframe_intervals,
            by = c("match_id", "interval", "team")) %>%
  left_join(successful_pressures_dataframe_intervals,
            by = c("match_id", "interval", "team")) %>%
  left_join(expected_goals_dataframe_intervals,
            by = c("match_id", "interval", "team")) %>%
  mutate(across(
    c(
      number_of_progressive_actions,
      number_of_defender_removing_actions,
      number_of_defenders_removed_from_actions,
      number_of_high_defender_removing_actions,
      number_of_opposition_turnovers,
      number_of_opposition_turnovers_in_their_own_half,
      number_of_successful_pressures,
      number_of_successful_pressures_in_opposition_half
    ),
    ~ replace_na(.x, 0)
  )) %>%
  mutate(across(
    c(
      opposition_turnover_closest_to_their_own_goal,
      successful_pressure_closest_to_opposition_goal
    ),
    ~ replace_na(.x, sqrt(120 ^ 2 + 40 ^ 2))
  )) %>%
  ungroup()

binary_modelling_dataframe <- modelling_dataframe %>%
  mutate(shots_occur = ifelse(number_of_shots > 0, 1, 0)) %>%
  ungroup()
