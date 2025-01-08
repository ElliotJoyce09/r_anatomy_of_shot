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

if (!require(hms)) {
  install.packages("hms")
  library(hms)
}

## detach("package:statsbombCleaning", unload = TRUE)

if (!require(statsbombCleaning)) {
  install_github("ElliotJoyce09/statsbombCleaning")
  library(statsbombCleaning)
}

source("indicator_functions.R")

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

measure_of_defensive_area_occupied_dataframe <- measure_of_defensive_area_occupied(merged_dataframe)

progressive_actions_dataframe <- progressive_actions(cleaned_dataframe)

defenders_removed_dataframe <- defenders_removed(merged_dataframe)

#CHECK DISTANCE
turnover_dataframe <- turnovers(cleaned_dataframe)

#CHECK DISTANCE
pressure_forcing_failure_dataframe <- defensive_forced_pressures(cleaned_dataframe)

#CHECK DISTANCE
unsuccessful_pressure_dataframe <- unsuccessful_defensive_pressures(cleaned_dataframe)

modelling_dataframe <- change_timestamp(cleaned_dataframe) %>%
  mutate(timestamp = as_hms(as.POSIXct(timestamp))) %>%
  left_join(measure_of_defensive_area_occupied_dataframe, by = "id") %>%
  left_join(progressive_actions_dataframe, by = "id") %>%
  left_join(defenders_removed_dataframe, by = "id") %>%
  left_join(turnover_dataframe, by = "id") %>%
  left_join(pressure_forcing_failure_dataframe, by = "id") %>%
  left_join(unsuccessful_pressure_dataframe, by = "id") %>%
  select(match_id, timestamp, type.name, possession_team.name.y, possession, timeinposs, shot.statsbomb_xg, team, row_sum, progressive_action, number_of_defenders_removed, turnover_distance_to_own_goal, pressure_forcing_failure, forced_failiure_pressure_distance_to_opposition_goal, failed_defensive_pressure, unsuccessful_pressure_distance_to_own_goal) %>%
  filter(!is.na(row_sum)) %>%
  rename(possession_team = possession_team.name.y) %>%
  group_by(match_id, timestamp, type.name, possession_team, possession, timeinposs, shot.statsbomb_xg, progressive_action, number_of_defenders_removed, turnover_distance_to_own_goal, pressure_forcing_failure, forced_failiure_pressure_distance_to_opposition_goal, failed_defensive_pressure, unsuccessful_pressure_distance_to_own_goal) %>%
  summarise(
    possession_row_sum = sum(row_sum[team == possession_team]),
    oop_row_sum = sum(row_sum[team != possession_team])
  )



# defensive_actions <- cleaned_dataframe %>%
#   filter(location != "NULL") %>%
#   mutate(x = map_dbl(location, 1), y = map_dbl(location, 2)) %>%
#   filter(x >= 48, str_detect(type.name, "foul_committed|tackle|interception")) %>%
#   group_by(team.name, match_id) %>%
#   summarise(total_defensive_actions = n(), .groups = 'drop') %>%
#   rename("match" = "match_id", "team" = "team.name")
# 
# defensive_passes <- cleaned_dataframe %>%
#   filter(location != "NULL") %>%
#   mutate(x = map_dbl(location, 1), y = map_dbl(location, 2)) %>%
#   filter(x <= 72, str_detect(type.name, "pass")) %>%
#   group_by(team.name, match_id) %>%
#   summarise(total_defensive_passes = n(), .groups = 'drop') %>%
#   rename("match" = "match_id", "team" = "team.name")
#   
# opposition_teams <- c()
# 
# for (match_id in unique(defensive_passes$match)) {
#   match_data <- subset(defensive_passes, match == match_id)
#   for (i in 1:nrow(match_data)) {
#     opposition_teams[defensive_passes$match == match_id &
#                        defensive_passes$team == match_data$team[i]] <- match_data$team[match_data$team != match_data$team[i]]
#   }
# }
# 
# defensive_passes$opposition_team <- opposition_teams
# 
# ppda_dataframe <- defensive_actions %>%
#   left_join(defensive_passes, by = c("team" = "opposition_team", "match")) %>%
#   mutate(ppda = total_defensive_passes/total_defensive_actions) %>%
#   rename("opposition_team" = "team.y")
# 
# ppda_correlation_dataframe <- ppda_dataframe %>%
#   left_join(xG_dataframe, by = c("team" = "opposition_team", "match")) %>%
#   rename("opposition_xG" = "sum_xG") %>%
#   select(-team.y)
# 
# ppda_correlation <- cor(
#   ppda_correlation_dataframe$opposition_xG,
#   ppda_correlation_dataframe$ppda,
#   method = "pearson"
# )
# 
# ggplot(ppda_correlation_dataframe, aes(x = ppda, y = opposition_xG)) +
#   geom_point()
# 
# sum_group_stage_ppda_correlation_dataframe <- ppda_dataframe %>%
#   filter(match %in% group_stage_list_of_match_ids) %>%
#   left_join(xG_dataframe, by = c("team" = "opposition_team", "match")) %>%
#   rename("opposition_xG" = "sum_xG") %>%
#   group_by(team) %>%
#   summarise(
#     sum_total_defensive_actions = sum(total_defensive_actions),
#     sum_total_defensive_passes = sum(total_defensive_passes),
#     sum_opposition_xG = sum(opposition_xG)
#   ) %>%
#   mutate(average_ppda = sum_total_defensive_passes/sum_total_defensive_actions)
#   
# sum_group_stage_ppda_correlation <- cor(
#   sum_group_stage_ppda_correlation_dataframe$sum_opposition_xG,
#   sum_group_stage_ppda_correlation_dataframe$average_ppda,
#   method = "pearson"
# )
# 
# ggplot(sum_group_stage_ppda_correlation_dataframe, aes(x = average_ppda, y = sum_opposition_xG)) +
#   geom_point()
# 
# first_pressures_dataframe <- cleaned_dataframe %>%
#   filter(str_detect(type.name, "pressure"), team.name != possession_team.name) %>%
#   mutate(x = map_dbl(location, 1), y = map_dbl(location, 2)) %>%
#   group_by(match_id, possession) %>%
#   slice(1) %>%
#   ungroup() %>%
#   rename("match" = "match_id", "team" = "team.name")
# 
# average_first_pressures_dataframe <- first_pressures_dataframe %>%
#   group_by(match, team) %>%
#   summarise(mean_x = mean(x), number_of_pressures = n())
# 
# average_first_pressures_correaltion_dataframe <- average_first_pressures_dataframe %>%
#   left_join(xG_dataframe, by = c("team" = "opposition_team", "match")) %>%
#   rename("opposition_xG" = "sum_xG") %>%
#   select(-team.y)
#   
# average_first_pressures_correlation <- cor(
#   average_first_pressures_correaltion_dataframe$opposition_xG,
#   average_first_pressures_correaltion_dataframe$mean_x,
#   method = "pearson"
# )
# 
# ggplot(average_first_pressures_correaltion_dataframe, aes(x = mean_x, y = opposition_xG)) +
#   geom_point()
# 
# sum_group_stage_average_first_pressures_correlation_dataframe <- average_first_pressures_correaltion_dataframe %>%
#   filter(match %in% group_stage_list_of_match_ids) %>%
#   mutate(calculation_column = mean_x * number_of_pressures) %>%
#   group_by(team) %>%
#   summarise(
#     sum_number_of_pressures = sum(number_of_pressures),
#     sum_calculation_column = sum(calculation_column),
#     sum_opposition_xG = sum(opposition_xG, na.rm = TRUE)
#   ) %>%
#   mutate(mean_x = sum_calculation_column / sum_number_of_pressures)
# 
# sum_group_stage_average_first_pressures_correlation <- cor(
#   sum_group_stage_average_first_pressures_correlation_dataframe$sum_opposition_xG,
#   sum_group_stage_average_first_pressures_correlation_dataframe$mean_x,
#   method = "pearson"
# )
# 
# ggplot(sum_group_stage_average_first_pressures_correlation_dataframe, aes(x = mean_x, y = sum_opposition_xG)) +
#   geom_point()
# 
# progressive_actions_correaltion_dataframe <- progressive_actions %>%
#   left_join(xG_dataframe, by = c("team", "match")) %>%
#   select(-opposition_team)
# 
# progressive_actions_correlation <- cor(
#   progressive_actions_correaltion_dataframe$sum_xG,
#   progressive_actions_correaltion_dataframe$number_of_progressive_actions,
#   method = "pearson"
# )
# 
# ggplot(progressive_actions_correaltion_dataframe, aes(x = number_of_progressive_actions, y = sum_xG)) +
#   geom_point()
# 
# sum_group_stage_progressive_actions_correlation_dataframe <- progressive_actions_correaltion_dataframe %>%
#   filter(match %in% group_stage_list_of_match_ids) %>%
#   group_by(team) %>%
#   summarise(
#     sum_progressive_actions = sum(number_of_progressive_actions),
#     sum_xG = sum(sum_xG, na.rm = TRUE)
#   )
# 
# sum_group_stage_progressive_actions_correlation <- cor(
#   sum_group_stage_progressive_actions_correlation_dataframe$sum_xG,
#   sum_group_stage_progressive_actions_correlation_dataframe$sum_progressive_actions,
#   method = "pearson"
# )
# 
# ggplot(sum_group_stage_progressive_actions_correlation_dataframe, aes(x = sum_progressive_actions, y = sum_xG)) +
#   geom_point()
# 
# 
# defender_removing_actions_dataframe <- cleaned_dataframe %>%
#   filter(str_detect(type.name, "carry") | (str_detect(type.name, "complete") & str_detect(type.name, "pass")) & !str_detect(type.name, "incomplete")) %>%
#   group_by(match_id, team.name) %>%
#   summarise(number_of_defender_removing_actions = n())
# 
# defenders_removed_dataframe <- defenders_removed_dataframe %>%
#   left_join(defender_removing_actions_dataframe, by = c("team.name", "match_id")) 
# 
# defenders_removed_dataframe <- defenders_removed_dataframe %>%
#   mutate(average_defenders_removed = number_of_defenders_removed/number_of_defender_removing_actions)
