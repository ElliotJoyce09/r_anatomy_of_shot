if (!require(tidyverse)) {
  install.packages("tidyverse")
  library(tidyverse)
}

binary_modelling_dataframe <- binary_modelling_dataframe %>%
  mutate(mdao_bin = ntile(average_MDAO, 20)) %>%
  mutate(opposition_mdao_bin = ntile(opposition_average_MDAO, 20)) %>%
  mutate(progressive_action_bin = ntile(number_of_progressive_actions, 20))

mdao_binned <- binary_modelling_dataframe %>%
  group_by(mdao_bin) %>%
  summarise(
    mean_predictor = mean(average_MDAO, na.rm = TRUE),
    prop = mean(shots_occur, na.rm = TRUE)
  ) %>%
  mutate(logit = log(prop / (1 - prop)))

opposition_mdao_binned <- binary_modelling_dataframe %>%
  group_by(opposition_mdao_bin) %>%
  summarise(
    mean_predictor = mean(opposition_average_MDAO, na.rm = TRUE),
    prop = mean(shots_occur, na.rm = TRUE)
  ) %>%
  mutate(logit = log(prop / (1 - prop)))

progressive_action_binned <- binary_modelling_dataframe %>%
  group_by(progressive_action_bin) %>%
  summarise(
    mean_predictor = mean(number_of_progressive_actions, na.rm = TRUE),
    prop = mean(shots_occur, na.rm = TRUE)
  ) %>%
  mutate(logit = log(prop / (1 - prop)))

binary_modelling_dataframe_summary <- binary_modelling_dataframe %>%
  group_by(shots_occur) %>%
  summarise(across(c("average_MDAO",
                     "opposition_average_MDAO",
                     "number_of_progressive_actions",
                     "number_of_defender_removing_actions",
                     "number_of_defenders_removed_from_actions",
                     "number_of_high_defender_removing_actions",
                     "number_of_opposition_turnovers",
                     "number_of_opposition_turnovers_in_their_own_half",
                     "number_of_successful_pressures",
                     "number_of_successful_pressures_in_opposition_half"), 
                   \(x) mean(x, na.rm = TRUE)))

successful_pressure_binary_dataframe <- binary_modelling_dataframe %>%
  filter(successful_pressure_closest_to_opposition_goal != max(successful_pressure_closest_to_opposition_goal))

binary_modelling_dataframe_successful_pressure_summary <- successful_pressure_binary_dataframe %>%
  group_by(shots_occur) %>%
  summarise(successful_pressure_closest_to_opposition_goal = mean(successful_pressure_closest_to_opposition_goal, na.rm = TRUE))

opposition_turnover_binary_dataframe <- binary_modelling_dataframe %>%
  filter(opposition_turnover_closest_to_their_own_goal != max(opposition_turnover_closest_to_their_own_goal))

binary_modelling_dataframe_opposition_turnover_summary <- opposition_turnover_binary_dataframe %>%
  group_by(shots_occur) %>%
  summarise(opposition_turnover_closest_to_their_own_goal = mean(opposition_turnover_closest_to_their_own_goal, na.rm = TRUE))
