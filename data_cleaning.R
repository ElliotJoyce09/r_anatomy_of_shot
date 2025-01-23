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

source("indicator_functions.R")

# if (!require(pROC)) {
#   install.packages("pROC")
#   library(pROC)
# }
# 
# if (!require(caret)) {
#   install.packages("caret")
#   library(caret)
# }
# 
# if (!require(e1071)) {
#   install.packages("e1071")
#   library(e1071)
# }
# 
# if (!require(glmnet)) {
#   install.packages("glmnet")
#   library(glmnet)
# }


# detach("package:statsbombCleaning", unload = TRUE)

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

turnover_dataframe <- turnovers(cleaned_dataframe)

pressure_forcing_failure_dataframe <- defensive_forced_pressures(cleaned_dataframe)

### not really 'own half' it is like a circle, maybe should change
basic_modelling_dataframe <- change_timestamp(cleaned_dataframe) %>%
  mutate(timestamp = as_hms(as.POSIXct(timestamp, "GMT"))) %>%
  left_join(measure_of_defensive_area_occupied_dataframe, by = "id") %>%
  left_join(progressive_actions_dataframe, by = "id") %>%
  left_join(defenders_removed_dataframe, by = "id") %>%
  left_join(turnover_dataframe, by = "id") %>%
  left_join(pressure_forcing_failure_dataframe, by = "id") %>%
  mutate(in_seconds = as.numeric(as.difftime(timestamp, format = "%H:%M:%OS"))) %>%
  mutate(interval = ceiling(in_seconds / 300)) %>%
  select(id, match_id, interval, team, possession_team.name.y, shot.statsbomb_xg, row_sum,
         progressive_action,
         number_of_defenders_removed,
         turnover_distance_to_own_goal,
         failure_from_opposition_pressure_distance_to_own_goal) %>%
  filter(!is.na(team)) %>%
  group_by(id) %>%
  mutate(other_row_sum = ifelse(row_number() == 1, lead(row_sum), lag(row_sum))) %>%
  ungroup() %>%  
  ## filter(as.character(team.name) == as.character(team)) %>%
  filter(!is.na(match_id), !is.na(interval), !is.na(team)) %>%
  group_by(match_id, interval, team) %>%
  summarise(
    number_of_shots = sum(!is.na(shot.statsbomb_xg) & team == possession_team.name.y, na.rm = TRUE),
    average_row_sum = mean(row_sum, na.rm = TRUE),
    average_opposition_row_sum = mean(other_row_sum, na.rm = TRUE),
    number_of_progressive_actions = sum(!is.na(progressive_action) & team == possession_team.name.y, na.rm = TRUE),
    number_of_defender_removing_actions = sum(!is.na(number_of_defenders_removed) & team == possession_team.name.y, na.rm = TRUE),
    sum_of_defenders_removed = sum(number_of_defenders_removed[team == possession_team.name.y], na.rm = TRUE),
    number_of_high_defender_removing_actions = sum(number_of_defenders_removed > 5 & team == possession_team.name.y, na.rm = TRUE),
    number_of_turnovers_by_opposition = sum(!is.na(turnover_distance_to_own_goal) & team != possession_team.name.y, na.rm = TRUE),
    number_of_own_half_turnovers_by_opposition = sum(turnover_distance_to_own_goal <= 60 & team != possession_team.name.y, na.rm = TRUE),
    closest_turnover_by_opposition_to_their_goal = 
      if (any(team != possession_team.name.y & !is.na(turnover_distance_to_own_goal))) {
        min(turnover_distance_to_own_goal[team != possession_team.name.y], na.rm = TRUE)
      } else {
        NA
      },
    number_of_pressures_causing_failure_by_opposition = sum(!is.na(failure_from_opposition_pressure_distance_to_own_goal) & team != possession_team.name.y, na.rm = TRUE),
    number_of_own_half_pressures_causing_failure_by_opposition = sum(failure_from_opposition_pressure_distance_to_own_goal <= 60 & team != possession_team.name.y, na.rm = TRUE),
    closest_pressure_causing_failure_by_opposition_to_their_goal = 
      if (any(team != possession_team.name.y & !is.na(failure_from_opposition_pressure_distance_to_own_goal))) {
        min(failure_from_opposition_pressure_distance_to_own_goal[team != possession_team.name.y], na.rm = TRUE)
      } else {
        NA
      }
  ) %>%
  filter(!is.na(average_opposition_row_sum)) 


############################################

boxplot_of_opposition_MDAO_against_shots <- ggplot(basic_modelling_dataframe, aes(x = as.factor(number_of_shots), y = average_opposition_row_sum)) +
  geom_boxplot(fill = "#4A7C59") +
  labs(title = "Box-plot of the Average Opposition MDAO for each Number of Shots",
       subtitle = "For each 5-minute interval, for each team",
       x = "Number of Shots",
       y = "Average Opposition MDAO") +
  theme_minimal()

ggsave("boxplot_of_opposition_MDAO_against_shots.png", plot = boxplot_of_opposition_MDAO_against_shots, dpi = 200)

#### there is no autocorrelation so 
acf_result <- basic_modelling_dataframe %>%
  group_by(match_id, team) %>%
  summarise(
    acf_values = list(acf(number_of_shots, plot = FALSE)$acf)
  ) %>%
  mutate(acf_values = map(acf_values, ~ set_names(.x, paste0("lag_", seq_along(.x) - 1)))) %>%
  unnest_wider(acf_values, names_sep = "_") %>%
  ungroup()

average_acf <- acf_result %>%
  summarise(across(starts_with("acf_values_"), mean, na.rm = TRUE))

####


not_so_basic_modelling_dataframe <- change_timestamp(cleaned_dataframe) %>%
  mutate(timestamp = as_hms(as.POSIXct(timestamp, "GMT"))) %>%
  left_join(measure_of_defensive_area_occupied_dataframe, by = "id") %>%
  mutate(in_seconds = as.numeric(as.difftime(timestamp, format = "%H:%M:%OS"))) %>%
  mutate(interval = ceiling(in_seconds / 300)) %>%
  select(id, match_id, interval, team.name, team, shot.statsbomb_xg, row_sum) %>%
  filter(!is.na(team)) %>%
  group_by(id) %>%
  mutate(other_row_sum = ifelse(row_number() == 1, lead(row_sum), lag(row_sum))) %>%
  ungroup() %>%  filter(as.character(team.name) == as.character(team)) %>%
  filter(!is.na(match_id), !is.na(interval), !is.na(team)) %>%
  group_by(match_id, interval, team) %>%
  summarise(
    sum_of_xG = sum(shot.statsbomb_xg, na.rm = TRUE),
    average_row_sum = mean(row_sum, na.rm = TRUE),
    average_opposition_row_sum = mean(other_row_sum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(average_opposition_row_sum)) 


ggplot(not_so_basic_modelling_dataframe, aes(x = average_row_sum, y = sum_of_xG)) +
  geom_point() +
  geom_smooth(method = "loess")

ggplot(not_so_basic_modelling_dataframe, aes(x = sum_of_xG)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of sum_of_xG", x = "sum_of_xG", y = "Count") +
  theme_minimal()

ggplot(not_so_basic_modelling_dataframe, aes(x = sum_of_xG)) +
  geom_density(fill = "green", alpha = 0.5) +
  labs(title = "Density Plot of sum_of_xG", x = "sum_of_xG", y = "Density") +
  theme_minimal()

no_zero_not_so_basic_modelling_dataframe <- not_so_basic_modelling_dataframe %>%
  filter(sum_of_xG != 0)

ggplot(no_zero_not_so_basic_modelling_dataframe, aes(x = average_row_sum, y = sum_of_xG)) +
  geom_point() +
  geom_smooth(method = "loess")

ggplot(no_zero_not_so_basic_modelling_dataframe, aes(x = sum_of_xG)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of sum_of_xG", x = "sum_of_xG", y = "Count") +
  theme_minimal()

ggplot(no_zero_not_so_basic_modelling_dataframe, aes(x = sum_of_xG)) +
  geom_density(fill = "green", alpha = 0.5) +
  labs(title = "Density Plot of sum_of_xG", x = "sum_of_xG", y = "Density") +
  theme_minimal()


logit_modelling_dataframe <- basic_modelling_dataframe %>%
  mutate(shots_occur = ifelse(number_of_shots == 0, 0, 1))

logit_model <- glm(shots_occur ~ average_opposition_row_sum, data = logit_modelling_dataframe, family = binomial)
summary(logit_model)

roc_curve <- roc(logit_modelling_dataframe$shots_occur, fitted(logit_model))
plot(roc_curve)

auc(roc_curve)

predicted_probs <- predict(logit_model, type = "response")
pred_classes <- ifelse(predicted_probs > 0.5, 1, 0)
table(pred_classes, logit_modelling_dataframe$shots_occur)


find_optimal_threshold <- function(predicted_probs, true_lables) {
  thresholds <- seq(0, 1, by = 0.01)
  performance_metrics <- data.frame(threshold = thresholds, accuracy = NA, F1_score = NA)
  
  for(t in thresholds) {
    predicted_classes <- ifelse(predicted_probs > t, 1, 0)
    TP <- sum(true_lables == 1 & predicted_classes == 1)
    FP <- sum(true_lables == 0 & predicted_classes == 1)
    FN <- sum(true_lables == 1 & predicted_classes == 0)
    TN <- sum(true_lables == 0 & predicted_classes == 0)
    precision <- TP / (TP + FP)
    recall <- TP / (TP + FN)
    confusion_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(true_lables))
    performance_metrics[performance_metrics$threshold == t, "accuracy"] <- confusion_matrix$overall['Accuracy']
    performance_metrics[performance_metrics$threshold == t, "F1_score"] <- 2 * (precision * recall) / (precision + recall)
  }
  return(performance_metrics)
}

# highest F1 score is 0.606 at threshold 0.28
metrics <-  find_optimal_threshold(predicted_probs, logit_modelling_dataframe$shots_occur)

# SVM NOT GOOD!
svm_model <- svm(shots_occur ~ average_opposition_row_sum, data = logit_modelling_dataframe, type = "C-classification", kernel = "radial", probability = TRUE)
summary(svm_model)

roc_curve <- roc(logit_modelling_dataframe$shots_occur, as.numeric(fitted(svm_model)))
plot(roc_curve)

auc(roc_curve)

predicted_probs <- attr(predict(svm_model, logit_modelling_dataframe, probability = TRUE), "probabilities")[, 2]
# highest F1 score is 0.56 at threshold 0.29
metrics <-  find_optimal_threshold(predicted_probs, logit_modelling_dataframe$shots_occur)

nb_model <- naiveBayes(shots_occur ~ average_opposition_row_sum, data = logit_modelling_dataframe)

predicted_probs <- predict(nb_model, logit_modelling_dataframe, type = "raw")[, 2]
# highest F1 score is 0.606 at threshold 0.27
metrics <- find_optimal_threshold(predicted_probs, logit_modelling_dataframe$shots_occur)

### NEEDS TWO OR MORE COLUMNS
enet_model <- cv.glmnet(as.matrix(logit_modelling_dataframe[, "average_opposition_row_sum"]), logit_modelling_dataframe$shots_occur, family = "binomial", alpha = 0.5)





###################################################
model_shots <- glm(number_of_shots ~ average_opposition_row_sum, data = basic_modelling_dataframe, family = poisson)

basic_modelling_dataframe$prediction <- predict(model_shots, type = "response")
basic_modelling_dataframe$residuals <- residuals(model_shots, type = "pearson")

ggplot(basic_modelling_dataframe, aes(x = prediction, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Pearson Residuals") +
  theme_minimal()



# this is close to one so poisson holds!
dispersion_stat <- sum(residuals(model_shots, type = "pearson")^2) / df.residual(model_shots)

model_shots_negative_binomial <- glm.nb(number_of_shots ~ average_row_sum, data = basic_modelling_dataframe)

likelihood_ratio_test <- anova(model_shots, model_shots_negative_binomial, test = "Chisq")

further_model_shots <- glm(number_of_shots ~ average_row_sum + average_opposition_row_sum, data = basic_modelling_dataframe, family = poisson)

basic_modelling_dataframe$second_prediction <- predict(further_model_shots, type = "response")

ggplot(filter(basic_modelling_dataframe, match_id == "3930184"), aes(x = interval, colour = team)) +
  geom_bar(aes(y = number_of_shots), stat = "identity", fill = "white") +
  geom_point(aes(y = second_prediction)) +
  theme_minimal()

ggplot(basic_modelling_dataframe, aes(x = as.factor(number_of_shots), y = second_prediction)) +
  geom_violin() +
  theme_minimal()

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
