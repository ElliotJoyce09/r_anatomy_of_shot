if (!require(tidyverse)) {
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require(MASS)) {
  install.packages("MASS")
  library(MASS)
}

# works out the mean of the number of shots that occur in an interval
shots_lambda_hat <- mean(modelling_dataframe$number_of_shots)
shots_observed_counts <- table(modelling_dataframe$number_of_shots)
shot_values <- as.integer(names(shots_observed_counts))
expected_probabilities <- dpois(shot_values, lambda = shots_lambda_hat)
expected_counts <- expected_probabilities * sum(shots_observed_counts)
chisq.test(x = shots_observed_counts, p = expected_probabilities, rescale.p = TRUE)

# here those with 4 and 5 are combined into 4, as the expected frequency is below 5
shots_observed_counts["4"] <- shots_observed_counts["4"] + shots_observed_counts["5"]
shots_observed_counts <- shots_observed_counts[-which(names(shots_observed_counts) == "5")]

# the new lambda_hat can be calculated then
new_shots_lambda_hat <-  sum(as.integer(names(shots_observed_counts)) * shots_observed_counts) / sum(shots_observed_counts)
shot_values <- as.integer(names(shots_observed_counts))
expected_probabilities <- dpois(shot_values, lambda = new_shots_lambda_hat)
expected_counts <- expected_probabilities * sum(shots_observed_counts)
chisq_test_result <- chisq.test(x = shots_observed_counts, p = expected_probabilities, rescale.p = TRUE)

variance_number_of_shots <- sd(modelling_dataframe$number_of_shots)^2

# fitting a negative binomial model
fit_nb <- glm.nb(number_of_shots ~ 1, data = modelling_dataframe)

# the estimated parameters are given as follows
mu_hat <- exp(coef(fit_nb))
theta_hat <- fit_nb$theta

r_hat <- theta_hat
p_hat <- unname(theta_hat / (theta_hat + mu_hat))

new_expected_probabilities <- dnbinom(shot_values, size = theta_hat, mu = mu_hat)
new_expected_counts <- new_expected_probabilities * sum(shots_observed_counts)
chisq.test(x = shots_observed_counts, p = new_expected_probabilities, rescale.p = TRUE)

# creates a matrix of the explanatory variables
explanatory_variables_matrix <- modelling_dataframe %>%
  ungroup() %>%
  dplyr::select(
    -c(
      match_id,
      interval,
      team,
      number_of_shots,
      sum_of_expected_goals,
      sum_of_non_penalty_expected_goals
    )
  ) %>%
  as.matrix()

correlation_explanatory_variables_matrix <- cor(explanatory_variables_matrix)

modelling_dataframe_opposition_turnover_filtered <- modelling_dataframe %>%
  filter(
    opposition_turnover_closest_to_their_own_goal != max(opposition_turnover_closest_to_their_own_goal)
  )

modelling_dataframe_successful_pressure_filtered <- modelling_dataframe %>%
  filter(
    successful_pressure_closest_to_opposition_goal != max(successful_pressure_closest_to_opposition_goal)
  )

progressive_actions_summary <- modelling_dataframe %>%
  group_by(number_of_shots) %>%
  summarise(
    mean_progressive_actions = mean(number_of_progressive_actions, na.rm = TRUE),
    sd_progressive_actions = sd(number_of_progressive_actions, na.rm = TRUE),
    count = n()
  )

progressive_actions_running_counts <- as.data.frame(
  table(
    modelling_dataframe$number_of_shots,
    modelling_dataframe$number_of_progressive_actions
  )
)
colnames(progressive_actions_running_counts) <- c("number_of_shots",
                                                  "number_of_progressive_actions",
                                                  "frequency")
progressive_actions_running_counts$number_of_shots <- as.numeric(as.character(progressive_actions_running_counts$number_of_shots))
progressive_actions_running_counts$number_of_progressive_actions <- as.numeric(as.character(
  progressive_actions_running_counts$number_of_progressive_actions
))

full_grid_progressive_actions <- expand.grid(
  number_of_shots = unique(progressive_actions_running_counts$number_of_shots),
  number_of_progressive_actions = 0:14
)

complete_progressive_actions_running_counts <- full_grid_progressive_actions %>%
  left_join(
    progressive_actions_running_counts,
    by = c("number_of_shots", "number_of_progressive_actions")
  ) %>%
  mutate(frequency = ifelse(is.na(frequency), 0, frequency))

modelling_dataframe <- modelling_dataframe %>%
  mutate(
    defender_removing_actions_bin = cut(
      number_of_defender_removing_actions,
      breaks = seq(
        0,
        max(number_of_defender_removing_actions, na.rm = TRUE) + 4,
        by = 4
      ),
      include.lowest = TRUE,
      right = TRUE
    )
  )

defender_removing_actions_summary <- modelling_dataframe %>%
  group_by(number_of_shots) %>%
  summarise(
    mean_defender_removing_actions = mean(number_of_defender_removing_actions, na.rm = TRUE),
    sd_defender_removing_actions = sd(number_of_defender_removing_actions, na.rm = TRUE),
    count = n()
  )

defender_removing_actions_running_counts <- as.data.frame(
  table(
    modelling_dataframe$number_of_shots,
    modelling_dataframe$defender_removing_actions_bin
  )
)
colnames(defender_removing_actions_running_counts) <- c("number_of_shots",
                                                        "number_of_defender_removing_actions",
                                                        "frequency")


