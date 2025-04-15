if (!require(rpart)) {
  install.packages("rpart")
  library(rpart)
}

if (!require(glmnet)) {
  install.packages("glmnet")
  library(glmnet)
}

if (!require(caret)) {
  install.packages("caret")
  library(caret)
}

# the training dataset for the binary response of the two-part model
training_binary_modelling_dataframe <- training_modelling_dataframe %>%
  mutate(shots_occur = ifelse(number_of_shots == 0, 0, 1))

# the testing dataset for the binary response of the two-part model
testing_binary_modelling_dataframe <- testing_modelling_dataframe %>%
  mutate(shots_occur = ifelse(number_of_shots == 0, 0, 1))

# the training dataset for the continuous response of the two-part model
training_non_zero_response_modelling_dataframe <- training_modelling_dataframe %>%
  filter(sum_of_non_penalty_expected_goals > 0)

# the testing dataset for the continuous response of the two-part model
testing_non_zero_response_modelling_dataframe <- testing_modelling_dataframe %>%
  filter(sum_of_non_penalty_expected_goals > 0)

non_zero_response_training_matrix  <- as.matrix(training_non_zero_response_modelling_dataframe[, c(
  "average_MDAO",
  "opposition_average_MDAO",
  "number_of_progressive_actions",
  "number_of_defender_removing_actions",
  "number_of_defenders_removed_from_actions",
  "number_of_high_defender_removing_actions",
  "number_of_opposition_turnovers",
  "number_of_opposition_turnovers_in_their_own_half",
  "opposition_turnover_closest_to_their_own_goal",
  "number_of_successful_pressures",
  "number_of_successful_pressures_in_opposition_half",
  "successful_pressure_closest_to_opposition_goal"
)])

non_zero_response_training_vector <- log(
  training_non_zero_response_modelling_dataframe$sum_of_non_penalty_expected_goals
)

non_zero_response_testing_matrix  <- as.matrix(testing_binary_modelling_dataframe[, c(
  "average_MDAO",
  "opposition_average_MDAO",
  "number_of_progressive_actions",
  "number_of_defender_removing_actions",
  "number_of_defenders_removed_from_actions",
  "number_of_high_defender_removing_actions",
  "number_of_opposition_turnovers",
  "number_of_opposition_turnovers_in_their_own_half",
  "opposition_turnover_closest_to_their_own_goal",
  "number_of_successful_pressures",
  "number_of_successful_pressures_in_opposition_half",
  "successful_pressure_closest_to_opposition_goal"
)])

non_zero_response_testing_vector <- log(testing_binary_modelling_dataframe$sum_of_non_penalty_expected_goals)

logit_shots_occur_model <- glm(
  shots_occur ~ average_MDAO +
    sqrt(opposition_average_MDAO) +
    number_of_progressive_actions +
    number_of_defender_removing_actions +
    number_of_defenders_removed_from_actions +
    number_of_high_defender_removing_actions +
    number_of_opposition_turnovers +
    number_of_opposition_turnovers_in_their_own_half +
    opposition_turnover_closest_to_their_own_goal +
    number_of_successful_pressures +
    number_of_successful_pressures_in_opposition_half +
    successful_pressure_closest_to_opposition_goal,
  data = training_binary_modelling_dataframe,
  family = binomial
)

stepwise_AIC_logit_shots_occur_model <- stepAIC(logit_shots_occur_model, direction = "both", k = 2)

stepwise_BIC_logit_shots_occur_model <- stepAIC(logit_shots_occur_model,
                                                direction = "both",
                                                k = log(nrow(training_binary_modelling_dataframe)))

find_optimal_threshold <- function(predicted_probs, true_lables) {
  thresholds <- seq(0, 1, by = 0.01)
  performance_metrics <- data.frame(threshold = thresholds,
                                    Accuracy = NA,
                                    F1 = NA)
  
  for (t in thresholds) {
    predicted_classes <- ifelse(predicted_probs > t, 1, 0)
    TP <- sum(true_lables == 1 & predicted_classes == 1)
    FP <- sum(true_lables == 0 & predicted_classes == 1)
    FN <- sum(true_lables == 1 & predicted_classes == 0)
    TN <- sum(true_lables == 0 & predicted_classes == 0)
    precision <- TP / (TP + FP)
    recall <- TP / (TP + FN)
    confusion_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(true_lables))
    performance_metrics[performance_metrics$threshold == t, "Accuracy"] <- confusion_matrix$overall['Accuracy']
    performance_metrics[performance_metrics$threshold == t, "F1"] <- 2 * (precision * recall) / (precision + recall)
  }
  return(performance_metrics)
}

stepwise_AIC_logit_shots_occur_predicted_probabilities <- predict(stepwise_AIC_logit_shots_occur_model, type = "response")
stepwise_AIC_logit_shots_occur_metrics <-  find_optimal_threshold(
  stepwise_AIC_logit_shots_occur_predicted_probabilities,
  training_binary_modelling_dataframe$shots_occur
)

stepwise_AIC_logit_shots_occur_probabilities <- predict(stepwise_AIC_logit_shots_occur_model,
                                                        newdata = testing_binary_modelling_dataframe,
                                                        type = "response")
testing_binary_modelling_dataframe <- cbind(
  testing_binary_modelling_dataframe,
  stepwise_AIC_logit_shots_occur_probabilities
)

testing_binary_modelling_dataframe <- testing_binary_modelling_dataframe %>%
  mutate(predicted_shot_AIC = ifelse(stepwise_AIC_logit_shots_occur_probabilities >= 0.50, 1, 0))

table(
  testing_binary_modelling_dataframe$predicted_shot_AIC,
  testing_binary_modelling_dataframe$shots_occur
)

roc_AIC <- roc(
  testing_binary_modelling_dataframe$shots_occur,
  testing_binary_modelling_dataframe$stepwise_AIC_logit_shots_occur_probabilities
)

AIC_roc_data <- data.frame(TPR = roc_AIC$sensitivities,
                           FPR = 1 - roc_AIC$specificities)
AIC_roc_data <- AIC_roc_data[order(AIC_roc_data$FPR, AIC_roc_data$TPR), ]

decision_tree_shots_occur_model <- rpart(
  shots_occur ~ average_MDAO +
    opposition_average_MDAO +
    number_of_progressive_actions +
    number_of_defender_removing_actions +
    number_of_defenders_removed_from_actions +
    number_of_high_defender_removing_actions +
    number_of_opposition_turnovers +
    number_of_opposition_turnovers_in_their_own_half +
    opposition_turnover_closest_to_their_own_goal +
    number_of_successful_pressures +
    number_of_successful_pressures_in_opposition_half +
    successful_pressure_closest_to_opposition_goal,
  data = training_binary_modelling_dataframe,
  method = "class"
)

decision_tree_shots_occur_prediction <- predict(decision_tree_shots_occur_model,
                                                testing_binary_modelling_dataframe,
                                                type = "class")
testing_binary_modelling_dataframe$predicted_shot_decision_tree <- decision_tree_shots_occur_prediction
table(
  decision_tree_shots_occur_prediction,
  testing_binary_modelling_dataframe$shots_occur
)

decision_tree_shots_occur_probabilities <- predict(decision_tree_shots_occur_model,
                                                   testing_binary_modelling_dataframe,
                                                   type = "prob")[, 2]

roc_decision_tree <- roc(
  testing_binary_modelling_dataframe$shots_occur,
  decision_tree_shots_occur_probabilities
)

decision_tree_roc_data <- data.frame(TPR = roc_decision_tree$sensitivities,
                                     FPR = 1 - roc_decision_tree$specificities)
decision_tree_roc_data <- decision_tree_roc_data[order(decision_tree_roc_data$FPR, decision_tree_roc_data$TPR), ]

log_npxg_values <- log(
  training_non_zero_response_modelling_dataframe$sum_of_non_penalty_expected_goals
)
mu <- mean(log_npxg_values)
sigma <- sd(log_npxg_values)

log_sum_of_non_penalty_expected_goals_data <- as.data.frame(
  log(
    training_non_zero_response_modelling_dataframe$sum_of_non_penalty_expected_goals
  )
)

non_zero_response_model <- lm(
  log(sum_of_non_penalty_expected_goals) ~ average_MDAO +
    opposition_average_MDAO +
    number_of_progressive_actions +
    number_of_defender_removing_actions +
    number_of_defenders_removed_from_actions +
    number_of_high_defender_removing_actions +
    number_of_opposition_turnovers +
    number_of_opposition_turnovers_in_their_own_half +
    opposition_turnover_closest_to_their_own_goal +
    number_of_successful_pressures +
    number_of_successful_pressures_in_opposition_half +
    successful_pressure_closest_to_opposition_goal,
  data = training_non_zero_response_modelling_dataframe
)

stepwise_AIC_non_zero_response_model <- step(non_zero_response_model, direction = "both")
testing_binary_modelling_dataframe$predicted_response_AIC <- exp(
  predict(stepwise_AIC_non_zero_response_model, newdata = testing_binary_modelling_dataframe)
)

stepwise_BIC_non_zero_response_model <- step(non_zero_response_model,
                                             direction = "both",
                                             k = log(nrow(
                                               training_non_zero_response_modelling_dataframe
                                             )))
testing_binary_modelling_dataframe$predicted_response_BIC <- exp(
  predict(stepwise_BIC_non_zero_response_model, newdata = testing_binary_modelling_dataframe)
)

set.seed(27)
cv_non_zero_response_lasso_model <- cv.glmnet(non_zero_response_training_matrix,
                                              non_zero_response_training_vector,
                                              alpha = 1)
best_lambda_non_zero_response_lasso <- cv_non_zero_response_lasso_model$lambda.1se
coefficients_non_zero_response_lasso_model <- coef(cv_non_zero_response_lasso_model, s = best_lambda_non_zero_response_lasso)
prediction_non_zero_response_lasso_model <- predict(
  cv_non_zero_response_lasso_model,
  non_zero_response_testing_matrix,
  s = best_lambda_non_zero_response_lasso,
  type = "response"
)
testing_binary_modelling_dataframe$predicted_response_lasso <- as.vector(exp(prediction_non_zero_response_lasso_model))

set.seed(27)
cv_non_zero_response_ridge_model <- cv.glmnet(non_zero_response_training_matrix,
                                              non_zero_response_training_vector,
                                              alpha = 0)
best_lambda_non_zero_response_ridge <- cv_non_zero_response_ridge_model$lambda.1se
coefficients_non_zero_response_ridge_model <- coef(cv_non_zero_response_ridge_model, s = best_lambda_non_zero_response_ridge)
prediction_non_zero_response_ridge_model <- predict(
  cv_non_zero_response_ridge_model,
  non_zero_response_testing_matrix,
  s = best_lambda_non_zero_response_ridge,
  type = "response"
)
testing_binary_modelling_dataframe$predicted_response_ridge <- as.vector(exp(prediction_non_zero_response_ridge_model))

two_part_model_outcomes_dataframe <- testing_binary_modelling_dataframe %>%
  mutate(predicted_shot_decision_tree = as.numeric(predicted_shot_decision_tree) - 1) %>%
  mutate(AIC_AIC = predicted_shot_AIC * predicted_response_AIC) %>%
  mutate(AIC_BIC = predicted_shot_AIC * predicted_response_BIC) %>%
  mutate(AIC_lasso = predicted_shot_AIC * predicted_response_lasso) %>%
  mutate(AIC_ridge = predicted_shot_AIC * predicted_response_ridge) %>%
  mutate(decision_tree_AIC = predicted_shot_decision_tree * predicted_response_AIC) %>%
  mutate(decision_tree_BIC = predicted_shot_decision_tree * predicted_response_BIC) %>%
  mutate(decision_tree_lasso = predicted_shot_decision_tree * predicted_response_lasso) %>%
  mutate(decision_tree_ridge = predicted_shot_decision_tree * predicted_response_ridge)

two_part_model_outcomes_dataframe <- two_part_model_outcomes_dataframe %>%
  dplyr::select(
    match_id,
    interval,
    team,
    sum_of_non_penalty_expected_goals,
    AIC_AIC,
    AIC_BIC,
    AIC_lasso,
    AIC_ridge,
    decision_tree_AIC,
    decision_tree_BIC,
    decision_tree_lasso,
    decision_tree_ridge
  )

aic_aic_r_squared <- 1 - sum((
  two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - two_part_model_outcomes_dataframe$AIC_AIC
) ^ 2
) / sum((
  two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - mean(
    two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals
  )
) ^ 2)

aic_bic_r_squared <- 1 - sum((
  two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - two_part_model_outcomes_dataframe$AIC_BIC
) ^ 2
) / sum((
  two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - mean(
    two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals
  )
) ^ 2)

aic_ridge_r_squared <- 1 - sum((
  two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - two_part_model_outcomes_dataframe$AIC_lasso
) ^ 2
) / sum((
  two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - mean(
    two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals
  )
) ^ 2)

aic_lasso_r_squared <- 1 - sum((
  two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - two_part_model_outcomes_dataframe$AIC_ridge
) ^ 2
) / sum((
  two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - mean(
    two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals
  )
) ^ 2)

decision_tree_aic_r_squared <- 1 - sum((
  two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - two_part_model_outcomes_dataframe$decision_tree_AIC
) ^ 2
) / sum((
  two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - mean(
    two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals
  )
) ^ 2)

decision_tree_bic_r_squared <- 1 - sum((
  two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - two_part_model_outcomes_dataframe$decision_tree_BIC
) ^ 2
) / sum((
  two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - mean(
    two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals
  )
) ^ 2)

decision_tree_ridge_r_squared <- 1 - sum((
  two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - two_part_model_outcomes_dataframe$decision_tree_lasso
) ^ 2
) / sum((
  two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - mean(
    two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals
  )
) ^ 2)

decision_tree_lasso_r_squared <- 1 - sum((
  two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - two_part_model_outcomes_dataframe$decision_tree_ridge
) ^ 2
) / sum((
  two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - mean(
    two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals
  )
) ^ 2)

aic_aic_mse <- mean((
  two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - two_part_model_outcomes_dataframe$AIC_AIC
) ^ 2
)
aic_bic_mse <- mean((
  two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - two_part_model_outcomes_dataframe$AIC_BIC
) ^ 2
)
aic_ridge_mse <- mean((
  two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - two_part_model_outcomes_dataframe$AIC_ridge
) ^ 2
)
aic_lasso_mse <- mean((
  two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - two_part_model_outcomes_dataframe$AIC_lasso
) ^ 2
)

decision_tree_aic_mse <- mean((
  two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - two_part_model_outcomes_dataframe$decision_tree_AIC
) ^ 2
)
decision_tree_bic_mse <- mean((
  two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - two_part_model_outcomes_dataframe$decision_tree_BIC
) ^ 2
)
decision_tree_ridge_mse <- mean((
  two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - two_part_model_outcomes_dataframe$decision_tree_ridge
) ^ 2
)
decision_tree_lasso_mse <- mean((
  two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - two_part_model_outcomes_dataframe$decision_tree_lasso
) ^ 2
)

aic_aic_mae <- mean(
  abs(
    two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - two_part_model_outcomes_dataframe$AIC_AIC
  )
)
aic_bic_mae <- mean(
  abs(
    two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - two_part_model_outcomes_dataframe$AIC_BIC
  )
)
aic_ridge_mae <- mean(
  abs(
    two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - two_part_model_outcomes_dataframe$AIC_ridge
  )
)
aic_lasso_mae <- mean(
  abs(
    two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - two_part_model_outcomes_dataframe$AIC_lasso
  )
)

decision_tree_aic_mae <- mean(
  abs(
    two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - two_part_model_outcomes_dataframe$decision_tree_AIC
  )
)
decision_tree_bic_mae <- mean(
  abs(
    two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - two_part_model_outcomes_dataframe$decision_tree_BIC
  )
)
decision_tree_ridge_mae <- mean(
  abs(
    two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - two_part_model_outcomes_dataframe$decision_tree_ridge
  )
)
decision_tree_lasso_mae <- mean(
  abs(
    two_part_model_outcomes_dataframe$sum_of_non_penalty_expected_goals - two_part_model_outcomes_dataframe$decision_tree_lasso
  )
)

