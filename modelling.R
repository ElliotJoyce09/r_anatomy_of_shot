if (!require(pROC)) {
  install.packages("pROC")
  library(pROC)
}

if (!require(tidyverse)) {
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require(MASS)) {
  install.packages("MASS")
  library(MASS)
}

if (!require(mpath)) {
  install.packages("mpath")
  library(mpath)
}

# the training dataset is all but the final 3 games of the tournament
training_modelling_dataframe <- modelling_dataframe %>%
  ungroup() %>%
  filter(!(match_id %in% c("3942819", "3943043", "3942752")))

negative_binomial_training_modelling_dataframe <- training_modelling_dataframe %>%
  dplyr::select(
    c(
      "number_of_shots",
      "average_MDAO",
      "opposition_average_MDAO",
      "number_of_progressive_actions",
      "number_of_defender_removing_actions",
      "number_of_defenders_removed_from_actions",
      "number_of_high_defender_removing_actions"
    )
  ) %>%
  mutate(average_MDAO = sqrt(average_MDAO))

# the testing dataset, therefore, is the final 3 games of the tournament
testing_modelling_dataframe <- modelling_dataframe %>%
  ungroup() %>%
  filter(match_id %in% c("3942819", "3943043", "3942752"))

negative_binomial_testing_modelling_dataframe <- testing_modelling_dataframe %>%
  dplyr::select(
    c(
      "number_of_shots",
      "average_MDAO",
      "opposition_average_MDAO",
      "number_of_progressive_actions",
      "number_of_defender_removing_actions",
      "number_of_defenders_removed_from_actions",
      "number_of_high_defender_removing_actions"
    )
  ) %>%
  mutate(average_MDAO = sqrt(average_MDAO))

## the number of shots model
set.seed(27)
number_of_shots_model_ridge_cv <- cv.glmregNB(
  formula = number_of_shots ~ .,
  data = negative_binomial_training_modelling_dataframe,
  alpha = 0,
  nfolds = 10,
  lambda = seq(0.001, 0.03, by = 0.001),
  plot.it = TRUE
)

number_of_shots_ridge_lambda_dataframe <- as.data.frame(cbind(
  number_of_shots_model_ridge_cv$lambda,
  number_of_shots_model_ridge_cv$cv
))
colnames(number_of_shots_ridge_lambda_dataframe) <- c("lambda", "log_likelihood")
optimal_ridge_lambda <- number_of_shots_model_ridge_cv$lambda.optim

number_of_shots_model_ridge_model <- glmregNB(
  formula = number_of_shots ~ .,
  data = negative_binomial_training_modelling_dataframe,
  alpha = 0,
  lambda = optimal_ridge_lambda
)

set.seed(27)
number_of_shots_model_lasso_cv <- cv.glmregNB(
  formula = number_of_shots ~ .,
  data = negative_binomial_training_modelling_dataframe,
  alpha = 1,
  nfolds = 10,
  lambda = seq(0.001, 0.03, by = 0.001),
  plot.it = TRUE
)

number_of_shots_lasso_lambda_dataframe <- as.data.frame(cbind(
  number_of_shots_model_lasso_cv$lambda,
  number_of_shots_model_lasso_cv$cv
))
colnames(number_of_shots_lasso_lambda_dataframe) <- c("lambda", "log_likelihood")
optimal_lasso_lambda <- number_of_shots_model_lasso_cv$lambda.optim

number_of_shots_model_lasso_model <- glmregNB(
  formula = number_of_shots ~ .,
  data = negative_binomial_training_modelling_dataframe,
  alpha = 1,
  lambda = optimal_lasso_lambda
)

full_number_of_shots_model <- glm.nb(
  number_of_shots ~
    average_MDAO +
    opposition_average_MDAO +
    number_of_progressive_actions +
    number_of_defender_removing_actions +
    number_of_defenders_removed_from_actions +
    number_of_high_defender_removing_actions,
  data = negative_binomial_training_modelling_dataframe
)

number_of_shots_model_aic <- stepAIC(full_number_of_shots_model, direction = "both")

number_of_shots_model_bic <- stepAIC(full_number_of_shots_model,
                                     direction = "both",
                                     k = log(nrow(
                                       negative_binomial_training_modelling_dataframe
                                     )))

negative_binomial_testing_modelling_dataframe$ridge_response <- as.vector(
  predict(
    number_of_shots_model_ridge_model,
    newx = negative_binomial_testing_modelling_dataframe,
    type = "response"
  )
)

negative_binomial_testing_modelling_dataframe$lasso_response <- as.vector(
  predict(
    number_of_shots_model_lasso_model,
    newx = negative_binomial_testing_modelling_dataframe,
    type = "response"
  )
)

negative_binomial_testing_modelling_dataframe$aic_response <- as.vector(
  predict(
    number_of_shots_model_aic,
    newdata = negative_binomial_testing_modelling_dataframe,
    type = "response"
  )
)

negative_binomial_testing_modelling_dataframe$bic_response <- as.vector(
  predict(
    number_of_shots_model_bic,
    newdata = negative_binomial_testing_modelling_dataframe,
    type = "response"
  )
)

ridge_r_squared <- 1 - sum((
  negative_binomial_testing_modelling_dataframe$number_of_shots - negative_binomial_testing_modelling_dataframe$ridge_response
) ^ 2
) / sum((
  negative_binomial_testing_modelling_dataframe$number_of_shots - mean(
    negative_binomial_testing_modelling_dataframe$number_of_shots
  )
) ^ 2)

lasso_r_squared <- 1 - sum((
  negative_binomial_testing_modelling_dataframe$number_of_shots - negative_binomial_testing_modelling_dataframe$lasso_response
) ^ 2
) / sum((
  negative_binomial_testing_modelling_dataframe$number_of_shots - mean(
    negative_binomial_testing_modelling_dataframe$number_of_shots
  )
) ^ 2)

aic_r_squared <- 1 - sum((
  negative_binomial_testing_modelling_dataframe$number_of_shots - negative_binomial_testing_modelling_dataframe$aic_response
) ^ 2
) / sum((
  negative_binomial_testing_modelling_dataframe$number_of_shots - mean(
    negative_binomial_testing_modelling_dataframe$number_of_shots
  )
) ^ 2)

bic_r_squared <- 1 - sum((
  negative_binomial_testing_modelling_dataframe$number_of_shots - negative_binomial_testing_modelling_dataframe$bic_response
) ^ 2
) / sum((
  negative_binomial_testing_modelling_dataframe$number_of_shots - mean(
    negative_binomial_testing_modelling_dataframe$number_of_shots
  )
) ^ 2)

ridge_mse <- mean((
  negative_binomial_testing_modelling_dataframe$number_of_shots - negative_binomial_testing_modelling_dataframe$ridge_response
) ^ 2
)
lasso_mse <- mean((
  negative_binomial_testing_modelling_dataframe$number_of_shots - negative_binomial_testing_modelling_dataframe$lasso_response
) ^ 2
)
aic_mse <- mean((
  negative_binomial_testing_modelling_dataframe$number_of_shots - negative_binomial_testing_modelling_dataframe$aic_response
) ^ 2
)
bic_mse <- mean((
  negative_binomial_testing_modelling_dataframe$number_of_shots - negative_binomial_testing_modelling_dataframe$bic_response
) ^ 2
)

ridge_mae <- mean(
  abs(
    negative_binomial_testing_modelling_dataframe$number_of_shots - negative_binomial_testing_modelling_dataframe$ridge_response
  )
)
lasso_mae <- mean(
  abs(
    negative_binomial_testing_modelling_dataframe$number_of_shots - negative_binomial_testing_modelling_dataframe$lasso_response
  )
)
aic_mae <- mean(
  abs(
    negative_binomial_testing_modelling_dataframe$number_of_shots - negative_binomial_testing_modelling_dataframe$aic_response
  )
)
bic_mae <- mean(
  abs(
    negative_binomial_testing_modelling_dataframe$number_of_shots - negative_binomial_testing_modelling_dataframe$bic_response
  )
)

france_versus_spain_testing_dataframe <- negative_binomial_testing_modelling_dataframe %>%
  filter(match_id == "3942752") %>%
  filter(team == "france"
  )