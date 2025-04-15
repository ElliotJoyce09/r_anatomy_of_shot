if(!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require(ggcorrplot)) {
  install.packages("ggcorrplot")
  library(ggcorrplot)
}

if (!require(gridExtra)) {
  install.packages("gridExtra")
  library(gridExtra)
}

## data_cleaning.R

event_type_counts <- ggplot(euro2024_events, aes(x = fct_infreq(type.name))) +
  geom_bar(stat = "count",
           fill = "#4A7C59",
           colour = "#FAF3DD") +
  labs(title = "Frequency of Event Type for Euro 2024", x = "Event Type", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 90,
    vjust = 0.5,
    hjust = 1
  ))

ggsave(
  "data_cleaning/event_type_counts.png",
  plot = event_type_counts,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

frequent_pass_events <- ggplot(frequent_pass_attempts_dataframe, aes(x = n, y = reorder(type.name, n))) +
  geom_col(fill = "#4A7C59", colour = "#FAF3DD") +
  labs(title = "Top 20 Most Frequent Pass Event Types", x = "Frequency", y = "Event Type") +
  theme_minimal()

ggsave(
  "data_cleaning/frequent_pass_events.png",
  plot = frequent_pass_events,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

visible_area_plot <- ggplot(polygon_dataframe, aes(x = x, y = y)) +
  geom_polygon(fill = "#4A7C59", colour = "#FAF3DD") +
  coord_fixed(xlim = c(0, 120),
              ylim = c(0, 80),
              expand = FALSE) +
  labs(title = "Visible Area of the Assist to Florian Wirtz's Goal", x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = alpha("darkgreen", 0.4), colour = "white"))

ggsave(
  "data_cleaning/visible_area_plot.png",
  plot = visible_area_plot,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

in_possession_mdao <- ggplot(
  filter(
    measure_of_defensive_area_occupied_dataframe,
    team == possession_team.name
  ),
  aes(x = row_sum)
) +
  geom_histogram(
    aes(y = after_stat(count) / sum(after_stat(count))),
    fill = "#4A7C59",
    colour = "#FAF3DD",
    bins = 50
  ) +
  labs(title = "Histogram of MDAO for In Possession Teams", x = "Measure of Defensive Area Occupied", y = "Proportion of Value") +
  ylim(0, 0.4) +
  theme_minimal()

ggsave(
  "definitions/in_possession_mdao.png",
  plot = in_possession_mdao,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

oop_mdao <- ggplot(
  filter(
    measure_of_defensive_area_occupied_dataframe,
    team != possession_team.name
  ),
  aes(x = row_sum)
) +
  geom_histogram(
    aes(y = after_stat(count) / sum(after_stat(count))),
    fill = "#4A7C59",
    colour = "#FAF3DD",
    bins = 50
  ) +
  labs(title = "Histogram of MDAO for Out of Possession Teams", x = "Measure of Defensive Area Occupied", y = "Proportion of Value") +
  ylim(0, 0.4) +
  theme_minimal()

ggsave(
  "definitions/oop_mdao.png",
  plot = oop_mdao,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)


difference_mdao_plot <- ggplot(germany_scotland_mdao, aes(x = timestamp)) +
  geom_point(aes(y = difference), colour = "black", alpha = 0.1) +
  geom_line(aes(y = smooth), colour = "#FAF3DD", size = 1) +
  geom_ribbon(aes(ymin = pmin(smooth, 0), ymax = pmax(smooth, 0)),
              fill = "#4A7C59",
              alpha = 0.7) +
  labs(
    title = "Difference of Germany's MDAO to Scotland's MDAO",
    subtitle = "Including a LOESS Regression Line",
    x = "Cummulative Time of Match (in Minutes)",
    y = "Difference of MDAO"
  ) +
  scale_x_continuous(
    breaks = seq(0, max(germany_scotland_mdao$timestamp), by = 15 * 60),
    labels = function(x)
      x / 60
  ) +
  theme_minimal()

ggsave(
  "definitions/difference_mdao_plot.png",
  plot = difference_mdao_plot,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

## eda.R

multicollinearity_plot <- ggcorrplot(
  correlation_explanatory_variables_matrix,
  lab = TRUE,
  lab_size = 2.5,
  type = "lower",
  colors = c("lightblue", "white", "orangered"),
  outline.color = "black",
  title = "Correlation Between the Explanatory Variables",
  tl.cex = 8,
  ggtheme = theme_minimal()
)

ggsave(
  "eda/multicolinearity_plot.png",
  multicollinearity_plot,
  width = 1200,
  height = 1200,
  units = "px",
  dpi = 150
)

average_mdao_explanatory_plot <- ggplot(modelling_dataframe, aes(y = log(number_of_shots + 1), x = average_MDAO)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess") +
  labs(title = "Relationship Between Log-Transformed Shots and Average MDAO", x = "Team's Average MDAO", y = "Log-Transformed Number of Shots") +
  theme_minimal()

ggsave(
  "eda/average_mdao_explanatory_plot.png",
  plot = average_mdao_explanatory_plot,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

transformed_average_mdao_explanatory_plot <- ggplot(modelling_dataframe, aes(y = log(number_of_shots + 1), x = sqrt(average_MDAO))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess") +
  labs(title = "Relationship Between Log-Transformed Shots and Transformed Average MDAO", x = "Square Root Tranformed Team's Average MDAO", y = "Log-Transformed Number of Shots") +
  theme_minimal()

ggsave(
  "eda/transformed_average_mdao_explanatory_plot.png",
  plot = transformed_average_mdao_explanatory_plot,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

opposition_average_mdao_explanatory_plot <- ggplot(modelling_dataframe, aes(y = log(number_of_shots + 1), x = opposition_average_MDAO)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess") +
  labs(title = "Relationship Between Log-Transformed Shots and Opposition Average MDAO", x = "Opposition Team's Average MDAO", y = "Log-Transformed Number of Shots") +
  theme_minimal() +
  theme(title = element_text(size = 9))

ggsave(
  "eda/opposition_average_mdao_explanatory_plot.png",
  plot = opposition_average_mdao_explanatory_plot,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

removed_outlier_opposition_average_mdao_explanatory_plot <- ggplot(modelling_dataframe, aes(y = log(number_of_shots + 1), x = opposition_average_MDAO))+
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess") +
  xlim(0, 6) +
  labs(title = "Relationship Between Log-Transformed Shots and Opposition Average MDAO", x = "Opposition Team's Average MDAO", y = "Log-Transformed Number of Shots") +
  theme_minimal() +
  theme(title = element_text(size = 9))

ggsave(
  "eda/removed_outlier_opposition_average_mdao_explanatory_plot.png",
  plot = removed_outlier_opposition_average_mdao_explanatory_plot,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

opposition_turnover_closest_to_their_own_goal_explanatory_plot <- ggplot(
  modelling_dataframe_opposition_turnover_filtered,
  aes(y = log(number_of_shots + 1), x = opposition_turnover_closest_to_their_own_goal)
)+
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess") +
  labs(title = "Relationship Between Log-Transformed Shots and Opposition's Closest Turnover", x = "Opposition's Closest Turnover to Own Goal", y = "Log-Transformed Number of Shots") +
  theme_minimal() +
  theme(title = element_text(size = 9))

ggsave(
  "eda/opposition_turnover_closest_to_their_own_goal_explanatory_plot.png",
  plot = opposition_turnover_closest_to_their_own_goal_explanatory_plot,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

successful_pressure_closest_to_opposition_goal_explanatory_plot <- ggplot(
  modelling_dataframe_successful_pressure_filtered,
  aes(y = log(number_of_shots + 1), x = successful_pressure_closest_to_opposition_goal)
) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess") +
  labs(title = "Relationship Between Log-Transformed Shots and Closest Successful Pressure", x = "Closest Successful Pressure to Opposition's Goal", y = "Log-Transformed Number of Shots") +
  theme_minimal() +
  theme(title = element_text(size = 9))

ggsave(
  "eda/successful_pressure_closest_to_opposition_goal_explanatory_plot.png",
  plot = successful_pressure_closest_to_opposition_goal_explanatory_plot,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

progressive_actions_heatmap <- ggplot(
  complete_progressive_actions_running_counts,
  aes(x = number_of_progressive_actions, y = number_of_shots)
) +
  geom_tile(aes(fill = log(frequency)), colour = "black") +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  scale_y_continuous(breaks = 0:5, limits = c(-0.5, 5.5)) +
  scale_x_continuous(breaks = 0:14, limits = c(-0.5, 14.5)) +
  labs(title = "Relationship Between Shots and Progressive Actions",
       x = "Team's Number of Progressive Actions",
       y = "Number of Shots",
       fill = "Log of Frequency") +
  theme_minimal()

ggsave(
  "eda/progressive_actions_heatmap.png",
  plot = progressive_actions_heatmap,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

defender_removing_actions_heatmap <- ggplot(
  defender_removing_actions_running_counts,
  aes(x = number_of_defender_removing_actions, y = number_of_shots)
) +
  geom_tile(aes(fill = log(frequency)), colour = "black") +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  labs(title = "Relationship Between Shots and Defenders Removing Actions",
       x = "Team's Number of Defender Removing Actions",
       y = "Number of Shots",
       fill = "Log of Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8))

ggsave(
  "eda/defender_removing_actions_heatmap.png",
  plot = defender_removing_actions_heatmap,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

histogram_of_shots <- ggplot(modelling_dataframe, aes(x = number_of_shots)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 6,
    fill = "#4A7C59",
    colour = "#FAF3DD"
  ) +
  scale_x_continuous(breaks = 0:5) +
  labs(title = "Histogram of Shots", x = "Number of Shots", y = "Density") +
  theme_minimal()

ggsave(
  "eda/histogram_of_shots.png",
  plot = histogram_of_shots,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

histogram_of_shots_with_poisson <- ggplot(modelling_dataframe, aes(x = number_of_shots)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 6,
    fill = "#4A7C59",
    colour = "#FAF3DD"
  ) +
  scale_x_continuous(breaks = 0:5) +
  stat_function(
    fun = function(x)
      dpois(round(x), lambda = shots_lambda_hat),
    geom = "point",
    n = 6,
    colour = "#CC441C",
    size = 2
  ) +
  labs(title = "Histogram of Shots, with the fitted Poisson PMF", x = "Number of Shots", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(size = 11))

ggsave(
  "eda/histogram_of_shots_with_poisson.png",
  plot = histogram_of_shots_with_poisson,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

histogram_of_shots_with_nb <- ggplot(modelling_dataframe, aes(x = number_of_shots)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 6,
    fill = "#4A7C59",
    colour = "#FAF3DD"
  ) +
  scale_x_continuous(breaks = 0:5) +
  stat_function(
    fun = function(x)
      dnbinom(round(x), size = r_hat, prob = p_hat),
    geom = "point",
    n = 6,
    colour = "#CC441C",
    size = 2
  ) +
  labs(title = "Histogram of Shots, with the fitted Negative Binomial PMF", x = "Number of Shots", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(size = 11))

ggsave(
  "eda/histogram_of_shots_with_nb.png",
  plot = histogram_of_shots_with_nb,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
  
)

## modelling.R

ridge_lambda_plot <- ggplot(number_of_shots_ridge_lambda_dataframe,
                            aes(x = log(lambda), y = log_likelihood)) +
  geom_point() +
  geom_line() +
  geom_vline(
    xintercept = log(optimal_ridge_lambda),
    colour = "red",
    linetype = "dashed"
  ) +
  labs(
    title = "Log-Likelihood for each Lambda Value for Ridge Regresion",
    subtitle = "Optimal Lambda given by the Dashed Line",
    x = "Log of Lambda",
    y = "Log-Likelihood"
  ) +
  theme_minimal()

ggsave(
  "modelling/ridge_lambda_plot.png",
  plot = ridge_lambda_plot,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

lasso_lambda_plot <- ggplot(number_of_shots_lasso_lambda_dataframe,
                            aes(x = log(lambda), y = log_likelihood)) +
  geom_point() +
  geom_line() +
  geom_vline(
    xintercept = log(optimal_lasso_lambda),
    colour = "red",
    linetype = "dashed"
  ) +
  labs(
    title = "Log-Likelihood for each Lambda Value for LASSO Regression",
    subtitle = "Optimal Lambda given by the Dashed Line",
    x = "Log of Lambda",
    y = "Log-Likelihood"
  ) +
  theme_minimal()

ggsave(
  "modelling/lasso_lambda_plot.png",
  plot = lasso_lambda_plot,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

ridge_response_violin_plot <- ggplot(
  negative_binomial_testing_modelling_dataframe,
  aes(x = as.factor(number_of_shots), y = ridge_response)
) +
  geom_violin(colour = "#4A7C59", fill = "lightgrey") +
  labs(title = "Predicted against Actual Number of Shots for Ridge Regression Model", x = "Actual Number of Shots", y = "Predicted Number of Shots") +
  ylim(0, 3) +
  theme_minimal()

ggsave(
  "modelling/ridge_response_violin_plot.png",
  plot = ridge_response_violin_plot,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

lasso_response_violin_plot <- ggplot(
  negative_binomial_testing_modelling_dataframe,
  aes(x = as.factor(number_of_shots), y = lasso_response)
) +
  geom_violin(colour = "#4A7C59", fill = "lightgrey") +
  labs(title = "Predicted against Actual Number of Shots for LASSO Regression Model", x = "Actual Number of Shots", y = "Predicted Number of Shots") +
  ylim(0, 3) +
  theme_minimal()

ggsave(
  "modelling/lasso_response_violin_plot.png",
  plot = lasso_response_violin_plot,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

aic_response_violin_plot <- ggplot(negative_binomial_testing_modelling_dataframe,
                                   aes(x = as.factor(number_of_shots), y = aic_response)) +
  geom_violin(colour = "#4A7C59", fill = "lightgrey") +
  labs(title = "Predicted against Actual Number of Shots for AIC Stepwise Model", x = "Actual Number of Shots", y = "Predicted Number of Shots") +
  ylim(0, 3) +
  theme_minimal()

ggsave(
  "modelling/aic_response_violin_plot.png",
  plot = aic_response_violin_plot,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

bic_response_violin_plot <- ggplot(negative_binomial_testing_modelling_dataframe,
                                   aes(x = as.factor(number_of_shots), y = bic_response)) +
  geom_violin(colour = "#4A7C59", fill = "lightgrey") +
  labs(title = "Predicted against Actual Number of Shots for BIC Stepwise Model", x = "Actual Number of Shots", y = "Predicted Number of Shots") +
  ylim(0, 3) +
  theme_minimal()

ggsave(
  "modelling/bic_response_violin_plot.png",
  plot = bic_response_violin_plot,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

france_versus_spain_bar_plot <- ggplot(france_versus_spain_testing_dataframe,
                                       aes(x = interval, y = number_of_shots)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  ylim(0, 2) +
  xlim(0.5, 20.5) +
  labs(
    title = "Actual Number of Shots per Interval",
    subtitle = "For France versus Spain",
    x = "Interval",
    y = "Actual Number of Shots"
  ) +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 8))

france_versus_spain_ridge_plot <- ggplot(france_versus_spain_testing_dataframe,
                                         aes(x = interval, y = ridge_response)) +
  geom_point(colour = "darkgreen") +
  ylim(0, 2) +
  xlim(0.5, 20.5) +
  geom_text(
    aes(label = signif(ridge_response, 2), y = ridge_response + 0.1),
    size = 3,
    hjust = 0.5,
    vjust = 0
  ) +
  labs(title = "Ridge Model Predicted Shots for France against Spain", x = "Interval", y = "Predicted Number of Shots from the Ridge Model") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 8))

france_versus_spain_lasso_plot <- ggplot(france_versus_spain_testing_dataframe,
                                         aes(x = interval, y = lasso_response)) +
  geom_point(colour = "darkgreen") +
  ylim(0, 2) +
  xlim(0.5, 20.5) +
  geom_text(
    aes(label = signif(lasso_response, 2), y = lasso_response + 0.1),
    size = 3,
    hjust = 0.5,
    vjust = 0
  ) +
  labs(title = "Ridge Model Predicted Shots for France against Spain", x = "Interval", y = "Predicted Number of Shots from the LASSO Model") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 8))

france_versus_spain_aic_plot <- ggplot(france_versus_spain_testing_dataframe,
                                       aes(x = interval, y = aic_response)) +
  geom_point(colour = "darkgreen") +
  ylim(0, 2) +
  xlim(0.5, 20.5) +
  geom_text(
    aes(label = signif(aic_response, 2), y = aic_response + 0.1),
    size = 3,
    hjust = 0.5,
    vjust = 0
  ) +
  labs(title = "Ridge Model Predicted Shots for France against Spain", x = "Interval", y = "Predicted Number of Shots from the AIC Model") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 8))

france_versus_spain_bic_plot <- ggplot(france_versus_spain_testing_dataframe,
                                       aes(x = interval, y = bic_response)) +
  geom_point(colour = "darkgreen") +
  ylim(0, 2) +
  xlim(0.5, 20.5) +
  geom_text(
    aes(label = signif(bic_response, 2), y = bic_response + 0.1),
    size = 3,
    hjust = 0.5,
    vjust = 0
  ) +
  labs(title = "Ridge Model Predicted Shots for France against Spain", x = "Interval", y = "Predicted Number of Shots from the BIC Model") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 8))

france_versus_spain_plot <- grid.arrange(
  france_versus_spain_bar_plot,
  france_versus_spain_ridge_plot,
  france_versus_spain_lasso_plot,
  france_versus_spain_aic_plot,
  france_versus_spain_bic_plot,
  ncol = 1
)

ggsave(
  "modelling/france_versus_spain_plot.png",
  plot = france_versus_spain_plot,
  dpi = 200,
  width = 1275,
  height = 859 * 4,
  units = "px"
)

## eda_2

density_of_xg <- ggplot(expected_goals_dataframe_intervals,
                        aes(x = sum_of_expected_goals)) +
  geom_density(fill = "#4A7C59") +
  labs(title = "Density Plot of Sum of Expected Goals", x = "Sum of Expected Goals", y = "Density") +
  theme_minimal()

ggsave(
  "two_part_models/density_of_xg.png",
  plot = density_of_xg,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

density_of_npxg <- ggplot(expected_goals_dataframe_intervals,
                          aes(x = sum_of_non_penalty_expected_goals)) +
  geom_density(fill = "#4A7C59") +
  labs(title = "Density Plot of Sum of Non-Penalty Expected Goals", x = "Sum of Non-Penalty Expected Goals", y = "Density") +
  theme_minimal()

ggsave(
  "two_part_models/density_of_npxg.png",
  plot = density_of_npxg,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

density_of_xg_no_zero <- ggplot(non_zero_response_modelling_dataframe,
                                aes(x = sum_of_expected_goals)) +
  geom_density(fill = "#4A7C59") +
  labs(title = "Density Plot of Sum of Expected Goals, Excluding 0", x = "Sum of Expected Goals", y = "Density") +
  theme_minimal()

ggsave(
  "eda_2/density_of_xg_no_zero.png",
  plot = density_of_xg_no_zero,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

density_of_npxg_no_zero <- ggplot(non_zero_response_modelling_dataframe,
                                  aes(x = sum_of_non_penalty_expected_goals)) +
  geom_density(fill = "#4A7C59") +
  labs(title = "Density Plot of Sum of Non-Penalty Expected Goals, Excluding 0", x = "Sum of Non-Penalty Expected Goals", y = "Density") +
  theme_minimal()

ggsave(
  "eda_2/density_of_npxg_no_zero.png",
  plot = density_of_npxg_no_zero,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

density_of_log_npxg <- ggplot(training_non_zero_response_modelling_dataframe, aes(x = log(sum_of_non_penalty_expected_goals))) +
  geom_density(fill = "#4A7C59") +
  labs(title = "Density Plot of Log-transformed Sum of Non-Penalty Expected Goals, Excluding 0", x = "Log-transformed Sum of Non-Penalty Expected Goals", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(size = 11))

ggsave(
  "eda_2/density_of_log_npxg.png",
  plot = density_of_log_npxg,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

qq_plot <- ggplot(
  log_sum_of_non_penalty_expected_goals_data,
  aes(sample = log_sum_of_non_penalty_expected_goals_data[, 1])
) +
  geom_qq_line(colour = "red", linetype = "dashed") +
  geom_qq(alpha = 0.2) +
  labs(title = "QQ Plot of Log-transformed Sum of Non-Penalty Expected Goals, Excluding 0", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal() +
  theme(plot.title = element_text(size = 11))

ggsave(
  "eda_2/qq_plot.png",
  plot = qq_plot,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

density_of_log_npxg_with_normal <- ggplot(training_non_zero_response_modelling_dataframe, aes(x = log(sum_of_non_penalty_expected_goals))) +
  geom_density(fill = "#4A7C59") +
  stat_function(
    fun = dnorm,
    args = list(mean = mu, sd = sigma),
    colour = "#CC441C",
    linewidth = 1
  ) +
  labs(
    title = "Density of Log-transformed Sum of Non-Penalty Expected Goals, Excluding 0",
    subtitle = "With the fitted normal PMF",
    x = "Log-transformed Sum of Non-Penalty Expected Goals",
    y = "Density"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 11))

ggsave(
  "eda_2/density_of_log_npxg_with_normal.png",
  plot = density_of_log_npxg_with_normal,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

mdao_log_odds <- ggplot(mdao_binned, aes(x = mean_predictor, y = logit)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess") +
  labs(title = "Relationship Between Log-odds of Shot Occuring and Average MDAO", x = "Team's Average MDAO", y = "Observed Log-Odds of Shot Occuring") +
  theme_minimal()

ggsave(
  "eda_2/mdao_log_odds.png",
  plot = mdao_log_odds,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

opposition_mdao_log_odds <- ggplot(opposition_mdao_binned, aes(x = mean_predictor, y = logit)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess") +
  labs(title = "Relationship Between Log-odds of Shot Occuring and Opposition Average MDAO", x = "Opposition Team's Average MDAO", y = "Observed Log-Odds of Shot Occuring") +
  theme_minimal() +
  theme(plot.title = element_text(size = 11))

ggsave(
  "eda_2/opposition_mdao_log_odds.png",
  plot = opposition_mdao_log_odds,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

transformed_opposition_mdao_log_odds <- ggplot(opposition_mdao_binned, aes(x = sqrt(mean_predictor), y = logit)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess") +
  labs(title = "Relationship Between Log-odds of Shot Occuring and Transformed Opposition Average MDAO", x = "Square Root Tranformed Opposition Team's Average MDAO", y = "Observed Log-Odds of Shot Occuring") +
  theme_minimal() +
  theme(plot.title = element_text(size = 11))

ggsave(
  "eda_2/transformed_opposition_mdao_log_odds.png",
  plot = transformed_opposition_mdao_log_odds,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

progressive_actions_log_odds <- ggplot(progressive_action_binned, aes(x = mean_predictor, y = logit)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess") +
  labs(title = "Relationship Between Log-odds of Shot Occuring and Progressive Actions", x = "Team's Number of Progressive Actions", y = "Observed Log-Odds of Shot Occuring") +
  theme_minimal() +
  theme(plot.title = element_text(size = 11))

ggsave(
  "eda_2/progressive_actions_log_odds.png",
  plot = progressive_actions_log_odds,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

npxg_mdao_plot <- ggplot(training_non_zero_response_modelling_dataframe,
                         aes(
                           x = average_MDAO,
                           y = log(sum_of_non_penalty_expected_goals)
                         )) +
  geom_point() +
  geom_smooth() +
  labs(title = "Relationship Between Log-Transformed npxG and Average MDAO", x = "Team's Average MDAO", y = "Log-Transformed Non-Penalty Expected Goals") +
  theme_minimal()

ggsave(
  "eda_2/npxg_mdao_plot.png",
  plot = npxg_mdao_plot,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

npxg_mdao_no_outlier_plot <- ggplot(training_non_zero_response_modelling_dataframe,
                                    aes(
                                      x = average_MDAO,
                                      y = log(sum_of_non_penalty_expected_goals)
                                    )) +
  geom_point() +
  geom_smooth() +
  xlim(0, 5) +
  labs(title = "Relationship Between Log-Transformed npxG and Average MDAO", x = "Team's Average MDAO", y = "Log-Transformed Non-Penalty Expected Goals") +
  theme_minimal()

ggsave(
  "eda_2/npxg_mdao_no_outlier_plot.png",
  plot = npxg_mdao_no_outlier_plot,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

npxg_closest_opposition_turnover_plot <- ggplot(
  training_non_zero_response_modelling_dataframe,
  aes(
    x = opposition_turnover_closest_to_their_own_goal,
    y = log(sum_of_non_penalty_expected_goals)
  )
) +
  geom_point() +
  geom_smooth() +
  xlim(0, 126) +
  labs(title = "Relationship Between Log-Transformed npxG and Opposition Turnover Closest to Their Goal", x = "Distance to Their Goal of Closest Opposition Turnover", y = "Log-Transformed Non-Penalty Expected Goals") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10))

ggsave(
  "eda_2/npxg_closest_opposition_turnover_plot.png",
  plot = npxg_closest_opposition_turnover_plot,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

npxg_progressive_actions_boxplot <- ggplot(training_non_zero_response_modelling_dataframe,
                                           aes(
                                             x = factor(number_of_progressive_actions),
                                             y = log(sum_of_non_penalty_expected_goals)
                                           )) +
  geom_boxplot() +
  scale_x_discrete(limits = as.character(0:14)) +
  labs(title = "Relationship Between Log-Transformed npxG and Progresive Actions", x = "Team's Number of Progressive Actions", y = "Log-Transformed Non-Penalty Expected Goals") +
  theme_minimal() +
  theme(title = element_text(size = 10))

ggsave(
  "eda_2/npxg_progressive_actions_boxplot.png",
  plot = npxg_progressive_actions_boxplot,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

npxg_high_defender_removing_boxplot <- ggplot(training_non_zero_response_modelling_dataframe,
                                              aes(
                                                x = factor(number_of_high_defender_removing_actions),
                                                y = log(sum_of_non_penalty_expected_goals)
                                              )) +
  geom_boxplot() +
  scale_x_discrete(limits = as.character(0:12)) +
  labs(title = "Relationship Between Log-Transformed npxG and High Defender Removing Actions", x = "Team's Number of High Defender Removing Actions", y = "Log-Transformed Non-Penalty Expected Goals") +
  theme_minimal() +
  theme(title = element_text(size = 9))

ggsave(
  "eda_2/npxg_high_defender_removing_boxplot.png",
  plot = npxg_high_defender_removing_boxplot,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

## modelling_2

AIC_roc_curve <- ggplot(AIC_roc_data, aes(x = FPR, y = TPR)) +
  geom_line(colour = "black") +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "red"
  ) +
  labs(
    title = "ROC Curve for the AIC/BIC Stepwise Logistic Model",
    subtitle = paste0("Area under the curve: ", signif(auc(roc_AIC), 3)),
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_minimal() +
  theme(title = element_text(size = 10))

ggsave(
  "modelling_2/AIC_roc_curve.png",
  plot = AIC_roc_curve,
  dpi = 200,
  width = 859,
  height = 859,
  units = "px"
)

decision_tree_roc_curve <- ggplot(decision_tree_roc_data, aes(x = FPR, y = TPR)) +
  geom_line(colour = "black") +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "red"
  ) +
  labs(
    title = "ROC Curve for the Decision Tree Model",
    subtitle = paste0("Area under the curve: ", signif(auc(roc_decision_tree), 3)),
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_minimal() +
  theme(title = element_text(size = 10))

ggsave(
  "modelling_2/decision_tree_roc_curve.png",
  plot = decision_tree_roc_curve,
  dpi = 200,
  width = 859,
  height = 859,
  units = "px"
)

aic_bic_outcomes_plot <- ggplot(
  two_part_model_outcomes_dataframe,
  aes(y = AIC_BIC, x = sum_of_non_penalty_expected_goals)
) +
  geom_point() +
  geom_abline(
    intercept = 0,
    slope = 1,
    colour = "red",
    linetype = "dashed"
  ) +
  labs(title = "Predicted against Actual Sum of Non-Penalty Expected Goals for the AIC/BIC Model", x = "Sum of Non-Penalty Expected Goals", y = "Predicted Sum of Non-Penalty Expected Goals") +
  theme_minimal() +
  theme(plot.title = element_text(size = 11))

ggsave(
  "modelling_2/aic_bic_outcomes_plot.png",
  plot = aic_bic_outcomes_plot,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
)

aic_bic_residual_plot <- ggplot(
  two_part_model_outcomes_dataframe,
  aes(x = AIC_BIC, y = sum_of_non_penalty_expected_goals - AIC_BIC)
) +
  geom_point() +
  geom_abline(
    intercept = 0,
    slope = 0,
    colour = "red",
    linetype = "dashed"
  ) +
  labs(title = "Residual Plot of Sum of Non-Penalty Expected Goals for the AIC/BIC Model", x = "Predicted Sum of Non-Penalty Expected Goals", y = "Standardised Residual") +
  theme_minimal() +
  theme(plot.title = element_text(size = 11))

ggsave(
  "modelling_2/aic_bic_residual_plot.png",
  plot = aic_bic_residual_plot,
  dpi = 200,
  width = 1275,
  height = 859,
  units = "px"
  
)




