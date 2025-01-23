histogram_of_shots <- ggplot(shot_distribution, aes(x = number_of_shots)) +
  geom_histogram(aes(y = after_stat(density)), bins = 6, fill = "#4A7C59", colour = "#FAF3DD") +
  scale_x_continuous(
    breaks = 0:5 # Specify tick marks for 0, 1, 2, 3, 4, and 5
  ) +
  ##stat_function(fun = function(x) dpois(round(x), lambda = number_of_shots_mean), geom = "point", n = 6, color = "red", size = 2) +
  labs(
    title = "Histogram of the Number of Shots",
    subtitle = "For each 5-minute interval, for each team",
    x = "Number of Shots",
    y = "Density"
  ) +
  theme_minimal()

ggsave("histogram_of_shots.png", plot = histogram_of_shots, dpi = 200)

histogram_of_shots_with_poisson <- ggplot(shot_distribution, aes(x = number_of_shots)) +
  geom_histogram(aes(y = after_stat(density)), bins = 6, fill = "#4A7C59", colour = "#FAF3DD") +
  scale_x_continuous(
    breaks = 0:5 # Specify tick marks for 0, 1, 2, 3, 4, and 5
  ) +
  stat_function(fun = function(x) dpois(round(x), lambda = number_of_shots_mean), geom = "point", n = 6, color = "#CC441C", size = 2) +
  labs(
    title = "Histogram of the Number of Shots, with the fitted Poisson probability mass function",
    subtitle = "For each 5-minute interval, for each team",
    x = "Number of Shots",
    y = "Density"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 11))

ggsave("histogram_of_shots_with_poisson.png", plot = histogram_of_shots_with_poisson, dpi = 200)
