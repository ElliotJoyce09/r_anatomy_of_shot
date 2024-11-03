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

test <- merged_dataframe %>%
  filter(match_id == "3930158")

test_filtered <- test %>%
  group_by(timestamp, period) %>%
  filter(id == first(id)) %>%  # Keeps all rows where id matches the first id in each group
  ungroup() 

test_filtered <- test_filtered %>%
  mutate(
    x = map_dbl(location, 1),   # Extract the first element in each list (x coordinate)
    y = map_dbl(location, 2)     # Extract the second element in each list (y coordinate)
  ) %>%
  # Apply conditional adjustments to x and y
  mutate(
    x = pmax(0, pmin(x, 120)),   # Ensure x is between 0 and 80
    y = pmax(0, pmin(y, 80))    # Ensure y is between 0 and 120
  ) %>%
  select(-location)

test_filtered <- test_filtered %>%
  mutate(
    x = if_else(team.name == possession_team.name, 120 - x, x),
    y = if_else(team.name == possession_team.name, 80 - y, y)
  )

zones <- data.frame(
  zone_id = 1:13,
  x_min = c(0, 18.8, 39.4, 0, 0, 18.8, 39.4, 18.8, 18.8, 18.8, 60, 78.8, 99.4),
  x_max = c(18.8, 39.4, 60, 18.8, 18.8, 39.4, 60, 60, 60, 60, 78.8, 99.4, 120),
  y_min = c(0, 0, 0, 16.3, 63.6, 63.6, 63.6, 50.8, 16.3, 29.2, 0, 0, 0),
  y_max = c(16.3, 16.3, 16.3, 63.6, 80, 80, 80, 63.6, 29.2, 50.8, 80, 80, 80)
)

zone_plot <- ggplot() +
  geom_rect(data = zones, 
            aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = as.factor(zone_id)), 
            alpha = 0.5) +  # Use transparency for better visibility
  scale_fill_manual(values = rainbow(nrow(zones))) +  # Use different colors for each zone
  coord_fixed(ratio = 1) +  # Keep aspect ratio
  labs(title = "Zones Visualization", x = "X Coordinate", y = "Y Coordinate", fill = "Zone ID") +
  theme_minimal() +  # Use a minimal theme
  theme(legend.position = "right")

test_filtered <- test_filtered %>%
  rowwise() %>%
  mutate(
    zone = case_when(
      x >= zones$x_min[1] & x < zones$x_max[1] & y >= zones$y_min[1] & y <= zones$y_max[1] ~ zones$zone_id[1],
      x >= zones$x_min[2] & x < zones$x_max[2] & y >= zones$y_min[2] & y <= zones$y_max[2] ~ zones$zone_id[2],
      x >= zones$x_min[3] & x < zones$x_max[3] & y >= zones$y_min[3] & y <= zones$y_max[3] ~ zones$zone_id[3],
      x >= zones$x_min[4] & x < zones$x_max[4] & y >= zones$y_min[4] & y <= zones$y_max[4] ~ zones$zone_id[4],
      x >= zones$x_min[5] & x < zones$x_max[5] & y >= zones$y_min[5] & y <= zones$y_max[5] ~ zones$zone_id[5],
      x >= zones$x_min[6] & x < zones$x_max[6] & y >= zones$y_min[6] & y <= zones$y_max[6] ~ zones$zone_id[6],
      x >= zones$x_min[7] & x < zones$x_max[7] & y >= zones$y_min[7] & y <= zones$y_max[7] ~ zones$zone_id[7],
      x >= zones$x_min[8] & x < zones$x_max[8] & y >= zones$y_min[8] & y <= zones$y_max[8] ~ zones$zone_id[8],
      x >= zones$x_min[9] & x < zones$x_max[9] & y >= zones$y_min[9] & y <= zones$y_max[9] ~ zones$zone_id[9],
      x >= zones$x_min[10] & x < zones$x_max[10] & y >= zones$y_min[10] & y <= zones$y_max[10] ~ zones$zone_id[10],
      x >= zones$x_min[11] & x < zones$x_max[11] & y >= zones$y_min[11] & y <= zones$y_max[11] ~ zones$zone_id[11],
      x >= zones$x_min[12] & x < zones$x_max[12] & y >= zones$y_min[12] & y <= zones$y_max[12] ~ zones$zone_id[12],
      x >= zones$x_min[13] & x <= zones$x_max[13] & y >= zones$y_min[13] & y <= zones$y_max[13] ~ zones$zone_id[13],
      TRUE ~ NA_integer_  # Assign NA if it doesn't fall into any zone
    )
  ) %>%
  ungroup()

zone_counts <- test_filtered %>%
  group_by(id, period, timestamp, team.name, possession_team.name, event_location, teammate, zone) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = zone, values_from = count, values_fill = 0)

zone_differences <- zone_counts %>%
  # Gather the counts into a long format for easier manipulation
  pivot_longer(
    cols = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"),
    names_to = "zone",
    values_to = "count"
  ) %>%
  
  # Group by all relevant columns to calculate the differences for each zone
  group_by(id, period, timestamp, team.name,possession_team.name, event_location, zone) %>%
  summarize(
    difference = count[teammate == TRUE] - count[teammate == FALSE],
    .groups = 'drop'
  ) %>%
  
  # Pivot back to a wider format with zones as columns
  pivot_wider(
    names_from = zone,
    values_from = difference,
    values_fill = 0
  ) %>%
  
  mutate(across(starts_with(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")), 
              ~ ifelse(team.name != possession_team.name, . * -1, .))) %>%
  select(-team.name)

