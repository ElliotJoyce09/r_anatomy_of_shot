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

library(hms)

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

xG_dataframe <- cleaned_dataframe %>%
  group_by(match_id, team.name) %>%
  summarise(sum_xG = sum(shot.statsbomb_xg, na.rm = TRUE)) %>%
  rename(team = team.name, match = match_id)

opposition_teams <- c()

for (match_id in unique(xG_dataframe$match)) {
  match_data <- subset(xG_dataframe, match == match_id)
  for (i in 1:nrow(match_data)) {
    opposition_teams[xG_dataframe$match == match_id &
                       xG_dataframe$team == match_data$team[i]] <- match_data$team[match_data$team != match_data$team[i]]
  }
}

xG_dataframe$opposition_team <- opposition_teams

measure_of_defensive_area_occupied <- function(dataframe) {
  dataframe <- dataframe %>%
    filter(period != 5)
  # retrieves the maximum timestamp value of the first half, second half and extra time first half (if needed)
  timestamp_1 <- max(filter(dataframe, period == 1)$timestamp)
  if (length(unique(dataframe$period)) > 2) {
    max_timestamp_2 <- max(filter(dataframe, period == 2)$timestamp)
    timestamp_2 <- as_hms(as.POSIXct(timestamp_1, format = "%H:%M:%OS")) + as_hms(as.POSIXct(max_timestamp_2, format = "%H:%M:%OS"))
    max_timestamp_3 <- max(filter(dataframe, period == 3)$timestamp)
    timestamp_3 <- timestamp_2 + as_hms(as.POSIXct(max_timestamp_3, format = "%H:%M:%OS"))
  }
  # retrieves the two teams playing in the game
  team_names <- unique(dataframe$team.name)
  # a dataframe of zones
  zones <- data.frame(
    zone_id = 1:13,
    x_min = c(0, 18.8, 39.4, 0, 0, 18.8, 39.4, 18.8, 18.8, 18.8, 60, 78.8, 99.4),
    x_max = c(18.8, 39.4, 60, 18.8, 18.8, 39.4, 60, 60, 60, 60, 78.8, 99.4, 120),
    y_min = c(0, 0, 0, 16.3, 63.6, 63.6, 63.6, 50.8, 16.3, 29.2, 0, 0, 0),
    y_max = c(16.3, 16.3, 16.3, 63.6, 80, 80, 80, 63.6, 29.2, 50.8, 80, 80, 80)
  )
  # a dataframe of the scaling factors for each zones
  scaling_factors <- c(
    "1" = 1 / 8,
    "2" = 1 / 16,
    "3" = 1 / 32,
    "4" = 1,
    "5" = 1 / 8,
    "6" = 1 / 16,
    "7" = 1 / 32,
    "8" = 1 / 2,
    "9" = 1 / 2,
    "10" = 1 / 4,
    "11" = 1 / 64,
    "12" = 1 / 128,
    "13" = 1 / 256
  )
  # now the second half timestamp's start from the end of the first half, and now each column is given a team value, depending on the team of the player's coordinates. Also only one event for each time
  dataframe_cleaned <- dataframe %>%
    mutate(timestamp = if_else(
      period == 2,
      as_hms(as.POSIXct(timestamp, format = "%H:%M:%OS")) +
        difftime(as_hms(
          as.POSIXct(timestamp_1, format = "%H:%M:%OS")
        ), as_hms(
          as.POSIXct("00:00:00", format = "%H:%M:%OS")
        )),
      as_hms(as.POSIXct(timestamp, format = "%H:%M:%OS"))
    ))
  if (length(unique(dataframe$period)) > 2) {
    dataframe_cleaned <- dataframe_cleaned %>%
      mutate(
        timestamp = if_else(
          period == 3,
          as_hms(as.POSIXct(timestamp, format = "%H:%M:%OS")) +
            difftime(as_hms(timestamp_2), as_hms(
              as.POSIXct("00:00:00", format = "%H:%M:%OS")
            )),
          as_hms(as.POSIXct(timestamp, format = "%H:%M:%OS"))
        ),
        timestamp = if_else(
          period == 4,
          as_hms(as.POSIXct(timestamp, format = "%H:%M:%OS")) +
            difftime(as_hms(timestamp_3), as_hms(
              as.POSIXct("00:00:00", format = "%H:%M:%OS")
            )),
          as_hms(as.POSIXct(timestamp, format = "%H:%M:%OS"))
        )
      )
  }
  dataframe_cleaned <- dataframe_cleaned %>%
    mutate(team = ifelse(
      teammate,
      team.name,
      ifelse(team.name == team_names[1], team_names[2], team_names[1])
    )) %>%
    group_by(timestamp) %>%
    filter(id == first(id)) %>%
    ungroup()
  # splits the location into x and y, and makes sure they are between [0, 120] and [0, 80] for x and y respectively. Also turns all of them to be in their defensive area starting at 0
  dataframe_coordinates <- dataframe_cleaned %>%
    mutate(x = map_dbl(location, 1), y = map_dbl(location, 2)) %>%
    mutate(x = pmax(0, pmin(x, 120)), y = pmax(0, pmin(y, 80))) %>%
    mutate(x = if_else(teammate == FALSE, 120 - x, x),
           y = if_else(teammate == FALSE, 80 - y, y))
  dataframe_zones <- dataframe_coordinates %>%
    rowwise() %>%
    mutate(
      zone = case_when(
        x >= zones$x_min[1] &
          x < zones$x_max[1] &
          y >= zones$y_min[1] & y <= zones$y_max[1] ~ 1,
        x >= zones$x_min[2] &
          x < zones$x_max[2] &
          y >= zones$y_min[2] & y <= zones$y_max[2] ~ 2,
        x >= zones$x_min[3] &
          x < zones$x_max[3] &
          y >= zones$y_min[3] & y <= zones$y_max[3] ~ 3,
        x >= zones$x_min[4] &
          x < zones$x_max[4] &
          y >= zones$y_min[4] & y <= zones$y_max[4] ~ 4,
        x >= zones$x_min[5] &
          x < zones$x_max[5] &
          y >= zones$y_min[5] & y <= zones$y_max[5] ~ 5,
        x >= zones$x_min[6] &
          x < zones$x_max[6] &
          y >= zones$y_min[6] & y <= zones$y_max[6] ~ 6,
        x >= zones$x_min[7] &
          x < zones$x_max[7] &
          y >= zones$y_min[7] & y <= zones$y_max[7] ~ 7,
        x >= zones$x_min[8] &
          x < zones$x_max[8] &
          y >= zones$y_min[8] & y <= zones$y_max[8] ~ 8,
        x >= zones$x_min[9] &
          x < zones$x_max[9] &
          y >= zones$y_min[9] & y <= zones$y_max[9] ~ 9,
        x >= zones$x_min[10] &
          x < zones$x_max[10] &
          y >= zones$y_min[10] & y <= zones$y_max[10] ~ 10,
        x >= zones$x_min[11] &
          x < zones$x_max[11] &
          y >= zones$y_min[11] & y <= zones$y_max[11] ~ 11,
        x >= zones$x_min[12] &
          x < zones$x_max[12] &
          y >= zones$y_min[12] & y <= zones$y_max[12] ~ 12,
        x >= zones$x_min[13] &
          x <= zones$x_max[13] &
          y >= zones$y_min[13] & y <= zones$y_max[13] ~ 13
      )
    ) %>%
    ungroup()
  dataframe_zones_counts <- dataframe_zones %>%
    group_by(timestamp, team, possession_team.name, zone) %>%
    summarise(count = n(), .groups = 'drop') %>%
    pivot_wider(
      names_from = zone,
      values_from = count,
      values_fill = 0
    )
  dataframe_summarised_zones_counts <- dataframe_zones_counts %>%
    mutate(across(names(scaling_factors), ~ . * scaling_factors[cur_column()])) %>%
    rowwise() %>%
    mutate(row_sum = sum(c_across(names(scaling_factors)))) %>%
    select(-c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))
}

list_of_match_ids <- unique(merged_dataframe$match_id)

for (match_id in list_of_match_ids) {
  dataframe_name <- paste0("mdao_dataframe_", match_id)
  if (!exists(dataframe_name)) {
    filtered_df <- merged_dataframe[merged_dataframe$match_id == match_id, ]
    mdao_df <- measure_of_defensive_area_occupied(filtered_df)
    assign(dataframe_name, mdao_df)
  }
}

for (match_id in list_of_match_ids) {
  dataframe_name <- paste0("mdao_dataframe_", match_id)
  average_dataframe_name <- paste0("average_mdao_dataframe_", match_id)
  if (exists(dataframe_name) && !exists(average_dataframe_name)) {
    mdao_df <- get(dataframe_name)
    average_mdao_df <- mdao_df %>%
      group_by(team) %>%
      summarize(average_row_sum = mean(row_sum, na.rm = TRUE)) %>%
      mutate(match = match_id)
    assign(average_dataframe_name, average_mdao_df)
  }
}

for (match_id in list_of_match_ids) {
  dataframe_name <- paste0("mdao_dataframe_", match_id)
  oop_average_dataframe_name <- paste0("oop_average_mdao_dataframe_", match_id)
  if (exists(dataframe_name) &&
      !exists(oop_average_dataframe_name)) {
    mdao_df <- get(dataframe_name)
    oop_average_mdao_df <- mdao_df %>%
      filter(team != possession_team.name) %>%
      group_by(team, possession_team.name) %>%
      summarize(average_row_sum = mean(row_sum, na.rm = TRUE)) %>%
      mutate(match = match_id)
    assign(oop_average_dataframe_name, oop_average_mdao_df)
  }
}


oop_average_mdao_dataframes_to_bind <- ls(pattern = "^oop_average_mdao_dataframe")
list_of_oop_average_mdao_dataframes <- lapply(oop_average_mdao_dataframes_to_bind, get)
combined_oop_average_mdao_dataframe <- bind_rows(list_of_oop_average_mdao_dataframes)

mdao_correlation_dataframe <- combined_oop_average_mdao_dataframe %>%
  left_join(xG_dataframe, by = c("team" = "opposition_team", "match")) %>%
  select(-c(possession_team.name, team.y)) %>%
  rename("opposition_xG" = "sum_xG")

group_stage_list_of_match_ids <- unique(filter(euro2024matches, competition_stage.name == "Group Stage")$match_id)

sum_group_stage_mdao_correlation_dataframe <- mdao_correlation_dataframe %>%
  filter(match %in% group_stage_list_of_match_ids) %>%
  group_by(team) %>%
  summarise(
    average_row_sum = mean(average_row_sum),
    sum_opposition_xG = sum(opposition_xG, na.rm = TRUE)
  )

mdao_correlation <- cor(
  mdao_correlation_dataframe$opposition_xG,
  mdao_correlation_dataframe$average_row_sum,
  method = "pearson"
)
sum_mdao_correlation <- cor(
  sum_group_stage_mdao_correlation_dataframe$sum_opposition_xG,
  sum_group_stage_mdao_correlation_dataframe$average_row_sum,
  method = "pearson"
)

ggplot(mdao_correlation_dataframe,
       aes(x = average_row_sum, y = opposition_xG)) +
  geom_point()

ggplot(
  sum_group_stage_mdao_correlation_dataframe,
  aes(x = average_row_sum, y = sum_opposition_xG)
) +
  geom_point(
  )

