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
    group_by(match_id) %>%
    mutate(
      team_1 = unique(team.name)[1],
      team_2 = unique(team.name)[2],
      team = if_else(teammate, team.name, if_else(team.name == team_1, team_2, team_1))
    ) %>%
    ungroup() %>%
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
    group_by(id, team, possession_team.name, zone) %>%
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

progressive_actions <- function(dataframe) {
  dataframe <- dataframe %>%
    filter(str_detect(type.name, "carry") | (str_detect(type.name, "complete") & str_detect(type.name, "pass")) & !str_detect(type.name, "incomplete")) %>%
    mutate(start_x = map_dbl(location, 1), start_y = map_dbl(location, 2), end_x = map_dbl(event.end_location, 1), end_y = map_dbl(event.end_location, 2)) %>%
    mutate(start_distance_to_goal = sqrt((120 - start_x)^2 + (start_y - 40)^2), end_distance_to_goal = sqrt((120 - end_x)^2 + (end_y - 40)^2)) %>%
    filter((start_distance_to_goal - end_distance_to_goal)/start_distance_to_goal >= 0.25) %>%
    mutate(progressive_action = 1) %>%
    select(id, progressive_action)
}

defenders_removed <- function(dataframe) {
  dataframe <- dataframe %>%
    filter(teammate == "FALSE") %>%
    filter(str_detect(type.name, "carry") | (str_detect(type.name, "complete") & str_detect(type.name, "pass")) & !str_detect(type.name, "incomplete")) %>%
    mutate(x = map_dbl(location, 1), y = map_dbl(location, 2), start_x = map_dbl(event_location, 1), start_y = map_dbl(event_location, 2), end_x = map_dbl(event.end_location, 1), end_y = map_dbl(event.end_location, 2)) %>%
    filter(x > start_x & x < end_x) %>%
    group_by(id, team.name) %>%
    summarise(number_of_defenders_removed = n())
}

turnovers <- function(dataframe) {
  dataframe <- dataframe %>%
    group_by(match_id) %>% # Group by match_id
    filter(lead(possession_team.name) != possession_team.name | is.na(lead(possession_team.name))) %>% 
    ungroup() %>%
    mutate(dispossessed_location = ifelse(event.end_location != "NA", event.end_location, location)) %>%
    filter(dispossessed_location != "NULL") %>%
    mutate(x = map_dbl(dispossessed_location, 1), y = map_dbl(dispossessed_location, 2)) %>%
    mutate(x = if_else(team.name != possession_team.name, 120 - x, x),
           y = if_else(team.name != possession_team.name, 80 - y, y)) %>%
    mutate(turnover_distance_to_own_goal = sqrt(x^2 + (y - 40)^2)) %>%
    select(id, turnover_distance_to_own_goal)
}

defensive_forced_pressures <- function(dataframe) {
  pressure_indices <- which(dataframe$type.name == "pressure" & 
                              dataframe$team.name != dataframe$possession_team.name)
  forced_failure_indicies <- which(dataframe$type.name %in% c("dispossessed", "miscontrol", "block", "error") |
                                     str_detect(dataframe$type.name, "incomplete|failed|clearance|blocked"))
  valid_indices <- pressure_indices[sapply(pressure_indices, function(i) (i + 1) %in% forced_failure_indicies)]
  dataframe <- dataframe[valid_indices + 1, ] %>%
  mutate(failure_location = ifelse(event.end_location != "NA" & event.end_location != "NULL", 
                                   event.end_location, location)) %>%
    mutate(x = map_dbl(failure_location, 1), y = map_dbl(failure_location, 2)) %>%
    mutate(x = if_else(team.name != possession_team.name, 120 - x, x),
           y = if_else(team.name != possession_team.name, 80 - y, y)) %>%
    mutate(failure_from_opposition_pressure_distance_to_own_goal = sqrt(x^2 + (y - 40)^2)) %>%
    select(id, failure_from_opposition_pressure_distance_to_own_goal)
}




