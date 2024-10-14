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


# this gives all competitions in which the 360 data is available

competitions <- FreeCompetitions() %>%
  filter(!is.na(match_available_360))

euro2024matches <- FreeMatches(competitions %>%
                                 filter(competition_id == 55 &
                                          season_name == 2024))

# the following commands take a little while to load, both around 200k entries

euro2024_360data <- free_allevents_360(euro2024matches, Parallel = TRUE) %>%
  rename(id = event_uuid)

euro2024_events <- get.opposingteam(allclean(free_allevents(euro2024matches, Parallel = TRUE)))

euro2024_events[] <- lapply(euro2024_events, function(x) {
  if (is.character(x)) {
    tolower(gsub("\\*", "", gsub(" ", "_", x)))
  } else {
    x
  }
})

modify_events_dataframe <- function(df) {
  
  df[["event.end_location"]] <- NA
  df[["other_player.name"]] <- NA
  
  pass_condition <- df[["type.name"]] %in% "pass"
  df[["type.name"]][pass_condition] <- paste0(
    ifelse(is.na(df[["pass.outcome.name"]][pass_condition]), "complete", df[["pass.outcome.name"]][pass_condition]),
    "_",
    ifelse(is.na(df[["pass.technique.name"]][pass_condition]), "", paste0(df[["pass.technique.name"]][pass_condition], "_")),
    df[["pass.height.name"]][pass_condition],
    "_from_",
    ifelse(is.na(df[["pass.body_part.name"]][pass_condition]), 
           ifelse(is.na(df[["pass.type.name"]][pass_condition]), "aerial_won", df[["pass.type.name"]][pass_condition]), 
           df[["pass.body_part.name"]][pass_condition])
  )
  
  df[["event.end_location"]][pass_condition] <- df[["pass.end_location"]][pass_condition]
  df[["other_player.name"]][pass_condition] <- df[["pass.recipient.name"]][pass_condition]
  
  ball_receipt_condition <- df[["type.name"]] %in% "ball_receipt"
  df[["type.name"]][ball_receipt_condition] <- paste0(
    ifelse(is.na(df[["ball_receipt.outcome.name"]][ball_receipt_condition]), "", 
            paste0(df[["ball_receipt.outcome.name"]][ball_receipt_condition], "_")),
    "ball_receipt"
  )
  
  
  dribble_condition <- df[["type.name"]] %in% "dribble"
  df[["type.name"]][dribble_condition] <- paste0(df[["dribble.outcome.name"]][dribble_condition], "_dribble")
 
  
  interception_condition <- df[["type.name"]] %in% "interception"
  df[["type.name"]][interception_condition] <- paste0(df[["interception.outcome.name"]][interception_condition], "_interception")
  
 
  duel_condition <- df[["type.name"]] %in% "duel"
  df[["type.name"]][duel_condition] <- paste0(
    ifelse(is.na(df[["duel.outcome.name"]][duel_condition]), "", 
            paste0(df[["duel.outcome.name"]][duel_condition], "_")),
    df[["duel.type.name"]][duel_condition]
  )

  
  clearance_condition <- df[["type.name"]] %in% "clearance"
  df[["type.name"]][clearance_condition] <- paste0("clearance_from_", df[["clearance.body_part.name"]][clearance_condition])

  
  foul_committed_condition <- df[["type.name"]] %in% "foul_committed"
  df[["type.name"]][foul_committed_condition] <- paste0(
    ifelse(is.na(df[["foul_committed.offensive"]][foul_committed_condition]), "", "offensive_"),
    ifelse(is.na(df[["foul_committed.card.name"]][foul_committed_condition]), "", 
            paste0(df[["foul_committed.card.name"]][foul_committed_condition], "_")),
    "foul_committed", 
    ifelse(is.na(df[["foul_committted.type.name"]][foul_committed_condition]), "",
           paste0("_", df[["foul_committted.type.name"]][foul_committed_condition], "_")),
    ifelse(is.na(df[["foul_committed.advantage"]][foul_committed_condition]), "", "_advantage_played")
  )
  
  
  foul_won_condition <- df[["type.name"]] %in% "foul_won"
  df[["type.name"]][foul_won_condition] <- paste0(
    ifelse(is.na(df[["foul_won.defensive"]][foul_won_condition]), "", "defensive_"),
    "foul_won", 
    ifelse(is.na(df[["foul_won.advantage"]][foul_won_condition]), "", "_advantage_played")
  )
  
  
  shot_condition <- df[["type.name"]] %in% "shot"
  df[["type.name"]][shot_condition] <- paste0(
    ifelse(is.na(df[["shot.one_on_one"]][shot_condition]), "", "one_on_one_"),
    ifelse(is.na(df[["shot.first_time"]][shot_condition]), "", "first_time_"), 
    ifelse(is.na(df[["shot.aerial_won"]][shot_condition]), "", "aerial_won_"),
    ifelse(is.na(df[["shot.deflected"]][shot_condition]), "", "deflected_"),
    df[["shot.technique.name"]][shot_condition],
    "_shot_with_",
    df[["shot.body_part.name"]][shot_condition],
    "_from_",
    df[["shot.type.name"]][shot_condition],
    "_which_was_",
    df[["shot.outcome.name"]][shot_condition]
  )
  
  df[["event.end_location"]][shot_condition] <- df[["shot.end_location"]][shot_condition]
  df[["other_player.name"]][shot_condition] <- df[["player.name.GK"]][shot_condition]

  
  block_condition <- df[["type.name"]] %in% "block"
  df[["type.name"]][block_condition] <- paste0(
    ifelse(is.na(df[["block.offensive"]][block_condition]), "", "offensive_"),
    ifelse(is.na(df[["block.save_block"]][block_condition]), "", "save_"), 
    ifelse(is.na(df[["block.deflection"]][block_condition]), "", "deflected_"),
    "block"
  )
  
  
  fifty_fifty_condition <- df[["type.name"]] %in% "50/50"
  df[["type.name"]][fifty_fifty_condition] <- paste0(df[["50_50.outcome.name"]][fifty_fifty_condition], "_50_50")
  
  
  substitution_condition <- df[["type.name"]] %in% "substitution"
  df[["type.name"]][substitution_condition] <- paste0(
    df[["substitution.outcome.name"]][substitution_condition], "_substitution"
  )
  
  df[["other_player.name"]][substitution_condition] <- df[["substitution.replacement.name"]][substitution_condition]
  
  
  bad_behaviour_condition <- df[["type.name"]] %in% "bad_behaviour"
  df[["type.name"]][bad_behaviour_condition] <- df[["bad_behaviour.card.name"]][bad_behaviour_condition]
  
  
  ball_recovery_condition <- df[["type.name"]] %in% "ball_recovery"
  df[["type.name"]][ball_recovery_condition] <- paste0(
    ifelse(is.na(df[["ball_recovery.recovery_failure"]][ball_recovery_condition]), "", "failed_"),
    ifelse(is.na(df[["ball_recovery.offensive"]][ball_recovery_condition]), "", "offensive_"),
    "ball_recovery"
  )
  
  
  goalkeeper_condition <- df[["type.name"]] %in% "goal_keeper"
  df[["type.name"]][goalkeeper_condition] <- paste0(
    ifelse(is.na(df[["goalkeeper.position.name"]][goalkeeper_condition]), "", paste0(df[["goalkeeper.position.name"]][goalkeeper_condition], "_")),
    "goalkeeper_",
    df[["goalkeeper.type.name"]][goalkeeper_condition],
    ifelse(is.na(df[["goalkeeper.outcome.name"]][goalkeeper_condition]), "", paste0("_", df[["goalkeeper.outcome.name"]][goalkeeper_condition])),
    ifelse(is.na(df[["goalkeeper.technique.name"]][goalkeeper_condition]), "", paste0("_by_", df[["goalkeeper.technique.name"]][goalkeeper_condition])),
    ifelse(is.na(df[["goalkeeper.body_part.name"]][goalkeeper_condition]), "", paste0("_using_", df[["goalkeeper.body_part.name"]][goalkeeper_condition]))
  )
  
  df[["event.end_location"]][shot_condition] <- df[["goalkeeper.end_location"]][shot_condition]
  
  
  carry_condition <- df[["type.name"]] %in% "carry"
  
  df[["event.end_location"]][carry_condition] <- df[["carry.end_location"]][carry_condition]
  
  return(df)
}

# Assuming euro2024_events is your dataframe
df_modified <- modify_events_dataframe(euro2024_events)

drops <- c(
  "pass.height.name",
  "pass.height.id",
  "pass.body_part.name",
  "pass.body_part.id",
  "pass.type.name",
  "pass.type.id",
  "pass.outcome.name",
  "pass.outcome.id",
  "pass.technique.name",
  "pass.technique.id",
  "pass.through_ball",
  "pass.assisted_shot_id",
  "pass.shot_assist",
  "pass.switch",
  "pass.deflected",
  "pass.outswinging",
  "pass.cut_back",
  "pass.goal_assist",
  "pass.aerial_won",
  "pass.cross",
  "ball_receipt.outcome.name",
  "ball_receipt.outcome.id",
  "dribble.outcome.name",
  "dribble.outcome.id",
  "interception.outcome.name",
  "interception.outcome.id",
  "duel.type.name",
  "duel.type.id",
  "duel.outcome.name",
  "duel.outcome.id",
  "clearance.head",
  "clearance.right_foot",
  "clearance.left_foot",
  "clearance.aerial_won",
  "clearance.body_part.name",
  "clearance.body_part.id",
  "foul_committed.body_part.name",
  "foul_committed.advantage",
  "foul_committed.penalty",
  "foul_committed.card.id",
  "foul_committed.card.name",
  "shot.first_time",
  "shot.aerial_won",
  "shot.deflected",
  "shot.technique.id",
  "shot.technique.name",
  "shot.body_part.id",
  "shot.body_part.name",
  "shot.type.id",
  "shot.type.name",
  "shot.outcome.id",
  "shot.outcome.name",
  "block.save_block",
  "block.deflection",
  "pass.no_touch",
  "pass.inswinging",
  "carry.end_location.x",
  "carry.end_location.y",
  "pass.end_location.x",
  "pass.end_location.y",
  "shot.end_location.x",
  "shot.end_location.y",
  "shot.end_location.z",
  "pass.end_location",
  "shot.end_location",
  "foul_won.advantage",
  "foul_won.penalty",
  "foul_won.defensive",
  "50_50.outcome.id",
  "50_50.outcome.name",
  "pass.recipient.name",
  "pass.recipient.id",
  "substitution.outcome.id",
  "substitution.outcome.name",
  "substitution.replacement.name",
  "substitution.replacement.id",
  "pass.recipient.id",
  "pass.recipient.name",
  "bad_behaviour.card.id",
  "bad_behaviour.card.name",
  "competition_id",
  "season_id",
  "foul_committed.type.name",
  "foul_committed.type.id",
  "foul_committed.offensive",
  "shot.key_pass_id",
  "dribble.nutmeg",
  "dribble.overrun",
  "shot.one_on_one",
  "ball_recovery.recovery_failure",
  "clearance.other",
  "miscontrol.aerial_won",
  "pass.straight",
  "block.offensive",
  "goalkeeper.position.name",
  "goalkeeper.position.id",
  "goalkeeper.type.name",
  "goalkeeper.type.id",
  "goalkeeper.outcome.name",
  "goalkeeper.outcome.id",
  "goalkeeper.technique.name",
  "goalkeeper.technique.id",
  "goalkeeper.body_part.name",
  "goalkeeper.body_part.id",
  "goalkeeper.end_location",
  "goalkeeper.shot_saved_to_post",
  "goalkeeper.success_in_play",
  "goalkeeper.shot_saved_off_target",
  "goalkeeper.penalty_saved_to_post",
  "carry.end_location",
  "player.name.GK",
  "player.id.GK",
  "shot.open_goal",
  "shot.saved_to_post",
  "dribble.no_touch",
  "pass.miscommunication",
  "player_off.permanent",
  "shot.saved_off_target",
  "shot.redirect",
  "shot.follows_dribble",
  "shot_impact_height",
  "ball_recovery.offensive",
  "goalkeeper.punched_out",
  "injury_stoppage.in_chain",
  "index",
  "minute",
  "second",
  "milliseconds",
  "StartOfPossesion",
  "TimeToPossEnd",
  "OpposingTeam.id",
  "type.id",
  "position.id"
)

removed_columns_df <- df_modified[, !(colnames(df_modified) %in% drops)]
