## Grab the nflscrapr data so that it's in one data frame

scrapr.fun <- function(year){
  file.read <- paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by#_play_data/regular_season/reg_pbp_",year,".csv")
  df.scrapr.temp <- suppressMessages(read_csv(file.read))
  print(year)
  df.scrapr.1 <- df.scrapr.temp %>% select(home_team, game_date, play_id, game_id, desc, half_seconds_remaining, game_seconds_remaining, 
                                           down, ydstogo, yardline_100, play_type, yards_gained, total_home_score, total_away_score, 
                                           touchdown, pass_touchdown, rush_touchdown, penalty, penalty_yards, posteam, fumble_recovery_1_team) 
  return(df.scrapr.1)
}

