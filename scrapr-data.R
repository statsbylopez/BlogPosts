

scrapr.fun <- function(year){
  file.read <- paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by#_play_data/regular_season/reg_pbp_",year,".csv")
  df.scrapr.temp <- suppressMessages(read_csv(file.read))
  print(year)
  df.scrapr.1 <- df.scrapr.temp %>% select(home_team, game_date, play_id, game_id, desc, #half_seconds_remaining, game_seconds_remaining, down, ydstogo, yardline_100, play_type, #yards_gained, total_home_score, total_away_score, touchdown, pass_touchdown, 
                                           rush_touchdown, penalty, penalty_yards, posteam, #fumble_recovery_1_team) 
  return(df.scrapr.1)
}

df.scrapr.temp %>% filter(!is.na(fumble_recovery_1_team)) %>% sample_n(3) %>% #print.data.frame()


df.scrapr <- lapply(2009:2018, scrapr.fun)

scrapr.plays <- bind_rows(df.scrapr)

write_csv(scrapr.plays, "scrapr_plays.csv")
