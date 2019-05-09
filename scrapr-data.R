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


df.scrapr <- lapply(2009:2018, scrapr.fun)

scrapr.plays <- bind_rows(df.scrapr)

write_csv(scrapr.plays, "scrapr_plays.csv")


### Function to sample plays

sample.rp.drive.needs.TD <- function(down, yards.to.go, yards.from.own.goal) {
  
  down.original <- down
  if (yards.from.own.goal <= 5){yards.from.own.goal <- 5}
  if (down == 4 & yards.to.go >= 3){down <- 3}
  if (yards.to.go >= 20){yards.to.go <- 20}
  
  if (yards.from.own.goal <= 70) {
      data.RP <- filter(df.scrimmage, yfog <= yards.from.own.goal + 3, 
                        yfog >= yards.from.own.goal - 3,
                        down == down, ydstogo == yards.to.go)
    } else if (yards.from.own.goal <= 90) {
      data.RP <- filter(df.scrimmage, yfog >= yards.from.own.goal - 2,
                        yfog <= yards.from.own.goal + 2,
                        down == down, ydstogo == yards.to.go)
    } else if (yards.from.own.goal <= 97) {
      data.RP <- filter(df.scrimmage, yfog >= yards.from.own.goal - 1,
                        yfog <= yards.from.own.goal + 1,
                        down == down, ydstogo == yards.to.go)
    } else {
      data.RP <- filter(df.scrimmage, yfog == yards.from.own.goal,
                        down == down, ydstogo == yards.to.go)
    }
    sim.RP <- sample_n(data.RP, 1)
    yds.gained <- sim.RP$yards_gained
    new.yfog <- yards.from.own.goal + yds.gained
    new.down <- ifelse(yds.gained >= yards.to.go, 1, down.original + 1)
    new.distance <- ifelse(yds.gained >= yards.to.go & new.yfog <= 90, 10, 
                           ifelse(yds.gained >= yards.to.go & new.yfog > 90, 100-new.yfog, 
                                  yards.to.go - yds.gained))
    if (new.distance <= 0){new.distance <- 1} 
    if (new.yfog >= 100){new.yfog <- 99}
    keep.drive <- data.frame(down.original, yards.to.go, yards.from.own.goal, 
                             yds.gained, play = sim.RP$desc, new.yfog, new.down, new.distance, 
                             is.turnover = sim.RP$is.turnover, 
                             is.td.offense = sim.RP$is.td.offense, 
                             end.drive = new.down > 4 | sim.RP$is.turnover | sim.RP$is.td.offense)
    return(keep.drive)
}  
