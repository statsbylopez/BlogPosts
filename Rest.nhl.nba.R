source("config.R")
load(file.path(data_raw, "bigfour.rda"))
### This uses the public .rda available at 
## https://github.com/bigfour/competitiveness/blob/master/data/bigfour_public.rda

winter.h <- winter %>% 
  mutate(sdiff = home_score - visitor_score, gameDate = as.Date(gameDate), type = "home") %>%
  select(gameDate, type, home_team, p_home, sdiff, sport, season) %>%
  rename(team = home_team, prob = p_home)

winter.v <- winter %>% 
  mutate(sdiff = visitor_score - home_score, gameDate = as.Date(gameDate), type = "vis") %>%
  select(gameDate, type, visitor_team, p_vis, sdiff, sport, season) %>%
  rename(team = visitor_team, prob = p_vis)

winter.all <- rbind(winter.h, winter.v) %>% 
  arrange(team, gameDate) %>%
  group_by(season, team) %>%
  mutate(date.lag = gameDate - lag(gameDate, 1), 
         date.lag = ifelse(date.lag > 3, 3, date.lag), 
         date.lag2 = gameDate - lag(gameDate, 2), 
         date.lag3 = gameDate - lag(gameDate, 3), 
         date.lag4 = gameDate - lag(gameDate, 4), 
         date.lag5 = gameDate - lag(gameDate, 5), 
         five.in.seven = date.lag5 <=7,
         four.in.six = date.lag4 <=6,
         three.in.four = date.lag3 <=4, 
         back.to.back = date.lag <=1) %>%
  ungroup()


winter.sum <- winter.all %>% filter(type == "home") %>%
  na.omit() %>%
  group_by(date.lag, sport) %>%
  summarise(ave.prob = mean(sdiff > 0), n.ex = n(), me = 1/sqrt(n.ex))

limits <- aes(ymin= ave.prob - me, ymax = ave.prob + me)

ggplot(filter(winter.sum, date.lag > 0), aes(date.lag, ave.prob, colour = sport)) + 
  geom_errorbar(limits, position = "dodge", width=0.25) + 
  geom_point(position = position_dodge(width = 0.25))+ 
  geom_line(position = position_dodge(width = 0.25)) + 
  scale_x_continuous(labels = c("1 day", "2 days", "3 days+"), 
                     breaks = c(1,2,3), "Days Rest") + 
  ggtitle("Win percentage by days rest, home team") +
  scale_y_continuous(labels = scales::percent, "")




winter.sum <- winter.all %>% filter(type == "vis") %>%
  na.omit() %>%
  group_by(date.lag, sport) %>%
  summarise(ave.prob = mean(sdiff > 0), n.ex = n(), me = 1/sqrt(n.ex))

limits <- aes(ymin= ave.prob - me, ymax = ave.prob + me)

ggplot(filter(winter.sum, date.lag > 0), aes(date.lag, ave.prob, colour = sport)) + 
  geom_errorbar(limits, position = "dodge", width=0.25) + 
  geom_point(position = position_dodge(width = 0.25))+ 
  geom_line(position = position_dodge(width = 0.25)) + 
  scale_x_continuous(labels = c("1 day", "2 days", "3 days+"), 
                     breaks = c(1,2,3), "Days Rest") + 
  ggtitle("Win percentage by days rest, visiting team") +
  scale_y_continuous(labels = scales::percent, "")





winter.new <- gather(na.omit(winter.all), RestType, 
                     RestVar, five.in.seven:back.to.back, factor_key=TRUE) 

rates.sport <- winter.new %>% group_by(sport, RestType) %>% 
  summarise(n.games.season = sum(RestVar)/(11*30))

rates.sport$RestType <- factor(rates.sport$RestType, 
         levels = c("back.to.back", "three.in.four",
                    "four.in.six", "five.in.seven"))

ggplot(rates.sport, aes(RestType, n.games.season, colour = sport, fill = sport)) + 
  geom_bar(stat = "identity", position="dodge") +
  scale_x_discrete(labels = c("2 in 2", "3 in 4", "4 in 6", "5 in 7")) + 
  ylab("") + ggtitle("Number of low-rest games per team per season")



rates.sport <- winter.new %>% filter(RestVar) %>%
  group_by(sport, RestType) %>% 
  summarise(n.games.season = sum(sdiff > 0)/n(), n.games = n())


rates.sport$RestType <- factor(rates.sport$RestType, 
        levels = c("back.to.back", "three.in.four",
           "four.in.six", "five.in.seven"))


ggplot(rates.sport, aes(RestType, n.games.season, colour = sport, fill = sport, size = n.games)) + 
  geom_point(stat = "identity", position="dodge") +
  scale_x_discrete(labels = c("2 in 2", "3 in 4", "4 in 6", "5 in 7")) + 
  ylab("") + ggtitle("Win percentage in low-rest games") +
  scale_y_continuous(labels = scales::percent, "")

