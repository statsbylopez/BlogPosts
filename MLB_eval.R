### Load in the data
library(tidyverse)
picks.all <- read.csv("mlb.csv")

########################## 
## Variable descriptions:
##########################
## Date of game
## team.type: away/home
## team: team backed
## ML: opening money line of chosen team
## opp: opponent
## prob.open: implied probability of a win using opening ML
## prob.tobeat: implied probability to beat
## prob.sim: model estimated probability
## units: wager amount
## towin: amoung to win
## won: TRUE/FALSE if the team won
## profit: game-level profit
## profit.cum: cumulative profit to date
## prob.close: implied probability of a win using closing ML
## delta.price: change in perceived probability, opening to closing ML
##########################

picks.all <- picks.all %>% mutate(pick.no = 1:n())
picks.all %>% group_by(team.type) %>% count()
picks.all %>% group_by(ML > 100) %>% count()
picks.all %>% group_by(team.type) %>% summarise(total.profit = sum(profit))
picks.all %>% group_by(ML > 100) %>% summarise(total.profit = sum(profit))

## Log loss
picks.all %>% mutate(ll = log(prob.open)*won + (1-won)*log(1-prob.open)) %>% summarise(ave.ll = mean(ll))
picks.all %>% mutate(ll = log(prob.tobeat)*won + (1-won)*log(1-prob.tobeat)) %>% summarise(ave.ll = mean(ll))
picks.all %>% mutate(ll = log(prob.sim)*won + (1-won)*log(1-prob.sim)) %>% summarise(ave.ll = mean(ll))

## ROI
roi.orig <- (picks.all %>% tail(1) %>% select(profit.cum) %>% unlist)
roi.orig <- as.numeric(roi.orig)/sum(as.numeric(picks.all$units))
roi.orig


ggplot(picks.all, aes(x = pick.no, y = profit.cum)) + 
  geom_line() +
  ylab("Profit") + xlab("Pick number") + 
  labs(title = "Year to date profit in units") 


####################
### Bootstrap profit
####################

set.seed(100)
profit.sim.sum <- NULL
cumsum.sim <- NULL
for (i in 1:1000){
  result.sim <- rbinom(nrow(picks.all), 1, picks.all$prob.open)   
  profit.sim <- ifelse(result.sim == 1, as.numeric(picks.all$towin), -1*as.numeric(picks.all$units))
  profit.sim.sum[i] <- sum(profit.sim)
  cumsum.sim <- rbind(cumsum.sim, cumsum(profit.sim))
}

df.sim <- data.frame(sim = profit.sim.sum)
ggplot(df.sim,aes(sim)) + geom_histogram() + 
  geom_vline(xintercept = sum(picks.all$profit), col = "red") + 
  labs(title = "1000 simulated seasons") + 
  xlab("Profit") + ylab("# seasons")
mean(df.sim$sim < sum(picks.all$profit))


p <-  ggplot(picks.all, aes(x = pick.no, y = profit.cum)) + 
  geom_line() +
  ylab("Profit") + xlab("Time (pick number)") + ylim(-70, 60) + 
  labs(title = "Year to date profit in units") 

for (i in 1:200){
  df <- data.frame(pick.no = 1:dim(cumsum.sim)[2], profit.cum = cumsum.sim[i,])
  p <- p + geom_line(data = df, aes(x = pick.no, y = profit.cum), colour = "grey")
}
p + geom_line(data = picks.all, aes(x = pick.no, y = profit.cum), colour = "red") + 
  geom_abline(intercept = 0, slope = 0)



########################
## Team level
########################
picks.team <- picks.all %>% group_by(team) %>% summarise(investment = sum(as.numeric(units)), total.profit = sum(profit)) %>% mutate(type = "Backed")
picks.opp<- picks.all %>% group_by(opp) %>% summarise(investment = sum(as.numeric(units)), total.profit = sum(profit)) %>% 
  rename(team = opp) %>% mutate(type = "Faded")

picks.plot <- bind_rows(picks.team, picks.opp)
ggplot(picks.plot, aes(investment, total.profit)) + geom_text(aes(label = team) ) + xlab("Investment") + ylab("Profit") +
  geom_hline(yintercept = 0, col = "red", lty = 2) + ggtitle("Profit ~ Investment (total units)") + facet_wrap(~type)


########################
## CLV
########################
df.move <- data.frame(worse = round(mean(picks.all$delta.price < 0, na.rm = TRUE)*100, 1),
                      same = round(mean(picks.all$delta.price == 0, na.rm = TRUE)*100, 1),
                      better = round(mean(picks.all$delta.price > 0, na.rm = TRUE)*100, 1))

ggplot(picks.all, aes(delta.price)) + geom_histogram(alpha = 0.5) + 
  geom_vline(xintercept = 0, col = "red", lty = 2) + xlim(-0.085, 0.085) + 
  annotate("text", x = c(-0.05, 0, 0.05), y = c(10, 10, 10), 
           label = paste(colnames(df.move), df.move[1,], "%"), colour = "red") +
  labs(title = "Closing line value, relative to initial price") + ylab("Number of games") + 
  xlab("Change in price of model picks") + facet_wrap(~(ML > 100))
mean(picks.all$delta.price == 0, na.rm = TRUE)
median(picks.all$delta.price, na.rm = TRUE)

picks.all <- picks.all %>% mutate(edge.cat = cut(units, 3))
picks.all %>% group_by(edge.cat) %>% summarise(ave.price = mean(delta.price, na.rm = TRUE), n = n())

### Random plots
ggplot(picks.all, aes(pick.no, delta.cumsum)) + 
  geom_point() + ggtitle("Cumulative edge over original price") + xlab("Time") + 
  ylab("Cumulative edge") 

ballpark <- picks.all %>% group_by(team.type, opp) %>% summarise(sum.profit = sum(profit)) %>% filter(team.type == "Away")
ggplot(ballpark, aes(opp, sum.profit)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("Profit") + xlab("Home team") +
  ggtitle("Profit, picking against these home teams")

