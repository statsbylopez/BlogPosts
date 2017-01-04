tab.coaches <- read.csv("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/coaches_clean.csv")
library(dplyr); library(ggplot2); library(Matching)
set.seed(0)

## Set up to have data direct from Github
tab.coaches <- tab.coaches %>%
  group_by(TeamID) %>%
  mutate(f.win_p = lead(win_p, 1)) %>%
  na.omit() %>% 
  ungroup()

## Overall: has it helped to fire a coach?
fired.temp <- filter(tab.coaches, fired ==1)
mean(fired.temp$f.win_p - fired.temp$win_p) * 16

### Estimate coach firing probability; covariates from Glickman and Chase
fit.glm <- glm(fired ~ win_p + base_win_p + year1 + year2 + div_pct + year3 + SoS + 
                 rings_per_yr + gm_change 
               + years, data = tab.coaches, family = binomial())
tab.coaches$predict.prob <- predict(fit.glm,tab.coaches,  type = "response")
tab.coaches$fired.cat <- ifelse(tab.coaches$fired == 1, "Fired", "Kept")


### Compare probabilities
p <- ggplot(tab.coaches, aes(x = fired.cat, y = predict.prob))
p + geom_jitter() + coord_flip()+
  xlab("") + ylab("Predicted probability of a coach being fired") +
labs(title = "Probability of a coach being fired")

## Identify the outlier
filter(tab.coaches, fired.cat == "Kept" & predict.prob > 0.75)


### Initialize the matching
rownames(tab.coaches) <- 1:nrow(tab.coaches)
m1<-Match(Y=tab.coaches$f.win_p, Tr=tab.coaches$fired,  
            replace=TRUE, caliper=0.25,
            X=fitted(fit.glm), ties = FALSE, M = 1, 
            estimand="ATT")

### Organize the files for plotting and summary stats
Matched.Controls <- tab.coaches[m1$index.control,]
Matched.Treated <- tab.coaches[m1$index.treated,]
Matched.Subset <- rbind(Matched.Controls, Matched.Treated)
tab.coaches$Matched <- rownames(tab.coaches) %in% c(m1$index.control, m1$index.treated)


### Who was matched to whom?
p <- ggplot(tab.coaches, aes(x = fired.cat, y = predict.prob))
p + geom_jitter(aes(colour = Matched, fill = Matched, alpha = Matched), 
                colour = "black", pch = 21, size = 2) + coord_flip() +
  scale_alpha_discrete(range = c(0.4, 1)) +
  xlab("") + ylab("Predicted probability of a coach being fired") +
  labs(title = "Probability of a coach being fired")


##### Love plot to assess standardized bias
SB <- function(mean.t, mean.c, sd.t){return(mean.t - mean.c)/sd.t}

funct.bias <- function (var, data) {
  Matched.Subset.0 <- filter(data, fired == 0)
  Matched.Subset.1 <- filter(data, fired == 1)
  l <- paste(var)
  x.0 <- c(Mean.0 = "mean", Sd.0 = "sd")
  x.1 <- c(Mean.1 = "mean", Sd.1 = "sd")
  out.0 <- Matched.Subset.0 %>% 
    summarise_each_(funs_(x.0), l)
  out.1 <- Matched.Subset.1 %>% 
    summarise_each_(funs_(x.1), l)
  return(data.frame(out.0, out.1, var = var))
}
tab.coaches <- ungroup(tab.coaches)

### Pre matching SB
win_p <- funct.bias("win_p", tab.coaches)
base_win_p <- funct.bias("base_win_p", tab.coaches)
year1 <- funct.bias("year1", tab.coaches)
year2  <- funct.bias("year2", tab.coaches)
div_pct  <- funct.bias("div_pct", tab.coaches)
year3 <- funct.bias("year3", tab.coaches)
SoS <- funct.bias("SoS", tab.coaches)
rings_per_yr <- funct.bias("rings_per_yr", tab.coaches)
gm_change <- funct.bias("gm_change", tab.coaches)
years <- funct.bias("years", tab.coaches)

bias.pre <- rbind(win_p, base_win_p, year1, year2, div_pct, year3, SoS, rings_per_yr, gm_change, years)


## Post matching SB
Matched.Subset <- ungroup(Matched.Subset)
win_p <- funct.bias("win_p", Matched.Subset)
base_win_p <- funct.bias("base_win_p", Matched.Subset)
year1 <- funct.bias("year1", Matched.Subset)
year2  <- funct.bias("year2", Matched.Subset)
div_pct  <- funct.bias("div_pct", Matched.Subset)
year3 <- funct.bias("year3", Matched.Subset)
SoS <- funct.bias("SoS", Matched.Subset)
rings_per_yr <- funct.bias("rings_per_yr", Matched.Subset)
gm_change <- funct.bias("gm_change", Matched.Subset)
years <- funct.bias("years", Matched.Subset)

bias.post <- rbind(win_p, base_win_p, year1, year2, div_pct, year3, SoS, rings_per_yr, gm_change, years)

bias.pre <- mutate(bias.pre, Type = 0)
bias.post <- mutate(bias.post, Type = 1)

bias.all <- rbind(bias.pre, bias.post)
bias.all <- mutate(bias.all, std.diff = (Mean.1-Mean.0)/Sd.1)

### Code for the love plot
p <- ggplot(bias.all, aes(x = Type, y = abs(std.diff), group = var)) + 
  geom_point() + 
  geom_line()

p + theme(legend.position = "none") +
  ggtitle("Pre and post-matched standardized bias") + 
  xlab("") + ylab("Absolute Standard Difference")  + 
  scale_x_continuous(breaks = c(0, 1), 
                     labels = c("Pre-matched", "Post-matched"), lim = c(-0.05, 1.05)) + 
  geom_label_repel(data=filter(bias.all, Type == 0, abs(std.diff) > 0.35), aes(label=var)) +
  geom_hline(yintercept = 0.25, lty = 2, colour = "red")

fit.outcome <- lm(f.win_p ~ win_p + base_win_p + year1 + year2 + div_pct + year3 + SoS + 
                 rings_per_yr + gm_change + years + fired.cat, 
               data = Matched.Subset)
summary(fit.outcome)


fit.outcome <- lm(f.win_p ~ fired.cat, 
                  data = Matched.Subset)
summary(fit.outcome)
 

