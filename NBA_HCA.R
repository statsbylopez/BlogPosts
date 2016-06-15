
library(ROCR)
library(readr)
library(dplyr)
library(lme4)
library(mosaic) 
library(randomForest)
library(glmnet)

df.1314 <- read_csv("~/Downloads/KOBE 2013_14 Updated Version.csv")
df.1415 <- read_csv("~/Downloads/KOBE 2014_15 Updated Version.csv")



df1314 <- select(df.1314, GAME_ID, MATCHUP, PLAYER_NAME, SHOT_NUMBER, PERIOD, LOCATION, GAME_CLOCK, SHOT_CLOCK, DRIBBLES, TOUCH_TIME, 
                  SHOT_DIST, PTS_TYPE, SHOT_RESULT, CLOSE_DEF_DIST, FGM, PTS, AGE, 
                  Team.Shooter, shot.clock, shot.dist, def.type, Dribble.type, dribble.num, 
                  MINUTES_REMAINING, SECONDS_REMAINING, HEIGHT_DIFF, defdist)

df1415 <- select(df.1415, GAME_ID, MATCHUP, PLAYER_NAME, SHOT_NUMBER, PERIOD, LOCATION, GAME_CLOCK, SHOT_CLOCK, DRIBBLES, TOUCH_TIME, 
                  SHOT_DIST, PTS_TYPE, SHOT_RESULT, CLOSE_DEF_DIST, FGM, PTS, AGE, 
                  Team.Shooter, shot.clock, shot.dist, def.type, Dribble.type, dribble.num, 
                  MINUTES_REMAINING, SECONDS_REMAINING, HEIGHT_DIFF, defdist)

df.all <- rbind(df1314, df1415)
df.all <- na.omit(df.all)
df.all <- df.all %>%
  mutate(age.cent = AGE-mean(AGE), 
         heightdiff.cent = HEIGHT_DIFF - mean(HEIGHT_DIFF),
         distance.cent = SHOT_DIST - mean(SHOT_DIST), 
         shot.clock.late = SHOT_CLOCK < 4, 
         shot.clock.early = SHOT_CLOCK > 22)

##Save data set with all shots for later work
df.games <- df.all


##Summarise data
df.all %>% 
  group_by(PTS_TYPE, LOCATION) %>%
  summarise(mean.shot = mean(FGM))

table(df.all$def.type)
table(df.all$Dribble.type)

## Only players with at least 25 shots 
n.players <- df.all %>% group_by(PLAYER_NAME) %>% summarise(n.shots = n()) %>% filter (n.shots >=25)
df.all <- filter(df.all, PLAYER_NAME %in% n.players$PLAYER_NAME)


## Training and test data 
set.seed(100)
N <- nrow(df.all)
train.set <- (rbinom(N, 1, prob = 0.9) == 1)
test.set <- (!train.set)


########################################
##glmer: Mixed models with shooter specific intercept
########################################

#Model with location
glm.fit1 <- glmer(as.factor(FGM) ~ LOCATION + poly(distance.cent, 2) + shot.clock.early +
                    shot.clock.late + def.type + Dribble.type + heightdiff.cent + age.cent +
                    (1|PLAYER_NAME), 
                  data = df.all[train.set,], family = "binomial", 
                 control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(glm.fit1)

## Model wo location
glm.fit2 <- glmer(as.factor(FGM) ~  poly(distance.cent, 2) + shot.clock.early +
                    shot.clock.late + def.type + Dribble.type + heightdiff.cent + age.cent +
                    (1|PLAYER_NAME), 
                  data = df.all[train.set,], family = "binomial", 
                  control=glmerControl(optCtrl=list(maxfun=2e4)))
summary(glm.fit2)


anova(glm.fit1, glm.fit2)

##Predictions from mixed model
predictions1 <- predict(glm.fit1, df.all, type = 'response', allow.new.levels = TRUE)
predictions2 <- predict(glm.fit2, df.all, type = 'response', allow.new.levels = TRUE)
outcome <- df.all$FGM

LogLoss<-function(actual, predicted)
{
  result<- -1/length(actual)*(sum((actual*log(predicted)+(1-actual)*log(1-predicted))))
  return(result)
}
LogLoss(outcome[test.set], predictions1[test.set])
LogLoss(outcome[test.set], predictions2[test.set])
## Small log loss improvement in full model (predictions1)


predictions <- predict(glm.fit1, df.all, type = 'response', allow.new.levels = TRUE)
preds <- prediction(predictions[test.set], df.all$FGM[test.set])
performance(preds, "auc")

predictions <- predict(glm.fit2, df.all, type = 'response', allow.new.levels = TRUE)
preds <- prediction(predictions[test.set], df.all$FGM[test.set])
performance(preds, "auc")
## Higher AUC in full model (glm.fit1)

########################################
##glmnet: using elastic net with alpha 0.5
########################################

#Without location
sparseX <- sparse.model.matrix(~  + (1 + distance.cent) * 
                (1 + shot.clock.early + shot.clock.late + def.type + Dribble.type + 
                 heightdiff.cent + age.cent + PLAYER_NAME), df.all)

m1 <- cv.glmnet(sparseX[train.set,],
                df.all$FGM[train.set],
                alpha = 0.5,
                family = 'binomial')

df.all$sparse.hat <- predict(m1, newx = sparseX, type = 'response')[,1]
preds <- prediction(df.all$sparse.hat[test.set], df.all$FGM[test.set])
perf <- performance(preds, 'tpr', 'fpr')
plot(perf)
performance(preds, 'auc')


#With location
sparseX <- sparse.model.matrix(~  + (1 + distance.cent) * 
                                 (1 + shot.clock.early + shot.clock.late + def.type + Dribble.type + 
                                    heightdiff.cent + age.cent + LOCATION + PLAYER_NAME), df.all)

m1 <- cv.glmnet(sparseX[train.set,],
                df.all$FGM[train.set],
                alpha = 0.5,
                family = 'binomial')

df.all$sparse.hat <- predict(m1, newx = sparseX, type = 'response')[,1]
preds <- prediction(df.all$sparse.hat[test.set], df.all$FGM[test.set])
perf <- performance(preds, 'tpr', 'fpr')
plot(perf)
performance(preds, 'auc')


## Practical significance
set.seed(100)
unique.games <- unique(df.all$MATCHUP)
games <- sample(unique.games, 100)
sample.games <- filter(df.all, MATCHUP %in% games)

##Imagine all 20 teams were away teams
sample.games$LOCATION <- "A"
sample.games$p.hatA <- predict(glm.fit1, sample.games, type = 'response', allow.new.levels = TRUE)

##Imagine all 20 teams were home teams
sample.games$LOCATION <- "H"
sample.games$p.hatH <- predict(glm.fit1, sample.games, type = 'response', allow.new.levels = TRUE)

#Points scored (predicted)
sample.games <- sample.games %>%
  mutate(pts.hatA = p.hatA *PTS_TYPE, pts.hatH = p.hatH*PTS_TYPE)

##Total points per game
sample <- sample.games %>%
  group_by(MATCHUP) %>%
  summarise(total.hatA = sum(pts.hatA), total.hatH = sum(pts.hatH), diff.pts = total.hatH - total.hatA)


histogram(sample$diff.pts)
quantile(sample$diff.pts, c(0.025, 0.975))


