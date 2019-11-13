# Preliminary analysis of 2013 UFC data. Conduct simple exploratory data analysis.  
# Questions we could ask about the data:
# what is the most likely outcome (i.e. winby) given the round by round data?

# what round by round features increase the likelihood of a win?
# what are the random effects (i.e. age, BMI) that increase the likelihood of a win? age, being heavier
# what round by round and random effects (age, being the heavier opponent) increase the likelihood of a win?

# what round is the fight most likely to end given the frequency of specific attacks/defenses?

library(dplyr)
library(mlr)
library(ggplot2)

source('get_UFCData.R')
########################################################################################################################################
# exploratory data analyais: group age, weight, height and look at their relationship to winning
winFun = function(red,blue,winner){
  blueWin = ifelse(winner == 'blue', blue, NA)
  redWin = ifelse(winner == 'red', red, NA)
  win = combine(blueWin,redWin)
  win = data.frame(X = win[!is.na(win)])
  win$ID = "Win"
  win
}

lossFun = function(red,blue,winner){
  blueLoss = ifelse(winner == 'blue', red, NA)
  redLoss = ifelse(winner == 'red', blue, NA)
  loss = combine(blueLoss,redLoss)
  loss = data.frame(X = loss[!is.na(loss)])
  loss$ID = "Loss"
  loss
}

winAge = winFun(randEffects$R_Age, randEffects$B_Age, randEffects$winner)
lossAge = lossFun(randEffects$R_Age, randEffects$B_Age, randEffects$winner)

ageDF = rbind(winAge,lossAge)

t.test(ageDF$X ~ ageDF$ID, ageDF) # age is a significant feature! younger fighters do better against older fighters. 31 could be prime fighting age...

ageDF %>% ggplot(., aes(ID,Age)) + geom_boxplot()


# was the winner taller, heavier, more dense than the loser?
# taller ... 
winTaller = winFun(randEffects$R_Taller, randEffects$B_Taller, randEffects$winner)
lossTaller = lossFun(randEffects$R_Taller, randEffects$B_Taller, randEffects$winner)

tallerDF = rbind(winTaller,lossTaller)

tallerTable = table(tallerDF)

chisq.test(tallerTable) # no effect of being taller

# heavier...

winHeavier = winFun(randEffects$R_Heavier, randEffects$B_Heavier, randEffects$winner)
lossHeavier = lossFun(randEffects$R_Heavier, randEffects$B_Heavier, randEffects$winner)

HeavierDF = rbind(winHeavier,lossHeavier)

HeavierTable = table(HeavierDF)

chisq.test(HeavierTable) # being heavier matters!

# density (BMI)... 
winHigherDense = winFun(randEffects$R_HigherDense, randEffects$B_HigherDense, randEffects$winner)
lossHigherDense = lossFun(randEffects$R_HigherDense, randEffects$B_HigherDense, randEffects$winner)

DenseDF = rbind(winHigherDense,lossHigherDense)

DenseTable = table(DenseDF)

chisq.test(DenseTable) # no effect of BMI

# is there a hometown advantage?
randEffects$B_HomeTown = as.character(randEffects$B_HomeTown)
randEffects$B_Location = as.character(randEffects$B_Location)

grepl(randEffects$B_HomeTown, randEffects$B_Location)


###### GLM model fitting ######
# create glm with age and being heavier 
allDatCleaned.1 = na.omit(allDatCleaned)
allDatCleaned.1.Red = allDatCleaned.1 %>% select(contains('R_')) %>% 
  select(-R_Name, -R_HomeTown, -R_Location, -R_Height, -R_Weight, -R_BMI)
allDatCleaned.1.Blue = allDatCleaned.1 %>% select(contains('B_')) %>% 
  select(-B_Name, -B_HomeTown, -B_Location, -B_Height, -B_Weight, -B_BMI)

summary(glm(randEffects$B_winner ~ randEffects$B_Age + randEffects$B_Heavier, data = randEffects))
summary(glm(randEffects$B_winner ~ randEffects$B_Age + randEffects$B_Heavier + randEffects$B_Taller, data = randEffects))
summary(glm(randEffects$R_winner ~ randEffects$R_Age + randEffects$R_Heavier + randEffects$R_Taller, data = randEffects))


###### MLR #########
# see what type of model performance can be acheived with
all_target = allDatCleaned$winner
all_data = allDatCleaned %>% 
  select(winner, B_Heavier, B_Age, B_Taller, R_Age, 
         B__Total_Strikes_Total.Strikes_Landed, R__Total_Strikes_Total.Strikes_Landed)
         
blue_target = allDatCleaned$B_winner
blue_data = allDatCleaned %>% select(contains("B_")) %>% 
  select(-B_Name, -B_HomeTown, -B_Location, -B_Height, -B_Weight, -B_BMI) %>%
  select(B_winner, B_Heavier, B_Age#, #B_Taller,
# B__Total_Strikes_Body.Total.Strikes_Attempts,   B__Total_Strikes_Clinch.Head.Strikes_Attempts,
# B__Total_Strikes_Distance.Body.Kicks_Attempts , B__Total_Strikes_Distance.Body.Punches_Landed,
# B__Total_Strikes_Distance.Strikes_Attempts     ,B__Total_Strikes_Ground.Total.Strikes_Attempts,
# B__Total_Strikes_Head.Total.Strikes_Attempts  , B__Total_Strikes_Punches_Attempts,
#B__Total_Strikes_Total.Strikes_Landed
# , B__Total_TIP_Back.Control.Time,
# B__Total_TIP_Clinch.Time                      , B__Total_TIP_Guard.Control.Time,
# B__Total_TIP_Misc..Ground.Control.Time        , B__Total_TIP_Mount.Control.Time
)

blue_data$B_winner = factor(blue_data$B_winner, levels = c(0,1))
blue_data$B_Heavier = factor(blue_data$B_Heavier, levels = c('FALSE','TRUE'))
blue_data$B_Taller = factor(blue_data$B_Taller, levels = c('FALSE','TRUE'))
blue_data$B_HigherDense = factor(blue_data$B_HigherDense, levels = c('FALSE','TRUE'))

blue_data = na.omit(blue_data)

blue_task = makeClassifTask(target = 'B_winner', data = blue_data)
blue_task = normalizeFeatures(obj = blue_task, method = 'standardize',cols= c('B_Age'))
ho = makeResampleInstance( "Holdout", blue_task)
task.train = subsetTask(blue_task,ho$train.inds[[1]])
task.test = subsetTask(blue_task,ho$test.inds[[1]])

set.seed(1)
cv = makeResampleDesc("CV", iters = 3)

log.Mod = makeLearner('classif.logreg', predict.type = "prob")
multinom.Mod = makeLearner('classif.multinom', predict.type = "prob")
cvglmnet.Mod = makeLearner('classif.cvglmnet', predict.type = "prob", par.vals = list(alpha = 0))
rpart.Mod = makeLearner("classif.rpart", predict.type = "prob")
RF.Mod = makeLearner('classif.randomForest', predict.type = "prob")

logMod.CV = resample(log.Mod, blue_task, cv, measures = list(acc,auc,f1), extract = function(x) getLearnerModel(x)$coefficients)
multinom.CV = resample(multinom.Mod, blue_task, cv, measures = list(acc,auc,f1), extract = function(x) getLearnerModel(x))
glmnet.CV = resample(cvglmnet.Mod, blue_task, cv, measures = list(acc,auc,f1), extract = function(x) getLearnerModel(x))
RF.CV = resample(RF.Mod, blue_task, cv, measures = list(acc,auc,f1), extract = function(x) getLearnerModel(x))
rpart.CV = resample(rpart.Mod, blue_task, cv, measures = list(acc,auc,f1), extract = function(x) getLearnerModel(x))

logMod.CV$extract
multinom.CV$extract
glmnet.CV$extract

getRRPredictions(logMod.CV)

log.train = train(log.Mod, task.train)
log.test = predict(log.train, task.test)
performance(log.test, measures = list(acc,auc,f1))

multinom.train = train(multinom.Mod, task.train)
multinom.test = predict(multinom.train, task.test)
performance(multinom.test, measures = list(acc,auc,f1))


cvglmnet.train = train(cvglmnet.Mod, task.train)
cvglmnet.test = predict(cvglmnet.train, task.test)
performance(cvglmnet.test, measures = list(acc,auc,f1))

set.seed(1)
RF.train = train(RF.Mod, task.train)
RF.test = predict(RF.train, task.test)
performance(RF.test, measures = list(acc,auc,f1))

set.seed(1)
rpart.train = train(rpart.Mod, task.train)
rpart.test = predict(rpart.train, task.test)
performance(rpart.test, measures = list(acc,auc,f1))


C50.Mod = makeLearner("classif.C50", predict.type = "prob")
C50.train = train(C50.Mod, task.train)
C50.test = predict(C50.train, task.test)
performance(C50.test, measures = acc)

cforest.Mod = makeLearner("classif.cforest", predict.type = "prob")


featImp = generateFilterValuesData(task=blue_task,method='randomForest_importance')
plotFilterValues(featImp)

### RED target
red_target = allDatCleaned$R_winner
red_data = allDatCleaned %>% select(contains("R_")) %>% 
  select(-R_Name, -R_HomeTown, -R_Location, -R_Height, -R_Weight, -R_BMI)

red_data$R_winner = factor(red_data$R_winner, levels = c(0,1))
red_data$R_Heavier = factor(red_data$R_Heavier, levels = c('FALSE','TRUE'))
red_data$R_Taller = factor(red_data$R_Taller, levels = c('FALSE','TRUE'))
red_data$R_Heavier = factor(red_data$R_Heavier, levels = c('FALSE','TRUE'))
red_data$R_HigherDense = factor(red_data$R_HigherDense, levels = c('FALSE','TRUE'))

red_data = na.omit(red_data)

red_task = makeClassifTask(target = 'R_winner', data = red_data)

ho = makeResampleInstance ("Holdout", red_task)
task.train = subsetTask(red_task,ho$train.inds[[1]])
task.test = subsetTask(red_task,ho$test.inds[[1]])
red_task = normalizeFeatures(obj = red_task, method = 'standardize',cols='R_Age')

# fixed effects

fixEffects$B_winner = 0
fixEffects$B_winner[fixEffects$Winner=="blue"] = 1
fixEffects$R_winner = 0
fixEffects$R_winner[fixEffects$Winner=="red"] = 1

### fixed effects feature selection
red_target = fixEffects$R_winner
fixEffects$R_winner = factor(fixEffects$R_winner, levels = c(0,1))

fixEffects_R = select(fixEffects, contains('R_')) 
fixEffects_R = na.omit(fixEffects_R)

red_task = makeClassifTask(target = 'R_winner', data = fixEffects_R)
ho = makeResampleInstance ("Holdout", red_task)
task.train = subsetTask(red_task,ho$train.inds[[1]])
task.test = subsetTask(red_task,ho$test.inds[[1]])

cv = makeResampleDesc("CV", iters = 3)
ctrl = makeFeatSelControlSequential(method='sbs', maxit=50, max.features=20)

red.feats = selectFeatures(rpart.Mod, red_task, measures = auc, resampling = cv, control = ctrl)
red.feats
# [1] "R__Total_Grappling_Takedowns_Landed"                  "R__Total_Strikes_Clinch.Total.Strikes_Attempts"
# [3] "R__Total_Strikes_Clinch.Total.Strikes_Landed"         "R__Total_Strikes_Distance.Body.Punches_Attempts"
# [5] "R__Total_Strikes_Distance.Body.Strikes_Landed"        "R__Total_Strikes_Distance.Head.Kicks_Landed"
# [7] "R__Total_Strikes_Distance.Head.Strikes_Landed"        "R__Total_Strikes_Distance.Strikes_Landed"
# [9] "R__Total_Strikes_Ground.Head.Strikes_Attempts"        "R__Total_Strikes_Ground.Head.Strikes_Landed"
# [11] "R__Total_Strikes_Ground.Significant.Strikes_Attempts" "R__Total_Strikes_Head.Significant.Strikes_Landed"
# [13] "R__Total_Strikes_Head.Total.Strikes_Landed"           "R__Total_Strikes_Significant.Strikes_Landed"
# [15] "R__Total_Strikes_Total.Strikes_Landed"                "R__Total_TIP_Ground.Time"
# [17] "R__Total_TIP_Half.Guard.Control.Time"                 "R__Total_TIP_Standing.Time"
# [19] "R_Age"

red.feats = c("R__Total_Grappling_Takedowns_Landed"                  ,"R__Total_Strikes_Clinch.Total.Strikes_Attempts",
"R__Total_Strikes_Clinch.Total.Strikes_Landed"        , "R__Total_Strikes_Distance.Body.Punches_Attempts",
"R__Total_Strikes_Distance.Body.Strikes_Landed"       , "R__Total_Strikes_Distance.Head.Kicks_Landed",
"R__Total_Strikes_Distance.Head.Strikes_Landed"       , "R__Total_Strikes_Distance.Strikes_Landed",
"R__Total_Strikes_Ground.Head.Strikes_Attempts"       , "R__Total_Strikes_Ground.Head.Strikes_Landed",
"R__Total_Strikes_Ground.Significant.Strikes_Attempts", "R__Total_Strikes_Head.Significant.Strikes_Landed",
 "R__Total_Strikes_Head.Total.Strikes_Landed"         ,  "R__Total_Strikes_Significant.Strikes_Landed",
"R__Total_Strikes_Total.Strikes_Landed"               , "R__Total_TIP_Ground.Time",
"R__Total_TIP_Half.Guard.Control.Time"                , "R__Total_TIP_Standing.Time",
 "R_Age")


# BLUE WIN FEATS
blue_target = allDatCleaned$B_winner
blue_data = allDatCleaned %>% select(contains("B_")) %>% 
  select(-B_Name, -B_HomeTown, -B_Location, -B_Height, -B_Weight, -B_BMI)

blue_data$B_winner = factor(blue_data$B_winner, levels = c(0,1))
blue_data$B_Heavier = factor(blue_data$B_Heavier, levels = c('FALSE','TRUE'))
blue_data$B_Taller = factor(blue_data$B_Taller, levels = c('FALSE','TRUE'))
blue_data$B_Heavier = factor(blue_data$B_Heavier, levels = c('FALSE','TRUE'))
blue_data$B_HigherDense = factor(blue_data$B_HigherDense, levels = c('FALSE','TRUE'))

blue_data = na.omit(blue_data)

blue_task = makeClassifTask(target = 'B_winner', data = blue_data)

ho = makeResampleInstance ("Holdout", blue_task)
task.train = subsetTask(blue_task,ho$train.inds[[1]])
task.test = subsetTask(blue_task,ho$test.inds[[1]])
blue_task = normalizeFeatures(obj = blue_task, method = 'standardize',cols='B_Age')

feats = selectFeatures(rpart.Mod, blue_task, measures = auc, resampling = cv, control = ctrl)
# [1]"B__Total_Strikes_Body.Total.Strikes_Attempts"   "B__Total_Strikes_Clinch.Head.Strikes_Attempts" 
# [3] "B__Total_Strikes_Distance.Body.Kicks_Attempts"  "B__Total_Strikes_Distance.Body.Punches_Landed" 
# [5] "B__Total_Strikes_Distance.Strikes_Attempts"     "B__Total_Strikes_Ground.Total.Strikes_Attempts"
# [7] "B__Total_Strikes_Head.Total.Strikes_Attempts"   "B__Total_Strikes_Punches_Attempts"             
# [9] "B__Total_Strikes_Total.Strikes_Landed"          "B__Total_TIP_Back.Control.Time"                
# [11] "B__Total_TIP_Clinch.Time"                       "B__Total_TIP_Guard.Control.Time"               
# [13] "B__Total_TIP_Misc..Ground.Control.Time"         "B__Total_TIP_Mount.Control.Time"               
# [15] "B_Age" 

blue.feats =c("B__Total_Strikes_Body.Total.Strikes_Attempts",   "B__Total_Strikes_Clinch.Head.Strikes_Attempts",
"B__Total_Strikes_Distance.Body.Kicks_Attempts" , "B__Total_Strikes_Distance.Body.Punches_Landed",
"B__Total_Strikes_Distance.Strikes_Attempts"     ,"B__Total_Strikes_Ground.Total.Strikes_Attempts",
"B__Total_Strikes_Head.Total.Strikes_Attempts"  , "B__Total_Strikes_Punches_Attempts",
"B__Total_Strikes_Total.Strikes_Landed"         , "B__Total_TIP_Back.Control.Time",
"B__Total_TIP_Clinch.Time"                      , "B__Total_TIP_Guard.Control.Time",
 "B__Total_TIP_Misc..Ground.Control.Time"        , "B__Total_TIP_Mount.Control.Time",
 "B_Age")

blue.feats = gsub('B_','',blue.feats)
red.feats = gsub('R_','',red.feats)
intersect(blue.feats, red.feats)
