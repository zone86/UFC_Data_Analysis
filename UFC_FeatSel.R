# Preliminary analysis of 2013 UFC data. Conduct simple exploratory data analysis.  
library(dplyr)
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

ageDF %>% ggplot(., aes(ID,X)) + geom_boxplot()

median(ageDF$X[ageDF$ID=='Win']) # the median age of winners in the UFC for 2013

fisher.test(table(randEffects$R_Younger, randEffects$winner))

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

###### GLM model fitting ######
# create glm with age and being heavier 
summary(glm(randEffects$B_winner ~ randEffects$B_Age + randEffects$R_Age + randEffects$R_Younger + randEffects$R_Heavier, data = randEffects))
summary(glm(randEffects$R_winner ~ randEffects$R_Younger + randEffects$R_Age + randEffects$R_Heavier, data = randEffects))

