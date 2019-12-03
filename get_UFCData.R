
library(dplyr)
UFCDat = read.csv('data.csv')

# look for missing data
missingProp = sapply(UFCDat, function(x) length(which(!is.na(x)))/nrow(UFCDat) )

# only keep data that we have at least 30% of
UFCDat.1 = UFCDat[,missingProp > .3]

# seperate fixed and random effects
randEffects = UFCDat.1 %>% select(B_Name,B_ID,B_Age,B_Height,B_Weight,BPrev,BStreak,B_HomeTown,B_Location,
                                  R_Name,R_ID,R_Age,R_Height,R_Weight,RPrev,R_HomeTown,R_Location,winner,winby)

fixEffects =  select(UFCDat.1, contains('Round')) 
fixEffects = mutate(fixEffects, Winner = UFCDat.1$winner, WinBy = UFCDat.1$winby)

# create new features from random effects
randEffects$R_Younger = randEffects$R_Age < randEffects$B_Age 

randEffects$B_BMI = randEffects$B_Weight/(randEffects$B_Height/100)^2
randEffects$R_BMI = randEffects$R_Weight/(randEffects$R_Height/100)^2

randEffects$R_Taller = randEffects$R_Height > randEffects$B_Height
randEffects$B_Taller = randEffects$B_Height > randEffects$R_Height

randEffects$R_Heavier = randEffects$R_Weight > randEffects$B_Weight
randEffects$B_Heavier = randEffects$B_Weight > randEffects$R_Weight

randEffects$R_HigherDense = randEffects$R_BMI > randEffects$B_BMI
randEffects$B_HigherDense = randEffects$B_BMI > randEffects$R_BMI

randEffects$WeightDifference = abs(randEffects$R_Weight - randEffects$B_Weight)
randEffects$HeightDifference = abs(randEffects$R_Height - randEffects$B_Height)
randEffects$AgeDifference = abs(randEffects$R_Age - randEffects$B_Age)

# create red and blue winner colunms
randEffects$B_winner = 0
randEffects$B_winner[randEffects$winner=="blue"] = 1
randEffects$R_winner = 0
randEffects$R_winner[randEffects$winner=="red"] = 1

# create new feature for if the red fighter was heavier   
UFCDat.1$R_Heavier = UFCDat.1$R_Weight > UFCDat.1$B_Weight
UFCDat.1$B_Heavier = UFCDat.1$B_Weight > UFCDat.1$R_Weight

# create cumulative round total scores
round1 = select(fixEffects, contains("Round1"))
round2 = select(fixEffects, contains("Round2"))
round3 = select(fixEffects, contains("Round3"))

totalRounds = data.frame(matrix(nrow = nrow(round1), ncol = ncol(round1)))
colnames(totalRounds) = colnames(round1)

for (i in 1:ncol(round1)){
  totalRounds[,i] = round1[,i] + round2[,i] + round3[,i]
}

colnames(totalRounds) = gsub("Round1","Total",colnames(totalRounds))

totalRounds = mutate(totalRounds, R_Name = randEffects$R_Name, B_Name =randEffects$B_Name, R_ID = randEffects$R_ID, B_ID =randEffects$B_ID, Winner = randEffects$winner)

redRounds = totalRounds %>% select(contains('R_'))
blueRounds = totalRounds %>% select(contains('B_'))

# create variables which show the differences in fight metrics between the fighters for each fight
roundDiffs = data.frame(matrix(nrow = nrow(blueRounds), ncol = ncol(blueRounds)))
colnames(roundDiffs) = colnames(blueRounds)
colnames(roundDiffs) = gsub("B_","Diff",colnames(roundDiffs))

for (i in 1:ncol(blueRounds)){
  roundDiffs[,i] = as.numeric(redRounds[,i]) - as.numeric(blueRounds[,i])
}

roundDiffs = mutate(roundDiffs, R_Name = randEffects$R_Name, B_Name =randEffects$B_Name, R_ID = randEffects$R_ID, B_ID =randEffects$B_ID, Winner = fixEffects$Winner)
