# see if we can create fighter statistics that could predict fight outcomes.
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

source('get_UFCData.R')

avg_blue_stats = aggregate(select(totalRounds,contains('B__')), 
                           by = select(totalRounds,contains('B_Name')), 
                           FUN = 'mean', na.rm = T )
avg_red_stats = aggregate(select(totalRounds,contains('R__')), 
                          by = select(totalRounds,contains('R_Name')), 
                          FUN = 'mean',  na.rm = T )

colnames(avg_red_stats) = gsub('R_','B_', colnames(avg_red_stats))
all_avg_stats = rbind(avg_blue_stats,avg_red_stats)

all_stats = aggregate(select(all_avg_stats,contains('B__')), by = select(all_avg_stats,contains('B_Name')), FUN = 'mean',  na.rm = T )
colnames(all_stats) = gsub('B_','',colnames(all_stats))

sapply(all_stats, function(x) length(which(!is.na(x)))/nrow(all_stats))

# create fight data frame with fight stats
fightNames = UFCDat %>% select(B_Name, R_Name)
fightNames$B_Name = as.character(fightNames$B_Name)
fightNames$R_Name = as.character(fightNames$R_Name)

blue_fights = merge(fightNames, all_stats, by.x = c('B_Name'), by.y = c('Name'))
colnames(blue_fights)[3:ncol(blue_fights)] = paste0('B_Avg', colnames(blue_fights)[3:ncol(blue_fights)])#gsub('X','B_Avg', colnames(blue_fights))

red_fights = merge(fightNames, all_stats, by.x = c('R_Name'), by.y = c('Name'))
colnames(red_fights)[3:ncol(red_fights)] = paste0('R_Avg', colnames(red_fights)[3:ncol(red_fights)])#gsub('X','R_Avg', colnames(red_fights))

all_fights = cbind(blue_fights, red_fights[,3:ncol(red_fights)], 
                    R_Heavier = randEffects$R_Heavier,
                    R_Younger = randEffects$R_Younger,
                    R_Age = randEffects$R_Age,
                    B_Age = randEffects$B_Age,
                    AgeDifference = randEffects$AgeDifference,
                    WeightDifference =randEffects$WeightDifference,
                    Winner = randEffects$winner,
                    Winby = randEffects$winby)

# create percentage features (landed/attempted)
names = colnames(all_fights)
fight_metric_percentages = data.frame(matrix(nrow = nrow(all_fights)))
j = 1

for (i in 3:length(names)){
  if (grepl('Attempts',names[i]) & grepl('Landed',names[i+1])) {
    if (unlist(strsplit(names[i], 'Attempts')) == unlist(strsplit(names[i+1], 'Landed')) ) {
      fight_metric_percentages[,j] = all_fights[,i+1] / all_fights[,i] 
      colnames(fight_metric_percentages)[j] = paste0(unlist(strsplit(names[i], 'Attempts')),'Percentage')
      j = j + 1
    }
  }
}

all_fight_precentages = cbind(all_fights, fight_metric_percentages)
