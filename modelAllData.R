library(ggplot2)
library(dplyr)
library(mlr)
library(mlrMBO)
library(rJava)
library(parallelMap)
library(parallel)
library(BatchJobs)

# retireves data from getUFCData and also creates fighter statistcs such as striking/takedown/etc.. percentages
source('createFighterStats.R') 

###############################################################################################
allDatCleaned = all_fight_precentages

# proportion of missing data
cols = names(sapply(allDatCleaned, function(x) length(which(!is.na(x)))/nrow(allDatCleaned) )[sapply(allDatCleaned, function(x) length(which(!is.na(x)))/nrow(allDatCleaned) )>.4])

# select all relevent fight data
all_data = allDatCleaned %>%
  select(.,contains('Percentage'), R_Age, B_Age, AgeDifference, R_Heavier, R_Younger, Winner)

# remove no contests and draws as they are extremely rare
all_data = all_data[!(all_data$Winner %in% c('no contest','draw')),]

# encode logicals as factors
all_data$Winner = factor(all_data$Winner, levels = c('red', 'blue'))
all_data$R_Heavier = factor(all_data$R_Heavier, levels = c('FALSE','TRUE'))
all_data$R_Younger = factor(all_data$R_Younger, levels = c('FALSE','TRUE'))

# find all NaN's and replace with NA
idx = sapply(all_data, function(x) is.nan(x))
all_data[idx] <- NA

###############################################################################################################
#####    Feature selection and model selection and validation with MLR      ###################################
###############################################################################################################
# feature selection based on feature filtering, followed by backward elimination
featSel_task = makeClassifTask(target = 'Winner', data = all_data)

# if you want to standardize the data, otherwise skip
featSel_task = normalizeFeatures(obj = featSel_task, method = 'standardize',
                             cols = colnames(all_data)[- which(sapply(all_data,is.factor))] ) 

filtered_feats = list(Result = data.frame(matrix(nrow = 30, ncol = 2)))

parallelStartSocket(cpus = 3, level = "mlr.selectFeatures")

for (x in 1:5) {
  
  set.seed(x)
  filter.feats = generateFilterValuesData(featSel_task, 
                                           method = c('randomForestSRC_importance'), 
                                           na.action = 'na.impute')
  order = order(filter.feats$data$value, decreasing = T)
  dat = filter.feats$data[order,c(1,4)]
  filtered_feats[[x]] = dat

}

parallelStop()
gc()

# plot feature filtering
all_feats = bind_rows(filtered_feats) %>% 
  group_by(name) %>% summarise(Mean = mean(value)) %>% arrange(Mean)

ggplot(all_feats, aes(reorder(name,-Mean), Mean)) +
  geom_bar(stat = 'identity') + 
  xlab('Feature') +
  theme(axis.text.y = element_text(colour = ifelse(grepl('B_', all_feats$name[reorder(all_feats$name,-all_feats$Mean)]),'blue','red'))) +
  ggtitle('Feature filtering \n(Random Forest Importance) - \npredicting red fighter win') + coord_flip()


# according to a number of feature filtering methods, including: oneR and RF variable selection, 
# it looks as though the initial model created in FeatSel.R contained the most significant features
# (e.g. R_Heavier, R_Younger, R_Age, B_Age, and AgeDiff) 
# we may proceed to perform backward elimination on some propotion of the top n (e.g. 20) features according to the filering methods

feat.ctrl.bwrd = makeFeatSelControlSequential(method='sfbs',maxit=100L,max.features=20)
#feat.ctrl.fwrd = makeFeatSelControlSequential(method='sffs',maxit=100L,max.features=20)
top20.bwrd = list()
#top20.fwrd = list()

imp_feats = head(all_feats$name[order(all_feats$Mean, decreasing = T)],20)
imp_feats = append('Winner', imp_feats)
new_data = all_data[,colnames(all_data) %in% imp_feats]

featSel_task_2 = makeClassifTask(target = 'Winner', data = new_data)

# if you want to standardize the data, otherwise skip
featSel_task_2 = normalizeFeatures(obj = featSel_task_2, method = 'standardize',
                                 cols = colnames(new_data)[- which(sapply(new_data,is.factor))] ) 


parallelStartSocket(cpus = 3, level = "mlr.selectFeatures")

#for(x in 1:2){
  set.seed(1)
  bwrd.feats = selectFeatures(makeLearner('classif.randomForestSRC', predict.type = "prob"), 
                              featSel_task_2, cv3, mlr::brier, control = feat.ctrl.bwrd)
  
  top20.bwrd[[x]] = list(feats = bwrd.feats$x, perf = bwrd.feats$y)
  
  # with set seed = 1
  # [1] "B_Avg_Total_Strikes_Distance.Leg.Strikes_Percentage" 
  # [2] "R_Avg_Total_Strikes_Distance.Head.Kicks_Percentage"  
  # [3] "R_Avg_Total_Strikes_Distance.Head.Strikes_Percentage"
  # [4] "R_Avg_Total_Strikes_Ground.Leg.Strikes_Percentage"   
  # [5] "R_Age"                                               
  # [6] "B_Age"
  
  # fwrd.feats = selectFeatures(makeLearner('classif.randomForestSRC', predict.type = "prob"),
  #                             featSel_task_2, cv3, mlr::brier, control = feat.ctrl.fwrd)
  # 
  # top20.fwrd[[x]] = list(feats = fwrd.feats$x, perf = fwrd.feats$y)
  
#}

  parallelStop()
  gc()
  
##########################################################################################
# create models after feature selection
Df = allDatCleaned %>%
    #select(bwrd.feats$x, Winner)
    #select(head(all_feats$name[reorder(all_feats$name,-all_feats$Mean)],16), Winner)
  
     select(B_Age, R_Age, R_Younger, AgeDifference, R_Heavier,
            # R_Avg_Total_Strikes_Head.Total.Strikes_Percentage,
            # R_Avg_Total_Strikes_Head.Significant.Strikes_Percentage,
            # R_Avg_Total_Strikes_Total.Strikes_Percentage,
            # R_Avg_Total_Strikes_Significant.Strikes_Percentage,
            # R_Avg_Total_Strikes_Distance.Body.Punches_Percentage,
            
            # B_Avg_Total_Strikes_Significant.Strikes_Percentage,
            # B_Avg_Total_Strikes_Head.Total.Strikes_Percentage,
            # B_Avg_Total_Strikes_Head.Significant.Strikes_Percentage,
            # B_Avg_Total_Strikes_Total.Strikes_Percentage,
            # B_Avg_Total_Strikes_Distance.Leg.Kicks_Percentage,
            # B_Avg_Total_Strikes_Distance.Body.Kicks_Percentage,
            # B_Avg_Total_Strikes_Kicks_Percentage,
            # B_Avg_Total_Strikes_Punches_Percentage,
            # B_Avg_Total_Strikes_Clinch.Significant.Kicks_Percentage,
            # B_Avg_Total_Strikes_Distance.Head.Punches_Percentage,
            # B_Avg_Total_Strikes_Distance.Head.Kicks_Percentage,
            # B_Avg_Total_Strikes_Clinch.Significant.Kicks_Percentage,
            # B_Avg_Total_Strikes_Ground.Significant.Punches_Percentage,
             Winner) 

# remove no contests and draws as they are extremely rare
Df = Df[!(Df$Winner %in% c('no contest','draw')),]

# encode logicals as factors
Df$Winner = factor(Df$Winner, levels = c('blue','red'))
Df$R_Younger = factor(Df$R_Younger, levels = c('FALSE','TRUE'))
Df$R_Heavier = factor(Df$R_Heavier, levels = c('FALSE','TRUE'))

# find all NaN's and replace with NA
idx = sapply(Df, function(x) is.nan(x))
Df[idx] <- NA

# use Df1 if you choose to omit NAs...
Df1 = na.omit(Df)
task = makeClassifTask(target = 'Winner', data = Df1)

# if you want to standardize the data, otherwise skip
task = normalizeFeatures(obj = task, method = 'standardize',
                                 cols = colnames(Df1)[- which(sapply(Df1,is.factor))] ) 

log.Mod = makeLearner('classif.logreg', predict.type = "prob")
cvglmnet.Mod = makeLearner('classif.cvglmnet', predict.type = "prob", par.vals = list(alpha = 0)) # Ridge model
svm.Mod = makeLearner('classif.svm', predict.type = "prob")
nnet.Mod = makeLearner('classif.nnet', predict.type = "prob")

rpart.Mod = makeLearner("classif.rpart", predict.type = "prob")
RF.Mod = makeLearner('classif.randomForestSRC', predict.type = "prob")
cforest.Mod = makeLearner('classif.cforest', predict.type = "prob")

###########################################################################
# find the accuracy of models with 100 different random seeds 
# models that cannot handle missing values
logMod.rand = data.frame(matrix(nrow = 0, ncol = 3))

for (i in 1:20) {
  set.seed(i)
  
  logMod.CV = resample(log.Mod, task, cv3, measures = list(brier,auc,acc), extract = function(x) getLearnerModel(x)$coefficients)  
  logMod.rand[i,] = logMod.CV$aggr
}

mean(logMod.rand$X1) # Brier score
mean(logMod.rand$X2) # AUC
mean(logMod.rand$X3) # Accuracy

log.learn = generateLearningCurveData(learners=log.Mod,task=task, measures = list(brier,auc,acc))
plotLearningCurve(log.learn) + ggtitle('Logistic Regression learning curve with "simple" features')

# TODO: check residuals and model assumptions
#### #### #### #### #### #### #### #### #### BENCHMARK PERFORMANCE #### #### #### #### #### #### #### #### #### #### ####
# With just age of red and blue fighters, red fighter weight advantage, red fighter younger advantage, and age difference
# Brier score: 0.2349362 
# AUC: 0.607522
# Accuracy: 0.6057864
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 

glmnet.rand = data.frame(matrix(nrow = 0, ncol = 3))

for (i in 1:20) {
  set.seed(i)
  glmnet.CV = resample(cvglmnet.Mod, task, cv3, measures = list(brier,auc,acc), extract = function(x) getLearnerModel(x))
  glmnet.rand[i,] = glmnet.CV$aggr
}

mean(glmnet.rand$X1)
mean(glmnet.rand$X2)
mean(glmnet.rand$X3)

# Performance with just: age of red and blue fighters, red fighter weight advantage, red fighter younger advantage, & age difference
# Before tuning
# Brier score: 0.2380093
# AUC: 0.6008183
# Accuracy: 0.5910634

# After tuning  MBO alpha=0,nlambda=100,type.logistic=Newton (SAME AS ABOVE)
# Brier score: 0.2380093
# AUC: 0.6008183
# Accuracy: 0.5910634

glmnet.learn = generateLearningCurveData(learners=cvglmnet.Mod,task=task, measures = list(brier,auc,acc))
plotLearningCurve(glmnet.learn) + ggtitle('Ridge Regression learning curve with "simple" features')


nnet.rand = data.frame(matrix(nrow = 0, ncol = 3))

for (i in 1:20) {
  set.seed(i)
  nnet.CV = resample(nnet.Mod, task, cv3, measures = list(brier,auc,acc), extract = function(x) getLearnerModel(x))
  nnet.rand[i,] = nnet.CV$aggr
}

sapply(nnet.rand, function (x) mean(x))

nnet.learn = generateLearningCurveData(learners=nnet.Mod,task=task, measures = list(brier,auc,acc))
plotLearningCurve(nnet.learn) + ggtitle('Neural Net learning curve with "simple" features')


# Performance with just: age of red and blue fighters, red fighter weight advantage, red fighter younger advantage, & age difference
# before tune
#    brier     auc       acc
# 0.2393581 0.5940696 0.5919571  

# after tune MBO: size=3,maxit=37,rang=0.692,decay=0.417,MaxNWts=1719
#    brier     auc       acc
# 0.2352274 0.6058100 0.6031055


svm.rand = data.frame(matrix(nrow = 0, ncol = 3))

for (i in 1:20) {
  set.seed(i)
  svm.CV = resample(svm.Mod, task, cv3, measures = list(brier,auc,acc), extract = function(x) getLearnerModel(x))
  svm.rand[i,] = svm.CV$aggr
}

sapply(svm.rand, function (x) mean(x))

# before tuning
#    brier     auc       acc
# 0.2379307 0.5918651 0.6010500 
# after tuning
#    brier     auc       acc
# 0.2380089 0.5882084 0.6014522

######################################################################################
# models that sample randomly and can handle missing values 
RF.rand = data.frame(matrix(nrow = 0, ncol = 3))

parallelStartSocket(cpus = 3, level = "mlr.resample")
for (i in 1:20) {
  set.seed(i)
  RF.CV = resample(RF.Mod, task, cv3, measures = list(brier,auc,acc), extract = function(x) getLearnerModel(x))
  RF.rand[i,] = RF.CV$aggr
}

parallelStop()
gc()
sapply(RF.rand, function (x) mean(x))

# Basic features
# Before tuning
#    brier     auc       acc
# 0.2802850 0.5567941 0.5703977 
# after tuning # ntree=1647,bootstrap=by.root,mtry=1,nodesize=1,sampsize=1524,samptype=swr
#    brier     auc       acc
# 0.2362199 0.5976843 0.5967605

# With feature filtering and backward elimination (with random forsest) for feature selection 
# Before tuning
#    brier     auc       acc
# 0.2370440 0.5994191 0.5991657
# after tuning MBO ntree=1647,bootstrap=by.root,mtry=1,nodesize=1,sampsize=1524,samptype=swr
# 0.2356254 0.6043402 0.5994297
# after tuning grid ntree=1750,bootstrap=by.root,mtry=2,nodesize=8,sampsize=1450,samptype=swr
# 0.2357247 0.6036547 0.6017571

rpart.rand = data.frame(matrix(nrow = 0, ncol = 3))

for (i in 1:20) {
  set.seed(i)
  rpart.CV = resample(rpart.Mod, task, cv3, measures = list(brier,auc,acc), extract = function(x) getLearnerModel(x))
  rpart.rand[i,] = rpart.CV$aggr
}

sapply(rpart.rand, function (x) mean(x))

# Before tuning
#    brier     auc       acc
# 0.2410201 0.5602524 0.5939455 
# After tuning
#    brier     auc       acc

# With feature filtering and backward elimination (with random forsest) for feature selection 
# Before tuning
#    brier     auc       acc
# 0.2413386 0.5599256 0.5937874

parallelStartSocket(cpus = 3, level = "mlr.resample")
cforest.rand = data.frame(matrix(nrow = 0, ncol = 3))

for (i in 1:20) {
  set.seed(i)
  cforest.CV = resample(cforest.Mod, task, cv3, measures = list(brier,auc,acc), extract = function(x) getLearnerModel(x))
  cforest.rand[i,] = cforest.CV$aggr
}

parallelStop()
gc()

sapply(cforest.rand, function (x) mean(x))

# Before tuning
#    brier     auc       acc
# 0.2403353 0.5907048 0.5906495

# After tuning
#    brier     auc       acc


# If we only have access to the features: B_Age, R_Age, R_Younger, AgeDifference, and R_Heavier. 
# It seems as though the simple logistic regresion model will produce the best average performance 
# overall (Brier: .235, AUC: .608, Acc: .606). 

#############################################################################################
# model optimization and hyperparameter tuning
# control settings for MBO
ctrl = makeMBOControl()
ctrl = setMBOControlTermination(ctrl, iters = 30)
ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI(),
                           opt = "focussearch", opt.focussearch.points = 300L)
tune.ctrl = makeTuneControlMBO(mbo.control = ctrl)
tune.ctrl.grid = makeTuneControlGrid(resolution = 3L) 
##############################################################################################
# declare hyperparam search space for each of the models
# that cant handle missing values...
glmnet.ps = makeParamSet( makeNumericParam(id = "alpha", lower = 0, upper = .01),
                          #makeNumericParam(id = "s", lower = 0, upper = 1),
                          #makeLogicalLearnerParam(id = "exact", default = FALSE, when = "predict"),
                          makeIntegerParam(id = "nlambda", lower = 100L, upper = 200L),
                          # makeNumericLearnerParam(id = "lambda.min.ratio", lower = 0, upper = 1),
                          # #makeNumericVectorParam(id = "lambda", lower = 0, ),
                          # makeLogicalLearnerParam(id = "standardize", default = TRUE),
                          # makeLogicalLearnerParam(id = "intercept", default = TRUE),
                          # makeNumericLearnerParam(id = "thresh", default = 1e-07, lower = 0),
                          # makeIntegerLearnerParam(id = "dfmax", lower = 0L),
                          # makeIntegerLearnerParam(id = "pmax", lower = 0L),
                          # makeIntegerVectorLearnerParam(id = "exclude", lower = 1L),
                          # makeNumericVectorLearnerParam(id = "penalty.factor", lower = 0, upper = 1),
                          # makeNumericVectorLearnerParam(id = "lower.limits", upper = 0),
                          # makeNumericVectorLearnerParam(id = "upper.limits", lower = 0),
                          # makeIntegerLearnerParam(id = "maxit", default = 100000L, lower = 1L),
                          makeDiscreteParam(id = "type.logistic", values = c("Newton", "modified.Newton"))
                          # makeDiscreteLearnerParam(id = "type.multinomial", values = c("ungrouped", "grouped")),
                          # makeNumericLearnerParam(id = "fdev", default = 1.0e-5, lower = 0, upper = 1),
                          # makeNumericLearnerParam(id = "devmax", default = 0.999, lower = 0, upper = 1),
                          # makeNumericLearnerParam(id = "eps", default = 1.0e-6, lower = 0, upper = 1),
                          # makeNumericLearnerParam(id = "big", default = 9.9e35),
                          # makeIntegerLearnerParam(id = "mnlam", default = 5, lower = 1),
                          # makeNumericLearnerParam(id = "pmin", default = 1.0e-9, lower = 0, upper = 1),
                          # makeNumericLearnerParam(id = "exmx", default = 250.0),
                          # makeNumericLearnerParam(id = "prec", default = 1e-10),
                          # makeIntegerLearnerParam(id = "mxit", default = 100L, lower = 1L),
                          # makeUntypedLearnerParam(id = "offset", default = NULL),
                          # makeDiscreteLearnerParam(id = "type.gaussian", values = c("covariance", "naive"), requires = quote(family == "gaussian")),
                          # makeLogicalLearnerParam(id = "relax", default = FALSE)
                          )

nnet.ps = makeParamSet(
  #makeIntegerLearnerParam(id = "size", default = 3L, lower = 0L),
  # FIXME size seems to have no default in nnet(). If it has, par.vals is redundant
  makeIntegerParam(id = "maxit", lower = 10L, upper = 50L),
  # nnet seems to set these manually and hard for classification.....
  #     makeLogicalLearnerParam(id = "linout", default = FALSE, requires = quote(entropy == FALSE && softmax == FALSE && censored == FALSE)),
  #     makeLogicalLearnerParam(id = "entropy", default = FALSE, requires = quote(linout == FALSE && softmax == FALSE && censored == FALSE)),
  #     makeLogicalLearnerParam(id = "softmax", default = FALSE, requires = quote(entropy == FALSE && linout == FALSE && censored == FALSE)),
  #     makeLogicalLearnerParam(id = "censored", default = FALSE, requires = quote(linout == FALSE && softmax == FALSE && entropy == FALSE)),
  #makeLogicalLearnerParam(id = "skip", default = FALSE),
  makeNumericParam(id = "rang", lower = 0.1, upper = 0.9),
  makeNumericParam(id = "decay", lower = 0, upper = 1),
  makeIntegerParam(id = "MaxNWts", lower = 100L, upper = 2000L, tunable = TRUE)
)

svm.ps = makeParamSet(
  makeDiscreteParam(id = "type", default = "C-classification", values = c("C-classification", "nu-classification")),
  makeNumericParam(id = "cost", lower = 0, upper = 1, requires = quote(type == "C-classification")),
  makeNumericParam(id = "nu", lower = 0.2, upper = 0.7, requires = quote(type == "nu-classification")),
  #makeNumericVectorLearnerParam("class.weights", len = NA_integer_, lower = 0),
  makeDiscreteParam(id = "kernel", default = "radial", values = c("polynomial","radial")),
  makeIntegerParam(id = "degree", lower = 1L, upper = 3L, requires = quote(kernel == "polynomial")),
  makeNumericParam(id = "coef0", lower = 0, upper = 1, requires = quote(kernel == "polynomial" || kernel == "sigmoid")),
  makeNumericParam(id = "gamma", lower = 0, upper= 1, requires = quote(kernel != "linear")),
  makeNumericParam(id = "cachesize", lower = 10L, upper = 80L),
  makeNumericParam(id = "tolerance", lower = 0.001, upper = 0.1)
  # makeLogicalLearnerParam(id = "shrinking", default = TRUE),
  # makeIntegerLearnerParam(id = "cross", default = 0L, lower = 0L, tunable = FALSE),
  # makeLogicalLearnerParam(id = "fitted", default = TRUE, tunable = FALSE),
  # makeLogicalVectorLearnerParam(id = "scale", default = TRUE, tunable = TRUE)
)

# tune models using different seeds
nnet.result = data.frame(matrix(nrow=100,ncol=1))
nnet.param = c()
parallelStartSocket(cpus = 2, level = "mlr.tuneParams")
for (i in 1:30) {
  set.seed(i)
  nnet.res = tuneParams(makeLearner("classif.nnet", predict.type = "prob"), 
                        task, cv3, par.set = nnet.ps, control = tune.ctrl,
                        measures = mlr::brier,
                        show.info = TRUE)
  nnet.param[[i]] = nnet.res$x
  nnet.result[i,] = nnet.res$y
}

parallelStop()
gc()

nnet.idx = which.min(nnet.result$matrix.nrow...100..ncol...1.)
nnet.param[[nnet.idx]]

nnet.Mod = setHyperPars(nnet.Mod, par.vals = nnet.param[[nnet.idx]])
nnet.Mod


svm.param = c()
svm.result = data.frame(matrix(nrow=30,ncol=1))

parallelStartSocket(cpus = 3, level = "mlr.tuneParams")

for (i in 1:30) {
  set.seed(i)
  svm.res = tuneParams(makeLearner("classif.svm", predict.type = "prob"), 
                       task, cv3, par.set = svm.ps, control = tune.ctrl,
                       measures = mlr::brier,
                       show.info = TRUE)
  svm.param[[i]] = svm.res$x
  svm.result[i,] = svm.res$y
}

parallelStop()
gc()

# which seeds produced mmce less than or equal to the median mmce
best.svm.mod = which.min(svm.result$matrix.nrow...30..ncol...1.)

svm.Mod = setHyperPars(svm.Mod, par.vals = svm.param[[best.svm.mod]])
svm.Mod                         

glmnet.param = c()
glmnet.result = data.frame(matrix(nrow=10,ncol=1))
parallelStartSocket(cpus = 2, level = "mlr.tuneParams")

for (i in 1:10) {
  set.seed(i)
  glmnet.res = tuneParams(makeLearner("classif.glmnet", predict.type = "prob"), 
                          task, cv3, par.set = glmnet.ps, control = tune.ctrl.grid,
                          measures = mlr::brier,
                          show.info = TRUE)
  glmnet.param[[i]] = glmnet.res$x
  glmnet.result[i,] = glmnet.res$y
}

which.min(glmnet.result$matrix.nrow...10..ncol...1.)
glmnet.param[[2]]

cvglmnet.Mod = setHyperPars(cvglmnet.Mod, par.vals = glmnet.param[[2]] )
cvglmnet.Mod

best.glmnet.mods.brier = glmnet.result$matrix.nrow...100..ncol...1.[which(glmnet.result$matrix.nrow...100..ncol...1. <= median(glmnet.result$matrix.nrow...100..ncol...1.))]


###########################################################################################
# tune models that can handle missing values

cforest.ps = makeParamSet(
  makeIntegerParam(id = "ntree", lower = 1000L, upper = 2000L),
  makeIntegerParam(id = "mtry", lower = 1L, upper = 5L),
  makeLogicalParam(id = "replace", default = FALSE),
  makeNumericParam(id = "fraction", lower = .1, upper = .9,
                   requires = quote(replace == FALSE)),
  # makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE),
  makeDiscreteParam(id = "teststat", values = c("quad", "max"), default = "quad"),
  # makeDiscreteParam(id = "testtype",
  #                           values = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic")),
  # #makeNumericParam(id = "mincriterion", lower = 0, default = 0),
  #makeIntegerParam(id = "minsplit", lower = 20L, upper = 30L),
  #makeIntegerParam(id = "minbucket", lower = 3L, upper = 10L)
  # makeLogicalLearnerParam(id = "stump", default = FALSE),
  makeIntegerParam(id = "nresample", lower = 450L, upper = 2050L)
  # makeIntegerLearnerParam(id = "maxsurrogate", lower = 0L, default = 0L),
  # makeIntegerLearnerParam(id = "maxdepth", lower = 0L, default = 0L),
  # makeLogicalLearnerParam(id = "savesplitstats", default = FALSE, tunable = FALSE)
)

cforest.param = c()
cforest.result = data.frame(matrix(nrow=10,ncol=1))

parallelStartSocket(cpus = 3, level = "mlr.tuneParams")

for (i in 1:30) {
  set.seed(i)
  cforest.res = tuneParams(makeLearner("classif.cforest", predict.type = "prob"), 
                          task, cv3, par.set = cforest.ps, control = tune.ctrl,
                          measures = mlr::brier,
                          show.info = TRUE)
  cforest.param[[i]] = cforest.res$x
  cforest.result[i,] = cforest.res$y
}


parallelStop()

cforest.result

cforest.Mod = setHyperPars(cforest.Mod, par.vals = cforest.param[[10]] )
cforest.Mod
# ntree=1647,bootstrap=by.root,mtry=1,nodesize=1,sampsize=1524,samptype=swr
RF.ps = makeParamSet(
  makeIntegerParam(id = "ntree", lower = 1500L, upper = 2000L),
  makeDiscreteParam(id = "bootstrap", default = "by.root", values = c("by.root")),
  makeIntegerParam(id = "mtry", lower = 1L, upper = 2L),
  makeIntegerParam(id = "nodesize", lower = 1L, upper = 15L),
  #makeIntegerLearnerParam(id = "nodedepth", default = -1L),
  # makeDiscreteLearnerParam(id = "splitrule", default = "gini",
  #                          values = c("gini", "gini.unwt", "gini.hvwt", "random")),
  # makeIntegerLearnerParam(id = "nsplit", lower = 0L, default = 0L,
  #                         requires = quote(splitrule != "random")), # nsplit is ignored and internally set to 1L for splitrule = "random"
  # makeLogicalLearnerParam(id = "split.null", default = FALSE),
  # makeDiscreteLearnerParam(id = "importance", default = FALSE, tunable = FALSE,
  #                          values = list(`FALSE` = FALSE, `TRUE` = TRUE, "none", "permute", "random", "anti",
  #                                        "permute.ensemble", "random.ensemble", "anti.ensemble")),
  # makeDiscreteLearnerParam(id = "na.action", default = "na.impute",
  #                          values = c("na.omit", "na.impute"), when = "both"),
  # FIXME the default in rfsrc() for na.action is na.omit
  # makeIntegerLearnerParam(id = "nimpute", default = 1L, lower = 1L),
  # makeDiscreteLearnerParam(id = "proximity", default = FALSE, tunable = FALSE,
  #                          values = list("inbag", "oob", "all", `TRUE` = TRUE, `FALSE` = FALSE)),
  makeIntegerParam(id = "sampsize", lower = 1450L, upper = 2050L,
                          requires = quote(bootstrap == "by.root")),
  makeDiscreteParam(id = "samptype", default = "swr", values = c("swr", "swor"),
                           requires = quote(bootstrap == "by.root"))
  # makeNumericVectorLearnerParam(id = "xvar.wt", lower = 0),
  # makeLogicalLearnerParam(id = "forest", default = TRUE, tunable = FALSE),
  # makeDiscreteLearnerParam(id = "var.used", default = FALSE, tunable = FALSE,
  #                          values = list(`FALSE` = FALSE, "all.trees", "by.tree")),
  # makeDiscreteLearnerParam(id = "split.depth", default = FALSE, tunable = FALSE,
  #                          values = list(`FALSE` = FALSE, "all.trees", "by.tree")),
  # makeIntegerLearnerParam(id = "seed", lower = 0L, tunable = FALSE),
  # makeLogicalLearnerParam(id = "do.trace", default = FALSE, tunable = FALSE, when = "both"), # is currently ignored
  # makeLogicalLearnerParam(id = "membership", default = TRUE, tunable = FALSE),
  # makeLogicalLearnerParam(id = "statistics", default = FALSE, tunable = FALSE),
  # makeLogicalLearnerParam(id = "tree.err", default = FALSE, tunable = FALSE)
)

RF.param = c()
RF.result = data.frame(matrix(nrow=3,ncol=1))

parallelStartSocket(cpus = 2, level = "mlr.tuneParams")

for (i in 1:3) {
  set.seed(i)
  RF.res = tuneParams(makeLearner("classif.randomForestSRC", predict.type = "prob"), 
                           task, cv3, par.set = RF.ps, control = tune.ctrl.grid,
                           measures = mlr::brier,
                           show.info = TRUE)
  RF.param[[i]] = RF.res$x
  RF.result[i,] = RF.res$y
}

RF.param
parallelStop()
gc()
RF.param
RF.param[[1]]

RF.params = list(ntree=1647,bootstrap='by.root',mtry=1,nodesize=1,sampsize=1524,samptype='swr')
RF.Mod = setHyperPars(RF.Mod, par.vals = RF.param[[1]] )
RF.Mod
