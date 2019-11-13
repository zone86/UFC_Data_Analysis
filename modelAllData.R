library(mlr)
library(ggplot2)
library(mlrMBO)
###############################################################################################
# using the features
allDatCleaned = all_fights

sapply(allDatCleaned, function(x) length(which(!is.na(x)))/nrow(allDatCleaned) )

all_target = allDatCleaned$Winner
vars = head(featImp$data$name[order(featImp$data$value, decreasing = T)],15)[-c(15,14,13,12,11,10)]
# SUBSET
# [1] "B_Avg_Total_Grappling_Submissions_Attempts"       "B_Age"                                           
# [3] "R_Avg_Total_Strikes_Legs.Total.Strikes_Attempts"  "R_Avg_Total_Strikes_Clinch.Body.Strikes_Attempts"
# [5] "AgeDifference"                                    "R_Avg_Total_Strikes_Body.Total.Strikes_Attempts" 
# [7] "R_Avg_Total_Strikes_Clinch.Body.Strikes_Landed"   "R_Avg_Total_Strikes_Ground.Leg.Strikes_Landed"   
# [9] "R_Age"

# ALL
# "AgeDifference"                                           "B_Age"                                                  
# [3] "R_Avg_Total_Strikes_Body.Significant.Strikes_Landed"     "B_Avg_Total_Grappling_Submissions_Attempts"             
# [5] "R_Avg_Total_Strikes_Clinch.Head.Strikes_Attempts"        "R_Avg_Total_Strikes_Legs.Total.Strikes_Attempts"        
# [7] "B_Avg_Total_TIP_Neutral.Time"                            "B_Avg_Total_Strikes_Distance.Head.Strikes_Landed"       
# [9] "R_Avg_Total_Strikes_Ground.Leg.Strikes_Landed"           "R_Avg_Total_Strikes_Ground.Significant.Strikes_Attempts"
# [11] "R_Avg_Total_Strikes_Legs.Significant.Strikes_Landed"     "R_Avg_Total_Strikes_Body.Total.Strikes_Landed"          
# [13] "R_Avg_Total_Strikes_Clinch.Body.Strikes_Attempts"        "R_Avg_Total_Strikes_Clinch.Head.Strikes_Landed"         
# [15] "B_Avg_Total_Grappling_Takedowns_Attempts"                "R_Avg_Total_Strikes_Clinch.Body.Strikes_Landed"         
# [17] "B_Avg_Total_Strikes_Total.Strikes_Attempts"              "R_Avg_Total_Strikes_Body.Total.Strikes_Attempts"        
# [19] "R_Avg_Total_TIP_Neutral.Time"                            "B_Avg_Total_Strikes_Head.Significant.Strikes_Attempts"  
# [21] "B_Avg_Total_Strikes_Head.Significant.Strikes_Landed"     "B_Avg_Total_Strikes_Punches_Attempts"                   
# [23] "B_Avg_Total_Strikes_Ground.Body.Strikes_Landed"          "R_Avg_Total_TIP_Ground.Time"                            
# [25] "R_Avg_Total_Strikes_Head.Significant.Strikes_Attempts"   "R_Avg_Total_Strikes_Distance.Body.Punches_Landed"       
#"R_Age" 

all_data = allDatCleaned %>%
  select(vars, Winner)


sapply(all_data, function(x) length(which(!is.na(x)))/nrow(all_data) ) # missing data proportion

all_data = all_data[!(all_data$Winner %in% c('no contest','draw')),]

all_data$Winner = factor(all_data$Winner, levels = c('blue','red'))
all_data$R_Heavier = factor(all_data$R_Heavier, levels = c('FALSE','TRUE'))
# all_data$B_Taller = factor(all_data$B_Taller, levels = c('FALSE','TRUE'))

idx = sapply(all_data, function(x) is.nan(x))
all_data[idx] = NA

all_task = makeClassifTask(target = 'Winner', data = all_data)

all_task = normalizeFeatures(obj = all_task, method = 'standardize',
                             cols = colnames(all_data)[-c(11)] )
set.seed(1)
ho = makeResampleInstance( "Holdout", all_task)
task.train = subsetTask(all_task,ho$train.inds[[1]])
task.test = subsetTask(all_task,ho$test.inds[[1]])

set.seed(1)
cv = makeResampleDesc("CV", iters = 3)

log.Mod = makeLearner('classif.logreg', predict.type = "prob")

multinom.Mod = makeLearner('classif.multinom', predict.type = "prob")

glmnet.Mod = makeLearner('classif.glmnet', predict.type = "prob", par.vals = list(alpha = 0))

rpart.Mod = makeLearner("classif.rpart", predict.type = "prob")
RF.Mod = makeLearner('classif.randomForestSRC', predict.type = "prob")
cforest.Mod = makeLearner("classif.cforest", predict.type = "prob")
C50.Mod = makeLearner("classif.C50", predict.type = "prob")

logMod.CV = resample(log.Mod, all_task, cv, measures = list(acc,auc,f1), extract = function(x) getLearnerModel(x)$coefficients)
multinom.CV = resample(multinom.Mod, all_task, cv, measures = list(acc,auc,f1), extract = function(x) getLearnerModel(x))
glmnet.CV = resample(glmnet.Mod, all_task, cv, measures = list(acc,auc,f1), extract = function(x) getLearnerModel(x))

log.train = train(log.Mod, task.train)
log.test = predict(log.train, task.test)
performance(log.test, measures = list(acc,auc,f1,tpr))

log.preds = getRRPredictions(logMod.CV)
log.preds$data


ps = makeParamSet(
  #makeLogicalParam(id = "Hess", default = FALSE, tunable = FALSE),
  #makeDiscreteParam(id = "summ", default = 0L, values = 0:3),
  #makeLogicalParam(id = "censored", default = FALSE),
  #makeLogicalParam(id = "model", default = FALSE, tunable = FALSE),
  makeIntegerParam(id = "maxit", lower = 100L, upper = 300L),
  #makeNumericParam(id = "rang", lower = 0.6, upper = .8),
  makeNumericParam(id = "decay", lower = 0, upper = .1) #def = 0
  # makeLogicalParam(id = "trace", default = TRUE, tunable = FALSE),
  # makeNumericParam(id = "abstol", default = 1.0e-4),
  # makeNumericParam(id = "reltol", default = 1.0e-8)
)

cvmultinom.obj = makeSingleObjectiveFunction(name = "multinom.tuning",
                                           fn = function(x) {
                                             lrn = makeLearner('classif.multinom', predict.type = "prob", par.vals = x)
                                             resample(lrn, all_task, cv3, show.info = FALSE)$aggr
                                           },
                                           par.set = ps,
                                           noisy = TRUE,
                                           has.simple.signature = FALSE,
                                           minimize = TRUE
)

ctrl = makeMBOControl()
ctrl = setMBOControlTermination(ctrl, iters = 50)

res = mbo(cvmultinom.obj, control = ctrl, show.info = TRUE)
res$x
multinom.Mod = setHyperPars(multinom.Mod, par.vals = res$x)
multinom.Mod
multinom.train = train(multinom.Mod, task.train)
multinom.test = predict(multinom.train, task.test)
performance(multinom.test, measures = list(acc,auc,f1,tpr))


ps = makeParamSet( makeNumericParam("alpha", values = seq(0., .3, by=.01)))

cvglmnet.obj = makeSingleObjectiveFunction(name = "glmnet.tuning",
                                  fn = function(x) {
                                    lrn = makeLearner('classif.glmnet', predict.type = "prob", par.vals = x)
                                    resample(lrn, all_task, cv3, show.info = FALSE)$aggr
                                  },
                                  par.set = ps,
                                  noisy = TRUE,
                                  has.simple.signature = FALSE,
                                  minimize = TRUE
)

res = mbo(cvglmnet.obj, control = ctrl, show.info = TRUE)
print(res)
res$x
#tr = tuneParams(cvglmnet.Mod, task.train, cv3, acc, ps, ctrl)
glmnet.Mod = setHyperPars(glmnet.Mod, par.vals = res$x)
glmnet.Mod

glmnet.train = train(glmnet.Mod, task.train)
glmnet.test = predict(glmnet.train, task.test)
performance(glmnet.test, measures = list(acc,auc,f1,tpr))

set.seed(1)
rpart.train = train(rpart.Mod, task.train)
rpart.test = predict(rpart.train, task.test)
performance(rpart.test, measures = list(acc,auc,f1,tpr))

set.seed(1)
C50.train = train(C50.Mod, task.train)
C50.test = predict(C50.train, task.test)
performance(C50.test, measures = list(acc,auc,f1,tpr))

set.seed(1)
ps = makeParamSet(
  makeIntegerParam(id = "ntree", lower = 1000L, upper = 1500L),
  # makeDiscreteLearnerParam(id = "bootstrap", default = "by.root",
  #                          values = c("by.root", "by.node", "none")),
  makeIntegerParam(id = "mtry", lower = 1L, upper = 5L),
  makeIntegerParam(id = "nodesize", lower = 1L, upper = 10L)
  # makeIntegerLearnerParam(id = "nodedepth", default = -1L),
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
  # # FIXME the default in rfsrc() for na.action is na.omit
  # makeIntegerLearnerParam(id = "nimpute", default = 1L, lower = 1L),
  # makeDiscreteLearnerParam(id = "proximity", default = FALSE, tunable = FALSE,
  #                          values = list("inbag", "oob", "all", `TRUE` = TRUE, `FALSE` = FALSE)),
  # makeIntegerLearnerParam(id = "sampsize", lower = 1L,
  #                         requires = quote(bootstrap == "by.root")),
  # makeDiscreteLearnerParam(id = "samptype", default = "swr", values = c("swr", "swor"),
  #                          requires = quote(bootstrap == "by.root")),
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

RF.obj = makeSingleObjectiveFunction(name = "RF.tuning",
                                           fn = function(x) {
                                             lrn = makeLearner('classif.randomForestSRC', predict.type = "prob", par.vals = x)
                                             resample(lrn, all_task, cv3, show.info = FALSE)$aggr
                                           },
                                           par.set = ps,
                                           noisy = TRUE,
                                           has.simple.signature = FALSE,
                                           minimize = TRUE
)

res = mbo(RF.obj, control = ctrl, show.info = TRUE)
print(res)
res$x

RF.Mod = setHyperPars(RF.Mod, par.vals = res$x)
RF.Mod

set.seed(1)
RF.train = train(RF.Mod, task.train)
RF.test = predict(RF.train, task.test)
performance(RF.test, measures = list(acc,auc,f1,tpr))

ps = makeParamSet(
  makeIntegerParam(id = "ntree", lower = 1000L, upper = 1500L),
  makeIntegerParam(id = "mtry", lower = 1L, upper = 5L),
  # makeLogicalLearnerParam(id = "replace", default = FALSE),
  # makeNumericLearnerParam(id = "fraction", lower = 0, upper = 1, default = 0.632,
  #                         requires = quote(replace == FALSE)),
  # makeLogicalLearnerParam(id = "trace", default = FALSE, tunable = FALSE),
  # makeDiscreteLearnerParam(id = "teststat", values = c("quad", "max"), default = "quad"),
  # makeDiscreteLearnerParam(id = "testtype",
  #                          values = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic"),
  #                          default = "Univariate"),
  #makeNumericParam(id = "mincriterion", lower = 0, default = 0),
  makeIntegerParam(id = "minsplit", lower = 20L, upper = 30L),
  makeIntegerParam(id = "minbucket", lower = 3L, upper = 10L)
  # makeLogicalLearnerParam(id = "stump", default = FALSE),
  # makeIntegerLearnerParam(id = "nresample", lower = 1L, default = 9999L),
  # makeIntegerLearnerParam(id = "maxsurrogate", lower = 0L, default = 0L),
  # makeIntegerLearnerParam(id = "maxdepth", lower = 0L, default = 0L),
  # makeLogicalLearnerParam(id = "savesplitstats", default = FALSE, tunable = FALSE)
)

cforest.obj = makeSingleObjectiveFunction(name = "cforest.tuning",
                                     fn = function(x) {
                                       lrn = makeLearner('classif.cforest', predict.type = "prob", par.vals = x)
                                       resample(lrn, all_task, cv3, show.info = FALSE)$aggr
                                     },
                                     par.set = ps,
                                     noisy = TRUE,
                                     has.simple.signature = FALSE,
                                     minimize = TRUE
)

res = mbo(cforest.obj, control = ctrl, show.info = TRUE)
print(res)
res$x

# $ntree
# [1] 1408
# 
# $mtry
# [1] 3
# 
# $minsplit
# [1] 24
# 
# $minbucket
# [1] 9

cforest.Mod = setHyperPars(cforest.Mod, par.vals = res$x)
cforest.Mod

set.seed(1)
cforest.train = train(cforest.Mod, task.train)
cforest.test = predict(cforest.train, task.test)
performance(cforest.test, measures = list(acc,auc,f1,tpr))


ctrl = makeFeatSelControlSequential(method='sbs', maxit=100, max.features=20)
all_feats = selectFeatures(multinom.Mod, all_task, measures = tpr, resampling = cv, control = ctrl)
sbs_featselect.multinom = all_feats$x
#sffs_featselect = all_feats$x
sffs_featselect.multinom = sffs_featselect

set.seed(1)
featImp = generateFilterValuesData(task= all_task)
plotFilterValues(featImp) + ggtitle('Feature Importance Random Forest SRC')

head(featImp$data[order(featImp$data$value, decreasing = T),],20)

