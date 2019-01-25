# ------------
# Introduction
# ------------
## The goal here is to simplify the logistic model with dataset v2 by feature selection.
NAME <- '7_logit_v2_feature_selection'


# ------------
# Preamble
# ------------
## Imports
source('1_code/00_utils.R')

if(!exists('dfLoad'))
  dfLoad <- data.table(readRDS(file.path(
    '0_data', 'ga_972_payer_dataset_v2.rds'
  )))

## Settings & working directory
setwd(file.path(PROJECT_DIR, PROJECT))
randomSeed <- 1024
sampleSize <- 100000
filePath = file.path(
  '2_pipeline', NAME, 'out', 'logit_v2_feature_selection_' %+% randomSeed %+% '.pdf'
)

## Set up pipeline folder if missing
pipeline <- file.path('2_pipeline', NAME)
if (!dir.exists(pipeline)) {
  dir.create(pipeline)
  dir.create(file.path(pipeline, 'out'))
}


# ---------
# Main code
# ---------
## Define and transform dataset
dfAll <- dfLoad[register_platform == 'google play' & source == 'marketing']
# dfAll <- dfLoad[register_platform == 'apple appstore' & source == 'marketing']
dfAll[, tier := as.numeric(tier)]
dfAll[, dy_payer := factor(dy_payer)]

## Sample dataset
set.seed(randomSeed)
dfSample <- dfAll[sample(nrow(dfAll), sampleSize)]

## Define variebles and downsample based on y
dat <- dfSample[, .(
  tier,
  dx_pay_count,
  dx_payer,
  dx_active_days,
  d0_session_count,
  d1x_session_count,
  d1x_daily_session_count_relative_to_d0,
  dy_payer
)]
datDown <- data.table(
  downSample(x = dat[, -ncol(dat), with = F], y = dat$dy_payer)
)
setnames(datDown, old = c('Class'), new = c('dy_payer'))


## Visualize variables
pdf(filePath)
## payment-related variables
X <- datDown[, 1:3]
y <- datDown$dy_payer
print(densityFeaturePlot(X, y, layout = c(2, 2)))
print(boxFeaturePlot(X, y, layout = c(2, 2)))
### Hard to see anything in the plots since there are few distinct values
#### and all medians are the same.
### It is still clear that payment variables are all relevant.

## session-related variables
X <- datDown[, 4:7]
y <- datDown$dy_payer
print(densityFeaturePlot(X, y, layout = c(2, 2)))
print(boxFeaturePlot(X, y, layout = c(2, 2)))
### All session variables seem relevant
### The most obvious is dx_active_days
### d0_session_count seems the least relevant



## Selection by filter
filterCtrl <- sbfControl(functions = rfSBF, method = "repeatedcv", repeats = 5)
set.seed(randomSeed)
rfWithFilter <- sbf(
  datDown[, -ncol(datDown), with = F],
  datDown$dy_payer,
  sbfControl = filterCtrl
)
printOutput(rfWithFilter)
## All 7 variables were selected - how interesting...



## CV-AUC of base model
K <- 6
# cvAuc <- calcCvAuc(dat, K)
# summary(cvAuc)
# cvAuc <- calcCvAuc(datDown, K)
# summary(cvAuc)
### AUC of downsampled model is the same as original ->
#### -> we can use datDown for variable selection



## Variable selection by examination 
## One variables
combs <- combn(1:7, 1)
combAuc <- calcCvAucCombinations(combs, datDown, K)
printOutput(combAuc, cex = 0.5)
### As visualizations suggested, dx_active_days is the best
#### d1x_session_count is very similar
#### dx_pay_count and dx_payer are very similar
#### best AUC = 0.9188


## Two variables
combs <- combn(1:7, 2)
combAuc <- calcCvAucCombinations(combs, datDown, K)
printOutput(combAuc, cex = 0.5)
### 3 best models are with dx_active_days -> we keep it
#### next best variables are (decreasing AUC): tier, dx_payer, dx_pay_count
#### best AUC = 0.9297


## Three variables - one is dx_active_days[4]
combs <- combn((1:7)[-4], 2)
combsFinal <- rbind(
  combs,
  matrix(4, nrow = 1, ncol = ncol(combs))
)
combAuc <- calcCvAucCombinations(combsFinal, datDown, K)
printOutput(combAuc, cex = 0.5)
### 3 best models have dx_active_days + tier -> we keep it
#### next best variables are (decreasing AUC): dx_payer, dx_pay_count, d0_session_count
#### best AUC = 0.938
#### !!! differences in AUC are very small here !!!


## Four variables - two are tier[1] and dx_active_days[4]
combs <- combn((1:7)[-c(1, 4)], 2)
combsFinal <- rbind(
  combs,
  matrix(1, nrow = 1, ncol = ncol(combs)),
  matrix(4, nrow = 1, ncol = ncol(combs))
)
combAuc <- calcCvAucCombinations(combsFinal, datDown, K)
printOutput(combAuc, cex = 0.5)
### best models have one session-related metric and one payer-related
#### differences in AUC are minimal, other metric is needed to proceed
#### I suggest using dx_pay_count over dx_payer
#### best AUC = 0.9436

## Four variables - three are tier[1], dx_pay_count[2], dx_active_days[4]
combs <- combn((1:7)[-c(1, 2, 4)], 1)
combsFinal <- rbind(
  combs,
  matrix(1, nrow = 1, ncol = ncol(combs)),
  matrix(2, nrow = 1, ncol = ncol(combs)),
  matrix(4, nrow = 1, ncol = ncol(combs))
)
combAuc <- calcCvAucCombinations(combsFinal, datDown, K)
printOutput(combAuc, cex = 0.5)
### d0_session_count is slightly better than other variables
### this seems the best: d0_session_count + dx_active_days + dx_pay_count + tier
#### best AUC = 0.9429


## To check, we will try 5, 6 and 7 variable models too
## 5 variables
combs <- combn((1:7), 5)
combAuc <- calcCvAucCombinations(combs, datDown, K)
printOutput(combAuc, cex = 0.5)
### best AUC = 0.9462
## 6 variables
combs <- combn((1:7), 6)
combAuc <- calcCvAucCombinations(combs, datDown, K)
printOutput(combAuc, cex = 0.5)
### best AUC = 0.9457
## 7 variables
combs <- combn((1:7), 7)
combAuc <- calcCvAucCombinations(combs, datDown, K)
printOutput(combAuc, cex = 0.5)
### best AUC = 0.9451
#### Looks like 5 variables is the best
#### Best are variables: 1, 2(or 3), 4, 5, 7


## These models should be compared in more detail:
## 7 variables: full model
## 4 variables: 1, 2, 4, 5
### - tier
### - dx_pay_count
### - dx_active_days
### - d0_session_count
## 5 variables: 1, 2, 4, 5, 7
### - tier
### - dx_pay_count
### - dx_active_days
### - d0_session_count
### - d1x_daily_session_count_relative_to_d0
combs <- matrix(c(1, 2, 4, 5))
combAuc <- calcCvAucCombinations(combs, dat, K)
printOutput(combAuc, cex = 0.5)

combs <- matrix(c(1, 2, 4, 5, 7))
combAuc <- calcCvAucCombinations(combs, dat, K)
printOutput(combAuc, cex = 0.5)

combs <- matrix(1:7)
combAuc <- calcCvAucCombinations(combs, dat, K)
printOutput(combAuc, cex = 0.5)

## ROC plot of full model
trainIndex <- createDataPartition(
  dat$dy_payer, p = 0.8, list = FALSE, times = 1
)
dfTrain <- dat[trainIndex, ]
dfTest <- dat[-trainIndex, ]
modList <- trainLogitModel(dfTrain, dfTest)
dfTest[, mod_fit := modList$modelFit]
summary(modList$modelObject)
rocObj <- roc(
  response = factor(dfTest$dy_payer),
  predictor = dfTest$mod_fit
)
plot(
  rocObj,
  print.thres = TRUE,
  print.auc = TRUE,
  main = 'ROC curve of full model (7 variables)'
)

## ROC plot of simplified model
dat2 <- dat[, c(1, 2, 4, 5, 8), with = F]
# dat2 <- dat[, c(1, 2, 4, 5, 7, 8), with = F]
trainIndex <- createDataPartition(
  dat2$dy_payer, p = 0.8, list = FALSE, times = 1
)
dfTrain <- dat2[trainIndex, ]
dfTest <- dat2[-trainIndex, ]
modList <- trainLogitModel(dfTrain, dfTest)
dfTest[, mod_fit := modList$modelFit]
summary(modList$modelObject)
rocObj <- roc(
  response = factor(dfTest$dy_payer),
  predictor = dfTest$mod_fit
)
plot(
  rocObj,
  print.thres = TRUE,
  print.auc = TRUE,
  main = 'ROC curve of simplified model (4 variables)'
)
printOutput(c(
  'I will choose the simplified model with 4 variables for now.',
  'The difference in AUC is insignificant compared to full v2 model.',
  'For more detailed comments, see code:',
  ' -> ' %+% NAME,
  'This was calculated on DA - Google play - marketing',
  'But we got very similar results on iOS too.',
  '  - tier is not as good a predictor on iOS',
  '  - d1x_daily_session_count_relative_to_d0 looks more promising on iOS'
))
## I will go with the 4 variable model at first to make it simple
### and because the difference in AUC is insignificant

## close the pdf file
dev.off()