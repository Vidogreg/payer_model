# ------------
# Introduction
# ------------
## This file evaluates a logistic model with payment&session data with CV.
NAME <- '4_logit_cv_pay&session'
PROJECT <- 'payer_model'
PROJECT_DIR <- file.path(
  'C:/Users/vgregor/OneDrive - PXFD',
  '_PIXEL FEDERATION/GA issues/Games General/GA-972 Payer model'
)


# ------------
# Preamble
# ------------
## Imports
source('1_code/00_utils.R')
packageTest('data.table')

if(!exists('dfLoad'))
  dfLoad <- data.table(readRDS(file.path(
    '0_data', 'ga_972_payer_dataset_v1.rds'
  )))


## Settings & working directory
setwd(file.path(PROJECT_DIR, PROJECT))
randomSeed <- 1024


## Set  up pipeline folder if missing
pipeline <- file.path('2_pipeline', NAME)
if (!dir.exists(pipeline)) {
  dir.create(pipeline)
  dir.create(file.path(pipeline, 'out'))
}


# ---------
# Main code
# ---------
## Define dataset
## filter all relevant observations - marketing
## select relevant features
dfLoad[, tier := as.numeric(tier)]
dfAll <- dfLoad[source == "marketing", .(
  tier,
  dx_pay_count,
  dx_payer,
  dx_active_days,
  d0_session_count,
  dx_session_count,
  dy_payer
)]
dfAll[, dy_payer := factor(dy_payer)]

## split dataset to train and test
## We will train and tune the model only on train dataset using CV
## test dataset will be used only in the end
set.seed(randomSeed)
trainIndex <- createDataPartition(
  dfAll$dy_payer, p = 0.8, list = FALSE, times = 1
)
dfTrainVal <- dfAll[trainIndex, ]
dfTest <- dfAll[-trainIndex, ]



##### TEMP
K <- 10
data <- dfTrainVal
k <- 1

## k-fold CV
makeCrossVal <- function(K, data) {
  
  foldsIndex <- createFolds(data$dy_payer, k = K)
  
  for(k in 1:K) {
    
    dfTrain <- data[-foldsIndex[[k]], ]
    mod <- glm(
      formula = dy_payer ~ .,
      data = dfTrain,
      family = 'binomial'
    )
    
    dfVal <- dfAll[foldsIndex[[k]], ]
    dfVal[, mod_fit := predict.glm(mod, newdata = dfVal, type = 'response')]
    
    rocObj <- roc(response = factor(dfVal$dy_payer), predictor = dfVal$mod_fit)
    
    cutOffOptimal <- getOptimalCutOff(
      criterion = "relDiffPosPred",
      ths = rocObj$thresholds,
      fit = dfVal$mod_fit,
      ref = dfVal$dy_payer
    )
    
    # print(table(dfTrain$dy_payer))
    # print(table(dfVal$dy_payer))
    
  }
  
}









makeCrossVal(K = K, data = dfTrainVal)












mod <- glm(
  formula = dy_payer ~ .,
  data = dfAll,
  family = 'binomial'
)

summary(mod)
dfAll[, mod_fit := predict.glm(mod, newdata = dfAll, type = 'response')]

rocObj <- roc(response = factor(dfAll$dy_payer), predictor = dfAll$mod_fit)
plot.roc(rocObj, print.thres = TRUE, print.auc = TRUE, main = 'ROC curve')
print(rocObj$auc)









# ---------
# Leftovers
# ---------
# dfAll[, .(avg_fit = mean(mod_fit)), by = tier]