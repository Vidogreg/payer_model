# ------------
# Introduction
# ------------
## The goal here is to improve the logistic model with dataset v2.
NAME <- '7_improve_logit_v2'


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
dfAll[, tier := as.numeric(tier)]
dfAll[, dy_payer := factor(dy_payer)]

## Sample dataset
set.seed(randomSeed)
dfSample <- dfAll[sample(nrow(dfAll), sampleSize)]

## CV-AUC of base model - mod0
K <- 6
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

# cvAuc <- calcCvAuc(dat, K)
# summary(cvAuc)









## downsample the dataset for visualization purposes
datDown <- data.table(
  downSample(x = dat[, -ncol(dat), with = F], y = dat$dy_payer)
)
setnames(datDown, old = c('Class'), new = c('dy_payer'))
set.seed(randomSeed)
datDown2 <- datDown[sample(nrow(datDown), 1000)]

## payment-related variables
X <- datDown2[, 1:3]
y <- datDown2$dy_payer
densityFeaturePlot(X, y, layout = c(2, 2))
boxFeaturePlot(X, y, layout = c(2, 2))
### Hard to see anything in the plots since there are few distinct values
#### and all medians are the same.
### It is still clear that payment variables are all relevant.


## session-related variables
X <- datDown2[, 4:7]
y <- datDown2$dy_payer
densityFeaturePlot(X, y, layout = c(2, 2))
boxFeaturePlot(X, y, layout = c(2, 2))
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
print(rfWithFilter)
## All 7 variables were selected.






























