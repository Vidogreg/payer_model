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












## experiments with visualizations
datDown <- data.table(downSample(x = dat[, -ncol(dat), with = F], y = dat$dy_payer))
setnames(datDown, old = c('Class'), new = c('dy_payer'))

datDown <- datDown[sample(nrow(datDown), 200)]

library(AppliedPredictiveModeling)

transparentTheme(trans = .4)
featurePlot(
  x = datDown[, 4:7, with = F],
  y = datDown$dy_payer,
  plot = 'ellipse',
  auto.key = list(columns = 2)
)

transparentTheme(trans = .9)
featurePlot(
  x = datDown[, 4:7],
  y = datDown$dy_payer,
  plot = "density",
  ## Pass in options to xyplot() to
  ## make it prettier
  scales = list(
    x = list(relation="free"),
    y = list(relation="free")
  ),
  adjust = 1.5,
  pch = "|",
  layout = c(4, 1),
  auto.key = list(columns = 2)
)

featurePlot(
  x = datDown[, 4:7],
  y = datDown$dy_payer,
  plot = "box",
  ## Pass in options to bwplot()
  scales = list(
    y = list(relation="free"),
    x = list(rot = 90)
  ),
  layout = c(4,1 ),
  auto.key = list(columns = 2)
)











## Function that calculates AUC using CV
calcCvAuc <- function(dat, K) {
  foldsIndex <- createFolds(dat$dy_payer, k = K)
  result <- list()
  for(k in 1:K) {
    ## train model
    dfTrain <- dat[-foldsIndex[[k]], ]
    mod <- glm(
      formula = dy_payer ~ .,
      data = dfTrain,
      family = 'binomial'
    )
    ## predict on test data
    dfTest <- dat[foldsIndex[[k]], ]
    dfTest[, mod_fit := predict.glm(mod, newdata = dfTest, type = 'response')]
    ## calculate AUC on test predictions
    auc <- auc(
      response = factor(dfTest$dy_payer),
      predictor = dfTest$mod_fit
    )
    ## write results
    result[[k]] <- auc
    print('Fold ' %+% k %+% '/' %+% K %+% ' done')
  }
  unlist(result)
}

