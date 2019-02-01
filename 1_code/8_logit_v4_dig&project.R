# ------------
# Introduction
# ------------
## The goal here is to simplify the logistic model with dataset v2 by feature selection.
NAME <- '8_logit_v4_by_dig&project'


# ------------
# Preamble
# ------------
## Imports
source('1_code/00_utils.R')

## Settings & working directory
setwd(file.path(PROJECT_DIR, PROJECT))
randomSeed <- 1024
# sampleSize <- 100000
sampleSizeDown <- 400
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
## Diggy
if(!exists('dfLoadDa'))
  dfLoadDa <- data.table(readRDS(file.path('0_data', 'da_payer_dataset_v4.rds')))
dfLoadDa[, dy_payer := factor(dfLoadDa$dy_pay_count > 0)]

dfAll <- dfLoadDa[
  source == "marketing" & register_platform == "google_play",
  .(
    player_id,
    days_in_game,
    register_month,
    tier,
    tier_payers,
    dx_pay_count,
    dx_active_days,
    d0_session_count,
    dy_payer
  )]

digs <- unique(dfAll$days_in_game)

for(dig in digs){
  
  ## Define datasets
  dat <- dfAll[
    days_in_game == dig,
    .(
      tier_payers,
      dx_pay_count,
      dx_active_days,
      d0_session_count,
      dy_payer
    )]
  set.seed(randomSeed)
  datDown <- data.table(
    downSample(x = dat[, -ncol(dat), with = F], y = dat$dy_payer)
  )
  setnames(datDown, old = c('Class'), new = c('dy_payer'))
  datDown <- datDown[sample(nrow(datDown), sampleSizeDown)]
  
  ## Visualize down-sampled dataset
  print(densityFeaturePlot(
    datDown[, 1:4], datDown$dy_payer, layout = c(2, 2),
    main = 'Features, days in game = ' %+% dig
  ))
  
  ## Define train & test datasets
  trainIndex <- createDataPartition(
    dat$dy_payer, p = 0.8, list = FALSE, times = 1
  )
  datTrain <- dat[trainIndex, ]
  datTest <- dat[-trainIndex, ]
  
  ## Train model
  if(dig == 0) {
    mod <- glm(
      formula = dy_payer ~ tier_payers + dx_pay_count + d0_session_count,
      data = datTrain,
      family = 'binomial'
    )
  } else {
    mod <- glm(
      formula = dy_payer ~ .,
      data = datTrain,
      family = 'binomial'
    )
  }
  
  ## Evaluate models
  datTest[, mod_fit := predict.glm(mod, newdata = datTest, type = 'response')]
  rocObj <- roc(
    response = datTest$dy_payer,
    predictor = datTest$mod_fit,
    auc = TRUE
  )
  printOutput(summary(mod))
  plot.roc(
    rocObj, print.thres = TRUE, print.auc = TRUE,
    main = 'ROC curve, days in game = ' %+% dig
  )
  
}























