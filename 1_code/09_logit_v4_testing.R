# ------------
# Introduction
# ------------
## Here, we test some modifications to the v4 model using CV
NAME <- '09_logit_v4_testing'


# ------------
# Preamble
# ------------
## Imports
source('1_code/00_utils.R')

## Settings & working directory
setwd(file.path(PROJECT_DIR, PROJECT))
randomSeed <- 8888
sampleSize <- 100000
sampleCount <- 10

## Set up pipeline folder if missing
pipeline <- file.path('2_pipeline', NAME)
if (!dir.exists(pipeline)) {
  dir.create(pipeline)
  dir.create(file.path(pipeline, 'out'))
}


# ---------
# Main code
# ---------
## Dataset is DA - Google play - marketing - dig = 3
if(!exists('dfLoad')) {
  dfLoad <- data.table(readRDS(file.path('0_data', 'da_payer_dataset_v4.rds')))
  dfLoad[, dy_payer := factor(dfLoad$dy_pay_count > 0)]
}
dfAll <- dfLoad[
  source == 'marketing' &
    register_platform == 'google_play',
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


## Compare tier vs. tier_payers
set.seed(randomSeed)
filePath <- file.path(
  '2_pipeline', NAME, 'out', 'test_tier_vs_tier_payers_' %+% randomSeed %+% '.pdf'
)
pdf(filePath)
for(i in 1:sampleCount) {
  # Sample data
  dat <- dfAll[days_in_game == 3, ]
  datSample <- dat[sample(nrow(dat), sampleSize)]
  trainIndex <- createDataPartition(
    datSample$dy_payer, p = 0.8, list = FALSE, times = 1
  )
  datTrain <- datSample[trainIndex, ]
  datTest <- datSample[-trainIndex, ]
  # Fit the models
  featureSubset <- c(4, 6, 7, 8)
  mod <- glm(
    formula = dy_payer ~ .,
    data = datTrain[, c(featureSubset, 9), with = F],
    family = 'binomial'
  )
  datTest[, mod_fit_tier := predict.glm(mod, newdata = datTest, type = 'response')]
  featureSubset <- c(5, 6, 7, 8)
  mod <- glm(
    formula = dy_payer ~ .,
    data = datTrain[, c(featureSubset, 9), with = F],
    family = 'binomial'
  )
  datTest[, mod_fit_tier_payers := predict.glm(mod, newdata = datTest, type = 'response')]
  # Calculate ROC and AUC
  rocObjTier <- roc(
    response = factor(datTest$dy_payer),
    predictor = datTest$mod_fit_tier,
    auc = TRUE
  )
  rocObjTierPayers <- roc(
    response = factor(datTest$dy_payer),
    predictor = datTest$mod_fit_tier_payers,
    auc = TRUE
  )
  printOutput(list(
    'sample ' %+% i,
    'comparison tier vs. tier_payers',
    rocObjTier$auc,
    rocObjTierPayers$auc
  ))
  plot(rocObjTier, col = 'red')
  plot(rocObjTierPayers, col = 'gray', add = TRUE)
}
printOutput('It seems that tier_payers is not worth the trouble...')
dev.off()


## How is d0 usefull?
set.seed(randomSeed)
filePath <- file.path(
  '2_pipeline', NAME, 'out', 'test_d0_session_count_by_dig_' %+% randomSeed %+% '.pdf'
)
pdf(filePath)
for(dig in 0:7) {
  # Prepare data
  dat <- dfAll[
    days_in_game == dig,
    .(
      tier,
      dx_pay_count,
      dx_active_days,
      d0_session_count,
      dy_payer
    )]
  trainIndex <- createDataPartition(
    dat$dy_payer, p = 0.8, list = FALSE, times = 1
  )
  datTrain <- dat[trainIndex, ]
  datTest <- dat[-trainIndex, ]
  # Fit the models
  mod <- glm(
    formula = dy_payer ~ .,
    data = datTrain,
    family = 'binomial'
  )
  datTest[, mod_fit_w := predict.glm(mod, newdata = datTest, type = 'response')]
  mod <- glm(
    formula = dy_payer ~ .,
    data = datTrain[, c(-4), with = F],
    family = 'binomial'
  )
  datTest[, mod_fit_wo := predict.glm(mod, newdata = datTest, type = 'response')]
  # Calculate ROC and AUC
  rocObj1 <- roc(
    response = factor(datTest$dy_payer),
    predictor = datTest$mod_fit_w,
    auc = TRUE
  )
  rocObj2 <- roc(
    response = factor(datTest$dy_payer),
    predictor = datTest$mod_fit_wo,
    auc = TRUE
  )
  printOutput(list(
    'comparison d0_session_count with vs. without',
    rocObj1$auc,
    rocObj2$auc
  ))
  plot(rocObj1, col = 'red', main = 'days in game = ' %+% dig)
  plot(rocObj2, col = 'gray', add = TRUE)
}
dev.off()