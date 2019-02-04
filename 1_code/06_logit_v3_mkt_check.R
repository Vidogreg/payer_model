# ------------
# Introduction
# ------------
## This file trains logistic model with payment&session data for several register months.
NAME <- '06_logit_v3_mkt_check'


# ------------
# Preamble
# ------------
## Imports
source('1_code/00_utils.R')
# packageTest('DBI')
# packageTest('odbc')
packageTest('data.table')

if(!exists('dfLoad'))
  dfLoad <- data.table(readRDS(file.path(
    '0_data', 'ga_972_payer_dataset_v3.rds'
  )))

## Settings & working directory
setwd(file.path(PROJECT_DIR, PROJECT))
randomSeed <- 1024
resultTableName <- 'ga_972_payer_prediction_v3'

# con <- DBI::dbConnect(
#   odbc::odbc(),
#   Driver = 'MapR Hive ODBC Connector',
#   Host = 'dwh-prod-mapr-master-02',
#   Schema = 'vgregor',
#   UID = 'mapr',
#   PWD = 'mapr',
#   Port = 10000
# )

## Set  up pipeline folder if missing
pipeline <- file.path('2_pipeline', NAME)
if (!dir.exists(pipeline)) {
  dir.create(pipeline)
  dir.create(file.path(pipeline, 'out'))
}


# ---------
# Main code
# ---------
## Define and transform dataset
dfAll <- dfLoad[
  source == "marketing" & register_platform == "google play",
  .(
    player_id,
    register_month,
    tier,
    dx_pay_count,
    dx_payer,
    dx_active_days,
    d0_session_count,
    d1x_session_count,
    d1x_daily_session_count_relative_to_d0,
    dy_payer
  )]
dfAll[, tier := as.numeric(tier)]
dfAll[, dy_payer := factor(dy_payer)]

## Define relevant register months
allRegMonths <- sort(unique(dfAll$register_month))
testRegMonths <- allRegMonths[4:length(allRegMonths)]

## Loop through register months and train relevant models
K <- 5
dfTestList <- list()

for(i in 1:length(testRegMonths)) {
  ## define relevant months
  testRegMonth <- testRegMonths[i]
  trainRegMonths <- allRegMonths[i:(i+2)]
  ## define train-validation & test datasets
  dfTrainVal <- dfAll[
    register_month >= min(trainRegMonths) &
      register_month <= max(trainRegMonths)]
  dfTrainVal[, c('player_id', 'register_month') := NULL]
  dfTest <- dfAll[register_month == testRegMonth]
  ## run CV to get optimal cut-off
  cvResult <- makeCrossValCutOff(K = K, data = dfTrainVal)
  optimalCutOff <- round(mean(cvResult$optimal_cutoff), 4)
  ## Train model on full train-validation dataset
  modTrainVal <- glm(
    formula = dy_payer ~ .,
    data = dfTrainVal,
    family = 'binomial'
  )
  ## Calculate predictions on test dataset and write results
  dfTest[, mod_fit := predict.glm(modTrainVal, newdata = dfTest, type = 'response')]
  dfTest[, mod_cut_off := optimalCutOff]
  dfTestList[[i]] <- dfTest
}

## Bind the results together and save it to hive
dfTestFinal <- rbindlist(dfTestList)
fwrite(
  dfTestFinal,
  file = file.path(
    '2_pipeline', NAME, 'out', resultTableName %+% '.csv'
  )
)

# print('Writing results to hive')
# DBI::dbSendQuery(con, 'drop table if exists ' %+% resultTableName)
# DBI::dbWriteTable(
#   con, resultTableName,
#   dfTestFinal[, .(player_id, dy_payer, mod_fit, mod_cut_off)]
# )
# print('Saving done')