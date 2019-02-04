# ------------
# Introduction
# ------------
## Evaluation of different models for different days in game
NAME <- '08_logit_v4_by_dig&project'


# ------------
# Preamble
# ------------
## Imports
source('1_code/00_utils.R')

## Settings & working directory
setwd(file.path(PROJECT_DIR, PROJECT))
randomSeed <- 1024
sampleSizeDown <- 400

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
dfAllDa <- dfLoadDa[
  source == 'marketing' & register_platform == 'google_play',
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
digs <- unique(dfAllDa$days_in_game)
evalModelV4ByDig('DA', dfAllDa, digs)


## Seaport
if(!exists('dfLoadSy'))
  dfLoadSy <- data.table(readRDS(file.path('0_data', 'sy_payer_dataset_v4.rds')))
dfLoadSy[, dy_payer := factor(dfLoadSy$dy_pay_count > 0)]
dfAllSy <- dfLoadSy[
  source == 'marketing' & register_platform == 'google_play',
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
digs <- unique(dfAllSy$days_in_game)
evalModelV4ByDig('SY', dfAllSy, digs)


## TrainStation
if(!exists('dfLoadTsm'))
  dfLoadTsm <- data.table(readRDS(file.path('0_data', 'tsm_payer_dataset_v4.rds')))
dfLoadTsm[, dy_payer := factor(dfLoadTsm$dy_pay_count > 0)]
dfAllTsm <- dfLoadTsm[
  source == 'marketing' & register_platform == 'google_play',
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
digs <- unique(dfAllTsm$days_in_game)
evalModelV4ByDig('TSM', dfAllTsm, digs)