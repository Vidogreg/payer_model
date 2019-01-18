# ------------
# Introduction
# ------------
## This file is ofr experimentation of new things.
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

if(!exists('dfSample'))
  dfSample <- data.table(readRDS(file.path(
    '2_pipeline', '0_load_data', 'out', 'dataset_v0_sample_seed_1.rds'
  )))

## Settings
randomSeed <- 1

## Set working directory
setwd(file.path(PROJECT_DIR, PROJECT))


# ---------
# Main code
# ---------
## Define dataset DT
DT <- dfSample[
  , .(
    dx_pay_count,
    dx_payer,
    dx_active_days,
    d0_session_count,
    dx_session_count,
    dy_payer
  )
  ]
DT[, dy_payer := factor(dy_payer)]

## Train the logit model
set.seed(randomSeed)
trainIndex <- createDataPartition(
  DT$dy_payer, p = 0.8, list = FALSE, times = 1
)
DTtrain <- DT[trainIndex, ]
DTtrainDown <- downSample(x = DTtrain[, -ncol(DTtrain), with = F], y = DTtrain$dy_payer)
DTtrainDown <- data.table(DTtrainDown)
setnames(DTtrainDown, old = c("Class"), new = c("dy_payer"))
DTtest <- DT[-trainIndex, ]

mod <- glm(
  formula = dy_payer ~ .,
  data = DTtrain,
  family = 'binomial'
)

modDown <- glm(
  formula = dy_payer ~ .,
  data = DTtrainDown,
  family = 'binomial'
)

## Predict on test dataset
DTtest[, mod_fit := predict.glm(mod, newdata = DTtest, type = 'response')]
## Evaluate the model and print the output to .pdf
evalClassModel(
  modelObj = mod,
  reference = DTtest$dy_payer,
  fit = DTtest$mod_fit,
  filePath = file.path(
    'TEMP_output_mod_' %+% randomSeed %+% '.pdf'
  )
)

## Predict on test dataset
DTtest[, mod_fit_down := predict.glm(modDown, newdata = DTtest, type = 'response')]
## Evaluate the model and print the output to .pdf
evalClassModel(
  modelObj = modDown,
  reference = DTtest$dy_payer,
  fit = DTtest$mod_fit_down,
  filePath = file.path(
    'TEMP_output_modDown_' %+% randomSeed %+% '.pdf'
  )
)