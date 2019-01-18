# ------------
# Introduction
# ------------
## This file evaluates a simple logistic regression model with only payment data.
NAME <- '2_logit_payments'
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
    '2_pipeline', '0_load_data', 'out', 'dataset_v1_sample_seed_1.rds'
  )))


## Settings
randomSeed <- 1


## Set working directory
setwd(file.path(PROJECT_DIR, PROJECT))


## Set  up pipeline folder if missing
pipeline <- file.path('2_pipeline', NAME)
if (!dir.exists(pipeline)) {
  dir.create(pipeline)
  for (folder in c('out', 'store', 'tmp')) {
    dir.create(file.path(pipeline, folder))
  }
}


# ---------
# Main code
# ---------
## Define dataset DT
DT <- dfSample[
  , .(
    dx_pay_count,
    dx_payer,
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
DTtest <- DT[-trainIndex, ]

mod <- glm(
  formula = dy_payer ~ dx_pay_count + dx_payer, data = DTtrain,
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
    '2_pipeline', NAME, 'out', 'model_eval_output_' %+% randomSeed %+% '.pdf'
  )
)