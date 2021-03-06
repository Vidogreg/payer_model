# ------------
# Introduction
# ------------
## This file evaluates a simple logistic regression model with payment & session data.
NAME <- '03_logit_pay&session'


# ------------
# Preamble
# ------------
## Imports
source('1_code/00_utils.R')
packageTest('data.table')

if(!exists('dfSample'))
  dfSample <- data.table(readRDS(file.path(
    '2_pipeline', '0_sample_data', 'out', 'dataset_v1_sample_seed_1.rds'
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
DTtest <- DT[-trainIndex, ]

mod <- glm(
  formula = dy_payer ~ .,
  data = DTtrain,
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