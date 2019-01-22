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


## split dataset to train-validation and test
## We will train and tune the model only on train-validation dataset using CV
## test dataset will be used only in the end
set.seed(randomSeed)
trainIndex <- createDataPartition(
  dfAll$dy_payer, p = 0.8, list = FALSE, times = 1
)
dfTrainVal <- dfAll[trainIndex, ]
dfTest <- dfAll[-trainIndex, ]


## Run cross-validation on train-validation dataset
K <- 10
cvResult <- makeCrossValCutOff(K = K, data = dfTrainVal)
optimalCutOff <- mean(cvResult$optimal_cutoff)


## Train model on full train-val dataset
modTrainVal <- glm(
  formula = dy_payer ~ .,
  data = dfTrainVal,
  family = 'binomial'
)
dfTest[, mod_fit := predict.glm(modTrainVal, newdata = dfTest, type = 'response')]


## print results to .pdf
filePath <- file.path(
  '2_pipeline', NAME, 'out', 'model_CV_output_' %+% randomSeed %+% '.pdf'
)
pdf(filePath)
### Vizualize results of CV
vioplot(
  cvResult$`(Intercept)`,
  cvResult$tier,
  cvResult$dx_pay_count,
  cvResult$dx_payerTRUE,
  cvResult$dx_active_days,
  cvResult$d0_session_count,
  cvResult$dx_session_count,
  cvResult$optimal_cutoff
)
hist(cvResult$optimal_cutoff, prob = TRUE)
lines(density(cvResult$optimal_cutoff))
### Print confusion table with optimal cut-off
printOutput(list(optimalCutOff = optimalCutOff))
reference <- dfTest$dy_payer
prediction <- factor(dfTest$mod_fit > optimalCutOff)
printOutput(table(prediction, reference))
dev.off()


## Evaluate the model and print the output to .pdf
evalClassModel(
  modelObj = modTrainVal,
  reference = dfTest$dy_payer,
  fit = dfTest$mod_fit,
  filePath = file.path(
    '2_pipeline', NAME, 'out', 'model_eval_output_' %+% randomSeed %+% '.pdf'
  )
)




# ---------
# Leftovers
# ---------
# dfAll[, .(avg_fit = mean(mod_fit)), by = tier]