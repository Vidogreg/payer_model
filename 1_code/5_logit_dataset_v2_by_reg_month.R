# ------------
# Introduction
# ------------
## This file trains logistic model with payment&session data for several register months.
NAME <- '5_logit_dataset_v2_by_reg_month'
# PROJECT <- 'payer_model'
# PROJECT_DIR <- file.path(
#   'C:/Users/vgregor/OneDrive - PXFD',
#   '_PIXEL FEDERATION/GA issues/Games General/GA-972 Payer model'
# )


# ------------
# Preamble
# ------------
## Imports
source('1_code/00_utils.R')
packageTest('data.table')
packageTest('ggplot2')

if(!exists('dfLoad'))
  dfLoad <- data.table(readRDS(file.path(
    '0_data', 'ga_972_payer_dataset_v3.rds'
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
dfLoad[, tier := as.numeric(tier)]
dfLoad[, dy_payer := factor(dy_payer)]

regMonths <- sort(unique(dfLoad$register_month))
cvResultList <- list()

## train model for each register month using 5-fold CV
for(i in 1:length(regMonths)) {
  regMonth <- regMonths[i]
  ## take only relevant register_month
  dfAll <- dfLoad[
    source == "marketing" & register_month == regMonth,
    .(
      tier,
      dx_pay_count,
      dx_payer,
      dx_active_days,
      d0_session_count,
      d1x_session_count,
      d1x_daily_session_count_relative_to_d0,
      dy_payer
    )]
  
  ## Run cross-validation
  K <- 6
  cvResult <- NULL
  cvResult <- makeCrossValCutOff(K = K, data = dfAll)
  cvResult$register_month <- regMonth
  cvResult$fold <- 1:K
  ## Save it to list
  cvResultList[[i]] <- cvResult
  print('register month ' %+% regMonth %+% ' finished')
}
result <- rbindlist(cvResultList)

## Write the results in a .pdf
filePath <- file.path(
  '2_pipeline', NAME, 'out', 'model_by_reg_month' %+% randomSeed %+% '.pdf'
)
pdf(filePath)
ggplot(result, aes(factor(register_month), optimal_cutoff)) +
  geom_violin(aes(fill = factor(register_month)))
for(coef in colnames(result)[1:(dim(result)[2] - 3)]) {
  p <- ggplot(result, aes(factor(register_month), result[[coef]])) +
    geom_violin(aes(fill = factor(register_month))) +
    labs(y = coef)
  print(p)
}
dev.off()