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
packageTest('caret')
packageTest('pROC')

if(!exists('dfSample'))
  dfSample <- data.table(readRDS(file.path(
    '2_pipeline', '0_load_data', 'out', 'dataset_v0_sample_seed_1.rds'
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






# ## Lift chart object
# liftResult <- data.frame(
#   class = factor(DTtest$dy_payer, levels = c(TRUE, FALSE)),
#   model = DTtest$mod_fit
# )
# liftObj <- lift(class ~ model, data = liftResult)
# 
# 
# ## ROC curve object
# rocObj <- roc(
#   response = factor(DTtest$dy_payer),
#   predictor = DTtest$mod_fit,
#   auc = TRUE
# )
# x <- rocObj$sensitivities + rocObj$specificities
# rocCutOff <- rocObj$thresholds[which(x == max(x))]
# 
# 
# ## Metrics for different cut-offs
# cutOffs <- rocObj$thresholds
# nCutOff <- length(cutOffs)
# TPR <- rep(0, nCutOff)
# TNR <- rep(0, nCutOff)
# FPR <- rep(0, nCutOff)
# PPV <- rep(0, nCutOff)
# posRelDiff <- rep(0, nCutOff)
# 
# for (i in 1:nCutOff) {
#   DTtest[, mod_pred := factor(mod_fit > cutOffs[i])]
#   confTable <- table(DTtest$mod_pred, DTtest$dy_payer)
#   
#   if(length(levels(DTtest$mod_pred)) < 2) {
#     if(levels(DTtest$mod_pred) == 'TRUE') {
#       TP <- confTable['TRUE', 'TRUE']
#       TN <- 0
#       FP <- confTable['TRUE', 'FALSE']
#       FN <- 0
#     } else {
#       TP <- 0
#       TN <- confTable['FALSE', 'FALSE']
#       FP <- 0
#       FN <- confTable['FALSE', 'TRUE']
#     }
#   } else{
#     TP <- confTable['TRUE', 'TRUE']
#     TN <- confTable['FALSE', 'FALSE']
#     FP <- confTable['TRUE', 'FALSE']
#     FN <- confTable['FALSE', 'TRUE']
#   }
#   
#   # Sensitivity = recall = hit rate = true positive rate
#   TPR[i] <- TP/(TP + FN)
#   # Specificity = selectivity = true negative rate
#   TNR[i] <- TN/(TN + FP)
#   # fallout = false positive rate
#   FPR[i] <- 1 - TNR[i]
#   # precision = positive predictive value
#   PPV[i] <- TP/(TP + FP)
#   # relative difference between count of predicted and actual positives
#   posRelDiff[i] <- (TP + FP)/(TP + FN)
# }
# 
# 
# ## Confusion matrix for threshold based on ROC
# cutOffRoc <- rocCutOff
# DTtest[, mod_pred := factor(mod_fit > cutOffRoc)]
# confMatrixRoc <- confusionMatrix(
#   data = DTtest$mod_pred,
#   reference = DTtest$dy_payer,
#   positive = 'TRUE'
# )
# 
# 
# ## Confusion matrix for threshold based on count of positives
# cutOffPosRelDiff <-
#   rocObj$thresholds[min(which(abs(posRelDiff - 1) == min(abs(posRelDiff - 1))))]
# DTtest[, mod_pred := factor(mod_fit > cutOffPosRelDiff)]
# confMatrixPosRelDiff <- confusionMatrix(
#   data = DTtest$mod_pred,
#   reference = DTtest$dy_payer,
#   positive = 'TRUE'
# )
# 
# 
# ## PRINTING
# pdf(file.path('2_pipeline', NAME, 'out', 'model_eval_output_' %+% randomSeed %+% '.pdf'))
# printOutput(summary(mod))
# print(plot(liftObj, value = 80, main = 'Lift chart'))
# plot.roc(rocObj, print.thres = TRUE, print.auc = TRUE, main = 'ROC curve')
# plot(TPR, PPV, type = 'l', main = 'Sensitivity vs. Precision')
# cutOffsNonInf <- cutOffs
# cutOffsNonInf[cutOffsNonInf == -Inf] <- 0
# cutOffsNonInf[cutOffsNonInf == Inf] <- 1
# plot(cutOffsNonInf, posRelDiff, type = 'l', main = 'predicted positives / actual positives')
# printOutput(c(
#   '1st matrix - threshold based on the ROC curve',
#   cutOffRoc,
#   '2nd matrix - threshold based on the predicted vs. actual count of positives',
#   cutOffPosRelDiff
# ))
# printOutput(confMatrixRoc)
# printOutput(confMatrixPosRelDiff)
# # Interesting are:
# # Sensitivity = TPR
# # Pos Pred Value = precision
# dev.off()



# ----------
# Leftovers
# ----------

# ## Function for printing the summary of a model into .pdf
# evalClassModel <- function(
#   modelObj, reference, fit,
#   liftChart = TRUE, rocCurve = TRUE
# ) {
# 
#   packageTest('caret')
#   packageTest('pROC')
#   
#   ## Lift chart
#   if(liftChart) {
#     liftResult <- data.frame(
#       class = factor(reference, levels = c(TRUE, FALSE)),
#       model = fit
#     )
#     liftObj <- lift(class ~ model, data = liftResult)
#     pLift <- plot(liftObj, value = 80, main = 'Lift chart')
#   }
#   
#   ## ROC curve
#   if(rocCurve) {
#     rocObj <- roc(
#       response = factor(reference, levels = c(TRUE, FALSE)),
#       predictor = fit,
#       auc = TRUE
#     )
#     
#     x <- rocObj$sensitivities + rocObj$specificities
#     rocCutOff <- rocObj$thresholds[which(x == max(x))]
#   }
#   
#   ## Confusion matrix
#   cutOff <- rocCutOff
#   confM <- confusionMatrix(
#     data = factor(fit > cutOff),
#     reference = reference,
#     positive = 'TRUE'
#   )
#   
# 
#   print(cutOff)
#   print(confM)
#   # print(confM$table)
#   # print(confM$byClass[1])
#   # print(confM$byClass[5])
#   
#   
#   print(pLift)
#   print(plot(
#     rocObj, print.thres = TRUE, print.auc = TRUE,
#     main = 'ROC curve'
#   ))
#   print(summary(modelObj))
# 
# }
# 
# 
# modelObj <- mod; reference <- DTtest$dy_payer;
# prediction <- DTtest$mod1_pred; fit <- DTtest$mod1_fit;
# 
# evalClassModel(
#   modelObj = mod,
#   reference = DTtest$dy_payer,
#   fit = DTtest$mod1_fit
# )