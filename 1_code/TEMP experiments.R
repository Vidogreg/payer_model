source('1_code/00_utils.R')
if(!exists('dfSample'))
  dfSample <- data.table(readRDS(file.path(
    '2_pipeline', '0_load_data', 'out', 'dataset_v0_sample_seed_1.rds'
  )))

packageTest('data.table')
packageTest('caret')
# packageTest('AppliedPredictiveModeling')


# define the dataset
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
DT[, avg_sessions_per_day := dx_session_count/dx_active_days]
DT[dx_active_days == 0, ]$avg_sessions_per_day <- 0
# DT[, diff_avg_d0_session_count := avg_sessions_per_day - d0_session_count]
yIndex <- which(colnames(DT) == 'dy_payer')
setcolorder(DT, c(colnames(DT)[-yIndex], colnames(DT)[yIndex]))


# # https://topepo.github.io/caret/visualizations.html
# # subsample dataset for practical inspection
# table(DT$dy_payer)
# DTdown <- data.table(downSample(x = DT[, -ncol(DT), with = FALSE], y = DT$dy_payer))
# names(DTdown)[names(DTdown) == 'Class'] <- 'dy_payer'
# table(DTdown$dy_payer)
# 
# featurePlot(
#   x = DTdown[, c(3, 5)],
#   y = DTdown[, ]$dy_payer,
#   plot = 'density',
#   adjust = 1.5,
#   auto.key = list(columns = 2)
# )
# featurePlot(
#   x = DTdown[, c(3, 5)],
#   y = DTdown[, ]$dy_payer,
#   plot = 'ellipse',
#   auto.key = list(columns = 2)
# )



# simple logistic regression model
set.seed(1)
trainIndex <- createDataPartition(
  DT$dy_payer, p = 0.8, list = FALSE, times = 1
)
DTtrain <- DT[trainIndex, ]
DTtest <- DT[-trainIndex, ]

mod1 <- glm(
  formula = dy_payer ~ dx_pay_count + dx_payer, data = DTtrain,
  family = 'binomial'
)
mod2 <- glm(
  formula = dy_payer ~ ., data = DTtrain,
  family = 'binomial'
)

mod <- mod2
print(summary(mod))
DTtest[, mod1_fit := predict.glm(mod, newdata = DTtest, type = 'response')]


# Lift chart
liftResult <- data.frame(
  class = factor(DTtest$dy_payer, levels = c(TRUE, FALSE)),
  model = DTtest$mod1_fit
)
liftObj <- lift(class ~ model, data = liftResult)
print(plot(liftObj, value = 80, auto.key = list(columns = 1, lines = TRUE, points = FALSE)))

# ROC curve
library(pROC)
rocObj <- roc(
  response = factor(DTtest$dy_payer, levels = c(TRUE, FALSE)),
  predictor = DTtest$mod1_fit,
  auc = TRUE
)
p <- plot.roc(rocObj, print.thres = TRUE, print.auc = TRUE)

# Confusion matrix for a cut off based on the ROC plot
cutOff <- 0.013
DTtest[, mod1_pred := factor(mod1_fit > cutOff)]
confM <- confusionMatrix(
  data = DTtest$mod1_pred,
  reference = DTtest$dy_payer,
  positive = 'TRUE'
)
print(cutOff)
print(confM$table)
print(confM$byClass[1])
print(confM$byClass[5])


# More plots - ROC & sensitivity vs. precision
# cutOffs <- seq(0.001, 0.05, by = 0.001)
cutOffs <- rocObj$thresholds
nCutOff <- length(cutOffs)
TPR <- rep(0, nCutOff)
TNR <- rep(0, nCutOff)
FPR <- rep(0, nCutOff)
PPV <- rep(0, nCutOff)
posCountRelDiff <- rep(0, nCutOff)

for (i in 1:nCutOff) {
  DTtest[, mod1_pred := factor(mod1_fit > cutOffs[i])]
  confM <- table(DTtest$mod1_pred, DTtest$dy_payer)
  
  if(length(levels(DTtest$mod1_pred)) < 2) {
    if(levels(DTtest$mod1_pred) == 'TRUE') {
      TP <- confM[1, 2]
      TN <- 0
      FP <- confM[1, 1]
      FN <- 0
    } else {
      TP <- 0
      TN <- confM[1, 1]
      FP <- 0
      FN <- confM[1, 2]
    }
  } else{
    TP <- confM[2, 2]
    TN <- confM[1, 1]
    FP <- confM[2, 1]
    FN <- confM[1, 2]
  }
  
  # Sensitivity = recall = hit rate = true positive rate
  TPR[i] <- TP/(TP + FN)
  # Specificity = selectivity = true negative rate
  TNR[i] <- TN/(TN + FP)
  # fallout = false positive rate
  FPR[i] <- 1 - TNR[i]
  # precision = positive predictive value
  PPV[i] <- TP/(TP + FP)
  # relative difference between count of predicted and actual positives
  posCountRelDiff[i] <- (TP + FP)/(TP + FN)
}

plot(FPR, TPR, type = 'l')
plot(TPR, PPV, type = 'l')
plot(cutOffs, posCountRelDiff, type = 'l')




# LEFTOVERS

# train is useless for log model, it is only for parameter tuning
# model1 <- train(
#   dy_payer ~ dx_pay_count, data = DT,
#   method = 'glm', family = 'binomial'
# )

# https://en.wikipedia.org/wiki/Confusion_matrix