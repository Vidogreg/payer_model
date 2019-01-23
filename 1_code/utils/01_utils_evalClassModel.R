## Function for printing the summary of a model into .pdf
packageTest('data.table')
packageTest('caret')
packageTest('pROC')

evalClassModel <- function(
  modelObj, reference, fit, filePath,
  liftChart = TRUE, rocCurve = TRUE
) {
  
  ### Lift chart object
  liftResult <- data.frame(
    class = factor(reference, levels = c(TRUE, FALSE)),
    model = fit
  )
  liftObj <- lift(class ~ model, data = liftResult)
  
  
  ### ROC curve object
  rocObj <- roc(
    response = factor(reference),
    predictor = fit,
    auc = TRUE
  )
  x <- rocObj$sensitivities + rocObj$specificities
  rocCutOff <- rocObj$thresholds[which(x == max(x))]
  
  
  ### Metrics for different cut-offs
  cutOffs <- rocObj$thresholds
  nCutOff <- length(cutOffs)
  TPR <- rep(0, nCutOff)
  TNR <- rep(0, nCutOff)
  FPR <- rep(0, nCutOff)
  PPV <- rep(0, nCutOff)
  posRelDiff <- rep(0, nCutOff)
  
  for (i in 1:nCutOff) {
    prediction <- factor(fit > cutOffs[i])
    confTable <- table(prediction, reference)
    
    if(length(levels(prediction)) < 2) {
      if(levels(prediction) == 'TRUE') {
        TP <- confTable['TRUE', 'TRUE']
        TN <- 0
        FP <- confTable['TRUE', 'FALSE']
        FN <- 0
      } else {
        TP <- 0
        TN <- confTable['FALSE', 'FALSE']
        FP <- 0
        FN <- confTable['FALSE', 'TRUE']
      }
    } else{
      TP <- confTable['TRUE', 'TRUE']
      TN <- confTable['FALSE', 'FALSE']
      FP <- confTable['TRUE', 'FALSE']
      FN <- confTable['FALSE', 'TRUE']
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
    posRelDiff[i] <- (TP + FP)/(TP + FN)
  }
  
  
  ### Confusion matrix for threshold based on ROC
  cutOffRoc <- rocCutOff
  prediction <- factor(fit > cutOffRoc)
  confMatrixRoc <- confusionMatrix(
    data = prediction,
    reference = reference,
    positive = 'TRUE'
  )
  
  
  ### Confusion matrix for threshold based on count of positives
  cutOffPosRelDiff <-
    rocObj$thresholds[min(which(abs(posRelDiff - 1) == min(abs(posRelDiff - 1))))]
  prediction <- factor(fit > cutOffPosRelDiff)
  confMatrixPosRelDiff <- confusionMatrix(
    data = prediction,
    reference = reference,
    positive = 'TRUE'
  )
  
  
  ### Printing
  pdf(filePath)
  printOutput(summary(modelObj))
  print(plot(liftObj, value = 80, main = 'Lift chart'))
  plot.roc(rocObj, print.thres = TRUE, print.auc = TRUE, main = 'ROC curve')
  plot(TPR, PPV, type = 'l', main = 'Sensitivity vs. Precision')
  cutOffsNonInf <- cutOffs
  cutOffsNonInf[cutOffsNonInf == -Inf] <- 0
  cutOffsNonInf[cutOffsNonInf == Inf] <- 1
  plot(
    cutOffsNonInf, posRelDiff, type = 'l', ylim = c(0, 2),
    main = 'predicted positives / actual positives'
  )
  printOutput(c(
    '1st matrix - threshold based on the ROC curve',
    cutOffRoc,
    '2nd matrix - threshold based on the predicted vs. actual count of positives',
    cutOffPosRelDiff
  ))
  printOutput(confMatrixRoc)
  printOutput(confMatrixPosRelDiff)
  # Interesting are:
  # Sensitivity = TPR
  # Pos Pred Value = precision
  dev.off()
}