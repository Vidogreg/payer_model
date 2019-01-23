## Calculates the optimal cut-off based on fitted model and validation sample reference
## The objective is to have the predicted and actual count of positives as close as possible
getOptimalCutOff <- function(
  fit, ref, criterion = "relDiffPosPred"
) {
  
  f <- function(x, fit, ref) {
    confMatrix <- table(fit > x, ref)
    if(dim(confMatrix)[1] < 2) {
      if(row.names(confMatrix) == "TRUE") confMatrix[1, 1]
      else confMatrix[1, 2]
    } else abs(confMatrix[1, 2] - confMatrix[2, 1])
  }
  
  cutOff <- optimize(f, interval = c(0, 1), fit = fit, ref = ref)
  cutOff$minimum
  
}


## k-fold CV with optimal cutoff
makeCrossValCutOff <- function(K, data) {
  foldsIndex <- createFolds(data$dy_payer, k = K)
  
  for(k in 1:K) {
    dfTrain <- data[-foldsIndex[[k]], ]
    mod <- glm(
      formula = dy_payer ~ .,
      data = dfTrain,
      family = 'binomial'
    )
    
    dfVal <- dfAll[foldsIndex[[k]], ]
    dfVal[, mod_fit := predict.glm(mod, newdata = dfVal, type = 'response')]
    
    cutOffOptimal <- getOptimalCutOff(
      fit = dfVal$mod_fit,
      ref = dfVal$dy_payer,
      criterion = "relDiffPosPred"
    )
    
    if(k == 1) {
      cvResult <- NULL
      cvResult <- data.frame(matrix(
        c(
          mod$coefficients,
          cutOffOptimal,
          rep(0, (K - 1)*(length(mod$coefficients) + 1))
        ),
        nrow = K, byrow = TRUE
      ))
      colnames(cvResult) <- c(names(mod$coefficients), "optimal_cutoff")
    } else {
      cvResult[k, ] <- c(mod$coefficients, cutOffOptimal)
    }
    
    print('Fold #' %+% k %+% ' DONE')
  }
  
  cvResult
}