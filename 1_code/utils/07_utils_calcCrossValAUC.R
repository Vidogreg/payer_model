## Function that calculates AUC using CV
calcCvAuc <- function(dat, K) {
  foldsIndex <- createFolds(dat$dy_payer, k = K)
  result <- list()
  for(k in 1:K) {
    ## train model
    dfTrain <- dat[-foldsIndex[[k]], ]
    mod <- glm(
      formula = dy_payer ~ .,
      data = dfTrain,
      family = 'binomial'
    )
    ## predict on test data
    dfTest <- dat[foldsIndex[[k]], ]
    dfTest[, mod_fit := predict.glm(mod, newdata = dfTest, type = 'response')]
    ## calculate AUC on test predictions
    auc <- auc(
      response = factor(dfTest$dy_payer),
      predictor = dfTest$mod_fit
    )
    ## write results
    result[[k]] <- auc
    print('Fold ' %+% k %+% '/' %+% K %+% ' done')
  }
  unlist(result)
}