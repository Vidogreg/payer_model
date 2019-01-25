## Wrapper for training logit model
trainLogitModel <- function(dfTrain, dfTest) {
  ## train model
  mod <- glm(
    formula = dy_payer ~ .,
    data = dfTrain,
    family = 'binomial'
  )
  ## predict on test data
  fit <- predict.glm(mod, newdata = dfTest, type = 'response')
  list(
    modelObject = mod,
    modelFit = fit
  )
}

## Function that calculates AUC using CV
calcCvAuc <- function(dat, K, printProgress = TRUE) {
  foldsIndex <- createFolds(dat$dy_payer, k = K)
  result <- list()
  for(k in 1:K) {
    ## define train & test datasets
    dfTrain <- dat[-foldsIndex[[k]], ]
    dfTest <- dat[foldsIndex[[k]], ]
    ## train model
    model <- trainLogitModel(dfTrain, dfTest)
    dfTest[, mod_fit := model$modelFit]
    ## calculate AUC on test predictions
    auc <- auc(
      response = factor(dfTest$dy_payer),
      predictor = dfTest$mod_fit
    )
    ## write results
    result[[k]] <- auc
    if(printProgress) print('Fold ' %+% k %+% '/' %+% K %+% ' done')
  }
  unlist(result)
}

## Calculates AUC using CV on combinations of variables
calcCvAucCombinations <- function(combs, dat, cvK) {
  combResult <- list()
  N <- dim(combs)[2]
  for(i in 1:N) {
    varsSubset <- combs[, i]
    datDownSubset <- dat[, c(varsSubset, ncol(dat)), with = F]
    cvAuc <- calcCvAuc(datDownSubset, cvK, FALSE)
    combResult[[i]] <- c(
      paste(
        strtrim(sort(colnames(dat[, c(varsSubset), with = F])), 16),
        collapse = '+'
      ),
      signif(summary(cvAuc), 4)
    )
    print('combination ' %+% i %+% '/' %+% N %+% ' done')
  }
  combAuc <- data.table(matrix(
    unlist(combResult),
    nrow = length(combResult), byrow = T
  ))
  colnames(combAuc) <- c('variables', names(combResult[[1]])[-1])
  combAuc <- combAuc[, lapply(.SD, as.numeric), by = variables]
  combAuc[order(-Mean)]
}