# Function for evaluating v4 model by days in game 
evalModelV4ByDig <- function(project, dfAll, digs) {
  # Open .pdf to write
  filePath = file.path(
    '2_pipeline', NAME, 'out', project %+% '_model_v4_by_dig_' %+% randomSeed %+% '.pdf'
  )
  pdf(filePath)
  # Loop through all days in game
  for(dig in digs){
    ## Define datasets
    dat <- dfAll[
      days_in_game == dig,
      .(
        tier_payers,
        dx_pay_count,
        dx_active_days,
        d0_session_count,
        dy_payer
      )]
    set.seed(randomSeed)
    datDown <- data.table(
      downSample(x = dat[, -ncol(dat), with = F], y = dat$dy_payer)
    )
    setnames(datDown, old = c('Class'), new = c('dy_payer'))
    datDown <- datDown[sample(nrow(datDown), sampleSizeDown)]
    
    ## Visualize down-sampled dataset
    print(densityFeaturePlot(
      datDown[, 1:4], datDown$dy_payer, layout = c(2, 2),
      main = 'Features, days in game = ' %+% dig
    ))
    
    ## Define train & test datasets
    trainIndex <- createDataPartition(
      dat$dy_payer, p = 0.8, list = FALSE, times = 1
    )
    datTrain <- dat[trainIndex, ]
    datTest <- dat[-trainIndex, ]
    
    ## Train model
    if(dig == 0) {
      mod <- glm(
        formula = dy_payer ~ tier_payers + dx_pay_count + d0_session_count,
        data = datTrain,
        family = 'binomial'
      )
    } else {
      mod <- glm(
        formula = dy_payer ~ .,
        data = datTrain,
        family = 'binomial'
      )
    }
    
    ## Evaluate models
    datTest[, mod_fit := predict.glm(mod, newdata = datTest, type = 'response')]
    rocObj <- roc(
      response = datTest$dy_payer,
      predictor = datTest$mod_fit,
      auc = TRUE
    )
    printOutput(summary(mod))
    plot.roc(
      rocObj, print.thres = TRUE, print.auc = TRUE,
      main = 'ROC curve, days in game = ' %+% dig
    )
  }
  # Close .pdf file
  dev.off()
}