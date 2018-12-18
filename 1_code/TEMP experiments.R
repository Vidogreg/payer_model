source('1_code/00_utils.R')
if(!exists('dfSample'))
  dfSample <- data.table(readRDS(file.path(
    '2_pipeline', '0_load_data', 'out', 'dataset_v0_sample_seed_1.rds'
  )))

packageTest('data.table')
packageTest('caret')
library(AppliedPredictiveModeling)


# define the dataset
DT <- dfSample[
  , .(
    register_platform,
    source,
    tier,
    is_us,
    dx_pay_count,
    dx_payer,
    dy_payer
  )
]
DT[, dx_payer := as.factor(dx_payer)]
DT[, dy_payer := as.factor(dy_payer)]
# DT[, tier := as.integer(tier)]



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



# One predictor logistic regression model

# train is useless for log model, it is only for parameter tuning
# model1 <- train(
#   dy_payer ~ dx_pay_count, data = DT,
#   method = 'glm', family = 'binomial'
# )

trainIndex <- createDataPartition(
  DT$dy_payer, p = 0.8, list = FALSE, times = 1
)
DTtrain <- DT[trainIndex, ]
DTtest <- DT[-trainIndex, ]

mod1 <- glm(
  formula = dy_payer ~ dx_pay_count, data = DTtrain,
  family = 'binomial'
)

for(cutOff in seq(0.1, 0.9, by = 0.1)) {
  
  DTtest[, mod1_pred := as.factor(mod1_fit > cutOff)]
  
  confM <- confusionMatrix(
    data = DTtest$mod1_pred,
    reference = DTtest$dy_payer,
    positive = 'TRUE'
  )
  print(confM$table)
  print(confM$byClass[1])
  print(confM$byClass[5])
  
}


liftResult <- data.frame(class = DTtest$dy_payer, model = DTtest$mod1_fit)
liftObj <- lift(class ~ model, data = liftResult)
plot(liftObj, value = 80, auto.key = list(columns = 1, lines = TRUE, points = FALSE))



