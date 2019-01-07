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

mod2 <- glm(
  formula = dy_payer ~ ., data = DTtrain,
  family = 'binomial'
)

mod <- mod2
print(summary(mod))
DTtest[, mod1_fit := predict.glm(mod, newdata = DTtest, type = 'response')]



# LEFTOVERS

# train is useless for log model, it is only for parameter tuning
# model1 <- train(
#   dy_payer ~ dx_pay_count, data = DT,
#   method = 'glm', family = 'binomial'
# )

# https://en.wikipedia.org/wiki/Confusion_matrix