source('1_code/00_utils.R')
if(!exists('dfSample'))
  dfSample <- data.table(readRDS(file.path(
    '2_pipeline', '0_load_data', 'out', 'dataset_v0_sample_seed_1.rds'
  )))

packageTest('data.table')
packageTest('caret')
library(AppliedPredictiveModeling)

DT <- dfSample[
  , .(
    register_platform,
    source,
    tier,
    is_us,
    dx_pay_count,
    dx_payer,
    dy_payer = as.factor(dx_payer)
  )
]






featurePlot(
  x = DT[, 5], 
  y = DT$dy_payer,
  plot = 'density'
)

featurePlot(
  x = DT$dx_pay_count,
  y = DT$dy_payer,
  plot = 'density',
  ## Pass in options to xyplot() to
  ## make it prettier
  scales = list(
    x = list(relation = 'free'),
    y = list(relation = 'free')
  ),
  adjust = 1.5,
  pch = '|',
  auto.key = list(columns = 2)
)








transparentTheme(trans = .4)

featurePlot(
  x = iris[, 1:4], 
  y = iris$Species, 
  plot = 'pairs',
  ## Add a key at the top
  auto.key = list(columns = 3)
)

featurePlot(
  x = iris[, 1:4], 
  y = iris$Species, 
  plot = 'ellipse',
  ## Add a key at the top
  auto.key = list(columns = 3)
)

transparentTheme(trans = .9)
featurePlot(
  x = iris[, 1:4], 
  y = iris$Species,
  plot = 'density', 
  ## Pass in options to xyplot() to 
  ## make it prettier
  scales = list(
    x = list(relation='free'), 
    y = list(relation='free')
  ), 
  adjust = 1.5, 
  pch = '|', 
  layout = c(4, 1), 
  auto.key = list(columns = 3)
)

featurePlot(
  x = iris[, 1:4], 
  y = iris$Species, 
  plot = 'box', 
  ## Pass in options to bwplot() 
  scales = list(
    y = list(relation='free'),
    x = list(rot = 90)
  ),  
  layout = c(4,1 ), 
  auto.key = list(columns = 2)
)