# https://topepo.github.io/caret/subsampling-for-class-imbalances.html

# 4 subsampling techniques
library(caret)
set.seed(2969)
imbal_train <- twoClassSim(10000, intercept = -20, linearVars = 20)
imbal_test  <- twoClassSim(10000, intercept = -20, linearVars = 20)
table(imbal_train$Class)

set.seed(9560)
down_train <- downSample(
  x = imbal_train[, -ncol(imbal_train)],
  y = imbal_train$Class
)
table(down_train$Class)

set.seed(9560)
up_train <- upSample(
  x = imbal_train[, -ncol(imbal_train)],
  y = imbal_train$Class
)
table(up_train$Class)

library(DMwR)
set.seed(9560)
smote_train <- SMOTE(Class ~ ., data  = imbal_train)                         
table(smote_train$Class)

library(ROSE)
set.seed(9560)
rose_train <- ROSE(Class ~ ., data  = imbal_train)$data                         
table(rose_train$Class)


# Bagged clasification using CV - takes super long time :/
ctrl <- trainControl(
  method = "repeatedcv", repeats = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

set.seed(5627)
orig_fit <- train(
  Class ~ ., data = imbal_train,
  method = "treebag",
  nbagg = 50,
  metric = "ROC",
  trControl = ctrl
)

set.seed(5627)
down_outside <- train(
  Class ~ ., data = down_train,
  method = "treebag",
  nbagg = 50,
  metric = "ROC",
  trControl = ctrl
)

set.seed(5627)
up_outside <- train(
  Class ~ ., data = up_train, 
  method = "treebag",
  nbagg = 50,
  metric = "ROC",
  trControl = ctrl
)

set.seed(5627)
rose_outside <- train(
  Class ~ ., data = rose_train, 
  method = "treebag",
  nbagg = 50,
  metric = "ROC",
  trControl = ctrl
)

set.seed(5627)
smote_outside <- train(
  Class ~ ., data = smote_train, 
  method = "treebag",
  nbagg = 50,
  metric = "ROC",
  trControl = ctrl
)