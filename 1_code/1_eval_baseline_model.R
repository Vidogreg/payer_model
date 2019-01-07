# ------------
# Introduction
# ------------
## This file evaluates the baseline model.
## Model: A player is a payer in the prediction interval (dy)
##        iff he is a payer in the observation period (dx)
## We calculate the confusion matrix, sensitivity & precision.
## And also the total count of payers (predicted and actual).

NAME <- '1_eval_baseline_model'
PROJECT <- 'payer_model'
PROJECT_DIR <- file.path(
  'C:/Users/vgregor/OneDrive - PXFD',
  '_PIXEL FEDERATION/GA issues/Games General/GA-972 Payer model'
)


# ------------
# Preamble
# ------------
## Imports
source('1_code/00_utils.R')
packageTest('data.table')
packageTest('caret')

## Set working directory
setwd(file.path(PROJECT_DIR, PROJECT))

## Set  up pipeline folder if missing
pipeline <- file.path('2_pipeline', NAME)
if (!dir.exists(pipeline)) {
  dir.create(pipeline)
  for (folder in c('out', 'store', 'tmp')) {
    dir.create(file.path(pipeline, folder))
  }
}


# ---------
# Main code
# ---------
## Load data and create DT
if(!exists('dfSample'))
  dfSample <- data.table(readRDS(file.path(
    '2_pipeline', '0_load_data', 'out', 'dataset_v0_sample_seed_1.rds'
  )))

DT <- dfSample[
  , .(
    player_id,
    y_pred = as.factor(dx_payer),
    y_real = as.factor(dy_payer)
  )
]


## Calculate confusion matrix
confM <- confusionMatrix(
  data = DT$y_pred,
  reference = DT$y_real,
  positive = 'TRUE'
)
TP <- confM$table['TRUE', 'TRUE']             # TP - true positive
TN <- confM$table['FALSE', 'FALSE']           # TN - true negative
FP <- confM$table['TRUE', 'FALSE']            # FP - false positive
FN <- confM$table['FALSE', 'TRUE']            # FN - false negative
total <- sum(confM$table)

# accuracy is not very informative because of the huge class imbalance
# error rate/misclassification rate = 1 - accuracy
accuracy <- (TP + TN)/total
# confM$overall[1]

# sensitivity/recall - true positive rate
# This is important for us
TPR <- TP/(FN + TP)
# confM$byClass[1]

# specificity - true negative rate
# This will always be very high - not that important
# false positive rate = 1 - TNR
TNR <- TN/(FP + TN)
# confM$byClass[2]

# precision
# This is important for us
precision <- TP/(FP + TP)
# confM$byClass[5]

# payer count
payers <- list(
  predictedCount = sum(confM$table[2, ]),
  referenceCount = sum(confM$table[, 2])
)
payers$relDiff <- payers$predictedCount/payers$referenceCount

# We are mainly interested in precision and sensitivity
# sensitivity - how many of actual payers have we identified
# precision   - how many of predicted payers are actual payers


## Print results in .pdf
pdf(file.path(
  '2_pipeline', NAME, 'out', 'baseline_model_eval_output.pdf'
))
printOutput(confM)
printOutput(list(
  confM$byClass[c(1, 5)],
  payers
))
dev.off()


# ----------
# Leftovers
# ----------
# https://sebastianraschka.com/blog/2016/model-evaluation-selection-part1.html