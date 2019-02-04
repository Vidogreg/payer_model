# ------------
# Introduction
# ------------
## This file loads the v0 dataset and samples a smaller dataset for later work
NAME <- '00_sample_data'


# ------------
# Preamble
# ------------
## Imports
source('1_code/00_utils.R')
packageTest('data.table')

## Settings
sampleSize <- 100000
randomSeed <- 1024
dataVersion <- 2
fileName <- 'ga_972_payer_dataset_v' %+% dataVersion %+% '.rds'

## Set working directory
setwd(file.path(PROJECT_DIR, PROJECT))

## Set  up pipeline folder if missing
pipeline <- file.path('2_pipeline', NAME)
if (!dir.exists(pipeline)) {
  dir.create(pipeline)
  for (folder in c('out')){
    dir.create(file.path(pipeline, folder))
  }
}


# ---------
# Main code
# ---------
## Load data from 0_data folder
dfLoad <- data.table(readRDS(file.path('0_data', fileName)))

## Sample the dataset
set.seed(randomSeed)
dfSample <- dfLoad[sample(nrow(dfLoad), sampleSize)]

## Save the sample
saveRDS(
  dfSample,
  file = file.path(
    '2_pipeline', NAME, 'out',
    'dataset_v' %+% dataVersion %+% '_sample_seed_' %+% randomSeed %+% '.rds'
  )
)