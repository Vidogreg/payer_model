# ------------
# Introduction
# ------------
## This file loads the v0 dataset and samples a smaller dataset later work
NAME <- '0_load_data'
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

## Settings
sampleSize <- 100000
randomSeed <- 1

fileName <- 'ga_972_payer_dataset_v1.rds'

## Set working directory
setwd(file.path(PROJECT_DIR, PROJECT))

## Set  up pipeline folder if missing
pipeline <- file.path('2_pipeline', NAME)
if (!dir.exists(pipeline)) {
  dir.create(pipeline)
  for (folder in c('out', 'store', 'tmp')){
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

for (col in colnames(dfSample)) {
  if(class(dfSample[[col]]) == 'integer64')
    dfSample[[col]] <- as.integer(dfSample[[col]])
}

## Save the sample
saveRDS(
  dfSample,
  file = file.path(
    '2_pipeline', NAME, 'out',
    'dataset_v0_sample_seed_' %+% randomSeed %+% '.rds'
  )
)