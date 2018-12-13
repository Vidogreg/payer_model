# ------------
# Introduction
# ------------

## This file loads v0 dataset
NAME <- '0_load_data'
PROJECT <- 'payer_model'
PROJECT_DIR <- file.path(
  'C:/Users/vgregor/OneDrive - PIXEL FEDERATION, s.r.o',
  '_PIXEL FEDERATION/GA issues/Games General/GA-972 Payer model'
)

# ------------
# Preamble
# ------------

## -------
## Imports
## -------

## --------
## Settings
## --------

fileName <- 'ga_972_payer_dataset_v0.rds'

## ---------------------
## Set working directory
## ---------------------
### The code below will traverse the path upwards until it finds the root folder of the project.

setwd(file.path(PROJECT_DIR, PROJECT))

## ----------------------------------
## Set  up pipeline folder if missing
## ----------------------------------
### The code below will automatically create a pipeline folder for this code file if it does not exist.

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

## -- Load data from 0_data folder --
dfLoad <- readRDS(file.path('0_data', fileName))