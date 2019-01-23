# ------------
# Introduction
# ------------
## This file runs all utils scripts.
PROJECT <- 'payer_model'
PROJECT_DIR <- file.path(
  'C:/Users/vgregor/OneDrive - PXFD',
  '_PIXEL FEDERATION/GA issues/Games General/GA-972 Payer model'
)

## Set working directory
setwd(file.path(PROJECT_DIR, PROJECT))

## Source utils folder
fileArray <- list.files('1_code/utils')
for(fileName in fileArray) {
  source(paste('1_code/utils/', fileName, sep = ''))
}