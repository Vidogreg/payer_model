# ------------
# Introduction
# ------------

## This file downloads data from hive and saves them locally
NAME <- 'import_data_from_hive'
PROJECT <- 'payer_model'
PROJECT_DIR <- file.path(
  'C:/Users/vgregor/OneDrive - PXFD',
  '_PIXEL FEDERATION/GA issues/Games General/GA-972 Payer model'
)

# ------------
# Preamble
# ------------

## -------
## Imports
## -------
source('1_code/00_utils.R')

packageTest('DBI')
packageTest('odbc')
packageTest('data.table')

## --------
## Settings
## --------

hiveTables <- c(
  'ga_972_payer_dataset_v0',
  'ga_972_payer_dataset_v1',
  'ga_972_payer_dataset_v2',
  'ga_972_payer_dataset_v3'
)

## ---------------------
## Set working directory
## ---------------------

setwd(file.path(PROJECT_DIR, PROJECT))

# ---------
# Main code
# ---------

con <- DBI::dbConnect(
  odbc::odbc(),
  Driver = 'MapR Hive ODBC Connector',
  Host = 'dwh-prod-mapr-master-02',
  Schema = 'vgregor',
  UID = 'mapr',
  PWD = 'mapr',
  Port = 10000
)

for(hiveTable in hiveTables) {
  
  print(paste(Sys.time(), hiveTable, 'querying...'))
  query <- paste(
    'SELECT * ', 'FROM vgregor.',
    hiveTable, sep = ''
  )
  dfLoad <- data.table(dbGetQuery(con, query))
  
  for (col in colnames(dfLoad)) {
    if(class(dfLoad[[col]]) == 'integer64')
      dfLoad[[col]] <- as.integer(dfLoad[[col]])
  }
  
  print(paste(Sys.time(), hiveTable, 'saving to .rds'))
  saveRDS(dfLoad, file = file.path('0_data', paste(hiveTable, '.rds', sep = '')))
  print(paste(Sys.time(), hiveTable, 'saved to .rds'))
  
}