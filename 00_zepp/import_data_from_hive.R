# ------------
# Introduction
# ------------

## This file downloads data from hive and saves them locally
NAME <- 'import_data_from_hive'
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

library(DBI)
library(data.table)

## --------
## Settings
## --------

hiveTables <- c('ga_972_payer_dataset_v0')

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
  Schema = 'default',
  UID = 'mapr',
  PWD = 'mapr',
  Port = 10000
)

for(hiveTable in hiveTables) {
  
  query <- paste(
    'SELECT * ', 'FROM vgregor.',
    hiveTable, sep = ''
  )
  dfLoad <- data.table(dbGetQuery(con, query))
  saveRDS(dfLoad, file = file.path('0_data', paste(hiveTable, '.rds', sep = '')))
  print(paste(Sys.time(), hiveTable, 'saved to .rds'))
  
}