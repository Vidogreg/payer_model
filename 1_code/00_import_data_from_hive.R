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
hiveTablesNew <- c(
  'da_payer_dataset_v4',
  'sy_payer_dataset_v4',
  'tsm_payer_dataset_v4'
)

## ---------------------
## Set working directory
## ---------------------
setwd(file.path(PROJECT_DIR, PROJECT))

# ---------
# Main code
# ---------
# Load from hive (old cluster)
con <- DBI::dbConnect(
  odbc::odbc(),
  dsn = 'dwh-prod-hive',
  UID = 'mapr',
  PWD = 'mapr'
)
for(hiveTable in hiveTables) {
  if(any(hiveTable %+% '.rds' == list.files('0_data'))){
    print(paste(Sys.time(), hiveTable, 'is already downloaded... skipped'))
  } else {
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
}

# Load from hive (new cluster)
con <- DBI::dbConnect(
  odbc::odbc(),
  dsn = 'dwh-prod-ht-hive',
  schema = 'vgregor'
)
for(hiveTable in hiveTablesNew) {
  if(any(hiveTable %+% '.rds' == list.files('0_data'))){
    print(paste(Sys.time(), hiveTable, 'is already downloaded... skip'))
  } else {
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
}