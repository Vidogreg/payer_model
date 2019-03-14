# ------------
# Introduction
# ------------
## Here, we calculate optimal_cutoff_points for payer model used in MKT prod
NAME <- '10_logit_v4_calculate_optimal_cutoff'


# ------------
# Preamble
# ------------
## Imports
source('1_code/00_utils.R')
packageTest('DBI')
packageTest('odbc')

## Settings & working directory
setwd(file.path(PROJECT_DIR, PROJECT))

## Set up pipeline folder if missing
pipeline <- file.path('2_pipeline', NAME)
if (!dir.exists(pipeline)) {
  dir.create(pipeline)
  dir.create(file.path(pipeline, 'out'))
}


# ---------
# Main code
# ---------
con <- DBI::dbConnect(
  odbc::odbc(),
  dsn = 'dwh-prod-ht-hive',
  schema = 'vgregor'
)
hiveTables <- c(
  'ga_972_payer_labels_v4',
  'ga_972_payer_features_v4'
)

# Create a table for results
predictMonthMin <- '2018-10-01'
# predictMonthMin <- '2018-10-31'
# trainMonthMin <- '2018-06-01'
# trainMonthMax <- '2018-06-30'

projects <- c('DA', 'SY', 'TSM')
platforms <- c('android', 'ios', 'canvas')
digs <- 0:7
# projects <- c('SY')
# platforms <- c('android')
# digs <- 0:1

result <- data.frame(matrix(
  nrow = length(projects)*length(platforms)*length(digs),
  ncol = 5
))
colnames(result) <- c(
  'project',
  'simplified_platform',
  'days_in_game',
  'predict_month',
  'optimal_cutoff'
)


# Cycle through all projects, platforms and days_in_game
j <- 1
for(project in projects) {
  for(platform in platforms) {
    for(dig in digs) {
      
      # Load data
      dfLoad <- list(labs = NULL, feats = NULL)
      for(i in 1:length(hiveTables)) {
        dfLoad[[i]] <- data.table(dbGetQuery(
          con,
          paste(
            'SELECT * ', 'FROM vgregor.',
            hiveTables[i], sep = '',
            ' WHERE ',
            'project = \"', project, '\" AND ',
            '((register_date >= \"2018-10-01\" AND register_date <= \"2018-10-31\") OR',
            '(register_date >= \"2018-06-01\" AND register_date <= \"2018-06-30\")) AND ',
            'days_in_game = ', dig
          )
        ))
        colnames(dfLoad[[i]]) <- gsub('.*\\.', '', colnames(dfLoad[[i]]))
        for (col in colnames(dfLoad[[i]])) {
          if(class(dfLoad[[i]][[col]]) == 'integer64')
            dfLoad[[i]][[col]] <- as.integer(dfLoad[[i]][[col]])
        }
        print(hiveTables[i])
      }
      
      # Join datasets
      dfLoad$feats <- dfLoad$feats[source == 'marketing', ]
      setkey(dfLoad$feats, project, player_id, days_in_game)
      setkey(dfLoad$labs, project, player_id, days_in_game)
      dfJoin <- merge(dfLoad$feats, dfLoad$labs, all.x = TRUE)
      dfJoin[, c('country', 'calculation_date', 'date_to', 'register_date.y') := NULL]
      setnames(dfJoin, old = c('register_date.x'), new = c('register_date'))
      dfJoin[, dy_payer := dy_pay_count > 0]
      
      # Train model on sample data
      dfTrain <- dfJoin[
        register_month == '2018-06-01' & register_platform %in% c('google_play', 'amazon'), ]
      mod <- glm(
        formula = dy_payer ~ tier + dx_pay_count + dx_active_days + d0_session_count,
        data = dfTrain,
        family = 'binomial'
      )
      dfTrain[, mod_fit := predict.glm(mod, newdata = dfTrain, type = 'response')]
      
      # Calculate cut-off
      cutOff <- getOptimalCutOff(dfTrain$mod_fit, dfTrain$dy_payer)
      
      # Write results
      result[j, ] <- c(project, platform, dig, predictMonthMin, cutOff)
      j <- j + 1
      
      print(paste('observations:', dim(dfTrain)[1]))
      print(paste(project, platform, dig, cutOff))
      
    }
  }
}

result$days_in_game <- as.numeric(result$days_in_game)
result$optimal_cutoff <- as.numeric(result$optimal_cutoff)

# write to csv (just in case)
filePath <- file.path(
  '2_pipeline', NAME, 'out', 'cutOffs.csv'
)
write.table(result, filePath, sep = ',')
# write to hive
# dbSendQuery(con, "drop table if exists ga_972_payer_optimal_cutoff_v4")
# dbWriteTable(con, 'ga_972_payer_optimal_cutoff_v4', result, overwrite = TRUE)