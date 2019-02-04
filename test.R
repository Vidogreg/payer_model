# Load data
if(!exists('dfLoad')) {
  dfLoad <- data.table(readRDS(file.path('0_data', 'da_payer_dataset_v4.rds')))
  dfLoad[, dy_payer := factor(dfLoad$dy_pay_count > 0)]
}
dfAll <- dfLoad[
  source == 'marketing' & register_platform == 'google_play',
  .(
    player_id,
    days_in_game,
    register_month,
    tier,
    tier_payers,
    dx_pay_count,
    dx_active_days,
    d0_session_count,
    dy_payer
  )]
dat <- dfAll[
  days_in_game == 4,
  .(
    player_id,
    register_month,
    tier_payers,
    dx_pay_count,
    dx_active_days,
    d0_session_count,
    dy_payer
  )]
datTrain <- dat[register_month == '2018-09-01', ]
datTest <- dat[register_month == '2018-10-01', ]

# Describe data
print(dim(datTrain))
print(summary(datTrain))
print(dim(datTest))
print(summary(datTest))

# Fit model
mod <- glm(
  formula = dy_payer ~ tier_payers + dx_pay_count + dx_active_days + d0_session_count,
  data = datTrain,
  family = 'binomial'
)
# Evaluate model
datTest[, mod_fit := predict.glm(mod, newdata = datTest, type = 'response')]
print(auc(response = datTest$dy_payer, predictor = datTest$mod_fit))
print(summary(mod))
print(datTest[player_id %in% c(28400038, 28400760, 28402895, 28455020, 28617098)])