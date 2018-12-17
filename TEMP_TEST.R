source('1_code/0_load_data.R')

packageTest('caret')



DT <- dfSample[, .(player_id, y_pred = as.factor(dx_payer), y_real = as.factor(dy_payer))]
DT[, loss := as.numeric(y_pred != y_real)]


# https://sebastianraschka.com/blog/2016/model-evaluation-selection-part1.html
n <- nrow(DT)
err <- sum(DT$loss)/n
accEst = 1 - err

z <- qnorm(0.975)
accVar <- accEst*(1 - accEst)/n
accConf <- c(accEst - z*sqrt(accVar), accEst + z*sqrt(accVar))


confusionMatrix(data = DT$y_pred, reference = DT$y_real)