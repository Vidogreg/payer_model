
criterion <- "relDiffPosPred"
ths <- rocObj$thresholds
fit <- dfVal$mod_fit
ref <- dfVal$dy_payer
initTh <- 0.1







f <- function(x, fit, ref) {
  confMatrix <- table(fit > 0.1, ref)
  if(dim(confMatrix)[1] < 2) min(confMatrix)
  
  
  abs(confMatrix[1, 2] - confMatrix[2, 1])
}

uniroot(f, interval = c(0, 1), fit = fit, ref = ref)






cutOffOptimal <- getOptimalCutOff(
  fit = dfVal$mod_fit,
  ref = dfVal$dy_payer,
  ths = rocObj$thresholds,
  criterion = "relDiffPosPred",
  initTh = 0.1
)