n_test = 100000
# n_firms = 50
LambdaPsi_test = 0.9
clusters = makeCluster(7)
registerDoSNOW(clusters)
test_results = foreach(n_firms = c(10, 50, 100, 200, 300, 500, 1000)) %dopar% {
  test = matrix(rnorm(n_firms * n_test), ncol = n_firms)
  
  Psi_test = matrix(rep(0, length(test)),
                   nrow = nrow(test),
                   dimnames = list(rownames(test), colnames(test)))
  
  Psi_test[1, ] = test[1, ]
  foreach(year = 2:nrow(test)) %do% {
          Psi_test[year, ] = Psi_test[year - 1, ] * LambdaPsi_test + test[year, ]
          NULL
  }
  
  test_analysis = data.table(logPsi = c(Psi_test),
                             firmid = rep(1:ncol(Psi_test), each = nrow(Psi_test)),
                             year = rep(1:nrow(Psi_test), times = ncol(Psi_test)))
  
  test_weights = data.table(firmid = 1:n_firms,
                            constantWeight = 1/n_firms,
                            unifRandomWeight = runif(n_firms),
                            logNormalRandomWeight = exp(rnorm(n_firms))
                          )[, unifRandomWeight := unifRandomWeight/sum(unifRandomWeight)
                          ][, logNormalRandomWeight := logNormalRandomWeight/sum(logNormalRandomWeight)]
  
  test_analysis[, Psi := exp(logPsi)
              ][test_weights, on = .(firmid), `:=`(constantWeight = i.constantWeight,
                                                   unifRandomWeight = i.unifRandomWeight,
                                                   logNormalRandomWeight = i.logNormalRandomWeight)]
  
  test_phi_table = test_analysis[,
                                 .(phiUnweighted = sum(Psi ^ (averageTheta - 1)) ^ (1 / (averageTheta - 1)),
                                   phiConstantWeight = sum(constantWeight * Psi ^ (averageTheta - 1)) ^ (1 / (averageTheta - 1)), 
                                   phiUnifRandomWeight = sum(unifRandomWeight * Psi ^ (averageTheta - 1)) ^ (1 / (averageTheta - 1)), 
                                   phiLogNormalRandomWeight = sum(logNormalRandomWeight * Psi ^ (averageTheta - 1)) ^ (1 / (averageTheta - 1))),
                                 year
                               ][, logPhiUnweighted := log(phiUnweighted)
                               ][, logPhiConstantWeight := log(phiConstantWeight)
                               ][, logPhiUnifRandomWeight := log(phiUnifRandomWeight)
                               ][, logPhiLogNormalRandomWeight := log(phiLogNormalRandomWeight)
                               ]
  
  list(
    summary(lm(formula(paste0('logPhiUnweighted ~ ', paste0('shift(logPhiUnweighted, ', 1:20, ')', collapse = ' + '))),
               data = test_phi_table)),
    summary(lm(formula(paste0('logPhiConstantWeight ~ ', paste0('shift(logPhiConstantWeight, ', 1:20, ')', collapse = ' + '))),
               data = test_phi_table)),
    summary(lm(formula(paste0('logPhiUnifRandomWeight ~ ', paste0('shift(logPhiUnifRandomWeight, ', 1:20, ')', collapse = ' + '))),
               data = test_phi_table)),
    summary(lm(formula(paste0('logPhiLogNormalRandomWeight ~ ', paste0('shift(logPhiLogNormalRandomWeight, ', 1:20, ')', collapse = ' + '))),
               data = test_phi_table))
  )
}
stopCluster(clusters)
test_results