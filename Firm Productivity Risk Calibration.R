# Getting calibrated values for "psi" using firm TFP risk
input_data = c(raw_dt = 'IntermediateFiles/raw_dt.feather', 
               dtcut = 'IntermediateFiles/dtcut_for_spreadsheets.feather', 
               TFPData = 'Data/TFPData_updated_2019.csv',
               FREDData = 'Data/FRED Data (Inflation and Interest Rates).feather')

ProfitShares = fread('Data/ProfitShare.csv')
ProfitShares = rbind(ProfitShares, data.table(year = c(2018, 2019),
                                              `Profit Share` = ProfitShares[year == 2017, `Profit Share`])
                   )[, theta := 1 / `Profit Share`]

tfp_data = fread(input_data['TFPData'])[, TFP_level := exp(TFP)] #[TFP > 1.5, TFP := 1.5][TFP < -1, TFP := -1]

dtcut = read_feather_dt(input_data['dtcut'])
dtcutmat = dtcut[, !c('UnappropriatedNetLoss', 'TreasuryStockAssets', 'UnbilledReceivables',
                     'InventoryStockRealEstateHeldforSale', 'InventoryStockOther',
                     'InventoryStockRealEstateHeldforDevelopment', 'PreferenceStockatCarryingValue',
                     'PreferredStockatCarryingValue', 'PremiumonPreferenceStock', 'PremiumonPreferredStock',
                     'PretaxIncomeDomestic', 'PretaxIncomeDomestic',
                     'indfmt', 'consol', 'popsrc', 'datafmt', 'curcd', 'conm',
                     'datadate', 'cusip', 'tic', 'cusip6')
               ]

testing = as.matrix(dtcutmat[, .SD, .SDcols = is.numeric])

# dtcutmat_with_factors = copy(dtcutmat)
# for (col in names(dtcutmat[, .SD, .SDcols = is.character]))
#         set(dtcutmat_with_factors,
#             j = col,
#             value = as.factor(dtcutmat_with_factors[[col]]))

# trying_again = mice::mice(testing, m = 1)

dt = dtcut[, `:=`(fyear = DataYearFiscal,
                  gvkey = as.integer(GlobalCompanyKey))
         ][tfp_data, on = .(fyear, gvkey), `:=`(TFP = i.TFP, TFP_level = i.TFP_level)
         ][ProfitShares, on = c(calendaryear = 'year'), theta := i.theta
         ][!is.na(TFP) & !is.na(SalesTurnoverNet)]

# aggregate_tfp = setkey(unique(dt[, .(average_TFP = Hmisc::wtd.mean(TFP_level, weights = SalesTurnoverNet)), calendaryear]), calendaryear)[]
# aggregate_tfp[, linear := predict(
#   lm(log(average_TFP) ~ calendaryear, data = aggregate_tfp[calendaryear >= 1980 & calendaryear < 2019]),
#   newdata = aggregate_tfp)]
# #some patchy reporting in 2019, I think it's a fiscal calendaryear thing
# aggregate_tfp[, little_zeta := exp(log(average_TFP) - linear)]
# dt[aggregate_tfp, on = 'calendaryear', average_TFP := i.average_TFP]

# dt = dt[!is.na(SalesTurnoverNet)]
dt[, bigPsi := TFP_level
 ][, logBigPsi := TFP
 ][, marketShare := SalesTurnoverNet/sum(SalesTurnoverNet), calendaryear
 ][, bigPsiPhiContribution := marketShare * bigPsi ^ (2 - 1)
 ][, bigPhi := sum(bigPsiPhiContribution) ^ (1 / (2 - 1)), calendaryear
 ][, linearBigPhi := exp(predict(
       lm(log(bigPhi) ~ calendaryear, data = unique(dt, #[calendaryear >= 1970 & calendaryear < 2019],
                                            by = 'calendaryear')),
       newdata = dt))]

dt[, Psi := bigPsi / linearBigPhi,
 ][, logPsi := log(Psi) - log(linearBigPhi)
 ][, PhiContribution := marketShare * Psi ^ (theta - 1)
 ][, Phi := sum(PhiContribution) ^ (1 / (theta - 1)), calendaryear]

setkey(dt[, .(first(linearBigPhi), first(bigPhi), first(Phi)), calendaryear], calendaryear)[]

dt[, salesWeightedAverage := sum(marketShare * Psi), calendaryear]
phiData = setkey(dt[, .(Phi = first(Phi), salesWeightedAverage = first(salesWeightedAverage)), calendaryear], calendaryear)[]
phiData[, Phi := Phi/mean(Phi)][, logPhi := log(Phi)][, lagLogPhi := shift(log(Phi))]
summary(lm(logPhi ~ lagLogPhi + 0, data = phiData[calendaryear > 1985]))

setkey(dt, GlobalCompanyKey, calendaryear)
dt[, laglogPsi := shift(logPsi), GlobalCompanyKey]


Psi_AR_mod = lm(logPsi ~ laglogPsi + 0, weights = marketShare, data = dt)
LambdaPsi = coef(Psi_AR_mod)[1]

dt[, elogPsi := predict(Psi_AR_mod, newdata = dt)]

dt[, shock := logPsi - elogPsi]

dt_with_shocks = dt[, n_real_shocks := sum(!is.na(shock)), GlobalCompanyKey
                  ][n_real_shocks > 5
                  ][, totalMarketShare := sum(marketShare, na.rm = TRUE)
                  ][, marketShareOverWholePeriod := sum(na0(marketShare))/totalMarketShare, GlobalCompanyKey]


# clusters = makeCluster(7)
# registerDoSNOW(clusters)
# superlong = foreach(LOOCVyear = 2000:2006, .combine = rbind) %dopar% {
# eek = cov(dcast(dt_with_shocks[calendaryear != LOOCVyear,
#                                .(calendaryear, GlobalCompanyKey, shock)],
#                 calendaryear ~ GlobalCompanyKey,
#                 value.var = 'shock'
#                )[, -'calendaryear'],
#           use = 'pairwise.complete.obs')
# 
# # mean_covariance = mean(c(eek), na.rm = TRUE)
# # eek[is.na(eek)] = mean_covariance
# 
# # hm = data.table(GlobalCompanyKey = row.names(eek), eek)
# 
# loong = data.table(covariance = c(eek), x = rep(colnames(eek), each = nrow(eek)), y = rep(rownames(eek), times = ncol(eek)))
# 
# #DCAST dt_with_shocks HERE so you have 1 col/year
# #also you might need as.integer(Globalcompanykey)
# loong[dt_with_shocks[calendaryear == LOOCVyear],
#       on = c(y = 'GlobalCompanyKey'),
#       shock_in_given_year_for_firm_y := i.shock]
# loong[dt_with_shocks[calendaryear == LOOCVyear],
#       on = c(x = 'GlobalCompanyKey'),
#       shock_in_given_year_for_firm_x := i.shock]
# loong[, year := LOOCVyear]
# }
# stopCluster(clusters)
# 
# summary(lm(shock_74_y ~ shock_74_x * covariance, data = loong[x != y]))





dt_with_shocks[, has_shocks := sum(!is.na(shock) & calendaryear > 1979), GlobalCompanyKey]
dt_im_using = dt_with_shocks[has_shocks == 40 & calendaryear > 1979
                           ][, totalMarketShare := sum(marketShare, na.rm = TRUE)
                           ][, marketShareOverWholePeriod := sum(na0(marketShare))/totalMarketShare, GlobalCompanyKey]
market_shares = unique(dt_im_using[, .(GlobalCompanyKey, marketShareOverWholePeriod)])
good = cov(dcast(dt_im_using[, .(calendaryear, GlobalCompanyKey, shock)],
                calendaryear ~ GlobalCompanyKey,
                value.var = 'shock'
               )[, -'calendaryear'],
          use = 'all.obs')


set.seed(22)
n_sims = 1000000
simData = rockchalk::mvrnorm(n = n_sims, mu = rep(0, nrow(good)), good)

Psi_mat = matrix(rep(0, length(simData)),
                 nrow = nrow(simData),
                 dimnames = list(rownames(simData), colnames(simData)))

Psi_mat[1, ] = simData[1, ]
foreach(year = 2:nrow(simData)) %do% {
        Psi_mat[year, ] = Psi_mat[year - 1, ] * LambdaPsi + simData[year, ]
        NULL
}

analysis_table = data.table(logPsi = c(Psi_mat),
                            GlobalCompanyKey = rep(colnames(Psi_mat), each = nrow(Psi_mat)),
                            year = rep(1:nrow(Psi_mat), times = ncol(Psi_mat)))
averageTheta = 4
analysis_table[market_shares, on = .(GlobalCompanyKey), marketShare := i.marketShareOverWholePeriod
             ][, Psi := exp(logPsi)
             ][, phiUnweighted := sum(Psi ^ (averageTheta - 1)) ^ (1 / (averageTheta - 1)), year
             ][, phiWeighted := sum(marketShare ^ averageTheta * Psi ^ (averageTheta - 1)) ^ (1 / (averageTheta - 1)), year
             ][, phiWeighted_alt := sum(marketShare * Psi ^ (averageTheta - 1)) ^ (1 / (averageTheta - 1)), year]



phi_table = setkey(unique(analysis_table[, .(phiUnweighted, phiWeighted, phiWeighted_alt, year)]), year
                 )[, logPhiUnweighted := log(phiUnweighted)
                 ][, logPhiWeighted := log(phiWeighted)
                 ][, logPhiWeighted_alt := log(phiWeighted_alt)
                 ]

summary(lm(formula(paste0('logPhiUnweighted ~ ', paste0('shift(logPhiUnweighted, ', 1:20, ')', collapse = ' + '))), data = phi_table))

summary(lm(formula(paste0('logPhiWeighted_alt ~ ', paste0('shift(logPhiWeighted_alt, ', 1:20, ')', collapse = ' + '))), data = phi_table))

dt_with_shocks[, threeDigitNAICS := substr(as.character(NAICS), 1, 3)
             ][, sixDigitNAICS := as.character(NAICS)
             ]


price_deflators = read_feather_dt(input_data['FREDData']
                                )[, .(gdpdef_level, calendaryear = year)]
data[price_deflators, on = 'calendaryear', `:=`(gdpDef = i.GDPDEF)]

dt_with_shocks[, realAssets := AssetsTotal / gdpDef
             ][, deltaAssets := (realAssets - shift(realAssets)) / shift(realAssets), GlobalCompanyKey]

summary(lm(shock ~ GlobalCompanyKey + DataYearFiscal * calendaryear + AssetsTotal * calendaryear +
                   IntangibleAssetsTotal * calendaryear + LiabilitiesTotal * calendaryear +
                   threeDigitNAICS * calendaryear + sixDigitNAICS + deltaAssets))