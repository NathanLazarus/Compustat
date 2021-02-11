library(data.table)
library(foreach)
library(doSNOW)
library(stringr)

rbind_and_fill = function(...) rbind(...,fill=T)

#TRANE CO, TRANE INC? Duplicates?

#This leaves financial firms and firms with missing market values in the data

setwd("C:/Users/Nathan/Downloads/Compustat")
companydata=readRDS('Data/Company Data (fixed identifying variables).rds')
dt = readRDS('Data/Annual Fundamentals (most variables, raw).rds')
# write_dta(companydata[dt[, c('year1', 'year2') := .(min(fyear), max(fyear)), gvkey], on = 'gvkey', `:=`(year1 = i.year1, year2 = i.year2)],
#           'C:/Users/Nathan/Downloads/Programs-20201123T183843Z-001/Programs/compustat_names.dta')
# write_dta(dt, 'C:/Users/Nathan/Downloads/Programs-20201123T183843Z-001/Programs/compustat_data.dta')

na0 = function(x) ifelse(!is.na(x),x,0)

# library(haven)
# dt_deloecker_eeckhout = dt[companydata, on='gvkey'
#                          ][datafmt == 'STD' & consol == 'C' &
#                              between(datadate, as.Date('1955-01-01'), as.Date('2016-12-31'))]
# 
# dt_deloecker_eeckhout[,(grep('i\\.',names(dt_deloecker_eeckhout),value = T)):=NULL]
# write_dta(dt_deloecker_eeckhout[, .(gvkey, datadate, fyear, indfmt, consol, popsrc, datafmt, conm, curcd,
#                                 cogs, csho, dvt, emp, intan, oibdp, ppegt, ppent, sale, xad, xlr, xrd,
#                                 xsga, costat, fic, prcc_c, mkvalt, prcc_f, naics)],
#           'C:/Users/Nathan/Downloads/DeloeckerEeckhout_v3/data/datafile.dta')


companydata[, sic := as.numeric(sic)]
companydata[, naics := as.numeric(naics)]
varnames = fread('Data/Variable Names and Descriptions.csv',header=F)
setnames(varnames,c('combined','varname','varfull'))
varnames[,lowervarname := tolower(varname)]
merge = varnames[data.table(lowervarname =tolower(names(dt))),on = 'lowervarname',nomatch = 0]
merge[, UpperCamel := gsub("[^[:alnum:]]","",varfull)]
setnames(dt,merge$lowervarname,merge$UpperCamel)

dt[companydata,on=c(GlobalCompanyKey='gvkey'),
   `:=`(currentsic=i.sic,currentnaics=i.naics,loc=i.loc)]
dt[, SIC:=StandardIndustrialClassificationHistorical]
dt[is.na(SIC),SIC:=currentsic]
dt[, NAICS:=NorthAmericaIndustrialClassificationSystemHistorical]
dt[is.na(NAICS),NAICS:=currentnaics]

dt[, calendaryear:=year(datadate)]
dt[, cusip6 := substr(cusip, 1, 6)]

KLD_data_clean = readRDS('IntermediateFiles/KLD Data Clean.rds')

dt[KLD_data_clean, on = c('cusip6', calendaryear = 'year'), `:=`(
  laborStrength = i.laborStrength, laborConcern = i.laborConcern,
  laborRelations = i.laborRelations, `Anticompetitive Practices` = `i.Anticompetitive Practices`
)]

fwrite(dt,'IntermediateFiles/raw_dt.csv')


dtcut = dt[curcd=='USD'&!is.na(curcd)
           &loc=='USA'
           &consol=='C'
           &datafmt=='STD'
           &AssetsTotal!=0
           &!is.na(SIC)] #the only firms missing SIC codes are firms that have yet to IPO. I don't understand the connection. They have NAICS codes.

dtcut = dtcut[dtcut[,.I[sum(indfmt=='INDL')==0|indfmt=='INDL'],.(GlobalCompanyKey,DataYearFiscal)]$V1] #I use indfmt FS and INDL and then drop FS reports when they're duplicated
dtcut[conm=='DELHAIZE AMERICA INC'&calendaryear==2001&CommonSharesOutstanding==91125.785,
      CommonSharesOutstanding:=dtcut[conm=='DELHAIZE AMERICA INC'&calendaryear==2000]$CommonSharesOutstanding]
dtcut[,MktVal:=MarketValueTotalFiscal]
dtcut[is.na(MktVal),MktVal:=PriceCloseAnnualFiscal*CommonSharesOutstanding]
dtcut[is.na(MktVal),MktVal:=PriceCloseAnnualCalendar*CommonSharesOutstanding]
dtcut[is.na(PreferredPreferenceStockCapitalTotal)&!is.na(PreferredPreferenceStockRedeemable),
      PreferredPreferenceStockCapitalTotal:=PreferredPreferenceStockRedeemable]

dtcut[,preferred:=pmax(PreferredPreferenceStockCapitalTotal,PreferredStockLiquidatingValue,PreferredStockRedemptionValue,PreferredStockConvertible,na.rm = T)]
dtcut[!is.na(preferred),MktVal:=MktVal+preferred]
dtcut = dtcut[MktVal != 0 | is.na(MktVal)] #drop about 100 firms with 0 common shares outstanding, mostly firms in the process of dissolving

dtcut[,haspreviousfiscalyear:=(DataYearFiscal-1)%in%DataYearFiscal,GlobalCompanyKey]
dtcut[,hascalendaryear:=DataYearFiscal%in%calendaryear,GlobalCompanyKey]
dtcut[,missing:=haspreviousfiscalyear&!hascalendaryear][,wasmissing:=0]
missings = dtcut[missing==T]
missings[,calendaryear:=DataYearFiscal][,wasmissing:=1]
dtcut = rbind(dtcut,missings)

setkey(dtcut,GlobalCompanyKey,calendaryear)
dtcut[,keep:=datadate==max(datadate),.(calendaryear,GlobalCompanyKey)]
dtcut = dtcut[keep==T]

dtcut = dtcut[calendaryear < 2020]

dtcut_without_utilities = dtcut[SIC %/% 100 == 49 & 
                                  as.numeric(str_pad(as.character(NAICS), width=6, side='right', pad='0')) %/% 1000 != 221]

#remove duplicates:


# duplicate_checking_data = merge(
#   dtcut_without_utilities,
#   companydata[, .SD, .SDcols = !c('conm', 'costat', 'loc',
#                                   'dlrsn', 'dldte', 'prican', 'prirow', 'priusa', 'idbflag', 'fyrc')],
#   by.x = 'GlobalCompanyKey',
#   by.y = 'gvkey')[, .SD, .SDcols = !c('curcd', 'currentsic', 'currentnaics', 'SIC', 'NAICS')
#                   ][, !(DataYearFiscal:cusip)
#                     ][, !(cusip6:keep)]
# duplicate_checking_data[, `:=`(datadate = as.numeric(as.Date(datadate)),
#                                ipodate = as.numeric(as.Date(ipodate)))]
# duplicate_checking_data[, `:=`(cik = as.numeric(cik),
#                                ggroup = as.numeric(ggroup),
#                                gind = as.numeric(gind),
#                                gsector = as.numeric(gsector),
#                                gsubind = as.numeric(gsubind),
#                                addzip = as.numeric(gsub('-| ','',addzip)),
#                                phone = as.numeric(gsub('-| |\\(|\\)','',phone)),
#                                fax = as.numeric(gsub('-| |\\(|\\)','',fax)),
#                                ein = as.numeric(gsub('-| ','',ein)))]
# 
# clusters = makeCluster(7)
# registerDoSNOW(clusters)
# 
# potential_duplicates = foreach(yr = unique(dtcut$calendaryear),
#                                .combine = rbind_and_fill,
#                                .multicombine = T,
#                                .packages = 'foreach'
#                                )%dopar%{
# 
#   checking_for_duplicates = duplicate_checking_data[calendaryear == yr]
#   checking_for_duplicates_numeric_cols = checking_for_duplicates[, .SD, .SDcols = is.numeric
#                                                                ][, .SD,  .SDcols = !c('exchg', 'EarningsPerShareDilutedIncludingExtraordinaryItems', 'calendaryear')]
#   n_firms = nrow(checking_for_duplicates)
#   numeric_cols_scores = foreach(i = 1:ncol(checking_for_duplicates_numeric_cols), .combine = `+`)%do%{
#     if(names(checking_for_duplicates_numeric_cols)[i] %in%
#                c('datadate', 'fyr', 'sic', 'naics', 'StandardIndustrialClassificationHistorical', 'NorthAmericaIndustrialClassificationSystemHistorical', 'ggroup', 'gind',
#                  'gsector', 'gsubind', 'spcindcd', 'spcseccd')
#        ) const = 4 else const = 10
#     vec = checking_for_duplicates_numeric_cols[[i]]
#     mat = t(replicate(n_firms, vec))
#     is_equal = mat == vec & vec != 0
#     is_close = mat > vec * 0.95 & mat < vec * 1.05 #this doesn't work for negative numbers, but there aren't many
#     score = const * is_equal + is_close
#     replace(score, which(is.na(score)), 0)
#   }
#   diag(numeric_cols_scores) = 0
#   # numeric_cols_results = data.table(similarity_score = c(numeric_cols_scores),
#   #                                   row = rep(1:n_firms, times = n_firms),
#   #                                   col = rep(1:n_firms, each = n_firms))
#   
#   checking_for_duplicates_char_cols = checking_for_duplicates[, .(state, incorp, add1, add2, add3, add4, city, county, spcsrc)] #ein fax phone
#   char_cols_scores = foreach(i = 1:ncol(checking_for_duplicates_char_cols), .combine = `+`)%do%{
#     vec = checking_for_duplicates_char_cols[[i]]
#     mat = t(replicate(n_firms, vec))
#     is_equal = mat == vec & vec != 0
#     score = 5 * is_equal
#     replace(score, which(is.na(score)), 0)
#   }
#   diag(char_cols_scores) = 0
#   
#   #wrap this in a function
#   vec = checking_for_duplicates$stko
#   mat = t(replicate(n_firms, vec))
#   is_subsidiary = mat == 1 | mat == 2 | vec == 1 | vec == 2
#   score = 10 * is_subsidiary
#   subsidiary_scores = replace(score, which(is.na(score)), 0)
#   diag(subsidiary_scores) = 0
#   
#   vec = checking_for_duplicates$exchg
#   mat = t(replicate(n_firms, vec))
#   is_subsidiary_stock_code = mat == 0 | vec == 0
#   score = 5 * is_subsidiary_stock_code
#   subsidiary_stock_scores = replace(score, which(is.na(score)), 0)
#   diag(subsidiary_stock_scores) = 0
#   
#   vec = as.integer(factor(checking_for_duplicates$costat, levels = c('A', 'I')))
#   mat = t(replicate(n_firms, vec))
#   both_active = mat == 1 & vec == 1
#   score = -5 * both_active
#   both_active_scores = replace(score, which(is.na(score)), 0)
#   diag(both_active_scores) = 0
#   
#   threshold_score = 80
#   results = data.table(similarity_score = c(numeric_cols_scores + char_cols_scores + subsidiary_scores + subsidiary_stock_scores + both_active_scores),
#                                     row = rep(1:n_firms, times = n_firms),
#                                     col = rep(1:n_firms, each = n_firms),
#                                     key = 'similarity_score'
#                        )[similarity_score > threshold_score][, pair_id := pmin(row, col) * 10000 + pmax(row, col)]
#   
#   most_likely_duplicates = setorder(results[results[, .I[which.max(similarity_score)], pair_id]$V1], -similarity_score)
#   duplicates_list = data.table(checking_for_duplicates[most_likely_duplicates$row, .(conm1 = conm, GlobalCompanyKey1 = GlobalCompanyKey)],
#                                checking_for_duplicates[most_likely_duplicates$col, .(conm2 = conm, GlobalCompanyKey2 = GlobalCompanyKey)],
#                                similarity_score = most_likely_duplicates$similarity_score)
#   # write.xlsx(duplicates_list, 'duplicates_list.xlsx')
#   #conm conml weburl
#   duplicates_list[, calendaryear := yr]
# }
# stopCluster(clusters)
# 
# # potential_duplicates[dtcut, on = c(conm1 = 'conm', 'calendaryear'), GlobalCompanyKey1 := i.GlobalCompanyKey
# #                    ][dtcut, on = c(conm2 = 'conm', 'calendaryear'), GlobalCompanyKey2 := i.GlobalCompanyKey
# #                    ]
# potential_duplicates[, uniqueid := fifelse(as.numeric(GlobalCompanyKey1)<as.numeric(GlobalCompanyKey2),
#                                            paste0(GlobalCompanyKey1, GlobalCompanyKey2),
#                                            paste0(GlobalCompanyKey2, GlobalCompanyKey1))]
# 
# pairs = unique(potential_duplicates, by = 'uniqueid')[, .(GlobalCompanyKey1, GlobalCompanyKey2, uniqueid, k = 1)]
# years = data.table(calendaryear = unique(potential_duplicates$calendaryear), k = 1)
# all_possible_year_pairs = pairs[years, on = 'k', allow.cartesian = T][, k := NULL]
# all_possible_year_pairs[just_compustat,
#                         on = c(GlobalCompanyKey1 = 'GlobalCompanyKey', 'calendaryear'),
#                         real_MW1 := i.real_MW
#                       ][just_compustat,
#                         on = c(GlobalCompanyKey2 = 'GlobalCompanyKey', 'calendaryear'),
#                         real_MW2 := i.real_MW]
# all_possible_year_pairs[, both_firms_appear_and_have_MW := !is.na(real_MW1) & !is.na(real_MW2)]
# 
# # would_pose_an_issue = merge(
# #   merge(potential_duplicates, just_compustat[, .(conm, calendaryear, real_MW1 = real_MW)],
# #         by.x = c('conm1', 'calendaryear'), by.y = c('conm', 'calendaryear')),
# #   just_compustat[, .(conm, calendaryear, real_MW2 = real_MW)],
# #   by.x = c('conm2', 'calendaryear'), by.y = c('conm', 'calendaryear'))[!is.na(real_MW1) & !is.na(real_MW2)]
# 
# would_pose_an_issue = merge(all_possible_year_pairs[both_firms_appear_and_have_MW == T],
#                             potential_duplicates[, .SD, .SDcols = !c('GlobalCompanyKey1', 'GlobalCompanyKey2')],
#                             by = c('uniqueid', 'calendaryear'), all.x = T, all.y = F
#                           )[, at_least_one_true_duplicate_year := sum(both_firms_appear_and_have_MW & !is.na(conm1)) > 0, uniqueid
#                           ][at_least_one_true_duplicate_year == T
#                           ][, c('at_least_one_true_duplicate_year', 'both_firms_appear_and_have_MW') := NULL]
# 
# would_pose_an_issue_with_one_row_per_firm = 
#   melt(would_pose_an_issue[, !(real_MW1:real_MW2)],
#      measure.vars = list(c('conm1', 'conm2'), c('GlobalCompanyKey1', 'GlobalCompanyKey2')),
#      variable.name = 'pair_id',
#      value.name = c('conm', 'GlobalCompanyKey'))
# to_check_for_duplicates_by_hand = merge(would_pose_an_issue_with_one_row_per_firm[, .SD, .SDcols = !c('conm')],
#                                         duplicate_checking_data,
#                                         by = c('GlobalCompanyKey', 'calendaryear'))
# to_check_for_duplicates_by_hand[, max_similarity_score := max(similarity_score, na.rm = T), uniqueid]
# to_check_for_duplicates_by_hand[readRDS('Data/firstyears.rds'), on = c(GlobalCompanyKey = 'gvkey'), `:=`(firstyear = i.year1, finalyear = i.year2)]
# to_check_for_duplicates_by_hand[, next_year := calendaryear + 1
#                               ][just_compustat, on = c('GlobalCompanyKey', next_year = 'calendaryear'),
#                                 `:=`(conm_next_year = i.conm, MW_next_year = i.real_MW)
#                               ][, c('Exists Next Year', 'Has MW Next Year') := .(!is.na(conm_next_year), !is.na(MW_next_year))
#                               ][, c('next_year', 'conm_next_year', 'MW_next_year') := NULL]
# setorder(to_check_for_duplicates_by_hand, -max_similarity_score, uniqueid, calendaryear, conm
#        )[, is_duplicate := +(similarity_score > 130 & !is.na(similarity_score))][, should_delete := NA_integer_]
# # would_pose_an_issue_unique = setorder(would_pose_an_issue[would_pose_an_issue[, .I[which.max(similarity_score)], uniqueid]$V1], -similarity_score)
# first_columns = c('conm', 'is_duplicate', 'should_delete', 'calendaryear', 'similarity_score',
#                   'PropertyPlantandEquipmentTotalGross', 'RevenueTotal', 'Employees',
#                   'Exists Next Year', 'Has MW Next Year', 'firstyear', 'finalyear',
#                   'naics', 'sic', 'add1', 'addzip', 'city', 'phone', 'fax')
# setcolorder(to_check_for_duplicates_by_hand, c(first_columns, names(to_check_for_duplicates_by_hand)[!names(to_check_for_duplicates_by_hand) %in% first_columns]))
# # write.xlsx(to_check_for_duplicates_by_hand, 'PotentialDuplicatesNewasdf.xlsx')

asdf = data.table(read.xlsx('Data/HandCodedDuplicates.xlsx'))
asdf[, total_should_delete_firm := sum(should_delete, na.rm = T), .(uniqueid, GlobalCompanyKey)]
asdf[, total_should_delete_pair := sum(should_delete, na.rm = T), uniqueid]
asdf[((total_should_delete_firm == 0 | total_should_delete_firm == -Inf) & total_should_delete_pair > 0) & is.na(should_delete), should_delete := 0]
asdf[total_should_delete_firm > 0 & total_should_delete_pair > 0 & is.na(should_delete), should_delete := 1]
asdf[is.na(should_delete) & similarity_score > 130 & pair_id == 1, should_delete := 1]
to_check_for_duplicates_by_hand[similarity_score > 100 & pair_id == 1, should_delete := 1]

dtcut[asdf[should_delete == 1, .(GlobalCompanyKey, calendaryear, should_delete, conm)], on = c('GlobalCompanyKey', 'calendaryear'), should_delete := i.should_delete]
dtcut[to_check_for_duplicates_by_hand[should_delete == 1], on = c('GlobalCompanyKey', 'calendaryear'), should_delete := i.should_delete]
dtcut = dtcut[should_delete != 1 | is.na(should_delete)]
dtcut[,AssetsOther:=AssetsOther-na0(DeferredCharges)-na0(PrepaidExpenses)]
dtcut[,intangibleratio:=IntangibleAssetsTotal/AssetsTotal]
setkey(dtcut,GlobalCompanyKey,calendaryear)

dtcut[,intangiblesadded:= +is.na(IntangibleAssetsTotal)]
dtcut_no_NA_intangibles = dtcut[!is.na(IntangibleAssetsTotal)]
setkey(dtcut_no_NA_intangibles,GlobalCompanyKey,calendaryear)
dtcut[,intangibleratio:=dtcut_no_NA_intangibles[dtcut,intangibleratio,roll='nearest']
    ][is.na(IntangibleAssetsTotal),`:=`(IntangibleAssetsTotal = pmin(intangibleratio*AssetsTotal,na0(Goodwill),pmax(na0(AssetsOther),0)),
                                        AssetsOther = na0(AssetsOther) - na0(pmin(intangibleratio*AssetsTotal,na0(Goodwill),pmax(na0(AssetsOther),0))))]

dtcut[,twodigitsic:=as.character(floor(SIC/100))]

intangiblemod = lm(intangibleratio~factor(calendaryear)+twodigitsic,data=dtcut)
dtcut[,predictedintangibleratio:=pmax(predict(intangiblemod,dtcut),0)
    ][is.na(IntangibleAssetsTotal),`:=`(IntangibleAssetsTotal = pmin(predictedintangibleratio*AssetsTotal,na0(Goodwill),pmax(na0(AssetsOther),0)),
                                        AssetsOther = na0(AssetsOther) - na0(pmin(predictedintangibleratio*AssetsTotal,na0(Goodwill),pmax(na0(AssetsOther),0))))]



fwrite(dtcut, 'IntermediateFiles/dtcut.csv', quote = T)