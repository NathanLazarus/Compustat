library(data.table)
library(RPostgres)
library(openxlsx)
setwd("C:/Users/Nathan/Downloads/Compustat")

login_info = fread('CompustatPassword.csv')

wrds = dbConnect(Postgres(),
                 host='wrds-pgdata.wharton.upenn.edu',
                 port=9737,
                 user=login_info$username,
                 password=login_info$password,
                 dbname='wrds',
                 sslmode='require')

data.comp.funda = dbGetQuery(wrds,"select GVKEY, DATADATE, FYR, FYEAR, INDFMT, CONSOL, POPSRC,
                            DATAFMT, TIC, CUSIP, CONM, CURCD, AT, CSHO, GDWL, INTAN, INTANO, LT,
                            PSTK, PSTKRV, PSTKL, PSTKC, PSTKN, PSTKR,UPMPF,UPMPFS,UPSTK,UPSTKC,
                            EXCHG, COSTAT, PRCC_C, MKVALT, PRCC_F, SICH, NAICSH,
                            IVAEQ, IVAO, PPENT, ACT, AO, CHE, CH, IVST,
                            ACO, INVT, INVFG, INVO, INVRM, INVWIP, INVOFS, INVREH, INVRES, INVREI,
                            RECT, RECCO, RECD, RECTR, RECUB,
                            FATE, FATN, FATO, PPENME, PPENNR, PPENO,
                            FATB,FATC,FATL,FATP,PPENB,PPENC,PPENLI,PPENLS,PPEGT,
                            AOX, DC, EA, XPP, UNL, TSA, REVT,
                            SALE, COGS, XLR, XSGA, XRD, XAD, EMP, DVT, OIBDP, DP, CAPX, DPACT,
                            XINT, TXFED, TXFO, TXO, TXS, TXT, TXC, TXDI, PI, PIDOM, PIFO, EBITDA, EBIT, EPSPI, EPSFI
                    from COMP.FUNDA")
# where DATAFMT='STD' and CONSOL='C' and POPSRC='D'") #INDFMT='INDL' and STD is unrestated data



data.comp.company = dbGetQuery(wrds,"select GVKEY, COSTAT, SIC, NAICS, CONM, FIC, LOC
                   from COMP.COMPANY")
# where DATAFMT='STD' and CONSOL='C' and POPSRC='D'") #INDFMT='INDL' and STD is unrestatd data


# AT, LT, SEQ, CEQ, PSTKL, PSTKRV, TXDITC, TXDB, ITCB,
# REVT, COGS, XINT, XSGA, IB, TXDI, DVC, ACT, CHE, LCT,
# DLC, TXP, DP, PPEGT, INVT


data.kld.history = data.table(dbGetQuery(wrds,"select COMPANYNAME, YEAR, CUSIP, TICKER, DOMICILE, PRO_CON_E,
                                           EMP_STR_A, EMP_STR_C, EMP_STR_D, EMP_STR_G,
                                           EMP_STR_J, EMP_STR_L, EMP_STR_M, EMP_STR_X, EMP_STR_NUM,
                                           EMP_CON_A, EMP_CON_B, EMP_CON_H, EMP_CON_X, EMP_CON_NUM
                             from KLD.HISTORY"))
setnames(data.kld.history,
         tolower(
         c('PRO_CON_E', 'EMP_STR_A', 'EMP_STR_C',
           'EMP_STR_D', 'EMP_STR_G',
           'EMP_STR_J', 'EMP_STR_L',
           'EMP_STR_M', 'EMP_STR_X', 'EMP_STR_NUM',
           'EMP_CON_A', 'EMP_CON_B',
           'EMP_CON_H', 'EMP_CON_X', 'EMP_CON_NUM')),
         c('Anticompetitive Practices', 'Union Relations Strength', 'Cash Profit Sharing',
           'Employee Involvement', 'Heatlh and Safety Strength',
           'Employee Relations Strength', 'Human Capital Development',
           'Labor Management Strength', 'Other Employee Relations Strength', 'Total Number of Employee Relations Strengths',
           'Union Relations Concerns', 'Health and Safety Concerns',
           'Labor Management Concerns', 'Other Employee Relations Concerns', 'Total Number of Employee Relations Concerns'))


saveRDS(data.table(data.comp.company),'companydata.rds')
saveRDS(data.table(data.comp.funda),'fundamentalsannualdata.rds')
saveRDS(data.table(data.kld.history),'KLD_stats_indicators.rds')
