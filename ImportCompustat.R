library(data.table)
library(RPostgres)
library(openxlsx)
setwd("C:/Users/Nathan/Downloads")
login_info = fread('Compustat/CompustatPassword.csv')
wrds = dbConnect(Postgres(),
                 host='wrds-pgdata.wharton.upenn.edu',
                 port=9737,
                 user=login_info$username,
                 password=login_info$password,
                 dbname='wrds',
                 sslmode='require')
res <- dbSendQuery(wrds,"select GVKEY, DATADATE, FYR, FYEAR, INDFMT, CONSOL, POPSRC,
                            DATAFMT, TIC, CUSIP, CONM, CURCD, AT, CSHO, GDWL, INTAN, INTANO, LT,
                            PSTK, PSTKRV, PSTKL, PSTKC, PSTKN, PSTKR,UPMPF,UPMPFS,UPSTK,UPSTKC
                            EXCHG, COSTAT, PRCC_C, MKVALT, PRCC_F, SICH, NAICSH,
                            IVAEQ, IVAO, PPENT, ACT, AO, CHE, CH, IVST,
                            ACO, INVT, INVFG, INVO, INVRM, INVWIP, INVOFS, INVREH, INVRES, INVREI,
                            RECT, RECCO, RECD, RECTR, RECUB,
                            FATE, FATN, FATO, PPENME, PPENNR, PPENO,
                            FATB,FATC,FATL,FATP,PPENB,PPENC,PPENLI,PPENLS,PPEGT
                            AOX, DC, EA, XPP, UNL, TSA
                    from COMP.FUNDA")
# where DATAFMT='STD' and CONSOL='C' and POPSRC='D'") #INDFMT='INDL' and STD is unrestated data
data.comp.funda = dbFetch(res, n = -1)

res <- dbSendQuery(wrds,"select GVKEY, COSTAT, SIC, NAICS, CONM, COSTAT, FIC, LOC
                   from COMP.COMPANY")
# where DATAFMT='STD' and CONSOL='C' and POPSRC='D'") #INDFMT='INDL' and STD is unrestatd data
data.comp.company = dbFetch(res, n = -1)


# SICH, NAICSH,
# AT, LT, SEQ, CEQ, PSTKL, PSTKRV, TXDITC, TXDB, ITCB,
# REVT, COGS, XINT, XSGA, IB, TXDI, DVC, ACT, CHE, LCT,
# DLC, TXP, DP, PPEGT, INVT

companydata = data.table(data.comp.company)
dt = data.table(data.comp.funda)

saveRDS(companydata,'Compustat/companydata.rds')
saveRDS(dt,'Compustat/fundamentalsannualdata.rds')