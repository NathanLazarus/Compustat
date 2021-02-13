login_info = fread('CompustatPassword.csv')

wrds = dbConnect(Postgres(),
                 host = 'wrds-pgdata.wharton.upenn.edu',
                 port = 9737,
                 user = login_info$username,
                 password = login_info$password,
                 dbname = 'wrds',
                 sslmode = 'require')

CompustatFundamentals =
  data.table(
    dbGetQuery(wrds,
               "select GVKEY, DATADATE, FYR, FYEAR, INDFMT, CONSOL, POPSRC,
                       DATAFMT, TIC, CUSIP, CONM, CURCD, AT, CSHO, GDWL, INTAN, INTANO, LT,
                       PSTK, PSTKRV, PSTKL, PSTKC, PSTKN, PSTKR,UPMPF,UPMPFS,UPSTK,UPSTKC,
                       EXCHG, COSTAT, PRCC_C, MKVALT, PRCC_F, SICH, NAICSH,
                       IVAEQ, IVAO, PPENT, ACT, AO, CHE, CH, IVST,
                       ACO, INVT, INVFG, INVO, INVRM, INVWIP, INVOFS, INVREH, INVRES, INVREI,
                       RECT, RECCO, RECD, RECTR, RECUB,
                       FATE, FATN, FATO, PPENME, PPENNR, PPENO,
                       FATB,FATC,FATL,FATP,PPENB,PPENC,PPENLI,PPENLS,PPEGT,
                       AOX, DC, EA, XPP, UNL, TSA, REVT,
                       SALE, COGS, XLR, XSGA, XRD, XAD, EMP, DVT, OIBDP, DP,
                       CAPX, DPACT, XINT, TXFED, TXFO, TXO, TXS, TXT, TXC, TXDI,
                       PI, PIDOM, PIFO, EBITDA, EBIT, EPSPI, EPSFI
                       from COMP.FUNDA"
               )
  )



CompustatCompany = 
  data.table(
    dbGetQuery(wrds,
               "select CONM, GVKEY, ADD1, ADD2, ADD3, ADD4, ADDZIP, BUSDESC,
                       CIK, CITY, CONML, COSTAT, COUNTY, DLRSN, EIN, FAX, FIC,
                       FYRC, GGROUP, GIND, GSECTOR, GSUBIND, IDBFLAG, INCORP, LOC,
                       NAICS, PHONE, PRICAN, PRIROW, PRIUSA, SIC,
                       SPCINDCD, SPCSECCD, SPCSRC, STATE, STKO, WEBURL, DLDTE, IPODATE
                from COMP.COMPANY"
               )
  )


KLDdata =
  data.table(
    dbGetQuery(wrds,
               "select COMPANYNAME, YEAR, CUSIP, TICKER, DOMICILE, PRO_CON_E,
                       EMP_STR_A, EMP_STR_C, EMP_STR_D, EMP_STR_G,
                       EMP_STR_J, EMP_STR_L, EMP_STR_M, EMP_STR_X, EMP_STR_NUM,
                       EMP_CON_A, EMP_CON_B, EMP_CON_H, EMP_CON_X, EMP_CON_NUM
                from KLD.HISTORY"
               )
  )

CRSPCompustatLink =
  data.table(
    dbGetQuery(wrds,"select GVKEY, LPERMCO, LPERMNO, LINKDT,
                            LINKENDDT, LINKTYPE, LIID, LINKPRIM
                     from CRSP.CCMXPF_LNKHIST"
               )
  )


CompustatNames =
  data.table(
    dbGetQuery(wrds,"select GVKEY, YEAR1, YEAR2, SIC, NAICS, CONM
                     from COMP.NAMES"
               )
  )

saveRDS(data.table(CompustatCompany),'Data/Company Data (fixed identifying variables).rds')
saveRDS(data.table(CompustatFundamentals),'Data/Annual Fundamentals (most variables, raw).rds')
saveRDS(data.table(KLDdata),'Data/KLD Data (Anticompetitive Practices).rds')
saveRDS(data.table(CRSPCompustatLink),'Data/CRSP Compustat Link (gvkey to permno and permco).rds')
saveRDS(data.table(CompustatNames),'Data/First and Last Year in Compustat.rds')
