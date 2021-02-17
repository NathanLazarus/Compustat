input_data = c(dtcutForSpreadsheets = 'IntermediateFiles/dtcut_for_spreadsheets.feather')

output_files = c(profitsTaxEstimates = 'SpreadsheetOutputs/profits_tax_estimates.xlsx')

dtcut = read_feather_dt(input_data['dtcutForSpreadsheets'])

implied_r = 0.04
tax_rate = 0.5

companies_for_profits_tax_table = dtcut[grepl(paste0('COCA-COLA|PROCTER|AMERICAN\\ EXPRESS CO|ADOBE|APPLE INC|', 
                                                     'MICROSOFT|SALESFORCE|GENERAL MOTORS CO|AMERICAN ELECTRIC POWER|DOW INC|', 
                                                     'FORD MOTOR CO|CHEVRON CORP|JOHNSON & JOHNSON|INTL BUSINESS MACHINES CORP|^GENERAL ELECTRIC CO'), 
                                              conm, ignore.case = T) & calendaryear == 2019, 
                                        .(conm, DataYearFiscal, consol, GlobalCompanyKey, SalesTurnoverNet, SellingGeneralandAdministrativeExpense, 
                                          CostofGoodsSold, LiabilitiesTotal, AssetsTotal, IntangibleAssetsTotal, MktVal, 
                                          OperatingIncomeBeforeDepreciation, DepreciationandAmortization, CapitalExpenditures, 
                                          ResearchandDevelopmentExpense, Employees, PretaxIncome, PretaxIncomeDomestic, PretaxIncomeForeign, 
                                          InterestandRelatedExpenseTotal, IncomeTaxesFederal, IncomeTaxesForeign, IncomeTaxesTotal, 
                                          EarningsBeforeInterest, EarningsBeforeInterestandTaxes)
                                        ][, profits := PretaxIncome - 
                                            implied_r * (AssetsTotal - IntangibleAssetsTotal) + implied_r * LiabilitiesTotal
                                          ][, profits_3_pct_r := PretaxIncome - 
                                              0.03 * (AssetsTotal - IntangibleAssetsTotal) + 0.03 * LiabilitiesTotal
                                            ][is.na(PretaxIncomeForeign), PretaxIncomeForeign := PretaxIncome - PretaxIncomeDomestic
                                              ][, .(`Company Name` = conm, `Revenue` = round(SalesTurnoverNet), 
                                                    `Accounting Profits` = round(PretaxIncome), `Profits due to Market Power, 3% Interest Rate` = round(profits_3_pct_r), 
                                                    `Profits due to Market Power, 4% Interest Rate` = round(profits), `Profits Tax Liability` = round(pmax(tax_rate * profits, 0)), 
                                                    Assets = AssetsTotal, Intangibles = IntangibleAssetsTotal, Liabilities = LiabilitiesTotal, 
                                                    `R&D Expenses` = ResearchandDevelopmentExpense, `Reported U.S. Accounting Profits` = PretaxIncomeDomestic, `Reported non-U.S. Accounting Profits` = PretaxIncomeForeign, 
                                                    `Current U.S. Corporate Tax Liability` = IncomeTaxesFederal, `Foreign Corporate Tax Liability` = IncomeTaxesForeign)
                                                ]

write.xlsx(setorder(companies_for_profits_tax_table, -`Profits Tax`), 
           output_files['profitsTaxEstimates'])
