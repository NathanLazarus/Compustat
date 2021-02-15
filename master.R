# Main Pipeline --------------------------
# * Get Data -----------------------------

source('ImportCompustat.R')
source('Get Fred Macro Data.R')

# * Clean Compustat Data -----------------

source('CompustatDataCleaning.R')
source('Z1Adjustment.R')
source('imputingNAICS.R')

# * Add Outside Data for Regressions -----

source('Clean KLD Data.R')
source('Clean BEA Capital Data.R')
source('addMarkups.R')
source('FernaldIT.R')
source('readSDC.R')
source('SoftwarePatents.R')
source('input_output_IT.R')
source('IT_employment.R')

# * Run Regressions ----------------------
source('Merge Data for Regressions.R')
source('cross_sectional_analysis.R')


# Other Stuff ----------------------------
# CorporateProfitsTaxRevenueEstimates.R
# Firm Age Distributions.R
# Firm Productivity Risk Calibration.R
# MWTW_decomposition.R
# Monopoly Wealth By IT Intensity Category.R


# Use these regexes for formatting -------
# (?<=[^ :<>`])([!=:]?=|%do%|%in%)(?![ ]) replace with " \1 " (operators without a space)
# (should add %%, < > <= >= %/% & | + - * / but be careful not to turn ?<= into ? <=)
# (?<!=[ ]?')(,)(?![ \n]) replace with ", " (commas not followed by a space)
