# library(RCurl)
library(tidyverse)
library(data.table)
# library(devtools)

# # import custom functions
source("functions/getCryptoHistoricalPrice.R")
source("functions/finding_stationary_pair.R")

# ADF, H0: alfa=1, there is unit root, series is non-stationary, small p-val reject H0 so stationary yt, hight p-val > fail to reject H0, yt non-stat 
# desired p-val < 0.05 
# Breusch–Godfrey test: The H0: no serial correlation of any order up to p.
# desired BG p-val > 0.05, fail to rejecto H0
# testing integration order

# dataframe with combinations of crypto currencies
all_combinations <- readRDS("./data/all_combinations.RDS")
dim(all_combinations) # [1] 4950    2

# --------------------------------------- LOG PRICES  ------------------------------------------------------------------------------
# perform cointegration test for loaded combiantions of crypto currencies
source("functions/finding_stationary_pair.R")

if(0){
  all_combinations_cointegr <- get_cointegration_table(all_combinations,         # table with combinations of cryptocurrencies
                                                       standardize = FALSE,            # if prices should be standardized
                                                       in_sample = 365,          # how many observations in sample
                                                       oo_sample = 15,           # how many observations out of sample (these are out of procedure)
                                                       data_list = crypto_currencies_data,  # list with dataframes of each currency
                                                       include_dates = TRUE,     # id date included in result table
                                                       log_prices = TRUE,       # if prices should be logaritmized
                                                       johansen = TRUE
                                                       ) 
  
  saveRDS(all_combinations_cointegr, "./data/all_combinations_2coint_2.RDS")
}
all_combinations_cointegr <- readRDS("./data/all_combinations_2coint_2.RDS")

# look in structure of resluts
table(all_combinations_cointegr$results$conint_info)
# compare combin_adf data not available     not integrated 
#         119                764               4067 

cointegr_tb <- all_combinations_cointegr$results

# transform to numeric variable
cointegr_tb$combin_adf <- as.numeric(as.character(cointegr_tb$combin_adf))

# order according to value of combin_adf
cointegr_tb <- cointegr_tb[order(cointegr_tb$combin_adf, decreasing = T),]

# select only those with highest values
cointegr_tb_signf <- cointegr_tb[cointegr_tb$combin_adf < -3.8 & cointegr_tb$conint_info == "compare combin_adf",]

dim(cointegr_tb_signf)
# [1] 35 13

# saveRDS(cointegr_tb_signf, "./data/cointegr_tb_signf_5_7.RDS")
cointegr_tb_signf <- readRDS("./data/cointegr_tb_signf_5_7.RDS")


cointegr_tb_signf
# first_is    last_is n_obs           cc1 cc1_p_adf cc1_diff_p_adf                   cc2 cc2_p_adf cc2_diff_p_adf johansen combin_adf          coint_vec        conint_info
# coint_info.4219 2019-06-03 2020-06-01   365         verge    0.1317           0.01                 quant    0.0873           0.01      one  -3.818304  1 -4.4194 -0.5065 compare combin_adf
# coint_info.1514 2019-06-03 2020-06-01   365          tron    0.1304           0.01                komodo     0.228           0.01      one  -3.825258  1 -4.3933 -1.1765 compare combin_adf
# coint_info.3714 2019-05-31 2020-05-29   365          lisk    0.2862           0.01                   wax    0.3094           0.01      one  -3.828249   1 3.3886 -1.1805 compare combin_adf
# coint_info.4732 2019-06-03 2020-06-01   365 kucoin-shares    0.3204           0.01             bitshares    0.2938           0.01      one  -3.838055    1 3.903 -1.6625 compare combin_adf
# coint_info.1640 2019-06-03 2020-06-01   365           neo    0.2485           0.01 basic-attention-token    0.0565           0.01      one  -3.852065    1 3.3928 -0.794 compare combin_adf

# coint_info.30   2019-06-03 2020-06-01   365       bitcoin    0.1625           0.01              dogecoin    0.2845           0.01      one  -3.853532  1 13.2943 -0.8046 compare combin_adf
# coint_info.1996 2019-06-03 2020-06-01   365          iota    0.3013           0.01                 quant    0.0873           0.01      one  -3.872559  1 -2.5269 -0.6162 compare combin_adf
# coint_info.3976 2019-06-03 2020-06-01   365     ravencoin    0.2732           0.01                 quant    0.0873           0.01      one  -3.880001  1 -3.3125 -0.4691 compare combin_adf
# coint_info.4595 2019-05-31 2020-05-29   365         quant    0.0922           0.01                  aelf    0.1252           0.01      one  -3.894652   1 3.1761 -0.4373 compare combin_adf
# coint_info.4394 2019-06-03 2020-06-01   365          holo    0.2483           0.01                 quant    0.0873           0.01      one  -3.898169  1 -4.5373 -0.4018 compare combin_adf

# coint_info.1165 2019-06-03 2020-06-01   365       stellar    0.1209           0.01               siacoin    0.2882           0.01      one  -3.910338   1 3.6631 -0.9451 compare combin_adf
# coint_info.4730 2019-06-03 2020-06-01   365 kucoin-shares    0.3204           0.01            bittorrent    0.2654           0.01      one  -3.910902    1 8.079 -1.9716 compare combin_adf
# coint_info.4434 2019-06-03 2020-06-01   365        komodo     0.228           0.01                 bytom    0.2395           0.01      one  -3.912311   1 2.1909 -0.7823 compare combin_adf
# coint_info.2647 2019-06-03 2020-06-01   365      dogecoin    0.2845           0.01             flexacoin    0.1546           0.01      one  -3.914578  1 -4.2631 -1.6869 compare combin_adf
# coint_info.11   2019-06-03 2020-06-01   365       bitcoin    0.1625           0.01               stellar    0.1209           0.01      one  -3.954342  1 13.1892 -1.1531 compare combin_adf

# coint_info.2624 2019-05-31 2020-05-29   365      dogecoin    0.1777           0.01                  nano    0.2167           0.01      one  -3.961021 1 -10.0903 -1.7123 compare combin_adf
# coint_info.4597 2019-05-31 2020-05-29   365         quant    0.0922           0.01          maidsafecoin    0.2774           0.01      one  -3.968494   1 3.2421 -0.6836 compare combin_adf
# coint_info.1345 2019-05-31 2020-05-29   365  unus-sed-leo    0.4738           0.01              loopring    0.1446           0.01      one  -3.969396    1 3.469 -1.1756 compare combin_adf
# coint_info.3841 2019-05-31 2020-05-29   365          nano    0.2167           0.01                 quant    0.0922           0.01      one  -3.979070  1 -1.7384 -0.6003 compare combin_adf
# coint_info.4208 2019-05-31 2020-05-29   365      monacoin    0.0872           0.01          decentraland    0.1686           0.01      one  -4.051944   1 3.5542 -0.6583 compare combin_adf

# coint_info.4603 2019-06-03 2020-06-01   365 dxchain-token    0.6491           0.01         kucoin-shares    0.3204           0.01      one  -4.059705    1 1.3575 0.2127 compare combin_adf
# coint_info.3468 2019-05-31 2020-05-29   365          icon    0.3385           0.01                  lisk    0.2862           0.01      one  -4.060641  1 -1.4254 -0.9095 compare combin_adf
# coint_info.2285 2019-06-03 2020-06-01   365         zcash    0.3843           0.01       bitcoin-diamond    0.1248           0.01      one  -4.065792   1 3.9894 -0.9017 compare combin_adf
# coint_info.4426 2019-06-03 2020-06-01   365        komodo     0.228           0.01                 quant    0.0873           0.01      one  -4.088241  1 -1.8409 -0.5356 compare combin_adf
# coint_info.345  2019-05-31 2020-05-29   365           xrp    0.3262           0.01             ravencoin     0.262           0.01      one  -4.166859   1 1.5374 -1.4636 compare combin_adf

# coint_info.4591 2019-06-03 2020-06-01   365         quant    0.0873           0.01             bitshares    0.2938           0.01      one  -4.225558   1 4.7324 -0.6842 compare combin_adf
# coint_info.3479 2019-05-31 2020-05-29   365          icon    0.3385           0.01              monacoin    0.0872           0.01      one  -4.253992  1 -1.5192 -0.7973 compare combin_adf
# coint_info.1181 2019-06-03 2020-06-01   365       stellar    0.1209           0.01                 quant    0.0873           0.01      one  -4.286871  1 -3.5104 -0.6836 compare combin_adf
# coint_info.4589 2019-06-03 2020-06-01   365         quant    0.0873           0.01            bittorrent    0.2654           0.01      one  -4.475204    1 9.0323 -0.793 compare combin_adf
# coint_info.1140 2019-06-03 2020-06-01   365       stellar    0.1209           0.01              dogecoin    0.2845           0.01      one  -4.494921   1 4.5313 -0.5363 compare combin_adf

# coint_info.3117 2019-05-31 2020-05-29   365 kyber-network    0.9169           0.01             unibright    0.9681           0.01      one  -4.544210   1 0.1868 -2.4636 compare combin_adf
# coint_info.4574 2019-06-03 2020-06-01   365         quant    0.0873           0.01             flexacoin    0.1546           0.01      one  -4.559779   1 6.9542 -0.6715 compare combin_adf
# coint_info.4214 2019-06-03 2020-06-01   365         verge    0.1317           0.01                komodo     0.228           0.01      one  -4.712723  1 -5.5264 -1.0757 compare combin_adf
# coint_info.4174 2019-05-31 2020-05-29   365      monacoin    0.0872           0.01              loopring    0.1446           0.01      one  -5.004075     1 3.6143 -0.77 compare combin_adf
# coint_info.3685 2019-05-31 2020-05-29   365          lisk    0.2862           0.01              monacoin    0.0872           0.01      one  -5.753957  1 -0.2701 -0.8671 compare combin_adf


# extract data from contegration test (data clipped to shape in scope was saved there)
crypto_list <- all_combinations_cointegr$pairs_data

# create plots of given pair, choosing by number of row from result table
cryptoPairPlots(crypto_list,         # list with data
                cointegr_tb_signf,  # table of cointefration results
                n_table = 6,        # which pair to prepare plots for
                log_prices = TRUE,   # should first plot show log prices?
                plot_lags = 15,      # how many lags in ACF/PACF 
                colerograms = TRUE,  # should ACF/PACF be showed
                diffPlots = TRUE,    # should plots of differenced prices/logprices be showed
                in_sample = 365,     # how many observations in scope
                oo_sample = 15,      # number of observations out of scope
                ggplot = FALSE,       # should first plot be a ggplot based
                return_data = FALSE  # if laso to return in sample data
                )         


# get data with differences and log prices
crypto_pair <- getDifferencesXTS(coint_table = cointegr_tb_signf,                # table of cointefration results
                                 n_table = 6,                                    # which pair to prepare plots for
                                 n_obs_is = 365,                                  # how many observations in scope
                                 n_obs_ooc = 15,                                  # number of observations out of scope
                                 clipped = crypto_list, # list with data after 
                                 # crypto_list = crypto_list,
                                 log_prices = TRUE
                                 )$in_smpl

