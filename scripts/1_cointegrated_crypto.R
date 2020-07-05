# library(RCurl)
x <- getURL("https://raw.github.com/aronlindberg/latent_growth_classes/master/LGC_data.csv")

# library(devtools)
# source_gist("4b7f54ce0da6d4e462f7d07b9b2a39e5", filename = "getCryptoHistoricalPrice.R")
source("functions/getCryptoHistoricalPrice.R")
source("functions/finding_stationary_pair.R")


library(tidyverse)
library(rvest)
library(data.table)

#  ------------------   get list of all top 100 crypto -----------------------------
#rvest
coinmarketcap <- xml2::read_html("https://coinmarketcap.com/")


crypto_names <- coinmarketcap %>%
  html_nodes(".cmc-link") %>%
  html_text() 

crypto_names <- crypto_names[18:length(crypto_names)]

top100_crypto <- as.data.frame(matrix(crypto_names, ncol = 4,  byrow = TRUE), stringsAsFactors = FALSE)
top100_crypto <- top100_crypto[,-4]
names(top100_crypto) <- c("Name", "Price", "Capitalization")

top100_crypto <- top100_crypto[-(101:nrow(top100_crypto)),]
top100_crypto[,2:3] <- lapply(top100_crypto[,2:3], function(x){
  as.numeric(gsub("\\$|,", "", x))
})

top100_crypto$Name <- stringr::str_to_lower(gsub("\\.", "-", top100_crypto$Name)) 
top100_crypto$Name <- gsub("\\s", "-", top100_crypto$Name)


# ADF, H0: alfa=1, there is unit root, series is non-stationary, small p-val reject H0 so stationary yt, hight p-val > fail to reject H0, yt non-stat 
# desired p-val < 0.05 
# Breusch–Godfrey test: The H0: no serial correlation of any order up to p.
# desired BG p-val > 0.05, fail to rejecto H0
# testing integration order

# -------------------  batch sample from all -------

all_combinations <- as.data.frame(t(combn(top100_crypto$Name, 2)), stringsAsFactors = F)
dim(all_combinations) # [1] 4950    2

# set.seed(12345)
# sampled <- sample(nrow(all_combinations), nrow(all_combinations)*0.15)
# sample_combinations <- all_combinations[sampled,]
# smpl_comb_conit <- get_cointegration_table(sample_combinations, standardize = T, max_obs = 504, crypto_list)
# smpl1_dt <- smpl_comb_conit$results
# saveRDS(smpl21_dt, "./data/crypto_curr_dt_spl_comb.RDS")
# crypto_list <- smpl_comb_conit$scraped_data
# saveRDS(crypto_list, "./data/crypto_currencies_data.RDS")

# --------------------------------------------------------- all ---------------------------------------------------------------------------

# calculate cointegration and return lsit of data and table of results
source("functions/finding_stationary_pair.R")
# 2 <- cardano    0.1748 |    siacoin   0.3862

# --------------------------------------- LOG PRICES  ------------------------------------------------------------------------------

# cointegration of log prices
all_combinations_cointegr3 <- get_cointegration_table(all_combinations, # tabela z kombinacja
                                                      standardize = F, # no standarization
                                                      log_prices = T, # logaritmization
                                                      in_sample = 365, 
                                                      oo_sample = 15,
                                                      crypto_list,
                                                      include_dates = T)
dim(all_combinations_cointegr3$results)

table(all_combinations_cointegr3$results$conint_info)
# compare combin_adf data not available     not integrated 
#             1973               2604                373 

# read saved data
cointegr_tb3 <- all_combinations_cointegr3$results

# transform to numeric variable
cointegr_tb3$combin_adf <- as.numeric(as.character(cointegr_tb3$combin_adf))

# order according to value of combin_adf
cointegr_tb3 <- cointegr_tb3[order(cointegr_tb3$combin_adf, decreasing = T),]

# select only those with highest values
cointegr_tb_signf3 <- cointegr_tb3[cointegr_tb3$combin_adf < -3.8 & cointegr_tb3$conint_info == "compare combin_adf",]

dim(cointegr_tb_signf3) # [1] 41 12
# saveRDS(cointegr_tb_signf, "./data/cointegr_tb_signf2.RDS")

cointegr_tb_signf3
#   first_is    last_is n_obs       cc1 cc1_p_adf cc1_diff_p_adf          cc2 cc2_p_adf cc2_diff_p_adf combin_adf          coint_vec        conint_info
# 2019-05-31 2020-05-29   365  monacoin    0.0872           0.01         aelf    0.1252           0.01  -3.803662   1 2.7839 -1.0343 compare combin_adf
# 2019-06-03 2020-06-01   365     verge    0.1317           0.01        quant    0.0873           0.01  -3.818304  1 -4.4194 -0.5065 compare combin_adf
# 2019-06-03 2020-06-01   365      tron    0.1304           0.01       komodo     0.228           0.01  -3.825258  1 -4.3933 -1.1765 compare combin_adf
# 2019-05-31 2020-05-29   365      lisk    0.2862           0.01          wax    0.3094           0.01  -3.828249   1 3.3886 -1.1805 compare combin_adf
# 2019-06-03 2020-06-01   365  dogecoin    0.2845           0.01      siacoin    0.2882           0.01  -3.837151  1 -3.3912 -1.6069 compare combin_adf
# 2019-06-03 2020-06-01   365   bitcoin    0.1625           0.01     dogecoin    0.2845           0.01  -3.853532  1 13.2943 -0.8046 compare combin_adf
# 2019-06-03 2020-06-01   365     maker    0.2427           0.01     ontology    0.2851           0.01  -3.868044   1 8.0957 -1.2514 compare combin_adf
# 2019-06-03 2020-06-01   365      iota    0.3013           0.01        quant    0.0873           0.01  -3.872559  1 -2.5269 -0.6162 compare combin_adf
# 2019-05-31 2020-05-29   365   cardano    0.2691           0.01 decentraland    0.1686           0.01  -3.874713   1 0.9668 -0.7804 compare combin_adf
# 2019-06-03 2020-06-01   365 ravencoin    0.2732           0.01        quant    0.0873           0.01  -3.880001  1 -3.3125 -0.4691 compare combin_adf
# 2019-05-31 2020-05-29   365     quant    0.0922           0.01         aelf    0.1252           0.01  -3.894652   1 3.1761 -0.4373 compare combin_adf
# 2019-06-03 2020-06-01   365  litecoin    0.4579           0.01    ravencoin    0.2732           0.01  -3.897687   1 7.7395 -1.0178 compare combin_adf
# 2019-06-03 2020-06-01   365      holo    0.2483           0.01        quant    0.0873           0.01  -3.898169  1 -4.5373 -0.4018 compare combin_adf
# 2019-06-03 2020-06-01   365  ontology    0.2851           0.01    ravencoin    0.2732           0.01  -3.909383   1 3.1959 -0.9802 compare combin_adf
# 2019-06-03 2020-06-01   365   stellar    0.1209           0.01      siacoin    0.2882           0.01  -3.910338   1 3.6631 -0.9451 compare combin_adf
# 2019-06-03 2020-06-01   365    komodo     0.228           0.01        bytom    0.2395           0.01  -3.912311   1 2.1909 -0.7823 compare combin_adf
# 2019-06-03 2020-06-01   365  dogecoin    0.2845           0.01    flexacoin    0.1546           0.01  -3.914578  1 -4.2631 -1.6869 compare combin_adf
# 2019-06-03 2020-06-01   365     zcash    0.3843           0.01 decentraland    0.0934           0.01  -3.935548   1 5.6592 -0.5995 compare combin_adf
# 2019-06-03 2020-06-01   365   bitcoin    0.1625           0.01      stellar    0.1209           0.01  -3.954342  1 13.1892 -1.1531 compare combin_adf
# 2019-05-31 2020-05-29   365  dogecoin    0.1777           0.01         nano    0.2167           0.01  -3.961021 1 -10.0903 -1.7123 compare combin_adf
# 2019-05-31 2020-05-29   365     quant    0.0922           0.01 maidsafecoin    0.2774           0.01  -3.968494   1 3.2421 -0.6836 compare combin_adf
# 2019-05-31 2020-05-29   365      nano    0.2167           0.01        quant    0.0922           0.01  -3.979070  1 -1.7384 -0.6003 compare combin_adf
# 2019-06-03 2020-06-01   365       neo    0.2485           0.01        augur    0.1124           0.01  -4.009393  1 -0.6661 -0.7628 compare combin_adf
# 2019-06-03 2020-06-01   365  litecoin    0.4579           0.01   bittorrent    0.2654           0.01  -4.022722  1 13.7095 -1.4548 compare combin_adf
# 2019-05-31 2020-05-29   365  monacoin    0.0872           0.01 decentraland    0.1686           0.01  -4.051944   1 3.5542 -0.6583 compare combin_adf
# 2019-05-31 2020-05-29   365      icon    0.3385           0.01         lisk    0.2862           0.01  -4.060641  1 -1.4254 -0.9095 compare combin_adf
# 2019-06-03 2020-06-01   365    komodo     0.228           0.01        quant    0.0873           0.01  -4.088241  1 -1.8409 -0.5356 compare combin_adf
# 2019-05-31 2020-05-29   365       xrp    0.3262           0.01    ravencoin     0.262           0.01  -4.166859   1 1.5374 -1.4636 compare combin_adf
# 2019-06-03 2020-06-01   365  dogecoin    0.2845           0.01        quant    0.0873           0.01  -4.218888  1 -9.9697 -1.3886 compare combin_adf
# 2019-06-03 2020-06-01   365     quant    0.0873           0.01    bitshares    0.2938           0.01  -4.225558   1 4.7324 -0.6842 compare combin_adf
# 2019-05-31 2020-05-29   365      icon    0.3385           0.01     monacoin    0.0872           0.01  -4.253992  1 -1.5192 -0.7973 compare combin_adf
# 2019-06-03 2020-06-01   365   stellar    0.1209           0.01        quant    0.0873           0.01  -4.286871  1 -3.5104 -0.6836 compare combin_adf
# 2019-06-03 2020-06-01   365     quant    0.0873           0.01   bittorrent    0.2654           0.01  -4.475204    1 9.0323 -0.793 compare combin_adf
# 2019-06-03 2020-06-01   365   stellar    0.1209           0.01     dogecoin    0.2845           0.01  -4.494921   1 4.5313 -0.5363 compare combin_adf
# 2019-06-03 2020-06-01   365     quant    0.0873           0.01    flexacoin    0.1546           0.01  -4.559779   1 6.9542 -0.6715 compare combin_adf
# 2019-05-31 2020-05-29   365  loopring    0.1446           0.01          wax    0.3094           0.01  -4.652268   1 -1.822 -1.5274 compare combin_adf
# 2019-06-03 2020-06-01   365     verge    0.1317           0.01       komodo     0.228           0.01  -4.712723  1 -5.5264 -1.0757 compare combin_adf
# 2019-06-03 2020-06-01   365      tron    0.1304           0.01        bytom    0.2395           0.01  -4.748618  1 -1.7843 -1.0535 compare combin_adf
# 2019-05-31 2020-05-29   365  monacoin    0.0872           0.01     loopring    0.1446           0.01  -5.004075     1 3.6143 -0.77 compare combin_adf
# 2019-05-31 2020-05-29   365  monacoin    0.0872           0.01        steem    0.1942           0.01  -5.505463   1 1.9835 -0.8978 compare combin_adf
# 2019-05-31 2020-05-29   365      lisk    0.2862           0.01     monacoin    0.0872           0.01  -5.753957  1 -0.2701 -0.8671 compare combin_adf






# ---------------------------------------------------------------------------------------------------------------------

names_ <- names(crypto_pair)
combination_fornula <- as.formula(paste(names_[2], names_[1], sep="~"))
combination_fornula_inv <- as.formula(paste(names_[1], names_[2], sep="~"))

# linear combination
model.coint <- lm(combination_fornula, 
                  data = crypto_pair)
# model_summary <- summary(model.coint)
# comb_test <- testdf(variable = residuals(model.coint),
#                        max.augmentations = 10, max.order = 10)
# bg_all_aug <- comb_test[rowSums(comb_test[15:24] > 0.05) == 10,"p_adf"][1,]


# -------------------------------------------- ECM --------------------------------------------------------

crypto_pair$lresid <- lag.xts(residuals(model.coint))
head(crypto_pair)

# Estimating ECM
combination_formula2 <- as.formula(paste(names_[4], "~" ,names_[3], " + lresid"))
combination_formula2_inv <- as.formula(paste(names_[3], "~" ,names_[4], " + lresid"))

model.ecm <- lm(combination_formula2,
                data = crypto_pair) 
summary(model.ecm)

# -------------------------------------------- GRANGER CASUALITY --------------------------------------------------------

# d_siacoin ~ d_cardano
combination_formula3 <- as.formula(paste(names_[4], names_[3], sep="~"))
# d_cardano ~ d_siacoin
combination_formula3_inv <- as.formula(paste(names_[3], names_[4], sep="~"))


casuality <- matrix(NA, ncol = 5, nrow = 7)
for(i in 1:7){
  
  casuality[i,1] <- i
  
  # d_siacoin ~ d_cardano
  gr4_3 <- grangertest(combination_formula3,
              data = crypto_pair,
              order = i)
  casuality[i,2] <- gr4_3$`Pr(>F)`[2]
  casuality[i,3] <- if(gr4_3$`Pr(>F)`[2] < 0.05) "cause" else "no"
  
  # d_cardano ~ d_siacoin
  gr3_4 <- grangertest(combination_formula3_inv,
              data = crypto_pair,
              order = i) # lag assumed
  casuality[i,4] <- gr3_4$`Pr(>F)`[2]
  casuality[i,5] <- if(gr3_4$`Pr(>F)`[2] < 0.05) "cause" else "no"
}

casuality <-as.data.frame(casuality)
names(casuality) <- c("lags", "d_siacoin_d_cardano", "if_granger_1", "d_cardano_d_siacoin", "if_granger_2")
casuality
# H0: no casuality

saveRDS(casuality, "./gr_casual_d_siacoin_d_cardano.RDS")


# -------------------------------------------- AUTO ARIMA --------------------------------------------------------
names(crypto_pair) # [1] "cardano"   "siacoin"   "d_cardano" "d_siacoin" "lresid" 

cardano.acuto.AIC <- auto.arima(crypto_pair[,1],
                            d = 1,             # parameter d of ARIMA model
                            max.p = 11,         # Maximum value of p
                            max.q = 11,         # Maximum value of q
                            max.order = 22,    # maximum p+q
                            start.p = 1,       # Starting value of p in stepwise procedure
                            start.q = 1,       # Starting value of q in stepwise procedure
                            ic = "aic",        # Information criterion to be used in model selection.
                            stepwise = FALSE,  # if FALSE considers all models
                            allowdrift = TRUE, # include a constant
                            trace = TRUE)      # show summary of all models considered
# Best model: ARIMA(2,1,0)  # if lags 5
# Best model: ARIMA(2,1,0)
# Best model: ARIMA(2,1,0) 

par(mfrow = c(2, 1)) 
acf(resid(cardano.acuto.AIC),
    lag.max = 12, 
    ylim = c(-0.5, 0.5),    
    lwd = 5,              
    col = "dark green",
    na.action = na.pass)   
pacf(resid(cardano.acuto.AIC), 
     lag.max = 12, 
     ylim = c(-0.5, 0.5),
     lwd = 5, col = "dark green",
     na.action = na.pass) 
par(mfrow = c(1, 1)) 

# hypothesis of randomness, there is no autocorrelation
Box.test(resid(cardano.acuto.AIC), type = "Ljung-Box", lag =  7)
Box.test(resid(cardano.acuto.AIC), type = "Ljung-Box", lag = 14)
Box.test(resid(cardano.acuto.AIC), type = "Ljung-Box", lag = 21)
Box.test(resid(cardano.acuto.AIC), type = "Ljung-Box", lag = 28)


cardano.acuto.BIC <- auto.arima(crypto_pair[,1],
                                d = 1,             # parameter d of ARIMA model
                                max.p = 11,         # Maximum value of p
                                max.q = 11,         # Maximum value of q
                                max.order = 22,    # maximum p+q
                                start.p = 1,       # Starting value of p in stepwise procedure
                                start.q = 1,       # Starting value of q in stepwise procedure
                                ic = "bic",        # Information criterion to be used in model selection.
                                stepwise = FALSE,  # if FALSE considers all models
                                allowdrift = TRUE, # include a constant
                                trace = TRUE)      # show summary of all models considered
#  Best model: ARIMA(0,1,2) 

par(mfrow = c(2, 1)) 
acf(resid(cardano.acuto.BIC),
    lag.max = 12, 
    ylim = c(-0.5, 0.5),    
    lwd = 5,              
    col = "dark green",
    na.action = na.pass)   
pacf(resid(cardano.acuto.BIC), 
     lag.max = 12, 
     ylim = c(-0.5, 0.5),
     lwd = 5, col = "dark green",
     na.action = na.pass) 
par(mfrow = c(1, 1)) 

# hypothesis of randomness, there is no autocorrelation
Box.test(resid(cardano.acuto.BIC), type = "Ljung-Box", lag =  7)
Box.test(resid(cardano.acuto.BIC), type = "Ljung-Box", lag = 14)
Box.test(resid(cardano.acuto.BIC), type = "Ljung-Box", lag = 21)
Box.test(resid(cardano.acuto.BIC), type = "Ljung-Box", lag = 28)
# no autocorrelation

names(crypto_pair) # [1] "cardano"   "siacoin"   "d_cardano" "d_siacoin" "lresid" 

siacoin.acuto.AIC <- auto.arima(crypto_pair[,2],
                                d = 1,             # parameter d of ARIMA model
                                max.p = 12,         # Maximum value of p
                                max.q = 12,         # Maximum value of q
                                max.order = 24,    # maximum p+q
                                start.p = 1,       # Starting value of p in stepwise procedure
                                start.q = 1,       # Starting value of q in stepwise procedure
                                ic = "aic",        # Information criterion to be used in model selection.
                                stepwise = FALSE,  # if FALSE considers all models
                                allowdrift = TRUE, # include a constant
                                trace = TRUE)      # show summary of all models considered
# Best model: ARIMA(2,1,5)   

par(mfrow = c(2, 1)) 
acf(resid(siacoin.acuto.AIC),
    lag.max = 12, 
    ylim = c(-0.5, 0.5),    
    lwd = 5,              
    col = "dark green",
    na.action = na.pass)   
pacf(resid(siacoin.acuto.AIC), 
     lag.max = 12, 
     ylim = c(-0.5, 0.5),
     lwd = 5, col = "dark green",
     na.action = na.pass) 
par(mfrow = c(1, 1)) 

# hypothesis of randomness, there is no autocorrelation
Box.test(resid(siacoin.acuto.AIC), type = "Ljung-Box", lag =  7)
Box.test(resid(siacoin.acuto.AIC), type = "Ljung-Box", lag = 14)
Box.test(resid(siacoin.acuto.AIC), type = "Ljung-Box", lag = 28)


siacoin.acuto.BIC <- auto.arima(crypto_pair[,2],
                                d = 1,             # parameter d of ARIMA model
                                max.p = 12,         # Maximum value of p
                                max.q = 12,         # Maximum value of q
                                max.order = 22,    # maximum p+q
                                start.p = 1,       # Starting value of p in stepwise procedure
                                start.q = 1,       # Starting value of q in stepwise procedure
                                ic = "bic",        # Information criterion to be used in model selection.
                                stepwise = FALSE,  # if FALSE considers all models
                                allowdrift = TRUE, # include a constant
                                trace = TRUE)      # show summary of all models considered
# Best model: ARIMA(0,1,0) 

par(mfrow = c(2, 1)) 
acf(resid(siacoin.acuto.BIC),
    lag.max = 12, 
    ylim = c(-0.5, 0.5),    
    lwd = 5,              
    col = "dark green",
    na.action = na.pass)   
pacf(resid(siacoin.acuto.BIC), 
     lag.max = 12, 
     ylim = c(-0.5, 0.5),
     lwd = 5, col = "dark green",
     na.action = na.pass) 
par(mfrow = c(1, 1)) 

# hypothesis of randomness, there is no autocorrelation
Box.test(resid(siacoin.acuto.BIC), type = "Ljung-Box", lag =  7)
Box.test(resid(siacoin.acuto.BIC), type = "Ljung-Box", lag = 14)
Box.test(resid(siacoin.acuto.BIC), type = "Ljung-Box", lag = 21)
Box.test(resid(siacoin.acuto.BIC), type = "Ljung-Box", lag = 28)
# THERE IS AN AUTOCORRELATION


# -------------------------------------------------------------------------------------------------------