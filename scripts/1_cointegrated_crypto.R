# library(RCurl)
x <- getURL("https://raw.github.com/aronlindberg/latent_growth_classes/master/LGC_data.csv")

# library(devtools)
# source_gist("4b7f54ce0da6d4e462f7d07b9b2a39e5", filename = "getCryptoHistoricalPrice.R")
source("functions/getCryptoHistoricalPrice.R")


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
source("functions/finding_stationary_pair.R")

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

all_combinations_cointegr <- get_cointegration_table(all_combinations, standardize = T, max_obs = 365, crypto_list)

all_combinations_cointegr_tb <- all_combinations_cointegr$results
length(all_combinations_cointegr$scraped_data)
saveRDS(all_combinations_cointegr_tb, "data/all_combinations_cointegr_365d.RDS")

cointegr_tb <- all_combinations_cointegr_tb
cointegr_tb <- cointegr_tb[order(cointegr_tb$combin_adf, decreasing = T),]
cointegr_tb$combin_adf <- as.numeric(as.character(cointegr_tb$combin_adf))
cointegr_tb_signf2 <- cointegr_tb[cointegr_tb$combin_adf < -3.8 & cointegr_tb$conint_info == "compare combin_adf",]
dim(cointegr_tb_signf2) # [1] 26 10
saveRDS(cointegr_tb_signf2, "./data/cointegr_tb_signf2.RDS")
dim(cointegr_tb_signf2) # [1] 29 10

min(cointegr_tb$combin_adf, na.rm = T)
hist(cointegr_tb_signf$combin_adf)

#                 n_obs       cc1 cc1_p_adf cc1_diff_p_adf          cc2 cc2_p_adf cc2_diff_p_adf combin_adf   coint_vec        conint_info
# coint_info.1494   365      tron    0.0722           0.01         qtum    0.2764           0.01  -5.503942 1 0 -0.9496 compare combin_adf
# coint_info.3284   365     theta    0.5391           0.01  electroneum    0.2158           0.01  -5.425056 1 0 -0.8045 compare combin_adf
# coint_info.4574   365     quant    0.0893           0.01    flexacoin    0.0707           0.01  -5.258627 1 0 -0.7961 compare combin_adf
# coint_info.2647   365  dogecoin    0.2137           0.01    flexacoin    0.0707           0.01  -4.770275 1 0 -0.7794 compare combin_adf
# coint_info.14     365   bitcoin    0.1572           0.01       monero    0.1373           0.01  -4.766124 1 0 -0.9018 compare combin_adf
# coint_info.3468   365      icon    0.1737           0.01         lisk    0.0967           0.01  -4.642468 1 0 -0.8558 compare combin_adf
# coint_info.2482   365  ontology    0.0851           0.01         qtum    0.2764           0.01  -4.581125 1 0 -0.9523 compare combin_adf
# coint_info.4208   365  monacoin    0.3464           0.01 decentraland    0.0547           0.01  -4.521856 1 0 -0.8615 compare combin_adf
# coint_info.3685   365      lisk    0.0967           0.01     monacoin    0.3464           0.01  -4.474282 1 0 -0.9402 compare combin_adf
# coint_info.74     365   bitcoin    0.1572           0.01    flexacoin    0.0707           0.01  -4.410615 1 0 -0.7443 compare combin_adf
# coint_info.1478   365      tron    0.0502           0.01     dogecoin    0.2137           0.01  -4.299848 1 0 -0.8943 compare combin_adf
# coint_info.2195   365   vechain    0.5119           0.01        augur    0.0556           0.01  -4.267314 1 0 -0.7624 compare combin_adf
# coint_info.157    365  ethereum    0.2578           0.01     monacoin    0.3464           0.01  -4.215708 1 0 -0.9094 compare combin_adf
# coint_info.1656   365       neo    0.1738           0.01        augur    0.0556           0.01  -4.132811 1 0 -0.8284 compare combin_adf
# coint_info.21     365   bitcoin    0.1572           0.01         iota    0.0599           0.01  -4.103743 1 0 -0.7642 compare combin_adf
# coint_info.1411   365    monero    0.1696           0.01         qtum    0.2764           0.01  -4.050396 1 0 -0.8466 compare combin_adf
# coint_info.1709   365       neo    0.1738           0.01 decentraland    0.0732           0.01  -4.047336 1 0 -0.8368 compare combin_adf
# coint_info.3853   230      nano    0.5826           0.01   blockstack    0.0729           0.01  -4.011529 1 0 -0.7757 compare combin_adf
# coint_info.3015   365   zilliqa    0.8713           0.01      siacoin      0.62           0.01  -3.993849 1 0 -0.7979 compare combin_adf
# coint_info.2440   365       nem    0.0852           0.01    flexacoin    0.0707           0.01  -3.991425  1 0 -0.721 compare combin_adf
# coint_info.362    365       xrp    0.1789           0.01        quant    0.0886           0.01  -3.969555 1 0 -0.5848 compare combin_adf
# coint_info.3479   365      icon    0.1737           0.01     monacoin    0.3464           0.01  -3.961616 1 0 -0.8264 compare combin_adf
# coint_info.2746   365  digibyte    0.8603           0.01        theta    0.5391           0.01  -3.961285 1 0 -0.7569 compare combin_adf
# coint_info.319    365       xrp    0.1789           0.01     ontology    0.0851           0.01  -3.915651 1 0 -0.9584 compare combin_adf
# coint_info.196    365  ethereum    0.2403           0.01 decentraland    0.0732           0.01  -3.893082 1 0 -0.8549 compare combin_adf
# coint_info.28     365   bitcoin    0.1572           0.01     ontology    0.0739           0.01  -3.847111 1 0 -0.7547 compare combin_adf
# coint_info.4454   365    status    0.4718           0.01          ren    0.3466           0.01  -3.844885 1 0 -0.6709 compare combin_adf
# coint_info.901    365   cardano    0.1748           0.01      siacoin    0.3862           0.01  -3.834585 1 0 -0.8827 compare combin_adf
# coint_info.1241   365 chainlink    0.2983           0.01        augur    0.0556           0.01  -3.813397 1 0 -0.4703 compare combin_adf

crypto_pair <- cryptoPairPlots(crypto_list, cointegr_tb_signf2, n = 28, colerograms = TRUE, diffPlots=FALSE)
# cointegr_tb_signf2[19,]
# 13 16 22
# 16
# 9
# 13 <- # NO CAUSALITY   ethereum    monacoin    0.
# 27 <- # NO CAUSALITY   status    0.4718           0.01          ren
# 28 <- cardano    0.1748 |    siacoin   0.3862


# coint_info.901    365   cardano    0.1748           0.01      siacoin    0.3862           0.01  -3.834585 1 0 -0.8827 compare combin_adf
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