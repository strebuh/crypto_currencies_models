library(xts)
library(forecast)
library(lmtest)
library(vars)

source("functions/finding_stationary_pair.R")

# ---------------------------------------------------- PREPARE DATA ---------------------------------------------------------------------------

# # get data with differences and log prices
# crypto_pair_all <- getDifferencesXTS(coint_table = cointegr_tb_signf,                # table of cointefration results
#                                      n_table = 36,                                    # which pair to prepare plots for
#                                      n_obs_is = 365,                                  # how many observations in scope
#                                      n_obs_ooc = 15,                                  # number of observations out of scope
#                                      clipped = all_combinations_cointegr3$pairs_data, # list with data after 
#                                      # crypto_list = crypto_list,
#                                      log_prices = TRUE
# )
# crypto_pair <- crypto_pair_all$in_smpl
crypto_pair_oos <- crypto_pair_all$oo_smpl


names(crypto_pair)
# ------------------------------------------------- VAR MODEL ------------------------------------------------------------------------------
# selection without seasons
VARselect(crypto_pair[,1:2], 
          lag.max = 14
          )

# AIC(n)  HQ(n)  SC(n) FPE(n)  # 3
# 2      2      1      2 


# -------------------------------------------------------------------------------------------------------------------------------
# selection with weakly seasons
VARselect(crypto_pair[,1:2],
          lag.max = 14,     
          season = 7)     

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 2      2      1      2 

# -------------------------------------------------------------------------------------------------------------------------------
# VAR model with 7 lags and seasons
crypto_pair.var2s <- VAR(crypto_pair[,1:2],
                         p = 2,
                         season = 7) 

summary(crypto_pair.var2s) ## you rarely interpet interpret ## ~44:00
# seasons are not significant

#--------------------
# ============================================ 
#   log_bitcoin = log_bitcoin.l1 + log_dogecoin.l1 + log_bitcoin.l2 + log_dogecoin.l2 + const + sd1 + sd2 + sd3 + sd4 + sd5 + sd6 
# 
# Estimate Std. Error t value Pr(>|t|)    
# log_bitcoin.l1   0.788461   0.080761   9.763  < 2e-16 ***
#   log_dogecoin.l1  0.133845   0.094954   1.410  0.15955    
# log_bitcoin.l2   0.135580   0.079798   1.699  0.09020 .  
# log_dogecoin.l2 -0.075250   0.094041  -0.800  0.42415    
# const            1.041229   0.377498   2.758  0.00611 ** 
#   sd1              0.013514   0.008668   1.559  0.11987    
# sd2              0.005757   0.008725   0.660  0.50982    
# sd3              0.013310   0.008674   1.534  0.12582    
# sd4              0.001965   0.008681   0.226  0.82101    
# sd5              0.014746   0.008665   1.702  0.08966 .  
# sd6              0.004566   0.008702   0.525  0.60015 

# log_dogecoin = log_bitcoin.l1 + log_dogecoin.l1 + log_bitcoin.l2 + log_dogecoin.l2 + const + sd1 + sd2 + sd3 + sd4 + sd5 + sd6 
# 
# Estimate Std. Error t value Pr(>|t|)    
# log_bitcoin.l1  -0.165469   0.067870  -2.438   0.0153 *  
#   log_dogecoin.l1  0.981616   0.079797  12.301   <2e-16 ***
#   log_bitcoin.l2   0.151317   0.067061   2.256   0.0247 *  
#   log_dogecoin.l2  0.008418   0.079030   0.107   0.9152    
# const            0.068276   0.317241   0.215   0.8297    
# sd1              0.001069   0.007284   0.147   0.8834    
# sd2             -0.003817   0.007333  -0.521   0.6030    
# sd3              0.001332   0.007290   0.183   0.8551    
# sd4             -0.003120   0.007295  -0.428   0.6692    
# sd5              0.012470   0.007282   1.713   0.0877 .  
# sd6              0.001302   0.007313   0.178   0.8588    

  
# AIC(crypto_pair.var2, crypto_pair.var2s)
# BIC(crypto_pair.var2, crypto_pair.var2s)
# -------------------------------------------------------------------------------------------------------------------------------
# model without seasonality component
crypto_pair.var2 <- VAR(crypto_pair[,1:2], 
                        p = 2)

summary(crypto_pair.var2)
# log_bitcoin = log_bitcoin.l1 + log_dogecoin.l1 + log_bitcoin.l2 + log_dogecoin.l2 + const 
# 
# Estimate Std. Error t value Pr(>|t|)    
# log_bitcoin.l1   0.77934    0.07993   9.750  < 2e-16 ***
#   log_dogecoin.l1  0.13046    0.09373   1.392  0.16482    
# log_bitcoin.l2   0.14408    0.07895   1.825  0.06884 .  
# log_dogecoin.l2 -0.07137    0.09279  -0.769  0.44233    
# const            1.04993    0.37721   2.783  0.00566 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  
# log_dogecoin = log_bitcoin.l1 + log_dogecoin.l1 + log_bitcoin.l2 + log_dogecoin.l2 + const 
# 
# Estimate Std. Error t value Pr(>|t|)    
# log_bitcoin.l1  -0.176360   0.067228  -2.623  0.00908 ** 
#   log_dogecoin.l1  0.985004   0.078834  12.495  < 2e-16 ***
#   log_bitcoin.l2   0.162923   0.066403   2.454  0.01462 *  
#   log_dogecoin.l2  0.004235   0.078043   0.054  0.95675    
# const            0.057055   0.317262   0.180  0.85738  
  

serial.test(crypto_pair.var2) ## 57:00+
# data:  Residuals of VAR object crypto_pair.var2
# Chi-squared = 73.469, df = 56, p-value = 0.05864

serial.test(crypto_pair.var2, type = "BG") # The null hypothesis is that there is no serial correlation of any order up to p.
# data:  Residuals of VAR object crypto_pair.var2
# Chi-squared = 18.8, df = 20, p-value = 0.5348


# lets do some basic diagnostics
plot(crypto_pair.var2)
# no autocorrelation

# ------------------ restricted VAR 1
# automatic restriction, based on first model with seasonality
restrict(crypto_pair.var2s, method = "ser")
# log_litecoin = log_litecoin.l1 + log_bittorrent.l1 + log_bittorrent.l2 + log_bittorrent.l4 + log_litecoin.l5
# log_bittorrent = log_bittorrent.l1 + log_litecoin.l4 + log_litecoin.l5 
# restrict <- matrix(c(1, 1, 1, 1, 1, 1, 1,
#                      1, 0, 1, 0, 0, 1, 0),
#                    nrow=2, ncol=7, byrow=TRUE)


# The Granger causality results showed the bi-directional feedback on 5% level.
casuality <- readRDS("./gr_casual_4_7_3.RDS")
# lags diff_log_dogecoin_diff_log_bitcoin if_granger_1 diff_log_bitcoin_diff_log_dogecoin if_granger_2
# 1    1                0.00782679670182001        cause                  0.269446060727148           no
# 2    2                0.00845032680142793        cause                  0.406055215607526           no
# 3    3                 0.0163411362312044        cause                  0.585861522585117           no
# 4    4                 0.0301524589862929        cause                  0.187066234083829           no
# 5    5                 0.0603106647820709           no                  0.247811136797591           no
# 6    6                  0.104423762200012           no                  0.330259651129534           no
# 7    7                  0.102145814485695           no                 0.0892475545984026           no

# for bittorrent granger matches significance from model
# for litecoin granger matches almost significance from model, lags 1,2,3 are significant up to 10%, but granger for 2nd lag is also around the bodrer 

 
# only GRANGER
restrict1 <- matrix(c(1, 0, 1, 0, 1, # dep log_bitcoin 
                      1, 1, 1, 0, 0), # dep-log_dogecoin 
                   nrow=2, ncol=5, byrow=TRUE) # only GC

crypto_pair.var2_restr1 <- restrict(crypto_pair.var2, method = "man", resmat = restrict1)
summary(crypto_pair.var2_restr1)
# log_bitcoin = log_bitcoin.l1 + log_bitcoin.l2 + const 
# 
# Estimate Std. Error t value Pr(>|t|)    
# log_bitcoin.l1  0.87421    0.05250  16.650   <2e-16 ***
#   log_bitcoin.l2  0.09634    0.05251   1.835   0.0674 .  
# const           0.26794    0.12243   2.188   0.0293 *     
  
# log_dogecoin = log_bitcoin.l1 + log_dogecoin.l1 + log_bitcoin.l2 
# 
# Estimate Std. Error t value Pr(>|t|)    
# log_bitcoin.l1  -0.175493   0.043450  -4.039 6.57e-05 ***
#   log_dogecoin.l1  0.985408   0.007259 135.749  < 2e-16 ***
#   log_bitcoin.l2   0.165813   0.043804   3.785  0.00018 ***

serial.test(crypto_pair.var2_restr1, type = "BG") 
# data:  Residuals of VAR object crypto_pair.var2_restr1
# Chi-squared = 23.937, df = 20, p-value = 0.2451
serial.test(crypto_pair.var2_restr1) 
# data:  Residuals of VAR object crypto_pair.var2_restr1
# Chi-squared = 80.553, df = 56, p-value = 0.01748

plot(crypto_pair.var2_restr1)
# although the plot looks allright, the Portmanteau test for autocorrelation revealsproblem of autocorrelation of residuals 


# ------------------ restricted VAR 2

# # Granger and first VAR model restriction
# restrict2 <- matrix(c(1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, # ind-cardano
#                       0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1), # ind-siacoin
#                     nrow=2, ncol=13, byrow=TRUE)
# 
# # lags d_siacoin_d_cardano if_granger_1 
# #    1   0.276727171250911           no 
# #    2  0.0307660109222102        cause 
# #    3   0.045346250988177        cause 
# #    4  0.0929440887878814           no 
# #    5   0.105121398655361           no 
# #    6  0.0316361829048299        cause 
# #    7 0.00319632534714409        cause 
# 
# 
# # cardano = cardano.l1 + siacoin.l1 + cardano.l2 + siacoin.l2 + cardano.l3 + siacoin.l3 + 
# #   cardano.l4 + siacoin.l4 + cardano.l5 + siacoin.l5 + cardano.l6 + siacoin.l6 + const
# # cardano.l1  0.9942175  0.0694343  14.319  < 2e-16 ***
# # siacoin.l1 -2.8090025  1.4808967  -1.897  0.05868 .  
# # cardano.l3 -0.2557171  0.0938754  -2.724  0.00678 ** 
# # cardano.l4  0.1699510  0.0931632   1.824  0.06898 .  
# # const       0.0010819  0.0004515   2.397  0.01708 *  
# 
# # siacoin = cardano.l1 + siacoin.l1 + cardano.l2 + siacoin.l2 + cardano.l3 + siacoin.l3 + 
# #   cardano.l4 + siacoin.l4 + cardano.l5 + siacoin.l5 + cardano.l6 + siacoin.l6 + const 
# # cardano.l1  5.653e-03  3.235e-03   1.748   0.0814 .  
# # siacoin.l1  7.800e-01  6.899e-02  11.307   <2e-16 ***
# # cardano.l3 -8.475e-03  4.373e-03  -1.938   0.0534 .  
# # siacoin.l3  2.084e-01  9.153e-02   2.276   0.0234 *  
# # siacoin.l6 -1.422e-01  6.865e-02  -2.072   0.0390 *  
# # const       4.546e-05  2.103e-05   2.161   0.0314 *  
# 
# crypto_pair.var6_restr2 <- restrict(crypto_pair.var6, method = "man", resmat = restrict2)
# summary(crypto_pair.var6_restr2)
# 
# # cardano = cardano.l1 + siacoin.l1 + cardano.l3 + cardano.l4 + const 
# # cardano.l1  1.0086771  0.0400269  25.200  < 2e-16 ***
# # siacoin.l1 -1.4883032  0.5434007  -2.739  0.00648 ** 
# # cardano.l3 -0.1091386  0.0634560  -1.720  0.08632 .  
# # cardano.l4  0.1378062  0.0519236   2.654  0.00831 ** 
# # const       0.0010980  0.0004418   2.485  0.01340 *  
# 
# # siacoin.l1  8.747e-01  3.967e-02  22.050  < 2e-16 ***
# # cardano.l3  2.967e-03  9.784e-04   3.033  0.00260 ** 
# # siacoin.l3  1.136e-01  5.046e-02   2.252  0.02491 *  
# # siacoin.l6 -8.486e-02  3.234e-02  -2.624  0.00906 ** 
# # const       4.749e-05  2.089e-05   2.273  0.02363 * 
# 
# serial.test(crypto_pair.var6_restr2, type = "BG") 
# # data:  Residuals of VAR object crypto_pair.var6_restr2
# # Chi-squared = 35.334, df = 20, p-value = 0.0184
# serial.test(crypto_pair.var6_restr2) 
# # data:  Residuals of VAR object crypto_pair.var6_restr2
# # Chi-squared = 80.197, df = 40, p-value = 0.0001671
# 
# plot(crypto_pair.var6_restr2)
# # although the plot looks allright, the Portmanteau test for autocorrelation revealsproblem of autocorrelation of residuals 

# ---------------------------------------------------------------------------------------------------------------------------------------------
AIC(crypto_pair.var2s, crypto_pair.var2, crypto_pair.var2_restr1)
BIC(crypto_pair.var2s, crypto_pair.var2, crypto_pair.var2_restr1)
# > AIC(crypto_pair.var2s, crypto_pair.var2, crypto_pair.var2_restr1)
# df       AIC
# crypto_pair.var2s       22 -2880.934
# crypto_pair.var2        10 -2889.614
# crypto_pair.var2_restr1  6 -2885.135
# > BIC(crypto_pair.var2s, crypto_pair.var2, crypto_pair.var2_restr1)
# df       BIC
# crypto_pair.var2s       22 -2795.257
# crypto_pair.var2        10 -2850.670
# crypto_pair.var2_restr1  6 -2861.768

# AIC prefere the most restricted model, BIC the leastwhich makes sense, however due to correlation in residuals

#----------------------------------------------------------------- FORECAST -------------------------------------------------------------------

# and run a forecast
crypto_pair.var2.forecast <- predict(crypto_pair.var2,
                                     n.ahead = 7,
                                     ci = 0.95) 

names(crypto_pair.var2.forecast)
# [1] "fcst"     "endog"    "model"    "exo.fcst"

# VAR forecasts for both currencies
crypto_pair.var2.forecast$fcst$log_bitcoin
crypto_pair.var2.forecast$fcst$log_dogecoin

dim(crypto_pair_oos)
head(crypto_pair_oos)
c1_forecast_VAR <- xts(crypto_pair.var2.forecast$fcst$log_bitcoin[,-4], 
                        head(index(crypto_pair_oos), 7))
# lets change the names 
names(c1_forecast_VAR) <- c(paste0(names_[1],"_fore_VAR"), paste0(names_[1],"_lower_VAR"), paste0(names_[1],"_upper_VAR"))

# lets do the same for cpi forecasts 
c2_forecast_VAR <- xts(crypto_pair.var2.forecast$fcst$log_dogecoin[,-4], 
                       head(index(crypto_pair_oos), 7))
names(c2_forecast_VAR) <- c(paste0(names_[2],"_fore_VAR"), paste0(names_[2],"_lower_VAR"), paste0(names_[2],"_upper_VAR"))

# add oos observations
crypto_pair_all_ <- rbind(crypto_pair[,-ncol(crypto_pair)], head(crypto_pair_oos, 7))
tail(crypto_pair_all, 20)

# lets put the data together
crypto_pair_VAR <- merge(crypto_pair_all_,
                     c1_forecast_VAR,
                     c2_forecast_VAR)
names(crypto_pair_VAR)
# [1] "log_bitcoin"            "log_dogecoin"           "diff_log_bitcoin"       "diff_log_dogecoin"      "log_bitcoin_fore_VAR"   "log_bitcoin_lower_VAR" 
# [7] "log_bitcoin_upper_VAR"  "log_dogecoin_fore_VAR"  "log_dogecoin_lower_VAR" "log_dogecoin_upper_VAR"

plot(crypto_pair_VAR[(nrow(crypto_pair_VAR)-30):nrow(crypto_pair_VAR), grep("^log_bitcoin", names(crypto_pair_VAR))], 
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 3,
     main = paste0("7 days forecast of ", names(crypto_pair_VAR))[1],
     col = c("black", "blue", "red", "red"))

plot(crypto_pair_VAR[(nrow(crypto_pair_VAR)-30):nrow(crypto_pair_VAR), grep("^log_dogecoin", names(crypto_pair_VAR))], 
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 3,
     main = paste0("7 days forecast of ", names(crypto_pair_VAR))[2],
     col = c("black", "blue", "red", "red"))

# errors
crypto_pair_VAR$mae.log_bitcoin   <-  abs(crypto_pair_VAR$log_bitcoin - crypto_pair_VAR$log_bitcoin_fore)
crypto_pair_VAR$mse.log_bitcoin <-  (crypto_pair_VAR$log_bitcoin - crypto_pair_VAR$log_bitcoin_fore)^2
crypto_pair_VAR$mape.log_bitcoin  <-  abs((crypto_pair_VAR$log_bitcoin - crypto_pair_VAR$log_bitcoin_fore)/crypto_pair_VAR$log_bitcoin)
crypto_pair_VAR$amape.log_bitcoin <-  abs((crypto_pair_VAR$log_bitcoin - crypto_pair_VAR$log_bitcoin_fore) / 
                                    (crypto_pair_VAR$log_bitcoin + crypto_pair_VAR$log_bitcoin_fore))

crypto_pair_VAR$mae.log_dogecoin   <-  abs(crypto_pair_VAR$log_dogecoin - crypto_pair_VAR$log_dogecoin_fore)
crypto_pair_VAR$mse.log_dogecoin   <-  (crypto_pair_VAR$log_dogecoin - crypto_pair_VAR$log_dogecoin_fore)^2
crypto_pair_VAR$mape.log_dogecoin  <-  abs((crypto_pair_VAR$log_dogecoin - crypto_pair_VAR$log_dogecoin_fore)/crypto_pair_VAR$log_dogecoin)
crypto_pair_VAR$amape.log_dogecoin <-  abs((crypto_pair_VAR$log_dogecoin - crypto_pair_VAR$log_dogecoin_fore) / 
                                    (crypto_pair_VAR$log_dogecoin + crypto_pair_VAR$log_dogecoin_fore))

# get measures
colMeans(crypto_pair_VAR[,11:18], na.rm = TRUE)
# mae.log_bitcoin    mse.log_bitcoin   mape.log_bitcoin  amape.log_bitcoin   mae.log_dogecoin   mse.log_dogecoin  mape.log_dogecoin amape.log_dogecoin 
# 1.874606e-02       5.768773e-04       2.043729e-03       1.020211e-03       7.054705e-03       8.200357e-05       1.184335e-03       5.924479e-04 


# --------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------- VECM ----------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------------------------------------------

# head(crypto_pair[,1:2])
johan.test.trace <- ca.jo(crypto_pair[,1:2],         
                          ecdet = "const", # "none", "const", "trend"
                          type = "trace",  
                          K = 2) 
                         #  ,        
                         # season = 7)

summary(johan.test.trace) 
#           test 10pct  5pct  1pct
# r <= 1 |  4.13  7.52  9.24 12.97
# r = 0  | 20.80 17.85 19.96 24.60

# https://www.researchgate.net/post/2_variables_and_2_cointegrating_equations_can_anyone_help_me2
johan.test.eigen <- ca.jo(crypto_pair[,1:2],         # data 
                          ecdet = "const", 
                          type = "eigen",  
                          K = 2) 
summary(johan.test.eigen) 
#           test 10pct  5pct  1pct
# r <= 1 |  4.13  7.52  9.24 12.97
# r = 0  | 16.68 13.75 15.67 20.20

get_pair_plot2(crypto_pair[,1:2], # dataframe or xts
                           # log_price = F, 
                           standardize = T,
                           ggplot = F
)
 
#-------------------------------------- model ------------------------------------------------------------------------------
crypto_pair.vecm.1 <- cajorls(johan.test.eigen, r = 1) 

summary(crypto_pair.vecm.1$rlm)

# estimates of the VECM model
crypto_pair.vecm.1$beta
#                         ect1
# log_bitcoin.l2    1.00000
# log_dogecoin.l2  -1.06445
# constant        -15.47143

# we can reparametrize the VEC model into VAR
crypto_pair.vecm.1.asVAR <- vec2var(johan.test.eigen, r = 1)
# lets see the result
crypto_pair.vecm.1.asVAR

#-------------------------------------- model ------------------------------------------------------------------------------

# based on the reparametrized model
# one can calculate and plot Impulse Response Functions
plot(irf(crypto_pair.vecm.1.asVAR, 
         n.ahead = 28))

# and variance decomposition
plot(fevd(crypto_pair.vecm.1.asVAR, 
          n.ahead = 28))

#-------------------------------------- model ------------------------------------------------------------------------------

# autocorrelation residuals
head(residuals(crypto_pair.vecm.1.asVAR))
serial.test(crypto_pair.vecm.1.asVAR, type = "BG")
# Chi-squared = 18.803, df = 20, p-value = 0.5347
# no autocorrelation

plot(serial.test(crypto_pair.vecm.1.asVAR))
# squared residuals could be better

# EDF = Empirical Distribution Function
# lets check normality of residuals
# this is Jarque-Bera (JB) test
# Johansen test is sensitive to lack of normality

normality.test(crypto_pair.vecm.1.asVAR)
# Chi-squared = 20336, df = 4, p-value < 2.2e-16
# H0: data are from a normal distribution
# data is not from normal distribution

#----------------------------------------------------------------- FORECAST -------------------------------------------------------------------

# and run a forecast
crypto_pair.vecm.1.asVAR.forecast <- predict(crypto_pair.vecm.1.asVAR,
                                             n.ahead = 7,
                                             ci = 0.95) # 95% confidence interval

# lets see the result
crypto_pair.vecm.1.asVAR.forecast
names(crypto_pair.vecm.1.asVAR.forecast)
# [1] "fcst"     "endog"    "model"    "exo.fcst"
# str(crypto_pair.vecm.1.asVAR.forecast)


# VAR forecasts for both currencies
crypto_pair.vecm.1.asVAR.forecast$fcst$log_bitcoin
crypto_pair.vecm.1.asVAR.forecast$fcst$log_dogecoin


head(crypto_pair_oos)
c1_forecast.VECM <- xts(crypto_pair.vecm.1.asVAR.forecast$fcst$log_bitcoin[,-4], 
                        head(index(crypto_pair_oos), 7))
names(c1_forecast.VECM) <- c(paste0(names_[1],"_fore_VAR"), paste0(names_[1],"_lower_VAR"), paste0(names_[1],"_upper_VAR"))

# lets do the same for cpi forecasts 
c2_forecast.VECM <- xts(crypto_pair.vecm.1.asVAR.forecast$fcst$log_dogecoin[,-4], 
                        head(index(crypto_pair_oos), 7))
names(c2_forecast.VECM) <- c(paste0(names_[2],"_fore_VAR"), paste0(names_[2],"_lower_VAR"), paste0(names_[2],"_upper_VAR"))

# add oos observations
crypto_pair_all_ <- rbind(crypto_pair[,-ncol(crypto_pair)], head(crypto_pair_oos, 7))

# lets put the data together
crypto_pair_VECM <- merge(crypto_pair_all_,
                         c1_forecast.VECM,
                         c2_forecast.VECM)


plot(crypto_pair_VECM[(nrow(crypto_pair_VECM)-30):nrow(crypto_pair_VECM), grep("^log_bitcoin", names(crypto_pair_VECM))], 
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 3,
     main = paste0("7 days forecast of ", names(crypto_pair_VECM))[1],
     col = c("black", "blue", "red", "red"))

plot(crypto_pair_VECM[(nrow(crypto_pair_VECM)-30):nrow(crypto_pair_VECM), grep("^log_dogecoin", names(crypto_pair_VECM))], 
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 3,
     main = paste0("7 days forecast of ", names(crypto_pair_VECM))[2],
     col = c("black", "blue", "red", "red"))

# errors
crypto_pair_VECM$mae.log_bitcoin   <-  abs(crypto_pair_VECM$log_bitcoin - crypto_pair_VECM$log_bitcoin_fore)
crypto_pair_VECM$mse.log_bitcoin <-  (crypto_pair_VECM$log_bitcoin - crypto_pair_VECM$log_bitcoin_fore)^2
crypto_pair_VECM$mape.log_bitcoin  <-  abs((crypto_pair_VECM$log_bitcoin - crypto_pair_VECM$log_bitcoin_fore)/crypto_pair_VECM$log_bitcoin)
crypto_pair_VECM$amape.log_bitcoin <-  abs((crypto_pair_VECM$log_bitcoin - crypto_pair_VECM$log_bitcoin_fore) / 
                                            (crypto_pair_VECM$log_bitcoin + crypto_pair_VECM$log_bitcoin_fore))

crypto_pair_VECM$mae.log_dogecoin   <-  abs(crypto_pair_VECM$log_dogecoin - crypto_pair_VECM$log_dogecoin_fore)
crypto_pair_VECM$mse.log_dogecoin   <-  (crypto_pair_VECM$log_dogecoin - crypto_pair_VECM$log_dogecoin_fore)^2
crypto_pair_VECM$mape.log_dogecoin  <-  abs((crypto_pair_VECM$log_dogecoin - crypto_pair_VECM$log_dogecoin_fore)/crypto_pair_VECM$log_dogecoin)
crypto_pair_VECM$amape.log_dogecoin <-  abs((crypto_pair_VECM$log_dogecoin - crypto_pair_VECM$log_dogecoin_fore) / 
                                             (crypto_pair_VECM$log_dogecoin + crypto_pair_VECM$log_dogecoin_fore))


#
colMeans(crypto_pair_VECM[,11:18], na.rm = TRUE)
# mae.log_bitcoin    mse.log_bitcoin   mape.log_bitcoin  amape.log_bitcoin   mae.log_dogecoin   mse.log_dogecoin  mape.log_dogecoin amape.log_dogecoin 
# 0.0245712339       0.0008026245       0.0026783285       0.0013367832       0.0124338330       0.0001816705       0.0020868074       0.0010446561 


# restrict VECM model
# https://quant.stackexchange.com/questions/21215/imposing-restrictions-on-cointegrating-vectors-r-example
