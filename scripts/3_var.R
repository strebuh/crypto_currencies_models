library(xts)
library(forecast)
library(lmtest)
library(vars)

source("functions/finding_stationary_pair.R")

# ---------------------------------------------------- PREPARE DATA ---------------------------------------------------------------------------

# load list of dataframes
crypto_list <- readRDS("./data/crypto_currencies_data.RDS")

# load dataframe with results of cointegration test of each comibation of variables
cointegr_tb_signf2 <- readRDS("./data/cointegr_tb_signf2.RDS")

# get plots of given pair
# crypto_pair <- cryptoPairPlots(crypto_list, cointegr_tb_signf2, n = 28, colerograms = TRUE, diffPlots=TRUE, nObs = 398)
# 398 = 365 + 30 + 3

# get dataframe with prices and first differences of chosen cointegrated pair 
crypto_pair_all <- getDifferencesXTS(cointegr_tb_signf2, n_table = 28, n_obs_is = 365, n_obs_ooc = 30, crypto_list)

# retrieve data from object

# in sample
crypto_pair <- crypto_pair_all$in_smpl 
dim(crypto_pair) # 15  4, 15 days for prediction 

# out of sample
crypto_pair_oos <- head(crypto_pair_all$oo_smpl, 15) 
dim(crypto_pair_oos) # [1] 365   4, 365 days for building model 

# compare if end of in-sample matches start of out-of-sample
tail(crypto_pair)
head(crypto_pair_oos)

# ------------------------------------------------- VAR MODEL ------------------------------------------------------------------------------
# selection without seasons
VARselect(crypto_pair[,1:2], lag.max = 14)

# AIC(n)  HQ(n)  SC(n) FPE(n) 
#     6      1      1      6 
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 1      1      1      1 

# -------------------------------------------------------------------------------------------------------------------------------
# selection without weakly seasons
VARselect(crypto_pair[,1:2],
          lag.max = 14,     
          season = 7)     

# $selection
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 1      1      1      1 


# -------------------------------------------------------------------------------------------------------------------------------
# VAR model with 6 lags and seasons
crypto_pair.var6s <- VAR(crypto_pair[,1:2],
                         p = 6,
                         season = 7) 

summary(crypto_pair.var6s) ## you rarely interpet interpret ## ~44:00
# seasons are not significant

#--------------------
# siacoin = cardano.l1 + siacoin.l1 + cardano.l2 + siacoin.l2 + cardano.l3 + siacoin.l3 + cardano.l4 + siacoin.l4 + cardano.l5 + siacoin.l5 + 
#   cardano.l6 + siacoin.l6 + const + sd1 + sd2 + sd3 + sd4 + sd5 + sd6 

# cardano.l1  6.126e-03  3.278e-03   1.869   0.0625 .  
# siacoin.l1  7.739e-01  6.942e-02  11.147   <2e-16 ***
# cardano.l3 -8.538e-03  4.446e-03  -1.920   0.0556 .  
# siacoin.l3  2.072e-01  9.204e-02   2.251   0.0250 *  
# siacoin.l6 -1.514e-01  6.905e-02  -2.193   0.0290 *  
# const       4.531e-05  2.108e-05   2.149   0.0323 *
# only few variables significant

# cardano = cardano.l1 + siacoin.l1 + cardano.l2 + siacoin.l2 + cardano.l3 + siacoin.l3 + cardano.l4 + siacoin.l4 + cardano.l5 + siacoin.l5 + cardano.l6 + siacoin.l6 
# + const + sd1 + sd2 + sd3 + sd4 + sd5 + sd6 
# cardano.l1  1.0076410  0.0699406  14.407  < 2e-16 ***
# siacoin.l1 -2.9000742  1.4813949  -1.958  0.05109 . 
# cardano.l3 -0.2493847  0.0948721  -2.629  0.00896 ** 
# cardano.l4  0.1735540  0.0941413   1.844  0.06612 .  
# const       0.0010721  0.0004499   2.383  0.01772 *  
  

# -------------------------------------------------------------------------------------------------------------------------------
# model without seasonality component
crypto_pair.var6 <- VAR(crypto_pair[,1:2], p = 6)

summary(crypto_pair.var6)
# cardano = cardano.l1 + siacoin.l1 + cardano.l2 + siacoin.l2 + cardano.l3 + siacoin.l3 + 
#   cardano.l4 + siacoin.l4 + cardano.l5 + siacoin.l5 + cardano.l6 + siacoin.l6 + const
# cardano.l1  0.9942175  0.0694343  14.319  < 2e-16 ***
# siacoin.l1 -2.8090025  1.4808967  -1.897  0.05868 .  
# cardano.l3 -0.2557171  0.0938754  -2.724  0.00678 ** 
# cardano.l4  0.1699510  0.0931632   1.824  0.06898 .  
# const       0.0010819  0.0004515   2.397  0.01708 *  
  
# siacoin = cardano.l1 + siacoin.l1 + cardano.l2 + siacoin.l2 + cardano.l3 + siacoin.l3 + 
#   cardano.l4 + siacoin.l4 + cardano.l5 + siacoin.l5 + cardano.l6 + siacoin.l6 + const 
# cardano.l1  5.653e-03  3.235e-03   1.748   0.0814 .  
# siacoin.l1  7.800e-01  6.899e-02  11.307   <2e-16 ***
# cardano.l3 -8.475e-03  4.373e-03  -1.938   0.0534 .  
# siacoin.l3  2.084e-01  9.153e-02   2.276   0.0234 *  
# siacoin.l6 -1.422e-01  6.865e-02  -2.072   0.0390 *  
# const       4.546e-05  2.103e-05   2.161   0.0314 *  
  

serial.test(crypto_pair.var6) ## 57:00+
# data:  Residuals of VAR object crypto_pair.var1
# Chi-squared = 46.402, df = 40, p-value = 0.2253

serial.test(crypto_pair.var6, type = "BG") # The null hypothesis is that there is no serial correlation of any order up to p.
# data:  Residuals of VAR object crypto_pair.var1
# Chi-squared = 22.503, df = 20, p-value = 0.3139

# Box.test(resid(crypto_pair.var6)[,1], type = "Ljung-Box", lag =  6) 
# Box.test(resid(crypto_pair.var6)[,2], type = "Ljung-Box", lag =  6)

# lets do some basic diagnostics
plot(crypto_pair.var6)
# no autocorrelation

# ------------------ restricted VAR 1
# automatic restriction
restrict(crypto_pair.var1, method = "ser")
# cardano = cardano.l1 + siacoin.l1 + cardano.l2 + cardano.l3 + siacoin.l3 + siacoin.l6 + const 
# siacoin = cardano.l1 + siacoin.l1 + cardano.l3 + siacoin.l3 + cardano.l5 + siacoin.l6 
# restrict <- matrix(c(1, 1, 1, 1, 1, 1, 1,
#                      1, 0, 1, 0, 0, 1, 0),
#                    nrow=2, ncol=7, byrow=TRUE)


# The Granger causality results showed the bi-directional feedback on 5% level.
casuality <- readRDS("./gr_casual_d_siacoin_d_cardano.RDS")
#   lags d_siacoin_d_cardano if_granger_1  d_cardano_d_siacoin if_granger_2
# 1    1   0.276727171250911           no   0.0180362794605223        cause
# 2    2  0.0307660109222102        cause  0.00084803166852094        cause
# 3    3   0.045346250988177        cause 0.000842796559054635        cause
# 4    4  0.0929440887878814           no  0.00196218371352549        cause
# 5    5   0.105121398655361           no  0.00242529384210634        cause
# 6    6  0.0316361829048299        cause  0.00472698599569277        cause
# 7    7 0.00319632534714409        cause  0.00524864370423215        cause
# restrict based on Granger casuality test result and previous model results

# only GRANGER
restrict1 <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, # ind-cardano
                      0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1), # ind-siacoin
                   nrow=2, ncol=13, byrow=TRUE) # only GC

crypto_pair.var6_restr1 <- restrict(crypto_pair.var6, method = "man", resmat = restrict1)
summary(crypto_pair.var6_restr1)
# cardano = cardano.l1 + siacoin.l1 + cardano.l2 + siacoin.l2 + cardano.l3 + siacoin.l3 + 
#   cardano.l4 + siacoin.l4 + cardano.l5 + siacoin.l5 + cardano.l6 + siacoin.l6 + const
# cardano.l1  0.9942175  0.0694343  14.319  < 2e-16 ***
# siacoin.l1 -2.8090025  1.4808967  -1.897  0.05868 .
# cardano.l3 -0.2557171  0.0938754  -2.724  0.00678 ** 
# cardano.l4  0.1699510  0.0931632   1.824  0.06898 .  
# const       0.0010819  0.0004515   2.397  0.01708 *  
  
# siacoin = siacoin.l1 + cardano.l2 + siacoin.l2 + cardano.l3 + siacoin.l3 + siacoin.l4 + siacoin.l5 + cardano.l6 + siacoin.l6 + const 
# siacoin.l1  8.528e-01  5.284e-02  16.139  < 2e-16 ***
# cardano.l2  5.794e-03  3.244e-03   1.786 0.075010 . 
# siacoin.l3  1.916e-01  8.729e-02   2.195 0.028823 *  
# cardano.l6  3.508e-03  2.092e-03   1.677 0.094399 .  
# siacoin.l6 -1.962e-01  5.869e-02  -3.343 0.000919 ***
# const       4.905e-05  2.104e-05   2.331 0.020296 *  

serial.test(crypto_pair.var6_restr1, type = "BG") 
# data:  Residuals of VAR object crypto_pair.var6_restr1
# Chi-squared = 29.258, df = 20, p-value = 0.08281
serial.test(crypto_pair.var6_restr1) 
# data:  Residuals of VAR object crypto_pair.var6_restr1
# Chi-squared = 57.742, df = 40, p-value = 0.03431

plot(crypto_pair.var6_restr1)
# although the plot looks allright, the Portmanteau test for autocorrelation revealsproblem of autocorrelation of residuals 

# ------------------ restricted VAR 2

# interesting notion, that for siacoin as dependent variable eqation, at lag one cardano is not Granger cause, although this component is significant
# as  regressor at 10% significance level (~6%), lags 2,3,6,7 are not significant at all

# Granger and first VAR model restriction
restrict2 <- matrix(c(1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, # ind-cardano
                      0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1), # ind-siacoin
                    nrow=2, ncol=13, byrow=TRUE)

# lags d_siacoin_d_cardano if_granger_1 
#    1   0.276727171250911           no 
#    2  0.0307660109222102        cause 
#    3   0.045346250988177        cause 
#    4  0.0929440887878814           no 
#    5   0.105121398655361           no 
#    6  0.0316361829048299        cause 
#    7 0.00319632534714409        cause 


# cardano = cardano.l1 + siacoin.l1 + cardano.l2 + siacoin.l2 + cardano.l3 + siacoin.l3 + 
#   cardano.l4 + siacoin.l4 + cardano.l5 + siacoin.l5 + cardano.l6 + siacoin.l6 + const
# cardano.l1  0.9942175  0.0694343  14.319  < 2e-16 ***
# siacoin.l1 -2.8090025  1.4808967  -1.897  0.05868 .  
# cardano.l3 -0.2557171  0.0938754  -2.724  0.00678 ** 
# cardano.l4  0.1699510  0.0931632   1.824  0.06898 .  
# const       0.0010819  0.0004515   2.397  0.01708 *  

# siacoin = cardano.l1 + siacoin.l1 + cardano.l2 + siacoin.l2 + cardano.l3 + siacoin.l3 + 
#   cardano.l4 + siacoin.l4 + cardano.l5 + siacoin.l5 + cardano.l6 + siacoin.l6 + const 
# cardano.l1  5.653e-03  3.235e-03   1.748   0.0814 .  
# siacoin.l1  7.800e-01  6.899e-02  11.307   <2e-16 ***
# cardano.l3 -8.475e-03  4.373e-03  -1.938   0.0534 .  
# siacoin.l3  2.084e-01  9.153e-02   2.276   0.0234 *  
# siacoin.l6 -1.422e-01  6.865e-02  -2.072   0.0390 *  
# const       4.546e-05  2.103e-05   2.161   0.0314 *  

crypto_pair.var6_restr2 <- restrict(crypto_pair.var6, method = "man", resmat = restrict2)
summary(crypto_pair.var6_restr2)

# cardano = cardano.l1 + siacoin.l1 + cardano.l3 + cardano.l4 + const 
# cardano.l1  1.0086771  0.0400269  25.200  < 2e-16 ***
# siacoin.l1 -1.4883032  0.5434007  -2.739  0.00648 ** 
# cardano.l3 -0.1091386  0.0634560  -1.720  0.08632 .  
# cardano.l4  0.1378062  0.0519236   2.654  0.00831 ** 
# const       0.0010980  0.0004418   2.485  0.01340 *  

# siacoin.l1  8.747e-01  3.967e-02  22.050  < 2e-16 ***
# cardano.l3  2.967e-03  9.784e-04   3.033  0.00260 ** 
# siacoin.l3  1.136e-01  5.046e-02   2.252  0.02491 *  
# siacoin.l6 -8.486e-02  3.234e-02  -2.624  0.00906 ** 
# const       4.749e-05  2.089e-05   2.273  0.02363 * 

serial.test(crypto_pair.var6_restr2, type = "BG") 
# data:  Residuals of VAR object crypto_pair.var6_restr2
# Chi-squared = 35.334, df = 20, p-value = 0.0184
serial.test(crypto_pair.var6_restr2) 
# data:  Residuals of VAR object crypto_pair.var6_restr2
# Chi-squared = 80.197, df = 40, p-value = 0.0001671

plot(crypto_pair.var6_restr2)
# although the plot looks allright, the Portmanteau test for autocorrelation revealsproblem of autocorrelation of residuals 

# ---------------------------------------------------------------------------------------------------------------------------------------------
AIC(crypto_pair.var6, crypto_pair.var6_restr1, crypto_pair.var6_restr2)
BIC(crypto_pair.var6, crypto_pair.var6_restr1, crypto_pair.var6_restr2)
# > AIC(crypto_pair.var6, crypto_pair.var6_restr1, crypto_pair.var6_restr2)
# df       AIC
# crypto_pair.var6        26 -8900.090
# crypto_pair.var6_restr1 23 -8895.827
# crypto_pair.var6_restr2 10 -8897.418
# > BIC(crypto_pair.var6, crypto_pair.var6_restr1, crypto_pair.var6_restr2)
# df       BIC
# crypto_pair.var6        26 -8799.123
# crypto_pair.var6_restr1 23 -8806.510
# crypto_pair.var6_restr2 10 -8858.585

# AIC prefere the most restricted model, BIC the leastwhich makes sense, however due to correlation in residuals

#----------------------------------------------------------------- FORECAST -------------------------------------------------------------------

# and run a forecast
crypto_pair.var6.forecast <- predict(crypto_pair.var6,
                                     n.ahead = 15,
                                     ci = 0.95) 

names(crypto_pair.var6)
# [1] "fcst"     "endog"    "model"    "exo.fcst"

# VAR forecasts for both currencies
crypto_pair.var6.forecast$fcst$cardano
crypto_pair.var6.forecast$fcst$siacoin

dim(crypto_pair_oos)
head(crypto_pair_oos)
cardano_forecast_VAR <- xts(crypto_pair.var6.forecast$fcst$cardano[,-4], 
                        index(crypto_pair_oos))
# lets change the names 
names(cardano_forecast_VAR)
names(cardano_forecast_VAR) <- c("cardano_fore_VAR", "cardano_lower_VAR", "cardano_upper_VAR")

# lets do the same for cpi forecasts 
siacoin_forecast_VAR <- xts(crypto_pair.var6.forecast$fcst$siacoin[,-4], 
                        index(crypto_pair_oos))
names(siacoin_forecast_VAR) <- c("siacoin_fore_VAR", "siacoin_lower_VAR", "siacoin_upper_VAR")

dim(crypto_pair_oos) # [1] 15  4
# crypto_pair[(nrow(crypto_pair)-29):nrow(crypto_pair),1:4] <- crypto_pair_oos

# add oos observations
crypto_pair <- rbind(crypto_pair, crypto_pair_oos)
tail(crypto_pair, 20)

# lets put the data together
crypto_pair_VAR <- merge(crypto_pair,
                     cardano_forecast_VAR,
                     siacoin_forecast_VAR)
names(crypto_pair_VAR)
# [1] "cardano"           "siacoin"           "d_cardano"         "d_siacoin"         "cardano_fore_VAR"  "cardano_lower_VAR" "cardano_upper_VAR" "siacoin_fore_VAR" 
# [9] "siacoin_lower_VAR" "siacoin_upper_VAR"

plot(crypto_pair_VAR[, c("cardano", "cardano_fore_VAR",
                     "cardano_lower_VAR", "cardano_upper_VAR")], 
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 3,
     main = "15 days forecast of cardano",
     col = c("black", "blue", "red", "red"))

plot(crypto_pair_VAR[, c("siacoin", "siacoin_fore_VAR",
                     "siacoin_lower_VAR", "siacoin_upper_VAR")], 
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 3,
     main = "15 days forecast of siacoin",
     col = c("black", "blue", "red", "red"))

# errors
crypto_pair_VAR$mae.cardano   <-  abs(crypto_pair_VAR$cardano - crypto_pair_VAR$cardano_fore)
crypto_pair_VAR$mse.cardano   <-  (crypto_pair_VAR$cardano - crypto_pair_VAR$cardano_fore)^2
crypto_pair_VAR$mape.cardano  <-  abs((crypto_pair_VAR$cardano - crypto_pair_VAR$cardano_fore)/crypto_pair_VAR$cardano)
crypto_pair_VAR$amape.cardano <-  abs((crypto_pair_VAR$cardano - crypto_pair_VAR$cardano_fore) / 
                                    (crypto_pair_VAR$cardano + crypto_pair_VAR$cardano_fore))
crypto_pair_VAR$mae.siacoin   <-  abs(crypto_pair_VAR$siacoin - crypto_pair_VAR$siacoin_fore)
crypto_pair_VAR$mse.siacoin   <-  (crypto_pair_VAR$siacoin - crypto_pair_VAR$siacoin_fore)^2
crypto_pair_VAR$mape.siacoin  <-  abs((crypto_pair_VAR$siacoin - crypto_pair_VAR$siacoin_fore)/crypto_pair_VAR$siacoin)
crypto_pair_VAR$amape.siacoin <-  abs((crypto_pair_VAR$siacoin - crypto_pair_VAR$siacoin_fore) / 
                                    (crypto_pair_VAR$siacoin + crypto_pair_VAR$siacoin_fore))

# get measures
colMeans(crypto_pair_VAR[,11:18], na.rm = TRUE)
# mae.cardano   mse.cardano  mape.cardano amape.cardano   mae.siacoin   mse.siacoin  mape.siacoin amape.siacoin 
# 5.283817e-03  4.661611e-05  9.078719e-02  4.884206e-02  1.348296e-04  3.538193e-08  5.895547e-02  3.099178e-02 


# --------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------- VECM ----------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------------------------------------------
johan.test.trace <- ca.jo(crypto_pair[,1:2],         
                          ecdet = "const", # "none", "const", "trend"
                          type = "trace",  
                          K = 7) 
                         #  ,        
                         # season = 7)

summary(johan.test.trace) 
# Eigenvalues (lambda):
#   [1]  5.226739e-02  1.263946e-02 -2.039714e-20
# 
# Values of teststatistic and critical values of test:
#   
#   test 10pct  5pct  1pct
# r <= 1 |  4.57  7.52  9.24 12.97
# r = 0  | 23.84 17.85 19.96 24.60

# https://www.researchgate.net/post/2_variables_and_2_cointegrating_equations_can_anyone_help_me2
johan.test.eigen <- ca.jo(crypto_pair[,1:2],         # data 
                          ecdet = "const", 
                          type = "eigen",  
                          K = 7) 
# ,           
#                           season = 12) 
summary(johan.test.eigen) 

#-------------------------------------- model ------------------------------------------------------------------------------
crypto_pair.vecm.1 <- cajorls(johan.test.eigen, r = 1) 

summary(crypto_pair.vecm.1$rlm)

# estimates of the VECM model
crypto_pair.vecm.1$beta
#                   ect1
# cardano.l7   1.00000000
# siacoin.l7 -29.94066366
# constant     0.01103105

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
# Chi-squared = 19.97, df = 20, p-value = 0.4598
# no autocorrelation

plot(serial.test(crypto_pair.vecm.1.asVAR))


# EDF = Empirical Distribution Function
# lets check normality of residuals
# this is Jarque-Bera (JB) test
# Johansen test is sensitive to lack of normality

normality.test(crypto_pair.vecm.1.asVAR)
# Chi-squared = 1519.8, df = 4, p-value < 2.2e-16
# H0: data are from a normal distribution
# data is not from normal distribution

#----------------------------------------------------------------- FORECAST -------------------------------------------------------------------

# and run a forecast
crypto_pair.vecm.1.asVAR.forecast <- predict(crypto_pair.vecm.1.asVAR,
                                             n.ahead = 15,
                                             ci = 0.95) # 95% confidence interval

# lets see the result
crypto_pair.vecm.1.asVAR.forecast
names(crypto_pair.vecm.1.asVAR.forecast)
# [1] "fcst"     "endog"    "model"    "exo.fcst"
# str(crypto_pair.vecm.1.asVAR.forecast)


# VAR forecasts for both currencies
crypto_pair.vecm.1.asVAR.forecast$fcst$cardano
crypto_pair.vecm.1.asVAR.forecast$fcst$siacoin


head(crypto_pair_oos)
cardano_forecast <- xts(crypto_pair.vecm.1.asVAR.forecast$fcst$cardano[,-4], 
                        index(crypto_pair_oos))

# lets change the names 
names(cardano_forecast)
names(cardano_forecast) <- c("cardano_fore", "cardano_lower", "cardano_upper")

names(crypto_pair)
# lets do the same for cpi forecasts 
siacoin_forecast <- xts(crypto_pair.vecm.1.asVAR.forecast$fcst$siacoin[,-4], 
                    index(crypto_pair_oos))

names(siacoin_forecast) <- c("siacoin_fore", "siacoin_lower", "siacoin_upper")

dim(crypto_pair_oos) # [1] 15  4
crypto_pair[(nrow(crypto_pair)-29):nrow(crypto_pair),1:4] <- crypto_pair_oos

# add oos observations
crypto_pair <- rbind(crypto_pair, crypto_pair_oos)

# lets put the data together
crypto_pair <- merge(crypto_pair,
                     cardano_forecast,
                     siacoin_forecast)
names(crypto_pair)
# [1] "cardano"       "siacoin"       "d_cardano"     "d_siacoin"     "cardano_fore"  "cardano_lower" "cardano_upper" "siacoin_fore"  "siacoin_lower" "siacoin_upper"

plot(crypto_pair[, c("cardano", "cardano_fore",
                     "cardano_lower", "cardano_upper")], 
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 3,
     main = "15 days forecast of energy cardano",
     col = c("black", "blue", "red", "red"))

dim(crypto_pair_oos) # [1] 15  4
plot(crypto_pair[, c("siacoin", "siacoin_fore",
                 "siacoin_lower", "siacoin_upper")], 
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 3,
     main = "15 days forecast of energy siacoin",
     col = c("black", "blue", "red", "red"))

# errors
crypto_pair$mae.cardano   <-  abs(crypto_pair$cardano - crypto_pair$cardano_fore)
crypto_pair$mse.cardano   <-  (crypto_pair$cardano - crypto_pair$cardano_fore)^2
crypto_pair$mape.cardano  <-  abs((crypto_pair$cardano - crypto_pair$cardano_fore)/crypto_pair$cardano)
crypto_pair$amape.cardano <-  abs((crypto_pair$cardano - crypto_pair$cardano_fore) / 
                            (crypto_pair$cardano + crypto_pair$cardano_fore))

crypto_pair$mae.siacoin   <-  abs(crypto_pair$siacoin - crypto_pair$siacoin_fore)
crypto_pair$mse.siacoin   <-  (crypto_pair$siacoin - crypto_pair$siacoin_fore)^2
crypto_pair$mape.siacoin  <-  abs((crypto_pair$siacoin - crypto_pair$siacoin_fore)/crypto_pair$siacoin)
crypto_pair$amape.siacoin <-  abs((crypto_pair$siacoin - crypto_pair$siacoin_fore) / 
                            (crypto_pair$siacoin + crypto_pair$siacoin_fore))

dim(crypto_pair[,11:18])

#
colMeans(crypto_pair[,11:18], na.rm = TRUE)
# mae.cardano   mse.cardano  mape.cardano amape.cardano   mae.siacoin   mse.siacoin  mape.siacoin amape.siacoin 
# 4.535449e-03  3.801145e-05  7.734426e-02  4.143065e-02  1.275986e-04  3.120179e-08  5.595134e-02  2.924735e-02 


tail(crypto_pair)
# restrict VECM model
# https://quant.stackexchange.com/questions/21215/imposing-restrictions-on-cointegrating-vectors-r-example
