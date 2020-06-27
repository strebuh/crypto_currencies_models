library(xts)
library(forecast)
library(lmtest)
library(vars)

source("functions/finding_stationary_pair.R")

# load list of dataframes
crypto_list <- readRDS("./data/crypto_currencies_data.RDS")
# 0.3862           0.01  -3.834585 1 0 -0.8827 compare combin_adf
names_ <- names(crypto_pair)

# crypto_pair <- cryptoPairPlots(crypto_list, smpl1_dt_sect, n = 5, colerograms = TRUE, diffPlots=TRUE, nObs = 504)

# load dataframe with results of cointegration test of each comibation of variables
cointegr_tb_signf2 <- readRDS("./data/cointegr_tb_signf2.RDS")

# get dataframe with prices and first differences of chosen cointegrated pair 
crypto_pair_all <- getDifferencesXTS(cointegr_tb_signf2, n_table = 28, n_obs_is = 365, n_obs_ooc = 30, crypto_list)
class(crypto_pair_all) # list

crypto_pair <- crypto_pair_all$in_smpl
crypto_pair_oos <- head(crypto_pair_all$oo_smpl, 15)
dim(crypto_pair_oos) #[1] 15  4
tail(crypto_pair)
head(crypto_pair_oos)

dim(crypto_pair) # [1] 365   4

# -------------------------------------------------------------------------------------------------------------------------------
VARselect(crypto_pair[,1:2], # input data for VAR
          lag.max = 14)     # maximum lag

# $selection
# AIC(n)  HQ(n)  SC(n) FPE(n) 
#     6      1      1      6 
# 
# $criteria
# 1             2             3             4             5             6             7             8             9            10            11
# AIC(n) -3.049383e+01 -3.049253e+01 -3.048485e+01 -3.048451e+01 -3.047849e+01 -3.049506e+01 -3.047835e+01 -3.046960e+01 -3.045579e+01 -3.044057e+01 -3.044107e+01
# HQ(n)  -3.046756e+01 -3.044875e+01 -3.042356e+01 -3.040571e+01 -3.038218e+01 -3.038124e+01 -3.034702e+01 -3.032075e+01 -3.028944e+01 -3.025670e+01 -3.023969e+01
# SC(n)  -3.042783e+01 -3.038253e+01 -3.033086e+01 -3.028652e+01 -3.023650e+01 -3.020907e+01 -3.014837e+01 -3.009562e+01 -3.003781e+01 -2.997859e+01 -2.993510e+01
# FPE(n)  5.710827e-14  5.718285e-14  5.762374e-14  5.764401e-14  5.799328e-14  5.704205e-14  5.800522e-14  5.851790e-14  5.933511e-14  6.024956e-14  6.022482e-14
# 12            13            14
# AIC(n) -3.042650e+01 -3.042748e+01 -3.041561e+01
# HQ(n)  -3.020762e+01 -3.019109e+01 -3.016171e+01
# SC(n)  -2.987653e+01 -2.983352e+01 -2.977765e+01
# FPE(n)  6.111495e-14  6.106273e-14  6.180087e-14

# -------------------------------------------------------------------------------------------------------------------------------
VARselect(crypto_pair[,1:2], # input data for VAR
          lag.max = 14,     # maximum lag
          season = 7)     # seasonal frequency

# $selection
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 1      1      1      1 
# 
# $criteria
#                   1             2             3             4             5             6             7             8             9            10            11
# AIC(n) -3.046457e+01 -3.046076e+01 -3.045229e+01 -3.045186e+01 -3.044669e+01 -3.046205e+01 -3.044449e+01 -3.043951e+01 -3.042532e+01 -3.040847e+01 -3.040926e+01
# HQ(n)  -3.038577e+01 -3.036445e+01 -3.033847e+01 -3.032053e+01 -3.029785e+01 -3.029570e+01 -3.026063e+01 -3.023813e+01 -3.020644e+01 -3.017208e+01 -3.015535e+01
# SC(n)  -3.026658e+01 -3.021877e+01 -3.016631e+01 -3.012188e+01 -3.007271e+01 -3.004408e+01 -2.998252e+01 -2.993354e+01 -2.987535e+01 -2.981450e+01 -2.977129e+01
# FPE(n)  5.880510e-14  5.903068e-14  5.953412e-14  5.956197e-14  5.987369e-14  5.896450e-14  6.001367e-14  6.031889e-14  6.118720e-14  6.223479e-14  6.219496e-14
# 12            13            14
# AIC(n) -3.039454e+01 -3.039376e+01 -3.038187e+01
# HQ(n)  -3.012312e+01 -3.010483e+01 -3.007543e+01
# SC(n)  -2.971257e+01 -2.966780e+01 -2.961191e+01
# FPE(n)  6.312794e-14  6.318915e-14  6.395865e-14

# -------------------------------------------------------------------------------------------------------------------------------
# let's comput model with 6 lags
crypto_pair.var1s <- VAR(crypto_pair[,1:2],
                    p = 6,  # order of VAR model
                    season = 7 ) ## include in both the month?

summary(crypto_pair.var1s) ## you rarely interpet interpret ## ~44:00
# seasons are not significant

# -------------------------------------------------------------------------------------------------------------------------------
crypto_pair.var1 <- VAR(crypto_pair[,1:2], p = 6)
summary(crypto_pair.var1)
# cardano = 
# Estimate Std. Error t value Pr(>|t|)    
# cardano.l1  0.9942175  0.0694343  14.319  < 2e-16 ***
#   siacoin.l1 -2.8090025  1.4808967  -1.897  0.05868 .  
# cardano.l2  0.1030454  0.0944719   1.091  0.27614    
# siacoin.l2  1.0956634  1.9659536   0.557  0.57767    
# cardano.l3 -0.2557171  0.0938754  -2.724  0.00678 ** 
#   siacoin.l3  2.2915550  1.9648335   1.166  0.24430    
# cardano.l4  0.1699510  0.0931632   1.824  0.06898 .  
# siacoin.l4 -1.5124139  1.9645746  -0.770  0.44192    
# cardano.l5  0.1263938  0.0931861   1.356  0.17587    
# siacoin.l5  0.2143595  1.9581938   0.109  0.91289    
# cardano.l6 -0.1069923  0.0706160  -1.515  0.13065    
# siacoin.l6 -0.5961106  1.4735938  -0.405  0.68607    
# const       0.0010819  0.0004515   2.397  0.01708 *  

# siacoin 
# Estimate Std. Error t value Pr(>|t|)    
# cardano.l1  5.653e-03  3.235e-03   1.748   0.0814 .  
# siacoin.l1  7.800e-01  6.899e-02  11.307   <2e-16 ***
# cardano.l2  9.620e-04  4.401e-03   0.219   0.8271    
# siacoin.l2  6.258e-03  9.158e-02   0.068   0.9456    
# cardano.l3 -8.475e-03  4.373e-03  -1.938   0.0534 .  
# siacoin.l3  2.084e-01  9.153e-02   2.276   0.0234 *  
#   cardano.l4  5.022e-04  4.340e-03   0.116   0.9080    
# siacoin.l4  3.731e-02  9.152e-02   0.408   0.6837    
# cardano.l5  5.773e-03  4.341e-03   1.330   0.1845    
# siacoin.l5 -7.128e-03  9.122e-02  -0.078   0.9378    
# cardano.l6 -5.466e-04  3.290e-03  -0.166   0.8681    
# siacoin.l6 -1.422e-01  6.865e-02  -2.072   0.0390 *  
  # const       4.546e-05  2.103e-05   2.161   0.0314 * 

serial.test(crypto_pair.var1) ## 57:00+
# data:  Residuals of VAR object crypto_pair.var1
# Chi-squared = 46.402, df = 40, p-value = 0.2253

serial.test(crypto_pair.var1, type = "BG") # The null hypothesis is that there is no serial correlation of any order up to p.
# data:  Residuals of VAR object crypto_pair.var1
# Chi-squared = 22.503, df = 20, p-value = 0.3139

Box.test(resid(crypto_pair.var1)[,1], type = "Ljung-Box", lag =  6) # The data are independently distributed 
Box.test(resid(crypto_pair.var1)[,2], type = "Ljung-Box", lag =  6)
# both reject

# lets do some basic diagnostics
plot(crypto_pair.var1)
# no autocorrelation

# -------------------------------------------------------------------------------------------------------------------------------
restrict(crypto_pair.var1, method = "ser")
# cardano = cardano.l1 + siacoin.l1 + cardano.l2 + cardano.l3 + siacoin.l3 + siacoin.l6 + const 
# siacoin = cardano.l1 + siacoin.l1 + cardano.l3 + siacoin.l3 + cardano.l5 + siacoin.l6 
restrict <- matrix(c(1, 1, 1, 1, 1, 1, 
                     1, 0, 1, 0, 0, 1),
                   nrow=2, ncol=6, byrow=TRUE)


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
restrict2 <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                      1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1),
                   nrow=2, ncol=13, byrow=TRUE)
crypto_pair.var1_restr <- restrict(crypto_pair.var1, method = "man", resmat = restrict2)
# [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
# [1,]    1    1    1    1    1    1    1    1    1     1     1     1     1
# [2,]    1    1    1    1    1    1    0    1    1     0     0     1     1
# > restrict(crypto_pair.var1, method = "man", resmat = restrict2)
# 
# VAR Estimation Results:
#   ======================= 
#   
#   Estimated coefficients for equation cardano: 
#   ============================================ 
#   Call:
#   cardano = cardano.l1 + siacoin.l1 + cardano.l2 + siacoin.l2 + cardano.l3 + siacoin.l3 + cardano.l4 + siacoin.l4 + cardano.l5 + siacoin.l5 + cardano.l6 + siacoin.l6 + const 
# 
# cardano.l1   siacoin.l1   cardano.l2   siacoin.l2   cardano.l3   siacoin.l3   cardano.l4   siacoin.l4   cardano.l5   siacoin.l5   cardano.l6   siacoin.l6 
# 0.997806735 -3.803095502  0.264256093 -0.407698073 -0.348172416  6.052334765  0.120188502 -3.244932861 -0.013295132  2.349845793 -0.004565477 -2.154955538 
# const 
# 0.001574729 
# 
# 
# Estimated coefficients for equation siacoin: 
#   ============================================ 
#   Call:
#   siacoin = cardano.l1 + siacoin.l1 + cardano.l2 + siacoin.l2 + cardano.l3 + siacoin.l3 + siacoin.l4 + cardano.l5 + siacoin.l6 + const 
# 
# cardano.l1    siacoin.l1    cardano.l2    siacoin.l2    cardano.l3    siacoin.l3    siacoin.l4    cardano.l5    siacoin.l6         const 
# 4.089622e-03  7.531305e-01  5.796415e-03 -3.594466e-02 -1.113982e-02  3.132136e-01 -2.338287e-02  5.727554e-03 -1.357849e-01  4.004316e-05 

summary.var1_restr <- summary(crypto_pair.var1_restr)

serial.test(crypto_pair.var1_restr, type = "BG") 
serial.test(crypto_pair.var1_restr) 
# data:  Residuals of VAR object crypto_pair.var1_restricted
# Chi-squared = 19.128, df = 20, p-value = 0.5135
# still no auticorrelation, although lower p-value

# ---------------------------------------------------------------------------------------------------------------------------------------------
VARbic(crypto_pair.var1_restr)
VARaic(crypto_pair.var1_restr)
VARbic(crypto_pair.var1)
VARaic(crypto_pair.var1)

AIC(crypto_pair.var1_restr, crypto_pair.var1)
BIC(crypto_pair.var1_restr, crypto_pair.var1)
# df       AIC
# crypto_pair.var1_restr 23 -8812.336
# crypto_pair.var1       26 -8806.954
# > BIC(crypto_pair.var1_restr, crypto_pair.var1)
# df       BIC
# crypto_pair.var1_restr 23 -8723.213
# crypto_pair.var1       26 -8706.205

# both prefer restricted model


# ---------------------------------------------------------------- VECM ----------------------------------------------------------------------------------
johan.test.trace <- ca.jo(crypto_pair[,1:2],         # data 
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

#--------------------------------------------------------------------------------------------------------------------
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

# based on the reparametrized model
# one can calculate and plot Impulse Response Functions
plot(irf(crypto_pair.vecm.1.asVAR, 
         n.ahead = 28))

# and variance decomposition
plot(fevd(crypto_pair.vecm.1.asVAR, 
          n.ahead = 28))

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
