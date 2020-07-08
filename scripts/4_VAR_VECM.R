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


# ------------------------------------------------- VAR MODEL ------------------------------------------------------------------------------
# selection without seasons
VARselect(crypto_pair[,1:2], 
          lag.max = 14
          )

# AIC(n)  HQ(n)  SC(n) FPE(n)  # 3
# 2      2      1      2 

# ------------------------------
# selection with weakly seasons
VARselect(crypto_pair[,1:2],
          lag.max = 14,     
          season = 7)     

# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 2      2      1      2 

# ------------------------------ 
# VAR model with 7 lags and seasons
crypto_pair.VAR.s <- VAR(crypto_pair[,1:2],
                         p = 2,
                         season = 7) 

summary(crypto_pair.VAR.s) ## you rarely interpet interpret ## ~44:00
# seasons are not significant

#--------------------
# ============================================ 
#   log_bitcoin = log_bitcoin.l1 + log_dogecoin.l1 + log_bitcoin.l2 + log_dogecoin.l2 + const + sd1 + sd2 + sd3 + sd4 + sd5 + sd6 
# 
# Estimate Std. Error t value Pr(>|t|)    
# log_bitcoin.l1   0.788461   0.080761   9.763  < 2e-16 ***
# log_dogecoin.l1  0.133845   0.094954   1.410  0.15955    
# log_bitcoin.l2   0.135580   0.079798   1.699  0.09020 .  
# log_dogecoin.l2 -0.075250   0.094041  -0.800  0.42415    
# const            1.041229   0.377498   2.758  0.00611 ** 
# sd1              0.013514   0.008668   1.559  0.11987    
# sd2              0.005757   0.008725   0.660  0.50982    
# sd3              0.013310   0.008674   1.534  0.12582    
# sd4              0.001965   0.008681   0.226  0.82101    
# sd5              0.014746   0.008665   1.702  0.08966 .  
# sd6              0.004566   0.008702   0.525  0.60015 

# log_dogecoin = log_bitcoin.l1 + log_dogecoin.l1 + log_bitcoin.l2 + log_dogecoin.l2 + const + sd1 + sd2 + sd3 + sd4 + sd5 + sd6 
# 
# Estimate Std. Error t value Pr(>|t|)    
# log_bitcoin.l1  -0.165469   0.067870  -2.438   0.0153 *  
# log_dogecoin.l1  0.981616   0.079797  12.301   <2e-16 ***
# log_bitcoin.l2   0.151317   0.067061   2.256   0.0247 *  
# log_dogecoin.l2  0.008418   0.079030   0.107   0.9152    
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
crypto_pair.VAR <- VAR(crypto_pair[,1:2], 
                        p = 2)

summary(crypto_pair.VAR)
# log_bitcoin = log_bitcoin.l1 + log_dogecoin.l1 + log_bitcoin.l2 + log_dogecoin.l2 + const 
# 
# Estimate Std. Error t value Pr(>|t|)    
# log_bitcoin.l1   0.77934    0.07993   9.750  < 2e-16 ***
# log_dogecoin.l1  0.13046    0.09373   1.392  0.16482    
# log_bitcoin.l2   0.14408    0.07895   1.825  0.06884 .  
# log_dogecoin.l2 -0.07137    0.09279  -0.769  0.44233    
# const            1.04993    0.37721   2.783  0.00566 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  
# log_dogecoin = log_bitcoin.l1 + log_dogecoin.l1 + log_bitcoin.l2 + log_dogecoin.l2 + const 
# 
# Estimate Std. Error t value Pr(>|t|)    
# log_bitcoin.l1  -0.176360   0.067228  -2.623  0.00908 ** 
# log_dogecoin.l1  0.985004   0.078834  12.495  < 2e-16 ***
# log_bitcoin.l2   0.162923   0.066403   2.454  0.01462 *  
# log_dogecoin.l2  0.004235   0.078043   0.054  0.95675    
# const            0.057055   0.317262   0.180  0.85738  
  

serial.test(crypto_pair.VAR) ## 57:00+
# data:  Residuals of VAR object crypto_pair.var2
# Chi-squared = 73.469, df = 56, p-value = 0.05864

serial.test(crypto_pair.VAR, type = "BG") # The null hypothesis is that there is no serial correlation of any order up to p.
# data:  Residuals of VAR object crypto_pair.var2
# Chi-squared = 18.8, df = 20, p-value = 0.5348


# lets do some basic diagnostics
plot(crypto_pair.VAR)
# no autocorrelation


# ------------------ restricted VAR 1
# automatic restriction, based on first model with seasonality
restrict(crypto_pair.VAR.s, method = "ser")
# Estimate Std. Error t value Pr(>|t|)    
# log_bitcoin.l1  -0.176360   0.067228  -2.623  0.00908 ** 
# log_dogecoin.l1  0.985004   0.078834  12.495  < 2e-16 ***
# log_bitcoin.l2   0.162923   0.066403   2.454  0.01462 *  
# log_dogecoin.l2  0.004235   0.078043   0.054  0.95675    
# const            0.057055   0.317262   0.180  0.85738   

# log_bitcoin = log_bitcoin.l1 + log_dogecoin.l1 + log_bitcoin.l2 + log_dogecoin.l2 + const 
# log_dogecoin = log_bitcoin.l1 + log_dogecoin.l1 + log_bitcoin.l2 + log_dogecoin.l2 + const
# restrict <- matrix(c(1, 1, 1, 1, 1, 1, 1,
#                      1, 0, 1, 0, 0, 1, 0),
#                    nrow=2, ncol=7, byrow=TRUE)


# The Granger causality results showed the bi-directional feedback on 5% level.
casuality <- readRDS("./data/gr_casual_dl_bitcoin_dl_dogecoin.RDS")
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

crypto_pair.VAR.restr1 <- restrict(crypto_pair.VAR, method = "man", resmat = restrict1)
summary(crypto_pair.VAR.restr1)
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

serial.test(crypto_pair.VAR.restr1, type = "BG") 
# data:  Residuals of VAR object crypto_pair.var2_restr1
# Chi-squared = 23.937, df = 20, p-value = 0.2451
serial.test(crypto_pair.VAR.restr1) 
# data:  Residuals of VAR object crypto_pair.var2_restr1
# Chi-squared = 80.553, df = 56, p-value = 0.01748

plot(crypto_pair.VAR.restr1)
# although the plot looks allright, the Portmanteau test for autocorrelation revealsproblem of autocorrelation of residuals 

# ------------------------------ 
crypto_pair.VAR.4 <- VAR(crypto_pair[,1:2],
                         p = 4) 
summary(crypto_pair.VAR.4)
# log_bitcoin = log_bitcoin.l1 + log_dogecoin.l1 + log_bitcoin.l2 + log_dogecoin.l2 + log_bitcoin.l3 + log_dogecoin.l3 + 
# log_bitcoin.l4 + log_dogecoin.l4 + const 
# Estimate Std. Error t value Pr(>|t|)    
# log_bitcoin.l1   0.767829   0.080944   9.486  < 2e-16 ***
# log_dogecoin.l1  0.161117   0.096249   1.674  0.09503 .  
# log_bitcoin.l2   0.236635   0.111459   2.123  0.03444 *
# const            1.128327   0.396847   2.843  0.00473 ** 
  
# log_dogecoin = log_bitcoin.l1 + log_dogecoin.l1 + log_bitcoin.l2 + log_dogecoin.l2 + log_bitcoin.l3 + log_dogecoin.l3 +
#   log_bitcoin.l4 + log_dogecoin.l4 + const 
# Estimate Std. Error t value Pr(>|t|)    
# log_bitcoin.l1  -0.18502    0.06782  -2.728  0.00669 ** 
#   log_dogecoin.l1  1.01137    0.08064  12.542  < 2e-16 ***
#   log_bitcoin.l2   0.27613    0.09338   2.957  0.00332 ** 

serial.test(crypto_pair.VAR.4, type = "BG") 

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
# The null hypothesis is: H_0: B_1 = … = B_h = 0 
 
var.models <- list(var.2.s=crypto_pair.VAR.s, var.2=crypto_pair.VAR, var.restricted=crypto_pair.VAR.restr1, var.4=crypto_pair.VAR.4)

var.BG_df <- c()

for(i in 1:length(var.models)){
  
  BG0 <- round(serial.test(var.models[[i]], type = "BG", lags.bg = 2)$serial$p.value, 3)
  BG1 <- round(serial.test(var.models[[i]], type = "BG", lags.bg = 7)$serial$p.value, 3)
  BG2 <- round(serial.test(var.models[[i]], type = "BG", lags.bg = 14)$serial$p.value, 3)
  BG3 <- round(serial.test(var.models[[i]], type = "BG", lags.bg = 21)$serial$p.value, 3)
  BG4 <- round(serial.test(var.models[[i]], type = "BG", lags.bg = 28)$serial$p.value, 3)
  
  var.BG_df <- rbind(var.BG_df, c(BG0,BG1,BG2,BG3,BG4))
}
var.BG_df <- as.data.frame(var.BG_df)
colnames(var.BG_df) <- c("B-G p-val 2d","B-G p-val 7d", "B-G p-val 14d", "B-G p-val 21d", "B-G p-val 28d")
rownames(var.BG_df) <- names(var.models)


plot_lags = 7
colors = c("black", "red", "blue", "dark green")
c1m1_acf <- acf(resid(var.models[[1]])[,1],
                lag.max = plot_lags,
                plot = FALSE,
                na.action = na.pass)   
c1m1_pacf <- pacf(resid(var.models[[1]])[,1], 
                  lag.max = plot_lags, 
                  plot = FALSE,
                  na.action = na.pass) 
c2m1_acf <- acf(resid(var.models[[1]])[,2],
                lag.max = plot_lags,
                plot = FALSE,
                na.action = na.pass)   
c2m1_pacf <- pacf(resid(var.models[[1]])[,2], 
                  lag.max = plot_lags, 
                  plot = FALSE,
                  na.action = na.pass)
c1m2_acf <- acf(resid(var.models[[2]])[,1],
                lag.max = plot_lags, 
                plot = FALSE,
                na.action = na.pass)
c1m2_pacf <- pacf(resid(var.models[[2]])[,1],
                  lag.max = plot_lags, 
                  plot = FALSE,
                  na.action = na.pass)
c2m2_acf <- acf(resid(var.models[[2]])[,2],
                lag.max = plot_lags, 
                plot = FALSE,
                na.action = na.pass)
c2m2_pacf <- pacf(resid(var.models[[2]])[,2],
                  lag.max = plot_lags, 
                  plot = FALSE,
                  na.action = na.pass)
c1m3_acf <- acf(resid(var.models[[3]])[,1],
                lag.max = plot_lags,
                plot = FALSE,
                na.action = na.pass)   
c1m3_pacf <- pacf(resid(var.models[[3]])[,1], 
                  lag.max = plot_lags, 
                  plot = FALSE,
                  na.action = na.pass) 
c2m3_acf <- acf(resid(var.models[[3]])[,2],
                lag.max = plot_lags,
                plot = FALSE,
                na.action = na.pass)  
c2m3_pacf <- pacf(resid(var.models[[3]])[,2], 
                  lag.max = plot_lags, 
                  plot = FALSE,
                  na.action = na.pass)
c1m4_acf <- acf(resid(var.models[[4]])[,1],
                lag.max = plot_lags,
                plot = FALSE,
                na.action = na.pass)   
c1m4_pacf <- pacf(resid(var.models[[4]])[,1], 
                  lag.max = plot_lags, 
                  plot = FALSE,
                  na.action = na.pass) 
c2m4_acf <- acf(resid(var.models[[4]])[,2],
                lag.max = plot_lags,
                plot = FALSE,
                na.action = na.pass)   
c2m4_pacf <- pacf(resid(var.models[[4]])[,2], 
                  lag.max = plot_lags, 
                  plot = FALSE,
                  na.action = na.pass) 

# names_ <- c("Bitcoin", "Dogecoin") 
par(mfrow = c(length(var.models), 4)) 
plot(c1m1_acf, 
     ylim = c(-0.5, 0.5),    
     lwd = 5,              
     col = colors[1],
     main = paste(names(var.models)[1], names_[1], "ACF"))
plot(c1m1_pacf, 
     ylim = c(-0.5, 0.5),    
     lwd = 5,              
     col = colors[1],
     main = paste(names(var.models)[1], names_[1], "PACF"))
plot(c2m1_acf, 
     ylim = c(-0.5, 0.5),    
     lwd = 5,              
     col = colors[1],
     main = paste(names(var.models)[1], names_[2],"ACF"))
plot(c2m1_pacf, 
     ylim = c(-0.5, 0.5),    
     lwd = 5,              
     col = colors[1],
     main = paste(names(var.models)[1], names_[2],"PACF"))
plot(c1m2_acf,
     ylim = c(-0.5, 0.5),
     lwd = 5,
     col = colors[2],
     main = paste(names(var.models)[2], names_[1],"ACF"))
plot(c1m2_pacf,
     ylim = c(-0.5, 0.5),
     lwd = 5,
     col = colors[2],
     main = paste(names(var.models)[2], names_[1],"PACF"))
plot(c2m2_acf,
     ylim = c(-0.5, 0.5),
     lwd = 5,
     col = colors[2],
     main = paste(names(var.models)[2], names_[2],"ACF"))
plot(c2m2_pacf,
     ylim = c(-0.5, 0.5),
     lwd = 5,
     col = colors[2],
     main = paste(names(var.models)[2], names_[2],"PACF"))
plot(c1m3_acf, 
     ylim = c(-0.5, 0.5),    
     lwd = 5,              
     col = colors[3],
     main = paste(names(var.models)[3], names_[1],"ACF"))
plot(c1m3_pacf, 
     ylim = c(-0.5, 0.5),    
     lwd = 5,              
     col = colors[3],
     main = paste(names(var.models)[3], names_[1],"PACF"))
plot(c2m3_acf, 
     ylim = c(-0.5, 0.5),    
     lwd = 5,              
     col = colors[3],
     main = paste(names(var.models)[3], names_[2],"ACF"))
plot(c2m3_pacf, 
     ylim = c(-0.5, 0.5),    
     lwd = 5,              
     col = colors[3],
     main = paste(names(var.models)[3], names_[2],"PACF"))
plot(c1m4_acf, 
     ylim = c(-0.5, 0.5),    
     lwd = 5,              
     col = colors[4],
     main = paste(names(var.models)[4], names_[1],"ACF"))
plot(c1m4_pacf, 
     ylim = c(-0.5, 0.5),    
     lwd = 5,              
     col = colors[4],
     main = paste(names(var.models)[4], names_[1],"PACF"))
plot(c2m4_acf, 
     ylim = c(-0.5, 0.5),    
     lwd = 5,              
     col = colors[4],
     main = paste(names(var.models)[4], names_[2],"ACF"))
plot(c2m4_pacf, 
     ylim = c(-0.5, 0.5),    
     lwd = 5,              
     col = colors[4],
     main = paste(names(var.models)[4], names_[2],"PACF"))
par(mfrow = c(1, 1)) 



coeftest(c1.auto.AIC)

AIC(crypto_pair.VAR.s, crypto_pair.VAR, crypto_pair.VAR.restr1)
BIC(crypto_pair.VAR.s, crypto_pair.VAR, crypto_pair.VAR.restr1)
# > AIC(crypto_pair.var2s, crypto_pair.var2, crypto_pair.var2_restr1)
# df       AIC
# crypto_pair.var2s       22 -2880.934
# crypto_pair.var2        10 -2889.614 <- 
# crypto_pair.var2_restr1  6 -2885.135
# > BIC(crypto_pair.var2s, crypto_pair.var2, crypto_pair.var2_restr1)
# df       BIC
# crypto_pair.var2s       22 -2795.257
# crypto_pair.var2        10 -2850.670
# crypto_pair.var2_restr1  6 -2861.768 <- 

# AIC prefere the most restricted model, BIC the leastwhich makes sense, however due to correlation in residuals

c1_var_acf <- acf(resid(crypto_pair.VAR)[,1],
                  lag.max = 10,
    plot = FALSE)   
c1_var_pacf <- pacf(resid(crypto_pair.VAR)[,1], 
     lag.max = 10, 
     plot = FALSE) 
c2_var_acf <- acf(resid(crypto_pair.VAR)[,2],
    lag.max = 10, 
    na.action = na.pass,
    plot = FALSE)   
c2_var_pacf <- pacf(resid(crypto_pair.VAR)[,2], 
     lag.max = 10, 
     na.action = na.pass,
     plot = FALSE) 

par(mfrow = c(2, 2)) 
plot(c1_var_acf, 
     main = "VAR bitcoing resid ACF",
     lag.max = 10, 
     ylim = c(-0.5, 0.5),    
     lwd = 5,              
     col = "dark green",
     na.action = na.pass)
plot(c2_var_acf,
     main = "VAR bitcoing resid PACF",
     lag.max = 10, 
     ylim = c(-0.5, 0.5),    
     lwd = 5,              
     col = "dark green",
     ann=FALSE,
     na.action = na.pass,)
plot(c1_var_pacf, 
     main = "VAR dogecoin resid ACF",
     lag.max = 10, 
     ylim = c(-0.5, 0.5),    
     lwd = 5,              
     col = "dark green",
     na.action = na.pass,)
plot(c2_var_pacf, 
     main = "VAR dogecoin resid PACF",
     lag.max = 10, 
     ylim = c(-0.5, 0.5),    
     lwd = 5,              
     col = "dark green",
     na.action = na.pass,)
par(mfrow = c(1, 1)) 



# --------------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------- VECM ----------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------------------------------------------

# johansen test
johan.test.trace <- ca.jo(crypto_pair[,1:2],         
                          ecdet = "const",
                          type = "trace",  
                          K = 2 # taken from VARselect
                          ) 

summary(johan.test.trace) 
#           test 10pct  5pct  1pct
# r <= 1 |  4.13  7.52  9.24 12.97
# r = 0  | 20.80 17.85 19.96 24.60
# rank 0 rejected, rank <=1 not rejected, so one cointegrationg vector

# https://www.researchgate.net/post/2_variables_and_2_cointegrating_equations_can_anyone_help_me2
johan.test.eigen <- ca.jo(crypto_pair[,1:2],        
                          ecdet = "const", 
                          type = "eigen",  
                          K = 2) 
summary(johan.test.eigen) 
#           test 10pct  5pct  1pct
# r <= 1 |  4.13  7.52  9.24 12.97
# r = 0  | 16.68 13.75 15.67 20.20
# results similar to previous test

get_pair_plot2(crypto_pair[,1:2], # dataframe or xts
                           # log_price = F, 
                           standardize = T,
                           ggplot = T
)
 
#-------------------------------------- model ------------------------------------------------------------------------------
crypto_pair.vecm.1 <- cajorls(johan.test.eigen, r = 1) 

summary(crypto_pair.vecm.1$rlm)
# lm(formula = log_bitcoin.d ~ ect1 + log_bitcoin.dl1 + log_dogecoin.dl1 - 
#      1, data = data.mat)
#                   Estimate Std. Error t value Pr(>|t|)   
# ect1             -0.06459    0.02392  -2.700  0.00725 **
#   log_bitcoin.dl1  -0.22099    0.07996  -2.764  0.00601 **
#   log_dogecoin.dl1  0.14323    0.09339   1.534  0.12599 

# lm(formula = log_dogecoin.d ~ ect1 + log_bitcoin.dl1 + log_dogecoin.dl1 - 
#      1, data = data.mat)
# Estimate Std. Error t value Pr(>|t|)   
# ect1             -2.894e-05  2.017e-02  -0.001  0.99886   
# log_bitcoin.dl1  -1.767e-01  6.742e-02  -2.621  0.00913 **
#   log_dogecoin.dl1 -7.213e-04  7.874e-02  -0.009  0.99270 

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


c1_vecmVAR_acf <- acf(resid(crypto_pair.vecm.1.asVAR)[,1],
                  lag.max = 10,
                  plot = FALSE)   
c1_vecmVAR_pacf <- pacf(resid(crypto_pair.vecm.1.asVAR)[,1], 
                    lag.max = 10, 
                    plot = FALSE) 
c2_vecmVAR_acf <- acf(resid(crypto_pair.vecm.1.asVAR)[,2],
                  lag.max = 10, 
                  na.action = na.pass,
                  plot = FALSE)   
c2_vecmVAR_pacf <- pacf(resid(crypto_pair.vecm.1.asVAR)[,2], 
                    lag.max = 10, 
                    na.action = na.pass,
                    plot = FALSE) 

par(mfrow = c(2, 2)) 
plot(c1_vecmVAR_acf, 
     main = "VECM bitcoing resid ACF",
     # lag.max = 10, 
     ylim = c(-0.5, 0.5),    
     lwd = 5,              
     col = "dark green"
     )
plot(c2_vecmVAR_acf,
     main = "VECM bitcoing resid PACF",
     # lag.max = 10, 
     ylim = c(-0.5, 0.5),    
     lwd = 5,              
     col = "dark green"
     )
plot(c1_vecmVAR_pacf, 
     main = "VECM dogecoin resid ACF",
     # lag.max = 10, 
     ylim = c(-0.5, 0.5),    
     lwd = 5,              
     col = "dark green"
     )
plot(c2_vecmVAR_pacf, 
     main = "VECM dogecoin resid PACF",
     # lag.max = 10, 
     ylim = c(-0.5, 0.5),    
     lwd = 5,              
     col = "dark green",
     )
par(mfrow = c(1, 1)) 







