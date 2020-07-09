library(xts)
library(forecast)
library(lmtest)
library(vars)

# laod function for data processing
source("functions/finding_stationary_pair.R")

# ---------------------------------------------------- PREPARE DATA ---------------------------------------------------------------------------

# load results of contegration checks
all_combinations_cointegr <- readRDS("./data/all_combinations_coint_2.RDS")
cointegr_tb_signf <- readRDS("./data/cointegr_tb_signf_5_7.RDS")

# get data with differences and log prices
crypto_pair_all <- getDifferencesXTS(coint_table = cointegr_tb_signf,                # table of cointefration results
                                 n_table = 6,                                    # which pair to prepare plots for
                                 n_obs_is = 365,                                 # how many observations in scope
                                 n_obs_ooc = 15,                                 # number of observations out of scope
                                 clipped = all_combinations_cointegr$pairs_data, # list with data after 
                                 # crypto_list = crypto_list,
                                 log_prices = TRUE)

# in sample data and out of sample data
crypto_pair <- crypto_pair_all$in_smpl
crypto_pair_oos <- crypto_pair_all$oo_smpl

# set samef used further
names_ <- c("Bitcoin", "Dogecoin")

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


# ------------------------------------------------------------------------------------------------- VAR(2) seasons
# VAR model with 7 lags and seasons
crypto_pair.VAR.2.s <- VAR(crypto_pair[,1:2],
                         p = 2,
                         season = 7) 
coeftest(crypto_pair.VAR.2.s) 
# Estimate Std. Error t value  Pr(>|t|)    
# log_bitcoin:(Intercept)       1.0412294  0.3774984  2.7582  0.006115 ** 
# log_bitcoin:log_bitcoin.l1    0.7884612  0.0807611  9.7629 < 2.2e-16 ***
# log_bitcoin:log_dogecoin.l1   0.1338445  0.0949541  1.4096  0.159549    
# log_bitcoin:log_bitcoin.l2    0.1355802  0.0797982  1.6990  0.090196 .  
# log_bitcoin:log_dogecoin.l2  -0.0752495  0.0940413 -0.8002  0.424149    
# log_bitcoin:sd1               0.0135140  0.0086678  1.5591  0.119873    
# log_bitcoin:sd2               0.0057569  0.0087254  0.6598  0.509822    
# log_bitcoin:sd3               0.0133102  0.0086742  1.5344  0.125818    
# log_bitcoin:sd4               0.0019654  0.0086808  0.2264  0.821014    
# log_bitcoin:sd5               0.0147465  0.0086647  1.7019  0.089657 .  
# log_bitcoin:sd6               0.0045658  0.0087024  0.5247  0.600146    
# log_dogecoin:(Intercept)      0.0682763  0.3172407  0.2152  0.829721    
# log_dogecoin:log_bitcoin.l1  -0.1654688  0.0678697 -2.4380  0.015261 *  
# log_dogecoin:log_dogecoin.l1  0.9816156  0.0797971 12.3014 < 2.2e-16 ***
# log_dogecoin:log_bitcoin.l2   0.1513172  0.0670605  2.2564  0.024657 *  
# log_dogecoin:log_dogecoin.l2  0.0084183  0.0790301  0.1065  0.915230    
# log_dogecoin:sd1              0.0010692  0.0072842  0.1468  0.883384    
# log_dogecoin:sd2             -0.0038166  0.0073326 -0.5205  0.603042    
# log_dogecoin:sd3              0.0013319  0.0072896  0.1827  0.855124    
# log_dogecoin:sd4             -0.0031196  0.0072951 -0.4276  0.669186    
# log_dogecoin:sd5              0.0124699  0.0072816  1.7125  0.087681 .  
# log_dogecoin:sd6              0.0013016  0.0073133  0.1780  0.858838   
# seasons are not significant

serial.test(crypto_pair.VAR.2.s, type = "BG") 
# p-value = 0.5112 <- NO AUTOCORRELATION

serial.test(crypto_pair.VAR.2.s) 
# p-value = 0.04946 <- AUTOCORRELATION!!!

plot(crypto_pair.VAR.2.s)

# ------------------------------------------------------------------------------------------------- VAR(2)
# model without seasonality component
crypto_pair.VAR.2 <- VAR(crypto_pair[,1:2], 
                        p = 2)

coeftest(crypto_pair.VAR.2)
#                                Estimate Std. Error t value  Pr(>|t|)    
# log_bitcoin:(Intercept)       1.0499325  0.3772064  2.7834  0.005664 ** 
# log_bitcoin:log_bitcoin.l1    0.7793363  0.0799301  9.7502 < 2.2e-16 ***
# log_bitcoin:log_dogecoin.l1   0.1304599  0.0937294  1.3919  0.164823    
# log_bitcoin:log_bitcoin.l2    0.1440768  0.0789488  1.8249  0.068844 .  
# log_bitcoin:log_dogecoin.l2  -0.0713651  0.0927883 -0.7691  0.442331    
# log_dogecoin:(Intercept)      0.0570554  0.3172618  0.1798  0.857382    
# log_dogecoin:log_bitcoin.l1  -0.1763602  0.0672279 -2.6233  0.009080 ** 
# log_dogecoin:log_dogecoin.l1  0.9850040  0.0788342 12.4946 < 2.2e-16 ***
# log_dogecoin:log_bitcoin.l2   0.1629226  0.0664025  2.4536  0.014621 *  
# log_dogecoin:log_dogecoin.l2  0.0042349  0.0780427  0.0543  0.956755   
# significance the same as in summary of the model

serial.test(crypto_pair.VAR.2, type = "BG") 
# p-value = 0.5348 <- NO AUTOCORRELATION

serial.test(crypto_pair.VAR.2) 
# p-value = 0.05864 <- NO AUTOCORRELATION at 5%, but Ho almost on edge


# lets do some basic diagnostics
plot(crypto_pair.VAR.2)
# no autocorrelation

# ------------------------------------------------------------------------------------------------- restricted VAR 1 

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


# restrict in accordance to Granger and crypto_pair.VAR.2 parameters significance
restrict1 <- matrix(c(1, 0, 1, 0, 1, # dep log_bitcoin 
                      1, 1, 1, 0, 0), # dep-log_dogecoin 
                   nrow=2, ncol=5, byrow=TRUE) # only GC

crypto_pair.VAR.restr1 <- restrict(crypto_pair.VAR, method = "man", resmat = restrict1)
summary(crypto_pair.VAR.restr1)
# log_bitcoin = log_bitcoin.l1 + log_bitcoin.l2 + const 
# 
#                  Estimate Std. Error t value Pr(>|t|)    
# log_bitcoin.l1  0.87421    0.05250  16.650   <2e-16 ***
# log_bitcoin.l2  0.09634    0.05251   1.835   0.0674 .  
# const           0.26794    0.12243   2.188   0.0293 *     
  
# log_dogecoin = log_bitcoin.l1 + log_dogecoin.l1 + log_bitcoin.l2 
# 
#                 Estimate Std. Error t value Pr(>|t|)    
# log_bitcoin.l1  -0.175493   0.043450  -4.039 6.57e-05 ***
# log_dogecoin.l1  0.985408   0.007259 135.749  < 2e-16 ***
# log_bitcoin.l2   0.165813   0.043804   3.785  0.00018 ***

coeftest(crypto_pair.VAR.restr1)
# Estimate Std. Error t value  Pr(>|t|)    
# log_bitcoin:(Intercept)       1.0499325  0.3772064  2.7834  0.005664 ** 
# log_bitcoin:log_bitcoin.l1    0.7793363  0.0799301  9.7502 < 2.2e-16 ***
# log_bitcoin:log_dogecoin.l1   0.1304599  0.0937294  1.3919  0.164823    
# log_bitcoin:log_bitcoin.l2    0.1440768  0.0789488  1.8249  0.068844 .  
# log_bitcoin:log_dogecoin.l2  -0.0713651  0.0927883 -0.7691  0.442331    
# log_dogecoin:(Intercept)      0.0570554  0.3172618  0.1798  0.857382    
# log_dogecoin:log_bitcoin.l1  -0.1763602  0.0672279 -2.6233  0.009080 ** 
# log_dogecoin:log_dogecoin.l1  0.9850040  0.0788342 12.4946 < 2.2e-16 ***
# log_dogecoin:log_bitcoin.l2   0.1629226  0.0664025  2.4536  0.014621 *  
# log_dogecoin:log_dogecoin.l2  0.0042349  0.0780427  0.0543  0.956755  

serial.test(crypto_pair.VAR.restr1, type = "BG") 
#p-value = 0.2451 <- NO AUTOCORRELATION

serial.test(crypto_pair.VAR.restr1) 
 # p-value = 0.01748 <- AUTOCORRELATION!!!

plot(crypto_pair.VAR.restr1)
# although the plot looks allright, the Portmanteau test for autocorrelation revealsproblem of autocorrelation of residuals. 

# ------------------------------------------------------------------------------------------------- VAR(4)
crypto_pair.VAR.4 <- VAR(crypto_pair[,1:2],
                         p = 4) 
coeftest(crypto_pair.VAR.4)
#                             Estimate Std. Error t value  Pr(>|t|)    
# log_bitcoin:(Intercept)       1.1283272  0.3968467  2.8432  0.004727 ** 
# log_bitcoin:log_bitcoin.l1    0.7678287  0.0809437  9.4860 < 2.2e-16 ***
# log_bitcoin:log_dogecoin.l1   0.1611169  0.0962487  1.6740  0.095026 .  
# log_bitcoin:log_bitcoin.l2    0.2366355  0.1114586  2.1231  0.034445 *  
# log_bitcoin:log_dogecoin.l2  -0.1147370  0.1305521 -0.8789  0.380077    
# log_bitcoin:log_bitcoin.l3   -0.0939306  0.1123318 -0.8362  0.403616    
# log_bitcoin:log_dogecoin.l3  -0.0197442  0.1282511 -0.1539  0.877738    
# log_bitcoin:log_bitcoin.l4    0.0073233  0.0802469  0.0913  0.927338    
# log_bitcoin:log_dogecoin.l4   0.0371224  0.0934796  0.3971  0.691521    
# log_dogecoin:(Intercept)      0.1041522  0.3324808  0.3133  0.754270    
# log_dogecoin:log_bitcoin.l1  -0.1850195  0.0678151 -2.7283  0.006686 ** 
# log_dogecoin:log_dogecoin.l1  1.0113690  0.0806378 12.5421 < 2.2e-16 ***
# log_dogecoin:log_bitcoin.l2   0.2761336  0.0933808  2.9571  0.003315 ** 
# log_dogecoin:log_dogecoin.l2 -0.0372705  0.1093774 -0.3408  0.733494    
# log_dogecoin:log_bitcoin.l3  -0.0435566  0.0941124 -0.4628  0.643783    
# log_dogecoin:log_dogecoin.l3 -0.0639158  0.1074496 -0.5948  0.552329    
# log_dogecoin:log_bitcoin.l4  -0.0654557  0.0672314 -0.9736  0.330930    
# log_dogecoin:log_dogecoin.l4  0.0801891  0.0783179  1.0239  0.306589   
# significance the same as in summary of the model

serial.test(crypto_pair.VAR.4, type = "BG") 
# p-value = 0.4784 <- no autocorrelation


# ---------------------------------------------------------------------------------------------------------------------------------------------

# list of VAR model
var.models <- list(var.2.s=crypto_pair.VAR.2.s, var.2=crypto_pair.VAR.2, var.restricted=crypto_pair.VAR.restr1, var.4=crypto_pair.VAR.4)

# Breusch-Godfrey test for autocorrelation of residuals up to 2,7,14 and 18 days, The null hypothesis is: H_0: B_1 = … = B_h = 0 
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

#                 B-G p-val 2d B-G p-val 7d B-G p-val 14d B-G p-val 21d B-G p-val 28d
# var.2.s               0.465        0.395         0.064         0.379         0.699
# var.2                 0.428        0.466         0.063         0.383         0.721
# var.restricted        0.376        0.298         0.029         0.237         0.541
# var.4                 0.424        0.475         0.125         0.487         0.787

# get ACF and PACF plot of models
if(1){
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
  
}

# comparison of information criteria for models
AIC(crypto_pair.VAR.2.s, crypto_pair.VAR.2, crypto_pair.VAR.restr1, crypto_pair.VAR.4)
BIC(crypto_pair.VAR.2.s, crypto_pair.VAR.2, crypto_pair.VAR.restr1, crypto_pair.VAR.4)

# df       AIC
# crypto_pair.VAR.s      22 -2880.934
# crypto_pair.VAR        10 -2889.614 <- 
# crypto_pair.VAR.restr1  6 -2885.135
# crypto_pair.VAR.4      18 -2862.513

# df       BIC
# crypto_pair.VAR.s      22 -2795.257
# crypto_pair.VAR        10 -2850.670
# crypto_pair.VAR.restr1  6 -2861.768 <-  
# crypto_pair.VAR.4      18 -2792.513


# final model coefficients significance
coeftest(crypto_pair.VAR.2)
# Estimate Std. Error t value  Pr(>|t|)    
# log_bitcoin:(Intercept)       1.0499325  0.3772064  2.7834  0.005664 ** 
# log_bitcoin:log_bitcoin.l1    0.7793363  0.0799301  9.7502 < 2.2e-16 ***
# log_bitcoin:log_dogecoin.l1   0.1304599  0.0937294  1.3919  0.164823    
# log_bitcoin:log_bitcoin.l2    0.1440768  0.0789488  1.8249  0.068844 .  
# log_bitcoin:log_dogecoin.l2  -0.0713651  0.0927883 -0.7691  0.442331    
# log_dogecoin:(Intercept)      0.0570554  0.3172618  0.1798  0.857382    
# log_dogecoin:log_bitcoin.l1  -0.1763602  0.0672279 -2.6233  0.009080 ** 
# log_dogecoin:log_dogecoin.l1  0.9850040  0.0788342 12.4946 < 2.2e-16 ***
# log_dogecoin:log_bitcoin.l2   0.1629226  0.0664025  2.4536  0.014621 *  
# log_dogecoin:log_dogecoin.l2  0.0042349  0.0780427  0.0543  0.956755    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# one can calculate and plot Impulse Response Functions
irf_var <- irf(crypto_pair.VAR.2, 
         n.ahead = 28)
plot(irf_var)

# and variance decomposition
fevd_var <- fevd(crypto_pair.VAR.2, 
                 n.ahead = 28)
plot(fevd_var)

# saveRDS(crypto_pair.VAR.2, "./data/crypto_pair_VAR2.rds")

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
# lm(formula = log_bitcoin.d ~ ect1 + log_bitcoin.dl1 + log_dogecoin.dl1 - 1, data = data.mat)
#                   Estimate Std. Error t value Pr(>|t|)   
# ect1             -0.06459    0.02392  -2.700  0.00725 **
# log_bitcoin.dl1  -0.22099    0.07996  -2.764  0.00601 **
# log_dogecoin.dl1  0.14323    0.09339   1.534  0.12599 

# lm(formula = log_dogecoin.d ~ ect1 + log_bitcoin.dl1 + log_dogecoin.dl1 - 1, data = data.mat)
# Estimate Std. Error t value Pr(>|t|)   
# ect1             -2.894e-05  2.017e-02  -0.001  0.99886   
# log_bitcoin.dl1  -1.767e-01  6.742e-02  -2.621  0.00913 **
# log_dogecoin.dl1 -7.213e-04  7.874e-02  -0.009  0.99270 

# estimates of the VECM model
crypto_pair.vecm.1$beta
#                     ect1
# log_bitcoin.l2    1.00000
# log_dogecoin.l2  -1.06445
# constant        -15.47143
# in a long run log_bitcoin 106 percentage of log dogecoin

#-------------------------------------- model ------------------------------------------------------------------------------
# we can reparametrize the VEC model into VAR
crypto_pair.vecm.1.asVAR <- vec2var(johan.test.eigen, r = 1)
# lets see the result
crypto_pair.vecm.1.asVAR

# Impulse Response Functions
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

normality.test(crypto_pair.vecm.1.asVAR)
# p-value < 2.2e-16
# H0: data are from a normal distribution
# data is not from normal distribution


# check serial correlation for given model
vecm.BG_df <- c()
if(1){
  BG0 <- round(serial.test(crypto_pair.vecm.1.asVAR, type = "BG", lags.bg = 2)$serial$p.value, 3)
  BG1 <- round(serial.test(crypto_pair.vecm.1.asVAR, type = "BG", lags.bg = 7)$serial$p.value, 3)
  BG2 <- round(serial.test(crypto_pair.vecm.1.asVAR, type = "BG", lags.bg = 14)$serial$p.value, 3)
  BG3 <- round(serial.test(crypto_pair.vecm.1.asVAR, type = "BG", lags.bg = 21)$serial$p.value, 3)
  BG4 <- round(serial.test(crypto_pair.vecm.1.asVAR, type = "BG", lags.bg = 28)$serial$p.value, 3)
  
  vecm.BG_df <- rbind(vecm.BG_df, c(BG0,BG1,BG2,BG3,BG4))
}
vecm.BG_df <- as.data.frame(vecm.BG_df)
colnames(vecm.BG_df) <- c("B-G p-val 2d","B-G p-val 7d", "B-G p-val 14d", "B-G p-val 21d", "B-G p-val 28d")
rownames(vecm.BG_df) <- "VECM2VAR"


if(1){
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
       main = paste("crypto_pair.vecm.1.asVAR", names_[1], "ACF"),
       ylim = c(-0.5, 0.5),    
       lwd = 5,              
       col = colors[1])
  plot(c1_vecmVAR_pacf, 
       main = paste("crypto_pair.vecm.1.asVAR", names_[2], "ACF"),
       ylim = c(-0.5, 0.5),    
       lwd = 5,              
       col = colors[1])
  plot(c2_vecmVAR_acf,
       main = paste("crypto_pair.vecm.1.asVAR", names_[1], "PACF"),
       ylim = c(-0.5, 0.5),    
       lwd = 5,              
       col = colors[2])
  plot(c2_vecmVAR_pacf, 
       main = paste("crypto_pair.vecm.1.asVAR", names_[2], "PACF"),
       ylim = c(-0.5, 0.5),    
       lwd = 5,              
       col = colors[2])
  par(mfrow = c(1, 1))
}




# saveRDS(crypto_pair.vecm.1.asVAR, "./data/crypto_pair_vecm1asVAR.rds")


