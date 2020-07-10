library(xts)
library(forecast)
library(lmtest)

# ---------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------

# laod function for data processing
source("functions/finding_stationary_pair.R")

# ---------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------
# PREPARE DATA

# load results of contegration checks
all_combinations_cointegr <- readRDS("./data/all_combinations_coint_2.RDS")
cointegr_tb_signf <- readRDS("./data/cointegr_tb_signf_5_7.RDS")

# get data with differences and log prices
crypto_pair_all <- getDifferencesXTS(coint_table = cointegr_tb_signf,                # table of cointefration results
                                     n_table = 6,                                    # which pair to prepare plots for
                                     n_obs_is = 365,                                 # how many observations in scope
                                     n_obs_ooc = 15,                                 # number of observations out of scope
                                     clipped = all_combinations_cointegr$pairs_data, # list with data after 
                                     log_prices = TRUE)

# in sample data and out of sample data
crypto_pair <- crypto_pair_all$in_smpl
crypto_pair_oos <- crypto_pair_all$oo_smpl

# set samef used further
names_ <- c("Bitcoin", "Dogecoin")

# ---------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------
# BITCOIN

# ---------------------------------------------------------------------------------------------------------------------------
#  C1 ARIMA 1

c1.arima.4.1.4 <- Arima(crypto_pair[,1],
                     order = c(4, 1, 4),
                     method = "CSS-ML",
                     optim.control = list(maxit = 1200),
                     optim.method = "L-BFGS-B",
                     include.mean = TRUE
                     )

coeftest(c1.arima.4.1.4)
# z test of coefficients:
#     Estimate Std. Error z value  Pr(>|z|)    
# ar1 -0.159247   1.376471 -0.1157  0.907896    
# ar2  0.046191   0.426256  0.1084  0.913707    
# ar3 -0.758755   0.148081 -5.1239 2.992e-07 ***
# ar4 -0.354516   1.032574 -0.3433  0.731348    
# ma1  0.047291   1.370163  0.0345  0.972467    
# ma2  0.017040   0.594976  0.0286  0.977152    
# ma3  0.771053   0.292611  2.6351  0.008412 ** 
# ma4  0.363401   0.981768  0.3701  0.711271    

par(mfrow = c(2, 1)) 
acf(resid(c1.arima.4.1.4),
    lag.max = 14, 
    ylim = c(-0.5, 0.5),    
    lwd = 5,              
    col = "dark green",
    na.action = na.pass)   
pacf(resid(c1.arima.4.1.4), 
     lag.max = 14, 
     ylim = c(-0.5, 0.5),
     lwd = 5, col = "dark green",
     na.action = na.pass) 
par(mfrow = c(1, 1)) 
# no autocorrelation up to 14 lags


Box.test(resid(c1.arima.4.1.4), type = "Ljung-Box", lag =  4)
Box.test(resid(c1.arima.4.1.4), type = "Ljung-Box", lag =  7)
Box.test(resid(c1.arima.4.1.4), type = "Ljung-Box", lag = 14)
Box.test(resid(c1.arima.4.1.4), type = "Ljung-Box", lag = 21)
Box.test(resid(c1.arima.4.1.4), type = "Ljung-Box", lag = 28)

# ---------------------------------------------------------------------------------------------------------------------------
#  C1 - ARIMA 2

c1.arima.7.1.7 <- Arima(crypto_pair[,1],
                        order = c(7, 1, 7),
                        method = "CSS-ML",
                        optim.control = list(maxit = 5000),
                        optim.method = "Nelder-Mead"
                        )

coeftest(c1.arima.7.1.7)
# Estimate Std. Error z value  Pr(>|z|)    
# ar1  0.116002   0.908729  0.1277  0.898423    
# ar2  0.396872   1.131256  0.3508  0.725720    
# ar3  0.252739   0.287337  0.8796  0.379082    
# ar4 -0.254749   0.226323 -1.1256  0.260335    
# ar5 -0.611998         NA      NA        NA    
# ar6  0.219639   0.880077  0.2496  0.802921    
# ar7 -0.130602   0.596465 -0.2190  0.826681    
# ma1 -0.208076   0.862514 -0.2412  0.809366    
# ma2 -0.385198   0.987556 -0.3901  0.696498    
# ma3 -0.282586   0.303827 -0.9301  0.352325    
# ma4  0.440444   0.103957  4.2368 2.267e-05 ***
# ma5  0.614928   0.201267  3.0553  0.002248 ** 
# ma6 -0.365628   0.971437 -0.3764  0.706636    
# ma7  0.067626   0.517825  0.1306  0.896094 

Box.test(resid(c1.arima.7.1.7), type = "Ljung-Box", lag =  4)
Box.test(resid(c1.arima.7.1.7), type = "Ljung-Box", lag =  7)
Box.test(resid(c1.arima.7.1.7), type = "Ljung-Box", lag = 14)
Box.test(resid(c1.arima.7.1.7), type = "Ljung-Box", lag = 21)
Box.test(resid(c1.arima.7.1.7), type = "Ljung-Box", lag = 28)


# ---------------------------------------------------------------------------------------------------------------------------
#   C1 - ARIMA 3

c1.auto.AIC <- auto.arima(crypto_pair[,1],
                          d = 1,             # parameter d of ARIMA model
                          D = 1,
                          max.p = 7,         # Maximum value of p
                          max.q = 7,         # Maximum value of q
                          max.P = 14,
                          max.Q = 14,
                          max.order = 42,    # maximum p+q
                          start.p = 1,       # Starting value of p in stepwise procedure
                          start.q = 1,       # Starting value of q in stepwise procedure
                          start.P = 1,       # Starting value of p in stepwise procedure
                          start.Q = 1,       # Starting value of q in stepwise procedure
                          ic = "aic",        # Information criterion to be used in model selection.
                          stepwise = FALSE,  # if FALSE considers all models
                          allowdrift = TRUE, # include a constant
                          trace = TRUE # show summary of all models considered
)      
# Best model: ARIMA(1,1,1)    

par(mfrow = c(2, 1)) 
acf(resid(c1.auto.AIC),
    lag.max = 10, 
    ylim = c(-0.5, 0.5),    
    lwd = 5,              
    col = "dark green",
    na.action = na.pass)   
pacf(resid(c1.auto.AIC), 
     lag.max = 10, 
     ylim = c(-0.5, 0.5),
     lwd = 5, col = "dark green",
     na.action = na.pass) 
par(mfrow = c(1, 1)) 

# hypothesis of randomness, there is no autocorrelation
Box.test(resid(c1.auto.AIC), type = "Ljung-Box", lag =  4)
Box.test(resid(c1.auto.AIC), type = "Ljung-Box", lag =  7)
Box.test(resid(c1.auto.AIC), type = "Ljung-Box", lag = 14)
Box.test(resid(c1.auto.AIC), type = "Ljung-Box", lag = 21)
Box.test(resid(c1.auto.AIC), type = "Ljung-Box", lag = 28)


# ---------------------------------------------------------------------------------------------------------------------------
# C1 - ARIMA 4

c1.auto.BIC <- auto.arima(crypto_pair[,1],
                          d = 1,             # parameter d of ARIMA model
                          D = 1,
                          max.p = 7,         # Maximum value of p
                          max.q = 7,         # Maximum value of q
                          max.P = 14,
                          max.Q = 14,
                          max.order = 42,    # maximum p+q
                          start.p = 1,       # Starting value of p in stepwise procedure
                          start.q = 1,       # Starting value of q in stepwise procedure
                          start.P = 1,       # Starting value of p in stepwise procedure
                          start.Q = 1,       # Starting value of q in stepwise procedure
                          ic = "bic",        # Information criterion to be used in model selection.
                          stepwise = FALSE,  # if FALSE considers all models
                          allowdrift = TRUE, # include a constant
                          trace = TRUE # show summary of all models considered
)
# Best model: ARIMA(0,1,1)  
summary(c1.auto.BIC)

par(mfrow = c(2, 1)) 
acf(resid(c1.auto.BIC),
    lag.max = 12, 
    ylim = c(-0.5, 0.5),    
    lwd = 5,              
    col = "dark green",
    na.action = na.pass)   
pacf(resid(c1.auto.BIC), 
     lag.max = 12, 
     ylim = c(-0.5, 0.5),
     lwd = 5, col = "dark green",
     na.action = na.pass) 
par(mfrow = c(1, 1)) 
# autocorrelation still a problem

Box.test(resid(c1.auto.BIC), type = "Ljung-Box", lag =  4)
Box.test(resid(c1.auto.BIC), type = "Ljung-Box", lag =  7)
Box.test(resid(c1.auto.BIC), type = "Ljung-Box", lag = 14)
Box.test(resid(c1.auto.BIC), type = "Ljung-Box", lag = 21)
Box.test(resid(c1.auto.BIC), type = "Ljung-Box", lag = 28)
# there is autocorrelation up to 7 days, almost up to 14 days

# ---------------------------------------------------------------------------------------------------------------------------
# models overview

c1.arima.models <- list(c1.arima.4.1.4=c1.arima.4.1.4, 
                        c1.arima.7.1.7=c1.arima.7.1.7, 
                        c1.auto.AIC=c1.auto.AIC, 
                        c1.auto.BIC=c1.auto.BIC)

c1.Box_Ljung_df <- c()

for(i in 1:4){
  LB0 <- round(Box.test(resid(c1.arima.models[[i]]), type = "Ljung-Box", lag =  4)$p.value, 3)
  LB1 <- round(Box.test(resid(c1.arima.models[[i]]), type = "Ljung-Box", lag =  7)$p.value, 3)
  LB2 <- round(Box.test(resid(c1.arima.models[[i]]), type = "Ljung-Box", lag = 14)$p.value, 3)
  LB3 <- round(Box.test(resid(c1.arima.models[[i]]), type = "Ljung-Box", lag = 21)$p.value, 3)
  LB4 <- round(Box.test(resid(c1.arima.models[[i]]), type = "Ljung-Box", lag = 28)$p.value, 3)
  c1.Box_Ljung_df <- rbind(c1.Box_Ljung_df, c(LB0,LB1,LB2,LB3,LB4))
}
c1.Box_Ljung_df <- as.data.frame(c1.Box_Ljung_df)
colnames(c1.Box_Ljung_df) <- c("L-B p-val 4d","L-B p-val 7d", "L-B p-val 14d", "L-B p-val 21d", "L-B p-val 28d")
rownames(c1.Box_Ljung_df) <- names(c1.arima.models)


if(1){
  plot_lags = 10
  colors = c("black", "red", "blue", "dark green")
  c1m1_acf <- acf(resid(c1.arima.models[[1]]),
                  lag.max = plot_lags,
                  plot = FALSE,
                  na.action = na.pass)   
  c1m1_pacf <- pacf(resid(c1.arima.models[[1]]), 
                    lag.max = plot_lags, 
                    plot = FALSE,
                    na.action = na.pass) 
  c1m2_acf <- acf(resid(c1.arima.models[[2]]),
                  lag.max = plot_lags, 
                  plot = FALSE,
                  na.action = na.pass)
  c1m2_pacf <- pacf(resid(c1.arima.models[[2]]),
                    lag.max = plot_lags, 
                    plot = FALSE,
                    na.action = na.pass)
  c1m3_acf <- acf(resid(c1.arima.models[[3]]),
                  lag.max = plot_lags,
                  plot = FALSE,
                  na.action = na.pass)   
  c1m3_pacf <- pacf(resid(c1.arima.models[[3]]), 
                    lag.max = plot_lags, 
                    plot = FALSE,
                    na.action = na.pass) 
  c1m4_acf <- acf(resid(c1.arima.models[[4]]),
                  lag.max = plot_lags, 
                  plot = FALSE,
                  na.action = na.pass)
  c1m4_pacf <- pacf(resid(c1.arima.models[[4]]),
                    lag.max = plot_lags, 
                    plot = FALSE,
                    na.action = na.pass)
  
  par(mfrow = c(4, 2)) 
  plot(c1m1_acf, 
       ylim = c(-0.5, 0.5),    
       lwd = 5,              
       col = colors[1],
       main = paste(names(c1.arima.models)[1], "ACF"))
  plot(c1m1_pacf, 
       ylim = c(-0.5, 0.5),    
       lwd = 5,              
       col = colors[1],
       main = paste(names(c1.arima.models)[1], "PACF"))
  plot(c1m2_acf,
       ylim = c(-0.5, 0.5),
       lwd = 5,
       col = colors[2],
       main = paste(names(c1.arima.models)[2], "ACF"))
  plot(c1m2_pacf,
       ylim = c(-0.5, 0.5),
       lwd = 5,
       col = colors[2],
       main = paste(names(c1.arima.models)[2], "PACF"))
  plot(c1m3_acf, 
       ylim = c(-0.5, 0.5),    
       lwd = 5,              
       col = colors[3],
       main = paste(names(c1.arima.models)[3], "ACF"))
  plot(c1m3_pacf, 
       ylim = c(-0.5, 0.5),    
       lwd = 5,              
       col = colors[3],
       main = paste(names(c1.arima.models)[3], "PACF"))
  plot(c1m4_acf,
       ylim = c(-0.5, 0.5),
       lwd = 5,
       col = colors[4],
       main = paste(names(c1.arima.models)[4], "ACF"))
  plot(c1m4_pacf,
       ylim = c(-0.5, 0.5),
       lwd = 5,
       col = colors[4],
       main = paste(names(c1.arima.models)[4], "PACF"))
  par(mfrow = c(1, 1)) 
}


AIC(c1.arima.4.1.4, c1.arima.7.1.7, c1.auto.AIC, c1.auto.BIC)
# df       AIC
# c1.arima.4.1.4  9 -1223.976
# c1.arima.7.1.7 15 -1217.149
# c1.auto.AIC     3 -1228.827 <- the best
# c1.auto.BIC     1 -1225.245
BIC(c1.arima.4.1.4, c1.arima.7.1.7, c1.auto.AIC, c1.auto.BIC)
# df       BIC
# c1.arima.4.1.4  9 -1188.902
# c1.arima.7.1.7 15 -1158.692
# c1.auto.AIC     3 -1217.136
# c1.auto.BIC     1 -1221.348 <- the best

coeftest(c1.auto.AIC)
# Estimate Std. Error z value  Pr(>|z|)    
# ar1 -0.78829    0.15365 -5.1306 2.888e-07 ***
# ma1  0.69842    0.17832  3.9167 8.976e-05 ***


# save model
saveRDS(c1.auto.AIC, "./data/c1.arima111.rds")


# ---------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------
# DOGECOIN

# ---------------------------------------------------------------------------------------------------------------------------
#  C2 - ARIMA 1

c2.arima.4.1.4 <- Arima(crypto_pair[,2],
                        order = c(4, 1, 4),
                        method = "CSS-ML",
                        optim.control = list(maxit = 1200),
                        optim.method = "L-BFGS-B",
)

coeftest(c2.arima.4.1.4)
#     Estimate Std. Error z value  Pr(>|z|)    
# ar1  1.163539   0.380942  3.0544 0.0022553 ** 
# ar2  0.232257   0.398564  0.5827 0.5600727    
# ar3 -0.997796   0.210271 -4.7453 2.082e-06 ***
# ar4  0.290874   0.277646  1.0476 0.2948021    
# ma1 -1.331229   0.370338 -3.5946 0.0003248 ***
# ma2  0.030766   0.447703  0.0687 0.9452129    
# ma3  0.941354   0.232510  4.0487 5.151e-05 ***
# ma4 -0.374865   0.227537 -1.6475 0.0994585 .  

  
par(mfrow = c(2, 1)) 
acf(resid(c2.arima.4.1.4),
    lag.max = 14, 
    ylim = c(-0.5, 0.5),    
    lwd = 5,              
    col = "dark green",
    na.action = na.pass)   
pacf(resid(c2.arima.4.1.4), 
     lag.max = 14, 
     ylim = c(-0.5, 0.5),
     lwd = 5, col = "dark green",
     na.action = na.pass) 
par(mfrow = c(1, 1)) 
# no autocorrelation up to 14 lags, around 10th lag is close

# Box-Ljung test
Box.test(resid(c2.arima.4.1.4), type = "Ljung-Box", lag =  4)
Box.test(resid(c2.arima.4.1.4), type = "Ljung-Box", lag =  7)
Box.test(resid(c2.arima.4.1.4), type = "Ljung-Box", lag = 14)
Box.test(resid(c2.arima.4.1.4), type = "Ljung-Box", lag = 21)
Box.test(resid(c2.arima.4.1.4), type = "Ljung-Box", lag = 28)

# ---------------------------------------------------------------------------------------------------------------------------
#  C2 - ARIMA 2 

c2.arima.7.1.7 <- Arima(crypto_pair[,2],
                        order = c(7, 1, 7),
                        method = "CSS-ML",
                        optim.control = list(maxit = 2500),
                        optim.method = "L-BFGS-B"
)

coeftest(c2.arima.7.1.7)
# z test of coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# ar1  0.295036         NA      NA       NA    
# ar2  0.284172   0.374979  0.7578  0.44855    
# ar3  0.213500         NA      NA       NA    
# ar4 -0.447693         NA      NA       NA    
# ar5 -0.497539   0.204011 -2.4388  0.01474 *  
# ar6  0.278029         NA      NA       NA    
# ar7 -0.169363   0.266208 -0.6362  0.52464    
# ma1 -0.434558         NA      NA       NA    
# ma2 -0.170179   0.383752 -0.4435  0.65743    
# ma3 -0.262637   0.029853 -8.7978  < 2e-16 ***
# ma4  0.640103         NA      NA       NA    
# ma5  0.366586   0.215851  1.6983  0.08945 .  
# ma6 -0.395326         NA      NA       NA    
# ma7  0.201267   0.221831  0.9073  0.36425    


Box.test(resid(c1.arima.7.1.7), type = "Ljung-Box", lag =  4)
Box.test(resid(c1.arima.7.1.7), type = "Ljung-Box", lag =  7)
Box.test(resid(c1.arima.7.1.7), type = "Ljung-Box", lag = 14)
Box.test(resid(c1.arima.7.1.7), type = "Ljung-Box", lag = 21)
Box.test(resid(c1.arima.7.1.7), type = "Ljung-Box", lag = 28)

# ---------------------------------------------------------------------------------------------------------------------------
# C2 - ARIMA 3 

c2.auto.AIC <- auto.arima(crypto_pair[,2],
                          d = 1,             # parameter d of ARIMA model
                          D = 1,
                          max.p = 7,         # Maximum value of p
                          max.q = 7,         # Maximum value of q
                          max.P = 14,
                          max.Q = 14,
                          max.order = 42,    # maximum p+q
                          start.p = 1,       # Starting value of p in stepwise procedure
                          start.q = 1,       # Starting value of q in stepwise procedure
                          start.P = 1,       # Starting value of p in stepwise procedure
                          start.Q = 1,       # Starting value of q in stepwise procedure
                          ic = "aic",        # Information criterion to be used in model selection.
                          stepwise = FALSE,  # if FALSE considers all models
                          allowdrift = TRUE, # include a constant
                          trace = TRUE # show summary of all models considered
)
# Best model: ARIMA(1,1,0)   3

par(mfrow = c(2, 1)) 
acf(resid(c2.auto.AIC),
    lag.max = 10, 
    ylim = c(-0.5, 0.5),    
    lwd = 5,              
    col = "dark green",
    na.action = na.pass)   
pacf(resid(c2.auto.AIC), 
     lag.max = 10, 
     ylim = c(-0.5, 0.5),
     lwd = 5, col = "dark green",
     na.action = na.pass) 
par(mfrow = c(1, 1)) 

# Box-Ljung test
# hypothesis of randomness, there is no autocorrelation
Box.test(resid(c2.auto.AIC), type = "Ljung-Box", lag =  4)
Box.test(resid(c2.auto.AIC), type = "Ljung-Box", lag =  7)
Box.test(resid(c2.auto.AIC), type = "Ljung-Box", lag = 14)
Box.test(resid(c2.auto.AIC), type = "Ljung-Box", lag = 21)
Box.test(resid(c2.auto.AIC), type = "Ljung-Box", lag = 28)


# ---------------------------------------------------------------------------------------------------------------------------
# C2 - ARIMA 4 

c2.auto.BIC <- auto.arima(crypto_pair[,2],
                          d = 1,             # parameter d of ARIMA model
                          D = 1,
                          max.p = 7,         # Maximum value of p
                          max.q = 7,         # Maximum value of q
                          max.P = 14,
                          max.Q = 14,
                          max.order = 42,    # maximum p+q
                          start.p = 1,       # Starting value of p in stepwise procedure
                          start.q = 1,       # Starting value of q in stepwise procedure
                          start.P = 1,       # Starting value of p in stepwise procedure
                          start.Q = 1,       # Starting value of q in stepwise procedure
                          ic = "bic",        # Information criterion to be used in model selection.
                          stepwise = FALSE,  # if FALSE considers all models
                          allowdrift = TRUE, # include a constant
                          trace = TRUE # show summary of all models considered
)
# Best model: ARIMA(1,1,0) 3

par(mfrow = c(2, 1)) 
acf(resid(c2.auto.BIC),
    lag.max = 12, 
    ylim = c(-0.5, 0.5),    
    lwd = 5,              
    col = "dark green",
    na.action = na.pass)   
pacf(resid(c2.auto.BIC), 
     lag.max = 12, 
     ylim = c(-0.5, 0.5),
     lwd = 5, col = "dark green",
     na.action = na.pass) 
par(mfrow = c(1, 1)) 

# hypothesis of randomness, there is no autocorrelation
Box.test(resid(c2.auto.BIC), type = "Ljung-Box", lag =  4)
Box.test(resid(c2.auto.BIC), type = "Ljung-Box", lag =  7)
Box.test(resid(c2.auto.BIC), type = "Ljung-Box", lag = 14)
Box.test(resid(c2.auto.BIC), type = "Ljung-Box", lag = 21)
Box.test(resid(c2.auto.BIC), type = "Ljung-Box", lag = 28)
# THERE IS AN AUTOCORRELATION


# ---------------------------------------------------------------------------------------------------------------------------
#  models overview

c2.arima.models <- list(c2.arima.4.1.4=c2.arima.4.1.4,
                        c2.arima.7.1.7=c2.arima.7.1.7, 
                        # c2.auto.AIC=c2.auto.AIC, 
                        c2.auto.BIC=c2.auto.BIC
                        )
names(c2.arima.models)

c2.Box_Ljung_df <- c()

for(i in 1:length(c2.arima.models)){
  LB0 <- round(Box.test(resid(c2.arima.models[[i]]), type = "Ljung-Box", lag =  4)$p.value, 3)
  LB1 <- round(Box.test(resid(c2.arima.models[[i]]), type = "Ljung-Box", lag =  7)$p.value, 3)
  LB2 <- round(Box.test(resid(c2.arima.models[[i]]), type = "Ljung-Box", lag = 14)$p.value, 3)
  LB3 <- round(Box.test(resid(c2.arima.models[[i]]), type = "Ljung-Box", lag = 21)$p.value, 3)
  LB4 <- round(Box.test(resid(c2.arima.models[[i]]), type = "Ljung-Box", lag = 28)$p.value, 3)
  c2.Box_Ljung_df <- rbind(c2.Box_Ljung_df, c(LB0,LB1,LB2,LB3,LB4))
}
c2.Box_Ljung_df <- as.data.frame(c2.Box_Ljung_df)
colnames(c2.Box_Ljung_df) <- c("L-B p-val 4d","L-B p-val 7d", "L-B p-val 14d", "L-B p-val 21d", "L-B p-val 28d")
rownames(c2.Box_Ljung_df) <- names(c2.arima.models)

if(1){
  plot_lags = 10
  colors = c("black", "red", "blue", "dark green")
  c1m1_acf <- acf(resid(c2.arima.models[[1]]),
                  lag.max = plot_lags,
                  plot = FALSE,
                  na.action = na.pass)   
  c1m1_pacf <- pacf(resid(c2.arima.models[[1]]), 
                    lag.max = plot_lags, 
                    plot = FALSE,
                    na.action = na.pass) 
  c1m2_acf <- acf(resid(c2.arima.models[[2]]),
                  lag.max = plot_lags, 
                  plot = FALSE,
                  na.action = na.pass)
  c1m2_pacf <- pacf(resid(c2.arima.models[[2]]),
                    lag.max = plot_lags, 
                    plot = FALSE,
                    na.action = na.pass)
  c1m3_acf <- acf(resid(c2.arima.models[[3]]),
                  lag.max = plot_lags,
                  plot = FALSE,
                  na.action = na.pass)   
  c1m3_pacf <- pacf(resid(c2.arima.models[[3]]), 
                    lag.max = plot_lags, 
                    plot = FALSE,
                    na.action = na.pass) 
  
  
  par(mfrow = c(length(c2.arima.models), 2)) 
  plot(c1m1_acf, 
       ylim = c(-0.5, 0.5),    
       lwd = 5,              
       col = colors[1],
       main = paste(names(c2.arima.models)[1], "ACF"))
  plot(c1m1_pacf, 
       ylim = c(-0.5, 0.5),    
       lwd = 5,              
       col = colors[1],
       main = paste(names(c2.arima.models)[1], "PACF"))
  plot(c1m2_acf,
       ylim = c(-0.5, 0.5),
       lwd = 5,
       col = colors[2],
       main = paste(names(c2.arima.models)[2], "ACF"))
  plot(c1m2_pacf,
       ylim = c(-0.5, 0.5),
       lwd = 5,
       col = colors[2],
       main = paste(names(c2.arima.models)[2], "PACF"))
  plot(c1m3_acf, 
       ylim = c(-0.5, 0.5),    
       lwd = 5,              
       col = colors[3],
       main = paste(names(c2.arima.models)[3], "ACF"))
  plot(c1m3_pacf, 
       ylim = c(-0.5, 0.5),    
       lwd = 5,              
       col = colors[3],
       main = paste(names(c2.arima.models)[3], "PACF"))
  par(mfrow = c(1, 1)) 
}



AIC(c2.arima.4.1.4, c2.arima.7.1.7, c2.auto.AIC)
# df       AIC
# c2.arima.4.1.4  9 -1353.149
# c2.arima.7.1.7 15 -1353.560 <- 
# c2.auto.AIC     2 -1353.478 <- 
# c2.auto.BIC     2 -1353.478

BIC(c2.arima.4.1.4, c2.arima.7.1.7, c2.auto.AIC)
# c2.arima.4.1.4  9 -1318.075 
# c2.arima.7.1.7 15 -1295.103
# c2.auto.AIC     2 -1345.683 <- 
# c2.auto.BIC     2 -1345.683

coeftest(c2.auto.AIC)
#   Estimate Std. Error z value Pr(>|z|)   
# ar1 -0.156974   0.051885 -3.0254 0.002483 **

# save model
saveRDS(c2.auto.AIC, "./data/c2.arima110.rds")
