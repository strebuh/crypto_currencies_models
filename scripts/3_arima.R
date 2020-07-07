library(xts)
library(forecast)
library(lmtest)

source("functions/finding_stationary_pair.R")


# load results of contegration checks
all_combinations_cointegr <- readRDS("./data/all_combinations_2coint_2.RDS")
cointegr_tb_signf <- readRDS("./data/cointegr_tb_signf_5_7.RDS")

# get data with differences and log prices
crypto_pair <- getDifferencesXTS(coint_table = cointegr_tb_signf,                # table of cointefration results
                                 n_table = 6,                                    # which pair to prepare plots for
                                 n_obs_is = 365,                                  # how many observations in scope
                                 n_obs_ooc = 15,                                  # number of observations out of scope
                                 clipped = crypto_list, # list with data after 
                                 # crypto_list = crypto_list,
                                 log_prices = TRUE
)

crypto_pair <- crypto_pair_all$in_smpl
crypto_pair_oos <- crypto_pair_all$oo_smpl

dim(crypto_pair) # [1] 365   5
names(crypto_pair)
# [1] "log_bitcoin"       "log_dogecoin"      "diff_log_bitcoin"  "diff_log_dogecoin" "lresid" 

# ---------------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------- ARIMA -----------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------- BITCOIN ----------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------

# ---- C1 - ARIMA 1
c1.arima.4.1.4 <- Arima(crypto_pair[,1],
                     order = c(4, 1, 4),
                     method = "CSS-ML",
                     optim.control = list(maxit = 1200),
                     optim.method = "L-BFGS-B",
                     include.mean = TRUE
                     )

# optim.control = list(maxit = 800),
# optim.method = "Brent"
# method = "CSS")

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
# ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
summary(c1.arima.4.1.4)

# third lags seem significant

# Series: crypto_pair[, 1] 
# ARIMA(4,1,4) 
# 
# Coefficients:
#     ar1     ar2      ar3      ar4     ma1    ma2     ma3     ma4
# -0.1592  0.0462  -0.7588  -0.3545  0.0473  0.017  0.7711  0.3634
# s.e.   1.3765  0.4263   0.1481   1.0326  1.3702  0.595  0.2926  0.9818
# 
# sigma^2 estimated as 0.001953:  log likelihood=620.99
# AIC=-1223.98   AICc=-1223.47   BIC=-1188.9
# 
# Training set error measures:
#     ME       RMSE        MAE         MPE      MAPE      MASE         ACF1
# Training set 0.0005959591 0.04364269 0.02717153 0.005326794 0.3000251 0.9961512 -0.004583973

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

Box.test(resid(c1.arima.4.1.4), 
         type = "Ljung-Box", 
         lag = 14) 
# confirmed by LB test
# Box-Ljung test
# 
# data:  resid(c1.arima.4.1.4)
# X-squared = 6.031, df = 14, p-value = 0.9657

# ---- C1 - ARIMA 2 
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
summary(c1.arima.7.1.7)
# Series: crypto_pair[, 1] 
# ARIMA(7,1,7) 
# 
# Coefficients:
#     ar1     ar2     ar3      ar4     ar5     ar6      ar7      ma1      ma2      ma3     ma4     ma5      ma6     ma7
# 0.1160  0.3969  0.2527  -0.2547  -0.612  0.2196  -0.1306  -0.2081  -0.3852  -0.2826  0.4404  0.6149  -0.3656  0.0676
# s.e.  0.9087  1.1313  0.2873   0.2263     NaN  0.8801   0.5965   0.8625   0.9876   0.3038  0.1040  0.2013   0.9714  0.5178
# 
# sigma^2 estimated as 0.001973:  log likelihood=623.57
# AIC=-1217.15   AICc=-1215.77   BIC=-1158.69
# 
# Training set error measures:
#     ME       RMSE        MAE         MPE      MAPE     MASE        ACF1
# Training set 0.0006248397 0.04349936 0.02731788 0.005557276 0.3017542 1.001517 -0.01950915

Box.test(resid(c1.arima.7.1.7), 
         type = "Ljung-Box", 
         lag = 14) 
# Box-Ljung test
# 
# data:  resid(c1.arima.7.1.7)
# X-squared = 4.0519, df = 14, p-value = 0.9951


# ---- C1 AUTO ARIMA 1 
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
# Best model: ARIMA(1,1,1) # 3  [1] "log_bitcoin"       "log_dogecoin"     

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
Box.test(resid(c1.auto.AIC), type = "Ljung-Box", lag =  7)
Box.test(resid(c1.auto.AIC), type = "Ljung-Box", lag = 14)
Box.test(resid(c1.auto.AIC), type = "Ljung-Box", lag = 21)
Box.test(resid(c1.auto.AIC), type = "Ljung-Box", lag = 28)

# ---- C1 AUTO ARIMA 2

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

# hypothesis of randomness, there is no autocorrelation
Box.test(resid(c1.auto.BIC), type = "Ljung-Box", lag =  7)
Box.test(resid(c1.auto.BIC), type = "Ljung-Box", lag = 14)
Box.test(resid(c1.auto.BIC), type = "Ljung-Box", lag = 21)
Box.test(resid(c1.auto.BIC), type = "Ljung-Box", lag = 28)
# there is autocorrelation up to 7 days, almost up to 14 days


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

# --------------------------------------------------- C1 FORCAST -----------------------------------------------------------
c1.arima.forecast <- forecast(c1.auto.AIC,
                              h = 7) 

c1.arima.forecast_df <- data.frame(as.numeric(c1.arima.forecast$mean),
                                   as.numeric(c1.arima.forecast$lower[, 2]),
                                   as.numeric(c1.arima.forecast$upper[, 2]))
names(c1.arima.forecast_df) <- c(paste0(names_[1],"_fore"), paste0(names_[1],"_lower"), paste0(names_[1],"_upper"))

# forecast data
c1.arima.forecast_df <- xts(c1.arima.forecast_df,
                            head(index(crypto_pair_oos), 7))

# add oos observations
crypto_pair_all <- rbind(crypto_pair[,1:4], head(crypto_pair_oos, 7))
dim(crypto_pair_all)

# we can put it together with the original data
c1_data_forecast <- merge(crypto_pair_all[,1], c1.arima.forecast_df)

tail(c1_data_forecast_[[1]] )

# revert log prices to prices
c1_data_forecast_ <- lapply(c1_data_forecast, function(x) exp(x[!is.na(x)]))

c1_data_forecast <- c1_data_forecast_[[1]] 
for(i in names(c1_data_forecast_)[-1]){
  c1_data_forecast <- merge(c1_data_forecast, c1_data_forecast_[[i]])
}
names(c1_data_forecast) <- c(gsub("log_","", names(c1_data_forecast)))

# original data
plot(c1_data_forecast[(nrow(c1_data_forecast)-30):nrow(c1_data_forecast),], 
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 3,
     main = "7 days ARIMA forecast of bitcoin",
     col = c("black", "blue", "red", "red"))

# real values and forecast, last 7 observations
c1.arima.forecast_df <- tail(c1_data_forecast, 7)

# finally we can calculate popular measures of ex-post prediction errors
c1.arima.forecast_df$mae   <-  abs(c1.arima.forecast_df$bitcoin - c1.arima.forecast_df$bitcoin_fore)
c1.arima.forecast_df$mse   <-  (c1.arima.forecast_df$bitcoin - c1.arima.forecast_df$bitcoin_fore)^2
c1.arima.forecast_df$mape  <-  abs((c1.arima.forecast_df$bitcoin - c1.arima.forecast_df$bitcoin_fore)/c1.arima.forecast_df$bitcoin)
c1.arima.forecast_df$amape <-  abs((c1.arima.forecast_df$bitcoin - c1.arima.forecast_df$bitcoin_fore)/(c1.arima.forecast_df$bitcoin + c1.arima.forecast_df$bitcoin_fore))

# and get their means
colMeans(c1.arima.forecast_df[, c("mae", "mse", "mape", "amape")])

# or medians
apply(c1.arima.forecast_df[, c("mae", "mse", "mape", "amape")], 2, FUN = median)


# ---------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------- DOGECOIN ---------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------

# ---- C2 - ARIMA 1
c2.arima.4.1.4 <- Arima(crypto_pair[,2],
                        order = c(4, 1, 4),
                        method = "CSS-ML",
                        optim.control = list(maxit = 1200),
                        optim.method = "L-BFGS-B",
)

coeftest(c2.arima.4.1.4)
# z test of coefficients:
#     
#     Estimate Std. Error z value  Pr(>|z|)    
# ar1  1.163539   0.380942  3.0544 0.0022553 ** 
#     ar2  0.232257   0.398564  0.5827 0.5600727    
# ar3 -0.997796   0.210271 -4.7453 2.082e-06 ***
#     ar4  0.290874   0.277646  1.0476 0.2948021    
# ma1 -1.331229   0.370338 -3.5946 0.0003248 ***
#     ma2  0.030766   0.447703  0.0687 0.9452129    
# ma3  0.941354   0.232510  4.0487 5.151e-05 ***
#     ma4 -0.374865   0.227537 -1.6475 0.0994585 .  

  
# ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
summary(c2.arima.4.1.4)
# Series: crypto_pair[, 2] 
# ARIMA(4,1,4) 
# 
# Coefficients:
#     ar1     ar2      ar3     ar4      ma1     ma2     ma3      ma4
# 1.1635  0.2323  -0.9978  0.2909  -1.3312  0.0308  0.9414  -0.3749
# s.e.  0.3809  0.3986   0.2103  0.2776   0.3703  0.4477  0.2325   0.2275
# 
# sigma^2 estimated as 0.001382:  log likelihood=685.57
# AIC=-1353.15   AICc=-1352.64   BIC=-1318.07
# 
# Training set error measures:
#     ME       RMSE        MAE         MPE      MAPE      MASE       ACF1
# Training set -0.0006492038 0.03671998 0.02420226 0.009008764 0.4036058 0.9950031 0.00408129

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

Box.test(resid(c2.arima.4.1.4), 
         type = "Ljung-Box", 
         lag = 14) 
# Box-Ljung test
# 
# data:  resid(c2.arima.4.1.4)
# X-squared = 4.0024, df = 14, p-value = 0.9955

# ---- C2 - ARIMA 2 

c2.arima.7.1.7 <- Arima(crypto_pair[,2],
                        order = c(7, 1, 7),
                        method = "CSS-ML",
                        optim.control = list(maxit = 2500),
                        optim.method = "L-BFGS-B"
)

# adfTest(crypto_pair[,2])

coeftest(c2.arima.7.1.7)
# z test of coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# ar1  0.295036         NA      NA       NA    
# ar2  0.284172   0.374979  0.7578  0.44855    
# ar3  0.213500         NA      NA       NA    
# ar4 -0.447693         NA      NA       NA    
# ar5 -0.497539   0.204011 -2.4388  0.01474 *  
#     ar6  0.278029         NA      NA       NA    
# ar7 -0.169363   0.266208 -0.6362  0.52464    
# ma1 -0.434558         NA      NA       NA    
# ma2 -0.170179   0.383752 -0.4435  0.65743    
# ma3 -0.262637   0.029853 -8.7978  < 2e-16 ***
#     ma4  0.640103         NA      NA       NA    
# ma5  0.366586   0.215851  1.6983  0.08945 .  
# ma6 -0.395326         NA      NA       NA    
# ma7  0.201267   0.221831  0.9073  0.36425    
summary(c1.arima.7.1.7)
# Series: crypto_pair[, 1] 
# ARIMA(7,1,7) 
# 
# Coefficients:
#     ar1     ar2     ar3      ar4     ar5     ar6      ar7      ma1      ma2      ma3     ma4     ma5      ma6     ma7
# 0.1160  0.3969  0.2527  -0.2547  -0.612  0.2196  -0.1306  -0.2081  -0.3852  -0.2826  0.4404  0.6149  -0.3656  0.0676
# s.e.  0.9087  1.1313  0.2873   0.2263     NaN  0.8801   0.5965   0.8625   0.9876   0.3038  0.1040  0.2013   0.9714  0.5178
# 
# sigma^2 estimated as 0.001973:  log likelihood=623.57
# AIC=-1217.15   AICc=-1215.77   BIC=-1158.69
# 
# Training set error measures:
#     ME       RMSE        MAE         MPE      MAPE     MASE        ACF1
# Training set 0.0006248397 0.04349936 0.02731788 0.005557276 0.3017542 1.001517 -0.01950915

Box.test(resid(c1.arima.7.1.7), 
         type = "Ljung-Box", 
         lag = 14) 
# Box-Ljung test
# 
# data:  resid(c1.arima.7.1.7)
# X-squared = 4.0519, df = 14, p-value = 0.9951


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

# hypothesis of randomness, there is no autocorrelation
Box.test(resid(c2.acuto.AIC), type = "Ljung-Box", lag =  7)
Box.test(resid(c2.acuto.AIC), type = "Ljung-Box", lag = 14)
Box.test(resid(c2.acuto.AIC), type = "Ljung-Box", lag = 28)
# Box-Ljung test
# 
# data:  resid(c2.acuto.AIC)
# X-squared = 4.6319, df = 14, p-value = 0.9903

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
Box.test(resid(c2.auto.BIC), type = "Ljung-Box", lag =  7)
Box.test(resid(c2.auto.BIC), type = "Ljung-Box", lag = 14)
Box.test(resid(c2.auto.BIC), type = "Ljung-Box", lag = 21)
Box.test(resid(c2.auto.BIC), type = "Ljung-Box", lag = 28)
# THERE IS AN AUTOCORRELATION

AIC(c2.arima.4.1.4, c2.arima.7.1.7, c2.auto.AIC, c2.auto.BIC)
# df       AIC
# c2.arima.4.1.4  9 -1353.149
# c2.arima.7.1.7 15 -1353.560 <- 
# c2.auto.AIC     2 -1353.478 <- 
# c2.auto.BIC     2 -1353.478

BIC(c2.arima.4.1.4, c2.arima.7.1.7, c2.auto.AIC, c2.auto.BIC)
# c2.arima.4.1.4  9 -1318.075 
# c2.arima.7.1.7 15 -1295.103
# c2.auto.AIC     2 -1345.683 <- 
# c2.auto.BIC     2 -1345.683


# --------------------------------------------------- C2 FORCAST -----------------------------------------------------------
c2.arima.forecast <- forecast(c2.auto.AIC,
                                 h = 7) 

c2.arima.forecast_df <- data.frame(as.numeric(c2.arima.forecast$mean),
                                as.numeric(c2.arima.forecast$lower[, 2]),
                                as.numeric(c2.arima.forecast$upper[, 2]))
names(c2.arima.forecast_df) <- c(paste0(names_[2],"_fore"), paste0(names_[2],"_lower"), paste0(names_[2],"_upper"))

# forecast data
c2.arima.forecast_df <- xts(c2.arima.forecast_df,
                         head(index(crypto_pair_oos), 7))

# add oos observations
crypto_pair_all <- rbind(crypto_pair[,1:4], head(crypto_pair_oos, 7))
dim(crypto_pair_all)

# we can put it together with the original data
c2_data_forecast <- merge(crypto_pair_all[,2], c2.arima.forecast_df)
dim(c2_data_forecast)

# revert log prices to prices
c2_data_forecast_ <- lapply(c2_data_forecast, function(x) exp(x[!is.na(x)]))

c2_data_forecast <- c2_data_forecast_[[1]] 
for(i in names(c2_data_forecast_)[-1]){
  c2_data_forecast <- merge(c2_data_forecast, c2_data_forecast_[[i]])
}
names(c2_data_forecast) <- c(gsub("log_","", names(c2_data_forecast)))

# original data
plot(c2_data_forecast[(nrow(c2_data_forecast)-30):nrow(c2_data_forecast),], 
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 3,
     main = "7 days ARIMA forecast of dogecoin",
     col = c("black", "blue", "red", "red"))

# real values and forecast, last 7 observations
c2.arima.forecast_df <- tail(c2_data_forecast, 7)

names(c2.arima.forecast_df)

# finally we can calculate popular measures of ex-post prediction errors
c2.arima.forecast_df$mae   <-  abs(c2.arima.forecast_df$dogecoin - c2.arima.forecast_df$dogecoin_fore)
c2.arima.forecast_df$mse   <-  (c2.arima.forecast_df$dogecoin - c2.arima.forecast_df$dogecoin_fore)^2
c2.arima.forecast_df$mape  <-  abs((c2.arima.forecast_df$dogecoin - c2.arima.forecast_df$dogecoin_fore)/c2.arima.forecast_df$dogecoin)
c2.arima.forecast_df$amape <-  abs((c2.arima.forecast_df$dogecoin - c2.arima.forecast_df$dogecoin_fore)/(c2.arima.forecast_df$dogecoin + c2.arima.forecast_df$dogecoin_fore))

# and get their means
colMeans(c2.arima.forecast_df[, c("mae", "mse", "mape", "amape")])

# or medians
apply(c2.arima.forecast_df[, c("mae", "mse", "mape", "amape")], 2, FUN = median)

