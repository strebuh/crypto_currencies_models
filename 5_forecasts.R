
# ---------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------- ARIMA ---------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------

# --------------------------------------------------- C1  -----------------------------------------------------------
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

# ---------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------- VAR ---------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------- FORECAST -------------------------------------------------------------------

# and run a forecast
crypto_pair.VAR.forecast <- predict(crypto_pair.VAR,
                                    n.ahead = 7,
                                    ci = 0.95) 

names(crypto_pair.VAR.forecast)
# [1] "fcst"     "endog"    "model"    "exo.fcst"

# VAR forecasts for both currencies
crypto_pair.VAR.forecast$fcst$log_bitcoin
crypto_pair.VAR.forecast$fcst$log_dogecoin

# add oos observations
crypto_pair_all <- rbind(crypto_pair[,1:4], head(crypto_pair_oos, 7))
dim(crypto_pair_all)

# --------- BITTORRENT -------
c1_VAR.forecast <- xts(crypto_pair.VAR.forecast$fcst$log_bitcoin[,-4], 
                       head(index(crypto_pair_oos), 7))

# lets change the names 
names(c1_VAR.forecast) <- c(paste0(names_[1],"_fore_VAR"), paste0(names_[1],"_lower_VAR"), paste0(names_[1],"_upper_VAR"))

# --------- DOGECOIN -------

# VAR forecasts
c2_VAR.forecast <- xts(crypto_pair.VAR.forecast$fcst$log_dogecoin[,-4], 
                       head(index(crypto_pair_oos), 7))
names(c2_VAR.forecast) <- c(paste0(names_[2],"_fore_VAR"), paste0(names_[2],"_lower_VAR"), paste0(names_[2],"_upper_VAR"))


# put the data together
crypto_pair_VAR <- merge(crypto_pair_all[,1:2],
                         c1_VAR.forecast,
                         c2_VAR.forecast)

# revert log prices to prices
crypto_pair_VAR_data_forecast_ <- lapply(crypto_pair_VAR, function(x) exp(x[!is.na(x)]))

crypto_pair_VAR_data_forecast <- crypto_pair_VAR_data_forecast_[[1]]
for(i in names(crypto_pair_VAR_data_forecast_)[-1]){
  crypto_pair_VAR_data_forecast <- merge(crypto_pair_VAR_data_forecast, crypto_pair_VAR_data_forecast_[[i]])
}
names(crypto_pair_VAR_data_forecast) <- c(gsub("log_","", names(crypto_pair_VAR)))


# plot 30 last days including forecast - bitcoint
plot(crypto_pair_VAR_data_forecast[(nrow(crypto_pair_VAR_data_forecast)-30):nrow(crypto_pair_VAR_data_forecast), 
                                   grep("^bitcoin", names(crypto_pair_VAR_data_forecast))], 
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 3,
     main = paste0("7 days VAR forecast of ", names(crypto_pair_VAR_data_forecast))[1],
     col = c("black", "blue", "red", "red"))

# plot 30 last days including forecast - dogecoin
plot(crypto_pair_VAR_data_forecast[(nrow(crypto_pair_VAR_data_forecast)-30):nrow(crypto_pair_VAR_data_forecast), 
                                   grep("^dogecoin", names(crypto_pair_VAR_data_forecast))], 
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 3,
     main = paste0("7 days VAR forecast of ", names(crypto_pair_VAR_data_forecast))[2],
     col = c("black", "blue", "red", "red"))


# real values and forecast, last 7 observations
crypto_pair_VAR <- tail(crypto_pair_VAR_data_forecast, 7)


# errors bitcoin
crypto_pair_VAR$mae.bitcoin   <-  abs(crypto_pair_VAR$bitcoin - crypto_pair_VAR$bitcoin_fore)
crypto_pair_VAR$mse.bitcoin <-  (crypto_pair_VAR$bitcoin - crypto_pair_VAR$bitcoin_fore)^2
crypto_pair_VAR$mape.bitcoin  <-  abs((crypto_pair_VAR$bitcoin - crypto_pair_VAR$bitcoin_fore)/crypto_pair_VAR$bitcoin)
crypto_pair_VAR$amape.bitcoin <-  abs((crypto_pair_VAR$bitcoin - crypto_pair_VAR$bitcoin_fore) / 
                                        (crypto_pair_VAR$bitcoin + crypto_pair_VAR$bitcoin_fore))
# errors dogecoin
crypto_pair_VAR$mae.dogecoin   <-  abs(crypto_pair_VAR$dogecoin - crypto_pair_VAR$dogecoin_fore)
crypto_pair_VAR$mse.dogecoin   <-  (crypto_pair_VAR$dogecoin - crypto_pair_VAR$dogecoin_fore)^2
crypto_pair_VAR$mape.dogecoin  <-  abs((crypto_pair_VAR$dogecoin - crypto_pair_VAR$dogecoin_fore)/crypto_pair_VAR$dogecoin)
crypto_pair_VAR$amape.dogecoin <-  abs((crypto_pair_VAR$dogecoin - crypto_pair_VAR$dogecoin_fore) / 
                                         (crypto_pair_VAR$dogecoin + crypto_pair_VAR$dogecoin_fore))

# get measures
colMeans(crypto_pair_VAR[,grepl("mae|mse|mape|amape", names(crypto_pair_VAR))], na.rm = TRUE)
# mae.bitcoin    mse.bitcoin   mape.bitcoin  amape.bitcoin   mae.dogecoin   mse.dogecoin  mape.dogecoin amape.dogecoin 
# 1.832093e+02   5.512044e+04   1.902838e-02   9.372086e-03   1.831285e-05   5.515529e-10   7.074903e-03   3.527304e-03 

# ---------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------- VECM ---------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------- FORECAST -------------------------------------------------------------------

# and run a forecast
crypto_pair.vecm.1.asVAR.forecast <- predict(crypto_pair.vecm.1.asVAR,
                                             n.ahead = 7,
                                             ci = 0.95)

# # lets see the result
# crypto_pair.vecm.1.asVAR.forecast
# names(crypto_pair.vecm.1.asVAR.forecast)
# # [1] "fcst"     "endog"    "model"    "exo.fcst"
# # str(crypto_pair.vecm.1.asVAR.forecast)
# 
# # VAR forecasts for both currencies
# crypto_pair.vecm.1.asVAR.forecast$fcst$log_bitcoin
# crypto_pair.vecm.1.asVAR.forecast$fcst$log_dogecoin


c1_VECM.forecast <- xts(crypto_pair.vecm.1.asVAR.forecast$fcst$log_bitcoin[,-4], 
                        head(index(crypto_pair_oos), 7))
names(c1_VECM.forecast) <- c(paste0(names_[1],"_fore_VAR"), paste0(names_[1],"_lower_VAR"), paste0(names_[1],"_upper_VAR"))

# dogecoin forecasts 
c2_VECM.forecast <- xts(crypto_pair.vecm.1.asVAR.forecast$fcst$log_dogecoin[,-4], 
                        head(index(crypto_pair_oos), 7))
names(c2_VECM.forecast) <- c(paste0(names_[2],"_fore_VAR"), paste0(names_[2],"_lower_VAR"), paste0(names_[2],"_upper_VAR"))

# # add oos observations
# crypto_pair_all_ <- rbind(crypto_pair[,-ncol(crypto_pair)], head(crypto_pair_oos, 7))

# lets put the data together
crypto_pair_VECM <- merge(crypto_pair_all[,1:2],
                          c1_VECM.forecast,
                          c2_VECM.forecast)


# revert log prices to prices
crypto_pair_VECM_data_forecast_ <- lapply(crypto_pair_VECM, function(x) exp(x[!is.na(x)]))

crypto_pair_VECM_data_forecast <- crypto_pair_VECM_data_forecast_[[1]]
for(i in names(crypto_pair_VECM_data_forecast_)[-1]){
  crypto_pair_VECM_data_forecast <- merge(crypto_pair_VECM_data_forecast, crypto_pair_VECM_data_forecast_[[i]])
}
names(crypto_pair_VECM_data_forecast) <- c(gsub("log_","", names(crypto_pair_VECM_data_forecast)))


# plot 30 last days including forecast - bitcoint
plot(crypto_pair_VECM_data_forecast[(nrow(crypto_pair_VECM_data_forecast)-30):nrow(crypto_pair_VECM_data_forecast), 
                                    grep("^bitcoin", names(crypto_pair_VECM_data_forecast))], 
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 3,
     main = paste0("7 days VECM forecast of ", names(crypto_pair_VECM_data_forecast))[1],
     col = c("black", "blue", "red", "red"))

# plot 30 last days including forecast - dogecoin
plot(crypto_pair_VECM_data_forecast[(nrow(crypto_pair_VECM_data_forecast)-30):nrow(crypto_pair_VECM_data_forecast), 
                                    grep("^dogecoin", names(crypto_pair_VECM_data_forecast))], 
     major.ticks = "years", 
     grid.ticks.on = "years",
     grid.ticks.lty = 3,
     main = paste0("7 days VECM forecast of ", names(crypto_pair_VECM_data_forecast))[2],
     col = c("black", "blue", "red", "red"))


# real values and forecast, last 7 observations
crypto_pair_VECM <- tail(crypto_pair_VECM_data_forecast, 7)


# errors bitcoin
crypto_pair_VECM$mae.bitcoin   <-  abs(crypto_pair_VECM$bitcoin - crypto_pair_VECM$bitcoin_fore)
crypto_pair_VECM$mse.bitcoin <-  (crypto_pair_VECM$bitcoin - crypto_pair_VECM$bitcoin_fore)^2
crypto_pair_VECM$mape.bitcoin  <-  abs((crypto_pair_VECM$bitcoin - crypto_pair_VECM$bitcoin_fore)/crypto_pair_VECM$bitcoin)
crypto_pair_VECM$amape.bitcoin <-  abs((crypto_pair_VECM$bitcoin - crypto_pair_VECM$bitcoin_fore) / 
                                         (crypto_pair_VECM$bitcoin + crypto_pair_VECM$bitcoin_fore))
# errors dogecoin
crypto_pair_VECM$mae.dogecoin   <-  abs(crypto_pair_VECM$dogecoin - crypto_pair_VECM$dogecoin_fore)
crypto_pair_VECM$mse.dogecoin   <-  (crypto_pair_VECM$dogecoin - crypto_pair_VECM$dogecoin_fore)^2
crypto_pair_VECM$mape.dogecoin  <-  abs((crypto_pair_VECM$dogecoin - crypto_pair_VECM$dogecoin_fore)/crypto_pair_VECM$dogecoin)
crypto_pair_VECM$amape.dogecoin <-  abs((crypto_pair_VECM$dogecoin - crypto_pair_VECM$dogecoin_fore) / 
                                          (crypto_pair_VECM$dogecoin + crypto_pair_VECM$dogecoin_fore))

# get measures
colMeans(crypto_pair_VECM[,grepl("mae|mse|mape|amape", names(crypto_pair_VECM))], na.rm = TRUE)
# mae.bitcoin    mse.bitcoin   mape.bitcoin  amape.bitcoin   mae.dogecoin   mse.dogecoin  mape.dogecoin amape.dogecoin 
# 2.408836e+02   7.703205e+04   2.497780e-02   1.228432e-02   3.236348e-05   1.228102e-09   1.252319e-02   6.216796e-03 
