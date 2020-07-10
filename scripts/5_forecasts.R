library(xts)
library(forecast)

# ---------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------
# prepare data

# load results of contegration checks
all_combinations_cointegr <- readRDS("./data/all_combinations_coint_2.RDS")
cointegr_tb_signf <- readRDS("./data/cointegr_tb_signf_5_7.RDS")

# get data with differences and log price
crypto_pair_all <- getDifferencesXTS(coint_table = cointegr_tb_signf,               
                                     n_table = 6,                                   
                                     n_obs_is = 365,                                
                                     n_obs_ooc = 15,                                
                                     clipped = all_combinations_cointegr$pairs_data,
                                     log_prices = TRUE)

# in sample data and out of sample data
crypto_pair <- crypto_pair_all$in_smpl
crypto_pair_oos <- head(crypto_pair_all$oo_smpl, 7)

# merge in sample with in out of sample data
crypto_pair_all <- rbind(crypto_pair, crypto_pair_oos)

# set samef used further
names_ <- c("Bitcoin", "Dogecoin")

# ---------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------
# load models

c1.auto.AIC <- readRDS("./data/c1.arima111.rds")
c2.auto.AIC <- readRDS("./data/c2.arima110.rds")
crypto_pair.VAR.2 <- readRDS("./data/crypto_pair_VAR2.rds")
crypto_pair.vecm.1.asVAR <- readRDS("./data/crypto_pair_vecm1asVAR.rds")

# ---------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------

# ARIMA forcast
c1.arima.forecast <- forecast(c1.auto.AIC, h = 7)
c1.arima.forecast <- xts(data.frame(fcst = c1.arima.forecast$mean, 
                                lower = c1.arima.forecast$lower[,2], 
                                upper = c1.arima.forecast$lower[,2]),
                         index(crypto_pair_oos))

names(c1.arima.forecast) <- c(paste0(names_[1],".arima.f"),
                              paste0(names_[1],".arima.l"), 
                              paste0(names_[1],".arima.u"))

c2.arima.forecast <- forecast(c2.auto.AIC, h = 7)
c2.arima.forecast <- xts(data.frame(fcst = c2.arima.forecast$mean, 
                                lower = c2.arima.forecast$lower[,2], 
                                upper = c2.arima.forecast$lower[,2]),
                         index(crypto_pair_oos))

names(c2.arima.forecast) <- c(paste0(names_[2],".arima.f"),
                              paste0(names_[2],".arima.l"), 
                              paste0(names_[2],".arima.u"))

# ---------------------------------------------------------------------------------------------------------------------------
# VAR forecast
VAR.forecast <- predict(crypto_pair.VAR, 
                        n.ahead = 7, 
                        ci = 0.95) 

c1.var.forecast <- xts(VAR.forecast$fcst$log_bitcoin[,-4], 
                       index(crypto_pair_oos))

names(c1.var.forecast) <- c(paste0(names_[1],".var.f"), 
                            paste0(names_[1],".var.l"), 
                            paste0(names_[1],".var.u"))

c2.var.forecast <- xts(VAR.forecast$fcst$log_dogecoin[,-4], 
                       index(crypto_pair_oos))

names(c2.var.forecast) <- c(paste0(names_[2],".var.f"), 
                            paste0(names_[2],".var.l"), 
                            paste0(names_[2],".var.u"))

# ---------------------------------------------------------------------------------------------------------------------------
# VECM forecast
VECM.asVAR.forecast <- predict(crypto_pair.vecm.1.asVAR, 
                               n.ahead = 7, 
                               ci = 0.95)

c1.vecm.forecast <- xts(VECM.asVAR.forecast$fcst$log_bitcoin[,-4], 
                        index(crypto_pair_oos))

names(c1.vecm.forecast) <- c(paste0(names_[1],".vecm.f"), 
                             paste0(names_[1],".vecm.l"), 
                             paste0(names_[1],".vecm.u"))

c2.vecm.forecast <- xts(VECM.asVAR.forecast$fcst$log_dogecoin[,-4], 
                        index(crypto_pair_oos))

names(c2.vecm.forecast) <- c(paste0(names_[2],".vecm.f"),
                             paste0(names_[2],".vecm.l"), 
                             paste0(names_[2],".vecm.u"))

# ---------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------
# combine forecasts


# -- Bitcoin --

# merge forecasts with in sample data
c1_forecasts <- cbind(c1.arima.forecast, 
                      c1.var.forecast, 
                      c1.vecm.forecast)

c1_data_forecast <- merge(crypto_pair_all[,1], 
                          c1_forecasts)

# revert log prices to prices
c1_data_forecast_org1 <- lapply(c1_data_forecast, function(x) exp(x[!is.na(x)]))

# transform from list to xts
c1_data_forecast <- c1_data_forecast_org1[[1]]

for(i in names(c1_data_forecast_org1)[-1]){
  c1_data_forecast <- merge(c1_data_forecast, 
                            c1_data_forecast_org1[[i]])
}
names(c1_data_forecast) <- c(gsub("log_","", names(c1_data_forecast)))
dim(c1_data_forecast) # [1] 372  10
tail(c1_data_forecast, 10)



# -- Dogecoin --

# merge forecasts with in sample data
c2_forecasts <- cbind(c2.arima.forecast, 
                      c2.var.forecast, 
                      c2.vecm.forecast)

c2_data_forecast <- merge(crypto_pair_all[,2], 
                          c2_forecasts)

# revert log prices to prices
c2_data_forecast_org1 <- lapply(c2_data_forecast, function(x) exp(x[!is.na(x)]))

# transform from list to xts
c2_data_forecast <- c2_data_forecast_org1[[1]] 

for(i in names(c2_data_forecast_org1)[-1]){
  c2_data_forecast <- merge(c2_data_forecast, 
                            c2_data_forecast_org1[[i]])
}
names(c2_data_forecast) <- c(gsub("log_","", names(c2_data_forecast)))
dim(c2_data_forecast) # [1] 372  10
tail(c2_data_forecast, 10)

# ---------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------
#  plots and get errors

if(1){
  # plto 30 days of forecasts in a loop
  model_names <- c("arima", "var", "vecm")
  model_datas <- list(c1_data_forecast, c2_data_forecast)
  
  errors <- list()
  
  for(data in model_datas){
    par(mfrow = c(3,1))
    print(
      plot(data[(nrow(data)-30):nrow(data),
                c(1, grep(model_names[1], names(data)))],
           major.ticks = "years",
           grid.ticks.on = "years",
           grid.ticks.lty = 3,
           main = paste("7 days", model_names[1], "forecast of", names(data)[1]),
           col = c("black", "blue", "red", "red"))
    )
    
    print(
      plot(data[(nrow(data)-30):nrow(data),
                c(1, grep(model_names[2], names(data)))],
           major.ticks = "years",
           grid.ticks.on = "years",
           grid.ticks.lty = 3,
           main = paste("7 days", model_names[2], "forecast of", names(data)[1]),
           col = c("black", "blue", "red", "red"))
    )
    print(
      plot(data[(nrow(data)-30):nrow(data),
                c(1, grep(model_names[3], names(data)))],
           major.ticks = "years",
           grid.ticks.on = "years",
           grid.ticks.lty = 3,
           main = paste("7 days", model_names[3], "forecast of", names(data)[1]),
           col = c("black", "blue", "red", "red"))
    )
    par(mfrow = c(1,1)) 
    
    # names of forecast variables
    f1 <- grep(paste0(model_names[1],"\\.f"), names(data))
    f2 <- grep(paste0(model_names[2],"\\.f"), names(data))
    f3 <- grep(paste0(model_names[3],"\\.f"), names(data))
    
    # data for errors
    data <- tail(data, 7)
    
    # ARIMA errors
    arima.mae   <-  abs(data[,names(data)[1]] - data[,f1])
    arima.mse   <-  (data[,names(data)[1]] - data[,f1])^2
    arima.mape  <-  abs((data[,names(data)[1]] - data[,f1])/
                          data[,names(data)[1]])
    arima.amape <-  abs((data[,names(data)[1]] - data[,f1])/
                          (data[,names(data)[1]] + data[,f1]))
    
    arima.errors <- cbind(arima.mae, arima.mse, arima.mape, arima.amape)
    arima.errors.mean <- colMeans(arima.errors)
    # if mse very low, multiply it
    arima.errors.mean[2] <- if(arima.errors.mean[2] < 0.000001) arima.errors.mean[2]*1000000 else round(arima.errors.mean[2], 5)  
    var.errors.mean <- round(arima.errors.mean, 5)
    
    arima.errors.median <- round(unlist(lapply(arima.errors, median)), 5)
    
    # VAR errors
    var.mae   <-  abs(data[,names(data)[1]] - data[,f2])
    var.mse   <-  (data[,names(data)[1]] - data[,f2])^2
    var.mape  <-  abs((data[,names(data)[1]] - data[,f2])/
                        data[,names(data)[1]])
    var.amape <-  abs((data[,names(data)[1]] - data[,f2])/
                        (data[,names(data)[1]] + data[,f2]))
    
    var.errors <- cbind(var.mae, var.mse, var.mape, var.amape)
    var.errors.mean <- colMeans(var.errors)
    # if mse very low, multiply it
    var.errors.mean[2] <- if(var.errors.mean[2] < 0.000001) var.errors.mean[2]*1000000 else round(var.errors.mean[2], 5) 
    var.errors.mean <- round(var.errors.mean, 5)
    var.errors.median <- round(unlist(lapply(var.errors, median)), 5)
    
    # VECM errors
    vecm.mae   <-  abs(data[,names(data)[1]] - data[,f3])
    vecm.mse   <-  (data[,names(data)[1]] - data[,f3])^2
    vecm.mape  <-  abs((data[,names(data)[1]] - data[,f3])/
                         data[,names(data)[1]])
    vecm.amape <-  abs((data[,names(data)[1]] - data[,f3])/
                         (data[,names(data)[1]] + data[,f3]))
    
    vecm.errors <- cbind(vecm.mae, vecm.mse, vecm.mape, vecm.amape)
    vecm.errors.mean <- colMeans(vecm.errors) 
    # if mse very low, multiply it
    vecm.errors.mean[2] <- if(vecm.errors.mean[2] < 0.000001) vecm.errors.mean[2]*1000000 else round(vecm.errors.mean[2], 5) 
    vecm.errors.mean <- round(vecm.errors.mean, 5)
    vecm.errors.median <- round(unlist(lapply(vecm.errors, median)), 5)
    
    # combine in one df
    current_errors <- as.data.frame(cbind(arima.errors.mean, var.errors.mean, vecm.errors.mean,
                                          arima.errors.median, var.errors.median, vecm.errors.median))
    rownames(current_errors) <- c("mae", "mse", "mape", "amape") 
    errors[[names(data)[1]]] <- current_errors
    
  }
  print(names(errors)[1])
  print(errors$bitcoin)
  print(names(errors)[2])
  print(errors$dogecoin)
}

