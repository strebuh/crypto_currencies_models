library(xts)
library(forecast)
library(lmtest)

source("functions/finding_stationary_pair.R")


crypto_list <- readRDS("./data/crypto_currencies_data.RDS")
cointegr_tb_signf2 <- readRDS("./data/cointegr_tb_signf2.RDS")
cointegr_tb_signf2[,c("cc1", "cc2")] <- sapply(cointegr_tb_signf2[,c("cc1", "cc2")], as.character )

# # coint_info.157    365  ethereum    0.2578           0.01     monacoin    0.3464           0.01  -4.215708 1 0 -0.9094 compare combin_adf




pair13 <- getDifferencesXTS(cointegr_tb_signf2, 13, 365, crypto_list)
head(pair13)
dim(pair13)

auto.best.AIC <- auto.arima(pair[,1],
                           d = 1,             # parameter d of ARIMA model
                           max.p = 8,         # Maximum value of p
                           max.q = 8,         # Maximum value of q
                           max.order = 16,    # maximum p+q
                           start.p = 1,       # Starting value of p in stepwise procedure
                           start.q = 1,       # Starting value of q in stepwise procedure
                           ic = "aic",        # Information criterion to be used in model selection.
                           stepwise = FALSE,  # if FALSE considers all models
                           allowdrift = TRUE, # include a constant
                           trace = TRUE)      # show summary of all models considered



# H0: The data are independently distributed
# Ha: The data are not independently distributed; they exhibit serial correlation.
# Box.test(resid(XRParima212), 
#          type = "Ljung-Box", 
#          lag = 9) 
# HOLO.arima11.1.11_3 <- Arima(crypto_pair$holo,  
#                              order = c(11, 1, 11),
#                              fixed = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0, NA, 0, NA, NA, NA, 0, 0)
#                              )
# coeftest(HOLO.arima11.1.11_3)
# summary(HOLO.arima11.1.11_3)
# Box.test(resid(XRParima212), 
#          type = "Ljung-Box", 
#          lag = 11) 

par(mfrow = c(2, 1)) 
acf(resid(HOLO.arima11.1.11_2),
    lag.max = 12, 
    ylim = c(-0.5, 0.5),    
    lwd = 5,              
    col = "dark green",
    na.action = na.pass)   
pacf(resid(HOLO.arima11.1.11_2), 
     lag.max = 12, 
     ylim = c(-0.5, 0.5),
     lwd = 5, col = "dark green",
     na.action = na.pass) 
par(mfrow = c(1, 1)) 



# -------------------------------------------------------------------------------------------------------------
crypto_pair <- cryptoPairPlots(smpl1_dt_sect, 4)






coeftest(try_arima)
a <- summary(try_arima)
try_arima$bic
try_arima$aicc
    
try_arima2 <- Arima(crypto_pair$dogecoin,
                   order = c(3, 1, 3)
                   # ,
                   # optim.control = list(maxit = 2000),
                   # optim.method = "L-BFGS-B"
)
coeftest(try_arima2)
summary(try_arima2)
install.packages("astsa")
library(astsa)
acf2(crypto_pair$d_dogecoin)
acf2(crypto_pair$d_nem)

# ---- 1 ----
names(crypto_pair)
iota.10.1.1 <- Arima(crypto_pair$iota,
                      order = c(15, 1, 15)
                      )
                           # optim.control = list(maxit = 800),
                           # optim.method = "Brent"
                           # method = "CSS")

coeftest(iota.15.1.15)
summary(iota.15.1.15)

iota.15.1.15.c <- Arima(crypto_pair$iota,  
                      order = c(15, 1, 15),
                      include.constant = T
                      # optim.control = list(maxit = 800),
                      # optim.method = "Brent"
                      # method = "CSS"
                      )
coeftest(iota.15.1.15.c)
summary(iota.15.1.15.c)

iota.10.1.10 <- Arima(crypto_pair$iota,  
                      order = c(10, 1, 10))
                      # optim.control = list(maxit = 800),
                      # optim.method = "Brent"
                      # method = "CSS")

coeftest(iota.10.1.10)
summary(iota.10.1.10)

length(crypto_pair$iota)
iota.10.1.10.c <- Arima(crypto_pair$iota,  
                      order = c(10, 1, 10),
                      include.constant = TRUE
                      )
# optim.control = list(maxit = 800),
# optim.method = "Brent"
# method = "CSS")

coeftest(iota.10.1.10.c)
summary(iota.10.1.10.c)
                      

HOLO.arima11.1.11_ <- Arima(crypto_pair$holo,  
                            order = c(11, 1, 11),
                            optim.control = list(maxit = 1400),
                            # optim.method = "L-BFGS-B",
                            # hessian = FALSE
                            # method = "CSS"
)

# optim.method: "Nelder-Mead" (default), "BFGS", "CG", 
# "L-BFGS-B", "SANN", "Brent"
# method: "CSS-ML", "ML", "CSS"

coeftest(HOLO.arima11.1.11_)
summary(HOLO.arima11.1.11_)

par(mfrow = c(2, 1)) 
acf(resid(HOLO.arima11.1.11),
    lag.max = 12, 
    ylim = c(-0.5, 0.5),    
    lwd = 5,              
    col = "dark green",
    na.action = na.pass)   
pacf(resid(HOLO.arima11.1.11), 
     lag.max = 12, 
     ylim = c(-0.5, 0.5),
     lwd = 5, col = "dark green",
     na.action = na.pass) 
par(mfrow = c(1, 1)) 

Box.test(resid(XRParima212), 
         type = "Ljung-Box", 
         lag = 11) 

# ---- 2 ----

HOLO.arima11.1.11_2 <- Arima(crypto_pair$holo,  
                             order = c(11, 1, 11),
                             include.constant = TRUE,
                             optim.control = list(maxit = 800),
                             optim.method = "L-BFGS-B"
)

coeftest(HOLO.arima11.1.11_2)
summary(HOLO.arima11.1.11_2)

par(mfrow = c(2, 1)) 
acf(resid(HOLO.arima11.1.11_2),
    lag.max = 12, 
    ylim = c(-0.5, 0.5),    
    lwd = 5,              
    col = "dark green",
    na.action = na.pass)   
pacf(resid(HOLO.arima11.1.11_2), 
     lag.max = 12, 
     ylim = c(-0.5, 0.5),
     lwd = 5, col = "dark green",
     na.action = na.pass) 
par(mfrow = c(1, 1)) 

Box.test(resid(XRParima212), 
         type = "Ljung-Box", 
         lag = 11) 
