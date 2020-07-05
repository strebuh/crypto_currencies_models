library(xts)
library(forecast)
# library(data.table)


# load function
source("functions/function_testdf2.R")
source("functions/finding_stationary_pair.R")

# ---------------------------------------------------------------------------------------------------------------------
# load results of contegration checks
all_combinations_cointegr <- readRDS("./data/all_combinations_2coint_2.RDS")
cointegr_tb_signf <- readRDS("./data/cointegr_tb_signf_5_7.RDS")



# coint_info.8  2019-06-03 2020-06-01   365         bitcoin    0.1625           0.01         dogecoin    0.2845           0.01      one -3.85353190132444

# read filtered table of cointegration results
# cointegr_tb_signf <- readRDS("./data/cointegr_tb_signf3.RDS")

# extract data from contegration test (data clipped to shape in scope was saved there)
crypto_list <- all_combinations_cointegr$pairs_data
length(crypto_list) # [1] 4278


# create plots of given pair, choosing by number of row from result table
cryptoPairPlots(crypto_list,         # list with data
                cointegr_tb_signf,  # table of cointefration results
                n_table = 6,        # which pair to prepare plots for
                log_prices = TRUE,   # should first plot show log prices?
                plot_lags = 15,      # how many lags in ACF/PACF 
                colerograms = TRUE,  # should ACF/PACF be showed
                diffPlots = TRUE,    # should plots of differenced prices/logprices be showed
                in_sample = 365,     # how many observations in scope
                oo_sample = 15,      # number of observations out of scope
                ggplot = FALSE,       # should first plot be a ggplot based
                return_data = FALSE  # if laso to return in sample data
)         


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



# prepare formula for linear combination
names_ <- names(crypto_pair)
combination_formula <- as.formula(paste(names_[2], names_[1], sep="~"))
combination_formula_inv <- as.formula(paste(names_[1], names_[2], sep="~"))

# linear combination
model.coint <- lm(combination_formula, 
                  data = crypto_pair)
model_summary <- summary(model.coint)

# -------------------------------------------- ECM --------------------------------------------------------
# get residuals of linear combination
crypto_pair$lresid <- lag.xts(residuals(model.coint))

# Estimating ECM
combination_formula2 <- as.formula(paste(names_[4], "~" ,names_[3], " + lresid"))
combination_formula2_inv <- as.formula(paste(names_[3], "~" ,names_[4], " + lresid"))

# ECM model
model.ecm <- lm(combination_formula2,
                data = crypto_pair) 
summary(model.ecm)

# -------------------------------------------- GRANGER CASUALITY --------------------------------------------------------

# log_dogecoin ~ log_bitcoin
combination_formula3 <- as.formula(paste(names_[4], names_[3], sep="~"))
# d_cardano ~ d_siacoin
combination_formula3_inv <- as.formula(paste(names_[3], names_[4], sep="~"))


casuality <- matrix(NA, ncol = 5, nrow = 7)
for(i in 1:7){
  
  casuality[i,1] <- i
  
  # d_siacoin ~ d_cardano
  gr4_3 <- grangertest(combination_formula3,
                       data = crypto_pair,
                       order = i)
  casuality[i,2] <- gr4_3$`Pr(>F)`[2]
  casuality[i,3] <- if(gr4_3$`Pr(>F)`[2] < 0.05) "cause" else "no"
  
  # d_cardano ~ d_siacoin
  gr3_4 <- grangertest(combination_formula3_inv,
                       data = crypto_pair,
                       order = i) # lag assumed
  casuality[i,4] <- gr3_4$`Pr(>F)`[2]
  casuality[i,5] <- if(gr3_4$`Pr(>F)`[2] < 0.05) "cause" else "no"
}

casuality <- as.data.frame(casuality)
names(casuality) <- c("lags", paste0(names_[4], "_", names_[3]), "if_granger_1", paste0(names_[3], "_", names_[4]), "if_granger_2")
casuality
# H0: no casuality

saveRDS(casuality, "./data/gr_casual_dl_bitcoin_dl_dogecoin.RDS")


# -------------------------------------------- AUTO ARIMA --------------------------------------------------------
names(crypto_pair) 

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


# -------------------------------------------------------------------------------
names(crypto_pair) 

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
# Best model: ARIMA(1,1,1) with drift  
# Best model: ARIMA(0,1,0) 
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
# Best model: ARIMA(0,1,0) 
#  Best model: ARIMA(0,1,0)  
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

