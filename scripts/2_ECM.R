library(xts)
library(forecast)
# library(data.table)

# ---------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------

# load function
source("functions/function_testdf2.R")
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
# visiual inspection

# create plots of given pair, choosing by number of row from result table
cryptoPairPlots(crypto_list,            # list with data
                cointegr_tb_signf,      # table of cointefration results
                n_table = 6,            # which pair to prepare plots for
                log_prices = TRUE,      # should first plot show log prices?
                plot_lags = 15,         # how many lags in ACF/PACF 
                # colerograms = TRUE,   # should ACF/PACF be showed
                diffPlots = TRUE,       # should plots of differenced prices/logprices be showed
                in_sample = 365,        # how many observations in scope
                oo_sample = 15,         # number of observations out of scope
                alpha = c(1, 0.5),
                ggplot = FALSE,         # should first plot be a ggplot based
                return_data = FALSE)    # if laso to return in sample data
         

# ---------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------
# linear combination

# prepare formula for linear combination
names_ <- names(crypto_pair)
combination_formula <- as.formula(paste(names_[2], names_[1], sep="~"))
combination_formula_inv <- as.formula(paste(names_[1], names_[2], sep="~"))

# linear combination
model.coint <- lm(combination_formula, 
                  data = crypto_pair)
model_summary <- summary(model.coint)
model_summary

# ---------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------
# ECM

# get residuals of linear combination
crypto_pair$lresid <- lag.xts(residuals(model.coint))

# Estimating ECM
ecm_formula <- as.formula(paste(names_[4], "~" ,names_[3], " + lresid"))
ecm_formula_inv <- as.formula(paste(names_[3], "~" ,names_[4], " + lresid"))

# ECM model
model.ecm <- lm(ecm_formula,
                data = crypto_pair) 
summary(model.ecm)

# ---------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------
# GRANGER CASUALITY

# log_dogecoin ~ log_bitcoin
granger_formula <- as.formula(paste(names_[4], names_[3], sep="~"))

# d_cardano ~ d_siacoin
granger_formula_inv <- as.formula(paste(names_[3], names_[4], sep="~"))

# matrix for results
casuality <- matrix(NA, ncol = 5, nrow = 7)
for(i in 1:7){
  
  # lag numer
  casuality[i,1] <- i
  
  # d_siacoin ~ d_cardano
  gr4_3 <- grangertest(granger_formula,
                       data = crypto_pair,
                       order = i)
  casuality[i,2] <- round(gr4_3$`Pr(>F)`[2],3)
  casuality[i,3] <- if(gr4_3$`Pr(>F)`[2] < 0.05) "cause" else "no"
  
  # d_cardano ~ d_siacoin
  gr3_4 <- grangertest(granger_formula_inv,
                       data = crypto_pair,
                       order = i) # lag assumed
  casuality[i,4] <- round(gr3_4$`Pr(>F)`[2],3)
  casuality[i,5] <- if(gr3_4$`Pr(>F)`[2] < 0.05) "cause" else "no"
}

# transform to data fame
casuality <- as.data.frame(casuality)
names(casuality) <- c("lags", paste0("p-val:", names_[3], "~", names_[4]), "if_granger_1", paste0("p-val:", names_[4], "~", names_[3]), "if_granger_2")
casuality

# save results
saveRDS(casuality, "./data/gr_casual_dl_bitcoin_dl_dogecoin.RDS")


