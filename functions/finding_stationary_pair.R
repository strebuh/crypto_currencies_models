library(tidyverse)
library(xts)
library(fUnitRoots)
library(lmtest)
library(vars)
library(tsDyn)
library(scales)

#----------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------

source("functions/getCryptoHistoricalPrice.R")
source("functions/function_testdf2.R")

#----------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------

#merge to xts into one df
merge_crypto_pair <- function(x,       # 1st df from getCryptoHistoricalPrice
                              y,       # 2nd df from getCryptoHistoricalPrice
                              x_,      # name of 1st item
                              y_,      # name of 1st item 
                              xts = T  # should be transformed to XTS?
                              ){
  
  xy <- merge(x[,c("Date", "Close")], y[,c("Date", "Close")], by="Date")
  
  if(xts){
    xy <- xts(xy[,-c(1)], order.by = xy[,"Date"])
    names(xy) <- c(x_, y_)
  }
  return(xy)
}

#----------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------
# do cointegration checks for given pair of currencies wihtoug Johansen

find_cointegration <- function(xy,                  # df merged xts with prices
                               aug = 10,            # numer of lags to use
                               include_dates = T,   # range of dates be incorporated in result
                               log_prices = T       # if to transform to log prices
                               ){

  # log transformation
  if(log_prices){
    xy[,1] <- log(xy[,1])
    xy[,2] <- log(xy[,2])
  }

  # 1st item ADF test
  x <- testdf(variable = xy[,1],
              max.augmentations = aug, max.order = aug)
  
  # columns holding Breush-Godfrey p-values 
  bg_test_cols <- names(x)[(ncol(x) - aug+1):ncol(x)]
  
  # select adf from line where all BG pvalues are above 5%
  x_p_adf <-x[rowSums(x[bg_test_cols] > 0.05) == aug, "p_adf"][1]

  # if that ADF over 5% compute difference and make for it ADF in similiar fashion
  if(x_p_adf > 0.05){
    x_d <- testdf(variable = diff.xts(xy[,1]),
                  max.augmentations = aug, max.order = aug)
    # pick value where all BG above 5%
    x_d_p_adf <- x_d[rowSums(x_d[bg_test_cols] > 0.05) == aug, "p_adf"][1]

  } else {
    # if ADF of 1st item below 5%, data stationary, difference gets NA (not comptted)
    x_d_p_adf <- NA
  }

  # do the same for 2nd item
  y <- testdf(variable = xy[,2],
              max.augmentations = aug,  max.order = aug)

  y_p_adf <-y[rowSums(y[bg_test_cols] > 0.05) == aug, "p_adf"][1]

  if(y_p_adf > 0.05){
    y_d <- testdf(variable = diff.xts(xy[,2]),
                  max.augmentations = aug, max.order = aug)
    y_d_p_adf <- y_d[rowSums(y_d[bg_test_cols] > 0.05) == aug, "p_adf"][1]

  } else {
    y_d_p_adf <- NA
  }

  names_ <- names(xy)
  combination_formula <- as.formula(paste(names_[2], names_[1], sep="~"))

  # -- linear combination -- 
  model.coint <- lm(combination_formula,
                    data = xy)
  model_summary <- summary(model.coint)

  # ADF of linear combination
  xy_comb_test <- testdf(variable = residuals(model.coint),
                         max.augmentations = aug, max.order = aug)

  # ADF result wher all BG above 5%
  bg_all_aug <- xy_comb_test[rowSums(xy_comb_test[bg_test_cols] > 0.05) == aug,]

  # if such item exists, prepare results items
  if(dim(bg_all_aug)[1] != 0){
    xy_comb_p_adf <- bg_all_aug$p_adf[1]
    xy_comb_adf_stat <- bg_all_aug$adf[1]
    xy_comb_aug <- bg_all_aug$augmentations[1]
  } else {
    xy_comb_p_adf <- NA
    xy_comb_adf_stat < NA
    xy_comb_aug <- NA
  }
  # cointegration vector to be printedin result
  coint_vec <- paste(c(1, round(-model_summary$coefficients[1,1],4), round(-model_summary$coefficients[2,1],4)), collapse=" ")

  # output information if data is cointegrated
  coint_info <- if(!is.na(x_d_p_adf) & !is.na(y_d_p_adf) & !is.na(xy_comb_adf_stat)) "compare combin_adf" else "not integrated"

  #  prepare vectorsof results, with or withoug dates
  if(include_dates){
    res_names <- c("first_is", "last_is", "n_obs","cc1", "cc1_p_adf", "cc1_diff_p_adf", "cc2", "cc2_p_adf", "cc2_diff_p_adf", "combin_adf", "coint_vec", "conint_info")
    results <- c(as.character(index(xy)[1]), as.character(index(xy)[nrow(xy)]), nrow(xy), names(xy)[1], round(x_p_adf,4), round(x_d_p_adf,4), names(xy)[2], round(y_p_adf,4), round(y_d_p_adf,4),
                 xy_comb_adf_stat, coint_vec, coint_info)
  } else {
    res_names <- c("n_obs","cc1", "cc1_p_adf", "cc1_diff_p_adf", "cc2", "cc2_p_adf", "cc2_diff_p_adf", "combin_adf", "coint_vec", "conint_info")
    results <- c(nrow(xy), names(xy)[1], round(x_p_adf,4), round(x_d_p_adf,4), names(xy)[2], round(y_p_adf,4), round(y_d_p_adf,4),
                 xy_comb_adf_stat, coint_vec, coint_info)
  }
  
  # rename results and return it
  names(results) <- res_names
  return(results)
}

#----------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------
# do cointegration checks for given pair of currencies WITH Johansen

find_cointegration2 <- function(xy, 
                                aug = 10, 
                                include_dates = T, 
                                log_prices = T
                                ){
  
  # log transformation
  if(log_prices){
    xy[,1] <- log(xy[,1])
    xy[,2] <- log(xy[,2])
  }
  
  # 1st item ADF test
  x <- testdf(variable = xy[,1],
              max.augmentations = aug, max.order = aug)
  
  # columns holding Breush-Godfrey p-values 
  bg_test_cols <- names(x)[(ncol(x) - aug+1):ncol(x)]
  
  # select adf from line where all BG pvalues are above 5%
  x_p_adf <-x[rowSums(x[bg_test_cols] > 0.05) == aug, "p_adf"][1]
  
  # if that ADF over 5% compute difference and make for it ADF in similiar fashion
  if(x_p_adf > 0.05){
    x_d <- testdf(variable = diff.xts(xy[,1]),
                  max.augmentations = aug, max.order = aug)
    # pick value where all BG above 5%
    x_d_p_adf <- x_d[rowSums(x_d[bg_test_cols] > 0.05) == aug, "p_adf"][1]
    
  } else {
    # if ADF of 1st item below 5%, data stationary, difference gets NA (not comptted)
    x_d_p_adf <- NA
  }
  
  # do the same for 2nd item
  y <- testdf(variable = xy[,2],
              max.augmentations = aug,  max.order = aug)
  
  y_p_adf <-y[rowSums(y[bg_test_cols] > 0.05) == aug, "p_adf"][1]
  
  if(y_p_adf > 0.05){
    y_d <- testdf(variable = diff.xts(xy[,2]),
                  max.augmentations = aug, max.order = aug)
    y_d_p_adf <- y_d[rowSums(y_d[bg_test_cols] > 0.05) == aug, "p_adf"][1]
    
  } else {
    y_d_p_adf <- NA
  }
  
  names_ <- names(xy)
  combination_formula <- as.formula(paste(names_[2], names_[1], sep="~"))
  
  # -- linear combination -- 
  model.coint <- lm(combination_formula,
                    data = xy)
  model_summary <- summary(model.coint)
  
  # ADF of linear combination
  xy_comb_test <- testdf(variable = residuals(model.coint),
                         max.augmentations = aug, max.order = aug)
  
  # ADF result wher all BG above 5%
  bg_all_aug <- xy_comb_test[rowSums(xy_comb_test[bg_test_cols] > 0.05) == aug,]
  
  # if such item exists, prepare results items
  if(dim(bg_all_aug)[1] != 0){
    xy_comb_p_adf <- bg_all_aug$p_adf[1]
    xy_comb_adf_stat <- bg_all_aug$adf[1]
    xy_comb_aug <- bg_all_aug$augmentations[1]
  } else {
    xy_comb_p_adf <- NA
    xy_comb_adf_stat < NA
    xy_comb_aug <- NA
  }
  # cointegration vector to be printedin result
  coint_vec <- paste(c(1, round(-model_summary$coefficients[1,1],4), round(-model_summary$coefficients[2,1],4)), collapse=" ")
  
  
  #  -- johansen -- 
  
  # select order of VAR 
  VAR_select <- VARselect(xy[,1:2], lag.max = 14, type = "const")
  K <- VAR_select$selection[1]

  # if order 1 perform additional test for rank, and based on that change K value (VAR order)
  if(K==1){
    suppressWarnings({
      vecm_rank <- rank.select(xy[,1:2], lag.max = 14, include = "const")
    })
    K <- vecm_rank$AIC_min[2]+1
  }

  # perform trance and eigenvalue johansen test
  johan.test.trace <- ca.jo(xy[,1:2],         
                            ecdet = "const", 
                            type = "trace",  
                            K = K) 
  
  johan.test.eigen <- ca.jo(xy[,1:2],       
                            ecdet = "const", 
                            type = "eigen",  
                            K = K) 
  # if r=0 H0 rejected and r<=1 not rejected in both one cointegration vector, else zero 
  if((johan.test.trace@cval[1,2] > johan.test.trace@teststat[1]) & 
     (johan.test.trace@cval[2,2] < johan.test.trace@teststat[2]) &
     (johan.test.eigen@cval[1,2] > johan.test.eigen@teststat[1]) &
     (johan.test.eigen@cval[2,2] < johan.test.eigen@teststat[2])
     ){
    johansen <- "one"
  } else {
    johansen <- "zero"
  }
  
  # based on E.-G. and johansen prepare info about cointegraion
  coint_info <- if(!is.na(x_d_p_adf) & 
                   !is.na(y_d_p_adf) &
                   !is.na(xy_comb_adf_stat) & 
                   (johansen == "one")
                   ) "compare combin_adf" else "not integrated"
  
  # prepare results
  if(include_dates){
    res_names <- c("first_is", "last_is", 
                   "n_obs","cc1", "cc1_p_adf", "cc1_diff_p_adf", 
                   "cc2", "cc2_p_adf", "cc2_diff_p_adf", 
                   "johansen",
                   "combin_adf", "coint_vec", "conint_info")
    
    results <- c(as.character(index(xy)[1]), as.character(index(xy)[nrow(xy)]), nrow(xy), 
                 names(xy)[1], round(x_p_adf,4), round(x_d_p_adf,4), 
                 names(xy)[2], round(y_p_adf,4), round(y_d_p_adf,4), 
                 johansen, 
                 xy_comb_adf_stat, coint_vec, coint_info)
  } else {
    res_names <- c("n_obs",
                   "cc1", "cc1_p_adf", "cc1_diff_p_adf", 
                   "cc2", "cc2_p_adf", "cc2_diff_p_adf", 
                   "johansen",
                   "combin_adf", "coint_vec", "conint_info")
    
    results <- c(nrow(xy), 
                 names(xy)[1], round(x_p_adf,4), round(x_d_p_adf,4), 
                 names(xy)[2], round(y_p_adf,4), round(y_d_p_adf,4), 
                 johansen,
                 xy_comb_adf_stat, coint_vec, coint_info)
  }
  
  names(results) <- res_names
  return(results)
}

#----------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------
# get table of cointegration based on provided table of pair of names


get_cointegration_table <- function(pairs_dt,               # 2 column df with names of currencies like: Bitcoin, XRP
                                    standardize = FALSE,    # should data be standardized before analysis
                                    in_sample = NULL,       # number of items in sample
                                    oo_sample = NULL,       # number of items out of sample
                                    data_list = NULL,       # list of dataframes with prices, if null getCryptoHistoricalPrice will try to download
                                    include_dates = TRUE,   # range of dates be incorporated in result
                                    save_as = NULL,         # full path where to save RDS
                                    log_prices = TRUE,      # if to transform to log prices
                                    johansen = FALSE        # Engle-Grangerif AND JOHANSEN, false will do only E.G.   
                                    ){
  
  pairs_list <- list()
  results <- c()
  
  if(is.null(data_list)){
    data_list = list()
  }

  for(i in (1:nrow(pairs_dt))){
    
    # whichpari is being processed
    message(paste("Current pair is:", pairs_dt[i,1], "and", pairs_dt[i,2]))
    
    # try to download data and perform analysis
    possibleError <- tryCatch(
      {
        # names of items
        c1 <- as.character(pairs_dt[i,1])
        c2 <- as.character(pairs_dt[i,2])
      
      # if such items are not in datalist try to download
      if(!c1 %in% names(data_list)){
        print(paste(c1,"pobierane"))
        c1_ <- getCryptoHistoricalPrice(c1)
        # add data to datalist if wasn't got earlier
        data_list[[c1]] <-  c1_
      }

      if(!c2 %in% names(data_list)){
        print(paste(c2,"pobierane"))
        c2_ <- getCryptoHistoricalPrice(c2)
        # add data to datalist if wasn't got earlier
        data_list[[c2]] <-  c2_
      }
        
        # get selected items from data list
        c1_ <- data_list[[c1]]
        c2_ <- data_list[[c2]]
        
        # merge then to one xts
        c1_c2 <- merge_crypto_pair(c1_, 
                                   c2_, 
                                   c1, 
                                   c2, 
                                   xts = T)
        # if in sample number given, strip the data acorrdingly
        if(!is.null(in_sample)){
          pairs_list[[paste0(c1,"_",c2)]] <- tail(c1_c2, in_sample + oo_sample)
          c1_c2 <- head(tail(c1_c2, in_sample + oo_sample), in_sample)
        }
        
        # if chosen standardize
        if(standardize){
          c1_c2 <- scale(c1_c2)
        }
        
        # perform cointegration analysis
        if(johansen){
          if(include_dates){
            if(log_prices){
              # based on log prices, dates to be included in output, johansen to be performed
              coint_info <- find_cointegration2(c1_c2, aug = 10, include_dates = T, log_prices = log_prices)
            } else {
              # based on raw prices, with dates in output and with johansen test
              coint_info <- find_cointegration2(c1_c2, aug = 10, include_dates = T, log_prices = log_prices)
            }
          } else {
            if(log_prices){
              # based on log prices, with dates in output, with johansen
              coint_info <- find_cointegration2(c1_c2, aug = 10, log_prices = log_prices)
            } else {
              # based on on raw prices, with dates in output, with johansen
              coint_info <- find_cointegration2(c1_c2, aug = 10, log_prices = log_prices)
            }
            } 
          } else {
            if(include_dates){
              if(log_prices){
                # without johansen, with dates in output, on logprices
                coint_info <- find_cointegration(c1_c2, aug = 10, include_dates = T, log_prices = log_prices)
              } else {
                # without johansen, with dates in output, on raw prices
                coint_info <- find_cointegration(c1_c2, aug = 10, include_dates = T, log_prices = log_prices)
              }
            } else {
              if(log_prices){
                # without johansen, without dates in output, on logprices
                coint_info <- find_cointegration(c1_c2, aug = 10, log_prices = log_prices)
              } else {
                # without johansen, without dates in output, on raw prices
                coint_info <- find_cointegration(c1_c2, aug = 10, log_prices = log_prices)
              }
            }
          } 
      }, error=function(e) e
    )
    
    # if there was error save in results just names of items, while the rest is NA, and print message which items failed to be downloaded
    if(inherits(possibleError, "error")){

      c1 <- as.character(pairs_dt[i,1])
      c2 <- as.character(pairs_dt[i,2])

      message(paste("c1:", c1 %in% names(data_list), "c2:", c2 %in% names(data_list)))

      if(include_dates){
        coint_info <- if(johansen) c(NA, NA, NA, c1, NA, NA, c2, NA, NA, NA, NA, NA, "data not available") else
          c(NA, NA, NA, c1, NA, NA, c2, NA, NA, NA, NA, "data not available")
      } else {
        coint_info <- if(johansen) c(NA, c1, NA, NA, c2, NA, NA, NA, NA, NA, "data not available") else
          c(NA, NA, NA, c1, NA, NA, c2, NA, NA, NA, NA, "data not available")
      }
    }

    # bind results to one matrix    
      # if(!is.null(coint_info)){
        results <- rbind(results, coint_info)
      # }
    
    if(!is.null(save_as)){
      saveRDS(results, save_as)
    }
  }
  
  # transform to data frame
  results <- as.data.frame(results)
  
  return_list <- list()
  # first elemet of result is dataframe with cointegration results
  return_list[['results']] <- results
  
  # second item is list of rad downloaded data
  return_list[['raw_data']] <- data_list
  
  # third item is list of datarames of combiend closing prices
  return_list[['pairs_data']] <- pairs_list

  return(return_list)
}

#----------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------
# prepare data from scraped data in list, by providing table of cointegration and number of row of the pair of currencies

getDifferencesXTS <- function(coint_table,          # table with results of cointegration from get_cointegration_table
                              n_table,              # which pair from table to take
                              n_obs_is,             # number of in sample observations
                              n_obs_ooc,            # number of out of sample observations
                              clipped = NULL,       # use data from 3rd element of get_cointegration_table output (merged pairs in one df)
                              crypto_list = NULL,   # use data from 1st element of get_cointegration_table output (list of df, each surr separately)
                              log_prices = FALSE    # if log prices to be outputed
                            ){
  
  c1 <- as.character(coint_table$cc1[n_table])
  c2 <- as.character(coint_table$cc2[n_table])

  if(!is.null(crypto_list)){
    # merge raw data
    crypto_pair <- merge_crypto_pair(crypto_list[[c1]], 
                                     crypto_list[[c2]], 
                                     c1, 
                                     c2, 
                                     xts = T)
  } else {
    crypto_pair <- clipped[[paste0(c1, "_", c2)]]
  }

  if(log_prices){
    
    crypto_pair[,c1] <- log(crypto_pair[,c1])
    crypto_pair[,c2] <- log(crypto_pair[,c2])
    
    # change names to indicate log
    names(crypto_pair)[match(names(crypto_pair),c(c1,c2))] <- c(paste0("log_",c1), paste0("log_",c2))
    c1 <- paste0("log_",c1)
    c2 <- paste0("log_",c2)
  }
  
  c1d <- paste0("diff_",c1)
  c2d <- paste0("diff_",c2)

  # subset
  crypto_pair <- tail(crypto_pair, n_obs_is+n_obs_ooc)

  # create differenced data
  crypto_pair <- as.data.frame(crypto_pair)
  crypto_pair$Date <- as.Date(rownames(crypto_pair), format="%Y-%m-%d")
  crypto_pair[,c1d] <- diff.xts(crypto_pair[,c1])
  crypto_pair[,c2d] <- diff.xts(crypto_pair[,c2])
  crypto_pair <- xts(crypto_pair[,-3], order.by = crypto_pair$Date)

  data <- list(in_smpl = head(crypto_pair, n_obs_is), oo_smpl = tail(crypto_pair, n_obs_ooc))
  return(data)
}

#----------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------
# get plot of a pair of currencies


get_pair_plot <- function(x,                    # name of 1st item
                          y,                    # name of 2nd item
                          data_list = NULL,     # list of data frames with data
                          n_last = NULL,        # how many last items to plot
                          log_price = FALSE,    # should log prices be used
                          standardize = TRUE,   # if data on plot to be standardized
                          ggplot = FALSE        # if false, base plot will generate plots
                          ){
  
  if(is.character(x) & is.character(y) & !is.null(data_list)){
    x_df <- data_list[[x]]
    y_df <- data_list[[y]]
  }
  
  # if ggplot prepare data for ggplot (needs to be long data frame)
  if(ggplot){
    
    xy <- merge_crypto_pair(x_df, y_df,  xts = F)
    
    if(!is.null(n_last)){
      xy <- tail(xy, n_last)
    }
    names(xy) <- c("Date", x, y)
    
    if(log_prices){
      xy[,2] <- log(xy[,2])
      xy[,3] <- log(xy[,3])
    }
    
    if(standardize){
      xy[,2:3] <- scale(xy[,2:3])
    }
    
    xy_long <- xy %>%
      gather(key = "variable", value = "value", -Date)

    gg_plot <- ggplot(xy_long, aes(x = Date, y = value)) +
      geom_line(aes(color = variable), size = 1) +
      scale_color_manual(values = c("#00AFBB", "#E7B800")) +
      theme_minimal() +
      ggtitle(paste(unique(xy_long$variable)[1],unique(xy_long$variable)[2], sep="-"))
    
    return(list(data = xy, plot = gg_plot))
    
  } else {
    # else prepare data for baseplot plotting
    xy <- merge_crypto_pair(x_df, y_df, x, y,  xts = T)
    
    if(!is.null(n_last)){
      xy <- tail(xy, n_last)
    }
    names(xy) <- c(x, y)
    
    if(standardize){
      xy <- scale(xy)
    }
    
    xy <- xy[, 1:2]
    # print(class(xy))
    print(plot(xy,
               col = c("black", "blue"),
               major.ticks = "years",
               grid.ticks.on = "years",
               grid.ticks.lty = 3,
               legend.loc = "topleft"))
  }
}


#----------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------

# plot based on dataframe, not names and list of dataframes
get_pair_plot2 <- function(data,                        # dataframe or xts, 2 first columsn will be ploted
                          # log_price = F, 
                          standardize = FALSE,          # if data on plot to be standardized
                          ggplot = FALSE,               # if false, base plot will generate plots, else ggplot
                          colors = c("black", "blue")   # vector colors of itmes on the plot
                          ){
  

  if(ggplot){
    
    # if xts transform to dataframe
    if(is.xts(data)){
      
      # get only data
      data <- data[,1:2]
      data <- as.data.frame(data)
      
      # add date column
      data$Date <- as.Date(rownames(data), format="%Y-%m-%d")
    }
    
    if(standardize){
      data[,1:2] <- scale(data[,1:2])
    }
    
    # print(names(data))
    # if(log_price){
    #   data[,2:3] <- scale(data[,2:3])
    # }
    
    data_long <- data %>%
      gather(key = "variable", value = "value", -Date)
    
    # Multiple line plot
    gg_plot <- ggplot(data_long, aes(x = Date, y = value)) +
      geom_line(aes(color = variable), size = 1) +
      scale_color_manual(values = colors) +
      theme_minimal() +
      theme(legend.position="top") +
      ggtitle(paste(unique(data_long$variable)[1], unique(data_long$variable)[2], sep="-"))
    
    print(gg_plot)
  } else {
    
    if(standardize){
      data[,1:2] <- scale(data[,1:2])
    }
    
    vars <- names(data)
    scl <- if(standardize) "standardized" else ""
    title <- paste(scl, vars[1], "and", vars[2], "prices") 
      
    data <- data[,1:2]
    print(plot(data,
               col = colors,
               major.ticks = "years",
               grid.ticks.on = "years",
               grid.ticks.lty = 3,
               legend.loc = "bottomleft",
               main = title))
  }
}

#----------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------

# prepare plots: cryptocurrencies, their differences, acf, pacf
cryptoPairPlots <- function(crypto_list,                 # list with data, but from list with names c1_c2
                            coint_table,                 # table of cointefration results
                            n_table,                     # which pair to prepare plots for
                            log_prices = TRUE,           # should first plot show log prices?
                            scale_plot = TRUE,           # if data on a plot sould be also scaled
                            plot_lags = 15,              # how many lags in ACF/PACF 
                            # colerograms = TRUE,        # should ACF/PACF be showed
                            diffPlots=TRUE,              # should plots of differenced prices/logprices be showed
                            in_sample = 365,             # how many observations in scope
                            oo_sample = 15,              # number of observations out of scope
                            ggplot = FALSE,              # should first plot be a ggplot based
                            return_data = FALSE,         # if laso to return in sample data
                            alpha = c(1, 0.5),           # transparency of first differences (they tend to be not readable)
                            colors = c("black", "blue")  # colors of items to the plot
                            ){
  
  # prapeare data with based on table and list of data
  crypto_pair <- getDifferencesXTS(coint_table = coint_table, 
                                   n_table = n_table,
                                   n_obs_is = in_sample, 
                                   n_obs_ooc = oo_sample, 
                                   clipped = crypto_list, # list of row data dataframes
                                   crypto_list = NULL,
                                   log_prices = log_prices)$in_smpl
  
  c1d <- names(crypto_pair)[3]
  c2d <- names(crypto_pair)[4]
  
  # plot of first differences
  if(diffPlots){
    
    par(mfrow = c(2, 1)) 
    get_pair_plot2(crypto_pair, ggplot = ggplot, standardize = scale_plot, colors = colors)
    print(plot(crypto_pair[,c(c1d,c2d)],
               main = paste(c1d, "and", c2d),
               col = alpha(colors, alpha),
               grid.ticks.lty = 3,
               legend.loc = "bottomleft",
               )) # c1d
    par(mfrow = c(1, 1))
  }

  # ACF PACF plots 
  c1_acf <- acf(crypto_pair[,3],
                lag.max = plot_lags,
                plot = FALSE,
                na.action = na.pass)
  c1_pacf <- pacf(crypto_pair[,3],
                  lag.max = plot_lags, 
                  plot = FALSE,
                  na.action = na.pass)
  c2_acf <- acf(crypto_pair[,4],
                lag.max = plot_lags, 
                plot = FALSE,
                na.action = na.pass)
  c2_pacf <- pacf(crypto_pair[,4],
                  lag.max = plot_lags, 
                  plot = FALSE,
                  na.action = na.pass)
  
  par(mfrow = c(2, 2))
  plot(c1_acf,
       ylim = c(-0.5, 0.5),    
       lwd = 5,              
       col = colors[1],
       main = paste(c1d, "ACF"))
  
  plot(c1_pacf,
       ylim = c(-0.5, 0.5),    
       lwd = 5,              
       col = colors[1],
       main = paste(c1d, "PACF"))
  plot(c2_acf,
       ylim = c(-0.5, 0.5),
       lwd = 5,
       col = colors[2],
       main = paste(c2d, "ACF"))
  plot(c2_pacf,
       ylim = c(-0.5, 0.5),
       lwd = 5,
       col = colors[2],
       main = paste(c2d, "PACF"))
  par(mfrow = c(1, 1)) 

  
  # conditional data to return
  if(return_data){
    return(crypto_pair)
  }
}

#----------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------
# functions to compute AIC and BIC for VAR model outside VARselect function for single model 


VARaic <- function(model){
  T_ <- model$obs
  p <- model$p
  K <- model$K
  s <- summary(model)
  aic <- log(det(s$covres)) + (2*p*K^2)/T_
  return(aic)
}

VARbic <- function(model){
  T_ <- model$obs
  p <- model$p
  K <- model$K
  s <- summary(model)
  bic <- log(det(s$covres)) + (p*K^2*log(T_))/T_
  return(bic)
}