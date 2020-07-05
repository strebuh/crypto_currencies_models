library(tidyverse)
library(xts)
library(fUnitRoots)
library(lmtest)
library(vars)
library(tsDyn)

source("functions/getCryptoHistoricalPrice.R")
source("functions/function_testdf2.R")

#----------------------------------------------------------------------------------------------------------------------------
#merge to xts into one df
merge_crypto_pair <- function(x, y, x_, y_, xts = T){
  
  # x[,"Date"] <- as.Date(x[,"Date"], format="%Y-%m-%d")
  # y[,"Date"] <- as.Date(y[,"Date"], format="%Y-%m-%d")
  # x_ <- getCryptoHistoricalPrice(x)
  # message(paste(x," has been downloaded."))
  # 
  # y_ <- getCryptoHistoricalPrice(y)
  # message(paste(y," has been downloaded."))
  
  
  xy <- merge(x[,c("Date", "Close")], y[,c("Date", "Close")], by="Date")
  
  if(xts){
    xy <- xts(xy[,-c(1)], order.by = xy[,"Date"])
    names(xy) <- c(x_, y_)
  }
  return(xy)
}

#----------------------------------------------------------------------------------------------------------------------------
# do cointegration checks for given pair of currencies
find_cointegration <- function(xy, aug = 10, include_dates = T, log_prices = T){

  if(log_prices){
    xy[,1] <- log(xy[,1])
    xy[,2] <- log(xy[,2])
  }

  x <- testdf(variable = xy[,1],
              max.augmentations = aug, max.order = aug)

  bg_test_cols <- names(x)[(ncol(x) - aug+1):ncol(x)]
  x_p_adf <-x[rowSums(x[bg_test_cols] > 0.05) == aug, "p_adf"][1]

  if(x_p_adf > 0.05){
    x_d <- testdf(variable = diff.xts(xy[,1]),
                  max.augmentations = aug, max.order = aug)
    x_d_p_adf <- x_d[rowSums(x_d[bg_test_cols] > 0.05) == aug, "p_adf"][1]

  } else {
    x_d_p_adf <- NA
  }

  y <- testdf(variable = xy[,2],
              max.augmentations = aug,  max.order = aug)
  # integ_ord_b <-y[b$p_adf < 0.05 &y$p_bg > 0.05, "augmentations"]

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

  # linear combination
  model.coint <- lm(combination_formula,
                    data = xy)

  model_summary <- summary(model.coint)

  xy_comb_test <- testdf(variable = residuals(model.coint),
                         max.augmentations = aug, max.order = aug)


  bg_all_aug <- xy_comb_test[rowSums(xy_comb_test[bg_test_cols] > 0.05) == aug,]

  if(dim(bg_all_aug)[1] != 0){
    xy_comb_p_adf <- bg_all_aug$p_adf[1]
    xy_comb_adf_stat <- bg_all_aug$adf[1]
    xy_comb_aug <- bg_all_aug$augmentations[1]
  } else {
    xy_comb_p_adf <- NA
    xy_comb_adf_stat < NA
    xy_comb_aug <- NA
  }

  coint_vec <- paste(c(1, round(-model_summary$coefficients[1,1],4), round(-model_summary$coefficients[2,1],4)), collapse=" ")

  coint_info <- if(!is.na(x_d_p_adf) & !is.na(y_d_p_adf) & !is.na(xy_comb_adf_stat)) "compare combin_adf" else "not integrated"

  if(include_dates){
    res_names <- c("first_is", "last_is", "n_obs","cc1", "cc1_p_adf", "cc1_diff_p_adf", "cc2", "cc2_p_adf", "cc2_diff_p_adf", "combin_adf", "coint_vec", "conint_info")
    results <- c(as.character(index(xy)[1]), as.character(index(xy)[nrow(xy)]), nrow(xy), names(xy)[1], round(x_p_adf,4), round(x_d_p_adf,4), names(xy)[2], round(y_p_adf,4), round(y_d_p_adf,4),
                 xy_comb_adf_stat, coint_vec, coint_info)
  } else {
    res_names <- c("n_obs","cc1", "cc1_p_adf", "cc1_diff_p_adf", "cc2", "cc2_p_adf", "cc2_diff_p_adf", "combin_adf", "coint_vec", "conint_info")
    results <- c(nrow(xy), names(xy)[1], round(x_p_adf,4), round(x_d_p_adf,4), names(xy)[2], round(y_p_adf,4), round(y_d_p_adf,4),
                 xy_comb_adf_stat, coint_vec, coint_info)
  }

  names(results) <- res_names
  return(results)
}

find_cointegration2 <- function(xy, aug = 10, include_dates = T, log_prices = T){
  
  if(log_prices){
    xy[,1] <- log(xy[,1])
    xy[,2] <- log(xy[,2])
  }
  x <- testdf(variable = xy[,1],
              max.augmentations = aug, max.order = aug)
  bg_test_cols <- names(x)[(ncol(x) - aug+1):ncol(x)]
  x_p_adf <-x[rowSums(x[bg_test_cols] > 0.05) == aug, "p_adf"][1] 
  
  if(x_p_adf > 0.05){
    x_d <- testdf(variable = diff.xts(xy[,1]),
                  max.augmentations = aug, max.order = aug)
    x_d_p_adf <- x_d[rowSums(x_d[bg_test_cols] > 0.05) == aug, "p_adf"][1] 
    
  } else {
    x_d_p_adf <- NA
  }
  
  y <- testdf(variable = xy[,2],
              max.augmentations = aug,  max.order = aug)
  # integ_ord_b <-y[b$p_adf < 0.05 &y$p_bg > 0.05, "augmentations"]
  
  y_p_adf <-y[rowSums(y[bg_test_cols] > 0.05) == aug, "p_adf"][1] 
  
  if(y_p_adf > 0.05){
    y_d <- testdf(variable = diff.xts(xy[,2]),
                  max.augmentations = aug, max.order = aug)
    y_d_p_adf <- y_d[rowSums(y_d[bg_test_cols] > 0.05) == aug, "p_adf"][1] 
    
  } else {
    y_d_p_adf <- NA
  }
  
  names_ <- names(xy)
  
  x <- grep("-", names_)
  for(i in x){
    names_[i] <- paste0("`", names_[i], "`")
  }
  
  combination_formula <- as.formula(paste(names_[2], names_[1], sep="~"))

    # linear combination
  model.coint <- lm(combination_formula, 
                    data = xy)
  
  model_summary <- summary(model.coint)
  
  xy_comb_test <- testdf(variable = residuals(model.coint),
                         max.augmentations = aug, max.order = aug)
  
  
  bg_all_aug <- xy_comb_test[rowSums(xy_comb_test[bg_test_cols] > 0.05) == aug,]
  
  if(dim(bg_all_aug)[1] != 0){
    xy_comb_p_adf <- bg_all_aug$p_adf[1] 
    xy_comb_adf_stat <- bg_all_aug$adf[1]
    xy_comb_aug <- bg_all_aug$augmentations[1]
  } else {
    xy_comb_p_adf <- NA
    xy_comb_adf_stat < NA
    xy_comb_aug <- NA
  } 
  
  coint_vec <- paste(c(1, round(-model_summary$coefficients[1,1],4), round(-model_summary$coefficients[2,1],4)), collapse=" ")
  
  # johansen
  VAR_select <- VARselect(xy[,1:2], lag.max = 14, type = "const")
  K <- VAR_select$selection[1]
  # print(K)
  
  if(K==1){
    suppressWarnings({
      vecm_rank <- rank.select(xy[,1:2], lag.max = 14, include = "const")
    })
    K <- vecm_rank$AIC_min[2]+1
    # print(K)
  }

  
  johan.test.trace <- ca.jo(xy[,1:2],         
                            ecdet = "const", 
                            type = "trace",  
                            K = K) 
  
  johan.test.eigen <- ca.jo(xy[,1:2],       
                            ecdet = "const", 
                            type = "eigen",  
                            K = K) 
  
  if((johan.test.trace@cval[1,2] > johan.test.trace@teststat[1]) & 
     (johan.test.trace@cval[2,2] < johan.test.trace@teststat[2]) &
     (johan.test.eigen@cval[1,2] > johan.test.eigen@teststat[1]) &
     (johan.test.eigen@cval[2,2] < johan.test.eigen@teststat[2])
     ){
    johansen <- "one"
  } else {
    johansen <- "zero"
  }
  
  coint_info <- if(!is.na(x_d_p_adf) & 
                   !is.na(y_d_p_adf) &
                   !is.na(xy_comb_adf_stat) & 
                   (johansen == "one")
                   ) "compare combin_adf" else "not integrated"
  
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
# get table of cointegration based on provided table of pair of names
get_cointegration_table <- function(pairs_dt,
                                    standardize = FALSE, 
                                    in_sample = NULL, 
                                    oo_sample = NULL, 
                                    data_list = NULL, 
                                    include_dates = TRUE, 
                                    save_as = NULL, 
                                    log_prices = TRUE,
                                    johansen = FALSE
                                    ){
  
  pairs_list <- list()
  # failed_pairs <- c()
  results <- c()
  
  if(is.null(data_list)){
    data_list = list()
  }

  for(i in (1:nrow(pairs_dt))){
    
    message(paste("Current pair is:", pairs_dt[i,1], "and", pairs_dt[i,2]))
    
    possibleError <- tryCatch(
      {
        c1 <- as.character(pairs_dt[i,1])
        c2 <- as.character(pairs_dt[i,2])

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
        
        # 
        c1_ <- data_list[[c1]]
        c2_ <- data_list[[c2]]
        
        c1_c2 <- merge_crypto_pair(c1_, 
                                   c2_, 
                                   c1, 
                                   c2, 
                                   xts = T)
        if(!is.null(in_sample)){
          pairs_list[[paste0(c1,"_",c2)]] <- tail(c1_c2, in_sample + oo_sample)
          c1_c2 <- head(tail(c1_c2, in_sample + oo_sample), in_sample)
        }
        
        if(standardize){
          c1_c2 <- scale(c1_c2)
        }
        
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
    
      # if(!is.null(coint_info)){
        results <- rbind(results, coint_info)
      # }
    
    if(!is.null(save_as)){
      saveRDS(results, save_as)
    }
  }
  
  results <- as.data.frame(results)
  
  return_list <- list()
  return_list[['results']] <- results
  return_list[['raw_data']] <- data_list
  return_list[['pairs_data']] <- pairs_list
  # return_list[['failed_pairs']] <- failed_pairs
  
  return(return_list)
}

#----------------------------------------------------------------------------------------------------------------------------
# prepare data from scraped data in list, by providing table of cointegration and number of row of the pair of currencies
getDifferencesXTS <- function(coint_table, # table with results of cointegration
                              n_table, 
                              n_obs_is, 
                              n_obs_ooc, 
                              clipped = NULL,
                              crypto_list = NULL,
                              log_prices = FALSE){
  
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
  # print(dim(crypto_pair))
  
  data <- list(in_smpl = head(crypto_pair, n_obs_is), oo_smpl = tail(crypto_pair, n_obs_ooc))
  return(data)
}


#----------------------------------------------------------------------------------------------------------------------------
# get plot of a pair of currencies
get_pair_plot <- function(x, y, data_list = NULL,  n_last = NULL, log_price = F, standardize = T, ggplot = F){
  
  if(is.character(x) & is.character(y) & !is.null(data_list)){
    x_df <- data_list[[x]]
    y_df <- data_list[[y]]
  }
  
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
    # Multiple line plot
    gg_plot <- ggplot(xy_long, aes(x = Date, y = value)) +
      geom_line(aes(color = variable), size = 1) +
      scale_color_manual(values = c("#00AFBB", "#E7B800")) +
      theme_minimal() +
      ggtitle(paste(unique(xy_long$variable)[1],unique(xy_long$variable)[2], sep="-"))
    
    return(list(data = xy, plot = gg_plot))
    
  } else {
    
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

# plot based on data
get_pair_plot2 <- function(data, # dataframe or xts
                          # log_price = F, 
                          standardize = F,
                          ggplot = F
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
      scale_color_manual(values = c("#00AFBB", "#E7B800")) +
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
               col = c("black", "blue"),
               major.ticks = "years",
               grid.ticks.on = "years",
               grid.ticks.lty = 3,
               legend.loc = "bottomleft",
               main = title))
  }
}

#----------------------------------------------------------------------------------------------------------------------------

# prepare plots: cryptocurrencies, their differences, acf, pacf
cryptoPairPlots <- function(crypto_list,             # list with data, but from list with names c1_c2
                            coint_table,             # table of cointefration results
                            n_table,                 # which pair to prepare plots for
                            log_prices = TRUE,       # should first plot show log prices?
                            scale_plot = TRUE,       # if data on a plot sould be also scaled
                            plot_lags = 15,          # how many lags in ACF/PACF 
                            colerograms = TRUE,      # should ACF/PACF be showed
                            diffPlots=TRUE,          # should plots of differenced prices/logprices be showed
                            in_sample = 365,         # how many observations in scope
                            oo_sample = 15,          # number of observations out of scope
                            ggplot = FALSE,          # should first plot be a ggplot based
                            return_data = FALSE      # if laso to return in sample data
                            ){
  
  # prapeare data with based on table and list of data
  crypto_pair <- getDifferencesXTS(coint_table = coint_table, 
                                   n_table = n_table,
                                   n_obs_is = in_sample, 
                                   n_obs_ooc = oo_sample, 
                                   clipped = crypto_list, # list of row data dataframes
                                   crypto_list = NULL,
                                   log_prices = log_prices)$in_smpl
  
  # # this hashed part is when you want prepare data manually
  # c1 <- as.character(coint_table$cc1[n_table])
  # c2 <- as.character(coint_table$cc2[n_table])
  # 
  # # merge raw data
  # crypto_pair <- merge_crypto_pair(crypto_list[[c1]],
  #                                  crypto_list[[c2]],
  #                                  c1,
  #                                  c2,
  #                                  xts = T)
  # if(log_prices){
  #   
  #   crypto_pair[,c1] <- log(crypto_pair[,c1])
  #   crypto_pair[,c2] <- log(crypto_pair[,c2])
  #   
  #   # change names to indicate log
  #   names(crypto_pair)[match(names(crypto_pair),c(c1,c2))] <- c(paste0("log_",c1), paste0("log_",c2))
  #   c1 <- paste0("log_",c1)
  #   c2 <- paste0("log_",c2)
  # }
  # 
  # c1d <- paste0("d_",c1)
  # c2d <- paste0("d_",c2)
  # 
  # # subset
  # crypto_pair <- head(tail(crypto_pair,in_sample + oo_sample), in_sample)
  
  # get plot of prices/log prices
  get_pair_plot2(crypto_pair, ggplot = ggplot, standardize = scale_plot)
  
  c1d <- names(crypto_pair)[3]
  c2d <- names(crypto_pair)[4]
  
  # plot of differences
  if(diffPlots){
    par(mfrow = c(2, 1)) 
    print(plot(crypto_pair[,c1d], xaxt = "n", main = c1d)) # c1d
    print(plot(crypto_pair[,c2d], main = c2d)) # c2d
    par(mfrow = c(1, 1))
  }

  # plot of colerograms
  if(colerograms){
    for(i in names(crypto_pair)[3:4]){ # c(c1d, c2d)
      title <- paste("ACF and PACF of", i)
      par(mfrow = c(2, 1)) 
      acf(crypto_pair[,i],
          lag.max = plot_lags, 
          ylim = c(-0.5, 0.5),    
          lwd = 5,              
          col = "dark green",
          na.action = na.pass,
          main = title)   
      pacf(crypto_pair[,i], 
           lag.max = plot_lags, 
           ylim = c(-0.5, 0.5),
           lwd = 5, col = "dark green",
           na.action = na.pass,
           main = ""
           ) 
      par(mfrow = c(1, 1))
    }
    par(mfrow = c(1, 1))
  }
  # conditional data to return
  if(return_data){
    return(crypto_pair)
  }
}


#----------------------------------------------------------------------------------------------------------------------------
# functions to compute AIC and BIC for VAR model outside VARselect function for single model 
# https://stackoverflow.com/questions/46174383/vars-package-of-r-aic-after-restrict
# http://www.phdeconomics.sssup.it/documents/Lesson18.pdf | VARselect {vars} documentation
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