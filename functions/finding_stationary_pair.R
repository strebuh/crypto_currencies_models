library(tidyverse)
library(xts)


# source("functions/getCryptoHistoricalPrice.R")
source("functions/function_testdf2.R")

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

# get plot of a pair of currencies
get_pair_plot <- function(x, y, data_list = NULL,  n_last = NULL, standardize = T, ggplot = F){
  
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
    print(class(xy))
    print(plot(xy,
               col = c("black", "blue"),
               major.ticks = "years",
               grid.ticks.on = "years",
               grid.ticks.lty = 3,
               legend.loc = "topleft"))
  }
}

# do cointegration checks for given pair of currencies
find_cointegration <- function(xy, aug = 10){
  
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
    y_d_p_adf <-y_d[rowSums(y_d[bg_test_cols] > 0.05) == aug, "p_adf"][1] 
    
  } else {
    y_d_p_adf <- NA
  }
  
  names_ <- names(xy)
  combination_fornula <- as.formula(paste(names_[2], names_[1], sep="~"))
  
  # linear combination
  model.coint <- lm(combination_fornula, 
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
  
  # names(xy)
  res_names <- c("n_obs","cc1", "cc1_p_adf", "cc1_diff_p_adf", "cc2", "cc2_p_adf", "cc2_diff_p_adf", "combin_adf", "coint_vec", "conint_info")
  
  results <- c(nrow(xy), names(xy)[1], round(x_p_adf,4), round(x_d_p_adf,4), names(xy)[2], round(y_p_adf,4), round(y_d_p_adf,4), 
               xy_comb_adf_stat, coint_vec, coint_info)
  names(results) <- res_names
  
  return(results)
}


# get table of cointegration based on provided table of pair of names
get_cointegration_table <- function(pairs_dt, standardize = FALSE, max_obs = NULL, data_list = NULL, save_as = NULL){
  
  if(is.null(data_list)){
    data_list = list()
  }
  
  # failed_pairs <- c()
  results <- c()
  
  for(i in (1:nrow(pairs_dt))){
    
    message(paste("Current pair is:",pairs_dt[i,1], "and", pairs_dt[i,2]))
    
    possibleError <- tryCatch(
      
      {
      if(!pairs_dt[i,1] %in% names(data_list)){
        c1_ <- getCryptoHistoricalPrice(pairs_dt[i,1])
        data_list[[pairs_dt[i,1]]] <-  c1_
      }
      
      if(!pairs_dt[i,2] %in% names(data_list)){
        c2_ <- getCryptoHistoricalPrice(pairs_dt[i,2])
        data_list[[pairs_dt[i,2]]] <-  c2_
      }

        c1 <- data_list[[pairs_dt[i,1]]]
        c2 <- data_list[[pairs_dt[i,2]]]
        
        c1_c2 <- merge_crypto_pair(c1, c2, as.character(pairs_dt[i,1]), as.character(pairs_dt[i,2]), xts = T)
        
        if(!is.null(max_obs)){
          c1_c2 <- tail(c1_c2, max_obs)
        }
        
        if(standardize){
          c1_c2 <- scale(c1_c2)
        }
        
        coint_info <- find_cointegration(c1_c2, aug = 10)        
        
      }, error=function(e) e
      )
      
    
    if(inherits(possibleError, "error")){
      coint_info <- c(NA, pairs_dt[i,1], NA, NA, pairs_dt[i,2], NA, NA, NA, NA, "data not available")
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
  return_list[['scraped_data']] <- data_list
  # return_list[['failed_pairs']] <- failed_pairs
  
  return(return_list)
  
}

# prepare plots: cryptocurrencies, their differences, acf, pacf
cryptoPairPlots <- function(crypto_list, cointTable, n, plot_lags = 15, colerograms = TRUE, diffPlots=TRUE, nObs = 365){
  
  c1 <- as.character(cointTable$cc1[n])
  c2 <- as.character(cointTable$cc2[n])
  
  get_pair_plot(c1, c2, data_list = crypto_list, n_last = nObs,  ggplot = F)
  
  crypto_pair <- merge_crypto_pair(tail(crypto_list[[c1]], nObs), 
                                   tail(crypto_list[[c2]], nObs), 
                                   c1, 
                                   c2, 
                                   xts = T)

  c1d <- paste0("d_",c1)
  c2d <- paste0("d_",c2)
  
  crypto_pair <- as.data.frame(crypto_pair)
  crypto_pair$Date <- as.Date(rownames(crypto_pair), format="%Y-%m-%d")
  crypto_pair[,c1d] <- diff.xts(crypto_pair[,c1])
  crypto_pair[,c2d] <- diff.xts(crypto_pair[,c2])
  crypto_pair <- xts(crypto_pair[,-3], order.by = crypto_pair$Date)

  names(crypto_pair)
  if(diffPlots){
    par(mfrow = c(2, 1)) 
    print(plot(crypto_pair[,c1d], xaxt = "n"))
    print(plot(crypto_pair[,c2d]))
    par(mfrow = c(1, 1))
  }
  

    if(colerograms){
    for(i in c(c1d, c2d)){
      par(mfrow = c(2, 1)) 
      acf(crypto_pair[,i],
          lag.max = plot_lags, 
          ylim = c(-0.5, 0.5),    
          lwd = 5,              
          col = "dark green",
          na.action = na.pass,
          main = i)   
      pacf(crypto_pair[,i], 
           lag.max = plot_lags, 
           ylim = c(-0.5, 0.5),
           lwd = 5, col = "dark green",
           na.action = na.pass,
           main = i) 
      par(mfrow = c(1, 1))
    }
  }
  return(crypto_pair)
}


# prepare data from scraped data in list, by providing table of cointegration and number of row of the pair of currencies
getDifferencesXTS <- function(coint_table, n_table, n_obs_is, n_obs_ooc, crypto_list){
  
  c1 <- as.character(coint_table$cc1[n_table])
  c2 <- as.character(coint_table$cc2[n_table])
  print(paste(c1, c2))
  print(class(crypto_list))
  
  crypto_pair <- merge_crypto_pair(tail(crypto_list[[c1]], n_obs_is+n_obs_ooc+3), 
                                   tail(crypto_list[[c2]], n_obs_is+n_obs_ooc+3), 
                                   c1, 
                                   c2, 
                                   xts = T)
  print(names(crypto_pair))
  c1d <- paste0("d_",c1)
  c2d <- paste0("d_",c2)
  
  crypto_pair <- as.data.frame(crypto_pair)
  crypto_pair$Date <- as.Date(rownames(crypto_pair), format="%Y-%m-%d")
  crypto_pair[,c1d] <- diff.xts(crypto_pair[,c1])
  crypto_pair[,c2d] <- diff.xts(crypto_pair[,c2])
  crypto_pair <- xts(crypto_pair[,-3], order.by = crypto_pair$Date)
  
  data <- list(in_smpl = head(crypto_pair, n_obs_is), oo_smpl = tail(crypto_pair, n_obs_ooc))
  
  return(data)
}

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