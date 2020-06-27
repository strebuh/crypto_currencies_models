getCryptoHistoricalPrice <- function(x){
  # this function scraps the OHLC historical crypto prices from www.coinmarketcap.com
  library(tidyverse)
  paste0("https://coinmarketcap.com/currencies/",
         x,
         "/historical-data/?start=20130428&end=21000101") %>%
    xml2::read_html() %>%
    rvest::html_table(.) %>%
    .[[3]] %>% 
    as_tibble() %>%
    rename(Open  = `Open*`,
           Close = `Close**`,
           MarketCap = `Market Cap`) %>%
    mutate(Date = as.Date(Date, format = "%b %d, %Y")) %>%
    mutate_if(is.character, function(x) as.numeric(gsub(",", "", x))) %>%
    arrange(Date) %>%
    return()
}

