library(rvest)
library(data.table)
library(stringi)

# ---------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------
#  get list of all top 100 crypto


# scrape names of cryptocurrencies
general_info = lapply(1:3, function(page){
  page_content <- xml2::read_html(paste0('https://coinmarketcap.com/?page=', page))
  table <- page_content %>%
    html_nodes(xpath = '//*[@id="__next"]/div/div[1]/div[2]/div/div[1]/div[5]/table[1]') %>%
    html_table() %>% .[[1]] 
})
general_info = rbindlist(general_info)
general_info[, c("V1", "#", 
                 "Name", 
                 "Price",
                 "24h %",
                 "7d %", 
                 "Market Cap", 
                 "Volume(24h)",
                 "Circulating Supply", 
                 "Last 7 Days", "V2"
                 ) := .(
                   NULL, NULL,
                   stri_replace(Name, "", regex = "(\\d+\\w+Buy)$"),
                   stri_replace_all(`Price`, "", regex = "\\$|,"),
                   stri_replace(`24h %`, "", fixed = "%"),
                   stri_replace(`7d %`, "", fixed = "%"),
                   stri_replace_all(`Market Cap`, "", regex = "\\$\\d+(\\.\\d+\\w\\$)*|,"),
                   stri_replace_all(stri_extract(`Volume(24h)`, regex = "(\\$)|(\\s\\w{3,})$"), "", regex = "\\$|,"),
                   stri_replace_all(`Circulating Supply`, "", regex = "(\\s\\w{3,}$)|,"),
                   NULL, NULL)]

# general_info = general_info[complete.cases(general_info),]


saveRDS(general_info, "./data/top100_crypto.RDS")
general_info <- readRDS("./data/top100_crypto.RDS")

# create dataframe with combinations of all currencies
all_combinations <- as.data.frame(t(combn(general_info$Name, 2)), stringsAsFactors = F)
dim(all_combinations) # [1] 44850     2

# save
saveRDS(all_combinations, "./data/all_combinations.RDS")
