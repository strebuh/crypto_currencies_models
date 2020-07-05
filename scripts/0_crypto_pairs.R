library(rvest)
library(data.table)

#  ------------------   get list of all top 100 crypto -----------------------------
# scrape names of cryptocurrencies
coinmarketcap <- xml2::read_html("https://coinmarketcap.com/")
crypto_names <- coinmarketcap %>%
  html_nodes(".cmc-link") %>%
  html_text() 

# get onlu part that contains currencies data
crypto_names <- crypto_names[18:length(crypto_names)]

# transform vector to dataframe
top100_crypto <- as.data.frame(matrix(crypto_names, ncol = 4,  byrow = TRUE), stringsAsFactors = FALSE)

# drop lines that do not contain relevant infroamtion
top100_crypto <- top100_crypto[,-4]

# raname columns
names(top100_crypto) <- c("Name", "Price", "Capitalization")

# drop last rows, that neither contain relevant information
top100_crypto <- top100_crypto[-(101:nrow(top100_crypto)),]

# drop dollar signs and commas as thousands delimiters 
top100_crypto[,2:3] <- lapply(top100_crypto[,2:3], function(x){
  as.numeric(gsub("\\$|,", "", x))
})

# transform to lower cases and replace . to dash, and space to dash in currencies names
top100_crypto$Name <- stringr::str_to_lower(gsub("\\.", "-", top100_crypto$Name)) 
top100_crypto$Name <- gsub("\\s", "-", top100_crypto$Name)

# saveRDS(top100_crypto, "./data/top100_crypto.RDS")
top100_crypto <- readRDS("./data/top100_crypto.RDS")

# create dataframe with combinations of all currencies
all_combinations <- as.data.frame(t(combn(top100_crypto$Name, 2)), stringsAsFactors = F)
dim(all_combinations) # [1] 4950    2

# save
# saveRDS(all_combinations, "./data/all_combinations.RDS")