#install.packages("crypto")
#library(devtools)
#install_github("jessevent/crypto")
#install.packages('doSNOW')
library(crypto)
library(dplyr)

# Get list of coins and rank
df_list_coin <- listCoins()
top_25 <- df_list_coin[1:25,3]
top_25_sym <- df_list_coin[1:25,1]

# Retrieve crypto market history for all coins
df_all_coins <- getCoins()

#filter to only coins of interest
df1 <- df_all_coins %>% filter(symbol %in% top_25_sym)#c('BTC','ETH','XRP'))

# Retrieve crypto market history for specific coin
#df_select <- getCoins(coin = 'ETH')

## save this model
id =  'export/df_all_coins.rda'
save(df_all_coins, file = id)
