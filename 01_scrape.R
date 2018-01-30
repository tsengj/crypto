install.packages("crypto")
library(crypto)
# Retrieve crypto market history for all coins
will_i_get_rich <- getCoins()

# Retrieve crypto market history for specific coin
will_i_get_rich_from <- getCoins("kin")

# Get list of coins and rank
rich_list <- listCoins()