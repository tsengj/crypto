#https://github.com/prouast/cryptocurrency-analysis/blob/master/analysis.R
#rm(list=ls(all=TRUE)) # Remove everything from environment

# install.packages("RSQLite")
# install.packages("ggplot2")
# install.packages("corrplot")
# install.packages("zoo")

library(ggplot2)
library(grid)
library(corrplot)
library(zoo)
library(magrittr)

### 1. Import and clean daily closing prices for each currency

### 2. Calculate overall market statistics

## Calculate market statistics
# returns: return(t) = (price(t) - price(t-1)) / price(t-1)
# logreturns: logreturn(t) = ln(price(t)/price(t-1))
# annualized volatility: sd(logreturns per x days)*sqrt(trading days=365)
# herfindahl: sum of squares of competitor market shares
market.data <- function(data) {
  dates <- sort(unique(data$date))
  cap <- sapply(dates, FUN=function(date) sum(data[data$date==date,9]))
  returns <- c(0,diff(cap)/cap[-length(cap)])
  logreturns <- c(0,log(cap[-1]/cap[-length(cap)]))
  volatility.30d <- sapply(1:length(logreturns), FUN=function(i) sd(logreturns[(max(i-30,0):i)]))*sqrt(365)
  volatility.90d <- sapply(1:length(logreturns), FUN=function(i) sd(logreturns[(max(i-90,0):i)]))*sqrt(365)
  herfindahl <- sapply(dates, FUN=function(date) sum((data[data$date==date,9]/sum(data[data$date==date,9]))^2))
  data.frame(date=dates, cap=cap, return=returns, logreturn=logreturns, volatility.30d=volatility.30d, volatility.90d=volatility.90d, herfindahl=herfindahl)
}
market <- market.data(df1)

# Plot market cap, market return, market volatility and herfindahl index
plot.market <- function(market) {
  p1 <- ggplot(market, aes(date, cap)) +
    geom_line() +
    labs(x="Date", y="Market cap", title="Overall market") +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
  p2 <- ggplot(market, aes(date, logreturn)) +
    geom_line() +
    labs(x="Date", y="Log return") +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
  p3 <- ggplot(market, aes(date, volatility.30d)) +
    geom_line() +
    labs(x="Date", y="Annualized volatility") +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
  p4 <- ggplot(market, aes(date, herfindahl)) + geom_line() + labs(x="Date", y="Herfindahl index")
  ## convert plots to gtable objects
  library(gtable)
  library(grid) # low-level grid functions are required
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  g3 <- ggplotGrob(p3)
  g4 <- ggplotGrob(p4)
  g <- rbind(g1, g2, g3, g4, size="first") # stack the plots
  g$widths <- unit.pmax(g1$widths, g2$widths, g3$widths, g4$widths) # use the largest widths
  # center the legend vertically
  g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
  grid.newpage()
  grid.draw(g)
  ggsave("Market-statistics.png", g, width=8, height=6, dpi=100, units="in")
}
plot.market(market)

### 3. Calculate individual currency statistics

# Calculate returns
df1$return <- Reduce(c,sapply(unique(df1$slug), FUN=function(x) c(0,diff(df1[df1$slug==x,]$close)/(df1[df1$slug==x,]$close)[-length(df1[df1$slug==x,]$close)])))
df1$logreturn <- Reduce(c,sapply(unique(df1$slug), FUN=function(x) c(0,log(df1[df1$slug==x,]$close[-1]/df1[df1$slug==x,]$close[-length(df1[df1$slug==x,]$close)]))))

# Calculate volatility (takes too long - do on demand in plot function)
#df1$volatility.30d <- Reduce(c,sapply(unique(df1$slug), FUN=function(x) sapply(1:length(df1[df1$slug==x,]$logreturn), FUN=function(i) sd(df1[df1$slug==x,]$logreturn[(max(i-30,0):i)]))))
#df1$volatility.90d <- Reduce(c,sapply(unique(df1$slug), FUN=function(x) sapply(1:length(df1[df1$slug==x,]$logreturn), FUN=function(i) sd(df1[df1$slug==x,]$logreturn[(max(i-90,0):i)]))))

# Plot currency cap, return and volatility
plot.currency <- function(data, slug) {
  data <- data[data$slug==slug,]
  data$volatility.30d <- sapply(1:nrow(data), FUN=function(i) sd(data$logreturn[(max(i-30,0):i)]))*sqrt(365)
  p1 <- ggplot(data, aes(date, market)) +
    geom_line() +
    labs(x="Date", y="Market cap", title=slug) +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
  p2 <- ggplot(data, aes(date, logreturn)) +
    geom_line() + labs(x="Date", y="Log return") +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
  p3 <- ggplot(data, aes(date, volatility.30d)) + geom_line() + labs(x="Date", y="Annualized volatility")
  ## convert plots to gtable objects
  library(gtable)
  library(grid) # low-level grid functions are required
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  g3 <- ggplotGrob(p3)
  g <- rbind(g1, g2, g3, size="first") # stack the plots
  g$widths <- unit.pmax(g1$widths, g2$widths, g3$widths) # use the largest widths
  # center the legend vertically
  g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
  grid.newpage()
  grid.draw(g)
  ggsave("Bitcoin-statistics.png", g, width=8, height=6, dpi=100, units="in")
}
plot.currency(df1, "bitcoin")

### 4. Comparing different currencies directly

# Plot currency cap, return and volatility for multiple currencies
plot.currencies <- function(data, slugs) {
  data <- data[data$slug %in% slugs,]
  data$volatility.30d <- Reduce(c,sapply(unique(data$slug), FUN=function(x) sapply(1:length(data[data$slug==x,]$logreturn), FUN=function(i) sd(data[data$slug==x,]$logreturn[(max(i-30,0):i)]))))*sqrt(365)
  p1 <- ggplot(data, aes(date, market, color=factor(slug))) +
    geom_line() +
    labs(x="Date", y="Market cap", title=paste(slugs, collapse=", ")) +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.title=element_blank())
  p2 <- ggplot(data, aes(date, logreturn, color=factor(slug))) +
    geom_line() +
    labs(x="Date", y="Log return") +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.title=element_blank())
  p3 <- ggplot(data, aes(date, volatility.30d, color=factor(slug))) +
    geom_line() +
    labs(x="Date", y="Annualized volatility")
  ## convert plots to gtable objects
  library(gtable)
  library(grid) # low-level grid functions are required
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  g3 <- ggplotGrob(p3)
  g <- rbind(g1, g2, g3, size="first") # stack the plots
  g$widths <- unit.pmax(g1$widths, g2$widths, g3$widths) # use the largest widths
  # center the legend vertically
  g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
  grid.newpage()
  grid.draw(g)
  ggsave("Coin-statistics.png", g, width=8, height=6, dpi=100, units="in")
}
plot.currencies(df1, c("bitcoin","ethereum", "ripple"))

# Generates a dataframe with complete daily information for a set of currencies
analysis.data <- function(currencies, data, market=NULL) {
  temp <- lapply(currencies, FUN=function(x) subset(data, slug==x))
  temp <- Reduce(function(df1, df2) merge(df1, df2, by="date"), temp)
  if (length(currencies) > 1)
    colnames(temp) <- c("date", sapply(currencies, function(slug) sapply(colnames(data)[c(1:3,5:15)], function(x) paste(x, slug, sep="_"))))
  if (!is.null(market))
    temp <- merge(temp, market, by="date")
  data.frame(temp)
}

# Plot returns against each other
plot.return.vs.return <- function(currency1, currency2, data) {
  data <- analysis.data(c(currency1, currency2), data)
  cor_ <- cor(data[[paste("logreturn_",currency1,sep="")]], data[[paste("logreturn_",currency2,sep="")]])
  p <- ggplot(data, aes_string(x=paste("logreturn_",currency1,sep=""), y=paste("logreturn_",currency2,sep="")))
  p + geom_point() +
    labs(title=paste("Returns: ",currency1," vs ",currency2," (cor = ",round(cor_, digits=4),")",sep=""), x=paste(currency1, "Return"), y=paste(currency2, "Return")) +
    theme(legend.title=element_blank())
  ggsave("Bitcoin-vs-ethereum-returns.png", width=8, height=4, dpi=100, units="in")
}
plot.return.vs.return("bitcoin", "ethereum", df1[df1$date>as.Date("2016-12-31"),])  

# Get list of coins and rank
df_list_coin <- listCoins()
top_25 <- df_list_coin[1:25,3]

# Generates a dataframe with daily returns for a set of currencies
analysis.return.data <- function(currencies, data) {
  data <- reshape(data[data$slug %in% top_25,c(1,4,15)], direction="wide", idvar="date", timevar="slug")
  col_old <- colnames(data)
  col_new <- gsub(pattern = "logreturn.",replacement = "", x  = col_old)
  colnames(data) <- col_new
  return(data)
}

# Plot the correlation matrix for top 25 currency returns
png(filename="Corrplot.png", width=800, height=700, units="px")
corrplot(cor(analysis.return.data(top_25,df1[df1$date>as.Date("2016-12-31"),])[,-1],
             use = "pairwise.complete.obs"), method="ellipse")
dev.off()

# Plot the correlation of two currencies over time
plot.corr.timeline <- function(currency1, currency2, mindays, maxdays, data) {
  data <- analysis.data(c(currency1, currency2), data)
  data$corr <- sapply(1:nrow(data), FUN=function(i) if(i<mindays) return(NA) else cor(data[max(1,i-maxdays):i,15],data[max(1,i-maxdays):i,29]))
  p <- ggplot(data, aes(date, corr))
  p + geom_line() + labs(x="Date", y="Correlation", title=paste("Correlation timeline: ", paste(c(currency1, currency2), collapse=", ")))
  ggsave("Corr-timeline.png", width=8, height=4, dpi=100, units="in")
}
plot.corr.timeline("bitcoin", "ethereum", 30, 90, df1)

### 5. Comparing currencies with overall market

# Plot return against weighted market return
plot.return.vs.market <- function(currency, data, market) {
  data <- analysis.data(currency, data, market)
  cor_ <- cor(data$logreturn.x, data$logreturn.y)
  p <- ggplot(data, aes(x=logreturn.x, y=logreturn.y))
  p + geom_point() +
    labs(title=paste("Returns: ",currency," vs Market (cor = ",round(cor_, digits=4),")",sep=""), x=paste(currency, "return"), y="Market return") +
    theme(legend.title=element_blank())
  ggsave("Ethereum-vs-market-return.png", width=8, height=4, dpi=100, units="in")
}
plot.return.vs.market("ethereum", df1[df1$date>as.Date("2016-12-31"),], market)

# Calculate betas
currency.beta <- function(currency, data, market) {
  dates <- intersect(data[data$slug==currency,]$date, market$date)
  return(cov(data[data$slug==currency & data$date %in% dates,]$logreturn,
             market[market$date %in% dates,]$logreturn)/var(market[market$date %in% dates,]$logreturn))
}
df1$beta <- sapply(df1$slug, FUN=currency.beta, df1[df1$date>as.Date("2016-12-31"),], market)

# Plot betas of top currencies against latest market cap
plot.beta.vs.mcap.num <- function(num, currencies) {
  data <- currencies %>% filter(date==max(date))
  data <- data[order(data$market, decreasing=TRUE),] # Sort
  data <- data[0:num,]
  p <- ggplot(data, aes(x=market, y=beta))
  p + geom_point() +
    scale_x_log10() +
    geom_text(aes(label=name),hjust=0, vjust=0) +
    labs(title="Beta vs Market capitalisation", x="Market capitalisation [USD] (log scale)", y="Beta") +
    theme(legend.title=element_blank())
  ggsave("Beta-vs-mcap.png", width=8, height=5, dpi=100, units="in")
}
plot.beta.vs.mcap.num(25, df1)

# Plot betas over time
plot.beta.timeline <- function(currencies, mindays, maxdays, data, market) {
  data <- data[data$slug %in% currencies,]
  dates <- intersect(data$date, market$date)
  result <- data.frame(date=as.Date(rep(dates, times=length(currencies)), origin="1970-01-01"), currency=rep(currencies,each=length(dates)))
  result$beta <- Reduce(c, sapply(currencies,
                                  function(currency) sapply(dates,
                                                            function(date) if(nrow(data[data$slug==currency & date-maxdays<data$date & data$date<=date,])<mindays) return(NA) else currency.beta(currency, data[data$slug==currency & date-maxdays<data$date & data$date<=date,], market))))
  p <- ggplot(result, aes(date, beta, color=factor(currency)))
  p + geom_line() + labs(x="Date", y="Beta", title=paste("Beta timeline: ", paste(currencies, collapse=", "))) + theme(legend.title=element_blank())
  ggsave("Beta-timeline.png", width=8, height=4, dpi=100, units="in")
}
plot.beta.timeline(c("bitcoin","ethereum","ripple"), 30, 90, df1, market)