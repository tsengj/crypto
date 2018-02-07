
library(feather)

library(ggplot2)
library(rpart)
library(rpart.plot)
library(party)
library(forecast)
library(plotly)
library(grid)
library(animation)

library(data.table)
library(forecast)

# Using regression trees for forecasting double-seasonal time series with trend

# how they can be helpful (rules, visualizations),
# they have many broad opportunities (ensemble learning improvement)
# very good forecasting accuracy performance
# poor on unexpected situations (do not handle well trend change, i.e. concept drift)

# Use feather (fast to share data) to read data.table
DT <- df1 %>% 
  filter(symbol=='ETH' &
           date>as.Date("2017-09-30")
         ) %>% 
  select(date,value = close,slug) %>% 
  mutate(date,
         date_time = date,
         weekday = weekdays(date),
         week_num = format(date, "%V")
         ) %>%
  select(date, date_time, weekday,week_num,value) %>%
  as.data.table()

# Calculate returns
DT$return <- Reduce(c,sapply(unique(DT$slug), FUN=function(x) c(0,diff(DT[DT$slug==x,]$value)/(DT[DT$slug==x,]$value)[-length(DT[DT$slug==x,]$value)])))

# store information of the type of consumer, date, weekday and period
n_date <- unique(DT[, date])
period <- 1

# store my default favourite theme to object for time series with ggplot2 and plot available data
theme_ts <- theme(panel.border = element_rect(fill = NA, 
                                              colour = "grey10"),
                  panel.background = element_blank(),
                  panel.grid.minor = element_line(colour = "grey85"),
                  panel.grid.major = element_line(colour = "grey85"),
                  panel.grid.major.x = element_line(colour = "grey85"),
                  axis.text = element_text(size = 13, face = "bold"),
                  axis.title = element_text(size = 15, face = "bold"),
                  plot.title = element_text(size = 16, face = "bold"),
                  strip.text = element_text(size = 16, face = "bold"),
                  strip.background = element_rect(colour = "black"),
                  legend.text = element_text(size = 15),
                  legend.title = element_text(size = 16, face = "bold"),
                  legend.background = element_rect(fill = "white"),
                  legend.key = element_rect(fill = "white"))

#
ggplot(DT, aes(date_time, value)) +
  geom_line() +
  theme_ts

# split data on train and test part, pick aggregated Light Industrial consumers as example case.
data_train <- DT[date %in% n_date[1:395]]
data_test <- DT[date %in% n_date[396:402]]

# we can see some trend incresing over time, maybe air conditioning is more used when gets more hotter in summer.

# Very useful method for visualization and analysis of time series is also STL decomposition.
# STL is based on LOESS regression, and it decomposes time series to three parts: seasonal, trend and remainder.
# We will use results from the STL decomposition to model our data as well. Let's look on results:

# using stl() from stats package, before that we must define weekly seasonality to our time series object
data_ts <- ts(data_train$value, freq = period * 7)
decomp_ts <- stl(data_ts, s.window = "periodic", robust = TRUE)$time.series

decomp_stl <- data.table(Load = c(data_train$value, as.numeric(decomp_ts)),
                         Date = rep(data_train[,date_time], ncol(decomp_ts)+1),
                         Type = factor(rep(c("original data", colnames(decomp_ts)),
                                           each = nrow(decomp_ts)),
                                       levels = c("original data", colnames(decomp_ts))))

ggplot(decomp_stl, aes(x = Date, y = Load)) +
  geom_line() + 
  facet_grid(Type ~ ., scales = "free_y", switch = "y") +
  labs(x = "Date", y = NULL,
       title = "Time Series Decomposition by STL") +
  theme_ts

# as was expecxted from previous picture, we can see that there is "slight" trend increasing (by 1000 kW so slightly big ;)).
# Remainder part (noise) is very fluctuate and not seems like classical white noise (we obviously missing additional informations like weather and other unexpected situations).

# Constructing features for double seasonal time series regression tree model with only historical values of time series no additional features like weather etc.
# Classical way to add seasonal features to model are vectors of form: (1, \dots, DailyPeriod, 1, ..., DailyPeriod,...)
# or (1, \dots, WeeklyPeriod, 1, ..., WeeklyPeriod,...)
# Better way to model seasonal variables is to tranform it to Fourier terms. It is more periodical and effective to trees models.

# Another great feature (most of the times most powerful) is lags of original time series. Lag by one day, one week..etc.
# Lag can be preprocessed by removing noise or trend. STL Decompostion method of time series.

# Regression trees cant predict trend becasuse they logically make rules and predict future values only by rules made by training set.
# Therefore original time series that inputs to regression tree as dependent variable must be detrended.
# Aquired trend part then can be forecasted by for example ARIMA model.

# We will make examples and experiments with two known simple regression tree methods RPART (i.e. CART) and CTREE.
# At the end we will compare both and concludes with future work and possible improvements (enhacements).

## Constructing features to model ----
# Double Seasonal Fourier terms, done by forecast package.
# first create multiple seasonal object by function msts

data_msts <- msts(data_train$value, seasonal.periods = c(period, period*7))

# now use fourier signals by fourier function from forecast package, using two conditions for number of K terms.
# Set K for example just to 1
K <- 2
fuur <- fourier(data_msts, K = c(K, K))

# it mades 2 pairs of daily seasonal signals and 2 pairs of weekly seasonal signal.
# if we compare it with approach described in previous post, so simple periodic vectors, it looks like this:

Daily <- rep(1:period, 21)
Weekly <- data_train[, week_num]

data_fuur_simple <- data.table(value = c(scale(Daily), fuur[,2], scale(Weekly), fuur[,6]),
                               date = rep(data_train$date_time, 4),
                               method = rep(c("simple-daily", "four-daily", "simple-weekly", "four-weekly"), each = nrow(fuur)),
                               type = rep(c("Daily Seasonality", "Weekly Seasonality"), each = nrow(fuur)*2))

ggplot(data_fuur_simple, aes(x = date, y = value, color = method)) +
  geom_line(size = 1.2, alpha = 0.7) + 
  facet_grid(type ~ ., scales = "free_y", switch = "y") +
  labs(x = "Date", y = NULL,
       title = "Features Comparison") +
  theme_ts

# more contention (closeness) between ending of day or week, which is more natural.

# detrending and forecasting trend part. With auto.arima.
trend_part <- ts(decomp_ts[,2])

trend_fit <- auto.arima(trend_part)
trend_for <- forecast(trend_fit, period)$mean

trend_data <- data.table(Load = c(decomp_ts[,2], trend_for),
                         Time = c(data_train$date_time, data_test$date_time),
                         Type = c(rep("Real", nrow(data_train)), rep("Forecast", nrow(data_test))))

ggplot(trend_data, aes(Time, Load, color = Type)) +
  geom_line(size = 1.2) +
  labs(title = paste(trend_fit)) +
  theme_ts

# make final features to model: lag and fouriers
# lag of one week and just take seasonal part from STL decomposition

N <- nrow(data_train)
window <- (N / period) - 1

new_load <- rowSums(decomp_ts[, c(1,3)])
lag_seas <- decomp_ts[1:(period*window), 1]

matrix_train <- data.table(Load = tail(new_load, window*period),
                           fuur[(period + 1):N,],
                           Lag = lag_seas)

#- RPART (CART) ----

# RPART (CART)
# perform an exhaustive search over all possible splits maximizing an information measure
# of node impurity selecting the covariate showing the best split.

# Try default settings of method rpart:
tree_1 <- rpart(Load ~ ., data = matrix_train)

# show variable importance
tree_1$variable.importance # Varibale Importance
printcp(tree_1) # Table of nodes and errors
# summary(tree_1) # Detailed summary of created nodes
tree_1$cptable[dim(tree_1$cptable)[1], "nsplit"] # Number of splits

# rsq.rpart(tree_1)
# You can use default visualization of tree, but we will use different
# plot(tree_1, compress = TRUE, margin = 0.1)
# text(tree_1, cex = 0.8, use.n = TRUE)

#  plot results with fancy rpart.plot function from same named package:
rpart.plot(tree_1, digits = 2, 
           box.palette = viridis::viridis(10, option = "D", begin = 0.85, end = 0), 
           shadow.col = "grey65", col = "grey99")

# rpart.plot::prp(tree_1, cex = 1.2) #rpart.plot
# rattle::fancyRpartPlot(tree_1, palettes = c("GnBu"), cex = 0.8, sub = NULL, gap = 3)

# plot fitted values to see resutls.
datas <- data.table(Load = c(matrix_train$Load,
                             predict(tree_1)),
                    Time = rep(1:length(matrix_train$Load), 2),
                    Type = rep(c("Real", "RPART"), each = length(matrix_train$Load)))

# Let's plot it
ggplot(datas, aes(Time, Load, color = Type)) +
  geom_line(size = 0.8, alpha = 0.75) +
  labs(y = "Detrended load", title = "Fitted values from RPART tree") +
  theme_ts

# Whuups. It's really simple (rectangular) and not really accurate.
# Key to achieve better results and have more accurate fit is to set manualy control hyperparameters of rpart.
# check ?rpart.control
# Hack is to change cp (complexity) parameter to very low to produce more splits (nodes)
# Set also minsplit to 2 and set maxdepth higher (maximum is 30).

tree_2 <- rpart(Load ~ ., data = matrix_train,
                control = rpart.control(minsplit = 2,
                                        maxdepth = 30,
                                        cp = 0.000001))

# make simple plot, to see depth of tree...
plot(tree_2, compress = TRUE)
# thats little bit impressive diffeernce than previous one, isnt it?
# chceck number of splits
tree_2$cptable[dim(tree_2$cptable)[1], "nsplit"] # Number of splits
# tree_2$variable.importance

# Lets plot fitted values now:
datas <- data.table(Load = c(matrix_train$Load,
                             predict(tree_2)),
                    Time = rep(1:length(matrix_train$Load), 2),
                    Type = rep(c("Real", "RPART"), each = length(matrix_train$Load)))

ggplot(datas, aes(Time, Load, color = Type)) +
  geom_line(size = 0.8, alpha = 0.75) +
  labs(y = "Detrended load", title = "Fitted values from RPART") +
  theme_ts

# much better, but obviously can be little bit overfitted now.

# adding together everything till now, forecast load one week ahead.
# create testing data matrix
test_lag <- decomp_ts[((period*window)+1):N, 1]
fuur_test <- fourier(data_msts, K = c(K, K), h = period)

matrix_test <- data.table(fuur_test,
                          Lag = test_lag)

# Predict detrended part with tree_2 model + add trend part of time series forecasted by ARIMA
for_rpart <- predict(tree_2, matrix_test) + trend_for

# Let's plot the results and compare it with real values from data_test
data_for <- data.table(Load = c(data_train$value, data_test$value, for_rpart),
                       Date = c(data_train$date_time, rep(data_test$date_time, 2)),
                       Type = c(rep("Train data", nrow(data_train)),
                                rep("Test data", nrow(data_test)),
                                rep("Forecast", nrow(data_test))))

ggplot(data_for, aes(Date, Load, color = Type)) +
  geom_line(size = 0.8, alpha = 0.75) +
  labs(title = "Forecast from RPART") +
  theme_ts

