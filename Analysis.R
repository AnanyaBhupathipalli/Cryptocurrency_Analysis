#Setting Working Directory
setwd("D:/LTSM - 777/Project")

#Loading Libraries
library(ggplot2)
library(astsa)
library(tseries)
library(forecast)
library(fUnitRoots)
library(urca)
library(MTS)
library(dplyr)
library(corrplot)
library(caret)

#Loading Cryptocurrency data
bitcoin <- read.csv("bitcoin_price.csv")
dash <- read.csv("dash_price.csv")
ethereum <- read.csv("ethereum_price.csv")
litecoin <- read.csv("litecoin_price.csv")
monero <- read.csv("monero_price.csv")
ripple <- read.csv("ripple_price.csv")

View(bitcoin_2015)
View(ethereum)
View(dash)
View(litecoin)
View(monero)
View(ripple)

#Taking data from Aug 07 2015 to keep in sync with Ethereum data
bitcoin_2015 <- bitcoin[1:929,]
dash <- dash[1:929,]
litecoin <- litecoin[1:929,]
monero <- monero[1:929,]
ripple <- ripple[1:929,]

View(bitcoin_2015)
str(ethereum)

bitcoin_2015$Date <- as.Date(bitcoin_2015$Date, format = "%b %d, %Y")
ethereum$Date <- as.Date(ethereum$Date, format = "%b %d, %Y")
dash$Date <- as.Date(dash$Date, format = "%d-%b-%y")
litecoin$Date <- as.Date(litecoin$Date, format = "%b %d, %Y")
monero$Date <- as.Date(monero$Date, format = "%b %d, %Y")
ripple$Date <- as.Date(ripple$Date, format = "%b %d, %Y")

bitcoin_2015 <- bitcoin_2015[order(bitcoin_2015$Date),]
ethereum <- ethereum[order(ethereum$Date),]
dash <- dash[order(dash$Date),]
litecoin <- litecoin[order(litecoin$Date),]
monero <- monero[order(monero$Date),]
ripple <- ripple[order(ripple$Date),]

row.names(bitcoin_2015) <- 1:nrow(bitcoin_2015)
row.names(ethereum) <- 1:nrow(ethereum)
row.names(dash) <- 1:nrow(dash)
row.names(litecoin) <- 1:nrow(litecoin)
row.names(monero) <- 1:nrow(monero)
row.names(ripple) <- 1:nrow(ripple)

#bitcoin_ts <- ts(bitcoin_2015$Close, frequency = 365)
#plot(bitcoin_ts)

#ethereum_ts <- ts(ethereum$Close, start = 2015, end = 2018, frequency = 365)
#plot(ethereum_ts)

bitcoin_plot <- ggplot(data = bitcoin_2015, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))

#Plotting the timeseries
ggplot(data = ethereum, x = Date) + geom_line(aes(x = Date, y = Close, col = "Blue"), data = ethereum) +
  geom_line(aes(x = Date, y = Close, col = "Dark Green"), data = dash) +
  geom_line(aes(x = Date, y = Close, col = "Black"), data = litecoin) +
  geom_line(aes(x = Date, y = Close, col = "Cyan"), data = monero) +
  geom_line(aes(x = Date, y = Close, col = "Green"), data = ripple) + 
  scale_colour_manual(name = 'Cryptocurrencies', 
                      values =c('Red','Dark Green','Gold','cyan','Green'), labels = c('Ethereum','Dash','Litecoin','Monero','Ripple'))
  
#Joining the two datasets

cryptocurrency <- bitcoin_2015 %>% 
  left_join(ethereum,by=c('Date'='Date')) %>%
  left_join(litecoin,by=c('Date'='Date')) %>%
  left_join(ripple,by=c('Date'='Date')) %>%
  left_join(monero,by=c('Date'='Date')) %>%
  left_join(dash,by=c('Date'='Date'))
View(cryptocurrency)

# Changing column names
cols <- c(sapply(colnames(bitcoin), function(x) paste0(x,".bitcoin")),
          sapply(colnames(ethereum)[-1], function(x) paste0(x,".ethereum")),
          sapply(colnames(litecoin)[-1], function(x) paste0(x,".litecoin")),
          sapply(colnames(ripple)[-1], function(x) paste0(x,".ripple")),
          sapply(colnames(monero)[-1], function(x) paste0(x,".monero")),
          sapply(colnames(dash)[-1], function(x) paste0(x,".dash")))

colnames(cryptocurrency) <- cols         

###############################################################################################

#Testing for forming a co-integration pair
m1 <- ca.jo(cryptocurrency[,c(5,11,17,23,29,35)], ecdet=c("none"), type = c("trace"), spec = c("transitory")) 
summary(m1)

#As per Johansen Test, r = 0 is greater than 1pct. Hence these variables are co-integrated

# co-int series - For all the bitcoins
coIntSeries <- cryptocurrency$Close.bitcoin + 4.33*cryptocurrency$Close.ethereum + 
  23.70*cryptocurrency$Close.litecoin - 1995.25*cryptocurrency$Close.ripple - 
  19.94*cryptocurrency$Close.monero - 14.46*cryptocurrency$Close.dash

write.csv(coIntSeries,"coint.csv")
adf.test(coIntSeries, k=1)

CoInt_ts <- ts(coIntSeries, start = 2015, end = 2018, frequency = 365)
plot(CoInt_ts)
#p-value is less than 0.05, hence we reject the null hypothesis. The data is Stationary.

#Positively co-integrated models
m2 <- ca.jo(cryptocurrency[,c(5,11,17)], ecdet=c("none"), type = c("trace"), spec = c("transitory")) 
summary(m2)

###################################################################################################

acf2(diff(cryptocurrency$Close.bitcoin))
acf2(cryptocurrency$Close.dash)
acf2(cryptocurrency$Close.ethereum)
acf2(cryptocurrency$Close.litecoin)
acf2(cryptocurrency$Close.ripple)
acf2(cryptocurrency$Close.monero)

acf2(cryptocurrency$Volume.bitcoin)
acf2(cryptocurrency$Close.dash)
acf2(cryptocurrency$Close.ethereum)
acf2(cryptocurrency$Close.litecoin)
acf2(cryptocurrency$Close.ripple)
acf2(cryptocurrency$Close.monero)

#Models for Volume
sarima(cryptocurrency$Volume.bitcoin, 2,1,1)
sarima(cryptocurrency$Close.bitcoin, 4,2,11)
sarima.for(cryptocurrency$Volume.bitcoin, 2,1,1, n.ahead = 20)
sarima.for(cryptocurrency$Close.bitcoin, 4,2,11, n.ahead = 20)

sarima(cryptocurrency$Volume.ethereum, 1,0,0)
sarima.for(cryptocurrency$Volume.ethereum, 1,0,0, n.ahead = 20)

sarima(cryptocurrency$Volume.litecoin, 3,1,3)
sarima.for(cryptocurrency$Volume.litecoin, 3,1,3, n.ahead = 20)

sarima(cryptocurrency$Volume.dash, 4,1,2)
sarima.for(cryptocurrency$Volume.dash, 4,1,2, n.ahead = 20)

sarima(cryptocurrency$Volume.monero, 2,0,2)
sarima.for(cryptocurrency$Volume.monero, 2,0,2, n.ahead = 10)

sarima(cryptocurrency$Volume.ripple, 3,1,2)
sarima.for(cryptocurrency$Volume.ripple, 3,1,2, n.ahead = 10)

auto.arima(cryptocurrency$Volume.monero)

ccm(cryptocurrency[,c(5,11,17,23,29,35)])

############################################################################################################

#Forecasting for Bitcoin

train <- bitcoin_2015[-c(910:929),]
test <- bitcoin_2015[c(910:929),]

row.names(holtdf)<- 1 : nrow(holtdf)

holtt <-  holt(train$Close, type = "additive", damped = F, h=20) #holt forecast values
holtf <- forecast(holtt, h = 20)
holtdf <- as.data.frame(holtf)
plot(holtf, ylim = c(0,20000)) 

View(test)

holtfdf <- cbind(test, holtdf[,1])
accuracy(holtdf[,1], test[,5])
  ggplot() + geom_line(data = holtfdf, aes(Date, holtfdf[,5]), color = "blue") + geom_line(data = holtfdf, aes(Date, holtfdf[,8]), color = "Dark Red")

######################################
cor <- cor(cryptocurrency[,c(5,11,17,23,29,35)])
View(cor)
corrplot(cor, method = "pie")
a <- cor(cryptocurrency[,c(5,11,17,23,29,35)], method = "kendall")
corrplot(a, method = "pie")

#Regression
#connection between volatility and volume
#find volatility and 

#Finding Volatility
library(TTR)
volatility_bitcoin <- volatility(cryptocurrency$Close.bitcoin, n = 10, calc = "close", N = 260, mean0 = FALSE)
cor <- cor(volatility_bitcoin,cryptocurrency$Volume.bitcoin)

volatility_bitcoin[is.na(volatility_bitcoin)] <- 0
corrplot(cor, method = "pie")

View(volatility_bitcoin)

str(cryptocurrency)
cryptocurrency$Volume.bitcoin <- as.numeric(cryptocurrency$Volume.bitcoin)


plot(volatility_bitcoin,cryptocurrency$Volume.bitcoin)

library(tseries)
data <- get.hist.quote('VOD.L')
price <- cryptocurrency$Close.bitcoin
ret <- log(lag(price),1) - log(price)
vol <- sd(ret) * sqrt(250) * 100
vol
dev.off()

#############################################################################
#Regression

acf2(diff(cryptocurrency$Close.bitcoin))

bit_m <- lm(cryptocurrency$Close.bitcoin ~ lag(cryptocurrency$Close.bitcoin,1) + lag(cryptocurrency$Close.bitcoin,5) + lag(cryptocurrency$Close.bitcoin,10) + lag(cryptocurrency$Close.bitcoin,19) + lag(cryptocurrency$Close.bitcoin,20) + cryptocurrency$Volume.bitcoin, data = cryptocurrency)
summary(bit_m)

m <- lm(cryptocurrency$Close.bitcoin ~ cryptocurrency$Volume.bitcoin, data = cryptocurrency)
summary(m)

Date <- seq(as.Date("2018-02-21"), by = "day", length.out = 10)
price <- c(0,0,0,0,0,0,0,0,0,0)
test_new <- data.frame(Date,price)

train <- cryptocurrency[-c(910:929),]
test <- cryptocurrency[c(910:929),]


#19 and 20 seem correlated

acf2(diff(cryptocurrency$Close.dash))

dash_m <- lm(cryptocurrency$Close.dash ~ lag(cryptocurrency$Close.dash,1) + lag(cryptocurrency$Close.dash,3) + lag(cryptocurrency$Close.dash,6) + lag(cryptocurrency$Close.dash,10) + lag(cryptocurrency$Close.dash,16) + lag(cryptocurrency$Close.dash,20), data = cryptocurrency)
summary(dash_m)

dash_m1 <- lm(cryptocurrency$Close.dash ~ lag(cryptocurrency$Close.dash,1) + lag(cryptocurrency$Close.dash,3) + lag(cryptocurrency$Close.dash,10), data = cryptocurrency)
summary(dash_m1)

acf2(diff(cryptocurrency$Close.ethereum))

eth_m <- lm(cryptocurrency$Close.ethereum ~ lag(cryptocurrency$Close.ethereum,6) + lag(cryptocurrency$Close.ethereum,12) + lag(cryptocurrency$Close.ethereum,19) + lag(cryptocurrency$Close.ethereum,20), data = cryptocurrency)
summary(eth_m)

acf2(diff(cryptocurrency$Close.litecoin))

lite_m <- lm(cryptocurrency$Close.litecoin ~ lag(cryptocurrency$Close.litecoin,6) + lag(cryptocurrency$Close.litecoin,10) + lag(cryptocurrency$Close.litecoin,13) + lag(cryptocurrency$Close.litecoin,16), data = cryptocurrency)
summary(lite_m)

acf2(diff(cryptocurrency$Close.ripple))

rip_m <- lm(cryptocurrency$Close.ripple ~ lag(cryptocurrency$Close.ripple,1) + lag(cryptocurrency$Close.ripple,10) + lag(cryptocurrency$Close.ripple,15) + lag(cryptocurrency$Close.ripple,18), data = cryptocurrency)
summary(rip_m)

acf2(diff(cryptocurrency$Close.monero))

mon_m <- lm(cryptocurrency$Close.monero ~ lag(cryptocurrency$Close.monero,1) + lag(cryptocurrency$Close.monero,6) + lag(cryptocurrency$Close.monero,10) + lag(cryptocurrency$Close.monero,17) + lag(cryptocurrency$Close.monero,20), data = cryptocurrency)
summary(mon_m)

#################################################################################

close <- cryptocurrency[,c(1,5,11,17,23,29,35)]
corr <- cor(close, use = "pairwise.complete")
corrplot(corr, order="hclust", diag = FALSE, tl.col = "black", tl.cex = 0.7,
         title = "Correlation matrix (ordered by hierarchical clustering)",
         mar = c(0,1,2,0))

#MarketCap Analysis
str(cryptocurrency)
backup <- cryptocurrency
cryptocurrency$Market.Cap.bitcoin  <- gsub(",","",cryptocurrency$Market.Cap.bitcoin)
cryptocurrency$Market.Cap.ethereum  <- gsub(",","",cryptocurrency$Market.Cap.ethereum)
cryptocurrency$Market.Cap.litecoin  <- gsub(",","",cryptocurrency$Market.Cap.litecoin)
cryptocurrency$Market.Cap.ripple  <- gsub(",","",cryptocurrency$Market.Cap.ripple)
cryptocurrency$Market.Cap.dash  <- gsub(",","",cryptocurrency$Market.Cap.dash)
cryptocurrency$Market.Cap.monero  <- gsub(",","",cryptocurrency$Market.Cap.monero)

Currency <- c("bitcoin","ethereum","litecoin","ripple","monero","dash")
cryptocurrency$Market.Cap.bitcoin <- as.numeric(cryptocurrency$Market.Cap.bitcoin)
cryptocurrency$Market.Cap.ethereum <- as.numeric(cryptocurrency$Market.Cap.ethereum)
cryptocurrency$Market.Cap.litecoin <- as.numeric(cryptocurrency$Market.Cap.litecoin)
cryptocurrency$Market.Cap.ripple <- as.numeric(cryptocurrency$Market.Cap.ripple)
cryptocurrency$Market.Cap.dash <- as.numeric(cryptocurrency$Market.Cap.dash)
cryptocurrency$Market.Cap.monero <- as.numeric(cryptocurrency$Market.Cap.monero)

eth <- na.omit(cryptocurrency$Market.Cap.ethereum)

meanMarketCap <- c(mean(cryptocurrency$Market.Cap.bitcoin),mean(eth),
                   mean(cryptocurrency$Market.Cap.litecoin),mean(cryptocurrency$Market.Cap.ripple),
                   mean(cryptocurrency$Market.Cap.monero),mean(cryptocurrency$Market.Cap.dash))
meanCap <- data.frame(Currency,meanMarketCap)
meanCap <- meanCap[order(meanCap$meanMarketCap),]
write.csv(meanCap,"MeanCap.csv")

barplot(meanCap[,2], names.arg = meanCap[,1],las=2 , cex.names=0.9, col = "gold",
        main="Average Market Capital of the Cryptocurrencies")


library(xts)
rownames(close) <- close$Date
close.xts <- as.xts(close)
View(close.xts)

price10 <- as.xts(close.xts$Close.bitcoin)
plot(price10, main="Price")

cryptocurrency$Date.bitcoin


#n <- length(close$Close.bitcoin);
#ret_bit <- log(close$Close.bitcoin[-1]/close$Close.bitcoin[-n])
#ret_eth <- log(close$Close.ethereum[-1]/close$Close.ethereum[-n])
#ret_lite <- log(close$Close.litecoin[-1]/close$Close.litecoin[-n])
#ret_rip <- log(close$Close.ripple[-1]/close$Close.ripple[-n])
#ret_mon <- log(close$Close.monero[-1]/close$Close.monero[-n])
#ret_dash <- log(close$Close.dash[-1]/close$Close.dash[-n])

#vol_bit <- volatility(ret_bit,n=10,calc = "close",N= 260)
#vol_eth <- volatility(ret_eth,n=10,calc = "close",N= 260)
#vol_lite <- volatility(ret_lite,n=10,calc = "close",N= 260)
#vol_rip <- volatility(ret_rip,n=10,calc = "close",N= 260)
#vol_mon <- volatility(ret_mon,n=10,calc = "close",N= 260)
#vol_dash <- volatility(ret_dash,n=10,calc = "close",N= 260)

ret_bit <- log(close$Close.bitcoin)/lag(log(close$Close.bitcoin))
ret_eth <- log(close$Close.ethereum)/lag(log(close$Close.ethereum))
ret_lite <- log(close$Close.litecoin)/lag(log(close$Close.litecoin))
ret_rip <- log(close$Close.ripple)/lag(log(close$Close.ripple))
ret_mon <- log(close$Close.monero)/lag(log(close$Close.monero))
ret_dash <- log(close$Close.dash)/lag(log(close$Close.dash))

vol_bit <- ret_bit*ret_bit
vol_eth <- ret_eth*ret_eth
vol_lite <- ret_lite*ret_lite
vol_rip <- ret_rip*ret_rip
vol_mon <- ret_mon*ret_mon
vol_dash <- ret_dash*ret_dash

date_cry <- cryptocurrency$Date.bitcoin[1:929]
CurrencyVolatility <- data.frame(date_cry,vol_bit,vol_eth,vol_lite,vol_rip,vol_mon,vol_dash)
View(CurrencyVolatility)


bitVol_plot <- ggplot(data = CurrencyVolatility, x = date_cry) + geom_line(aes(x = date_cry, y = vol_bit, col = "red"))

ggplot(data = CurrencyVolatility, x = date_cry) + geom_line(aes(x = date_cry, y = vol_bit, col = "red")) + ggtitle("Bitcoin Volatility")
ggplot(data = CurrencyVolatility, x = date_cry) + geom_line(aes(x = date_cry, y = vol_dash, col = "red")) + ggtitle("Dash Volatility")
ggplot(data = CurrencyVolatility, x = date_cry) + geom_line(aes(x = date_cry, y = vol_eth, col = "red")) + ggtitle("Ethereum Volatility")
ggplot(data = CurrencyVolatility, x = date_cry) + geom_line(aes(x = date_cry, y = vol_lite, col = "red")) + ggtitle("Litecoin Volatility")
ggplot(data = CurrencyVolatility, x = date_cry) + geom_line(aes(x = date_cry, y = vol_mon, col = "red")) + ggtitle("Monero Volatility")
ggplot(data = CurrencyVolatility, x = date_cry) + geom_line(aes(x = date_cry, y = vol_rip, col = "red")) + ggtitle("Ripple Volatility")


bitcoin_plot + geom_line(aes(x = date_cry, y = vol_eth, col = "Blue"), data = CurrencyVolatility) +
  geom_line(aes(x = date_cry, y = vol_lite, col = "Dark Green"), data = CurrencyVolatility) +
  geom_line(aes(x = date_cry, y = vol_rip, col = "Black"), data = CurrencyVolatility) +
  geom_line(aes(x = date_cry, y = vol_mon, col = "Cyan"), data = CurrencyVolatility) +
  geom_line(aes(x = date_cry, y = vol_dash, col = "Green"), data = CurrencyVolatility)


##########################################################################################################
cryptocurrency$Volume.bitcoin  <- gsub(",","",cryptocurrency$Volume.bitcoin)
cryptocurrency$Volume.ethereum  <- gsub(",","",cryptocurrency$Volume.ethereum)
cryptocurrency$Volume.litecoin  <- gsub(",","",cryptocurrency$Volume.litecoin)
cryptocurrency$Volume.ripple  <- gsub(",","",cryptocurrency$Volume.ripple)
cryptocurrency$Volume.dash  <- gsub(",","",cryptocurrency$Volume.dash)
cryptocurrency$Volume.monero  <- gsub(",","",cryptocurrency$Volume.monero)

cryptocurrency$Volume.bitcoin <- as.numeric(cryptocurrency$Volume.bitcoin)
cryptocurrency$Volume.ethereum <- as.numeric(cryptocurrency$Volume.ethereum)
cryptocurrency$Volume.litecoin <- as.numeric(cryptocurrency$Volume.litecoin)
cryptocurrency$Volume.ripple <- as.numeric(cryptocurrency$Volume.ripple)
cryptocurrency$Volume.dash <- as.numeric(cryptocurrency$Volume.dash)
cryptocurrency$Volume.monero <- as.numeric(cryptocurrency$Volume.monero)

CurrencyVolume <- cryptocurrency[,c(6,12,18,24,30,36)]

str(CurrencyVolatility)

cor <- cor(CurrencyVolatility[,2:7],CurrencyVolume)
View(cor)
corrplot(cor, method = "pie")

bitcoin_df <- data.frame(CurrencyVolatility$date_cry, cryptocurrency[2:929,5],CurrencyVolatility$vol_bit,CurrencyVolume$Volume.bitcoin)
View(bitcoin_df)
colnames(bitcoin_df) <- c("Date","Close","Volatility","Volume")

#Training and Testing data
train <- bitcoin_df[700:920,]
test <- bitcoin_df[921:929,]

#Regression Model
m <- lm(Close ~ lag(Volatility) + log(Volume), train)
summary(m)

predict <- predict(m,test)
compTable <- data.frame(test$Date,test$Close,predict)
View(compTable)

compTable <- na.omit(compTable)

compTable$error <- abs(compTable$test.Close - compTable$predict)/compTable$test.Close
mean(compTable$error)
View(test)

a <- data.frame(compTable$test.Date,compTable$predict) 
b <- data.frame(train$Date,train$Close)
colnames(a) <- c("Date","Close")
colnames(b) <- c("Date","Close")

comp_bit <- compTable

forecast_series <- rbind(b,a)
View(comp_bit)

bit_fore <- ggplot(data = forecast_series, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))
bit <- ggplot(data = train, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))

install.packages("Metrics")
library(Metrics)

forecast::accuracy(comp_bit$predict,comp_bit$test.Close)

#MAPE = 7.98

write.csv(forecast_series,"forecastValues.csv")

##################
#Ethereum
View(cryptocurrency)
eth_df <- data.frame(CurrencyVolatility$date_cry, cryptocurrency[1:929,11],CurrencyVolatility$vol_eth,CurrencyVolume$Volume.ethereum)
View(eth_df)
colnames(eth_df) <- c("Date","Close","Volatility","Volume")

#Training and Testing data
train <- eth_df[700:920,]
test <- eth_df[921:929,]

#Regression Model
m <- lm(Close ~ lag(Volatility) + log(Volume), train)
summary(m)

predict <- predict(m,test)
compTable <- data.frame(test$Date,test$Close,predict)
View(compTable)

compTable <- na.omit(compTable)

compTable$error <- abs(compTable$test.Close - compTable$predict)/compTable$test.Close
mean(compTable$error)
View(train)

comp_eth <- compTable

a <- data.frame(compTable$test.Date,compTable$predict) 
b <- data.frame(train$Date,train$Close)
colnames(a) <- c("Date","Close")
colnames(b) <- c("Date","Close")

forecast_series <- rbind(b,a)
View(forecast_series)

ggplot(data = forecast_series, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))
ggplot(data = train, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))

forecast::accuracy(comp_eth$predict,comp_eth$test.Close)

#MAPE = 26.17

##################
#Litecoin
View(cryptocurrency)
lite_df <- data.frame(CurrencyVolatility$date_cry, cryptocurrency[1:929,17],CurrencyVolatility$vol_lite,CurrencyVolume$Volume.litecoin)
View(lite_df)
colnames(lite_df) <- c("Date","Close","Volatility","Volume")

#Training and Testing data
train <- lite_df[700:920,]
test <- lite_df[921:929,]

#Regression Model
m <- lm(Close ~ lag(Volatility) + log(Volume), train)
summary(m)

predict <- predict(m,test)
compTable <- data.frame(test$Date,test$Close,predict)
View(compTable)

compTable <- na.omit(compTable)

compTable$error <- abs(compTable$test.Close - compTable$predict)/compTable$test.Close
mean(compTable$error)
View(train)

comp_lite <- compTable

a <- data.frame(compTable$test.Date,compTable$predict) 
b <- data.frame(train$Date,train$Close)
colnames(a) <- c("Date","Close")
colnames(b) <- c("Date","Close")

forecast_series <- rbind(b,a)
View(forecast_series)

ggplot(data = forecast_series, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))
ggplot(data = train, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))

forecast::accuracy(comp_lite$predict,comp_lite$test.Close)
#MAPE = 17.18

##################################
##################
#Dash
View(cryptocurrency)
dash_df <- data.frame(CurrencyVolatility$date_cry, cryptocurrency[1:929,35],CurrencyVolatility$vol_dash,CurrencyVolume$Volume.dash)
View(dash_df)
colnames(dash_df) <- c("Date","Close","Volatility","Volume")

#Training and Testing data
train <- dash_df[700:920,]
test <- dash_df[921:929,]

#Regression Model
m <- lm(Close ~ lag(Volatility) + log(Volume), train)
summary(m)

predict <- predict(m,test)
compTable <- data.frame(test$Date,test$Close,predict)
View(compTable)

compTable <- na.omit(compTable)

compTable$error <- abs(compTable$test.Close - compTable$predict)/compTable$test.Close
mean(compTable$error)
View(train)

comp_dash <- compTable

a <- data.frame(compTable$test.Date,compTable$predict) 
b <- data.frame(train$Date,train$Close)
colnames(a) <- c("Date","Close")
colnames(b) <- c("Date","Close")

forecast_series <- rbind(b,a)
View(forecast_series)

ggplot(data = forecast_series, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))
ggplot(data = train, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))

forecast::accuracy(comp_dash$predict,comp_dash$test.Close)
#MAPE = 15.20

###################################################################3
#Ripple
View(cryptocurrency)
rip_df <- data.frame(CurrencyVolatility$date_cry, cryptocurrency[1:929,23],CurrencyVolatility$vol_rip,CurrencyVolume$Volume.ripple)
View(rip_df)
colnames(rip_df) <- c("Date","Close","Volatility","Volume")

#Training and Testing data
train <- rip_df[700:920,]
test <- rip_df[921:929,]

#Regression Model
m <- lm(Close ~ log(Volume) + lag(Volatility), train)
summary(m)

predict <- predict(m,test)
compTable <- data.frame(test$Date,test$Close,predict)
View(compTable)

compTable <- compTable[-c(1,2),]

compTable$error <- abs(compTable$test.Close - compTable$predict)/compTable$test.Close
mean(compTable$error)
View(train)

comp_rip <- compTable

a <- data.frame(compTable$test.Date,compTable$predict) 
b <- data.frame(train$Date,train$Close)
colnames(a) <- c("Date","Close")
colnames(b) <- c("Date","Close")

forecast_series <- rbind(b,a)

ggplot(data = forecast_series, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))
ggplot(data = train, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))

forecast::accuracy(compTable$predict,compTable$test.Close)
#MAPE = 15.84

#########################################
#Monero
View(cryptocurrency)
mon_df <- data.frame(CurrencyVolatility$date_cry, cryptocurrency[1:929,29],CurrencyVolatility$vol_mon,CurrencyVolume$Volume.monero)
View(mon_df)
colnames(mon_df) <- c("Date","Close","Volatility","Volume")

#Training and Testing data
train <- mon_df[700:920,]
test <- mon_df[921:929,]

#Regression Model
m <- lm(Close ~ lag(Volatility) + log(Volume), train)
summary(m)

predict <- predict(m,test)
compTable <- data.frame(test$Date,test$Close,predict)
View(compTable)

compTable <- na.omit(compTable)

compTable$error <- abs(compTable$test.Close - compTable$predict)/compTable$test.Close
mean(compTable$error)
View(train)

comp_mon <- compTable

a <- data.frame(compTable$test.Date,compTable$predict) 
b <- data.frame(train$Date,train$Close)
colnames(a) <- c("Date","Close")
colnames(b) <- c("Date","Close")

forecast_series <- rbind(b,a)
View(forecast_series)

ggplot(data = forecast_series, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))
ggplot(data = train, x = Date) + geom_line(aes(x = Date, y = Close, col = "red"))

forecast::accuracy(comp_mon$predict,comp_mon$test.Close)
#MAPE = 34.21


########################################################3
## ARIMAX

acf2(diff(log(train$Close)))

vars_matrix <- cbind(lag(train$Volatility),log(train$Volume))
View(vars_matrix)

library(lmtest)
arimax_model <- auto.arima(train$Close, xreg = vars_matrix)
coeftest(arimax_model)

arimax_model_2 <- auto.arima(train$Close, xreg = vars_matrix[,1])
coeftest(arimax_model_2)

forecast(arimax_model_2,xreg =vars_matrix_test[,1], h=8) -> arimax_forecast

vars_matrix_test <- cbind(lag(test$Volatility),log(test$Volume))

test <- na.omit(test)
test$Close
