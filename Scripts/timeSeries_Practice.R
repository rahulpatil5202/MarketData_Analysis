### Time series prediction practise

# pckgs<-c("Quandl","Sweep","tidyverse","tidyquant","ggplot","forcats","stringr")
# install.packages(pckgs,dependencies = TRUE)
library(timeSeries)
library(Quandl)
library(forecast)
library(zoo)
library(xts)

# quandl_api_key('CuryXaHBCuZX8LsRs1g1')
# YES_BANK = Quandl("NSE/YESBANK",collapse="daily",start_date="2017-05-15",type="raw")
# YES_BANK$Stock <- "YES BANK"

YES_BANK <- read.csv("D:/Downloads/YESBANK.csv", header = T, stringsAsFactors = F)

head(YES_BANK)
str(YES_BANK)

sum(is.na(YES_BANK$Adj.Close))

#Other than NA values found. Let's force it to NA if not numeric 
YES_BANK$Adj.Close <- as.numeric(as.character(YES_BANK$Adj.Close))

# Now remove NAs by complete.cases
YES_BANK <- YES_BANK[complete.cases(YES_BANK),] 

yes_xts <- as.xts(YES_BANK[,-1],order.by = YES_BANK$Date, dateFormat = "%Y-%m-%d")

start(yes_ts)
end(yes_ts)
frequency(yes_ts)
periodicity(yes_ts)


plot.zoo(yes_ts, plot.type = "single")

plot(diff(log(yes_ts)), plot.)

acf(x = log(yes_ts), lag.max = 20, na.action = na.pass)

train_ts <- yes_ts[1:3000]

auto.arima(yes_ts)

fit1 <- arima(train_ts,order = c(3,1,0))

pred_vals <- predict(fit1, se.fit = T, n.ahead = 10)
act_vals<- yes_ts[3001:3010]

pred_vals$pred
YES_BANK[3001:3010,"Adj.Close"]

head(yes_ts)
head(log(yes_ts))
head(diff(log(yes_ts)))
