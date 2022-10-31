### Time series prediction practise

# pckgs<-c("Quandl","Sweep","tidyverse","tidyquant","ggplot","forcats","stringr", "smooth", "Mcomp")
# install.packages(pckgs,dependencies = TRUE)
library(timeSeries)
library(Quandl)
library(forecast)
library(zoo)
library(xts)
library(Mcomp)
library(smooth)
library(tidyverse)
library(lubridate)
# quandl_api_key('CuryXaHBCuZX8LsRs1g1')
# YES_BANK = Quandl("NSE/YESBANK",collapse="daily",start_date="2017-05-15",type="raw")
# YES_BANK$Stock <- "YES BANK"

YES_BANK <- read.csv("D:/Downloads/YESBANK.csv", header = T, stringsAsFactors = F)

head(YES_BANK)
summary(YES_BANK)
str(YES_BANK)

YES_BANK$Date <- as.Date(YES_BANK$Date)

sum(is.na(YES_BANK$Adj.Close))

#Other than NA values found. Let's force it to NA if not numeric 
YES_BANK$Adj.Close <- as.numeric(as.character(YES_BANK$Adj.Close)) #Error warnings
#says there are other than numeric values in this column

# Now remove NAs by complete.cases
YES_BANK <- YES_BANK[complete.cases(YES_BANK),] 

#Chnage all columns to numeric from column 2

for (i in 2:7)
{
  
  YES_BANK[,i] <- as.numeric(YES_BANK[,i])
}

YES_BANK <- YES_BANK[order(YES_BANK$Date),]

#Let's add 10,30,50 and 200 days moving averages

YES_BANK$MA10 <- rollmean(YES_BANK$Adj.Close, 10, fill = NA, align = "right")
YES_BANK$MA30 <- rollmean(YES_BANK$Adj.Close, 30, fill = NA, align = "right")
YES_BANK$MA50 <- rollmean(YES_BANK$Adj.Close, 50, fill = NA, align = "right")
YES_BANK$MA200 <- rollmean(YES_BANK$Adj.Close, 200, fill = NA, align = "right")

#lets visualize moving average differences
#Yearly
ggplot(YES_BANK, aes(x=Date))+
  geom_line(aes(y=MA10), color="red")+
  geom_line(aes(y=MA30), color="green")+
  geom_line(aes(y=MA50), color="blue")+
  geom_line(aes(y=MA200), color="black")+
  labs(title="Moving average", x="Dates", y="Value")+
  scale_x_date(date_breaks = "years",date_labels = ("%Y"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))


#More focused on quarters starting from 2017
ggplot(YES_BANK[YES_BANK$Date > "2016-01-01",], aes(x=Date))+
  geom_line(aes(y=MA10), color="red")+
  geom_line(aes(y=MA30), color="green")+
  geom_line(aes(y=MA50), color="blue")+
  geom_line(aes(y=MA200), color="black")+
  labs(title="Moving average", x="Dates", y="Value")+
  scale_x_date(date_breaks = "3 months",date_labels = ("%Y-%m"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))


#Create timeseries using xts
yes_xts <- as.xts(YES_BANK[,-1],order.by = YES_BANK$Date, dateFormat = "%Y-%m-%d")


start(yes_xts)
end(yes_xts)
frequency(yes_xts)
periodicity(yes_xts)

#Single plot
plot.zoo(yes_xts)

#Multiple plots


#lets add moving averages

yes_xts$MA10 <- rollmean(yes_xts$Adj.Close,k = 10, fill = NA) #keeping default align = center
#i.e. 5 values from above and five from below to calculate rolling mean
yes_xts$MA20 <- rollmean(yes_xts$Adj.Close,k = 20, fill = NA)

plot.zoo(yes_xts)
