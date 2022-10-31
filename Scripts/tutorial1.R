###Tutorial 1
pckgs<-c("Quandl","Sweep","tidyverse","tidyquant","ggplot","forcats","stringr")
install.packages(pckgs,dependencies = TRUE)
library(Quandl)
library(tidyverse)
library(ggplot2)
library(tidyquant)
library(timetk)
library(forcats)
library(stringr)
library(gganimate)
library(plyr)
library(stringr)
library(gridExtra)

## Setup the Quandl Free Account and API Key, Please copy and paste the API key in order to #authenticate

Quandl.api_key("CuryXaHBCuZX8LsRs1g1")

## Download the data Set
ICICI = Quandl("NSE/ICICIBANK",collapse="daily",start_date="2017-05-15",type="raw")
PNB= Quandl("NSE/PNB",collapse="daily",start_date="2017-05-15",type="raw")
Axis=Quandl("NSE/AXISBANK",collapse="daily",start_date="2017-05-15",type="raw")
Canara=Quandl("NSE/CANBK",collapse="daily",start_date="2017-05-15",type="raw")
BOB=Quandl("NSE/BANKBARODA",collapse="daily",start_date="2017-05-15",type="raw")
SBI=Quandl("NSE/SBIN",collapse="daily",start_date="2017-05-15",type="raw")


## Add another ("Stock") coloumn in Datasets using cbind command

ICICI<-cbind(ICICI,Stock="")
PNB<-cbind(PNB,Stock="")
Axis<-cbind(Axis,Stock="")
SBI<-cbind(SBI,Stock="")
Canara<-cbind(Canara,Stock="")
BOB<-cbind(BOB,Stock="")


## Paste the stock name in stock column

ICICI$Stock<-paste(ICICI$Stock,"ICICI",sep="")
PNB$Stock<-paste(PNB$Stock,"PNB",sep="")
Axis$Stock<-paste(Axis$Stock,"Axis",sep="")
SBI$Stock<-paste(SBI$Stock,"SBI",sep="")
Canara$Stock<-paste(Canara$Stock,"Canara",sep="")
BOB$Stock<-paste(BOB$Stock,"BOB",sep="")

## Consolidate under one dataset

Master_Data<-rbind(ICICI,PNB,Axis,SBI,Canara,BOB)

## Visualisation in ggplot2 ("Comparative Visulisation of Close Price listed on NSE")

## Convert the dates into character in order to split the coloumn into "Y" "m" "dd"" columns
Master_Data$Date<-as.character(Master_Data$Date)

## Split the date and create a list for the same

list<-strsplit(Master_Data$Date,"-")

## Convert the list into dataframe
library(plyr)
Master_Date1<-ldply(list)
colnames(Master_Date1)<-c("Year","Month","Day")

## Column bind with the main dataframe
Master_Data<-cbind(Master_Data,Master_Date1)
names(Master_Data)

Master_Data$`Total Trade Quantity`<-Master_Data$`Total Trade Quantity`/100000
## Convert the Date to as.Date()

Master_Data$Date<-as.Date(Master_Data$Date)


library(devtools)
library(RCurl)
library(httr)
library(gganimate)
# set_config( config( ssl_verifypeer = 0L ) )
devtools::install_github("dgrtwo/gganimate")
install.packages("ImageMagick")

## Visualisation with Bubble Plot
P<- ggplot(Master_Data,aes(factor(Stock),Close,color=Stock,frame=Month)) +
  geom_jitter(aes(size = Close, colour=Stock, alpha=.02)) +
  ylim(0,1000)+
  labs(title = "Bank Stock Monthly Prices", x = "Banks", y= "Close Price") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(colour = "grey61", size = 0.5, linetype = "dotted"),
        panel.grid.minor = element_blank(),
        axis.line=element_line(colour="black"),
        plot.title = element_text(hjust = 0.5,size=18,colour="indianred4"))+
  theme(legend.position="none")

P1<-gganimate(P,'Price_Range.gif',ani.width=600,ani.height=400,interval=1)

## Group By Stock

Master_Data<-Master_Data%>%
  tibble::as.tibble()%>%
  group_by(Stock)

## Visualisation for Daily Stock Prices

Master_Data %>%
  ggplot(aes(x = Date, y = Close, color = Stock)) +
  geom_point() +
  labs(title = "Daily Close Price", x = "Month",y="Close Price") +
  facet_wrap(~ Stock, ncol = 3, scale = "free_y") +
  scale_fill_tq(fill="green4",theme="light") +
  theme_tq() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(colour = "grey61", size = 0.5, linetype = "dotted"),
        panel.grid.minor = element_blank(),
        axis.line=element_line(colour="black"),
        plot.title = element_text(hjust = 0.5,size=18,colour="indianred4"))+
  theme(legend.position="none")




## Traded Quantity vs Price

z<-Master_Data %>%
  ggplot(aes(x = `Total Trade Quantity`, y = Close, color = Stock,frame=Month)) +
  geom_smooth(method='loess') +
  xlim(0,400)+
  labs(title = "Monthly Traded Quantity vs Price", x = "Traded Quantity (Lacs)",y="Close Price") +
  facet_wrap(~ Stock, ncol = 3, scale = "free_y") +
  scale_fill_tq(fill="green4",theme="light") +
  theme_tq() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5,size=18,colour="indianred4"),
        axis.line = element_line(colour = "black"))+
  theme(legend.position="none")

z

z1<-gganimate(z,'Quantity_Price.gif',ani.width=600,ani.height=400,interval=0.7)


## Deviation from High & Low Price
Master_Data_High<-Master_Data%>%mutate(Dev_High=High-Open)
Master_Data_Low<-Master_Data%>%mutate(Dev_Low=Open-Low)

## Computation of weekly average for high Price

Master_Data_High_Week <- Master_Data_High %>%
  tq_transmute(
    select     = Dev_High,
    mutate_fun = apply.weekly, 
    FUN        = mean,
    na.rm      = TRUE,
    col_rename = "Dev_High_Mean"
  )

## Computation weekly average for Low Price

Master_Data_Low_Week<-Master_Data_Low%>%
  tq_transmute(
    select  = Dev_Low,
    mutate_fun = apply.weekly,
    FUN = mean,
    na.rm = TRUE,
    col_rename = "Dev_Low_Mean"
  )

## Visualisation of density distribution of High Price

High<-Master_Data_High_Week%>%ggplot(aes(x=Dev_High_Mean,color=Stock))+
  geom_dotplot(binwidth=0.50,aes(fill=Stock))+
  xlim(0,10)+
  scale_fill_manual(values=c("#999999", "#E69F00","#CC9933","#99FF00","#CC3399","#FF9933"))+
  labs(title="Distribution of High Price Deviation from Open Price",x="Weekly Mean Deviation")+
  facet_wrap(~Stock,ncol=3,scale="free_y")+
  scale_color_tq(values=c("#999999"))+
  theme_tq()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(colour = "grey61", size = 0.5, linetype = "dotted"),
        panel.grid.minor = element_blank(),
        axis.line=element_line(colour="black"),
        plot.title = element_text(hjust = 0.5,size=16,colour="indianred4"))+
  theme(legend.position="none")

## Visualisation of density distribution of Low Price
Low<-Master_Data_Low_Week%>%ggplot(aes(x=Dev_Low_Mean,color=Stock))+
  geom_dotplot(binwidth=0.50,aes(fill=Stock))+
  xlim(0,10)+
  scale_fill_manual(values=c("#999999", "#E69F00","#CC9933","#99FF00","#CC3399","#FF9933"))+
  labs(title="Distribution of Weekly Low Price Deviation from Open Price",x="Weekly Mean Deviation")+
  facet_wrap(~Stock,ncol=3,scale="free_y")+
  scale_color_tq(values=c("#999999"))+
  theme_tq()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(colour = "grey61", size = 0.5, linetype = "dotted"),
        panel.grid.minor = element_blank(),
        axis.line=element_line(colour="black"),
        plot.title = element_text(hjust = 0.5,size=16,colour="indianred4"))+
  theme(legend.position="none")
## Arrange
grid.arrange(High,Low,ncol = 2, nrow = 1)
# 
# Observing the Autocorrelation lags
# The lag operator (also known as backshift operator) is a function that shifts (offsets) a time series such that the “lagged” values are aligned with the actual time series. The lags can be shifted any number of units, which simply controls the length of the backshift.
# 
# Here, “k” is denoted as lag. We will see the lag of 180 days period and see how stocks behave.
# 
# These are the steps for Computation
# 
# Define k lag period
# Create columns for lag periods
# Group the data by Stock by creating new data frame for lags
# Apply lag.xts using tq_mutate() function on the new dataframe
# Apply Auto-correaltion


k <- 1:180
col_names <- paste0("lag_", k)

## Only Select Columns "Date" and "Close" from hte master data frame.

Master_Data_lags<-Master_Data%>%
  tibble::as_tibble() %>%
  group_by(Stock)

Master_Data_lags<-Master_Data_lags%>%select(Date,Close)
# Apply lag.xts function using tq_mutate

Master_Data_lags<-Master_Data_lags%>%
  tq_mutate(
    select = Close,
    mutate_fun = lag.xts,
    k=1:180,
    col_rename=col_names
  )

# Calculate the autocorrelations and 95% cutoffs

Master_Data_AutoCorrelations<-Master_Data_lags %>%
  gather(key = "lag", value = "lag_value", -c(Stock,Date, Close)) %>%
  mutate(lag = str_sub(lag, start = 5) %>% as.numeric) %>%
  group_by(Stock, lag) %>%
  summarize(
    cor = cor(x = Close, y = lag_value, use = "pairwise.complete.obs"),
    cutoff_upper = 2/(n())^0.5,
    cutoff_lower = -2/(n())^0.5
  )

## Visualisation of Autocorrelation: ACF Plot

Master_Data_AutoCorrelations %>%
  ggplot(aes(x = lag, y = cor, color = Stock, group = Stock)) +
  
  # Add horizontal line a y=0
  geom_hline(yintercept = 0) +
  
  # Plot autocorrelations
  geom_point(size = 2) +
  geom_segment(aes(xend = lag, yend = 0), size = 1) +
  
  # Add cutoffs
  geom_line(aes(y = cutoff_upper), color = "blue", linetype = 2) +
  geom_line(aes(y = cutoff_lower), color = "blue", linetype = 2) +
  
  # Add facets
  facet_wrap(~ Stock, ncol = 3) +
  
  # Aesthetics
  expand_limits(y = c(-1, 1)) +
  scale_color_tq() +
  theme_tq() +
  labs(
    title = paste0("Tidyverse ACF Plot: Lags ", rlang::expr_text(k)),
    x = "Lags"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(colour = "grey61", size = 0.5, linetype = "dotted"),
    plot.title = element_text(hjust = 0.5,size=18,colour="indianred4")
  )