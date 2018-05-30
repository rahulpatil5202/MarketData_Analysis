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



Quandl('NSE/BOROSIL')
