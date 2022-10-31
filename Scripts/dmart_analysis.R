library(tidyverse)
library(Boruta)
library(randomForest)
library(modelr)
library(lubridate)

dmart <- read.csv('E:/datasets/dmart_1yr.csv', header=T, stringsAsFactors = F)
sensex <- read.csv('E:/datasets/SENSEX_1yr.csv', header = T, stringsAsFactors = F)

head(dmart)
head(sensex)

summary(dmart)
summary(sensex)

#Change date formats to 'Date' from 'Character' class
dmart$Date <- dmy(dmart$Date)
sensex$Date <- dmy(sensex$Date)

class(dmart$Date)
class(sensex$Date)


#Map sensex columns and add them to dmart df
dmart$Sensex.Open <- sensex[match(sensex$Date,dmart$Date),]$Open
dmart$Sensex.Close <- sensex[match(sensex$Date,dmart$Date),]$Close
dmart$sensex.High <- sensex[match(sensex$Date,dmart$Date),]$High
dmart$sensex.Low <- sensex[match(sensex$Date,dmart$Date),]$Low
dmart$dd <- day(dmart$Date)
dmart$mm <- month(dmart$Date)

#Add zero to na's
dmart[is.na(dmart)] <- 0

#add prev.close column but before that we need dataframe sorted on date column
dmart <- dmart[order(as.Date(dmart$Date, format="%d/%m/%Y")),]
dmart_close_price <- c()
dmart_close_price <- dmart$Close.Price

for (i in 2:nrow(dmart)){
  dmart$Prev.Close[i] <- dmart_close_price[i-1]
}

head(dmart)

#let's prepare training and testing data of dmart dataframe
dmart[is.na(dmart)] <- 0
dmart <- resample_partition(dmart, c(test = 0.30, train = 0.70))
dmart_train <- as.data.frame(dmart$train)
dmart_test <- as.data.frame(dmart$test)

set.seed(123)
borutaTrain <- Boruta(Open.Price~.,dmart_train, doTrace = 2)
print(borutaTrain)
getConfirmedFormula(borutaTrain) #Apply these results to selection variables to random Forest

plot(borutaTrain, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(borutaTrain$ImpHistory),function(i)
  borutaTrain$ImpHistory[is.finite(borutaTrain$ImpHistory[,i]),i])
names(lz) <- colnames(borutaTrain$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(borutaTrain$ImpHistory), cex.axis = 0.7)

fit.randomForest <- randomForest(Open.Price ~ High.Price + Low.Price + Close.Price + WAP + 
                                   No.of.Shares + Total.Turnover..Rs.. + Spread.High.Low + Spread.Close.Open + 
                                   Sensex.Open + Sensex.Close + sensex.High + sensex.Low + mm + 
                                   Prev.Close, data=dmart_train)
predict(fit.randomForest, dmart_test)

#check top10 rows of dmart_test against prediction
head(predict(fit.randomForest, dmart_test),10)
head(dmart_test$Open.Price,10)

mae(model = fit.randomForest, data = dmart_test)


fit.randomForest2 <- randomForest(Open.Price ~ Sensex.Close + dd, data = dmart_train)

predict(fit.randomForest2, dmart_test)
head(predict(fit.randomForest2, dmart_test),10)
head(dmart_test$Open.Price,10)

mae(model = fit.randomForest2, data = dmart_test)


test2<-head(dmart_test,1)
test2$dd <- 23
test2$mm <- 5
test2$Sensex.Close <- 34344.91

predict(fit.randomForest2, test2)
predict(fit.randomForest, test2)

