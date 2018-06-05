library(tidyverse)
library(timeSeries)
library(zoo)
library(xts)
library(reshape)
library(Boruta)
library(randomForest)

#Regression analysis

#load csv
crime.data <- read.csv("E:/datasets/crime_us.csv", sep = ",",na.strings = c(" ","\t","\n"),check.names = T,skip = 3,strip.white = T,fill = T, stringsAsFactors = F)

#check header
head(crime.data,100)

#Verify column names as in above command head returning garbage data headers missing
colnames(crime.data)

#take off cols 21 to 24
crime.data <- crime.data[,1:20]


#Get a clear view with top 1000 rows
View(head(crime.data, 1000))

#first part contains records til 2013 in first 20 rows
data1 <- crime.data[1:20,]

#Row 30 to 33 shows change in percentage for each crime category.
data2 <- crime.data[30:33,]
data2 <- data2[-1,] #deleting header

#lets clean our data
data1

#last character from year which repesents count estimation method need to removed
data1$Year <- str_sub(data1$Year,1,4)

#as all columns are numeric, let's remove commas and convert them to numeric using gsub
str(data1)
class(data1)

for(i in seq_along(colnames(data1))){
  data1[[i]] <- gsub(",","",data1[[i]]) ## Noice [[]] bracket to index charcter string rather than to [] to point list
  data1[[i]] <- as.numeric(data1[[i]])
}

#lets plot year wise crime category
colnames(data1)
ggplot(data1, aes(x=Year, y=Violent.crime))+
  geom_bar(stat="identity")


#Reshape data to create proper variable table
data1_pivot <- melt(data = data1,id.vars = "Year")


ggplot(data1_pivot, aes(x=Year, y=value))+
  geom_bar(stat="identity")+
  facet_wrap("variable", scales = "free", shrink = T, ncol = 4)

#Let's analyse correlation

cols_wo_rate <- grep(colnames(data1), pattern = "rate", value = T,invert = T) #invert removes column names 
#matching string rate

data1_wo_rate <- data1[,cols_wo_rate]

data1_wo_rate <- melt(data1_wo_rate, id.vars = c("Year","Population1"), variable_name = c("Crime.Type"))

ggplot(data1_wo_rate, aes(x=Year, y=value, color=value))+
  geom_point()+
  facet_wrap("Crime.Type", scales = "free")

#Pattern seems more or less same. let's find YoY change

data1_wo_rate <- data1[,cols_wo_rate]








