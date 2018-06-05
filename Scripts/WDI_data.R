library(tidyverse)
library(randomForest)
library(Boruta)
library(zoo)


wdi.data <- read.csv("E:/datasets/WDI_Data.csv",header = T, stringsAsFactors = F, check.names = T )

str(wdi.data)

wdi.data <- rename(wdi.data, Series.Name = ï..Series.Name)
wdi.data <- rename_at(wdi.data, vars(contains("YR")), function(x) str_extract_all(x, pattern = "YR[0-9]+", simplify = T))

#Take off commas from first column

wdi.data$Series.Name <- gsub(",","", wdi.data$Series.Name)

str(wdi.data)
dim(wdi.data)

#out of 22 columns first four are character class, rest all are numeric

for(i in 5:22) {
  wdi.data[[i]] <- as.numeric(wdi.data[[i]])
}

str(wdi.data)

wdi.data <- wdi.data[,-22]

ind_pak <- wdi.data %>% 
  filter(wdi.data$Country.Name == "India" | wdi.data$Country.Name == "Pakistan")
  

head(ind_pak)


