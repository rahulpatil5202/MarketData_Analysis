library(tidyverse)
library(randomForest)
library(Boruta)
library(zoo)
library(reshape)

#options("scipen"=100, "digits"=4)

wdi.data <- read.csv("E:/datasets/WDI_Data.csv",header = T, stringsAsFactors = F, check.names = T )

str(wdi.data)

wdi.data <- dplyr::rename(wdi.data, Series.Name = ï..Series.Name)
wdi.data <- dplyr::rename_at(wdi.data, vars(contains("YR")), function(x) str_extract_all(x, pattern = "[0-9]+", simplify = T))

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
  filter(wdi.data$Country.Name == "India" | wdi.data$Country.Name == "Pakistan")%>%
  format(scientific =F, digits = 2)
  

head(ind_pak)
colnames(ind_pak)


ind_pak_melt <- melt(ind_pak, id.vars =c("Series.Name", "Series.Code","Country.Name", "Country.Code"),variable_name = "Year")


ggplot(ind_pak_melt, aes(x=Year, y=value, fill=Country.Name))+
  geom_bar(stat = "sum")+
  facet_wrap("Series.Name", scales = "free", ncol = 3)


