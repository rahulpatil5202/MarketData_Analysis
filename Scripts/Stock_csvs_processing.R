library(plyr)
library(tidyverse)
library(modelr)
library(lubridate)
library(randomForest)
library(stringi)
library(RPostgreSQL)
library(anytime)
##########################################
#Define function to clean column names
cleanColnames <- function (x){
  colnames(x) <- gsub("[[:punct:]]+",'_', colnames(x))
  colnames(x) <- gsub("[[:punct:]]$",'', colnames(x))
  colnames(x) <- gsub("$[[:punct:]]",'', colnames(x))
  colnames(x) <- tolower(colnames(x))
  return(x)
}

trimSpaces <- function(df){
  for(i in seq_along(colnames(df))){
    if(is.character(df[,i])==T){
      df[,i] <- trimws(df[,i])
    }
  }
  return(df)
}

rootCleanUp <- function(){
  cat('\n\nCleaning BSE root folder\n\n')
  bseZips <- list.files(path = 'E:/MarketData/BSE_Bhavcopies', pattern = "\\.zip$", full.names = T)
  file.copy(from = bseZips, to = 'E:/MarketData/BSE_Bhavcopies/Archive/', overwrite = T, copy.mode = T)
  file.remove(bseZips)
  bseCsvs <- list.files(path = 'E:/MarketData/BSE_Bhavcopies/unzipped_csvs', pattern = "\\.csv$|\\.CSV$", full.names = T)
  file.copy(from = bseCsvs, to = 'E:/MarketData/BSE_Bhavcopies/Archive/unzipped_csvs/', overwrite = T, copy.mode = T)
  file.remove(bseCsvs)
  cat('\n\nDone cleaning BSE root folder')
  
  cat('\n\nCleaning NSE root folder\n\n')
  nseZips <- list.files(path = 'E:/MarketData/NSE_Bhavcopies', pattern = "\\.zip$", full.names = T)
  file.copy(from = nseZips, to = 'E:/MarketData/NSE_Bhavcopies/Archive/', overwrite = T, copy.mode = T)
  file.remove(nseZips)
  nseCsvs <- list.files(path = 'E:/MarketData/NSE_Bhavcopies/unzipped_csvs', pattern = "\\.csv$|\\.CSV$", full.names = T)
  file.copy(from = nseCsvs, to = 'E:/MarketData/NSE_Bhavcopies/Archive/unzipped_csvs', overwrite = T, copy.mode = T)
  file.remove(nseCsvs)
  cat('\n\nDone cleaning NSE root folder')
  
  cat('\n\nCleaning NSE Indices folder\n\n')
  nseIndCsvs <- list.files(path = 'E:/MarketData/NSE_Indices', pattern = "\\.csv$|\\.CSV$", full.names = T)
  file.copy(from = nseIndCsvs, to = 'E:/MarketData/NSE_Indices/Archive/', overwrite = T, copy.mode = T)
  file.remove(nseIndCsvs)
  cat('\n\nDone cleaning NSE Indices root folder')
  
  rm(bseZips,bseCsvs,nseZips,nseCsvs,nseIndCsvs)
  
}


###########################################
## NSE Bhavcopy Processing for CSVs console

# get all the zip files
zipFiles <- file.info(list.files(path = "E:/MarketData/NSE_Bhavcopies", pattern = "\\.zip$", full.names = TRUE))
corruptZips <- zipFiles[zipFiles$size < 1000,]
#Delete corrupt files
unlink(row.names(corruptZips), recursive = F, force = F)

#Reset zip files after corrupt files deletion process
zipFiles <- file.info(list.files(path = "E:/MarketData/NSE_Bhavcopies", pattern = "\\.zip$", full.names = TRUE))

# unzip all your files
outpath <- "E:/MarketData/NSE_Bhavcopies/unzipped_csvs"
ldply(.data = row.names(zipFiles), .fun = unzip, exdir=outpath)

# get the csv files
csv_files <- list.files(path = outpath, pattern = "\\.csv$")

# read the csv files
setwd(outpath)
getwd()
NSE_stock_data <- ldply(.data = csv_files, function(x) read.csv(x, stringsAsFactors = F, as.is = T, check.names = T))

head(NSE_stock_data)
str(NSE_stock_data)


#Let's check NA's in our Stock data
na_count <-sapply(NSE_stock_data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

#Drop useless column X
NSE_stock_data <- subset(NSE_stock_data, select = -X)
summary(NSE_stock_data)
str(NSE_stock_data)

#Change date column to proper formating

NSE_stock_data$TIMESTAMP <- lubridate::dmy(NSE_stock_data$TIMESTAMP) 

#Change 'Date' column name as it is reserved keyword
colnames(NSE_stock_data)[11] <- "TRADE_DATE"


rm(corruptZips,na_count,zipFiles, csv_files, outpath)

#####################################
## NSE Indices processing to console

csvs_path <- "E:/MarketData/NSE_Indices"
csv_files <- file.info(list.files(path = csvs_path, pattern = "\\.csv$", full.names = T))
corrupt_csvs <- csv_files[csv_files$size < 1000,]
unlink(row.names(corrupt_csvs), recursive = F, force = F)

#Reset csv list after corrupt file deletion
csv_files <- list.files(path = csvs_path, pattern = "\\.csv$", full.names = T)
NSE_Indices_data <- ldply(.data = csv_files, function(x) read.csv(x, stringsAsFactors = F, as.is = T, check.names = T)) 
head(NSE_Indices_data)
str(NSE_Indices_data)
summary(NSE_Indices_data)

#change date and other formats
NSE_Indices_data$Index.Date <- lubridate::dmy(NSE_Indices_data$Index.Date)

numericFields <- colnames(NSE_Indices_data[,-c(1,2)])
for(i in numericFields){
  NSE_Indices_data[,i] <- as.numeric(as.character(NSE_Indices_data[,i]))
  
}


#Check NAs
na_count <- sapply(NSE_Indices_data, function(x) sum(length(which(is.na(x)))))
na_count <- data.frame(na_count)
na_count

#Dropping records having NA values in important columns
NSE_Indices_data <- NSE_Indices_data[!is.na(NSE_Indices_data$Open.Index.Value),]

rm(corrupt_csvs,na_count,csv_files,csvs_path,i,numericFields)

#######################################
## BSE bhavcopies processing to console

#Begin with deleting files with size less than 10 kb (invalid downloads)

## Get vector of all file names
zipFiles <- file.info(list.files(path="E:/MarketData/BSE_Bhavcopies",pattern="\\.zip$", full.names=TRUE))
## Extract vector of empty files' names
corrupt_zips <- zipFiles[zipFiles$size < 7000,]
corrupt_zips_names <- row.names(corrupt_zips)
## Remove empty files
unlink(corrupt_zips_names, recursive=TRUE, force=FALSE)

# get all the zip files again as deleted corrupt files

zipFiles <- list.files(path = "E:/MarketData/BSE_Bhavcopies", pattern = "\\.zip$", full.names = TRUE)

# unzip all your files
outpath <- "E:/MarketData/BSE_Bhavcopies/unzipped_csvs"
ldply(.data = zipFiles, .fun = unzip, exdir=outpath)

# get the csv files
csv_files <- list.files(path = outpath, pattern = "\\.CSV$")

# read the csv files
setwd("E:/MarketData/BSE_Bhavcopies/unzipped_csvs/")
getwd()

#Let's add a new column date using csvs file name to each csv
#Adding this column because some csv files mssing Trade Date column.
# We will use file name to generate trade date as file names have respective dates of trade in their names

for(i in seq_along(csv_files)){
  raw <- read.csv(csv_files[i], header = T, stringsAsFactors = F)
  raw$Trade_Date_New <- dmy(stringi::stri_sub(csv_files[i],-10,-5))
  write.csv(raw,csv_files[i])
  print(paste("Writing",i,"of", length(csv_files),"..Adding new column to ", csv_files[i]))
}

rm(raw)

BSE_stock_data <- ldply(.data = csv_files, function(x) read.csv(x, header = T, stringsAsFactors = F, as.is = T, check.names = T))

head(BSE_stock_data)
str(BSE_stock_data)
#Let's check NA's in our Stock data
na_count <-sapply(BSE_stock_data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

#Dropping NA and useless coulmns
BSE_stock_data <- subset(BSE_stock_data, select = c(SC_CODE,SC_NAME,SC_GROUP,SC_TYPE,OPEN,HIGH,+
              LOW,CLOSE,LAST,PREVCLOSE,NO_TRADES,NET_TURNOV,TDCLOINDI,ISIN_CODE,Trade_Date_New))

summary(BSE_stock_data)
str(BSE_stock_data)

#change trading date to date format
BSE_stock_data$Trade_Date_New <- as.Date(BSE_stock_data$Trade_Date_New)
str(BSE_stock_data)

rm(corruptZips,na_count,csv_files, corrupt_csvs, zipFiles, i, outpath, corrupt_zips)


#Write csvs of all combined data and 
#write all combined csvs to PostgreSQL database clean column names and trim white spaces first

#Cleaning column names
NSE_stock_data <- cleanColnames(NSE_stock_data)
NSE_Indices_data <- cleanColnames(NSE_Indices_data)
BSE_stock_data <- cleanColnames(BSE_stock_data)

#trim white spaces from character columns using defines trimSpaces function

NSE_stock_data <- trimSpaces(NSE_stock_data)
NSE_Indices_data <- trimSpaces(NSE_Indices_data)
BSE_stock_data <- trimSpaces(BSE_stock_data)


#creating connection for postgresql
cn1 <- dbConnect(PostgreSQL(), host = "localhost", port = 5432, dbname = "data_science", user = "rahul", password = "postgres@123")

#NSE csv
#Adding table to postgreSQL
dbWriteTable(cn1, "nse", NSE_stock_data, row.names = F, append = T)

mindate <- min(NSE_stock_data$trade_date)
maxdate <- max(NSE_stock_data$trade_date)
write.csv(NSE_stock_data,file = paste("E:/MarketData/NSE_",mindate,"-",maxdate,".csv", sep = ""))

#NSE Indices csv

dbWriteTable(cn1, "nse_indices",NSE_Indices_data, append = T,row.names = F)

mindate <- min(NSE_Indices_data$index_date)
maxdate <- max(NSE_Indices_data$index_date)
write.csv(NSE_Indices_data,file = paste("E:/MarketData/NSE_Indices_",mindate,"-",maxdate,".csv", sep = ""), row.names = F)

#BSE csv
dbWriteTable(cn1, "bse", BSE_stock_data, append = T, row.names = F)

mindate <- min(BSE_stock_data$trade_date_new)
maxdate <- max(BSE_stock_data$trade_date_new)
write.csv(BSE_stock_data,file = paste("E:/MarketData/BSE_",mindate,"-",maxdate,".csv", sep = ""))

#Let's clean root folders to archives
rootCleanUp()


rm(list=ls())
dbDisconnect(cn1)










## Some interesting analysis and plots
dmart_data <- NSE_stock_data[which(NSE_stock_data$SYMBOL == "DMART" | NSE_stock_data$ISIN == "INE192R01011"),]

head(dmart_data[order(dmart_data$TIMESTAMP),])

ggplot(dmart_data, aes(x=TIMESTAMP, y=LAST))+
  geom_line(size = 1)+
  geom_smooth(method = 'lm')

demat_ac <- read.csv('E:/Rahul_IMP/Investment/demat_24may2018.csv', header = T, stringsAsFactors = F)
head(demat_ac)

portfolio <- NSE_stock_data %>% filter(SYMBOL %in% demat_ac$ScripCode & SERIES == 'EQ')
head(portfolio)

p1 <- ggplot(portfolio, aes(x=TIMESTAMP, y=LAST))+
  geom_line()+
  facet_wrap('SYMBOL', ncol = 7)

p1

ggsave(plot = p1, width = 12, height = 5, dpi = 300, filename = "portfolio_performance.pdf")

portfolio_stock_names <- portfolio %>% group_by(SYMBOL)%>%
  summarise(stock_count = n())
portfolio_stock_names

class(portfolio_stock_names)

# lets plot line graph for each stock in portfolio individually

plots <- list()
for (i in 1:nrow(portfolio_stock_names)){
  plot_data <- portfolio %>% filter(SYMBOL %in% portfolio_stock_names$SYMBOL[i])
  plots[[i]] <- ggplot(plot_data, aes(x=TIMESTAMP, y=LAST))+geom_line()+
    labs(x="Date", y="Price")+
    ggtitle(plot_data$SYMBOL)
}

class(plots)
library(gridExtra) #Code suggested to create one pdf file.
pdf("test.pdf", onefile = TRUE)
for (i in seq(length(plots))){
  do.call("grid.arrange", plots[i])
}
dev.off()

library(Boruta)
head(dmart_data)

dmart_data[is.na(dmart_data)] <- "NOT_KNOWN"
boruta1 <- Boruta(OPEN~.,dmart_data, doTrace = 2)

boruta1$ImpHistory
print(boruta1)
plot(boruta1)
getConfirmedFormula(boruta1)

rf1 <- randomForest(OPEN ~ HIGH + LOW + CLOSE + LAST + PREVCLOSE + TOTTRDQTY + TOTTRDVAL + 
                      TIMESTAMP + TOTALTRADES, data = dmart_data)

test <- head(dmart_data)
predict(rf1, test)
test$OPEN

mae(rf1, dmart_data)
