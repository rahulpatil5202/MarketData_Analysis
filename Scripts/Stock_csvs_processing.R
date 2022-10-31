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
cleanColnames <- function (x)
  {
   colnames(x) <- gsub("[[:punct:]]+",'_', colnames(x))
   colnames(x) <- gsub("[[:punct:]]$",'', colnames(x))
   colnames(x) <- gsub("$[[:punct:]]",'', colnames(x))
   colnames(x) <- tolower(colnames(x))
   return(x)
  }


#Function to clean whitespaces
trimSpaces <- function(df)
  {
   for(i in seq_along(colnames(df)))
    {
    if(is.character(df[,i])==T)
      {
       df[,i] <- trimws(df[,i])
      }
    }
  return(df)
  }


#Function to archive processed files
rootCleanUp <- function()
  {
   cat('\n\nCleaning BSE root folder\n\n')
   bseZips <- list.files(path = 'D:/marketdata/bse_bhavcopies', pattern = "\\.zip$", full.names = T)
   file.copy(from = bseZips, to = 'D:/marketdata/bse_bhavcopies/Archive/', overwrite = T, copy.mode = T)
   file.remove(bseZips)
   bseCsvs <- list.files(path = 'D:/marketdata/bse_bhavcopies/unzipped_csvs', pattern = "\\.csv$|\\.CSV$", full.names = T)
   file.copy(from = bseCsvs, to = 'D:/marketdata/bse_bhavcopies/Archive/unzipped_csvs/', overwrite = T, copy.mode = T)
   file.remove(bseCsvs)
   cat('\n\nDone cleaning BSE root folder')
  
   cat('\n\nCleaning NSE root folder\n\n')
   nseZips <- list.files(path = 'D:/marketdata/nse_bhavcopies', pattern = "\\.zip$", full.names = T)
   file.copy(from = nseZips, to = 'D:/marketdata/nse_bhavcopies/Archive/', overwrite = T, copy.mode = T)
   file.remove(nseZips)
   nseCsvs <- list.files(path = 'D:/marketdata/nse_bhavcopies/unzipped_csvs', pattern = "\\.csv$|\\.CSV$", full.names = T)
   file.copy(from = nseCsvs, to = 'D:/marketdata/nse_bhavcopies/Archive/unzipped_csvs', overwrite = T, copy.mode = T)
   file.remove(nseCsvs)
   cat('\n\nDone cleaning NSE root folder')
  
   cat('\n\nCleaning NSE Indices root folder\n\n')
   nseIndCsvs <- list.files(path = 'D:/marketdata/nse_indices', pattern = "\\.csv$|\\.CSV$", full.names = T)
   file.copy(from = nseIndCsvs, to = 'D:/marketdata/nse_indices/Archive/', overwrite = T, copy.mode = T)
   file.remove(nseIndCsvs)
   cat('\n\nDone cleaning NSE Indices root folder')
  
   cat('\n\nCleaning NSE Sectoral Indices root folder\n\n')
   nseSecIndCsvs <- list.files(path = 'D:/marketdata/NSE_sectoral_Indices', pattern = "\\.csv$|\\.CSV$", full.names = T)
   file.copy(from = nseSecIndCsvs, to = 'D:/marketdata/NSE_sectoral_Indices/Archive/', overwrite = T, copy.mode = T)
   file.remove(nseSecIndCsvs)
   cat('\n\nDone cleaning NSE Sectoral Indices root folder')
  
  
   rm(bseZips,bseCsvs,nseZips,nseCsvs,nseIndCsvs)
  
  }


###########################################
## NSE Bhavcopy Processing for CSVs console

# get all the zip files
zipFiles <- file.info(list.files(path = "D:/marketdata/nse_bhavcopies", pattern = "\\.zip$", full.names = TRUE))
corruptZips <- zipFiles[zipFiles$size < 1000,]

#Delete corrupt files
unlink(row.names(corruptZips), recursive = F, force = F)

#Reset zip files after corrupt files deletion process
zipFiles <- file.info(list.files(path = "D:/marketdata/nse_bhavcopies", pattern = "\\.zip$", full.names = TRUE))

# unzip all your files
outpath <- "D:/marketdata/nse_bhavcopies/unzipped_csvs"
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

csvs_path <- "D:/marketdata/nse_indices"
csv_files <- file.info(list.files(path = csvs_path, pattern = "\\.csv$", full.names = T))
corrupt_csvs <- csv_files[csv_files$size < 1000,]
unlink(row.names(corrupt_csvs), recursive = F, force = F)

#Reset csv list after corrupt file deletion
csv_files <- list.files(path = csvs_path, pattern = "\\.csv$", full.names = T)
nse_indices_data <- ldply(.data = csv_files, function(x) read.csv(x, stringsAsFactors = F, as.is = T, check.names = T)) 
head(nse_indices_data)
str(nse_indices_data)
summary(nse_indices_data)

#change date and other formats
nse_indices_data$Index.Date <- lubridate::dmy(nse_indices_data$Index.Date)

numericFields <- colnames(nse_indices_data[,-c(1,2)])

for(i in numericFields)
  {
   nse_indices_data[,i] <- as.numeric(as.character(nse_indices_data[,i]))
  }


#Check NA values
na_count <- sapply(nse_indices_data, function(x) sum(length(which(is.na(x)))))
na_count <- data.frame(na_count)
na_count

#Dropping records having NA values in important columns
nse_indices_data <- nse_indices_data[!is.na(nse_indices_data$Open.Index.Value),]

rm(corrupt_csvs,na_count,csv_files,csvs_path,i,numericFields)

#######################################
## BSE bhavcopies processing to console

#Begin with deleting files with size less than 10 kb (invalid downloads)

## Get vector of all file names
zipFiles <- file.info(list.files(path="D:/marketdata/bse_bhavcopies",pattern="\\.zip$", full.names=TRUE))
## Extract vector of empty files' names
corrupt_zips <- zipFiles[zipFiles$size < 10000,]
corrupt_zips_names <- row.names(corrupt_zips)
## Remove empty files
unlink(corrupt_zips_names, recursive=TRUE, force=FALSE)

# get all the zip files again as deleted corrupt files

zipFiles <- file.info(list.files(path = "D:/marketdata/bse_bhavcopies", pattern = "\\.zip$", full.names = TRUE))

# unzip all your files
outpath <- "D:/marketdata/bse_bhavcopies/unzipped_csvs"
ldply(.data = row.names(zipFiles), .fun = unzip, exdir=outpath)

# get the csv files
csv_files <- list.files(path = outpath, pattern = "\\.CSV$")

# read the csv files
setwd("D:/marketdata/bse_bhavcopies/unzipped_csvs/")
getwd()

#Let's add a new column date using csvs file name to each csv
#Adding this column because some csv files mssing Trade Date column.
# We will use file name to generate trade date as file names have respective dates of trade in their names

for(i in seq_along(csv_files))
  {
   raw <- read.csv(csv_files[i], header = T, stringsAsFactors = F)
   raw$Trade_Date_New <- dmy(stringi::stri_sub(csv_files[i],-10,-5))
   write.csv(raw,csv_files[i],row.names = F)
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



#######################################
## NSE sectoral indices csvs processing to console

# get all the zip files again as deleted corrupt files

csv_files <- list.files(path = "D:/marketdata/nse_sectoral_indices/", pattern = "\\.csv$|\\.CSV$", full.names = TRUE)
sec_map <- read.csv("D:/marketdata/nse_sectoral_indices/downloadPath.txt", header = T, stringsAsFactors = F)
for(i in seq_along(csv_files))
  {
   index <- sec_map[which(csv_files[i] == sec_map$filepath),'index']
   raw <- read.csv(csv_files[i])
   raw$index_name <- index
   write.csv(raw,csv_files[i], row.names = F)
  }

NSE_sec_indices <- ldply(.data = csv_files, function(x) read.csv(x, header = T, stringsAsFactors = F, as.is = T, check.names = T))
NSE_sec_indices <- select(NSE_sec_indices, c("Company.Name","Industry","Symbol","Series","ISIN.Code","index_name"))
rm(corruptZips,na_count,csv_files, corrupt_csvs, zipFiles, i, outpath, corrupt_zips)


#Write csvs of all combined data and 
#write all combined csvs to PostgreSQL database clean column names and trim white spaces first

#Cleaning column names
NSE_stock_data <- cleanColnames(NSE_stock_data)
nse_indices_data <- cleanColnames(nse_indices_data)
BSE_stock_data <- cleanColnames(BSE_stock_data)
NSE_sec_indices <- cleanColnames(NSE_sec_indices)


#trim white spaces from character columns using defines trimSpaces function
NSE_stock_data <- trimSpaces(NSE_stock_data)
nse_indices_data <- trimSpaces(nse_indices_data)
BSE_stock_data <- trimSpaces(BSE_stock_data)
NSE_sec_indices <- trimSpaces(NSE_sec_indices)


#creating connection for postgresql
cn1 <- dbConnect(odbc::odbc(),dsn="RDSN")

#NSE csv
#Adding tables to postgreSQL
dbWriteTable(cn1, "nse", NSE_stock_data, row.names = F, append = T)

#mindate <- min(NSE_stock_data$trade_date)
#maxdate <- max(NSE_stock_data$trade_date)
#write.csv(NSE_stock_data,file = paste("D:/marketdata/NSE_",mindate,"-",maxdate,".csv", sep = ""))

#NSE Indices csv

dbWriteTable(cn1, "nse_indices",nse_indices_data, append = T,row.names = F)

#mindate <- min(nse_indices_data$index_date)
#maxdate <- max(nse_indices_data$index_date)
#write.csv(nse_indices_data,file = paste("D:/marketdata/nse_indices_",mindate,"-",maxdate,".csv", sep = ""), row.names = F)

#BSE csv
dbWriteTable(cn1, "bse", BSE_stock_data, append = T, row.names = F)

#mindate <- min(BSE_stock_data$trade_date_new)
#maxdate <- max(BSE_stock_data$trade_date_new)
#write.csv(BSE_stock_data,file = paste("D:/marketdata/BSE_",mindate,"-",maxdate,".csv", sep = ""))


#NSE Sectoral Indices
dbWriteTable(cn1, "nse_sectoralindices", NSE_sec_indices, append = F, row.names = F, overwrite = T)

#Let's clean root folders to archives
rootCleanUp()

dbDisconnect(cn1)
rm(list=ls())
