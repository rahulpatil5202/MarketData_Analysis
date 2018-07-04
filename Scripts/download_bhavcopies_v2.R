library(tidyverse)
library(lubridate)
library(stringi)


report_days <- seq(as.Date("2018-06-20"),as.Date("2018-07-03"),"days")

nseDownload <- function(date_range)
{
  message('\n\nDownloading NSE Bhav copies now..\n\n')
  Sys.sleep(1)
  for(i in seq_along(date_range)){
    dd <- formatC(lubridate::day(date_range[i]),width = 2, flag = "0")
    mm <- toupper(months(as.Date(date_range[i]), abbr = T))
    yy <- lubridate::year(date_range[i])
    link <- paste('https://www.nseindia.com/content/historical/EQUITIES/',yy,'/',mm,'/','cm',dd,mm,yy,'bhav.csv.zip', sep="")
    filename <- paste('cm',dd,mm,yy,'bhav.csv.zip',sep = "")
    filepath <- paste('E:/MarketData/NSE_Bhavcopies/', filename, sep="")
    tryCatch({
      download.file(url=link, destfile = filepath, method = 'curl',mode = "wb")
      downloadedMsg <- paste(weekdays(date_range[i],abbreviate=T),",",date_range[i],",",filepath," Downloaded Sucessfully @ ",Sys.time(),sep = "")
      write(downloadedMsg,"E:/MarketData/NSE_Bhavcopies/NSE_Bhav_Success.txt",append = TRUE)
      
    }, error=function(err.msg){
      write(paste(weekdays(date_range[i],abbreviate=T),date_range[i],"File Failed @ ",Sys.time(),toString(err.msg), sep = ","), 
            "E:/MarketData/NSE_Bhavcopies/NSE_Bhav_Failure.txt", append=TRUE)
    })
    
  }
  nseReattemptDownload("E:/MarketData/NSE_Bhavcopies/NSE_Bhav_Failure.txt")
}

nseReattemptDownload <- function(failure_file)
{
  while(!is.null(failure_file))
  {
    message("\n\nRe-Attempting failed NSE Bhavcopy files\n\n")
    Sys.sleep(2)
    failed_data <- read.csv("E:/MarketData/NSE_Bhavcopies/NSE_Bhav_Failure.txt", stringsAsFactors = F, header = F)
    failed_dates <- as.Date(failed_data$V2)
    write(NULL,"E:/MarketData/NSE_Bhavcopies/NSE_Bhav_Failure.txt")
    nseDownload(failed_dates)
  }
 
}




nseIndicesDownload <- function(date_range)
{
  message('\n\nDownloading NSE Indices data now..\n\n')
  Sys.sleep(1)
  for (i in seq_along(date_range)){
    dd <- formatC(lubridate::day(date_range[i]),width = 2, flag = "0")
    mm <- formatC(lubridate::month(date_range[i]),width = 2, flag = "0")
    yy <- lubridate::year(date_range[i])
    link <- paste('https://www.nseindia.com/content/indices/ind_close_all_',dd,mm,yy,'.csv', sep="")
    filename <- paste('ind_close_all_',dd,mm,yy,'.csv',sep = "")
    filepath <- paste('E:/MarketData/NSE_Indices/', filename, sep="")
    tryCatch({
      download.file(url=link, destfile = filepath, method = 'curl',mode = "wb")
      downloadedMsg <- paste(weekdays(date_range[i],abbreviate=T),",",date_range[i],",",filepath," Downloaded Sucessfully @ ",Sys.time(),sep = "")
      write(downloadedMsg,"E:/MarketData/NSE_Indices/Success.txt",append = TRUE)
      
    }, error=function(err.msg){
      write(paste(weekdays(date_range[i],abbreviate=T),date_range[i],"File Failed @ ",Sys.time(),toString(err.msg), sep = ","), 
            "E:/MarketData/NSE_Indices/Failure.txt", append=TRUE)
    })
  }
  nseIndicesReattemptDownload("E:/MarketData/NSE_Indices/Failure.txt")
}

nseIndicesReattemptDownload <- function(failure_file)
{
  while(!is.null(failure_file))
  {
    message("\n\nRe-Attempting failed NSE Indices files\n\n")
    Sys.sleep(2)
    failed_data <- read.csv("E:/MarketData/NSE_Indices/NSE_Bhav_Failure.txt", stringsAsFactors = F, header = F)
    failed_dates <- as.Date(failed_data$V2)
    write(NULL,"E:/MarketData/NSE_Indices/NSE_Bhav_Failure.txt")
    nseIndicesDownload(failed_dates)
  }
    
}


bseDownload <- function(date_range)
{
  message('\n\nDownloading BSE Bhavcopies now..\n\n')
  Sys.sleep(1)
  for (i in seq_along(date_range)){
    dd <- formatC(lubridate::day(date_range[i]),width = 2, flag = "0")
    mm <- formatC(lubridate::month(date_range[i]),width = 2, flag = "0")
    yy <- stringi::stri_sub(lubridate::year(date_range[i]),-2,-1)
    link <- paste('https://www.bseindia.com/download/BhavCopy/Equity/EQ_ISINCODE_',dd,mm,yy,'.zip', sep="")
    filename <- paste('EQ_ISINCODE_',dd,mm,yy,'.zip',sep = "")
    filepath <- paste('E:/MarketData/BSE_Bhavcopies/', filename, sep="")
    tryCatch({
      download.file(url=link, destfile = filepath, method = 'curl',mode = "wb")
      downloadedMsg <- paste(weekdays(date_range[i],abbreviate=T),",",date_range[i],",",filepath," Downloaded Sucessfully @ ",Sys.time(),sep = "")
      write(downloadedMsg,"E:/MarketData/BSE_Bhavcopies/BSE_Bhav_Success.txt",append = TRUE)
      
    }, error=function(err.msg){
      write(paste(weekdays(date_range[i],abbreviate=T),date_range[i],"File Failed @ ",Sys.time(),toString(err.msg),sep = ","),
            "E:/MarketData/BSE_Bhavcopies/BSE_Bhav_Failure.txt", append=TRUE)
    })
  }
  bseReattemptDownload("E:/MarketData/BSE_Bhavcopies/BSE_Bhav_Failure.txt")
}


bseReattemptDownload <- function(failure_file)
{
  message('\n\nRe-attempting failed BSE Bhavcopy files\n\n')
  Sys.sleep(1)
  while(!is.null(failure_file))
  {
    failed_data <- read.csv("E:/MarketData/BSE_Bhavcopies/BSE_Bhav_Failure.txt", stringsAsFactors = F, header = F)
    failed_dates <- as.Date(failed_data$V2)
    bseDownload(failed_dates)
  }
}


nseDownload(report_days)
nseIndicesDownload(report_days)
bseDownload(report_days)