library(tidyverse)
library(lubridate)
library(stringi)
library(RPostgreSQL)


##Internet connection availability function
isInternetConnected <- function() {
  if (.Platform$OS.type == "windows") {
    ipmessage <- system("ipconfig", intern = TRUE)
  } else {
    ipmessage <- system("ifconfig", intern = TRUE)
  }
  validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
  any(grep(validIP, ipmessage))
}

isPingOk <- function(x, stderr = FALSE, stdout = FALSE, ...)
{
  pingvec <- system2("ping", x,
                     stderr = FALSE,
                     stdout = FALSE,...)
  if (pingvec == 0) TRUE else FALSE
}

nseDownload <- function(date_range)
{
  write(NULL,"E:/MarketData/NSE_Bhavcopies/NSE_Bhav_Failure.txt")
  cat('\n\nDownloading NSE Bhav copies now..\n\n')
  Sys.sleep(1)
  for(i in seq_along(date_range)){
    dd <- formatC(lubridate::day(date_range[i]),width = 2, flag = "0")
    mm <- toupper(months(as.Date(date_range[i]), abbr = T))
    yy <- lubridate::year(date_range[i])
    link <- paste('https://www1.nseindia.com/content/historical/EQUITIES/',yy,'/',mm,'/','cm',dd,mm,yy,'bhav.csv.zip', sep="")
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
  if(file.info(failure_file)$size != 0)
  {
    cat("\n\nRe-Attempting failed NSE Bhavcopy files\n\n")
    Sys.sleep(2)
    failed_data <- tryCatch(read.csv("E:/MarketData/NSE_Bhavcopies/NSE_Bhav_Failure.txt", stringsAsFactors = F, header = F), error=function(e) NULL)
    if(!is.null(failed_data))
      {
       failed_dates <- as.Date(failed_data$V2)
       write(NULL,"E:/MarketData/NSE_Bhavcopies/NSE_Bhav_Failure.txt")
       nseDownload(failed_dates)
      }
    else
      {
        cat("\n\nNothing to re-attempt\n\n") 
        write(NULL,"E:/MarketData/NSE_Bhavcopies/NSE_Bhav_Failure.txt")
      }
    }
  else
  {
      cat('\n\nNSE Bhavcopy download Completed\n\n')
      Sys.sleep(1)
    }
 
}




nseIndicesDownload <- function(date_range)
{
  write(NULL,"E:/MarketData/NSE_Indices/Failure.txt")
  cat('\n\nDownloading NSE Indices data now..\n\n')
  Sys.sleep(1)
  for (i in seq_along(date_range)){
    dd <- formatC(lubridate::day(date_range[i]),width = 2, flag = "0")
    mm <- formatC(lubridate::month(date_range[i]),width = 2, flag = "0")
    yy <- lubridate::year(date_range[i])
    link <- paste('https://www1.nseindia.com/content/indices/ind_close_all_',dd,mm,yy,'.csv', sep="")
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
  if(file.info(failure_file)$size != 0)
  {
    cat("\n\nRe-Attempting failed NSE Indices files\n\n")
    Sys.sleep(2)
    failed_data <- tryCatch(read.csv("E:/MarketData/NSE_Indices/Failure.txt", stringsAsFactors = F, header = F), error=function(e) NULL)
    if(!is.null(failed_data))
    {
      failed_dates <- as.Date(failed_data$V2)
      write(NULL,"E:/MarketData/NSE_Indices/Failure.txt")
      nseIndicesDownload(failed_dates)
    }
    else
    {
      cat("\n\nNothing to re-attempt\n\n")
      write(NULL,"E:/MarketData/NSE_Indices/Failure.txt")
    }
  }
  else
  {
      cat('\n\nNSE Indices data download Completed\n\n')
      Sys.sleep(1)
  }
    
}


bseDownload <- function(date_range)
{
  write(NULL,"E:/MarketData/BSE_Bhavcopies/BSE_Bhav_Failure.txt")
  cat('\n\nDownloading BSE Bhavcopies now..\n\n')
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
  cat('\n\nRe-attempting failed BSE Bhavcopy files\n\n')
  Sys.sleep(1)
  if(file.info(failure_file)$size != 0)
  {
    failed_data <- tryCatch(read.csv("E:/MarketData/BSE_Bhavcopies/BSE_Bhav_Failure.txt", stringsAsFactors = F, header = F), error=function(e) NULL)
    if(!is.null(failed_data))
    {
      failed_dates <- as.Date(failed_data$V2)
      write(NULL,"E:/MarketData/BSE_Bhavcopies/BSE_Bhav_Failure.txt")
      bseDownload(failed_dates)
    }
    else
    {
      cat("\n\nNothing to re-attempt\n\n")
      write(NULL,"E:/MarketData/BSE_Bhavcopies/BSE_Bhav_Failure.txt")
    }
    
  }
  else
  {
      cat('\n\nBSE Bhavcopy download Completed\n\n')
      Sys.sleep(1)
  }
}

nseScrips_Indices_Download <- function()
{
  cat('\n\nDownloading Sectoral indices and companies data now..\n\n')
  Sys.sleep(1)
    linkfile <- read.csv('E:/MarketData/NSE_Secotral_Indices/downloadPath.txt', header = T, stringsAsFactors = F)
    for(i in 1:nrow(linkfile)){
      link <- linkfile[i,1]
      filepath <- linkfile[i,3]
      tryCatch({
        download.file(url=link, destfile = filepath, method = 'curl',mode = "wb")
        downloadedMsg <- paste(filepath," Downloaded Sucessfully @ ",Sys.time(),sep = "")
        write(downloadedMsg,"E:/MarketData/NSE_Secotral_Indices/Sectoral_Indices_Success.txt",append = TRUE)
        
      }, error=function(err.msg){
        write(paste(weekdays(date_range[i],abbreviate=T),date_range[i],"File Failed @ ",Sys.time(),toString(err.msg),sep = ","),
              "E:/MarketData/NSE_Secotral_Indices/Sectoral_Indices_Failure.txt", append=TRUE)
      })
    }
}



## Starting main thread here

cn1 <- dbConnect(odbc::odbc(),dsn="RDSN")

maxdb_date_nse <- dbGetQuery(cn1, 'select max(trade_date) from nse')
nse_date_range <- seq.Date(maxdb_date_nse$max+1,today(),"days")
maxdb_date_nseIndices <- dbGetQuery(cn1, 'select max(index_date) from nse_indices')
nseIndices_date_range <- seq.Date(maxdb_date_nseIndices$max+1,today(),"days")

maxdb_date_bse <- dbGetQuery(cn1, 'select max(trade_date_new) from bse')
bse_date_range <- seq.Date(maxdb_date_bse$max+1,today(), "days")


## Test internet connection and start downloading reports
if(isInternetConnected() == T && isPingOk("nse.com") == T)
  {
    if(nse_date_range[1]<=today())
      {
      nseDownload(nse_date_range)
      }else
        {
        cat("\n\nNSE Database Upto Date\n\n") 
        Sys.sleep(2)
        }
    if(nseIndices_date_range[1]<=today())
      {
      nseIndicesDownload(nseIndices_date_range)
      }else
        {
        cat("\n\nNSE Indices Database Upto Date\n\n") 
        Sys.sleep(2)
        }
    if(bse_date_range[1]<=today())
      {
      bseDownload(bse_date_range)
      }else
        {
        cat("\n\nBSE Database Upto Date\n\n") 
        Sys.sleep(2)
        }
    nseScrips_Indices_Download()
}else{
  cat("\n\n")
  for(i in 1:4){
    cat("No internet connection available or NSE/BSE sites not reachanble to download reports...\n")
    Sys.sleep(1)
    }
  cat("\n\n Exiting...\n\n")
  cat ("Press [enter] to continue")
  line <- readline()
  #Sys.sleep(2)
}

## Explicitly call to download date range files
# report_days <- seq(as.Date("2018-07-25"),as.Date("2018-07-27"),"days")
# 
# nseDownload(report_days)
# nseIndicesDownload(report_days)
# bseDownload(report_days)
