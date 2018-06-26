library(tidyverse)
library(lubridate)
library(stringi)

## Let's first download NSE Bhavcopies from BSE India's official website...
NSE_Bhav_Days <- seq(as.Date("2018-05-29"),as.Date("2018-06-25"),"days")

#######################################################################################################
# # un comment below code for retrying failure dates and run from below till for loop end
# NSE_Bhav_Days <- read.csv("E:/MarketData/NSE_Bhavcopies/NSE_Bhav_Failure.txt", stringsAsFactors = F, header = F)
# str(NSE_Bhav_Days)
# NSE_Bhav_Days$V2 <- as.Date(NSE_Bhav_Days$V2)
# NSE_Bhav_Days <- NSE_Bhav_Days$V2
# write(NULL,"E:/MarketData/NSE_Bhavcopies/NSE_Bhav_Failure.txt")
#######################################################################################################


for(i in seq_along(NSE_Bhav_Days)){
  dd <- formatC(lubridate::day(NSE_Bhav_Days[i]),width = 2, flag = "0")
  mm <- toupper(months(as.Date(NSE_Bhav_Days[i]), abbr = T))
  yy <- lubridate::year(NSE_Bhav_Days[i])
  link <- paste('https://www.nseindia.com/content/historical/EQUITIES/',yy,'/',mm,'/','cm',dd,mm,yy,'bhav.csv.zip', sep="")
  filename <- paste('cm',dd,mm,yy,'bhav.csv.zip',sep = "")
  filepath <- paste('E:/MarketData/NSE_Bhavcopies/', filename, sep="")
  tryCatch({
    download.file(url=link, destfile = filepath, method = 'curl',mode = "wb")
    downloadedMsg <- paste(weekdays(NSE_Bhav_Days[i],abbreviate=T),",",NSE_Bhav_Days[i],",",filepath," Downloaded Sucessfully @ ",Sys.time(),sep = "")
    write(downloadedMsg,"E:/MarketData/NSE_Bhavcopies/NSE_Bhav_Success.txt",append = TRUE)
    
  }, error=function(err.msg){
    write(paste(weekdays(NSE_Bhav_Days[i],abbreviate=T),NSE_Bhav_Days[i],"File Failed @ ",Sys.time(),toString(err.msg), sep = ","), 
          "E:/MarketData/NSE_Bhavcopies/NSE_Bhav_Failure.txt", append=TRUE)
  })

}





## Let's now download NSE All Indices Daily Report

#Here starts original start and end dates of downloading data
NSE_Indices_Days <- seq(as.Date("2018-05-28"),as.Date("2018-06-25"),"days")

# #######################################################################################################
# # un comment below code for retrying failure dates. let's start with import from failure log
# NSE_Indices_Days <- c()
# NSE_Indices_Days <- read.csv("E:/MarketData/NSE_Indices/NSE_Bhav_Failure.txt", stringsAsFactors = F, header = F)
# str(NSE_Indices_Days)
# NSE_Indices_Days$V2 <- as.Date(NSE_Indices_Days$V2)
# NSE_Indices_Days <- NSE_Indices_Days$V2
# write(NULL,"E:/MarketData/NSE_Indices/NSE_Bhav_Failure.txt")
#######################################################################################################

for (i in seq_along(NSE_Indices_Days)){
  dd <- formatC(lubridate::day(NSE_Indices_Days[i]),width = 2, flag = "0")
  mm <- formatC(lubridate::month(NSE_Indices_Days[i]),width = 2, flag = "0")
  yy <- lubridate::year(NSE_Indices_Days[i])
  link <- paste('https://www.nseindia.com/content/indices/ind_close_all_',dd,mm,yy,'.csv', sep="")
  filename <- paste('ind_close_all_',dd,mm,yy,'.csv',sep = "")
  filepath <- paste('E:/MarketData/NSE_Indices/', filename, sep="")
  tryCatch({
    download.file(url=link, destfile = filepath, method = 'curl',mode = "wb")
    downloadedMsg <- paste(weekdays(NSE_Indices_Days[i],abbreviate=T),",",NSE_Indices_Days[i],",",filepath," Downloaded Sucessfully @ ",Sys.time(),sep = "")
    write(downloadedMsg,"E:/MarketData/NSE_Indices/Success.txt",append = TRUE)
    
  }, error=function(err.msg){
    write(paste(weekdays(NSE_Indices_Days[i],abbreviate=T),NSE_Indices_Days[i],"File Failed @ ",Sys.time(),toString(err.msg), sep = ","), 
          "E:/MarketData/NSE_Indices/Failure.txt", append=TRUE)
  })
}





## Let's now download BSE bhavcopies

## Set from and to date to download files
BSE_Bhavdays <- seq(as.Date("2018-05-28"),as.Date("2018-06-25"),"days")

# # ##############################################################################################
# # #un comment below block of code for retrying failure dates. let's start with import from failure log
# BSE_Bhavdays <- c()
# BSE_Bhavdays <- read.csv("E:/MarketData/BSE_Bhavcopies/BSE_Bhav_Failure.txt", stringsAsFactors = F, header = F)
# str(BSE_Bhavdays)
# BSE_Bhavdays$V2 <- as.Date(BSE_Bhavdays$V2)
# BSE_Bhavdays <- BSE_Bhavdays$V2
# write(NULL,"E:/MarketData/BSE_Bhavcopies/BSE_Bhav_Failure.txt")
###############################################################################################

for (i in seq_along(BSE_Bhavdays)){
  dd <- formatC(lubridate::day(BSE_Bhavdays[i]),width = 2, flag = "0")
  mm <- formatC(lubridate::month(BSE_Bhavdays[i]),width = 2, flag = "0")
  yy <- stringi::stri_sub(lubridate::year(BSE_Bhavdays[i]),-2,-1)
  link <- paste('https://www.bseindia.com/download/BhavCopy/Equity/EQ_ISINCODE_',dd,mm,yy,'.zip', sep="")
  filename <- paste('EQ_ISINCODE_',dd,mm,yy,'.zip',sep = "")
  filepath <- paste('E:/MarketData/BSE_Bhavcopies/', filename, sep="")
  tryCatch({
    download.file(url=link, destfile = filepath, method = 'curl',mode = "wb")
    downloadedMsg <- paste(weekdays(BSE_Bhavdays[i],abbreviate=T),",",BSE_Bhavdays[i],",",filepath," Downloaded Sucessfully @ ",Sys.time(),sep = "")
    write(downloadedMsg,"E:/MarketData/BSE_Bhavcopies/BSE_Bhav_Success.txt",append = TRUE)

  }, error=function(err.msg){
    write(paste(weekdays(BSE_Bhavdays[i],abbreviate=T),BSE_Bhavdays[i],"File Failed @ ",Sys.time(),toString(err.msg),sep = ","),
          "E:/MarketData/BSE_Bhavcopies/BSE_Bhav_Failure.txt", append=TRUE)
  })
}

