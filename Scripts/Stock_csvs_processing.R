library(plyr)
library(tidyverse)
library(modelr)
library(lubridate)
library(randomForest)
library(stringi)

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
NSE_stock_data <- ldply(.data = csv_files, .fun = read.csv)

head(NSE_stock_data)
str(NSE_stock_data)

anyDuplicated(NSE_stock_data,incomparables = F, nmax=1)

#Let's check NA's in our Stock data
na_count <-sapply(NSE_stock_data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

summary(NSE_stock_data)
str(NSE_stock_data)

#Change factors to continious variable types
NSE_stock_data$SYMBOL <- as.character(NSE_stock_data$SYMBOL)
NSE_stock_data$SERIES <- as.character(NSE_stock_data$SERIES)
NSE_stock_data$TIMESTAMP <- as.character(NSE_stock_data$TIMESTAMP)
NSE_stock_data$ISIN <- as.character(NSE_stock_data$ISIN)

#Change date column to proper formating

NSE_stock_data$TIMESTAMP <- dmy(NSE_stock_data$TIMESTAMP)
head(NSE_stock_data)



#####################################
## NSE Indices processing to console

csvs_path <- "E:/MarketData/NSE_Indices"
all_files <- file.info(list.files(path = csvs_path, pattern = "\\.csv$", full.names = T))
corrupt_files <- all_files[all_files$size < 1000,]
View(head(corrupt_files)) #row names are filenames in this dataframe
unlink(row.names(corrupt_files), recursive = F, force = F)

#Combine all csvs
csvs_list <- list.files(path = csvs_path, pattern = "\\.csv$", full.names = T)
NSE_Indices_data <- ldply(.data = csvs_list, function(x) read.csv(x, stringsAsFactors = F)) 
head(NSE_Indices_data)
str(NSE_Indices_data)
summary(NSE_Indices_data)

#change date format
NSE_Indices_data$Index.Date <- as.Date(NSE_Indices_data$Index.Date)

str(NSE_Indices_data)
#Check NAs
na_count <- sapply(NSE_Indices_data, function(x) sum(length(which(is.na(x)))))
na_count <- data.frame(na_count)
na_count

#Dropping records having NA values
NSE_Indices_data <- NSE_Indices_data[complete.cases(NSE_Indices_data),]


#######################################
## BSE bhavcopies processing to console

#Begin with deleting files with size less than 10 kb (invalid downloads)

## Get vector of all file names
all_files <- file.info(list.files(path="E:/MarketData/BSE_Bhavcopies",pattern=".zip", full.names=TRUE))
## Extract vector of empty files' names
corrupt_files <- all_files[all_files$size < 7000,]
corrupt_files_names <- row.names(corrupt_files)
## Remove empty files
unlink(corrupt_files_names, recursive=TRUE, force=FALSE)

# get all the zip files again as deleted corrupt files

zipF <- list.files(path = "E:/MarketData/BSE_Bhavcopies", pattern = "\\.zip$", full.names = TRUE)

# unzip all your files
outpath <- "E:/MarketData/BSE_Bhavcopies/unzipped_csvs"
ldply(.data = zipF, .fun = unzip, exdir=outpath)

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
  print(paste("Wrtting",i,"of", length(csv_files),"..Adding new column to ", csv_files[i]))
}


BSE_stock_data <- ldply(.data = csv_files, function(x) read.csv(x, header = T, stringsAsFactors = F))

head(BSE_stock_data)

#Let's check NA's in our Stock data
na_count <-sapply(BSE_stock_data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

#Dropping NA and useless coulmns
BSE_stock_data <- subset(BSE_stock_data, select = -c(TRADING_DATE,FILLER2,FILLER3,FILLER1))

summary(BSE_stock_data)
str(BSE_stock_data)

#change trading date to date format
BSE_stock_data$Trade_Date_New <- as.Date(BSE_stock_data$Trade_Date_New)

str(BSE_stock_data)

#Write csvs of all combined data

#NSE csv
mindate <- min(NSE_stock_data$TIMESTAMP)
maxdate <- max(NSE_stock_data$TIMESTAMP)
write.csv(NSE_stock_data,file = paste("E:/MarketData/NSE_",mindate,"-",maxdate,".csv", sep = ""))

#NSE Indices csv
mindate <- min(NSE_Indices_data$Index.Date)
maxdate <- max(NSE_Indices_data$Index.Date)
write.csv(NSE_Indices_data,file = paste("E:/MarketData/NSE_Indices_",mindate,"-",maxdate,".csv", sep = ""))

#BSE csv
mindate <- min(BSE_stock_data$Trade_Date_New)
maxdate <- max(BSE_stock_data$Trade_Date_New)
write.csv(BSE_stock_data,file = paste("E:/MarketData/BSE_",mindate,"-",maxdate,".csv", sep = ""))




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
