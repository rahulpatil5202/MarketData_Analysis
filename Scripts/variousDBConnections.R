library(RPostgreSQL)
library(RODBC)
library(tidyverse)
library(lubridate)
library(corrplot)

#connecting to postgreSQL

cn1 <- dbConnect(odbc::odbc(),dsn="RDSN")

sbilife <- dbGetQuery(cn1, "select nse.trade_date, demat.exchange,demat.scrip_code, demat.isin, demat.dp_bal as current_dpBal, demat.hold_price as dp_holdprice, nse.close, nse.prevclose, 
nse.tottrdqty,nse.tottrdval, nse_indices.index_name, nse_indices.closing_index_value from demat
                      inner join nse on demat.isin = nse.isin
                      left outer join nse_indices on nse.trade_date = nse_indices.index_date
                      where nse_indices.index_name = 'Nifty 50' and nse.symbol = 'SBILIFE'
                      order by nse.trade_date, demat.scrip_code")


ggplot(sbilife, aes(x=trade_date, y=close))+
  geom_line(stat = 'identity')+
  geom_line(aes(y=sbilife$closing_index_value/15, color='red'))


# connecting to MSSQLSERVER
cn2 <- odbcConnect(dsn = 'localSQL', uid = 'rahul', pwd = 'sql@123') #localSQL dsn created in windows to connect to DB

x <- sqlQuery(channel = cn2, query = 'select * from dbo.demat')
sqlSave(cn2, )


#Connecting to remote azure VM SQLSERVER

cn3 <- odbcConnect(dsn = 'azureSQL', uid = 'rahul', pwd = 'azuresql@123') #localSQL dsn created in windows to connect to DB
sqlQuery(channel = cn3, query = 'select * from dbo.demat')



