library(tidyverse)
library(RPostgreSQL)


cn1 <- dbConnect(PostgreSQL(), 'localhost',5432,user='rahul',password='postgres@123', dbname='data_science')


data <- dbGetQuery(cn1, paste('select nse.trade_date, sum(demat.dp_bal * nse.close) as portfolioValue, nse_indices.closing_index_value from demat
inner join nse on demat.isin = nse.isin
left join nse_indices on nse_indices.index_date = nse.trade_date
group by nse.trade_date, nse_indices.index_name, nse_indices.closing_index_value
having nse_indices.index_name=\'Nifty 50\'
order by nse.trade_date desc'))

head(data)

ggplot(data = data, aes(x=trade_date, y=portfoliovalue))+
  geom_line()+
  geom_smooth(method = 'auto')+
  geom_line(aes(x=trade_date, y=closing_index_value), color='red')

