library(tidyverse)
library(RPostgreSQL)

cn1 <- dbConnect(PostgreSQL(), host='localhost', port=5432, user='rahul', password = 'postgres@123', dbname = 'data_science')

data <- dbGetQuery(cn1, 'select nse.trade_date, industryclass.sector_name, sum(nse.close) as sector_value from nse
inner join scrips on nse.isin = scrips.isin
                   left join industryclass on scrips.industry = industryclass.industry_subgroup
                   where nse.trade_date >= \'2018-04-01\' and industryclass.sector_name is not NULL
                   group by nse.trade_date, industryclass.sector_name
                   order by industryclass.sector_name, nse.trade_date')

#Visualize sctorwise trend
ggplot(data, aes(x=trade_date, y=sector_value))+
  geom_line()+
  geom_smooth(method = 'auto')+
  facet_wrap('sector_name', scales = "free_y",dir = 'v')



data2 <- dbGetQuery(cn1, 'select nse.trade_date, industryclass.sector_name, industryclass.industry_name,sum(nse.close) as industry_value from nse
inner join scrips on nse.isin = scrips.isin
left join industryclass on scrips.industry = industryclass.industry_subgroup
where nse.trade_date >= \'2018-04-01\' and industryclass.sector_name is not NULL
group by nse.trade_date, industryclass.sector_name, industryclass.industry_name
order by industryclass.sector_name, nse.trade_date')

#Visualize industrywise trend

ggplot(data2, aes(x=trade_date, y=industry_value))+
  geom_line()+
  geom_smooth(method = 'auto')+
  facet_wrap('industry_name', scales = "free_y",dir = 'v')


#Check on stocks specific to industry or sector. Change query to realign data

data3 <- dbGetQuery(cn1, 'select nse.trade_date, industryclass.sector_name, industryclass.industry_name,nse.symbol, nse.close from nse
inner join scrips on nse.isin = scrips.isin
left join industryclass on scrips.industry = industryclass.industry_subgroup
where industryclass.industry_name = \'Retailing\' and nse.trade_date >= \'2018-04-01\' and industryclass.sector_name is not NULL
group by nse.trade_date, industryclass.sector_name, industryclass.industry_name,nse.symbol, nse.close
order by nse.symbol,nse.trade_date')

ggplot(data3, aes(x=trade_date, y=close))+
  geom_line()+
  facet_wrap('symbol', scales = "free_y",dir = 'v')
