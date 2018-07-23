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


#portfoliodata visualization
data4 <- dbGetQuery(cn1, 'select nse.trade_date, demat.scrip_code, demat.hold_value, (demat.dp_bal * nse.close) as currentValue, ROUND(((demat.dp_bal * nse.close) - demat.hold_value)::numeric,2) as profit, ROUND((((demat.dp_bal * nse.close) - demat.hold_value)/demat.hold_value * 100)::numeric,2) as profit_percent from demat
inner join nse on demat.isin = nse.isin
where nse.trade_date >= \'2017-09-01\'
                    group by demat.scrip_code, nse.trade_date, demat.hold_value, demat.dp_bal, nse.close
                    order by nse.trade_date desc, profit')

ggplot(data4, aes(x=trade_date, y=profit_percent))+
  geom_line()+
  geom_smooth(method = 'auto')+
  facet_wrap('scrip_code', scales = "free_y",dir = 'h')
