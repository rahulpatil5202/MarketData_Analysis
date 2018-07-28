library(tidyverse)
library(RPostgreSQL)
library(ggvis)
library(plotly)

cn1 <- dbConnect(PostgreSQL(), host='localhost', port=5432, user='rahul', password = 'postgres@123', dbname = 'data_science')

data <- dbGetQuery(cn1, 'select nse.trade_date, industryclass.sector_name, sum(nse.close) as sector_value from nse
inner join scrips on nse.isin = scrips.isin
                   left join industryclass on scrips.industry = industryclass.industry_subgroup
                   where nse.trade_date >= \'2018-04-01\' and industryclass.sector_name is not NULL
                   group by nse.trade_date, industryclass.sector_name
                   order by industryclass.sector_name, nse.trade_date')

#Visualize sctorwise trend
p1 <- ggplot(data, aes(x=trade_date, y=sector_value))+
  geom_line()+
  geom_smooth(method = 'auto')+
  facet_wrap('sector_name', scales = "free_y",dir = 'h', ncol = 3)+
  ggtitle('Sector weight ~ Date')

ggplotly(p1)


data2 <- dbGetQuery(cn1, 'select nse.trade_date, industryclass.sector_name, industryclass.industry_name,sum(nse.close) as industry_value from nse
inner join scrips on nse.isin = scrips.isin
left join industryclass on scrips.industry = industryclass.industry_subgroup
where nse.trade_date >= \'2017-04-01\' and industryclass.sector_name is not NULL
group by nse.trade_date, industryclass.sector_name, industryclass.industry_name
order by industryclass.sector_name, nse.trade_date')

#Visualize industrywise trend

p2<- ggplot(data2, aes(x=trade_date, y=industry_value))+
  geom_line()+
  geom_smooth(method = 'auto')+
  facet_wrap('industry_name', scales = "free_y",dir = 'h', ncol = 3)+
  ggtitle('Industry weight ~ Date')

ggplotly(p2)

#Check on stocks specific to industry or sector. Change query to realign data

data3 <- dbGetQuery(cn1, 'select nse.trade_date, industryclass.sector_name, industryclass.industry_name,nse.symbol, nse.close from nse
inner join scrips on nse.isin = scrips.isin
left join industryclass on scrips.industry = industryclass.industry_subgroup
where industryclass.industry_name = \'Retailing\' and nse.trade_date >= \'2018-04-01\' and industryclass.sector_name is not NULL
group by nse.trade_date, industryclass.sector_name, industryclass.industry_name,nse.symbol, nse.close
order by nse.symbol,nse.trade_date')

ggplot(data3, aes(x=trade_date, y=close))+
  geom_line()+
  geom_smooth(method = 'auto')+
  facet_wrap('symbol', scales = "free_y",dir = 'v')+
  ggtitle('Industry specific stocks and performance')


#portfoliodata visualization
data4 <- dbGetQuery(cn1, 'select nse.trade_date, demat.scrip_code, demat.hold_value, (demat.dp_bal * nse.close) as currentValue, ROUND(((demat.dp_bal * nse.close) - demat.hold_value)::numeric,2) as profit, ROUND((((demat.dp_bal * nse.close) - demat.hold_value)/demat.hold_value * 100)::numeric,2) as profit_percent from demat
inner join nse on demat.isin = nse.isin
where nse.trade_date >= \'2017-09-01\'
                    group by demat.scrip_code, nse.trade_date, demat.hold_value, demat.dp_bal, nse.close
                    order by nse.trade_date desc, profit')

ggplot(data4, aes(x=trade_date, y=profit_percent))+
  geom_line()+
  geom_smooth(method = 'auto')+
  facet_wrap('scrip_code', scales = "free_y",dir = 'h')+
  ggtitle('Portfoli stocks and Performance')

#Check profit percent trend 
data5 <- dbGetQuery(cn1, 'select nse.trade_date, sum(demat.dp_bal * nse.close) as portfolioValue, ROUND((sum(demat.dp_bal * nse.close) - sum(demat.hold_value))::numeric,2) as profit, ROUND(((sum(demat.dp_bal * nse.close) - sum(demat.hold_value))/sum(demat.hold_value)*100)::numeric,2) as profit_percent, nse_indices.closing_index_value from demat
                    inner join nse on demat.isin = nse.isin
                    left join nse_indices on nse_indices.index_date = nse.trade_date
                    where nse_indices.index_name = \'Nifty 50\' and nse.trade_date >= \'2017-08-01\'
                    group by nse.trade_date, nse_indices.index_name, nse_indices.closing_index_value
                    order by nse.trade_date desc')

ggplot(data5, aes(x=trade_date, y=profit_percent))+
  geom_line()+
  geom_smooth(method = 'auto')+
  geom_hline(yintercept = 0, color = 'red', linetype='dashed')+
  geom_line(aes(y=closing_index_value/1000), color='blue')+
  ggtitle('Portfolio Profit % ~ Nifty Trend')


nse <- dbGetQuery(cn1,'select nse.trade_date, nse.symbol, nse.isin, nse.close, industryclass.sector_name, industryclass.industry_name  from nse
left join scrips on nse.isin = scrips.isin
left join industryclass on industryclass.industry_subgroup = scrips.industry
group by nse.symbol, nse.isin, nse.trade_date, nse.close, industryclass.sector_name, industryclass.industry_name
order by nse.trade_date desc, nse.symbol')

nse_days <- dbGetQuery(cn1,'select trade_date from nse group by trade_date order by trade_date desc')

d1 <- nse[nse$trade_date %in% nse_days[1,1],]
d60 <- nse[nse$trade_date %in% nse_days[60,1],]
d120 <- nse[nse$trade_date %in% nse_days[120,1],]
d180 <- nse[nse$trade_date %in% nse_days[180,1],]
d240 <- nse[nse$trade_date %in% nse_days[240,1],]
d300 <- nse[nse$trade_date %in% nse_days[300,1],]
d360 <- nse[nse$trade_date %in% nse_days[360,1],]

trend_data <- d1
trend_data$d60 <- d60[match(trend_data$isin, d60$isin),'close']
trend_data$d120 <- d120[match(trend_data$isin, d120$isin),'close']
trend_data$d180 <- d180[match(trend_data$isin, d180$isin),'close']
trend_data$d240 <- d240[match(trend_data$isin, d240$isin),'close']
trend_data$d300 <- d300[match(trend_data$isin, d300$isin),'close']
trend_data$d360 <- d360[match(trend_data$isin, d360$isin),'close']

trend_data <- trend_data[complete.cases(trend_data),]

trend_data$d60_prct <- round((trend_data$close - trend_data$d60)/trend_data$close * 100,2)
trend_data$d120_prct <- round((trend_data$close - trend_data$d120)/trend_data$close * 100,2)
trend_data$d180_prct <- round((trend_data$close - trend_data$d180)/trend_data$close * 100,2)
trend_data$d240_prct <- round((trend_data$close - trend_data$d240)/trend_data$close * 100,2)
trend_data$d300_prct <- round((trend_data$close - trend_data$d300)/trend_data$close * 100,2)
trend_data$d360_prct <- round((trend_data$close - trend_data$d360)/trend_data$close * 100,2)

d360_top10 <- top_n(trend_data, 10, wt = trend_data$d360_prct)
d60_top10 <- top_n(trend_data, 10, wt = trend_data$d60_prct)
ggplot(d360_top10, aes(x=reorder(symbol,d360_prct), y=d360_prct))+
  geom_col(stat='identity')+
  coord_flip()

ggplot(nse[nse$isin %in% d360_top10$isin,], aes(x=trade_date, y=close))+
  geom_line()+
  geom_smooth(method = 'auto')+
  facet_wrap('symbol', scales = "free_y")+
  ggtitle('Longterm 360+ trades toppers')



ggplot(nse[nse$isin %in% d60_top10$isin,], aes(x=trade_date, y=close))+
  geom_line()+
  geom_smooth(method = 'auto')+
  facet_wrap('symbol', scales = "free_y")+
  ggtitle('Short Term 60+ trades toppers')


industries_d60_top5 <- trend_data %>% 
  arrange(industry_name, desc(d60_prct)) %>%
  group_by(industry_name) %>%
  top_n(n=5,wt = d60_prct) # %>%
  #filter(row_number() <=5)


ggplot(industries_d60_top5, aes(x=reorder(symbol,d60_prct), y=d60_prct))+
  geom_col()+
  coord_flip()+
  facet_wrap('industry_name', scales = 'free_y')+
  ggtitle('Short Term 60+ trades toppers')



rm(list=ls())
dev.off()
