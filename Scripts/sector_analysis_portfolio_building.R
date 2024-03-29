library(tidyverse)
library(RPostgreSQL)
library(ggvis)
library(plotly)
library(ggthemes)
library(reshape)
library(htmlwidgets)
library(odbc)


cn1 <- dbConnect(odbc::odbc(),dsn="RDSN")


##Nse indices analysis and visualization

sec_indices <- read.csv("E:/MarketData/NSE_Secotral_Indices/downloadPath.txt", stringsAsFactors = F)
sec_indices <- sec_indices$index

sec_indices_data <- dbGetQuery(cn1, 'select index_name,index_date,change,closing_index_value from nse_indices')
sec_indices_data <- sec_indices_data[which(sec_indices_data$index_name %in% sec_indices),]

sec_ind_trend <- sec_indices_data%>%select(index_name,index_date,change)%>%
  group_by(index_name)%>%
  arrange(index_date)%>%
  mutate(cum_change = cumsum(change))%>%
  filter(change < quantile(change,0.99999))%>%
  filter(change > quantile(change,0.00001))%>%
  select(index_name,index_date,change,cum_change)

stats_sec_indices <- sec_ind_trend%>%filter(index_date >= "2017-01-01")%>%
  group_by(index_name)%>%
  summarise(q1=quantile(change,.25), mean=mean(change),meadin=median(change),q3=quantile(change,0.75), std = sd(change))


## %Chnage trend of each index
sec_chart <- ggplot(sec_ind_trend, aes(x=index_date, y=cum_change, color=index_name))+
  geom_line()+
  facet_wrap("index_name")+
  geom_hline(yintercept=0,color="red", linetype="dotted", size=1)+
  scale_x_date(date_breaks = "6 months", date_labels = "%b %y")+
  xlab("")+
  ylab("")+
  theme(plot.title = element_text(margin = margin(t = 0,b = 100,l = 0,r = 0,unit = "pt")))+
  theme(axis.text.x = element_text(angle = -30))+
  theme(legend.position = "none")+
  ggtitle("Nifty Sectoral Indices - Trend (% Change)")

ggplotly(sec_chart,height = 500,width = 900)




## Visualizing Standard deviation distribution

sd_plot2_indices<-ggplot(data=stats_sec_indices, aes(x=reorder(index_name,std), y=std, fill=index_name))+
  geom_bar(stat="identity")+
  coord_flip()+
  ggtitle("Index and Standard Deviation")

ggplotly(sd_plot2_indices,height = 500,width = 900)


sd_plot_indices <- ggplot(data=sec_ind_trend, aes(x=index_name, y=change, fill=index_name))+
  geom_violin()+
  stat_summary(fun.y=sd, geom = "point", shape=3, size=2, fill="black")+
  theme(axis.text.x = element_text(angle = -30))+
  xlab("")+
  ylab("")+
  geom_hline(yintercept = 0, color="red", linetype="dotted", size=1)+
  theme(legend.position = "none")+
  ggtitle("%Change distribution with Standard deviation")

ggplotly(sd_plot_indices,height = 500,width = 900)

sd_plot3_indices_cum <- ggplot(data=sec_ind_trend, aes(x=index_name, y=cum_change, fill=index_name))+
  geom_violin()+
  stat_summary(fun.y=sd, geom="point", shape=3, size=2, fill="black")+
  theme(axis.text.x = element_text(angle=-30))+
  geom_hline(yintercept = 0, color="red",linetype="dotted", size=0.6)+
  ggtitle("Cumulative %Change distribution with Standard deviation")

ggplotly(sd_plot3_indices_cum,height = 500,width = 900)


sd_plot4_indices <- ggplot(data=sec_ind_trend, aes(x=index_name, y=change))+
  geom_boxplot(aes(fill=index_name))+
  stat_summary(fun.y=sd, geom="point", shape=5, size=2)+
  stat_summary(fun.y=mean, geom="point",shape=1,size=2)+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = -30))+
  xlab("")+
  ylab("")+
  ggtitle("Indices Stats")

ggplotly(sd_plot4_indices,height = 700,width = 1100)


#Datewise sector weight from SQL

data <- dbGetQuery(cn1, 'select nse.trade_date, industryclass.sector_name, sum(nse.close) as sector_value from nse
                   inner join scrips on nse.isin = scrips.isin
                   left join industryclass on scrips.industry = industryclass.industry_subgroup
                   where nse.trade_date >= \'2017-04-01\' and industryclass.sector_name is not NULL
                   group by nse.trade_date, industryclass.sector_name
                   order by industryclass.sector_name, nse.trade_date')


#Visualize sctorwise trend

p1 <- ggplot(data, aes(x=trade_date, y=sector_value))+
  geom_line()+
  geom_smooth(method = 'auto')+
  facet_wrap('sector_name', scales = "free_y",dir = 'h', ncol = 3)+
  theme(strip.text = element_text(size=8, face = "bold"))+
  xlab("")+
  ylab("")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y")+
  theme(axis.text.x = element_text(angle = -90, size = 7))+
  theme(axis.text.y = element_text(size = 7))+
  theme(panel.spacing = unit(10,"points"))+
  theme(strip.text = element_text(size = 7))+
  ggtitle('Sector weight ~ Date')


ggplotly(p1, height = 700, width = 1100)


#Datewise secctor-Industry weight from SQL

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
  facet_wrap('industry_name', scales = "free_y",dir = 'h', ncol = 4)+
  theme(strip.text = element_text(size=8, face = "bold"))+
  xlab("")+
  ylab("")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y")+
  theme(axis.text.x = element_text(size = 7, angle = -30))+
  theme(axis.text.y = element_text(size = 7))+
  ggtitle('Industry weight ~ Date')

ggplotly(p2, height = 1200,width = 1100)


#Check on stocks specific to industry or sector. Change query to realign data

data3 <- dbGetQuery(cn1, 'select nse.trade_date, industryclass.sector_name, industryclass.industry_name,nse.symbol, nse.close from nse
                    inner join scrips on nse.isin = scrips.isin
                    left join industryclass on scrips.industry = industryclass.industry_subgroup
                    where industryclass.industry_name = \'Retailing\' and nse.trade_date >= \'2017-04-01\' and industryclass.sector_name is not NULL
                    group by nse.trade_date, industryclass.sector_name, industryclass.industry_name,nse.symbol, nse.close
                    order by nse.symbol,nse.trade_date')

p3 <- ggplot(data3, aes(x=trade_date, y=close))+
  geom_line()+
  geom_smooth(method = 'auto')+
  facet_wrap('symbol', scales = "free_y", ncol = 4)+
  xlab("")+
  ylab("")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y")+
  theme(axis.text.x = element_text(size = 8, angle = -30))+
  theme(axis.text.y = element_text(size = 8))+
  theme(panel.spacing = unit(0.2,"cm"))+
  ggtitle(paste(data3$industry_name[1]," Sector stocks performace"))

ggplotly(p3, height = 700, width = 1100)


#portfoliodata visualization

data4 <- dbGetQuery(cn1, 'select nse.trade_date, demat.scrip_code, demat.hold_value, (demat.dp_bal * nse.close) as currentValue, ROUND(((demat.dp_bal * nse.close) - demat.hold_value)::numeric,2) as profit, ROUND((((demat.dp_bal * nse.close) - demat.hold_value)/demat.hold_value * 100)::numeric,2) as profit_percent from demat
                    inner join nse on demat.isin = nse.isin
                    where nse.trade_date >= \'2017-09-01\'
                    group by demat.scrip_code, nse.trade_date, demat.hold_value, demat.dp_bal, nse.close
                    order by nse.trade_date desc, profit')

p4 <- ggplot(data4, aes(x=trade_date, y=profit_percent))+
  geom_line()+
  geom_smooth(method = 'auto')+
  geom_hline(yintercept = 0, size = 0.3, linetype="dotted", color="red")+
  facet_wrap('scrip_code', scales = "free_y",ncol = 5)+
  xlab("")+
  ylab("")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y")+
  theme(axis.text.x = element_text(size = 8, angle = -30))+
  theme(axis.text.y = element_text(size = 8))+
  ggtitle('Portfolio stocks - Datewise Profit Percent Trend')

ggplotly(p4, height = 700, width = 1000)

portfolio_gains <- dbGetQuery(cn1, "select nse.trade_date, nse.symbol, nse.close, sum(demat.hold_value)as investedvalue,(demat.dp_bal * nse.close) as currentvalue,
ROUND(((demat.dp_bal * nse.close) - lag((demat.dp_bal * nse.close)) over(order by symbol, trade_date))::numeric,2) as days_gain,
                              ROUND((((demat.dp_bal * nse.close) - lag((demat.dp_bal * nse.close)) over(order by symbol, trade_date))/lag((demat.dp_bal * nse.close)) over(order by symbol, trade_date))::numeric,2) as days_gain_percent,
                              ROUND(((demat.dp_bal * nse.close) - sum(demat.hold_value))::numeric,2) as overallgain,
                              ROUND((((demat.dp_bal * nse.close) - sum(demat.hold_value))/sum(demat.hold_value)*100)::numeric,2) as overallgain_percent,
                              nse_indices.closing_index_value as Nifty,
                              ROUND((nse_indices.closing_index_value - lag(nse_indices.closing_index_value) over (order by symbol, trade_date))::numeric,2) as niftychange,
                              ROUND((((nse_indices.closing_index_value) - lag(nse_indices.closing_index_value) over (order by symbol, trade_date))/(lag(nse_indices.closing_index_value) over (order by symbol, trade_date))*100)::numeric,2) as niftychange_percent
                              from nse
                              inner join demat on demat.isin = nse.isin
                              left join nse_indices on nse_indices.index_date = nse.trade_date
                              where nse_indices.index_name = 'Nifty 50' and nse.trade_date >= \'2017-09-01\'
                              group by nse.trade_date, nse.symbol, nse.close, demat.dp_bal,nse_indices.closing_index_value
                              order by nse.trade_date desc, nse.symbol")


portfolio_gains <- drop_na(portfolio_gains)



# for(i in seq_along(portfolio_gains$days_gain))
# {
#   if(portfolio_gains$days_gain[i] <= 0){
#     portfolio_gains$growthType[i] <- "Negative"
#   }else{
#     portfolio_gains$growthType[i] <- "Positive"
#   }
# }

portfolio_gains$growthType <- sapply(portfolio_gains$days_gain, function(x) if(x<=0){"Negative"}else{"Positive"})


pos_neg <- portfolio_gains%>%select(symbol, growthType)%>%
  group_by(symbol,growthType)%>%
  summarise(cnt=n())%>%
  arrange(growthType,-cnt)


pos_neg_p <- ggplot(pos_neg[which(pos_neg$growthType=="Negative"),], aes(x=reorder(symbol, cnt), y=cnt, fill="lightred"))+
  geom_bar(stat="identity")+
  coord_flip()+
  theme(legend.position = "none")+
  ggtitle("No. of days Stock gave negative returns")

ggplotly(pos_neg_p, height = 500, width = 900)


## Lets plot violin with std and mean





##calculate positive negative drag

p_n_drag <- portfolio_gains%>%select(symbol, days_gain_percent, growthType)%>%
  group_by(symbol,growthType)%>%
  arrange(desc(days_gain_percent))%>%
  slice(2:n())%>%   ## dropping first row as it might contain outliers due to non availability of data on earlier dates
  summarise(total=sum(days_gain_percent))
  

    
p_n_drag$total <- abs(p_n_drag$total)

p_n_drag <- p_n_drag%>%arrange(desc(growthType), desc(total))


p_n_drag_chart <- ggplot(p_n_drag, aes(x=reorder(symbol, -total), y=total, fill=growthType))+
  geom_bar(stat="identity", position = "fill")+
  coord_flip()+
  labs(x="",y="",title="Positive and Negative Drag")

ggplotly(p_n_drag_chart, height = 600, width = 1100)


## Check culprits dragging portfolio even when nifty change is positive

culprit_stocks <- portfolio_gains[which(portfolio_gains$days_gain < 0 & portfolio_gains$niftychange > 0),] 

culprit_stocks_stats <- culprit_stocks%>%select(symbol, days_gain_percent, overallgain)%>%
  group_by(symbol)%>%
  summarise(times_defaulted_nifty=n(),lossPercent_during_default=sum(days_gain_percent), loss_during_default = sum(overallgain[1]))



ggplot(culprit_stocks, aes(x=symbol))+
  geom_bar()+
  coord_flip()


## Last 10 days performance

last_10d_p <- ggplot(portfolio_gains%>%select(trade_date, symbol, growthType)%>%
  group_by(symbol)%>%
  top_n(trade_date,n = 10), aes(x=factor(trade_date), y=symbol, color=growthType))+
  geom_point(shape=15,size=9)

last_10d_p





## Daily portfolio value, profit % and Nifty 50 closing index

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


#Check portfolio days gain % against Nifty gain %

data6 <- dbGetQuery(cn1, 'select nse.trade_date, sum(demat.dp_bal * nse.close) as portfolioValue,(ROUND((sum(demat.dp_bal * nse.close) - sum(demat.hold_value))::numeric,2)) - lag(ROUND((sum(demat.dp_bal * nse.close) - sum(demat.hold_value))::numeric,2)) over (order by trade_date) as days_gain,ROUND(((ROUND((sum(demat.dp_bal * nse.close) - sum(demat.hold_value))::numeric,2)) - lag(ROUND((sum(demat.dp_bal * nse.close) - sum(demat.hold_value))::numeric,2)) over (order by trade_date))*100/(sum(demat.hold_value))::numeric,2) as days_gain_percent,ROUND((sum(demat.dp_bal * nse.close) - sum(demat.hold_value))::numeric,2) as overall_gain,ROUND(((sum(demat.dp_bal * nse.close) - sum(demat.hold_value))/sum(demat.hold_value)*100)::numeric,2) as overall_gain_percent, nse_indices.closing_index_value as nifty, ROUND((((nse_indices.closing_index_value - lag(nse_indices.closing_index_value) over (order by nse.trade_date))/nse_indices.closing_index_value)*100)::numeric,2) as nifty_change_percent  from demat
                    inner join nse on demat.isin = nse.isin
                    left join nse_indices on nse_indices.index_date = nse.trade_date
                    group by nse.trade_date, nse_indices.index_name, nse_indices.closing_index_value
                    having nse_indices.index_name = \'Nifty 50\' and nse.trade_date > \'2017-08-01\'
                    order by nse.trade_date desc')

data6[is.na(data6)]<- 0

sub1_data6 <- data6[,c(1,4,8)]
sub1_data6 <- sub1_data6%>%
  arrange(trade_date)%>%
  mutate(Portfolio_Gain_Percent = cumsum(days_gain_percent), Nifty_Gain_Percent = cumsum(nifty_change_percent))%>%
  select(trade_date, Portfolio_Gain_Percent, Nifty_Gain_Percent)

sub1_data6 <- melt(sub1_data6, id=c("trade_date"))



p6<-ggplot(sub1_data6, aes(x=trade_date, y=value, color=variable))+
  geom_line(size=0.7)+
  xlab("Trade Date")+
  ylab("Gain %")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y")+
  ggtitle("Daily Portfolio Gain % ~ Nifty Change %")+
  theme_economist()



ggplotly(p6, width = 1000, height = 400)
### Analysing current trends


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

p7 <- ggplot(d360_top10, aes(x=reorder(symbol,d360_prct), y=d360_prct, fill=symbol))+
  geom_col()+
  coord_flip()+
  ggtitle("Top 10 Performers - Long Term 360+ Trades")+
  xlab("")+
  ylab("")

ggplotly(p7, height = 400, width = 800)


LT_Top10_Trend <- ggplot(nse[nse$isin %in% d360_top10$isin,], aes(x=trade_date, y=close))+
  geom_line()+
  geom_smooth(method = 'auto')+
  xlab("")+
  ylab("")+
  facet_wrap('symbol', scales = "free_y")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y")+
  theme(axis.text.x = element_text(angle = -30))+
  ggtitle("TREND - Top 10 Performers - Long Term 360+ Trades")

ggplotly(LT_Top10_Trend, width=1000, height=600)


ST_top10_p <- ggplot(d60_top10, aes(x=reorder(symbol,d60_prct), y=d60_prct, fill=symbol))+
  geom_col()+
  coord_flip()+
  ggtitle("Top 10 Performers - Short Term 60+ Trades")+
  xlab("")+
  ylab("")

ggplotly(ST_top10_p, height = 400, width = 800)



ST_top10_p_trend <- ggplot(nse[nse$isin %in% d60_top10$isin,], aes(x=trade_date, y=close))+
  geom_line()+
  geom_smooth(method = 'auto')+
  facet_wrap('symbol', scales = "free_y")+
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y")+
  theme(axis.text.x = element_text(angle = -30))+
  ggtitle('TREND - TOP 10 Short Term 60+ trades')

ggplotly(ST_top10_p_trend, width=1000, height=600)


industries_d60_top5 <- trend_data %>% 
  arrange(industry_name, desc(d60_prct)) %>%
  group_by(industry_name) %>%
  top_n(n=5,wt = d60_prct) # %>%
#filter(row_number() <=5)


p_short_top5 <- ggplot(industries_d60_top5, aes(x=reorder(symbol,d60_prct), y=d60_prct))+
  geom_col()+
  coord_flip()+
  facet_wrap('industry_name', scales = 'free_y', ncol = 4)+
  xlab("")+
  ylab("")+
  theme(strip.text = element_text(size = 6, face = "bold"))+
  theme(axis.text.y = element_text(size = 6))+
  theme(panel.spacing.y = unit(0.5,"lines"))+
  theme(panel.spacing.x = unit(0.5,"lines"))+
  ggtitle('Short Term 60+ trades toppers in % gain')

ggplotly(p_short_top5, height = 800, width = 1000)


industries_d360_top5 <- trend_data %>% 
  arrange(industry_name, desc(d360_prct)) %>%
  group_by(industry_name) %>%
  top_n(n=5,wt = d360_prct) # %>%



p_long_top5 <- ggplot(industries_d360_top5, aes(x=reorder(symbol,d360_prct), y=d360_prct))+
  geom_col()+
  theme_bw()+
  coord_flip()+
  facet_wrap('industry_name', scales = 'free_y', ncol = 4)+
  xlab("")+
  ylab("")+
  theme(strip.text = element_text(size = 7, face = "bold", vjust = 0))+
  theme(axis.text.y = element_text(size = 7, family="Segoe UI", face="bold"))+
  theme(panel.spacing.y = unit(0.5,"lines"))+
  theme(panel.spacing.x = unit(0.5,"lines"))+
  ggtitle('Long Term 60+ trades toppers in % gain')


ggplotly(p_long_top5, height = 1000, width = 1275)
