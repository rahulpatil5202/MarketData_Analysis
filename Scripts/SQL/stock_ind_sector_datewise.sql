select nse.trade_date, nse.symbol, industryclass.sector_name, industryclass.industry_name, nse.close from nse
inner join scrips on nse.isin = scrips.isin
left join industryclass on scrips.industry = industryclass.industry_subgroup
where industryclass.industry_name = 'Retailing' and nse.trade_date >= '2018-01-01'
group by nse.trade_date, industryclass.sector_name, industryclass.industry_name,nse.symbol, nse.close
order by nse.symbol,nse.trade_date