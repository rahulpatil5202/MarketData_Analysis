select nse.trade_date, industryclass.sector_name, sum(nse.close) as sector_value from nse
inner join scrips on nse.isin = scrips.isin
left join industryclass on scrips.industry = industryclass.industry_subgroup
group by nse.trade_date, industryclass.sector_name
order by industryclass.sector_name, nse.trade_date