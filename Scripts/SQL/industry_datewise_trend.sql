select nse.trade_date, industryclass.sector_name, industryclass.industry_name,sum(nse.close) as industry_value from nse
inner join scrips on nse.isin = scrips.isin
left join industryclass on scrips.industry = industryclass.industry_subgroup
group by nse.trade_date, industryclass.sector_name, industryclass.industry_name
order by industryclass.industry_name, nse.trade_date