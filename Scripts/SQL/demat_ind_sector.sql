select demat.scrip_code, scrips.security_name, scrips.industry as industry_subgroup, industryclass.industry_name, industryclass.sector_name from demat
inner join scrips on demat.isin = scrips.isin
left join industryclass on scrips.industry = industryclass.industry_subgroup
group by demat.scrip_code, scrips.security_name, scrips.industry, industryclass.industry_name, industryclass.sector_name
order by industryclass.sector_name