select nse.trade_date, sum(demat.dp_bal * nse.close) as portfolioValue, nse_indices.closing_index_value from demat
inner join nse on demat.isin = nse.isin
left join nse_indices on nse_indices.index_date = nse.trade_date
group by nse.trade_date, nse_indices.index_name, nse_indices.closing_index_value
having nse_indices.index_name = 'Nifty 50'
order by nse.trade_date desc