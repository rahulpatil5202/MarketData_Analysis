select nse.trade_date, demat.exchange,demat.scrip_code, demat.isin, demat.dp_bal as current_dpBal, demat.hold_price as dp_holdprice, nse.close, nse.prevclose, 
nse.tottrdqty,nse.tottrdval, nse_indices.index_name, nse_indices.closing_index_value from demat
inner join nse on demat.isin = nse.isin
left join nse_indices on nse.trade_date = nse_indices.index_date
where nse_indices.index_name = 'Nifty 50'
order by nse.trade_date