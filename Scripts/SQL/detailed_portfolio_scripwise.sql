select nse.trade_date, nse.symbol, nse.close, sum(demat.hold_value)as investedvalue,(demat.dp_bal * nse.close) as currentvalue,
ROUND(((demat.dp_bal * nse.close) - lag((demat.dp_bal * nse.close)) over(order by symbol, trade_date))::numeric,2) as days_gain,
ROUND((((demat.dp_bal * nse.close) - lag((demat.dp_bal * nse.close)) over(order by symbol, trade_date))/sum(demat.hold_value)*100)::numeric,2) as days_gain_percent,
ROUND(((demat.dp_bal * nse.close) - sum(demat.hold_value))::numeric,2) as overallgain,
ROUND((((demat.dp_bal * nse.close) - sum(demat.hold_value))/sum(demat.hold_value)*100)::numeric,2) as overallgain_percent,
nse_indices.closing_index_value as Nifty,
ROUND((nse_indices.closing_index_value - lag(nse_indices.closing_index_value) over (order by symbol, trade_date))::numeric,2) as niftychange,
ROUND((((nse_indices.closing_index_value) - lag(nse_indices.closing_index_value) over (order by symbol, trade_date))/(lag(nse_indices.closing_index_value) over (order by symbol, trade_date))*100)::numeric,2) as niftychange_percent
from nse
inner join demat on demat.isin = nse.isin
left join nse_indices on nse_indices.index_date = nse.trade_date
where nse_indices.index_name = 'Nifty 50'
group by nse.trade_date, nse.symbol, nse.close, demat.dp_bal,nse_indices.closing_index_value
order by nse.trade_date desc, nse.symbol