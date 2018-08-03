/* Days gain percent calculated on d-1 portfolio value*/

select nse.trade_date, sum(demat.dp_bal * nse.close) as portfolioValue,
(ROUND((sum(demat.dp_bal * nse.close) - sum(demat.hold_value))::numeric,2)) - lag(ROUND((sum(demat.dp_bal * nse.close) - sum(demat.hold_value))::numeric,2)) over (order by trade_date) as days_gain,
ROUND((((sum(demat.dp_bal * nse.close) - sum(demat.hold_value)) - lag((sum(demat.dp_bal * nse.close) - sum(demat.hold_value))) over (order by trade_date))*100/(lag((sum(demat.dp_bal * nse.close))) over (order by trade_date)))::numeric,2) as days_gain_percent,ROUND((sum(demat.dp_bal * nse.close) - sum(demat.hold_value))::numeric,2) as overall_gain,ROUND(((sum(demat.dp_bal * nse.close) - sum(demat.hold_value))/sum(demat.hold_value)*100)::numeric,2) as overall_gain_percent, nse_indices.closing_index_value as nifty, ROUND((((nse_indices.closing_index_value - lag(nse_indices.closing_index_value) over (order by nse.trade_date))/nse_indices.closing_index_value)*100)::numeric,2) as nifty_change_percent  from demat
inner join nse on demat.isin = nse.isin
left join nse_indices on nse_indices.index_date = nse.trade_date
group by nse.trade_date, nse_indices.index_name, nse_indices.closing_index_value
having nse_indices.index_name = 'Nifty 50'
order by nse.trade_date desc