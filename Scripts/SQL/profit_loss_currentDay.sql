select demat.scrip_code, demat.hold_value, (demat.dp_bal * nse.close) as currentValue, ROUND(((demat.dp_bal * nse.close) - demat.hold_value)::numeric,2) as profit, ROUND((((demat.dp_bal * nse.close) - demat.hold_value)/demat.hold_value * 100)::numeric,2) as profit_percent from demat
inner join nse on demat.isin = nse.isin
where nse.trade_date = '2018-07-20'
group by demat.scrip_code, demat.hold_value, demat.dp_bal, nse.close
order by profit_percent, demat.scrip_code