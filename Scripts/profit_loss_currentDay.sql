select demat.scrip_code, demat.hold_value, (demat.dp_bal * nse.close) as currentValue, ((demat.dp_bal * nse.close) - demat.hold_value) as profit from demat
inner join nse on demat.isin = nse.isin
group by demat.scrip_code, demat.hold_value, demat.dp_bal, nse.close
order by demat.scrip_code
where nse.trade_date = 2018-07-20