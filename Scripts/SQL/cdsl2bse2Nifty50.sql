select bse.*, (cdsl.valuation/cdsl.quantity) as hold_value, nse_indices.index_name, nse_indices.closing_index_value from cdsl
inner join bse on bse.isin_code = cdsl.isin
inner join nse_indices on bse.trade_date_new = nse_indices.index_date
where nse_indices.index_name = 'Nifty 50'
order by bse.trade_date_new