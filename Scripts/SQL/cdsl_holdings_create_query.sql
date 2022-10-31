CREATE TABLE public.cdsl_holdings
(
	Description text,	
	ISIN text,
	Scrip text,
	Quantity double precision,
	Valuation double precision
)
WITH (
    OIDS = FALSE
)
TABLESPACE pg_default;

ALTER TABLE public.cdsl_holdings
    OWNER to rahul;