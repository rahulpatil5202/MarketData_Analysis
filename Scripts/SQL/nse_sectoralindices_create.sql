CREATE TABLE public.nse_sectoralindices
(
    company_name text,
	industry text,
	symbol text,
	series text,
	isin text,
	index_name text
)
WITH (
    OIDS = FALSE
)
TABLESPACE pg_default;

ALTER TABLE public.nse_sectoralindices
    OWNER to rahul;