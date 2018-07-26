CREATE TABLE public.industryclass
(
    sector_name text,
	industry_name text,
	industry_group text,
	industry subgroup text,
	definition text
)
WITH (
    OIDS = FALSE
)
TABLESPACE pg_default;

ALTER TABLE public.industryclass
    OWNER to rahul;