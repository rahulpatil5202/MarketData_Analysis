CREATE TABLE public.nse
(
    symbol text COLLATE pg_catalog."default",
    series text COLLATE pg_catalog."default",
    open double precision,
    high double precision,
    low double precision,
    close double precision,
    last double precision,
    prevclose double precision,
    tottrdqty integer,
    tottrdval double precision,
    trade_date date,
    totaltrades integer,
    isin text COLLATE pg_catalog."default"
)
WITH (
    OIDS = FALSE
)
TABLESPACE pg_default;

ALTER TABLE public.nse
    OWNER to rahul;