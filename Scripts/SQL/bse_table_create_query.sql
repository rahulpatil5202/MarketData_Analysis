CREATE TABLE public.bse
(
    sc_code integer,
    sc_name text COLLATE pg_catalog."default",
    sc_group text COLLATE pg_catalog."default",
    sc_type text COLLATE pg_catalog."default",
    open double precision,
    high double precision,
    low double precision,
    close double precision,
    last double precision,
    prevclose double precision,
    no_trades integer,
    net_turnov double precision,
    tdcloindi text COLLATE pg_catalog."default",
    isin_code text COLLATE pg_catalog."default",
    trade_date_new date
)
WITH (
    OIDS = FALSE
)
TABLESPACE pg_default;

ALTER TABLE public.bse
    OWNER to rahul;