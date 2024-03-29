CREATE TABLE public.demat
(
    exchange text COLLATE pg_catalog."default",
    isin text,
    scrip_code text COLLATE pg_catalog."default",
    industry text COLLATE pg_catalog."default",
    dp_bal integer,
    rec_bal integer,
    client_stk_bal integer,
    dpmar_bal integer,
    sk_lock integer,
    net_bal integer,
    hold_price double precision,
    ltp double precision,
    hold_vale double precision,
    mkt_value double precision,
    profit double precision,
    change text COLLATE pg_catalog."default",
    mkt_cap text COLLATE pg_catalog."default"
)
WITH (
    OIDS = FALSE
)
TABLESPACE pg_default;

ALTER TABLE public.demat
    OWNER to rahul;