CREATE TABLE public.scrips
(
    security_code double precision,
	security_id text,
	security_name text,
	status text,
	security_group text,
	face_value double precision,
	isin text,
	industry text,
	instrument text
)
WITH (
    OIDS = FALSE
)
TABLESPACE pg_default;

ALTER TABLE public.scrips
    OWNER to rahul;