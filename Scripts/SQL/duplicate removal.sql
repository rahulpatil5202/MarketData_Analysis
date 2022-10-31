select distinct * into tmp From nse_indices go

DELETE from nse_indices go
insert into nse_indices
  	select * from tmp go
drop table tmp

select distinct * into tmp From nse go
delete from nse go
insert into nse 
  	select * from tmp go
drop table tmp

select distinct * into tmp From bse go
delete from bse go
insert into bse
  	select * from tmp go
drop table tmp