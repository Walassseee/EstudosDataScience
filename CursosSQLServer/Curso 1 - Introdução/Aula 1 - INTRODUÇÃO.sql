create table [Clientes]
(
    [Nome][varchar](10) null,
    [Sobrenome][varchar](15) null,
    [cpf][varchar](11) null,
    [salario][money] null,
    [codigo][smallint] null,
    [já comprou][bit] null
)

insert into [Clientes]([Nome],[Sobrenome],[cpf],[salario],[codigo],[já comprou])
values ('Walasse','Tomaz','10710237995',3000,7859,1),
       ('Waldecyr','Tomaz','31904050930',2500,7849,0);

select * from [Clientes];

update [Clientes] set [salario] = 3256 where [Nome] = 'Waldecyr';

select * from [Clientes];

delete from [Clientes] where [Nome] = 'Waldecyr';

insert into [Clientes]([Nome],[Sobrenome],[cpf],[salario],[codigo],[já comprou])
values ('Monica','Frutuoso','02789975711',1000,4859,1),
       ('Waldecyr','Tomaz','31904050930',2500,7849,0);

select * from [Clientes];

alter table [Clientes]
alter column [cpf] [varchar](11) not null

alter table [Clientes]
add constraint pk_cpf
    primary key clustered ([cpf])

select * from [Clientes]
where salario >= 1500;

select * from [Clientes] where salario between 1000 and 2500;

drop table [Clientes];

