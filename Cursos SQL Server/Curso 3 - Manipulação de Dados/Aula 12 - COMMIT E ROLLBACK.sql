
SELECT * FROM [VENDEDORES]

BEGIN TRANSACTION

UPDATE [VENDEDORES] SET [COMISS�O] = [COMISS�O] * 1.15

SELECT * FROM [VENDEDORES]

INSERT INTO [VENDEDORES] ([Matricula], [NOME], [BAIRRO],[COMISS�O], [DATA ADMISS�O], [Ferias])
VALUES ('99999','Jo�o da Silva','Icara�',0.08,'2014-09-01',0)

SELECT * FROM [VENDEDORES]

ROLLBACK

COMMIT

