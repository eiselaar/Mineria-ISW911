### SEMANA 3

SELECT * FROM dbo.Categories

SELECT INTO dbo.Categories FROM(
SELECT * FROM Northwind.dbo.Categories ------COPIAR LOS MISMOS DATOS EN OTRA TABLA
)tc

INSERT INTO dbo.Categories
SELECT * FROM Northwind.dbo.Categories------PARA HACER INSERCION

CREATE TABLE nombre_tabla
SELECT * FROM Northwind.dbo.Categories----PARA HACER COPIAS MASIVAS

ETL

SELECT * FROM OPENQUERY (DB_ORA, 'select * from cliente')