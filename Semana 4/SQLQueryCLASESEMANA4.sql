
use DW

-- datos en el DW
create table dim_cliente(
id_cliente int identity constraint pk_client primary key,
codigo_cliente nvarchar(30),
nombre_cliente nvarchar(80),
asesor_venta nvarchar(80),
ciudad nvarchar(50),
region nvarchar(50),
pais nvarchar(50),
categoria nvarchar(50)
)

--------------------------
Use STAGING

select * from v_cliente-----para ver la vista

create view v_cliente as---creamos vista
select
 codigo_cliente,
 nombre_cliente ,
 asesor_ventas  ,
 ciudad ,
 region ,
 pais ,
null categoria---aun en nulo porque no hemos creado la categoria
from(
--cj cliente jardineria
select cast(cj.CODIGO_CLIENTE as varchar(10)) codigo_cliente, cj.NOMBRE_CLIENTE, CONCAT(ej.nombre ,' ', APELLIDO1,' ',APELLIDO2) asesor_ventas,
cj.ciudad, cj.region, cj.pais 
from cliente cj
left join empleado ej
on cj.CODIGO_EMPLEADO_REP_VENTAS=ej.CODIGO_EMPLEADO
union all
---datos de tabla Northwind
select cn.CustomerID, cn.CompanyName, CONCAT(en.FirstName ,' ',en.LastName) asesor,
cn.City, cn.Region, cn.Country 
from Customers cn
left join 
(select CustomerID, EmployeeID from Orders o1
where orderid=(select max(orderid) from Orders o2 where o2.CustomerID=o1.CustomerID)
group by CustomerID, EmployeeID
) ae
on cn.CustomerID=ae.CustomerID
left join Employees en
on en.EmployeeID=ae.EmployeeID
) cp


--------CREAMOS LA CATEGORIA------------
select * from PEDIDO p
inner join  DETALLE_PEDIDO dp
on p.CODIGO_PEDIDO=dp.CODIGO_PEDIDO
---------------------------------------

select codigo_cliente, sum(dp.cantidad*dp.precio_unidad) ventas,
ntile(4) over (order by sum(dp.cantidad*dp.precio_unidad) desc )segmento
from pedido p
inner join detalle_pedido dp
on p.codigo_pedido=dp.codigo_pedido
where estado ='ENTREGADO'
group by codigo_cliente

----el union solito elimina duplicados------
-----NORTHWIND------ASIGNAMOS CATEGORIA EN CADA UNA DE LAS VENTAS DE LAS BD
--UNIMOS LAS CONSULTAS
alter view v_cliente as 

with segmentos_cliente as(
select cast(codigo_cliente as varchar(10)) codigo_cliente,sum(dp.cantidad*dp.precio_unidad) ventas,
ntile(4) over (order by sum(dp.cantidad*dp.precio_unidad) desc ) segmento
from pedido p
inner join detalle_pedido dp
on p.codigo_pedido=dp.codigo_pedido
where estado='ENTREGADO'
group by codigo_cliente
union 
select customerid,sum(dp.quantity*dp.unitprice) ventas,
ntile(4) over (order by sum(dp.quantity*dp.unitprice) desc ) segmento
from orders p
inner join orderdetails dp
on p.orderid=dp.orderid
group by customerid
)

select 
cp.codigo_cliente ,
nombre_cliente ,
asesor_ventas  ,
ciudad   ,
region   ,
pais   ,
segmento categoria

from (

select cast(cj.CODIGO_CLIENTE as varchar(10)) codigo_cliente ,cj.nombre_cliente, CONCAT(ej.nombre,' ',APELLIDO1,' ',APELLIDO2) asesor_ventas,
cj.ciudad,cj.region,cj.pais
from cliente cj
left join empleado ej
on cj.CODIGO_EMPLEADO_REP_VENTAS=ej.CODIGO_EMPLEADO
union all 
select cn.CustomerID,cn.CompanyName, CONCAT(en.FirstName,' ',en.LastName) asesor,
cn.City,cn.Region,cn.Country
from Customers cn
left join (
select  CustomerID,EmployeeID from Orders o1
where orderid=(select max(orderid) from Orders o2 where o2.CustomerID=o1.CustomerID )
group by CustomerID,EmployeeID
) ae
on cn.CustomerID=ae.CustomerID
left join Employees en
on en.EmployeeID=ae.EmployeeID
) cp
left join segmentos_cliente sc
on cp.codigo_cliente=sc.codigo_cliente

------------ORDENAR LAS CATEGORIAS----------------------------------------------
alter view v_cliente as 

with segmentos_cliente as(
select cast(codigo_cliente as varchar(10)) codigo_cliente,sum(dp.cantidad*dp.precio_unidad) ventas,
ntile(4) over (order by sum(dp.cantidad*dp.precio_unidad) desc ) segmento
from pedido p
inner join detalle_pedido dp
on p.codigo_pedido=dp.codigo_pedido
where estado='ENTREGADO'
group by codigo_cliente
union 
select customerid,sum(dp.quantity*dp.unitprice) ventas,
ntile(4) over (order by sum(dp.quantity*dp.unitprice) desc ) segmento
from orders p
inner join orderdetails dp
on p.orderid=dp.orderid
group by customerid
)

select 
cp.codigo_cliente ,
upper(nombre_cliente) nombre_cliente, --agregamos el upper
upper(isnull(asesor_ventas, 'Lorena Paxton'))asesor_ventas , -- cambiamos aca
upper(ciudad) ciudad   ,
upper(isnull(region,ciudad)) region   , -- is es nulo replacelo por ciudad
upper(case when pais='UK' then 'UNITED KINGDOM' else pais end)  pais ,
case when isnull(segmento,0)=1 then 'CLASE A'
when isnull(segmento,0)=2 then 'CLASE B'
when isnull(segmento,0)=3 then 'CLASE C'
when isnull(segmento,0)=4 then 'CLASE D'
else 'CLASE E' end categoria

from (

select cast(cj.CODIGO_CLIENTE as varchar(10)) codigo_cliente ,cj.nombre_cliente, CONCAT(ej.nombre,' ',APELLIDO1,' ',APELLIDO2) asesor_ventas,
cj.ciudad,cj.region,cj.pais
from cliente cj
left join empleado ej
on cj.CODIGO_EMPLEADO_REP_VENTAS=ej.CODIGO_EMPLEADO
union all 
select cn.CustomerID,cn.CompanyName, CONCAT(en.FirstName,' ',en.LastName) asesor,
cn.City,cn.Region,cn.Country
from Customers cn
left join (
select  CustomerID,EmployeeID from Orders o1
where orderid=(select max(orderid) from Orders o2 where o2.CustomerID=o1.CustomerID )
group by CustomerID,EmployeeID
) ae
on cn.CustomerID=ae.CustomerID
left join Employees en
on en.EmployeeID=ae.EmployeeID
) cp
left join segmentos_cliente sc
on cp.codigo_cliente=sc.codigo_cliente
------------------------------------------
SELECT * FROM v_cliente
------------------------------------------------------



---pasos para verificar-------------------------------

select * from Orders

-- 2 , BUSCAR DUPLICADOS HAVING
select count (*), CustomerID, EmployeeID from Orders
group by CustomerID, EmployeeID
having count(1)>1

--3 SACAR LA ULTIMA ORDE VENDAS Y ASIGANAR EL VENDOR QUE TIENE ESA ORDEN
select count (*), CustomerID, EmployeeID from Orders o1
where orderid=(select max(orderid) from Orders o2 where o2.CustomerID=o1.CustomerID)
group by CustomerID, EmployeeID
having count(1)>1

---4
select count(1), asesor_ventas from v_cliente
group by asesor_ventas

----5
Select * from v_cliente
group by pais

-----------------------DW---------------------
---cambiamos a DW----------------------------------
USE DW
select * from asesor_ventas
select * from dim_cliente


insert into dim_cliente(codigo_cliente, nombre_cliente, asesor_venta, ciudad, region, pais, categoria)
select codigo_cliente, nombre_cliente, asesor_ventas, ciudad, region, pais, categoria from 
staging.dbo.v_cliente

-------------------DIMENSION DE TIEMPO--------------------------

create table DIM_TIEMPO
(
    idtiempo int identity constraint pk_tiempo  PRIMARY KEY CLUSTERED,
	Fecha date not null, 
    Anio smallint not null,
    Mes smallint not null,
    Dia smallint not null,
    NMes nvarchar(30) not null,
	DiaAnnio int
)




begin

declare @fechainicio date;
declare @fechafinal date;
set @fechainicio ='2024-01-01';
set @fechafinal=getdate();
declare @Anio int;
declare @Mes int;
declare @Dia int; 
declare @DiaAnnio int ;
declare @NMes varchar(20);

while @fechainicio<@fechafinal
	begin

	SELECT @Anio = DATEPART(yy, @fechainicio)
    SELECT @Mes = DATEPART(m, @fechainicio)
    SELECT @Dia = RIGHT('0' + DATEPART(dd, @fechainicio),2)
    SELECT @NMes = DATENAME(mm, @fechainicio)
    SELECT @DiaAnnio =datepart( DAYOFYEAR, @fechainicio)

	insert into DIM_TIEMPO(
	Fecha ,     Anio ,    Mes ,    Dia ,    NMes,	DiaAnnio) 
	values (@fechainicio,@Anio,@Mes,@Dia,@NMes,@DiaAnnio)

	set @fechainicio=dateadd(day,1,@fechainicio);
	end 

end

---------select--------------
select * from DIM_TIEMPO
----------------------------
 USE DW

 -----CREACION DE TABLA DE HECHOS------------------------------
 -----id_cliente - id_producto- id_empleado- fecha - cantidad - precio

SELECT 
DC.id_cliente id_cliente,
NULL id_producto,
NULL id_empleado,
CAST (P.FECHA_PEDIDO AS DATE) fecha,
DP.cantidad,
DP.PRECIO_UNIDAD PRECIO
FROM STAGING.DBO.PEDIDO P
INNER JOIN STAGING.DBO.DETALLE_PEDIDO DP
ON P.CODIGO_PEDIDO= DP.CODIGO_PEDIDO
INNER JOIN dim_cliente DC
ON CAST( P.CODIGO_CLIENTE AS varchar(10))=DC.codigo_cliente
WHERE FECHA_ENTREGA IS NOT NULL

select * from FACT_VENTAS
--------------------TERMINAR TABLA DE TAREA-----------------------

----PARA HACER CONSTRAINT -----ALTER TABLE FACT_VENTAS ADD CONSTRAINT FK_VENTAS_CLIENTE FOREING KEY (ID_CLIENTE)
----REFERENCES DIM_CLIENTE (ID_CLIENTE)

--------------------pendiente ejecutar las siguientes consultas----------
-- DIMENSION PRODUCTO

CREATE TABLE DIM_PRODUCTO (
    id_producto INT IDENTITY(1,1) PRIMARY KEY,
    codigo_producto VARCHAR(50) NOT NULL,
    nombre VARCHAR(100) NOT NULL,
    gama VARCHAR(50),
    dimensiones VARCHAR(50),
    proveedor VARCHAR(100),
    descripcion VARCHAR(255),
    cantidad_en_stock INT,
    precio_venta DECIMAL(10,2),
    precio_proveedor DECIMAL(10,2)
);

-- Insertar datos en la dimensión de Productos
INSERT INTO DIM_PRODUCTO (codigo_producto, nombre, gama, dimensiones, proveedor, descripcion, cantidad_en_stock, precio_venta, precio_proveedor)
SELECT CODIGO_PRODUCTO, NOMBRE, GAMA, DIMENSIONES, PROVEEDOR, DESCRIPCION, CANTIDAD_EN_STOCK, PRECIO_VENTA, PRECIO_PROVEEDOR
FROM staging.dbo.v_producto;


--- CREATE TABLE DIM_EMPLEADO (
    id_empleado INT IDENTITY(1,1) PRIMARY KEY,
    codigo_empleado INT NOT NULL,
    nombre VARCHAR(50),
    apellido1 VARCHAR(50),
    apellido2 VARCHAR(50),
    extension VARCHAR(20),
    email VARCHAR(100),
    codigo_oficina VARCHAR(10),
    codigo_jefe INT,
    puesto VARCHAR(50)
);

-- Insertar datos en la dimensión de Empleados
INSERT INTO DIM_EMPLEADO (codigo_empleado, nombre, apellido1, apellido2, extension, email, codigo_oficina, codigo_jefe, puesto)
SELECT CODIGO_EMPLEADO, NOMBRE, APELLIDO1, APELLIDO2, EXTENSION, EMAIL, CODIGO_OFICINA, CODIGO_JEFE, PUESTO
FROM staging.dbo.v_empleado;