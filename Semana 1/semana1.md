# Semana1

select * from cliente


CREATE VIEW dbo.VentasPorCliente
WITH SCHEMABINDING  -- Necesario para crear índices
AS
SELECT ClienteID, SUM(TotalVenta) AS TotalVentas
FROM dbo.Ventas
GROUP BY ClienteID;

CREATE UNIQUE CLUSTERED INDEX IDX_VentasPorCliente 
ON dbo.VentasPorCliente (ClienteID);


select * from JARDINERIA.MV_EJEMPLO



# ESTO NOS PASAMOS A JARDINERIA
CREATE MATERIALIZED VIEW JARDINERIA.MV_EJEMPLO
   BUILD IMMEDIATE
   REFRESH ON DEMAND
   START WITH SYSDATE NEXT SYSDATE +1
   AS 
   SELECT CODIGO_CLIENTE,NOMBRE_CLIENTE 
   FROM jardineria.CLIENTE;
   

# ESTO SE  HACE CON SYSTEM
CREATE MATERIALIZED VIEW LOG ON JARDINERIA.CLIENTE
WITH PRIMARY KEY, ROWID;  
      
grant create materialized view to  jardineria

BEGIN
    DBMS_MVIEW.REFRESH('MV_EJEMPLO');
END;

## Usuarioso con privilegios

Select * from all_views

BEGIN
    DBMS_VIEW.REFRESH('MV_EJEMPLO');
END;

---------------------------------------------
## Bulg collect hacer consultas en bloques de 2mil a 3mil consul por tiro

CREATE OR REPLACE PROCEDURE pa_carga_masiva AS
BEGIN
  BEGIN
    EXECUTE IMMEDIATE 'TRUNCATE TABLE VENTAS'; ##procesos de 9horas en 9 seg
  END;
  BEGIN
    DECLARE
      TYPE t_tabla IS TABLE OF VENTAS%ROWTYPE;
      l_datos t_tabla;
      CURSOR c_tabla IS
        SELECT * FROM jardineria.v_pedidos;
    BEGIN
      OPEN c_tabla;
      LOOP
        FETCH c_tabla BULK COLLECT
          INTO l_datos LIMIT 2000;
        FORALL i IN 1 .. l_datos.count
          INSERT INTO clientes.ventas VALUES l_datos (i);
        EXIT WHEN c_tabla%NOTFOUND;
      END LOOP;
      CLOSE c_tabla;
    END;
  END;
  END;


## PARA TRANSFORMAR LA INFORMACION:
PIVOD
LEAD
LISTAGG
PARTION BY
NTILE
ROLLUP
CUBE

PIVOD: En bases de datos, el comando PIVOT se usa para transformar datos de filas en columnas. Es especialmente útil cuando deseas reorganizar o resumir información para análisis o generación de reportes.

Ejemplos utilizando PIVOD:

SELECT * FROM (
SELECT ANNIO, ESTADO, (CANTIDAD*PRECIO_UNIDAD) VENTAS FROM V_VENTA
)PIVOT (SUM(VENTAS)VENTAS, COUNT(1)CANTIDAD FOR ESTADO IN ('Entregado'ENTREGADO, 'Pendiente' PENDIENTE
, 'Rechazado'RECHAZADO))

SELECT EXTRACT(YEAR FROM P.FECHA_PEDIDO) ANNIO, P.FECHA_PEDIDO, P.ESTADO, DP.CANTIDAD, DP.PRECIO_UNIDAD
FROM PEDIDO P
INNER JOIN DETALLE_PEDIDO PP
ON P.CODIGO_PEDIDO= DP.CODIGO_PEDIDO
INNER JOIN CLIENTE C
ON P.CODIGO_CLIENTE=C.CODIGO_CLIENTE
--------------------------------------------------------------------------------

para crear una vista:

CREATE VIEW JARDINERIA.V_VENTAS AS 
SELECT EXTRACT(YEAR FROM P.FECHA_PEDIDO) ANNIO, P.FECHA_PEDIDO,
NVL(P.ESTADO,'SIN ESTADO') ESTADO,NVL(DP.CANTIDAD,0) CANTIDAD,DP.PRECIO_UNIDAD 
FROM PEDIDO P
INNER JOIN DETALLE_PEDIDO DP
ON P.CODIGO_PEDIDO=DP.CODIGO_PEDIDO
INNER JOIN CLIENTE C
ON P.CODIGO_CLIENTE=C.CODIGO_CLIENTE
-------------------------------------------------------------------------------
LEAD:
En bases de datos, la función LEAD es una función analítica (o de ventana) que se utiliza para acceder a una fila posterior (siguiente) dentro de un conjunto de datos, sin necesidad de realizar un auto-join o subconsulta adicional. Es común en sistemas SQL como Oracle, SQL Server, PostgreSQL, entre otros.

EJEMPLOS:

CREATE VIEW V_INCREMENTO AS
SELECT NOMBRE, ANNIO, SUM((CANTIDAD * PRECIO_UNIDAD)) VENTAS_ACTUALES,
LAG (SUM(CANTIDAD * PRECIO_UNIDAD)) OVER (ORDER BY ANNIO, NOMBRE) VENTAS_ANTERIORES
FROM JARDINERIA.V_VENTA
GROUP BY NOMBRE, ANNIO
ORDER BY ANNIO ASC;


LAG:

SELECT I.*,((NVL(VENTAS_ANTERIORES,0))-NVL(VENTAS_ACTUALES,0))/VENTAS_ACTUALES INCREMENTO
FROM V_INCREMENTO I
SELECT I.*,(NVL(VENTAS_ACTUALES,0)- NVL(VENTAS_ANTERIORES,0)) DIFERENCIA
FROM V_INCREMENTO I;


CREATE OR REPLACE VIEW V_INCREMENTO AS
SELECT NOMBRE, ANNIO, SUM((CANTIDAD * PRECIO_UNIDAD)) VENTAS_ACTUALES,
LAG (SUM(CANTIDAD * PRECIO_UNIDAD)) OVER 
(PARTITION BY NOMBRE ORDER BY ANNIO ASC) VENTAS_ANTERIORES
FROM JARDINERIA.V_VENTA
GROUP BY NOMBRE, ANNIO
ORDER BY ANNIO ASC;

----------------------------------------------------------------------------------------
NTILE: La función NTILE es una función analítica en SQL que divide un conjunto de filas ordenadas en un número especificado de grupos (o "cubetas"), asignando un número único a cada fila dentro del grupo. Se utiliza para análisis y distribución de datos.

# PERCENTIL:

select i.*,(nvl(ventas_actuales,0)-nvl(ventas_anterior,0)) diferencia
from jardineria.v_incremento i

create or replace view jardineria.v_incremento as
SELECT producto, annio,sum(cantidad*precio_unidad) ventas_actuales,
lag(sum(cantidad*precio_unidad))over 
( partition by producto order by annio asc )
ventas_anterior

FROM JARDINERIA.V_VENTAS
group by producto, annio

-----------------------------------------------------------------

select ciudad,nombre_cliente,ventas,ntile(4) 
over (partition by ciudad order by ventas desc) grupo from (
select ciudad,nombre_cliente,sum(precio_unidad*cantidad) ventas
from JARDINERIA.V_VENTAS
group by nombre_cliente,ciudad
)
    
-----------------------------------------------------------------------
CREATE OR REPLACE VIEW JARDINERIA.V_VENTAS AS 
SELECT c.nombre_cliente,c.ciudad,PR.NOMBRE PRODUCTO, 
EXTRACT(YEAR FROM P.FECHA_PEDIDO) ANNIO, P.FECHA_PEDIDO,
NVL(P.ESTADO,'SIN ESTADO') ESTADO,NVL(DP.CANTIDAD,0) 
CANTIDAD,DP.PRECIO_UNIDAD 
FROM PEDIDO P
INNER JOIN DETALLE_PEDIDO DP
ON P.CODIGO_PEDIDO=DP.CODIGO_PEDIDO
INNER JOIN CLIENTE C
ON P.CODIGO_CLIENTE=C.CODIGO_CLIENTE
INNER JOIN PRODUCTO PR
ON DP.CODIGO_PRODUCTO =PR.CODIGO_PRODUCTO


## LISTAG

La función LISTAGG en bases de datos se utiliza para concatenar valores de varias filas en un solo string, agrupados según un criterio específico. 

EJEMPLO:

SELECT NOMBRE_CLIENTE,
LISTAGG(NOMBRE, ',')WITHIN GROUP(ORDER BY NOMBRE)AS LISTA_PRODUCTOS
FROM V_VENTA
GROUP BY NOMBRE_CLIENTE



## ROLLUP

select ciudad , annio, sum(cantidad* precio_unidad) ventas

from  JARDINERIA.V_VENTAS
group by rollup(   annio,ciudad)
order by 1,2
--------------------------------------------------------

select ciudad , annio, sum(cantidad* precio_unidad) ventas
from  JARDINERIA.V_VENTAS
group by cup(   annio,ciudad)
order by 1,2