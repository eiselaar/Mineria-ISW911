
# Semana 1: Consultas SQL y Vistas Materializadas

## Consultas Básicas

```sql
-- Seleccionar todos los clientes
SELECT * FROM cliente;
```

## Creacion de Vistas e Indices
```sql
-- Crear una vista para las ventas por cliente
CREATE VIEW dbo.VentasPorCliente
WITH SCHEMABINDING  -- Necesario para crear índices
AS
SELECT ClienteID, SUM(TotalVenta) AS TotalVentas
FROM dbo.Ventas
GROUP BY ClienteID;

-- Crear un índice único agrupado en la vista
CREATE UNIQUE CLUSTERED INDEX IDX_VentasPorCliente 
ON dbo.VentasPorCliente (ClienteID);
```
## Vistas Materializadas en JARDINERIA
```sql
-- Crear una vista materializada en JARDINERIA
CREATE MATERIALIZED VIEW JARDINERIA.MV_EJEMPLO
   BUILD IMMEDIATE
   REFRESH ON DEMAND
   START WITH SYSDATE NEXT SYSDATE +1
   AS 
   SELECT CODIGO_CLIENTE, NOMBRE_CLIENTE 
   FROM jardineria.CLIENTE;

-- Crear un log de vista materializada
CREATE MATERIALIZED VIEW LOG ON JARDINERIA.CLIENTE
WITH PRIMARY KEY, ROWID;

-- Otorgar permisos para crear vistas materializadas
GRANT CREATE MATERIALIZED VIEW TO jardineria;

```

## Consulta Avanzada
``` sql
-- Seleccionar todas las vistas materializadas
SELECT * FROM all_mviews;

-- Refrescar la vista materializada
BEGIN
    DBMS_MVIEW.REFRESH('MV_EJEMPLO');
END;

```

## Carga Masiva de Datos
``` sql
-- Procedimiento para carga masiva de datos
CREATE OR REPLACE PROCEDURE pa_carga_masiva AS
BEGIN
  BEGIN
    EXECUTE IMMEDIATE 'TRUNCATE TABLE VENTAS'; -- Procesos de 9 horas en 9 segundos
  END;
  BEGIN
    DECLARE
      TYPE t_tabla IS TABLE OF VENTAS%ROWTYPE;
      l_datos t_tabla;
      CURSOR c_tabla IS
        SELECT * FROM jardineria.v_pedidos;
    BEGIN
      OPEN c_tabla;
      LOOP  -- Cursor
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
```

## Transformacion de Datos

```sql

-- Crear una vista para las ventas
CREATE VIEW JARDINERIA.V_VENTAS AS 
SELECT EXTRACT(YEAR FROM P.FECHA_PEDIDO) ANNIO, P.FECHA_PEDIDO,
NVL(P.ESTADO,'SIN ESTADO') ESTADO, NVL(DP.CANTIDAD,0) CANTIDAD, DP.PRECIO_UNIDAD 
FROM PEDIDO P
INNER JOIN DETALLE_PEDIDO DP
ON P.CODIGO_PEDIDO = DP.CODIGO_PEDIDO
INNER JOIN CLIENTE C
ON P.CODIGO_CLIENTE = C.CODIGO_CLIENTE;

-- Consulta con PIVOT
SELECT * FROM (
    SELECT ANNIO, ESTADO, (CANTIDAD * PRECIO_UNIDAD) VENTAS 
    FROM JARDINERIA.V_VENTAS
) PIVOT (
    SUM(VENTAS) VENTAS, COUNT(1) CANTIDAD
    FOR ESTADO IN ('entregado' entregado, 'pendiente' pendiente, 'rechazado' rechazado)
);
```

## Funciones de Ventana

```sql
-- Crear una vista para el incremento de ventas
CREATE OR REPLACE VIEW V_INCREMENTO AS
SELECT NOMBRE, ANNIO, SUM((CANTIDAD * PRECIO_UNIDAD)) VENTAS_ACTUALES,
LAG (SUM(CANTIDAD * PRECIO_UNIDAD)) OVER (PARTITION BY NOMBRE ORDER BY ANNIO ASC) VENTAS_ANTERIORES
FROM JARDINERIA.V_VENTA
GROUP BY NOMBRE, ANNIO
ORDER BY ANNIO ASC;

-- Consulta para calcular el incremento de ventas
SELECT I.*, (NVL(VENTAS_ACTUALES,0) - NVL(VENTAS_ANTERIORES,0)) DIFERENCIA
FROM V_INCREMENTO I;

```
## Percentiles y Agrupaciones

```sql
-- Consulta para calcular percentiles
SELECT CIUDAD, NOMBRE_CLIENTE, VENTAS, NTILE(4) 
OVER (PARTITION BY CIUDAD ORDER BY VENTAS DESC) GRUPO 
FROM (
    SELECT CIUDAD, NOMBRE_CLIENTE, SUM(PRECIO_UNIDAD * CANTIDAD) VENTAS
    FROM JARDINERIA.V_VENTAS
    GROUP BY NOMBRE_CLIENTE, CIUDAD
);

-- Consulta con LISTAGG
SELECT NOMBRE_CLIENTE,
LISTAGG(NOMBRE, ',') WITHIN GROUP (ORDER BY NOMBRE) AS LISTA_PRODUCTOS
FROM V_VENTA
GROUP BY NOMBRE_CLIENTE;

```

## Agrupaciones con ROLLUP y CUBE

```sql
-- Consulta con ROLLUP
SELECT CIUDAD, ANNIO, SUM(CANTIDAD * PRECIO_UNIDAD) VENTAS
FROM JARDINERIA.V_VENTAS
GROUP BY ROLLUP(ANNIO, CIUDAD)
ORDER BY 1, 2;

-- Consulta con CUBE
SELECT CIUDAD, ANNIO, SUM(CANTIDAD * PRECIO_UNIDAD) VENTAS
FROM JARDINERIA.V_VENTAS
GROUP BY CUBE(ANNIO, CIUDAD)
ORDER BY 1, 2;
```
### Nota finales
PIVOT: Utilizado para transformar filas en columnas.

LAG: Función de ventana que permite acceder a valores de filas anteriores.

LISTAGG: Agrega valores en una lista separada por comas.

ROLLUP y CUBE: Funciones de agrupación que permiten realizar subtotales y totales generales.
