# EDA Analisis Exploratorio de datos

1. creacion de conexiones

library(RODBC)

2. conexion y crear las consultas

con.SqlServer <-odbcConnect("ODBC_SQL",uid = "sa",pwd = "Admin12345")
facturas<-sqlQuery(con.SqlServer,"select *  from invoices")
View(facturas)

3. crear el diccionario de datos 

Docuemnto en word que describe sus variables y sus propositos

4. explorar el conjunto de datos (summary,tipos de datos,dimenciones)
library(dplyr)
glimpse(facturas)
str(facturas)
dim(facturas)
class(facturas)
summary(facturas)


5. convercion de tipos de datos

facturas$OrderDate<-as.Date(facturas$OrderDate)
facturas$RequiredDate<-as.Date(facturas$RequiredDate)
facturas$ShippedDate<-as.Date(facturas$ShippedDate)
facturas$Quantity<-as.numeric(facturas$Quantity)



6. identificacion  de valores nulos
# Identificar valores nulos en todo el dataframe
sum(is.na(facturas)) 

nulos <- facturas[!complete.cases(facturas),]
nrow(nulos)  # Cantidad de filas con al menos un NA

colSums(is.na(facturas))
facturas$Region<-NULL
facturas$ShipRegion<-NULL
facturas$ShipPostalCode<-NULL
facturas$ShippedDate<-NULL
facturas$PostalCode<-NULL


apply(is.na(nulos), 2, mean)   # Porcentaje de NA por columna
apply(is.na(nulos), 2, sum)    # Cantidad de NA por columna
apply(is.na(nulos), 2, which)  # Posición de NA por columna

# Visualizar la distribución de valores nulos (requiere paquete naniar)
install.packages("naniar")
library(naniar)
vis_miss(facturas)  # Visualización gráfica de valores faltantes



7. identificacion de valores duplicados

sum(duplicated(facturas))

8. identificacion de valores atipicos
colnames(facturas)
View(facturas)
facturas<-facturas %>% mutate(totalsales=Quantity+UnitPrice)

library(ggplot)

ggplot(facturas, aes(y = Quantity)) + 
  geom_boxplot() +
  ggtitle("Distribución de Cantidad con Valores Atípicos")


ggplot(facturas, aes(x = Country, y = totalsales)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Distribución de Ventas Totales por País")


ggplot(facturas, aes(x = ProductName, y = totalsales)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Distribución de Ventas Totales por Producto")

outliers_mediana <- function(df, columna, groupby_col) {
  df[[groupby_col]] <- as.factor(df[[groupby_col]])
  resultados <- list()
  for (grupo in levels(df[[groupby_col]])) {
    datos_grupo <- df[df[[groupby_col]] == grupo, ]
    Q1 <- quantile(datos_grupo[[columna]], 0.25)
    Q3 <- quantile(datos_grupo[[columna]], 0.75)
    IQR <- Q3 - Q1
    limite_inferior <- Q1 - 1.5 * IQR
    limite_superior <- Q3 + 1.5 * IQR
    
    mediana_grupo <- median(datos_grupo[[columna]], na.rm = TRUE)
    
    datos_grupo[[columna]] <- ifelse(
      datos_grupo[[columna]] < limite_inferior | 
        datos_grupo[[columna]] > limite_superior,
      mediana_grupo,
      datos_grupo[[columna]]
    )
    
    resultados[[grupo]] <- datos_grupo
  }
  
  # Combinar todos los grupos en un solo data frame
  df_resultado <- do.call(rbind, resultados)
  
  return(df_resultado)
}

df_resultado <- outliers_mediana(facturas, 'totalsales', 'Country')
df_resultado

ggplot(df_resultado, aes(x = Country, y = totalsales)) +
  geom_boxplot(aes(group = Country), fill = "lightblue", color = "black", alpha = 0.5) +  # Boxplot por país
  scale_color_manual(values = c("red", "blue")) + 
  labs(title = "Valores Atípicos Reemplazados por la Mediana",
       x = "País", y = "Total Ventas",
       color = "Es Outlier") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


9. identificacion de correlaciones
numericas <- sapply(facturas, is.numeric)
df_numerico <-facturas[, numericas]

cor_matrix <- cor(df_numerico, use = "complete.obs")
print(cor_matrix)

10.dentificacion de concentraciones de datos
Agrupaciones,barplot

#  Imputacion de datos  
11. creacion de funcion para el manejo de NAs

df_resultado <- outliers_mediana(facturas, 'totalsales', 'Country')
df_resultado

library(corrplot)
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.cex = 0.8)

names(facturas) 
facturas %>% group_by(Country) 
%>% summarise(total=sum(totalsales), total_cliente=n())

12. creacion de funcion para el manejo de atipicos 
13. guardar el conjunto de datos en csv


install.packages("RODBC")
install.packages("xlsx")
library(RODBC)
library(xlsx)

#Lectura de datos de bases de datos relacionales
con<-odbcConnect("ODBC-ORACLE",uid = "pedidos",pwd = "Admin12345")

##Conexion  con Oracle

tbl_cliente <- sqlQuery(con, "select codigo_cliente as id_cliente, nombre_cliente, ciudad from cliente")
tbl_producto <- sqlQuery(con, "select codigo_producto as id_producto, nombre, gama from producto")
tbl_pedido <- sqlQuery(con, "select codigo_pedido id_pedido, codigo_cliente id_cliente,  estado from pedidos.pedido")
tbl_depedido <- sqlQuery(con, "select codigo_pedido as id_pedido, codigo_producto as id_producto, cantidad,precio_unidad from detalle_pedido")

library(dplyr)
ventas <- inner_join(tbl_cliente, tbl_pedido, by = "ID_CLIENTE") %>% 
  inner_join(tbl_depedido, by = "ID_PEDIDO") 
ventas <- inner_join(ventas, tbl_producto, by = "ID_PRODUCTO")
# Crear una nueva columna de ventas 
ventas$INGRESOS <- ventas$CANTIDAD * ventas$PRECIO_UNIDAD

datos <- ventas[,c("ESTADO", "GAMA", "CIUDAD", "INGRESOS")]

boxplot( ventas$INGRESOS ~ ESTADO + GAMA +  CIUDAD, data = datos, 
         main = "Diagrama de caja de ventas por estado, gama y ciudad", 
         xlab = "Combinaciones de estado, gama y ciudad", 
         ylab = "Ventas", outlier.colour = "red")

cliente <- sqlQuery(con,"Select codigo_cliente,nombre_cliente,ciudad from cliente")
print(cliente)

con.SqlServer <-odbcConnect("ODBC_SQL",uid = "sa",pwd = "Admin12345")

query.employees.sqlserver<-sqlQuery(con.SqlServer,"select *  from invoices")
query.employees.oracle<-sqlQuery(con.Oracle,"select *  from hr.employees")