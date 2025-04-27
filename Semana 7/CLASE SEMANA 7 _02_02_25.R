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
facturas$Quantity<-as.numeric(facturas$Quantity)


6. Identificacion de valores nulos

colSums(is.na(facturas))###suma la cantidad de nulos por columna
facturas$Region<- NULL
facturas$ShipRegion<- NULL###eliminamos valores nulos
facturas$ShipPostalCode<- NULL
facturas$ShippedDate<- NULL
facturas$PostalCode<- NULL


7. identificacion de valores duplicados
 sum (duplicated(facturas))

8. identificacion de valores atipicos
colnames(facturas)### devuelve el nombre de las columnas
facturas %>% mutate(total_sales = Quantity * UnitPrice)
View(facturas)






9. identificacion de correlaciones- country-product name-general

ggplot(facturas, aes(x = Country, y = totalsales)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Distribución de Ventas Totales por País")


ggplot(facturas, aes(x = ProductName, y = totalsales)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Distribución de Ventas Totales por Producto")


10.dentificacion de concentraciones de datos Agrupaciones,barplot

names (facturas)
facturas %>% group_by(Country) %>%
  summarise(total=sum)

############################################################

library(ggplot2)
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

df_resultado <- outliers_mediana(facturas, 'total_sales', 'Country')
df_resultado

ggplot(df_resultado, aes(x = Country, y = total_sales)) +
  geom_boxplot(aes(group = Country), fill = "lightblue", color = "black", alpha = 0.5) +  # Boxplot por país
  scale_color_manual(values = c("red", "blue")) + 
  labs(title = "Valores Atípicos Reemplazados por la Mediana",
       x = "País", y = "Total Ventas",
       color = "Es Outlier") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##############################################################


#  Imputacion de datos  
11. creacion de funcion para el manejo de NAs
df_resultado <- outliers_mediana(facturas, 'total_sales', 'Country')
df_resultado

12. creacion de funcion para el manejo de atipicos 

13. guardar el conjunto de datos en csv













