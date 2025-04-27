# EDA Analisis Exploratorio de datos

# EDA Analisis Exploratorio de datos


1. CREACION DE CONEXIONES

install.packages("RODBC")
library(RODBC)



2.⁠ ⁠conexion y crear las consultas

con.SqlServer <-odbcConnect("ODBC_SQL",uid = "sa",pwd = "Admin12345")
facturas<-sqlQuery(con.SqlServer,"select *from invoices")
View(facturas)

3.⁠ ⁠crear el diccionario de datos 

Documento en word que describe sus variables y sus propositos

4.⁠ ⁠explorar el conjunto de datos (summary,tipos de datos,dimenciones)

library(dplyr)
glimpse(facturas)
str(facturas)
dim(facturas)
class(facturas)


5.⁠CONVERSION DE TIPO DE DATOS

facturas$OrderDate<-as.Date(facturas$OrderDate)
facturas$Quantity<-as.numeric(facturas$Quantity)


#6. IDENTIFICACION DE VALORES NULOS

sum(is.na(facturas)) 
nulos <- facturas[!complete.cases(facturas),]
nrow(nulos)  # Cantidad de filas con al menos un NA

apply(is.na(facturas), 2, mean)   # Porcentaje de NA por columna
apply(is.na(facturas), 2, sum)    # Cantidad de NA por columna
apply(is.na(facturas), 2, which)  # Posición de NA por columna

sapply(facturas, function(x) sum(is.na(x)))

#SOLUCION

facturas$Region<-NULL
facturas$ShipRegion<-NULL
facturas$PostalCode<-NULL
facturas$ShipPostalCode<-NULL
facturas$ShippedDate<-NULL


#7. IDENTIFICACION DE VALORES DUPLICADOS

sum(duplicated(facturas))


#8. IDENTIFICACION DE VALORES ATIPICOS

colnames(facturas)
facturas<-facturas %>% mutate(total_sales=Quantity*UnitPrice)
View(facturas)

boxplot(facturas$total_sales,boxwex=0.8)

boxplot(total_sales ~ Country, data = facturas,
        main = "Distribución de Ventas Totales por País",
        xlab = "País", ylab = "Ventas Totales",
        col = "lightblue", border = "darkblue", boxwex = 0.8)

boxplot(total_sales ~ ProductName, data = facturas,
        main = "Distribución de Ventas Totales por Producto",
        xlab = "Producto", ylab = "Ventas Totales",
        col = "lightgreen", border = "darkgreen", las = 2, boxwex = 0.8)

boxplot(total_sales ~ interaction(Country, ProductName), data = facturas,
        main = "Distribución de Ventas por País y Producto",
        xlab = "País - Producto", ylab = "Ventas Totales",
        col = "orange", border = "red", las = 2, boxwex = 0.8)

library(ggplot2)

ggplot(facturas, aes(x = Country, y = total_sales, fill = ProductName)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Ventas Totales por País y Producto", x = "País", y = "Ventas Totales")



#9. IDENTIFICACION DE CORRELACIONES

numericas <- sapply(facturas, is.numeric)
df_numerico <- facturas[, numericas]

cor_matrix <- cor (df_numerico, use = "complete.obs")
print(cor_matrix)


# Cargar librerías necesarias
library(ggplot2)
library(reshape2)  # Para convertir la matriz en formato largo
library(corrplot)  # Para un gráfico especializado en correlaciones

# Filtrar solo las columnas numéricas
numericas <- sapply(facturas, is.numeric)
df_numerico <- facturas[, numericas]

# Calcular la matriz de correlación
cor_matrix <- cor(df_numerico, use = "complete.obs")

# 📌 OPCIÓN 1: Heatmap con ggplot2 --------------------------
# Convertir la matriz de correlación en un formato largo para ggplot2
cor_data <- melt(cor_matrix)

# Crear el heatmap con ggplot2
ggplot(cor_data, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Matriz de Correlación", x = "", y = "", fill = "Correlación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 📌 OPCIÓN 2: Usando corrplot ------------------------------
corrplot(cor_matrix, method = "color", type = "lower",
         col = colorRampPalette(c("blue", "white", "red"))(200),
         tl.col = "black", tl.srt = 45)  # Etiquetas inclinadas



#10 IDENTIFICACION DE CONCENTRACIONES DE DATOS

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)

# 📌 1. HISTOGRAMA (Distribución de Frecuencia)
ggplot(facturas, aes(x = total_sales)) +
  geom_histogram(binwidth = 100, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Ventas Totales", x = "Total Sales", y = "Frecuencia") +
  theme_minimal()

# 📌 2. GRÁFICO DE DENSIDAD (Para ver la concentración de datos)
ggplot(facturas, aes(x = total_sales)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Densidad de Ventas Totales", x = "Total Sales", y = "Densidad") +
  theme_minimal()

# 📌 3. BOXPLOT POR PAÍS (Ver concentración por categoría)
ggplot(facturas, aes(x = Country, y = total_sales)) +
  geom_boxplot(fill = "lightblue", color = "black", alpha = 0.5) +
  labs(title = "Distribución de Ventas por País", x = "País", y = "Total Ventas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 📌 4. HEATMAP PARA CONCENTRACIÓN DE DATOS (País vs. Producto)
library(reshape2)  # Para transformar datos a formato largo

# Crear un conteo de ventas por País y Producto
df_concentracion <- facturas %>%
  group_by(Country, ProductName) %>%
  summarise(Ventas_Totales = sum(total_sales, na.rm = TRUE)) %>%
  ungroup()

# Transformar datos para ggplot
df_melted <- melt(df_concentracion, id.vars = c("Country", "ProductName"))

# Graficar el Heatmap
ggplot(df_melted, aes(x = Country, y = ProductName, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Concentración de Ventas por Producto y País", x = "País", y = "Producto", fill = "Ventas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#11  IMPUTACION DE DATOS

# Cargar librerías necesarias
library(dplyr)
library(ggplot2)

# Función para reemplazar valores atípicos por la mediana en cada grupo
outliers_mediana <- function(df, columna, groupby_col) {
  df[[groupby_col]] <- as.factor(df[[groupby_col]])  # Convertir en factor
  
  df <- df %>% 
    group_by(!!sym(groupby_col)) %>%
    mutate(
      Q1 = quantile(!!sym(columna), 0.25, na.rm = TRUE),
      Q3 = quantile(!!sym(columna), 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      limite_inferior = Q1 - 1.5 * IQR,
      limite_superior = Q3 + 1.5 * IQR,
      mediana = median(!!sym(columna), na.rm = TRUE),
      !!sym(columna) := ifelse(
        (!!sym(columna) < limite_inferior | !!sym(columna) > limite_superior),
        mediana, !!sym(columna)
      )
    ) %>%
    select(-Q1, -Q3, -IQR, -limite_inferior, -limite_superior, -mediana) %>%
    ungroup()
  
  return(df)
}

# Aplicar la función al DataFrame 'facturas'
df_resultado <- outliers_mediana(facturas, 'total_sales', 'Country')

# Verificar los valores mínimo y máximo después de la corrección
df_resultado %>% 
  group_by(Country) %>% 
  summarise(
    Min_After = min(total_sales, na.rm = TRUE),
    Max_After = max(total_sales, na.rm = TRUE)
  )

# Graficar con ggplot2 sin mostrar valores atípicos
ggplot(df_resultado, aes(x = Country, y = total_sales)) +
  geom_boxplot(aes(group = Country), fill = "lightblue", color = "black", alpha = 0.5, outlier.shape = NA) +  
  labs(title = "Valores Atípicos Reemplazados por la Mediana",
       x = "País", y = "Total Ventas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



11. creacion de funcion para el manejo de NAs
12. creacion de funcion para el manejo de atipicos 
13. guardar el conjunto de datos en csv


####################################################################################


