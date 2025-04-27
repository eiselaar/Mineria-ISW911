
install.packages("rvest")
install.packages("dplyr")
install.packages("stringr")


library(rvest)
library(dplyr)
library(stringr)



rm(list = ls())
cat("\014")
options(encoding = "utf-8")


cars <- data.frame(name = character(), price = character(),atributes = character(),location = character(), stringsAsFactors = FALSE)
link = "https://autos.mercadolibre.co.cr/autos-camionetas/"
page = read_html(link)

name=page %>% html_nodes(".poly-component__title") %>% html_text() %>% trimws()
price=page %>% html_nodes(".poly-component__price") %>% html_text() %>% trimws()
year=page %>% html_nodes(".poly-component__attributes-list") %>% html_text() %>% trimws()
year=substr(year, 1, 4)
kilometers=page %>% html_nodes(".poly-component__attributes-list") %>% html_text() %>% trimws() %>% gsub("Km", "", .) 
kilometers=substr(kilometers, 5, nchar(kilometers))%>% trimws()
location=page %>% html_nodes(".poly-component__location") %>% html_text() %>% trimws()

cars = data.frame(name,price,year,kilometers,location, stringsAsFactors = FALSE)

cars <- cars %>% mutate(currency = ifelse(grepl("\\$", price), "dolares", "colones"))
cars$price <-gsub("[₡$,US]", "", cars$price)
cars$price<-as.double(cars$price)
View(cars)

View(cars)


# Cargar la librería tidyr
install.packages("tidyr")
library(tidyr)

# Separar la columna location usando separate()
cars <- cars %>%
  separate(location, into = c("canton", "provincia"), 
           sep = " - ",
           fill = "right")  # por si no hay guion en algunos casos

# Limpiar espacios en blanco
cars <- cars %>%
  mutate(
    canton = trimws(canton),
    province = trimws(province)
  )

# Verificar el resultado
head(cars)


vehiculos_provincia<-cars %>% group_by(provincia) %>% 
  summarise(cantidad=n())

