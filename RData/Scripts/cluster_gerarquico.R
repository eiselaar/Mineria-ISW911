#Clustering jerárquico:
#Comunmente utilizados para medir qué tan "cercanos" están dos puntos o grupos de datos.

library(dplyr)

protein <- read.csv("files/protein.csv",sep = ";")
head(protein)

View(protein)
data <- as.data.frame(scale(protein[,-1]))

View(data)
data$Country = protein$Country
rownames(data) = data$Country
names(data)
View(data)
data$Country<-NULL

hc <- hclust(dist(data, method = "euclidean"),
             method = "ward.D2")

print(hc)
#Distancia: Se calcula una matriz de distancias euclídeas O (cuantificar la cercanía entre puntos en un espacio multidimensional.) entre las filas (países).
#Método de agrupamiento: Se utiliza el método de Ward (ward.D2), que minimiza la varianza dentro de los grupos.

hc
hcd <- as.dendrogram(hc)

print(hc)

nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), 
                cex = 0.7, col = "blue")



# Customized plot; remove labels
plot(hcd, ylab = "Height", nodePar = nodePar, leaflab = "none")
plot(hcd,  xlab = "Height",
     nodePar = nodePar, horiz = TRUE)

plot(hcd, xlim = c(1, 20), ylim = c(1,8))
plot(hcd,  xlab = "Height", nodePar = nodePar, 
     edgePar = list(col = 2:3, lwd = 2:1))


fit <- cutree(hc, k=4)
table(fit)
rect.hclust(hc, k=4, border="steelblue")


# cantidad de cluster 

set.seed(42) # Para reproducibilidad
inertia <- numeric() # Vector para almacenar la inercia

for (k in 1:10) {
  kmeans_model <- kmeans(data, centers = k, nstart = 25)
  inertia[k] <- kmeans_model$tot.withinss
}

# Identificar el punto de inflexión (K óptimo)
optimal_k <- which.min(diff(diff(inertia)))

# Graficar el método del codo con el punto de inflexión marcado
plot(1:10, inertia, type = "b", pch = 19, frame = FALSE,
     xlab = "Cantidad de Clusters (K)", ylab = "Inercia",
     main = "Método del Codo para Determinar K")
points(optimal_k, inertia[optimal_k], col = "red", pch = 19, cex = 2) # Marcar el punto de inflexión
text(optimal_k, inertia[optimal_k], labels = paste("K =", optimal_k), pos = 3, col = "red")
grid()

install.packages("factoextra")
install.packages("cluster")

library(factoextra)
library(cluster)
silhouette_scores <- numeric() # Vector para almacenar los scores

for (k in 2:10) { # El Silhouette Score requiere al menos 2 clusters
  kmeans_model <- kmeans(data, centers = k, nstart = 25)
  ss <- silhouette(kmeans_model$cluster, dist(data))
  silhouette_scores[k] <- mean(ss[, 3]) # Promedio del Silhouette Score
}

# Determinar el K con el mayor Silhouette Score
optimal_k <- which.max(silhouette_scores)

# Graficar el Silhouette Score con el punto óptimo marcado
plot(2:10, silhouette_scores[2:10], type = "b", pch = 19, frame = FALSE,
     xlab = "Cantidad de Clusters (K)", ylab = "Silhouette Score",
     main = "Silhouette Score para Diferentes Valores de K")
points(optimal_k, silhouette_scores[optimal_k], col = "red", pch = 19, cex = 3) # Marcar el punto óptimo
text(optimal_k, silhouette_scores[optimal_k], labels = paste("K =", optimal_k), pos = 3, col = "red")
grid()


####################################################################
# 
install.packages("ape")



library("ape")
library("Rcpp")
library("ape")
library("Rcpp")

par(mar = c(5, 5, 2, 2), cex.axis = 0.5, cex.lab = 0.7, pin = c(6, 6), mai = c(0.8, 0.8, 0.2, 0.2))
plot(as.phylo(hc), type = "unrooted", cex = 0.8, no.margin = TRUE, lwd = 2, font = 3)

# mismo grafico pero mas bonito

# Cargar bibliotecas necesarias
library(ape)
# Crear un conjunto de colores para los clusters
colors <- c("red", "blue", "green", "purple", "orange", "cyan", "pink", "yellow")
num_clusters <- 4 # Define el número de clusters que deseas (puedes ajustarlo)
clus_assignments <- cutree(hc, num_clusters) # Asignar clusters

# Asignar colores a cada cluster
tip_colors <- colors[clus_assignments]

# Graficar el árbol mas bonito 
plot(as.phylo(hc), 
     type = "unrooted", 
     cex = 0.9, 
     no.margin = TRUE, 
     lwd = 2, 
     font = 3, 
     tip.color = tip_colors, # Colores para las hojas según el cluster
     edge.color = "gray40") # Color de las ramas


####################################################################

# otra visualizacion

colors = c("red", "blue", "green", "black")
clus4 = cutree(hc, 4)
plot(as.phylo(hc), type = "fan",  font = 4, tip.color = colors[clus4],
     label.offset = 1, cex = 0.8,lwd = 4)

# un poco mejor

colors <- c("red", "blue", "green", "black")
num_clusters <- 4
clus4 <- cutree(hc, num_clusters) 

# Crear un vector de colores para las líneas según los clusters
edge_colors <- colors[clus4[hc$order]]

# Graficar el árbol en forma de abanico con colores y grosor personalizado
plot(as.phylo(hc), 
     type = "fan",  
     font = 4, 
     tip.color = colors[clus4],      # Colores en las hojas según los clusters
     edge.color = edge_colors,       # Colores de las líneas según los clusters
     label.offset = 1, 
     cex = 0.8, 
     lwd = 5)                        # Grosor de las líneas



# asignar los clustes a los datos para los promedios de cada cluster
num_clusters <- 4
protein$Cluster <- cutree(hc, k = num_clusters)

# Resumen tabular por cluster
cluster_summary <- protein %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean)) # calcular promedios

# Mostrar la tabla
print(cluster_summary)

# extraccion de los datos de 1 cluster especifico 
protein %>% filter(Cluster==1)

