# Librerias 
library(cluster.datasets)
library(dendextend)
library(circlize)

# Base de datos 
data("all.mammals.milk.1956")
AMM=all.mammals.milk.1956

# Exploración de la matriz

head(AMM)
dim(AMM)
str(AMM)
anyNA(AMM)

# Cálculo de la matriz de distancias de Mahalonobis

dist.AMM<-dist(AMM[,2:6])

Convertir los resultados del calculo de la distancia a una matriz de datos y me indique 3 digitos.   

round(as.matrix(dist.AMM)[1:6, 1:6],3)


# Calculo del dendrograma
dend.AMM<-as.dendrogram(hclust(dist.AMM))

# Creación del dendrograma

#Agregamos etiquetas al gráfico.

AMM.nombres=AMM
rownames(AMM.nombres)= AMM.nombres$name
AMM.nombres=AMM.nombres[,-1]

#Construimos de nuevo el grafico
plot(as.dendrogram(hclust(dist(AMM.nombres))))

# Modificamos el gráfico

#Guardar las etiquetas en un objeto "L"
L=labels(dend.AMM)

labels(dend.AMM)=AMM$name[L]

#Cambiar el tamaño de las etiquetas
dend.AMM %>%
  set(what="labels_col", "blue") %>% #Colores etiqueta
  set(what="labels_cex", 0.8) %>%
  plot(main="Dendrograma de mamiferos")
circlize_dendrogram(dend.AMM, labels_track_height = NA,
                    dend_track_height = 0.1,
                    sector.index ="b",
                    track.index = 3)
