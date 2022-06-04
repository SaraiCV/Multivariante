#Librerías
library(MASS)
library(class)

# Matriz
#Se trabajará con la base de datos de iris precargada en R.
Z<-as.data.frame(iris)
colnames(Z)

#Se define la matriz de datos y la variable respuesta, con las clasificaciones.
x<-Z[,1:4]
y<-Z[,5]

#Se definen las variables y las observaciones.
n<-nrow(x)
p<-ncol(x)

#Se realiza el gráfico scatter plot.
col.iris<-c("deeppink","green3","royalblue2")[y]
pairs(x, main="Data set Iris, Setosa(rosa), Versicolor(verde), Virginica(azul)", 
      pch=19,col=col.iris)

# Método k-vecinos más próximos
#Se fija una "semilla" (para obtener los mismos valores).
set.seed(1000)

# Creación de los ciclos
#En este caso será un ciclo de k=1 hasta k=20 (el "k" puede variar de manera arbitraria).
#Inicialización de una lista vacia de tamaño 20
knn.class<-vector(mode="list",length=20)
knn.tables<-vector(mode="list", length=20)

#Clasificaciones erróneas
knn.mis<-matrix(NA, nrow=20, ncol=1)
for(k in 1:20){
  knn.class[[k]]<-knn.cv(x,y,k=k)
  knn.tables[[k]]<-table(y,knn.class[[k]])
  # la suma de las clasificaciones menos las correctas
  knn.mis[k]<- n-sum(y==knn.class[[k]])
}

knn.mis

# Número óptimo de k-vecinos
which(knn.mis==min(knn.mis))


#Se visualizan los resultados que nos arrojó el ciclo con el error más bajo.
knn.tables[[14]]
knn.tables[[18]]
knn.tables[[19]]

#El resultado en los tres casos es el mismo, todas las setosa están bien clasificadas,y en versicolor 48 flores están bien clasificadas y dos de ellas se identifican como virginica de las cuales sólo una es clasificada como versicolor.

#Se señala el k mas eficiente
k.opt<-14
knn.cv.opt<-knn.class[[k.opt]]

#Se visualiza la tabla de contingencia con las clasificaciones buenas y malas:
knn.tables[[k.opt]]

#La cantidad de observaciones mal clasificadas:
knn.mis[k.opt]
#Esto quiere decir que de 100 flores, 2 no están bien clasificadas.

# Error de clasificacion (MR)
knn.mis[k.opt]/n

#Ahora se crea un gráfico identificando las clasificaciones correctas y erróneas.

# Grafico de clasificaciones
col.knn.iris<-c("violetred3","green")[1*(y==knn.cv.opt)+1]
pairs(x, main="Clasificación kNN de Iris",
      pch=19, col=col.knn.iris)

#----------------------------------
#      PRACTICA PENGUINS 
#----------------------------------

#En esta practica se realizará el mismo ejercicio pero con una matriz diferente, en este caso se trabajó con la matriz **penguins** la cualse encuentra en la libreria datos ya precargada en R, pero en mi caso la extraeré de excel.  

#Libreria
library(readxl)

#Obtenemos y previsualizamos la matriz
X <- read_excel("C:/Users/USUARIO/Documents/MULTIVARIADA/penguins.xlsx")
X[1:10,]

#Exploración de la matriz 
colnames(X)

#Se convierte la base de datos a un data.frame
X<-data.frame(X)

#Se define la matriz de datos y la variable respuesta con las clasificaciones. Para este caso la clasificación será por especie.
x<-X[,4:7]
y<-X[,2]

#Se definen las variables y las observaciones
n<-nrow(x)
p<-ncol(x)

# Método k-vecinos más próximos
#Se fija una "semilla" (para obtener los mismos valores).
set.seed(1500)

# Creación de los ciclos
#En este caso será un ciclo de k=1 hasta k=30 (el "k" puede variar de manera arbitraria).
#Inicialización de una lista vacia de tamaño 30
knn.class<-vector(mode="list",length=30)
knn.tables<-vector(mode="list", length=30)

# Clasificaciones erróneas
knn.mis<-matrix(NA, nrow=30, ncol=1)
for(k in 1:30){
  knn.class[[k]]<-knn.cv(x,y,k=k)
  knn.tables[[k]]<-table(y,knn.class[[k]])
  knn.mis[k]<- n-sum(y==knn.class[[k]])
}
knn.mis

# Número óptimo de k-vecinos
which(knn.mis==min(knn.mis))

#Se visualiza el resultado que arrojó el ciclo con el error más bajo.
knn.tables[[1]]
#La especie Adelie 18 están clasificados como Chinstrap y 2 en Gentoo, con la especie Chinstrap, existe un número elevado que no está bien clasificados dentro de la especie, ya que se identifican 12 como Adelie y 4 como Gentoo. Respecto a la especie de Gentoo en total nos encontramos 8 pinguinos, de los cuales todos están bien clasificados, 4 en Adelie y 4 en Chinstrap.

#Se señala el k mas eficiente.
k.opt<-1
knn.cv.opt<-knn.class[[k.opt]]

#Se visualiza la tabla de contingencia con las clasificaciones buenas y malas. En este caso es el número 1, ya que en el resultado del ciclo fue el número más pequeño de las 30 iteraciones.
knn.tables[[k.opt]]

#La cantidad de observaciones mal clasificadas:
knn.mis[k.opt]
#Esto quiere decir que de 100 pinguinos, aproximadamente 12 o 13 no están bien clasificados con respecto a la especie.

# Error de clasificacion (MR)
knn.mis[k.opt]/n

#Ahora se crea un gráfico identificando las clasificaciones correctas y erróneas.
# Grafico de clasificaciones
col.knn.iris<-c("mediumpurple4","turquoise")[1*(y==knn.cv.opt)+1]
pairs(x, main="Clasificación kNN de pinguinos por género",
      pch=19, col=col.knn.iris)
