#ANALISIS DISCRIMINANTE (LDA)

##Importación de la matriz

#1.- Se cargan los datos de la base de Iris

library(MASS)
Z<-as.data.frame(iris)
head(Z)

#2.- Se define la matriz de datos y la variable respuesta con las categorías.
x<-Z[,1:4]
y<-Z[,5]

#3.- Definir como n y p el número de flores y variables
n<-nrow(x)
p<-ncol(x)

#4.- Se aplica el Análisis discriminante lineal (LDA) con Cross validation (cv): clasificación optima
lda.iris<-lda(Z$Species~.,data=Z,CV=TRUE)

#5.- lda.iris$class contiene las clasificaciones hechas por CV usando LDA.

lda.iris$class

#6.- Creación de la tabla de clasificaciones buenas y malas

table.lda<-table(y,lda.iris$class)
table.lda

#7.- Proporción de errores
mis.lda<- n-sum(y==lda.iris$class)
mis.lda/n

#8.- scater plot (Buenas clasificaciones en negro y malas en rojo)
col.lda.iris<-c("indianred1","black")[1*(y==lda.iris$class)+1]
pairs(x,main="Buena clasificación (negro), mala clasificación (rojo)",
      pch=19,col=col.lda.iris)

#9.- Probabilidad de pertenencia a uno de los tres grupos
lda.iris$posterior[1:10,]
