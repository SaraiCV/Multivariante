# Análisis Canónico

# Prepararción de la matriz
library(tidyverse)
library(readxl)
penguins=read_excel("C:/Users/sisig/Downloads/penguins (1).xlsx")

## Exploración de la matriz
#2.- Dimensión de la matriz. La matriz cuenta con 344 observaciones y 9 variables. 
dim(penguins)

#3.- Tipo de variable 
#La base de datos esta conformado por 4 variables tipo caracter y 5 numericas. 
str(penguins)

#4.- Nombre de las variables 
colnames(penguins)

#5.- Se buscan valores perdidos en la matriz
anyNA(penguins)
#No se encuentran valores nulos en la matriz

#6.- Generación de las variables **X**
  X <- penguins %>% 
  select(grosor_pico_mm, largo_pico_mm) %>%
  scale()
head(X)

#7.- Generación de variables **Y**
  Y <- penguins %>%
  select(largo_aleta_mm,masa_corporal_g) %>%
  scale()
head(Y)
#8.- Análisis canónico con un par de  variables

# Libreria
library(CCA)

# Analisis
ac<-cancor(X,Y)

#9.- Visualización de la matriz X
ac$xcoef

#10.- Visualización de la matriz Y
ac$ycoef

#11.- Visualización de la correlación canónica
ac$cor

#12.- Obtención de la matriz de variables canónicas
#Se obtiene multiplicando los coeficientes por cada una de las variables (X1 y Y1)
ac1_X <- as.matrix(X) %*% ac$xcoef[, 1]
ac1_Y <- as.matrix(Y) %*% ac$ycoef[, 1]

#13.- Visualización de los primeros 20 datos
ac1_X[1:20,]
ac1_Y[1:20,]

#14.- Correlación canónica entre variable X1 y Y1

cor(ac1_X,ac1_Y)

#15.- Verificación de la correlación canónica
assertthat::are_equal(ac$cor[1], 
                      cor(ac1_X,ac1_Y)[1])

## Análisis canónico con dos pares de variables
#16.- Calculo de las variables X2 y Y2
ac2_X <- as.matrix(X) %*% ac$xcoef[, 2]
ac2_Y <- as.matrix(Y) %*% ac$ycoef[, 2]

#17.- Agregamos las variables generadas a la matriz original de penguins
ac_df <- penguins %>% 
  mutate(ac1_X=ac1_X,
         ac1_Y=ac1_Y,
         ac2_X=ac2_X,
         ac2_Y=ac2_Y)

#18.- Visualización de los nombres de las variables
colnames(ac_df)

#19.- Generación del gráfico scater plot para la visualización de X1 y Y1
ac_df %>% 
  ggplot(aes(x=ac1_X,y=ac1_Y))+
  geom_point(color="indianred1")

#20.- Generación de un boxplot
ac_df %>% 
  ggplot(aes(x=especie,y=ac1_X, color=especie))+
  geom_boxplot(width=0.5)+
  geom_jitter(width=0.15)+
  ggtitle("Variable Canónica X1 contra Especie")

#Se observa una correlación entre la variable canónica X1 y la variable latente **Especie**
  ac_df %>% 
  ggplot(aes(x=especie,y=ac1_Y, color=especie))+
  geom_boxplot(width=0.5)+
  geom_jitter(width=0.15)+
  ggtitle("Variable Canónica Y1 contra Especie")
ac_df %>% 
  ggplot(aes(x=ac1_X,y=ac1_Y, color=especie))+
  geom_point()+
  ggtitle("Variable Canónica X1 contra Y1")

#21.- Scatter plot con las variables canónicas X2 y Y2 separadas por genero.
ac_df %>% 
  ggplot(aes(x=ac2_X,y=ac2_Y, color=genero))+
  geom_point()+
  ggtitle("Variable Canónica X2 contra Y2")

#No de identifica correlación entre el conjunto de variables X2 y Y2 separadas por genero.
