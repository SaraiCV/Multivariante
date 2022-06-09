#Crear los vectores de la matriz

CDM<-c(0,531,1790,2743,540,898,1429,1306,402,484,1884,834,2576,1216,378)
GDJR<-c(532,0,1526,2215,10,787,1165,1840,328,220,1356,698,2047,687,859)
CJ<-c(1806,1535,0,1164,1543,1349,355,3100,1384,1312,847,1092,1007,1088,2181)
TJN<-c(2747,2218,1164,0,2214,2506,1354,4055,2383,2311,863,2091,175,1553,3074)
ZPP<-c(540,9,1533,2209,0,795,1173,1848,336,228,1351,706,2042,682,867)
MTR<-c(906,795,1169,2326,804,0,803,1986,512,573,1709,88,2169,1040,1281)
CHUA<-c(1443,1172,353,1353,1180,809,0,2737,1021,949,696,729,1196,736,1818)
MRD<-c(1316,1849,3099,4061,1857,1991,2738,0,1711,1793,3202,2143,3893,2533,1574)
SLP<-c(408,328,1378,2378,336,511,1017,1702,0,165,1602,447,2221,934,783)
AGCL<-c(491,219,1306,2306,228,567,945,1785,166,0,1572,478,2149,904,866)
HMSLL<-c(1885,1356,847,860,1351,1713,697,3193,1681,1572,0,1633,693,691,2212)
STLL<-c(841,705,1085,2085,714,87,724,2135,448,483,1631,0,1929,962,1216)
MXC<-c(2578,2049,1007,169,2044,2349,1198,3885,2227,2155,694,1935,0,1384,2904)
CUL<-c(1230,701,1086,1550,696,1058,736,2537,1026,917,691,978,1382,0,1557)
ACJ<-c(378,859,2172,3070,867,1280,1812,1566,784,866,2212,1217,2903,1543,0)

#Crear la matriz a prtir de los vectores
datos <- matrix(c(CDM,GDJR,CJ,TJN,ZPP,MTR,CHUA,MRD,SLP,AGCL,HMSLL,STLL,MXC,CUL,
                  ACJ), nrow = 15, ncol = 15 )

#Agregar nombre a las columnas
colnames(datos) <-c("Ciudad de México","Guadalajara","Ciudad Juárez","Tijuana",
                 "Zapopan","Monterrey","Chihuahua","Mérida","San Luis Potosí",
                 "Aguascalientes","Hermosillo","Saltillo","Mexicali","Culiacán",
                 "Acapulco de Juárez")

#Agregar nombre a las filas
rownames(datos) <-c("Ciudad de México","Guadalajara","Ciudad Juárez","Tijuana",
                             "Zapopan","Monterrey","Chihuahua","Mérida",
                            "San Luis Potosí","Aguascalientes","Hermosillo",
                            "Saltillo","Mexicali","Culiacán","Acapulco de Juárez")

#Observamos la base de datos desde una nueva ventana
View(datos) 
#La matriz queda de la siguiente manera
datos

# Transformamos los datos en matriz
datos<-as.matrix(datos)

#Exploración de la matriz
#Dimension
dim(datos)
#Variables
str(datos)
#Nombre de columnas
colnames(datos)
#Datos perdidos
anyNA(datos)

#-----------------------------------
#  Extracción de las filas de la matriz
#-----------------------------------
#Numero de ciudades
n<-nrow(datos)

#------------------------------------
# Escalado multidimensional clásico
#------------------------------------
# 1.- Cálculo de autovalores
# Dentro del objeto modelo se encuentran
# almacenado los valores propios (eigenvalues)



mds.ciudades <- cmdscale(datos, eig = TRUE)

# 2.- Generación del gráfico
plot(mds.ciudades$eig, pch=19, col="magenta", main = "Gráfico de valores propios", 
     xlab="Ciudades", ylab="Valores Propios",
     type="o")
abline(a=0, b=0, col="steelblue4")

# Interpretacion: se identifican autovalores negativos
# Se considera como solución el seleccionar
# r=2 coordenadas principales.


# 3.- Medidas de precision

m<-sum(abs(mds.ciudades$eig[1:2]))/sum(abs(mds.ciudades$eig))

#4.- Obtencion de coordenadas principales fijando
# k=2 y se realice con los dos primeros autovalores.
mds.ciudades<-cmdscale(datos, eig=TRUE, k=2)

x1<-mds.ciudades$points[,1]
x2<-mds.ciudades$points[,2]

# 5.- Generacion del gráfico en dos dimensiones de los
# datos con las coordenadas obtenidas
plot(x1,x2,pch=19, col="violetred2",main = "Distancia entre ciudades de México por carretera",
     sub = "Distancia en Kilómetros", xlim = range(x1)+c(0,600))
text(x1,x2, pos=4, labels = rownames(datos),
     col="black")

# Se invierten los ejes del plot
x2<--x2

plot2<-plot(x1,x2,pch=19, col="purple", main = "Distancia entre ciudades de México por carretera",
            sub = "Distancia en Kilómetros", xlim = range(x1)+c(0,600))
text(x1,x2, pos=4, labels = rownames(datos),
     col="black")
