#install.packages("dplyr")
library(dplyr)
library(readr)

#Descargar documentoc con los datos
Web <- "http://archive.ics.uci.edu/ml/machine-learning-databases/internet_ads/ad.data"

#Agregar el documento descargado a una tabla
ad.data <- read.csv(Web, header=FALSE, sep=",", dec = ".")  

#Muestra la dimensón de la tabla 
dim(ad.data)

#Se visualizan todos los datos
View(ad.data)

#copia de los datos en una nueva tabla para no volver a cargar los datos
ad.data1<-ad.data

#Reemplazar los "?" en NA
ad.data1[ad.data1=="   ?"] <- NA
ad.data1[ad.data1=="     ?"] <- NA
ad.data1[ad.data1=="?"] <- NA

#Muestra que tipo de vectores tiene la tabla
str(ad.data1)

#Muestra si existen NA
is.na(ad.data1$V1)

#Verifica si existe algun NA
any(is.na(ad.data1$V1))

#Se visualizan todos los datos
View(ad.data1)

#Se visualizan en la consola todos los datos de un vector
ad.data1$V1

#Muestra la dimensón de la tabla 
dim(ad.data1)

#omite las filas que contengan NA
ad.data2<-na.omit(ad.data1)

#Muestra la dimensón de la tabla 
dim(ad.data2)

#Se visualizan todos los datos
View(ad.data2)


