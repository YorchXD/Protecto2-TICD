#pagina de donde hacer el proyecto
#https://rstudio-pubs-static.s3.amazonaws.com/293405_4029f1f23f834b7195189d5504a436b2.html
#install.packages("dplyr")
install.packages("clara")
library(dplyr)
library(readr)
library (ggplot2)

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

#Muestra que tipo de vectores tiene la tabla
str(ad.data2)

#Muestra la dimensón de la tabla 
dim(ad.data2)

#Se visualizan todos los datos
View(ad.data2)

#Separacion de casos publicitarios y no publicitarios
ad.noAd<-split(ad.data2, ad.data2$V1559)
View(ad.noAd)
View(ad.noAd$ad)

dim(ad.noAd$ad.)
dim(ad.noAd$nonad.)

#Frecuencias de ad/nonad
frec<-c(nrow(ad.noAd$ad.),nrow(ad.noAd$nonad.))
frec

nombresFrec<-c("ad","nonad")
tablaFrec<-data.frame(nombresFrec,frec)

#Grafico
ggplot(data=tablaFrec, aes(x=nombresFrec, y=frec, fill=nombresFrec)) + geom_bar(stat="identity")



ad.data3<-ad.data2[,-c(1:4,ncol(ad.data2))]

#ad.data3<-ad.data2

#estandarizando valores
ad.data4 <- scale(ad.data3)

colMeans(ad.data3)
View(ad.data3)
dim(ad.data3)

# 3. Matriz de correlacion con p.value
library("corrplot")
library("Hmisc")
cor_2 <- rcorr(as.matrix(ad.data3))
cor_2


Cluster_df <- kmeans(ad.data3, 2, algorithm = "Forgy")


# Determine number of clusters
wss <- (nrow(ad.data3)-1)*sum(apply(ad.data3,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(ad.data3,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 


# K-Means Clustering with 5 clusters
fit <- kmeans(ad.data3, 2)

# Cluster Plot against 1st 2 principal components

# Centroid Plot against 1st 2 discriminant functions
#install.packages("fpc")
library(fpc)
plotcluster(ad.data3, fit$cluster) 

# K-Means Clustering with 5 clusters
fit <- kmeans(ad.data3, 10)

#install.packages("factoextra")
library("factoextra")
fviz_cluster(fit, data = ad.data3)
fviz_cluster(fit, data = ad.data3,ellipse.type = "convex",palette = "jco",ggtheme = theme_minimal())





















ad.data4<-ad.data3

library("Hmisc")
library("corrplot")
library("ggpubr")
library("ggplot2")

# 3. Matriz de correlacion con p.value
cor_2 <- rcorr(as.matrix(ad.data4))
cor_2

# 4. Gráficos de colores

#M<-cor(datos_trans)
#corrplot(M, method = "pie")

# 4.1 Obteniendo la matriz de correlación con sus respectivos p-values
cor_5 <- rcorr(as.matrix(ad.data4))

# 4.2 Extrayendo la matriz de correlación
M <- cor_5$r

# 4.3 Extrayendo la matriz de p-values
p_mat <- cor_5$P

View(p_mat)

# 4.4 Realizando la paleta de colores que se utilizará en la matriz gráfica
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

# 4.5 Marcando los coeficientes insignificantes de acuerdo con el nivel de significancia del valor p especificado
corrplot(M, method = "color", col = col(200),  
         type = "full", order = "hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "darkblue", tl.srt = 45, #Text label color and rotation
         # Combine with significance level
         p.mat = p_mat, sig.level = 0.01,  
         # hide correlation coefficient on the principal diagonal
         diag = FALSE 
)

# 4.6 Mostrando la matriz triangular superior para visualizar mejor los datos
corrplot(M, method = "color", col = col(200),  
         type = "upper", order = "hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "darkblue", tl.srt = 45, #Text label color and rotation
         # Combine with significance level
         p.mat = p_mat, sig.level = 0.01,  
         # hide correlation coefficient on the principal diagonal
         diag = FALSE 
)

# 4.7 Mostrando la matriz triangular inferior para visualizar mejor los datos
corrplot(M, method = "color", col = col(200),  
         type = "lower", order = "hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "darkblue", tl.srt = 45, #Text label color and rotation
         # Combine with significance level
         p.mat = p_mat, sig.level = 0.01,  
         # hide correlation coefficient on the principal diagonal
         diag = FALSE 
)

#NOTA: Al analizar las distintas técnicas visuales y de colores, se puede observar que el coeficiente con mayor p-value se encuetra en la matriz inferior y está relacionado con el "Tmax" y "fats"

# 5. Gráfico de dispersión
ggscatter(ad.data4, x = "fats", y = "Tmax",add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.coeff.args = list(method = "pearson", label.x = 10, label.sep = "\n"))














