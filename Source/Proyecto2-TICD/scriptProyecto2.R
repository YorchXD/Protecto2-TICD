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








########################################################################################
####################################Limpieza de datos###################################

library(dplyr)
library(readr)

#Descargar documentoc con los datos
Web <- "http://archive.ics.uci.edu/ml/machine-learning-databases/internet_ads/ad.data"

#Agregar el documento descargado a una tabla
ad.data <- read.csv(Web, header=FALSE, sep=",", dec = ".")  

#Muestra la dimensón de la tabla 
dim(ad.data)

#Se visualizan todos los datos
#View(ad.data)

#copia de los datos en una nueva tabla para no volver a cargar los datos
ad.data1<-ad.data

#Reemplazar los "?" en NA
ad.data1[ad.data1=="   ?"] <- NA
ad.data1[ad.data1=="     ?"] <- NA
ad.data1[ad.data1=="?"] <- NA

#Muestra que tipo de vectores tiene la tabla
str(ad.data1)

#Muestra si existen NA
#is.na(ad.data1$V1)

#Verifica si existe algun NA
#any(is.na(ad.data1$V1))

#Se visualizan todos los datos
#View(ad.data1)

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
#View(ad.data2)

########################################################################################

########################################################################################
######################Seleccion de datos para entrenamiento#############################

#install.packages("magrittr")
library(magrittr) #es ára ocupar la funcionalidad %>%

# Selección muestra entrenamiento
datos <- ad.data2 %>%select_(~V1, ~V2, ~V3, ~V4, ~V1559)

datos$V1<-as.numeric(as.character(datos$V1))
datos$V2<-as.numeric(as.character(datos$V2))
datos$V3<-as.numeric(as.character(datos$V3))
datos$V4<-as.numeric(as.character(datos$V4))
str(datos)

datos1<- datos %>%select_(~V1, ~V2, ~V3, ~V4)

# división de la muestra en entrenamiento y validación
train=sample(seq(length(datos$V1559)),length(datos$V1559)*0.70,replace=FALSE)
########################################################################################

########################################################################################
library(MASS)

# Lineal Discriminat Analisys
ad=lda(datos$V1559~.,data=datos)

#predicción
probs=predict(ad,newdata=datos,type="prob")
data.frame(probs)[1:5,]

table(probs$class,datos$V1559)
mean(probs$class==datos$V1559) #porcentaje de bien clasificados

# Curva ROC
#install.packages("ROCR")
library("ROCR")
pred <- prediction(probs$posterior[,2],datos$V1559)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
abline(a=0, b= 1)

########################################################################################

########################################################################################

library(MASS)

# Lineal Discriminat Analisys
ad=lda(datos$V1559[train]~.,data=datos[train,])

#predicción
probs=predict(ad,newdata=datos[-train,],type="prob")
data.frame(probs)[1:5,]

table(probs$class,datos$V1559[-train])
mean(probs$class==datos$V1559[-train]) #porcentaje de bien clasificados
########################################################################################

########################################################################################
library(class)
# Selección de variables
x=data.frame(datos[,-ncol(datos)])
str(x)
# K-Nearest Neighbors
knn.prd=knn(x[train,],x[-train,],datos$V1559[train],k=3,prob=TRUE)
table(knn.prd,datos$V1559[-train])
mean(knn.prd==datos$V1559[-train]) #porcentaje de bien clasificados

# Validación cruzada con la muestra de entrenamiento
knn.cross=knn.cv(x[train,],datos$V1559[train],k=3,prob=TRUE)
table(knn.cross,datos$V1559[train])
#mean(knn.cross==datos$V1559[-train]) #porcentaje de bien clasificados
########################################################################################

########################################################################################
#install.packages("tree")
library(tree)

#View(train)
datos.tree = tree(datos$V1559~.,datos,subset=train)
summary(datos.tree)
plot(datos.tree);text(datos.tree,pretty=0)
datos.tree
tree.pred=predict(datos.tree,datos[-train],type="class")
summary(tree.pred)
with(datos[-train],table(tree.pred,V1559))


# Mediante validación cruzada se busca el mejor arbol de decision
cv.datos=cv.tree(datos.tree,FUN=prune.misclass)
cv.datos
plot(cv.datos)
########################################################################################

########################################################################################
#install.packages("C50")
library(C50)

# Selección de una submuestra de 105 (el 70% de los datos)
set.seed(101)
iris.indices <- sample(1:nrow(iris),size=105)
iris.entrenamiento <- iris[iris.indices,]
iris.test <- iris[-iris.indices,]

modelo <- C5.0(V1559~., data = datos[train,],rules=TRUE)
modelo

# predicción
prediccion <- predict(modelo,newdata=datos[-train,])
# Matriz de confusión
tabla <- table(prediccion, datos[-train,]$V1559)
tabla

# % correctamente clasificados
100 * sum(diag(tabla)) / sum(tabla)

########################################################################################


########################################################################################
#install.packages("e1071")
library("e1071")

# se estima un modelo svm lineal para la muestra de entrenamiento
svmfit=svm(datos$V1559~.,data=datos,kernel="linear",scale=FALSE,subset=train)
print(svmfit)

table(datos$V1559[train],svmfit$fitted)

# Predicción para la muestra test
svm.pred=predict(svmfit,datos[-train,])
summary(svm.pred)

with(datos[-train,],table(svm.pred,V1559))

# se estima un modelo svm lineal para la muestra de entrenamiento y se predice la muestra de test
svmfit2=svm(datos$V1559~.,data=datos,kernel="radial",scale=FALSE,subset=train,probability=TRUE)
print(svmfit2)

svm.pred=predict(svmfit2,datos[-train,],probability=TRUE)
summary(svm.pred)

with(datos[-train,],table(svm.pred,V1559))
########################################################################################

########################################################################################
library(e1071)
m <- naiveBayes(V1559 ~ ., data = datos)
table(predict(m, datos), datos$V1559)
########################################################################################


########################################################################################
#######################################En duda##########################################
#install.packages("neuralnet")
library(neuralnet)

# Leemos los datos y hacemos dos conjuntos uno de entrenamiento y otro de test
#data(infert, package="datasets")
#str(infert)

#n        <- nrow(datos)
#muestra  <- sample(n,n*.70)

train1    <- datos[train, ]
test     <- datos[-train, ]
# Entrenamos la red neuronal
net.infert <- neuralnet(as.numeric(V1559)~V1+V2+V3+V4, train1,err.fct="sse", linear.output=FALSE,likelihood=TRUE)
plot(net.infert)

# Computamos la red neuronal
resultados <- compute(net.infert,data.frame(test[,1:4]))$net.result

# recodificamos los resultados y evaluamos
test.result=ifelse(resultados>0.5,2,1)
table(test.result,as.numeric(test$V1559))
########################################################################################



########################################################################################
############################Analisis discriminante sin train############################
library(MASS)

# Lineal Discriminat Analisys
ad=lda(datos$V1559~.,data=datos)

#predicción
probs=predict(ad,newdata=datos,type="prob")
data.frame(probs)[1:5,]

table(probs$class,datos$V1559)
mean(probs$class==datos$V1559) #porcentaje de bien clasificados

# Curva ROC
#A veces no funciona la prediccion, si sucede, es mejor reinstalar el package
#install.packages("ROCR")
library("ROCR")
pred <- prediction(probs$posterior[,2],datos$V1559)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
abline(a=0, b= 1)
########################################################################################

########################################################################################
############################Analisis discriminante con train############################

library(MASS)

# Lineal Discriminat Analisys
ad=lda(datos$V1559[train]~.,data=datos[train,])

#predicción
probs=predict(ad,newdata=datos[-train,],type="prob")
data.frame(probs)[1:5,]

table(probs$class,datos$V1559[-train])
mean(probs$class==datos$V1559[-train]) #porcentaje de bien clasificados
########################################################################################




########################################################################################
library(e1071)
m <- naiveBayes(V1559 ~ ., data = datos)

#predicción
predic.naiveBayes=predict(m, datos)

table(predic.naiveBayes, datos$V1559)

mean(predic.naiveBayes==datos$V1559) #porcentaje de bien clasificados
########################################################################################










########################################################################################
#######################################En duda##########################################
#install.packages("neuralnet")
library(neuralnet)

train1    <- datos[train, ]
test     <- datos[-train, ]
# Entrenamos la red neuronal
net.infert <- neuralnet(as.numeric(V1559)~V1+V2+V3+V4, train1,err.fct="sse", linear.output=FALSE,likelihood=TRUE)
plot(net.infert)

# Computamos la red neuronal
resultados <- compute(net.infert,data.frame(test[,1:4]))$net.result

# recodificamos los resultados y evaluamos
test.result=ifelse(resultados>0.5,2,1)
table(test.result,as.numeric(test$V1559))
32########################################################################################

########################################################################################
##############################Ejemplo de arbol de clasificacion#########################
library(tree)
# Selección muestra entrenamiento
train=sample(seq(length(iris$Species)),length(iris$Species)*0.70,replace=FALSE)
iris.tree = tree(iris$Species~.,iris,subset=train)
summary(iris.tree)

plot(iris.tree);text(iris.tree,pretty=0)

iris.tree

tree.pred=predict(iris.tree,iris[-train],type="class")
summary(tree.pred)

with(iris[-train],table(tree.pred,Species))

# Mediante validación cruzada se busca el mejor arbol de decision
cv.iris=cv.tree(iris.tree,FUN=prune.misclass)
cv.iris

## [1] "prune"         "tree.sequence"

plot(cv.iris)
########################################################################################

########################################################################################
######################################No funciona#######################################
install.packages("tree")
library(tree)

datos.tree = tree(datos$V1559~.,datos,subset=train)
str(datos)
summary(datos.tree)
plot(datos.tree);text(datos.tree,pretty=0)
datos.tree
datos[-train,]
datos
datos.tree
tree.pred=predict(datos.tree,datos[-train,],type="class")
summary(tree.pred)
with(datos[-train,],table(tree.pred,V1559))


# Mediante validación cruzada se busca el mejor arbol de decision
cv.datos=cv.tree(datos.tree,FUN=prune.misclass)
cv.datos
plot(cv.datos)
########################################################################################













