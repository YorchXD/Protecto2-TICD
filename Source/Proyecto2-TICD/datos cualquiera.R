#library(rpart)
#library(rpart.plot)
library(tree)
#library(party)
#library(randomForest)
#library(evtree)

data(iris)
View(iris)
str(iris)
attach(iris)
nrow(iris)
## paquete tree

arb1 <- tree(Species ~., iris)
arb1

summary(arb1)

plot(arb1)
text(arb1)

misclass.tree(arb1, detail=TRUE)
predict(arb1)
predict(arb1,type="class")
table(predict(arb1))
arb11<- snip.tree(arb1, nodes = c(12, 7))
plot(iris[, 3],iris[, 4], type="n",
     xlab="petal length", ylab="petal width")
text(iris[, 3], iris[, 4], c("s", "c", "v")[iris[, 5]])
partition.tree(arb11, add = TRUE, cex = 1.5)


########################################################################################
##############################Ejemplo de arbol de clasificacion#########################
library(tree)
# Selección muestra entrenamiento
train=sample(seq(length(iris$Species)),length(iris$Species)*0.70,replace=FALSE)
datos.tree = tree(datos$V1559~.,datos,subset=train)
summary(datos.tree)

plot(datos.tree);text(datos.tree,pretty=0)

datos.tree

datos.pred=predict(datos.tree,datos[-train],type="class")
summary(datos.pred)

with(datos[-train],table(datos.pred,V1559))

# Mediante validación cruzada se busca el mejor arbol de decision
cv.datos=cv.tree(datos.tree,FUN=prune.misclass)
cv.datos

## [1] "prune"         "tree.sequence"

plot(cv.datos)
########################################################################################
