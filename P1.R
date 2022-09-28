#Este es mi primer script
#Mónica Lara Escalante
#19 de septiembre

#Operaciones de calculadora ####
2+5 
5*3

#Operadores con los que podemos hacer cálculos
1:5 #los dos puntos sirven para secuencias
seq(1,10,0.5)
c("a", "b", "c") #vector
40<80 #operación lógica

2+2==5 #es falso

T==TRUE
F==FALSE

#Objetos ####
x <- 4
x
print(x)

#Vectores ###
y <- c(2, 4, 6)
y <- c("Primaria", "Secundaria")

y[2] #segundo elemento
y[3]

sexo <- 1:2
names(sexo)<- c("Femenino", "Masculino")
#quiero declarar un tercer elemento
sexo[3]<-3
names(sexo)[3]<- "No binarie"
sexo


#Matrices ####
m <- matrix(nrow=2, 
            ncol=3, 
            data=1:6, 
            byrow=T)
m[1,]
m[,1]
m[1,1]

dim(m)


n<- 1:6
dim(n)<-c(2,3)

xx<-10:12
yy<-14:16

#combinar por renglones
rbind(xx,yy)
rbind(yy,xx)

#combinar por columnas
cbind(xx,yy)
cbind(yy,xx)


#Listas ####
lista<-list(m, sexo, xx, x)
lista


#Funciones ####
sum(10, 20, 30)
rep("buenas tardes", times=17)
sqrt(9)
help(sum)
example(sum)

sum(10, 20, 30, NA, na.rm=T)

#Para saber qué tenemos en nuestro ambiente
ls()
#Para guardar mi ambiente
save.image("miprimerambiente.RData")
#puedo limpiar mi ambiente y luego volver a abrirlo
rm(list=ls())
#lo vuelvo a abrir
load("miprimerambiente.RData")
rm() #para eliminar objetos

gc()
#reporta cuanto estamos usando y gastando en la memoria

#quiero saber donde estoy trabajando
getwd()

#instalar paquetes ####
install.packages("foreign", dependencies = TRUE)
install.packages("haven", dependencies = TRUE)

library(foreign)
library(haven)

ejemplo_dbf <- foreign::read.dbf("datos/ejemplo_dbf.dbf")
