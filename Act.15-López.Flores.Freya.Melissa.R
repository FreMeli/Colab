#Alumno: Freya Melissa López Flores
#No. de cuenta: A225952-0
#Profesor: Lázaro Bustio Martínez
#Fecha de entrega: 08/03/2023
#Objetivo: El objetivo de este programa es aprender a hacer un análisis exploratorio de datos en R
#https://www.kaggle.com/code/alejandroguipe/analisis-exploratorio-de-datos-con-lenguaje-r/report 

#1. Instalación de paquetes:
install.packages("ggplot2") #es un potente paquete de graficación y visualización de datos
install.packages("corrplot") # para el cálculo y la graficación de la matriz de correlación de un conjunto de datos. 
install.packages("plyr") # para la manipulación de datos y subconjunntos de los mismo
install.packages("plotrix") #para la graficación de diagramas de torta en 3D.
library(ggplot2)
library(corrplot)
library(plyr)
library(plotrix)
#2. Carga y reorganización de datos:
datos<-read.csv("/Users/uobvvbcuvyvflores/Documents/Ciencia de Datos/T15.R.StudentsPerformance.csv") 
#cargamos los datos del archivo StudentsPerformance.csv al data frame “datos”

colnames(datos)<-c("gender","race","education", "lunch", "testprep", "math","reading", "writing")
#renombramos la columnas del data frame para facilitar lectura y escritura de código

#Separamos nuestra base de datos de acuerdo al nivel de educación de los padres en 6 conjuntos.
datos1 <- datos[datos$education=="associate's degree",]
datos2 <- datos[datos$education=="bachelor's degree",]
datos3 <- datos[datos$education=="high school",]
datos4 <- datos[datos$education=="master's degree",]
datos5 <- datos[datos$education=="some college",]
datos6 <- datos[datos$education=="some high school",]

#3. Resumen y visualización de la composición los datos:
# head y tails nos permiten ver las primeras y últimas 6 filas del data frame “datos”.
head(datos) 
tail(datos)
#El comando names observamos los nombres de las columnas de la matriz.
names(datos)

#El comando str lista el conjunto de variables que se encuentran en la base datos 
#junto a algunos datos particulares que destacan el número y naturaleza de las variables.
str(datos)

#comando summary nos ofrece un resumen de las variables tanto numéricas como no numéricas, 
#sus frecuencias, cantidad, media, mediana y valores máximos y mínimos.
summary(datos)


#4. Análisis descriptivo de los datos:
#Medidas de tendencia central por nivel de educación de los padres
summary(datos1) 
summary(datos2) 
summary(datos3) 
summary(datos4) 
summary(datos5) 
summary(datos6) 
#Observaciones:
#la media y mediana más alta de todos los grupos separados por educación de los padres son las del grupo de los estudiantes que tienen padres con nivel de educación “master’s degree”
#la media y mediana más baja, particularmente en la asignatura de matemáticas es la del grupo con padres que tienen nivel de educación “asoociate’s degree”.
#En las asignaturas de lectura y escritura, la media y mediana más baja es la del grupo con padres que tienen nivel de educación “high school”.

#Observaciones de las medidas de desviación de las variables numéricas de acuerdo al nivel de educación de los padres
#sd para calcular la desviación estándar:
#columnas de 6 a la 8 que contienene las notas de las asignaturas a cada uno de las base de datos separadas por eduación de los padres (datos1, datos2,…)
sd1=c(sd(datos1[,6]),sd(datos1[,7]),sd(datos1[,8]))
sd2=c(sd(datos2[,6]),sd(datos2[,7]),sd(datos2[,8]))
sd3=c(sd(datos3[,6]),sd(datos3[,7]),sd(datos3[,8]))
sd4=c(sd(datos4[,6]),sd(datos4[,7]),sd(datos4[,8]))
sd5=c(sd(datos5[,6]),sd(datos5[,7]),sd(datos5[,8]))
sd6=c(sd(datos6[,6]),sd(datos6[,7]),sd(datos6[,8]))

#el vector se compone de la desviación estándar de cada columna que corresponde a las notas de las asignaturas matemáticas, lectura y escritura
notas=c("Notas de matemática", "Notas de Lectura", "Notas de escritura")
#Se construye un data frame para mostrar toda la información de manera organizada
datosd= data.frame(notas,sd1,sd2,sd3,sd4,sd5,sd6)
colnames(datosd)=c("                  ","  SDAD  ","  SDBD  ", "  SDHS  ", "  SDMD  ", "  SDSC  ", "  SDSH  ") 
datosd
#SD: Desviación Estándar
#SDAD: SD Associate's Degree
#SDBD: SD Bachelor's Degree
#SDHS: SD High School
#SDMD: SD Master's Degree
#SDSC: SD Some College
#SDSH: SD Some High School

#5. Análisis Gráfico
#Matriz de dispersión
# matriz de dispersión de los datos a través del comando pairs,
#muestra gráficamente el grado de correlación entre las variables
pairs(datos1, main="MDD Associate's Degree")  
pairs(datos2, main="MDD Bachelor's Degree")  
pairs(datos3, main="MDD High School")  
pairs(datos4, main="MDD Master's Degree")  
pairs(datos5, main="MDD Some College") 
pairs(datos6, main="MDD Some High School")  
#Observaciones:
#El patrón que se percibe en todos los grupos según el nivel de educación en la matriz de dispersión es una fuerte correlacion positiva entre las notas de lectura y escritura y también entre matematicas y demás asignaturas.
#implica que a mejores notas en lectura los estudiantes sacan mejores notas en escritura y viceversa. 


#Diagramas de dispersión de todas las asignaturas de acuerdo al nivel de educación de los padres
# consolidamos sólo para las variables numéricas (notas de matemáticas, lectura y escritura
mdd1=ggplot(datos, aes(math,reading, color=education)) +geom_point() + stat_smooth(method = "loess", formula = y ~ x)+ labs(title="Diagrama de Dispersión Notas de Matemáticas vs Notas de Lectura")+ labs(x="Notas de Matemática", y="Notas de Lectura") 
mdd1

mdd2=ggplot(datos, aes(reading,writing, color=education)) +geom_point() + stat_smooth(method = "loess", formula = y ~ x) + labs(title="Diagrama de Dispersión Notas de Lectura vs Notas de Escritura")+ labs(x="Notas de Lectura", y="Notas de Escritura") 
mdd2

mdd3=ggplot(datos, aes(math,writing, color=education)) +geom_point() + stat_smooth(method = "loess", formula = y ~ x) + labs(title="Diagrama de Dispersión Notas de Matemática vs Notas de Escritura")+ labs(x="Notas de Lectura", y="Notas de Escritura") 
mdd3

#Histogramas
#comando hist podemos obtener los diferentes histogramas 
#comando layout que organiza nuestra ventana de visualización en 6 secciones.
#Histograma de notas de mate
layout(matrix(c(1:6), nrow=2, byrow=FALSE))
hist(datos1$math, main="Associate's Degree", xlab="Notas de Matemáticas", ylab="Frecuencia")
hist(datos2$math, main="Bachelor's Degree", xlab="Notas de Matemáticas", ylab="Frecuencia")
hist(datos3$math, main="High School", xlab="Notas de Matemáticas", ylab="Frecuencia")
hist(datos4$math, main="Master's Degree", xlab="Notas de Matemáticas", ylab="Frecuencia")
hist(datos5$math, main="Some College", xlab="Notas de Matemáticas", ylab="Frecuencia")
hist(datos6$math, main="Some High School", xlab="Notas de Matemáticas", ylab="Frecuencia")

#Histograma de notas de lectura
layout(matrix(c(1:6), nrow=2, byrow=FALSE))
hist(datos1$reading, main="Associate's Degree", xlab="Notas de Lectura", ylab="Frecuencia")
hist(datos2$reading, main="Bachelor's Degree", xlab="Notas de Lectura", ylab="Frecuencia")
hist(datos3$reading, main="High School", xlab="Notas de Lectura", ylab="Frecuencia")
hist(datos4$reading, main="Master's Degree", xlab="Notas de Lectura", ylab="Frecuencia")
hist(datos5$reading, main="Some College", xlab="Notas de Lectura", ylab="Frecuencia")
hist(datos6$reading, main="Some High School", xlab="Notas de Lectura", ylab="Frecuencia")

#Histograma de notas de escritura
layout(matrix(c(1:6), nrow=2, byrow=FALSE))
hist(datos1$writing, main="Associate's Degree", xlab="Notas de Escritura", ylab="Frecuencia")
hist(datos2$writing, main="Bachelor's Degree", xlab="Notas de Escritura", ylab="Frecuencia")
hist(datos3$writing, main="High School", xlab="Notas de Escritura", ylab="Frecuencia")
hist(datos4$writing, main="Master's Degree", xlab="Notas de Escritura", ylab="Frecuencia")
hist(datos5$writing, main="Some College", xlab="Notas de Escritura", ylab="Frecuencia")
hist(datos6$writing, main="Some High School", xlab="Notas de Escritura", ylab="Frecuencia")

#Si usamos el paquete ggplot2 podemos simplificar las instrucciones 
#y mostrar en un sólo gráfico todos los histogramas de acuerdo al nivel de educación de los padres 
#en 3 gráficos que corresponden a cada asignatura.
hist1=ggplot(data=datos, aes(math)) + geom_histogram(binwidth=5, color="gray", aes(fill=education))+ labs(title="Notas de Matemáticas vs Educación de Padres")+ labs(x="Notas de Matemática", y="Frecuencia") 
hist1

hist2=ggplot(data=datos, aes(reading)) + geom_histogram(binwidth=5, color="gray", aes(fill=education))+ labs(title="Notas de Lectura vs Educación de Padres")+ labs(x="Notas de Lectura", y="Frecuencia") 
hist2

hist3=ggplot(data=datos, aes(writing)) + geom_histogram(binwidth=5, color="gray", aes(fill=education))+ labs(title="Notas de Escritura vs Educación de Padres")+ labs(x="Notas de Escritura", y="Frecuencia") 
hist3

#Diagramas de caja y bigote
box1=ggplot(datos, aes(education, math, color = education)) + geom_boxplot() + labs(title="Notas de Matemáticas vs Educación de Padres")+ labs(x="Educación de Padres", y="Notas de Matemáticas") + stat_summary(fun.y=mean, geom="point", shape=18,size=3, color="red")
box1

box2=ggplot(datos, aes(education, reading, color = education)) + geom_boxplot() + labs(title="Notas de Lectura vs Educación de Padres")+ labs(x="Educación de Padres", y="Notas de Lectura") + stat_summary(fun.y=mean, geom="point", shape=18,size=3, color="green")
box2

box3=ggplot(datos, aes(education, writing, color = education)) + geom_boxplot() + labs(title="Notas de Escritura vs Educación de Padres")+ labs(x="Educación de Padres", y="Notas de Escritura") + stat_summary(fun.y=mean, geom="point", shape=18,size=3, color="blue")
box3

#Matriz de correlación de los datos
corrplot(cor(datos[,6:8]), method="color", addCoef.col = "white", type="upper", diag=FALSE, tl.col="black")

#Prueba gráfica de normalidad
#nos permite identificar si los datos se ajustan a una distribución normal o que tanto se aproximan a esta.
#comandos qqnorm y qqline para extraer las conclusiones. 
layout(matrix(c(1:6), nrow=2, byrow=FALSE))

qqnorm(datos1$math, main= "Math Associate's Degree")
qqline(datos1$math)

qqnorm(datos2$math, main= "Math Bachelor's Degree")
qqline(datos2$math)

qqnorm(datos3$math, main= "Math High School")
qqline(datos3$math)

qqnorm(datos4$math, main= "Math Master's Degree")
qqline(datos4$math)

qqnorm(datos5$math, main= "Math Some College")
qqline(datos5$math)

qqnorm(datos6$math, main= "Math Some High School")
qqline(datos6$math)

#--------------------
layout(matrix(c(1:6), nrow=2, byrow=FALSE))

qqnorm(datos1$reading, main= "Reading Associate's Degree")
qqline(datos1$reading)

qqnorm(datos2$reading, main= "Reading Bachelor's Degree")
qqline(datos2$reading)

qqnorm(datos3$reading, main= "Reading High School")
qqline(datos3$reading)

qqnorm(datos4$reading, main= "Reading Master's Degree")
qqline(datos4$reading)

qqnorm(datos5$reading, main= "Reading Some College")
qqline(datos5$reading)

qqnorm(datos6$reading, main= "Reading Some High School")
qqline(datos6$reading)

#--------------------
layout(matrix(c(1:6), nrow=2, byrow=FALSE))

qqnorm(datos1$writing, main= "Writing Associate's Degree")
qqline(datos1$writing)

qqnorm(datos2$writing, main= "Writing Bachelor's Degree")
qqline(datos2$writing)

qqnorm(datos3$writing, main= "Writing High School")
qqline(datos3$writing)

qqnorm(datos4$writing, main= "Writing Master's Degree")
qqline(datos4$writing)

qqnorm(datos5$writing, main= "Writing Some College")
qqline(datos5$writing)

qqnorm(datos6$writing, main= "Writing Some High School")
qqline(datos6$writing)

#6. Cálculo de probabilidades con lenguaje R
#Simulación de datos que siguen una distribución normal
nx=rnorm(100,25,sqrt(5))
#Generamos 15 números aleatorios que siguen distribución normal N(25,5)

#Prueba gráfica de normalidad
qqnorm(nx, main = "Prueba de normalidad de los valores simulados")
qqline(nx)

hist(nx,freq=F,ylim=c(0.0,0.3), main="Histograma de nuestros datos vs la curva normal")
curve(dnorm(x,mean=25,sd=sqrt(5)),add=T)

plot(density(nx),col="red",ylim=c(0.0,0.3), main="Densidad de nuestros datos vs la curva normal")
curve(dnorm(x,mean=25,sd=sqrt(5)),add=T, col="blue")
abline(v=mean(nx))

#Gráfica de la función de densidad 
curve(dnorm(x,25,sqrt(5)),xlim=c(10,40),ylab="f(x)",xlab="x", main="Curva Normal Generalizada\n media=25 y var=5", col="blue")


#Cálculo de probabilidades de la distribución normal N(25,5)
#Se calculan las probabilidades considerando una distribución normal de media 25 y varianza 5
1-pnorm(12,25,sqrt(5))  # Observación: este valor arroja 1 ya que la probabilidad 12 es tan pequeña que es casi cero
pnorm(32,25,sqrt(5))-pnorm(17,25,sqrt(5))
pnorm(25,25,sqrt(5))

#Se calculan los valores que dan determinada probabilidad en una distribución normal de media 25 y varianza 5

#Valor k tal que a la derecha hay un área de 0.05 a la derecha
qnorm(1-0.05,25,sqrt(5))

#Valor k tal que a la derecha hay un área de 0.025 a la derecha
qnorm(1-0.025,25,sqrt(5))

#Valor k tal que a la derecha hay un área de 0.95 a la derecha
qnorm(1-0.95,25,sqrt(5))

#Cáculo de probabilidades de la distribución binomial B(2000, 4.2%)
#Secuencia de 2000 números aleatorios binomiales con n=2000 y p=0.042#
bin=rbinom(2000,1,0.042)

#Tabla de frecuencias para los artículos con defectos
require("plyr")
bin=as.factor(bin)
bin=revalue(bin, c("0"="No defectuoso", "1"="Defectuoso"))
table(bin)

#Se grafican pies de torta para las tablas obtenidas anteriormente
porcentaje= round(100*(table(bin))/sum(table(bin)),2)
lab=paste(porcentaje,"%")
pie(table(bin), col=rainbow(2), labels=lab, main="Calidad de Artículos", border="white")
legend("topleft", legend=c("No defectuoso", "Defectuoso"), fill=rainbow(2))


#Se grafican diagramas de torta usando el paquete plotrix y el comando pie3D
pie3D(table(bin),explode=0.2, labels=lab)

#Calculo de probabilidades usando la distribución binomial
1-pbinom(4,120,0.042)

#Cálculo usando directamente la distribución binomial
sum(dbinom(7:10,120,0.042))

#Se grafica la función de densidad de la distribución binomial ajustada a los parámetros solicitados 
plot(density(as.numeric(bin)),type="h",xlab="X Artículos",ylab="Artículos con defectos",main="Función de Probabilidad B(2000,0.042)")

#Cálculo de probabilidades de la distribución chi-cuadrado con 23 grados de libertad
#Secuencia de 150 números aleatorios chi cuadrado#
chi <- rchisq(150,23)
chi

#Función de densidad chi cuadrado#
curve(dchisq(x,df = 23),xlim=c(0,100),xlab="U",ylab="f(U)", main="Función de densidad chi-cuadrada\n grados de libertad = 23")

#Cálculo de proababilidad usando la distribución chi cuadrado
#P(U>17.75)
1-pchisq(17.75,23)

#P(31.5<U<22.4)
pchisq(31.5,23)-pchisq(22.4,23)

#Valor k tal que a la derecha hay un área de 0.05 a la derecha
qchisq(1-0.05,23)

#Valor k tal que a la derecha hay un área de 0.99 a la derecha
qchisq(1-0.99,23)

#Cálculo de probabilidades de la distribución t-student con 37 grados de libertad
#Secuencia de números aleatorios t-student#
t <- rt(150,37)
t

#Función de densidad t-student#
curve(dt(x,df = 37),xlim=c(-5,5),xlab="T",ylab="f(T)", main="Función de densidad T-Student\n grados de libertad = 37")

#Cálculo de probabilidades usando la distribución t-student
#P(T>17.75)
1-pt(1.43,37)

#P(-0.57<T<0.52)
pt(0.52,37)-pt(-0.57,37)

#Valor k tal que a la derecha hay un área de 0.025 a la derecha
qt(1-0.025,37)

#Valor k tal que a la derecha hay un área de 0.095 a la derecha
qt(1-0.095,37)
