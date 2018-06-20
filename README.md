---
title: "Tarea HE Big Data-1"
author: "David S�nchez y Adri�n D�az"
date: "3 de enero de 2018"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#1.	C�LCULO DE NUEVAS VARIABLES, RECODIFICACI�N Y FILTRADO

####1.1.	Importar el fichero JaenIndicadores.txt y denominar a la hoja de datos (data frame) Datos.Jaen
  
```{r warning=FALSE}
Datos.Jaen <- read.delim2("E:/datos david.sanchez/Dropbox/00 PERSONAL/56 MASTER BIG DATA/01 ASIGNATURAS/03 Herramientas Estadisticas Para/Tarea sesion parte 1/JaenIndicadores.txt", stringsAsFactors=FALSE)
as.numeric(Datos.Jaen$Poblaci�n)  

Datos.Jaen$Poblaci�n<-as.integer(Datos.Jaen$Poblaci�n)
Datos.Jaen$Consumo.de.energ�a.el�ctrica<-as.integer(Datos.Jaen$Consumo.de.energ�a.el�ctrica)
Datos.Jaen$Consumo.de.agua..Invierno<-as.integer(Datos.Jaen$Consumo.de.agua..Invierno)
Datos.Jaen$Consumo.de.agua..Verano<-as.integer(Datos.Jaen$Consumo.de.agua..Verano)

Datos.Jaen$Residuos.s�lidos.urbanos..Cantidad<-gsub(",",".",Datos.Jaen$Residuos.s�lidos.urbanos..Cantidad)
Datos.Jaen$Residuos.s�lidos.urbanos..Cantidad<-as.numeric(Datos.Jaen$Residuos.s�lidos.urbanos..Cantidad)

summary(Datos.Jaen)
head(Datos.Jaen)
```

####1.2.	Recodificar la variable Poblacion en una variable cualitativa tipo factor llamada Tama�o con tres categor�as:
Si la poblaci�n es inferior a 2000 habitantes, Tama�o ser� "Peque�o".  
Si la poblaci�n est� entre 2000 y 4500 habitantes, Tama�o ser� "Mediano".  
Si la poblaci�n es superior a 4500 habitantes, Tama�o ser� "Grande".  


```{r}
Datos.Jaen$Tama�o[ Datos.Jaen$Poblaci�n < 2000 ] <- "Peque�o"
Datos.Jaen$Tama�o[ Datos.Jaen$Poblaci�n > 2000 & Datos.Jaen$Poblaci�n <= 4500 ] <- "Mediano"
Datos.Jaen$Tama�o[ Datos.Jaen$Poblaci�n > 4500 ] <- "Grande"

Datos.Jaen$Tama�o<-as.factor(Datos.Jaen$Tama�o)
```

####1.3.	Calcular los siguientes promedios que se especifican a continuaci�n y a�adirlos como nuevas variables al fichero Datos.Jaen obtenidas a partir de las variables existentes: 
Variable elec.hab que contendr� el consumo de energ�a el�ctrica por habitante, obtenida como Consumo.de.energia.electrica/Poblacion  
  
Variable agua.hab que contendr� el consumo medio de agua por habitante y d�a, obtenida como (Consumo.de.agua..Invierno + Consumo.de.agua..V erano)/Poblacion  
  
Variable res.hab que contendr� los residuos s�lidos urbanos por habitante, obtenida como Residuos.solidos.urbanos..Cantidad/Poblacion  

```{r}
Datos.Jaen$elec.hab<-(Datos.Jaen$Consumo.de.energ�a.el�ctrica/Datos.Jaen$Poblaci�n)
Datos.Jaen$agua.hab<-((Datos.Jaen$Consumo.de.agua..Invierno + Datos.Jaen$Consumo.de.agua..Verano)/
                        Datos.Jaen$Poblaci�n)
Datos.Jaen$res.hab<-(Datos.Jaen$Residuos.s�lidos.urbanos..Cantidad/Datos.Jaen$Poblaci�n)
```

####1.4.	Crear una nueva hoja de datos con todas las variables que contiene actualmente el data frame Datos.Jaen, pero referida s�lo a los municipios de tama�o mediano y denominarla Datos.Jaen.Medianos
  
```{r}
Datos.Jaen.Medianos<-subset(Datos.Jaen, (Tama�o=="Mediano"))
```

####1.5.	Guardar la hoja de datos Datos.Jaen con las nuevas variables creadas en los apartados anteriores y la hoja que contiene los datos de las poblaciones medianas (Datos.Jaen.Medianos) en un archivo de datos de R y llamadlo JaenIndicadores.RData
  
```{r}
save.image("E:/datos david.sanchez/Dropbox/00 PERSONAL/56 MASTER BIG DATA/01 ASIGNATURAS/03 Herramientas Estadisticas Para/Tarea sesion parte 1/JaenIndicadores.RData")
```
  
***

#2.	AN�LISIS ESTAD�STICO DESCRIPTIVO DE DATOS
   
####Instalar el paquete Hmisc si es preciso
```{r}
if(!is.element('e1071', installed.packages())) install.packages('e1071', repos = 'https://cran.rediris.es/', dependencies = T)
```
####Cargar paquete
```{r}
library(e1071)  
```

####2.1.	Importar el fichero Andalucia.txt y denominar a la hoja de datos (data frame) Datos.Andalucia. Comprobar si en el archivo .txt hay datos faltantes y c�mo est�n codificados.
```{r warning=FALSE}
Datos.Andalucia <- read.delim2("E:/datos david.sanchez/Dropbox/00 PERSONAL/56 MASTER BIG DATA/01 ASIGNATURAS/03 Herramientas Estadisticas Para/Tarea sesion parte 1/Andalucia.txt", stringsAsFactors=FALSE)

Datos.Andalucia$Poblacion.2001 <- as.integer(Datos.Andalucia$Poblacion.2001)

summary(Datos.Andalucia)
head(Datos.Andalucia)
```
  
####2.2.1.	A partir de la variable c�digo INE, construir una variable tipo factor que distinga la provincia de pertenencia de cada municipio, denominarla "Provincia" y a�adirla al data frame. 
```{r warning=FALSE}
Datos.Andalucia$Provincia [Datos.Andalucia$Codigo.INE<= 4999 ] <- "Almeria"
Datos.Andalucia$Provincia [Datos.Andalucia$Codigo.INE >= 11000 & Datos.Andalucia$Codigo.INE <= 11999  ] <-"C�diz"
Datos.Andalucia$Provincia [Datos.Andalucia$Codigo.INE >= 14000 & Datos.Andalucia$Codigo.INE <= 14999  ] <-"C�rdoba"
Datos.Andalucia$Provincia [Datos.Andalucia$Codigo.INE >= 18000 & Datos.Andalucia$Codigo.INE <= 18999  ] <-"Granada"
Datos.Andalucia$Provincia [Datos.Andalucia$Codigo.INE >= 21000 & Datos.Andalucia$Codigo.INE <= 21999  ] <-"Huelva"
Datos.Andalucia$Provincia [Datos.Andalucia$Codigo.INE >= 23000 & Datos.Andalucia$Codigo.INE <= 23999  ] <-"Ja�n"
Datos.Andalucia$Provincia [Datos.Andalucia$Codigo.INE >= 29000 & Datos.Andalucia$Codigo.INE <= 29999  ] <-"M�laga"
Datos.Andalucia$Provincia [Datos.Andalucia$Codigo.INE >= 41000 & Datos.Andalucia$Codigo.INE <= 41999  ] <-"Sevilla"

Datos.Andalucia$Provincia <-as.factor(Datos.Andalucia$Provincia)

View(Datos.Andalucia)
summary(Datos.Andalucia$Provincia)

names(Datos.Andalucia)
str(Datos.Andalucia)

str(Datos.Andalucia$Provincia)

Datos.Andalucia$Provincia
which.max(Datos.Andalucia$Provincia)

which.min(Datos.Andalucia$Provincia)
```

####2.2.2 Obtener la distribuci�n de frecuencias absolutas y relativas.
```{r warning=FALSE}
Frec.abosolutas <- table(Datos.Andalucia$Provincia)
Frec.abosolutas

Frec.rel <- prop.table(Frec.abosolutas)
Frec.rel

Frec.rel <- round(Frec.rel*100, 2)
Frec.rel
```

####2.2.3 Un diagrama de barras con las frecuencias absolutas
```{r}
barras.frec.abosolutas <-barplot(Frec.abosolutas, col = rainbow(8), xlab="Povincias Andalucia", ylab = "N� de Pobalciones")
text(barras.frec.abosolutas,Frec.abosolutas + 1,labels=Frec.abosolutas, xpd = TRUE)
title(main = "Numero de poblaciones por provincias")
```

####2.2.4. Un diagrama de sectores con las frecuencias relativas en porcentajes de esta variable tipo factor.
```{r}
sectores.frec.rel <- pie(Frec.rel,labels = names(Frec.rel),main = "Distribuci�n de porcenjates de la variable Provincia")
sectores.frec.rel.por <- pie(Frec.rel,labels = paste(names(Frec.rel),Frec.rel,"%"),main = "Distribuci�n de porcenjates de la variable Provincia")
```
  
**�Qu� provincia tiene m�s municipios?**   
Granada con 168 municipios
  
**�Cual tiene menos?**  
Cadiz con 44 municipios

**�Qu� porcentaje representa en cada caso?**  
Grandda un 21.82% y C�diz un 5.71%


####2.3.	Obtener un resumen descriptivo de la variable tasa de actividad de 2001 que incluya:

###Par�metros de posici�n.
```{r}
summary(Datos.Andalucia$Tasa.actividad.2001)
fivenum(Datos.Andalucia$Tasa.actividad.2001)

```

####Dispersi�n
```{r}
plot (Datos.Andalucia$Provincia, Datos.Andalucia$Tasa.actividad.2001, type = "p", col = "red")
```

####Asimetr�a 
```{r}
skewness(Datos.Andalucia$Tasa.actividad.2001,na.rm = TRUE)
```

####Curtosis 
```{r}
kurtosis(Datos.Andalucia$Tasa.actividad.2001,na.rm = TRUE)
```

####Histograma 
```{r}
hist(Datos.Andalucia$Tasa.actividad.2001, breaks = 10, freq = TRUE, main = "Historigrama de la Tasa de actividad del 200,",xlab = "Tasa 2001",ylab = "Frecuencias", col = "lightblue", border = "blue")
```

####Diagrama de caja. 
```{r}
boxplot(Datos.Andalucia$Tasa.actividad.2001, main="Diagrama de caja para la tasa de actividad del 2001",ylab="Tasa 2001", col="lightblue")
```

####En funci�n de este resumen, contestar a las siguientes preguntas:

####2.3.1.	�Cu�l es la tasa media de actividad de los municipios andaluces? 
51.44
  
####�Crees que este valor es adecuado para representar la Tasa de Actividad de los municipios andaluces durante 2001?
Si

####2.3.2.	�C�mo valoras la homogeneidad de los valores de la tasa de actividad en los municipios andaluces? �Qu� par�metro elegir�as para representar la dispersi�n de la Tasa de Actividad de 2001?
La varianza y la desviaci�n t�pica.

####2.3.3.	�En ese sentido, qu� municipios andaluces destacan significativamente del resto (como at�picos) por su alta tasa de actividad y por su baja tasa de actividad? 
Sevilla por alta y Almeria por baja.

####�Se te ocurre alguna explicaci�n al respecto?

####2.3.4.	�C�mo valoras la simetr�a de la distribuci�n de frecuencias?
 
*** 
  
#3.	DISTRIBUCIONES DE PROBABILIDAD

####3.1.	Consideremos una variable aleatoria que sigue una distribuci�n B (15, 0.33). Se pide:

####3.1.1.	�Qu� valor de la variable deja por debajo de s� el 75% de la probabilidad?
  
```{r}
qbinom(0.75, 15, 0.33)
```
  
####3.1.2.	Calcular el percentil 95% de la distribuci�n.
  
```{r}
qbinom(0.95, 15, 0.33)
```
  
####3.1.3.	Obtener una muestra de tama�o 1000 de esta distribuci�n, 
  
```{r}
rbinom (1000,15,0.33)
```
  
####Representarla gr�ficamente las frecuencias observadas de cada valor de la distribuci�n mediante un diagrama de barras

####Comparar �ste con las frecuencias esperadas seg�n el modelo que genera los datos.
  
```{r}
tabla.binom<-c(rbinom (1000,15,0.33))


hist(tabla.binom, xlab = "Valores", ylab = "Fecuencias")
```

####Comparar �ste con las frecuencias esperadas seg�n el modelo que genera los datos.
  
```{r}
barplot(dbinom(0:15,15,0.33))
```

####3.2.	Consideremos una variable aleatoria W con distribuci�n N (250, 13). Se pide:
  
####3.2.1.	P [240 < W ??? 245.5]

```{r}

pnorm(c(245.5),mean = 250,sd = 13)-pnorm(c(240),mean = 250,sd = 13)
```
  
####3.2.2.	P [W ??? 256].

```{r}
pnorm(256,13,250, lower.tail = F)
```
  
####3.2.3.	Si queremos desechar el 5% de valores m�s altos de la distribuci�n y el 5% de valores m�s bajos, �con qu� intervalo de valores nos quedaremos?

```{r}
w1 <- qnorm(((1-0.95)/2), 250, 13)
w1

w2 <- qnorm(((1-0.95)/2), 250, 13, lower.tail = F)
w2
```
  
####3.2.4.	Obtener una muestra de tama�o 1000 de la distribuci�n, representar la funci�n de densidad de esta distribuci�n y compararla con el histograma de la muestra obtenida.
  
```{r}
n<-1000
m<-250
sigma<-13

muestra <- rnorm(n, m, sigma)
hist(muestra)
mean(muestra)
sd(muestra)

int<-round(sqrt(n), 0)

hist(muestra, breaks=int, freq=F, xlab="muestra", ylab="Densidad", main="Histograma",
     col="lightblue", border="blue")
lines(sort(muestra), dnorm(sort(muestra),m,sigma), type="l", col="red")
```
  
***
  
#4.	CONTRASTES DE HIP�TESIS E INTERVALOS DE CONFIANZA

####DESCRIPCI�N DEL DATASET
####Mediante una red de sensores se han recogido datos sobre la temperatura media diaria (�C) en dos estaciones A y B durante 52 d�as. Los valores recogidos de la temperatura se encuentran en la hoja de datos "Temper" incluida en el fichero Temperatura.RData.

####4.1.	Cargar el fichero Temperatura.RData. 
```{r}
load("Temperatura.RData")
```
  
####4.2.	Crear dos nuevas variables, temp.A y temp.B, que contengan las temperaturas de las estaciones A y B, respectivamente.
  
```{r}
table.A <- subset(Temper, (Estacion=="A"))
temp.A <- table.A$Temper
temp.A


table.B <- subset(Temper, (Estacion=="B"))
temp.B <- table.B$Temper
temp.B
```

####4.3.	Da un intervalo de confianza para la temperatura media diaria de la estaci�n A, al 95%,  y a partir de �ste indica si se puede admitir, y por qu�, que la temperatura media diaria en dicha estaci�n sea de 19�C, con ese mismo nivel de confianza.

```{r}
mean(temp.A)

test.tempo.A <- t.test(temp.A, alternative = "two.sided", conf.level = 0.95)
test.tempo.A
```

####4.4.	Plantea un test de hip�tesis que refleje la pregunta del apartado anterior y resu�lvelo sin usar el intervalo de confianza (riesgo de 1� especie 5%)
  
```{r}
test.tempo.A.hipo <- t.test(temp.A, alternative = "two.sided", mu = 19, conf.level = 0.95)
test.tempo.A.hipo
```

####4.5.	Determina si puede admitirse, con un riesgo de primera especie de 1%, que la temperatura media diaria es la misma en las dos estaciones. Plantea previamente el correspondiente contraste de hip�tesis.

```{r}
mean(temp.A)
mean(temp.B)

var.test.AB <- var.test(temp.A, temp.B, ratio = 1, alternative = "two.sided", conf.level = 0.99)
var.test.AB
```
El p-valor es 0.6825, y es mayor que 0,1, por lo que podemos afirmar que la varianza de las temperaturas entre las estaciones no difiere, con un riesgo de 1� especie del 1%.


####4.6.	Obt�n un intervalo de confianza (99%) para la diferencia de temperaturas entre estaciones. �Aporta alguna informaci�n adicional al resultado obtenido en el apartado anterior?
  
```{r}  
mean.test.AB <- t.test(temp.A, temp.B, alternative = "two.sided", mu = 0, paired = F, var.equal = T, conf.level = 0.99)
mean.test.AB
```

El p-valor es 0.5224, y es mayor que 0,1, por lo que podemos afirmar que la temperatura medida de la primera estaci�n no difiere de la segunda, con un riesgo de 1� especie del 1%.
  

####4.7.	Se sabe que a lo largo de los 52 d�as, la estaci�n A fall� 5 d�as y la B 7 d�as. �Puede afirmarse con un nivel de confianza del 90% que la proporci�n de d�as fallados es la misma en las dos estaciones? 

```{r}
dias <- 52

fallo.A <- 5
fallo.B <- 7

no.fallo.A <- dias-fallo.A
no.fallo.B <- dias-fallo.B

tabla.fallos <- matrix(c(fallo.A, fallo.B, no.fallo.A, no.fallo.B), 2, 2)
tabla.fallos

prop.test(tabla.fallos, alternative = "two.sided", conf.level = 0.9)
```

Como p-valor es 0.7589, y es mayor que 0,1, podemos afirmar que la proporci�n d�as fallados es la misma en las dos estaciones, con un riesgo de 1� especie del 10%.
