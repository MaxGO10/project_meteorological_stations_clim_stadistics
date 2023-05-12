############----------- Manejo basico de datos


### Establecemos nuestro directorio de trabajo
path <- "C:/codes_max/estadistics_R/homogen"
setwd(path)

### Comprobamos nuestro directorio
getwd()

### Leemos los datos de nuestra estaci?n
data <-read.table("ho00000776.txt",na.strings = -99.9)

#Visualizamos lo primeros 6 datos
head(data,10)

#Establecemos los nombres de columnas   
colnames(data) <- c("Año","Mes","D?a","Pp","Tmax","Tmin")

#Resumen estadistico
summary(data)

#Creamos vector de fecha
data$Fecha <- as.Date(paste0(data$A?o,"-",data$Mes,"-",data$D?a))

#Tipo de objeto
class(data$Fecha)

head(data)

#Verificamos nuestros datos
summary(data)

#Cantidad de NAs de cada columna
sum(is.na(data$Pp))
sum(is.na(data$Tmax))
sum(is.na(data$Tmin))

####M?todos de ploteo

###1) Grafico con R base
plot(data$Fecha,data$Pp,type="p",pch=16,col="green",main="Precipitacion Estacion .............",xlab="Fecha",ylab="Precipitaci?n")
plot(data$Fecha,data$Tmax,type="l",col="red",main="Temperatura maxima Estacion .............",xlab="Fecha",ylab="Temperatura")
plot(data$Fecha,data$Tmin,type="l",col="blue",main="Temperatura minima Estacion .............",xlab="Fecha",ylab="Temperatura")

###2) Grafico con Ggplot2
#install.packages("ggplot2")
library(ggplot2)
ggplot(data,aes(x=Fecha,y=Pp))+
  geom_point(col="blue")+ggtitle("Precipitaci?n Estaci?n ........")

g1 <- ggplot(data,aes(x=Fecha,y=Pp))+
  geom_point(col="green")
g2 <- ggplot(data,aes(x=Fecha,y=Tmax))+
  geom_line(col="red")
g3 <- ggplot(data,aes(x=Fecha,y=Tmin))+
  geom_line(col="blue")
#Gr?ficos interactivos
#install.packages("plotly")
library(plotly)
ggplotly(g1)
ggplotly(g2)
ggplotly(g3)

###########C?lculo de medias de temperatura y acumulados de pp mensuales

#Funci?n para aplicar suma a un objeto, si todo es NA, que todo de NA
sum_with_na <- function(x) {if (all(is.na(x)) == TRUE) {NA} else {sum(x, na.rm = TRUE)}}

sum(c(1,2,45,NA), na.rm = T)
sum(c(NA,NA,NA),na.rm = T)

#M?todo 1 bucles

pm=NULL

for (j in 1965:2019){
  for (i in 1:12){
    Tmin <- mean(data$Tmin[data$Año == j & data$Mes ==i],na.rm=T)
    Tmax <- mean(data$Tmax[data$Año == j & data$Mes ==i],na.rm=T)
    Pp <- sum_with_na(data$Pp[data$Año == j & data$Mes ==i])
    df <- data.frame(Tmin,Tmax,Pp)
    pm=rbind(pm,df)
  }
}

#Vector de fechas mensuales
Fecha <- seq.Date(as.Date("1965-01-01"),as.Date("2019-12-01"),by="month")

head(pm,15)
pm <- data.frame(pm,Fecha)
head(pm,10)

install.packages("xlsx")
library(xlsx)
#Exportar dataframe
write.xlsx(pm,"PP_mensual.xlsx")

#M?todo 2 librer?a dplyr

install.packages("tidyverse")
library(tidyverse)

prom_m <- data %>% group_by(A?o,Mes) %>%
  summarise(tmax_m = mean(Tmax, na.rm = T), tmin_m = mean(Tmin, na.rm = T),pp_s=sum_with_na(Pp))

Fecha <- seq.Date(as.Date("1965-01-01"),as.Date("2019-12-01"),by="month")
prom_m <- data.frame(prom_m,Fecha)
head(prom_m,15) 






############################
########################## Homogeneizaci?n (climatol)

install.packages("climatol")
library(climatol)

### 1) generar archivos de entrada
install.packages("readxl")
library(readxl)
metad <- read_excel("Estaciones_Meteorológicas_Peru.xlsx",col_names = F)
metad
View(metad)

#Nombres de columnas
colnames(metad) <- c("code","lat_grad","lat_min","lat_seg","lon_grad","lon_min","lon_seg","alt","nombre")
head(metad)

#Vector con nombres de archivos de estaciones
station= list.files(pattern = ".*.csv")[2:11]
station

est <- NULL
for (i in 1:length(station)){
  est <- rbind(est,metad[metad$code == substr(station[i],1,10),])
}
est
i=1
install.packages("GeodesiCL")
library(GeodesiCL)

?sexagesimal
#Convertimos a grados decimales
lat <- sexagesimal(est$lat_grad,est$lat_min,est$lat_seg, digits = 4)
lat
lon <- sexagesimal(est$lon_grad,est$lon_min,est$lon_seg, digits = 4)
lon

# Nombre de archivos

Archivo <- station

#Ordenar fichero al formato de climatol
Est <- data.frame(Archivo,lon,lat,alt=est$alt,code=est$code,nombre=est$nombre)

#Exportando el fichero
#write.csv(Est, 'Est.csv', row.names=FALSE, col.names=FALSE)
write.csv(Est,'Est.csv',row.names=FALSE,quote=FALSE)
#Modificando los datos de estaciones al formato de climatol

#En datcol solo modificar el 4 por 5 o 6 si es que la variable es tmax o tmin

daily2climatol(stfile = "Est.csv",stcol=1:6,datcol = c(1:3,4),varcli= "pre",anyi = 1980,anyf=2019,dec=".",header=F,na.strings = -99.9)

### AED
homogen("pre",anyi= 1980, anyf=1995,expl= T)

#3) Convertir datos diarios a datos mensuales
# Variabilidad no muy bien observada en series diarias
#1: suma
#2: media
dd2m("pre",anyi= 1980, anyf=1995,valm= 1)

#AED series mensuales
homogen("pre-m",anyi= 1980, anyf=1995,expl=T)

#Homogenizando serie de tiempo anual
homogen("pre-m",1980,1995,dz.min=-5,dz.max=6,snht1=9,snht2 =15,std=2,vmin = 0 )

#Homogenizando serie de tiempo diaria
homogen("pre",1980,1995,dz.min=-7,dz.max=13,vmin = 0,metad=T)

#Cargando resultados
load("pre_1980-1995.rda")

#Generando serie de tiempo homogenizada
dahstat("pre",1980,1995,stat="series")

#Exportando serie
homogen <- read.table("pre_1980-1995_series.csv",sep=",",header = T)
homogen

#pod: porcentaje de datos originales
#ios: numero de estacion
#ope: 0 : finaliza con un dato calculado
#     1 : 
#snht: prueba de homogeneidad
#rmse: error cuadratico medio

###Para evaluar:
# %datos originales,menor RMSE, menor SNHT

View(est.c)



