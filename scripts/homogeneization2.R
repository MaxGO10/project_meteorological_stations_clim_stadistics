

### Establecemos nuestro directorio de trabajo
path <- "C:/codes_max/estadistics_R/homogen"
setwd(path)

#install.packages("climatol")
library(climatol)

### 1) generar archivos de entrada
#install.packages("readxl")
library(readxl)
metad <- read_excel("Estaciones_Meteorologicas_Peru.xlsx",col_names = F)
metad


#Nombres de columnas
colnames(metad) <- c("code","lat_grad","lat_min","lat_seg","lon_grad","lon_min","lon_seg","alt","nombre")
head(metad)

#Vector con nombres de archivos de estaciones
station= list.files(pattern = ".*.txt")

est <- NULL
for (i in 1:length(station)){
  est <- rbind(est,metad[metad$code == substr(station[i],1,10),])
}
est

#install.packages("GeodesiCL")
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
write.table(Est, 'Est.txt', row.names=FALSE, col.names=FALSE)

############################################

ds <- c(2,3,3)
n_col <- c(4,5,6)
inicio <- 1965
final <- 2019
var <- c("pre","tmax","tmin")
valm <- c(1,2,2)
select <- list()
serie <- list()


for (i in 1:3){
  
  #Modificando los datos de estaciones al formato de climatol
  #En datcol solo modificar el 4 por 5 o 6 si es que la variable es tmax o tmin
  daily2climatol(stfile = "Est.csv",stcol=1:6,datcol = c(1:3,n_col[i]),varcli= var[i],anyi = inicio,anyf=final,dec=".",header=F,na.strings = -99.9)
  
  ### AED
  homogen(var[i],anyi= inicio, anyf=final,expl= T)
  
  #3) Convertir datos diarios a datos mensuales
  # Variabilidad no muy bien observada en series diarias
  #1: suma
  #2: media
  dd2m(var[i],anyi= inicio, anyf=final,valm= valm[i])
  
  #AED series mensuales
  homogen(paste0(var[i],"-m"),anyi= inicio,anyf=final,expl=T)
  
  #std=2 pp
  #std=3 tmax,tmin
  #Homogenizando serie de tiempo mensual
  
  if (i == 1){
    homogen(paste0(var[i],"-m"),inicio,final,std=ds[i],vmin = 0)
    
    #Homogenizando serie de tiempo diaria
    homogen(var[i],inicio,final,vmin = 0,metad=T)
  }else if (i == 2| i ==3){
    homogen(paste0(var[i],"-m"),inicio,final,std=ds[i])
    
    #Homogenizando serie de tiempo diaria
    homogen(var[i],inicio,final,metad=T)
  }

  #Cargando resultados
  load(paste0(var[i],"_",inicio,"-",final,".rda"))
  
  #Generando serie de tiempo homogenizada
  dahstat(var[i],inicio,final,stat="series")
  
  #Exportando serie
  serie[[i]] <- read.table(paste0(var[i],"_",inicio,"-",final,"_series.csv"),sep=",",header = T)
  select[[i]] <- est.c
}
### +%datos originales,menor RMSE, menor SNHT

select[[1]]  #PP
select[[2]]  #Tmax
select[[3]]  #Tmin

#####Exportar elfichero de % datos originales, RMSE, SNHT
write.table(select[[1]],"Pre_sel.txt")
write.table(select[[2]],"Tmax_sel.txt")
write.table(select[[3]],"Tmin_sel.txt")


paste0(var[1],"_",inicio,"-",final,"_series.csv")



