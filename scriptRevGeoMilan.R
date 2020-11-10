#Formateo de datos inspirado en el Notebook Mobile Phone activity - exploratory analysis
#Librerías y directorio de datos
library(data.table) #Se usa para juntar los datos
library(rgdal)    #Para leer el GeoJson
library(ggplot2)  #Plots
library(magrittr) 
library(dplyr)    #Magrittr y Dplyr se usan para trabajar con tuberías
library(broom)    #Formateo de datos
library(dbscan)   #Clusterizado
library(splitstackshape)
library(nominatim)
library(stringr)
library(lubridate)
library(anytime)  #Estas son para trabajar con fechas
require(maps)
require(viridis)
#Ruta, cambiar al directorio donde se guarden los archivos
setwd('C:\\Users\\GUILLERMO\\Desktop\\ProgramasVarios\\Python\\TFG\\archivos')
#Lectura de los archivos
#El GeoJson
archivoGeoJson <- readOGR(dsn="milano-grid.geojson", layer="milano-grid")
archivoGeoJson<-tidy(archivoGeoJson)
archivoGeoJson<-as.data.frame(archivoGeoJson)
print(head(archivoGeoJson))
#Listamos, leemos los archivos (.. es cualquier carácter) 
#Y los guardamos en auxiliarInternet, ahí los pasamos a un dataframe
#Se usa rbindlist porque es mucho más rápido y eficiente que docall
archivos <- list.files(pattern="sms-call-internet-mi-2013-11-0..txt")
auxiliarInternet <- list()
for (k in 1:length(archivos)){
  auxiliarInternet[[k]] <- setDT(read.table(archivos[k],sep = "\t"))
}
dataframeCDRs<-data.frame(rbindlist(auxiliarInternet, fill=TRUE, idcol=NULL),row.names=NULL)
#Como en el txt no vienen los nombres de las columnas, se los ponemos nosotros
#Según la documentación del conjunto
names(dataframeCDRs) <- c("CellID","datetimeAux","countrycode","smsin","smsout","callin","callout","internet")
#Es necesario pasar de Epoch a Central European Time y dropear los NAs para trabajar
dataframeCDRs$datetimeAux<-dataframeCDRs$datetimeAux/1000
dataframeCDRs$datetimeAux<-anytime(dataframeCDRs$datetimeAux, tz="CET")
dataframeCDRs[is.na(dataframeCDRs)] <- 0
#Agrupamos por horas y celdas como paso previo al agrupamiento por medias
dataframeCDRs$datetime <- droplevels(cut(dataframeCDRs$datetimeAux, breaks="hour"))
print(head(dataframeCDRs))
dataframeCDRs <- aggregate(cbind(callin, callout, smsin, smsout, internet) ~ datetime + CellID, data=dataframeCDRs, FUN=sum)
print(head(dataframeCDRs))
#Juntamos las columnas de sms y llamadas
dataframeCDRs$sms<-dataframeCDRs$smsin+dataframeCDRs$smsout
dataframeCDRs$calls<-dataframeCDRs$callin+dataframeCDRs$callout
dataframeCDRs$smsin <- NULL
dataframeCDRs$smsout <- NULL
dataframeCDRs$callin <- NULL
dataframeCDRs$callout <- NULL
#Devolvemos un tibble agrupado por fecha e Id, sumamos el resto de columnas
#Esta notación significa el uso de tuberías mediante dyplr y magritte
#Añadimos una columa de horas
hours <- as.numeric(format(as.POSIXct(strptime(dataframeCDRs$datetime,format="%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H"))+24*(as.numeric(format(as.POSIXct(strptime(dataframeCDRs$datetime,format="%Y-%m-%d %H:%M:%S",tz="")) ,format = "%d"))-1)
dataframeCDRs$hour<-hours
print(head(dataframeCDRs))
print(tail(dataframeCDRs))
#A partir de aquí nuestro análisis difiere, pero usaremos este conjunto de datos como base
#Internet (Agrupamos por medias)
dataframeInternetCallsSMS<-select(dataframeCDRs, CellID, internet,calls,sms)
summary2<-dataframeInternetCallsSMS %>%
  group_by(CellID) %>%
  summarise(internet=mean(internet),calls=mean(calls),sms=mean(sms))
dataframeInternetCallsSMS<-as.data.frame(summary2)
#Para trabajar con la información Geoespacial
#Necesitaremos primero castear la columna id a numeric
#y llevar internet,sms y llamadas al GeoJson
archivoGeoJson$id <- as.numeric(archivoGeoJson$id)
names(archivoGeoJson)[names(archivoGeoJson) == "id"] <- "CellID"
archivoGeoJson <- merge(x=archivoGeoJson,y=dataframeInternetCallsSMS,by="CellID",all.x=TRUE)
archivoGeoJson[is.na(archivoGeoJson)] <- 0
archivoGeoJson
long<-archivoGeoJson[which.max(archivoGeoJson$internet),2]
lat<-archivoGeoJson[which.max(archivoGeoJson$internet),3]
internet<-archivoGeoJson[which.max(archivoGeoJson$internet),8]
print(long)
print(lat)
print(internet)
emb_coded_coords <- reverse_geocode_coords(lat, long,key='sYiO2A8cc6pAhsdykyERWWjzkc0NsGUu')

emb_coded_coords %>% select(display_name)