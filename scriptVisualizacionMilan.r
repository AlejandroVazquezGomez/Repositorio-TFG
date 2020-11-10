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
print(dataframeInternetCallsSMS)
print(tail(dataframeInternetCallsSMS))
dataframeInternet<-select(dataframeInternetCallsSMS, CellID, internet)
dataframeCalls<-select(dataframeInternetCallsSMS, CellID, calls)
dataframeSMS<-select(dataframeInternetCallsSMS, CellID, sms)
#Primeros plot preliminares
#Se ha descubierto que los datos parecen seguir una distribución de campana
#Como veremos, es porque las celdas se numeran de abajo a arriba izda a dcha
#Y conforme llegas al centro aumenta
#Estos plots ayudan tmb a determinar la escala para el mapa de calor
plot(dataframeInternet,xlab="Cell ID",
     ylab="Internet")
plot(dataframeCalls,xlab="Cell ID",
     ylab="Calls")
plot(dataframeSMS,xlab="Cell ID",
     ylab="SMS")
#Si fuéramos a determinar número de clusters
#d_clust <- Mclust(as.matrix(dataframeInternetCallsSMS), G=1:20)
#m.best <- dim(d_clust$z)[2]
#cat("model-based optimal number of clusters:", m.best, "\n")
#4 clusters
#plot(d_clust)

#wss <- (nrow(dataframeInternetCallsSMS)-1)*sum(apply(dataframeInternetCallsSMS,2,var))
#for (i in 2:15) wss[i] <- sum(kmeans(dataframeInternetCallsSMS,
#centers=i)$withinss)
#plot(1:15, wss, type="b", xlab="Number of Clusters",
#ylab="Within groups sum of squares")

#Para trabajar con la información Geoespacial
#Necesitaremos primero castear la columna id a numeric
#y llevar internet,sms y llamadas al GeoJson
archivoGeoJson$id <- as.numeric(archivoGeoJson$id)
names(archivoGeoJson)[names(archivoGeoJson) == "id"] <- "CellID"
archivoGeoJson <- merge(x=archivoGeoJson,y=dataframeInternetCallsSMS,by="CellID",all.x=TRUE)
archivoGeoJson[is.na(archivoGeoJson)] <- 0
archivoGeoJson
#Plots
ggplot(archivoGeoJson, aes(long, lat, group = group))+
  geom_polygon(aes(fill = internet), color = "white")+
  scale_fill_continuous(low='white', 'high'='red',limits=c(0,5000))
theme_classic()
#Hay que cambiar la escala
ggplot(archivoGeoJson, aes(long, lat, group = group))+
  geom_polygon(aes(fill = calls), color = "white")+
  scale_fill_continuous(low='white', 'high'='red',limits=c(0,400))
theme_classic()

ggplot(archivoGeoJson, aes(long, lat, group = group))+
  geom_polygon(aes(fill = sms), color = "white")+
  scale_fill_continuous(low='white', 'high'='red',limits=c(0,600))
theme_classic()
