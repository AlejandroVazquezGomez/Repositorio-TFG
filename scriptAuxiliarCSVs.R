#Formateo de datos inspirado en el Notebook Mobile Phone activity - exploratory analysis
#Librerías y directorio de datos
library(rgdal)
library(broom) 
library(ggplot2)
#Ruta, cambiar al directorio donde se guarden los archivos
setwd('C:\\Users\\GUILLERMO\\Desktop\\ProgramasVarios\\Python\\TFG\\archivos')
#Lectura de los archivos
#El GeoJson
archivoGeoJson <- readOGR(dsn="milano-grid.geojson", layer="milano-grid")
archivoGeoJson<-tidy(archivoGeoJson)
archivoGeoJson<-as.data.frame(archivoGeoJson)
names(archivoGeoJson) <- c("long","lat","order","hole","piece","group","CellID")
write.csv(archivoGeoJson,'csvmilano.csv')