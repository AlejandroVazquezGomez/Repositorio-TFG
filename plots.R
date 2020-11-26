#Plots
library(ggplot2)  
require(maps)
require(viridis)
library(dplyr)
#Ruta, cambiar al directorio donde se guarden los archivos
setwd('C:\\Users\\GUILLERMO\\Desktop\\ProgramasVarios\\Python\\TFG\\archivos')
#Lectura de los archivos

archivos <- list.files(pattern="export.csv")
comprobacion<-as.data.frame(read.csv("exportComprobacion.csv"))
arrange(comprobacion,CellID)
dataframeCDRs<-as.data.frame(read.csv("export.csv"))
#Como en el txt no vienen los nombres de las columnas, se los ponemos nosotros
#Según la documentación del conjunto


ggplot(dataframeCDRs, aes(long, lat, group = group))+
  geom_polygon(aes(fill = sms), color = "white")+
  scale_fill_continuous(low='white', 'high'='red',limits=c(0,600))
theme_classic()

