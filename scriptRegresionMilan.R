library(data.table)
library(rgdal)
library(ggplot2)
library(magrittr)
library(dplyr)
library(broom)
library(dbscan)
library(splitstackshape)
library(stringr)
library(lubridate)
library(anytime)
library(openintro)
require(maps)
require(viridis)
#Ruta, cambiar al directorio donde se guarden los archivos
setwd('C:\\Users\\GUILLERMO\\Desktop\\ProgramasVarios\\Python\\TFG\\archivos')
#Lectura de los archivos
#Csvs, listamos, leemos (. es cualquier car�cter) 
#Y los guardamos en auxiliarInternet, ah� los pasamos a un dataframe
archivos <- list.files(pattern="sms-call-internet-mi-2013-11-0..txt")
auxiliarInternet <- list()
for (k in 1:length(archivos)){
  auxiliarInternet[[k]] <- setDT(read.table(archivos[k],sep = "\t"))
}
dataframeCDRs<-data.frame(rbindlist(auxiliarInternet, fill=TRUE, idcol=NULL),row.names=NULL)
#Como en el txt no vienen los nombres de las columnas, se los ponemos nosotros
#Seg�n la documentaci�n del conjunto
names(dataframeCDRs) <- c("CellID","datetimeAux","countrycode","smsin","smsout","callin","callout","internet")
#Es necesario pasar de Epoch a Central European Time y dropear los NAs para trabajar
dataframeCDRs$datetimeAux<-dataframeCDRs$datetimeAux/1000
dataframeCDRs$datetimeAux<-anytime(dataframeCDRs$datetimeAux, tz="CET")
#Para este script no resulta necesario realizar trasformaciones en el dataframe
#Pair Scatterplot
#pairs(dataframeCDRs)
#Regresi�n Lineal, como hacer scattterplot es computacionalmente muy intensivo
#Primero hacemos un plot de la regresi�n para despu�s construir el modelo y
#Contrastar con la predicci�n (Entrenamiento con train y test)
#lmPlot(dataframeCDRs$callin, dataframeCDRs$callout) #Funciona
#lmPlot(dataframeCDRs$smsin, dataframeCDRs$smsout) #Tambi�n se observa correlaci�n
#lmPlot(dataframeCDRs$callin, dataframeCDRs$smsout) #Hay correlaci�n pero un poco menos
#lmPlot(dataframeCDRs$callin, dataframeCDRs$smsin) #Bastante correlaci�n
#lmPlot(dataframeCDRs$callin, dataframeCDRs$internet) #Poca correlaci�n
#lmPlot(dataframeCDRs$smsout, dataframeCDRs$internet) #Poqu�sima correlaci�n
#Tratamos los NAs que se vuelve loco haciendo el modelo
dataframeCDRs[is.na(dataframeCDRs)] <- 0 #O drop NAs?
linearMod <- lm(callin ~ smsin, data=dataframeCDRs)
summary(linearMod)
trainingRowIndex <- sample(1:nrow(dataframeCDRs), 0.4*nrow(dataframeCDRs))  
trainingData <- dataframeCDRs[trainingRowIndex, ]
testData  <- dataframeCDRs
lmMod <- lm(callin ~ smsin, data=trainingData)
smspred <- predict(lmMod, testData)
summary (lmMod)
actualsPreds <- data.frame(cbind(actuals=testData$callin, predicteds=smspred))
correlationAccuracy <- cor(actualsPreds)
print(tail(actualsPreds))
#Resultado: Donde m�s correlaci�n se ha visto ha sido en la relaci�n entre
#Mensajes/llamadas entr/sal y mensajes y llamadas entrantes