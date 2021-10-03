#*******************************************************************************************
#*******************************************************************************************
#********Script Principal para el análisis y calibración del banco de items 
#******** Modulo de Análisis Psicometrico 
#*******************************************************************************************
#*******************************************************************************************
rm(list = ls())
#*******************************************************************************************
#*******************************************************************************************
#****** Se cargan las librerias de R necesarias para el proceso
#*******************************************************************************************
#*******************************************************************************************

library(tools)
library(RODBC)
library(ltm)
library(TAM)
library(mirt)
library(ggplot2)
library(ggrepel)
library(psych)
library(polycor)
library(reshape2)
library(grid) 
library(gridExtra)
library(readxl)
library(mixtools)
library(mixAK)
library(difR)
library(dplyr)
library(tidyverse)
#*******************************************************************************************
#*******************************************************************************************



#*******************************************************************************************
#*******************************************************************************************
#********** Se ejecuta el Script que permite definir los parámetros 
#*******************************************************************************************
#*******************************************************************************************

maindir="C:/MAP"
source(paste(maindir,'/','parametros.R',sep=''))
#*******************************************************************************************
#*******************************************************************************************


#********************************************************************************************
#****** Funciones iniciales a ejecutar necesarias para el la carga de las princiaples****
#********************************************************************************************

# FUNCION sacaident
# Del vector "Sources" se retorna un vector con los nombres de los archivos con extension .R 
# que no contienen el valore de la variable ident EN SU DESCRIPCION

sacaident = function(ident,Sources) {
	ind = grep(ident,Sources)
	if(length(ind) > 0 ) Sources = Sources[-ind]
	Sources = Sources[grep('\\.R',Sources,ignore.case = T)]
	return(Sources)
}

# FUNCION ejecutascripts
# argumentos: 
#       path parametro que indica la ruta al directorio donde se
#       encuentran los scripts a ser ejecutados
#       ident parametro del tipo caracter que indica que los scripts
#      con esa cadena de caracteres no se ejecutaran, 
#      admite el caracter " "
ejecutascripts=function(path,ident,coneccion=con){
    Sources = sacaident(ident,dir(path))
    if(length(Sources) > 0) {
		for(i in Sources) {
		        sink(coneccion,append=TRUE)
		        sink(coneccion,append=TRUE,type="message")
			source(paste(path,i,sep = '/'))
			print(i)
			sink()
			sink(type="message")
		}
	}	
}


# Función mensaje de lo ejecutado

agregoproc<-function(mensaje){
	sink(conr,append=TRUE)
	print("******************************************")
	print(date())
	print(mensaje)
	print("******************************************")
	sink()
}
#********************************************************************************************
#********************************************************************************************
#********************************************************************************************
#********************************************************************************************


#********************************************************************************************
#********************************************************************************************
#***** Se crea la estructura de directorios si es necesario hacerlo
#***** Se debe crear antes de ejcutar ningun análisis
#********************************************************************************************
#********************************************************************************************

if(creaestructura==TRUE) {
	source(paste(maindir,'/','creaestructura.R',sep = ''))
	print("Se crea la estructura de directorios")
	con<-file(paste(raiz,'/Main.log',sep=''),open="a+")
	conr<-file(paste(raiz,'/reportejecucion.txt',sep=''),open="a+")
	agregoproc("Inicio del proceso Principal")
	agregoproc("Se crea la estructura de directorios")
}else{
	print("No se creo la estructura de directorios")
	con<-file(paste(raiz,'/Main.log',sep=''),open="a+")
	conr<-file(paste(raiz,'/reportejecucion.txt',sep=''),open="a+")
	agregoproc("Inicio del proceso Principal")
	agregoproc("No se creo la estructura de directorios")
}



#####################################################################
Se corre el armado de bases de SEA si es necesario
#####################################################################

if(bajabases==TRUE) {
	source(paste(maindir,'/','ArmabasesSEA','/','ArmabasesSEA.R',sep = ''))
	print("Se bajaron las bases de la plataforma SEA")
	agregoproc("Se bajaron las bases de la plataforma SEA")
}else{
	print("No se bajaron las bases de la plataforma SEA")
	agregoproc("No se bajaron las bases de la plataforma SEA")
}





#********************************************************************************************
#********************************************************************************************
##### Se cargan los paths para efectuar los análisis
#********************************************************************************************
#********************************************************************************************

source(paste(maindir,'/','Paths.R',sep = ''))
Paths(maindir,numnodos,nodo1,nodo2,nodo3,nodo4,nodo5)
agregoproc("Se generaron los paths necesarios")
#********************************************************************************************
#********************************************************************************************


#********************************************************************************************
#********************************************************************************************
#*********** Directorio de Trabajo
setwd(rutaAnalisisPrueba)
#********************************************************************************************
#********************************************************************************************


#********************************************************************************************
#********************************************************************************************
#*********Se cargan las funciones a usar
#********************************************************************************************
	path = rutaFUN
        ejecutascripts(path," ")

#********************************************************************************************
#********************************************************************************************


#********************************************************************************************
#********************************************************************************************
#*******Se generan las bases genéricas para el análisis si es necesario.
#********************************************************************************************
#********************************************************************************************

if(creabases==TRUE){
	source(paste(rutaBASES,'1_ArmaBasesGen.R',sep = '/'))
	print('Se crearon las bases genéricas')
	agregoproc("Se crearon las bases genéricas")
}else{
	print('ARMAR BASES GENERICAS NO SOLICITADO')
	agregoproc("No se solicitó crear las bases genéricas del análisis")
}



#********************************************************************************************
#********************************************************************************************
#******* ANALISIS DESCRIPTIVOS DE LOS ITEMS 
#********************************************************************************************
#
#********************************************************************************************

#***** Primero se controla existan las bases necesarias para este análisis
#***** Luego se ejcutan los scripts 
#********************************************************************************************

	options(warn=-1)	
	if(desitemsc==TRUE  ){
		dfb=data.frame(c("ConsultaAplicacionPruebas.RData","respuestas.RData"),
		c("BasesDescargadas/","BasesDescargadas/"))
		names(dfb)=c("Dato","Directorio")
	        for(i in 1:dim(dfb)[1]){
			if(file_test("-f",paste0(rutaAnalisisAristas,dfb[i,2],dfb[i,1]))==FALSE){
				agregoproc("No se realizan Descriptivos")
				agregoproc(paste0("No se encuentra ",dfb[i,1],". Debe crear bases"))
				print("No se realizan Descriptivos")
				stop(paste0("No se encuentra ",dfb[i,1],". Debe crear bases"))
			}	
		}
		path = rutaDESC_ITEMS 
		ejecutascripts(path," ")
 		print("Se realizaron solo descriptivos de items cerrados")
		agregoproc("Se realizaron solo descriptivos de items cerrados")
	} else{
		print("No se realizan Descriptivos")
		agregoproc("No se realizan Descriptivos")
	}
	options(warn=0)



#********************************************************************************************
#********************************************************************************************


