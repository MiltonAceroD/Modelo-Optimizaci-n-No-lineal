#Modelo <- function(){

################ Bibliotecas Necesarias ###############
library(beepr) # Sonido
library(ggplot2) # para heat map
library(RColorBrewer) #paleta de color
library(reshape2) #Transforamación de datos
library(plyr) #División y combinación de data frames
library(MASS) # Write Data frames

  rm(list=ls()) #clear workspace
  setwd("C:/Users/macero/Documents/R/Modelo Optimizacon No lineal")
  #cat("\014") #clear console
  alfa<-0.5
  #library(dplyr)

  ########################### ENTRADAS ##################
  Datos <-read.csv("~/R/Modelo Optimizacon No lineal/Datos.csv", sep=";", stringsAsFactors=FALSE)
  Oferta <-read.csv("~/R/Modelo Optimizacon No lineal/Oferta.csv", sep=";", stringsAsFactors=FALSE)
  Tactica <-read.csv("~/R/Modelo Optimizacon No lineal/Tactica.csv", sep=";", stringsAsFactors=FALSE)
  Perfil <-read.csv("~/R/Modelo Optimizacon No lineal/Perfil.csv", sep=";", stringsAsFactors=FALSE)

  ########### quitando columnas ###############  
  Datos= Datos[-1:-2] #Quitando columnas 1 y 2
  Oferta = Oferta[-1]  #Quitando columna

  ########################## ELIMINANDO TÁCTICAS DEMANDA = 0 ####
  Tactica<-subset(Tactica,Tactica$HORAS_DEMANDA!=0)
  
  ########################## ELIMINANDO OFERTA = 0 ##############
  Oferta<-subset(Oferta,Oferta$TOTAL!=0)

  ######################## Perfiles_únicos ########################
  v_Perfiles<-unique(Datos$PERFIL)#Perfiles_únicos
  n_Perfiles<-length(v_Perfiles)
  pb <- winProgressBar(title="Modelo Ejecutando", label="0% Completado", min=0, max=100, initial=0)
  tiempo <- proc.time()
  sw<- FALSE
  indice_perfil<-1 #1
  
  
  for (indice_perfil in 1:n_Perfiles){
    perfil_seleccionado<- Perfil$NOMBRE_PERFIL[v_Perfiles[indice_perfil]]
    Datos_por_perfil<-Datos[Datos$PERFIL==v_Perfiles[indice_perfil],] #3. MEGA T2
    #Datos_por_perfil<-subset(Datos, PERFIL==v_Perfiles[indice_perfil])
    ####################### renombrando nombres de campos de "Datos por perfil" #########
    names(Oferta)[names(Oferta)=="ID_PERFIL"] <- "PERFIL"
    names(Oferta)[names(Oferta)=="FECHA"] <- "FECHA_EJECUCION"
    #################### Merge Datos + Oferta ###########################################
    Datos_por_perfil<-merge(Datos_por_perfil, Oferta, by=c("FECHA_EJECUCION","PERFIL"))
    names(Tactica)[names(Tactica)=="ID_TACTICA"] <- "TACTICA"
    Datos_por_perfil<-merge(Datos_por_perfil, Tactica, by=c("TACTICA"))
    ########################## CALCULANDO FRECUENCIAS #############
    Frecuencia_Fecha<-count(Datos_por_perfil, c("FECHA_EJECUCION"))
    Frecuencia_Tactica<-count(Datos_por_perfil, c("TACTICA"))
    ########################## RENOMBRANDO FRECUENCIAS #############
    names(Frecuencia_Fecha)[names(Frecuencia_Fecha)=="freq"] <- "Frecuencia_Fecha"
    names(Frecuencia_Tactica)[names(Frecuencia_Tactica)=="freq"] <- "Frecuencia_Tactica"
    #######################UNIENDO FRECUENCIA ###################
    Datos_por_perfil<-merge(Datos_por_perfil, Frecuencia_Fecha, by=c("FECHA_EJECUCION"))
    Datos_por_perfil<-merge(Datos_por_perfil, Frecuencia_Tactica, by=c("TACTICA"))
    n_Datos_P<-nrow(Datos_por_perfil)
    ################## CALCULANDO SOLUCIÓN INICIAL ######################
    Datos_por_perfil$HORAS_DEMANDA<-as.double(Datos_por_perfil$HORAS_DEMANDA)
    Datos_por_perfil$Frecuencia_Tactica<-as.double(Datos_por_perfil$Frecuencia_Tactica)
    Datos_por_perfil$Frecuencia_Fecha<-as.double(Datos_por_perfil$Frecuencia_Fecha)
    Datos_por_perfil$TOTAL<-as.double(Datos_por_perfil$TOTAL)
    
    ###### Calculando Cantidad para Datos por Perfil #####################
    for(i in 1:n_Datos_P){
      Datos_por_perfil$CANTIDAD[i]=alfa*min(Datos_por_perfil$HORAS_DEMANDA[i]/Datos_por_perfil$Frecuencia_Tactica[i],Datos_por_perfil$TOTAL[i]/Datos_por_perfil$Frecuencia_Fecha[i])
    }
    Xi=1:n_Datos_P
    Datos_por_perfil<-cbind(Datos_por_perfil,Xi) # Añadiendo Xi
    ############# Vector de Tácticas únicas ###################
    v_Tacticas<-unique(Datos_por_perfil$TACTICA)#Tácticas_únicas en un perfil # filas
    n_Tacticas<-length(v_Tacticas)
    ############# Vector de Dias únicos ###################
    v_Dias<-unique(Datos_por_perfil$FECHA_EJECUCION)#Dias_únicos en un perfil # columnas
    n_Dias<-length(v_Dias)
    ###################### RESTRICCIONES NO NEGATIVIDAD ###################
    A<-diag(n_Datos_P) # Matriz Identidad
    b<-rep(0,n_Datos_P)#b<-rep(0,n_Datos_P)#
    c<-Datos_por_perfil$CANTIDAD
    Tactica$HORAS_DEMANDA<- as.double(Tactica$HORAS_DEMANDA)
    ###################### RESTRICCIONES TIPO DEMANDA #####################
    for (indice_tactica in 1:n_Tacticas){
      restriccion=Datos_por_perfil[Datos_por_perfil$TACTICA==v_Tacticas[indice_tactica],]#
      n_restriccion =nrow(restriccion)
      temporal<-rep(0,n_Datos_P)
      for (indice_restriccion in 1:n_restriccion){
        temporal[restriccion$Xi[indice_restriccion]]=-1
      }
      #####################
      i2=which(Tactica$TACTICA==v_Tacticas[indice_tactica])
      A <-rbind(A,temporal) #Incluyendo Lado Izquierdo desigualdad
      b<-append(b,(-1)*Tactica$HORAS_DEMANDA[i2])
    }
    #################### RESTRICCIONES TIPO OFERTA ###################
    indice_dias <- 1
    for (indice_dias in 1:n_Dias){
      restriccion=Datos_por_perfil[Datos_por_perfil$FECHA_EJECUCION==v_Dias[indice_dias],]
      #restriccion=Datos_por_perfil[Datos_por_perfil$TACTICA==v_Tacticas[indice_tactica],]#
      n_restriccion =nrow(restriccion)
      temporal<-rep(0,n_Datos_P)
      conteo<- 0
      indice_restriccion<-1
      for (indice_restriccion in 1:n_restriccion){
        
        temporal[restriccion$Xi[indice_restriccion]]=-1
        conteo<-conteo+1
      }
      Derecha=Oferta[Oferta$PERFIL==v_Perfiles[indice_perfil] & Oferta$FECHA_EJECUCION==v_Dias[indice_dias],]
      b<-append(b,-1*as.double(Derecha$TOTAL[1]))
      A <-rbind(A,temporal)
    }
    #################### ESCRIBIR MATRIZ ###################
    
    source("Escribe.R")
    Escribe(n_Datos_P)
    source("fb.R")
    ##################### MODELO ####################################
    list_r<-constrOptim(c,fb,NULL,A,b,method="Nelder-Mead",outer.iterations=5500,outer.eps=0.000000000000001)
    ##################### MODELO ####################################
    resultado<-unlist(list_r[1])
    resultado_redondeado<-round(resultado,2)#para efectos gráficos'
    Datos_por_perfil2<-cbind(Datos_por_perfil,resultado,resultado_redondeado)
    ejeTactica<-as.character(Datos_por_perfil2$TACTICA) # convertir a cadena'
    ejeDia<-substr(Datos_por_perfil2$FECHA_EJECUCION,1,2) # extraer el día'
    Datos_por_perfil2<-cbind(Datos_por_perfil2,ejeDia)
    
    if (sw==FALSE){
      Acumulado<-Datos_por_perfil2
      sw<-TRUE
    }else{
      Acumulado<-rbind(Acumulado,Datos_por_perfil2)
    }
    
    ############### GRAFICANDO SOLUCIÓN ########################
    if (FALSE) { 
    library(ggplot2)
    library(RColorBrewer)
    library(reshape2)
    v_Dias_dia=substr(v_Dias,1,2)
    ggplot(Datos_por_perfil2, aes(x = ejeDia, y =TÁCTICA,fill = resultado)) +
      geom_tile()+ labs(x = "Día")+
      labs(y = "Táctica")+
      coord_fixed(ratio = 1)+
      ggtitle(perfil_seleccionado) +
      geom_text(aes(label=resultado_redondeado), color="grey", size=3)
    ggsave(paste0(perfil_seleccionado," Gantt2.png"), width = 16, height = 12)
    }

    ############################## BARRA DE PROGRESO #####################
    info <- sprintf("%d%% completado", round((indice_perfil/n_Perfiles)*100))
    setWinProgressBar(pb, indice_perfil/(n_Perfiles )*100, label=info)
  } # end for
########### Almacena Resultados Dataframe #####################
    write.matrix(Acumulado,"Acumlado.csv",sep = ";")
  tiempo<-proc.time()-tiempo # Calculando tiempo de ejecucción
  tiempo
  close(pb) # cierro barra de progreso
  beep() # aviso de terminación
 #} end-function



