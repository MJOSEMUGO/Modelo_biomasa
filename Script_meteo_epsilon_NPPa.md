EL PRESENTE CÓDIGO CONTIENE 3 PARTES: INTERPOLACIÓN VALORES METEOROLÓGICOS, CALCULO EPSILON, MODELO NPPa
https://github.com/Ablazcar/grasslands.git

#INTERPOLACIÓN VALORES METEOROLÓGICOS.

library(tidyverse)
library(sf)
library(dplyr)
library(raster)
library(sp)
library(mapview)
library(rgdal)
library (rgeos)
library(wesanderson)
library(gstat)
library (readxl)

setwd('DATOS_METEOROLOGICOS/Interpolacion') #directorio
ruta<-'estaciones.xltx'                    #como tengo varias hojas en el mismo excel le pongo ruta al documento
lts<-lapply(excel_sheets(ruta), read_excel, path = ruta)     #lista de hojas que tengo en el excel

estaciones<-shapefile('estaciones.shp')       #cargamos los puntos de las estaciones
plot (estaciones)  #observamos los puntos de las estaciones
mapview()   #la libreria mapview para ver la información de mapas que contiene
showMethods("mapview")  #vemos que se puede presentar con el mapview
mapview(estaciones)  #veo la localización de mis estaciones

#como voy hacer una interpolación tengo que preparar el espacio donde se van adicionar los valores de la interpolación
ext <- extent(estaciones)
grd <- expand.grid(x = seq(from = ext[1], to = ext[2], by = 750),      #x va a ir desde el arranque (from de la extensión 1 hasta el final extensión 2, y que lo haga para pixel de 750 metros)
                   y = seq(from = ext[3], to = ext[4], by = 750))      #para justificar el grid que va a tener la extensión

coordinates(grd) <- ~ x + y  #coordenadas del objeto grd, le asignamos el valor del objeto x + y. Ahora mismo está en espacial points
gridded(grd) <- TRUE   #lo pasamos a cuadricula porque aqui lo que vamos a generar es el lienzo o espacio donde se van a guardar los valores interpolados

crs(grd) <- crs(estaciones) #le vamos a dar el sistema de referencia que tiene el shp de las estaciones
plot (grd) #plot para ver el objeto grd

#seguimos preparando la información para la interpolación
pts <- lts[[1]] #el objeto lo tenemos como lista y vamos a seleccionar la información. Los puntos serán el primer objeto de la lista
coordinates(pts) <- ~ x + y #creamos un objeto espacial con esta información
crs(pts) <- crs(estaciones) #le damos el sistema de referencia de las estaciones

#interpolación
idw.p <- gstat::idw(Rad ~ 1, pts, grd) #creamos un objeto idw.p de puntos, seleccionamos la libreria gstat, voy a empezar la interpolacion con la radiacion, el siguiente argumento va a ser pts que es lo que contiene la informacion y luego vamos a pasar esta informacion al objeto grd
idw.p <- raster (idw.p) #le asignamos el valor raster

calc_idw <- function(x){
  print (x) #para ir llevando un control
  pts <- lts[[x]] 
  a <- unique(pts$estacion) #creamos un objeto que se llame a para poner los valores únicos y la etiqueta del campo quincena
  coordinates(pts) <- ~ x + y 
  crs(pts) <- crs(estaciones)
  idw.p <- gstat::idw(Rad ~ 1, pts, grd)
  idw.p <- raster (idw.p)
  dir.create("Rad")  #creo un directorio que se llame radiacion 
  writeRaster(idw.p, paste0("Rad/idw_Rad_",a,".tif"), overwrite = TRUE)  #realice un raster con la información del objeto idw.p y que lo guarde en la carpeta creada de radiacion y con el idw_Rad más el objeto a (quincena) y que sea .tif
}


tbl <- rasterToPoints(lyr, xy= TRUE) %>% as_tibble() %>%    #cómo tenemos la información en tipo lista, voy a extraer los valores del objeto lyr, con las coordenaas xy
  gather (var, value, -x, -y) 

write.csv(tbl,file=paste('tbl','.csv',sep=''))

#Una vez que tenemos creadas las tablas quincenas y fechas_quincenas pasamos al cálculo del epsilon.


#CALCULO EPSILON
read.csv('prueba.csv')   #leemos el fichero .csv donde tengamos la tabla meteo
tabla1<-read.csv('prueba.csv')   #le asignamos el nombre tabla1
tabla1$fecha<-dmy(tabla1$fecha)                  #le damos el formato dmy a la columna fecha
tabla1<-merge(tabla1,fechas_quincenas,by='fecha')  #une tabla1.csv y fechas_quincenas.csv por la columna de fecha

#ec. de presión de vapor en saturación.
tabla1$es<-(0.611*exp((17.27*tabla1$TMed)/(237.3+tabla1$TMed)))*10 

#ec. presión de vapor ambiental.
tabla1$eamb<-tabla1$es*tabla1$HR/100

#en kPa
tabla1$DPV<-(tabla1$es-tabla1$eamb)/10

#Se obtiene la Tmin escalar y la VPD escalar a partir de las restricciones de BPLUT y el epsilon máximo de la cubierta escogida:
tabla1$tminesc<-(tabla1$Tmin-tmin_min)/(tmin_max-tmin_min)
if(length(tabla1[tabla1$tminesc<0,]$tminesc)>0) {tabla1[tabla1$tminesc<0,]$tminesc<-0}
if(length(tabla1[tabla1$tminesc>1,]$tminesc)>0) {tabla1[tabla1$tminesc>1,]$tminesc<-1}
tabla1$VPDesc<-1+((vpd_min-tabla1$DPV)/(vpd_max-vpd_min))
if(length(tabla1[tabla1$VPDesc<0,]$VPDesc)>0){tabla1[tabla1$VPDesc<0,]$VPDesc<-0}
if(length(tabla1[tabla1$VPDesc>1,]$VPDesc)>0){tabla1[tabla1$VPDesc>1,]$VPDesc<-1}

#obtención de epsilon
tabla1$epsilon<-emax*tabla1$tminesc*tabla1$VPDesc
tabla1$epsilong<-0.86*tabla1$tminesc*tabla1$VPDesc

#obtención?n de PAR a partir de la radiación
tabla1$PAR<-0.48*tabla1$Rad

#exportamos la tabla meteo
write.csv(tabla1,file=paste('tabla1','.csv',sep=''))

#matriz para reclasificar fpar y NPP
reclass_0<-matrix(c(-Inf,0,0),ncol=3) #matriz para reclasificar todos los valores 
                                      #entre -Inf y 0 haci?ndolos iguales a 0


for (i in 1:nlayers(NDVI_brick_interpoladas)){
        imagen<-raster(NDVI_brick_interpoladas,layer=i)
        year<-substr(names(imagen),5,8)
        if (file_test('-f',paste('verano/verano',year,aoi,'.tif',sep=''))==FALSE){ #Cuando no existe la imagen de verano
        base<-raster(paste('verano/verano',as.integer(year)-1,aoi,'.tif',sep=''))} #Se coge la del año anterior
        else{base<-raster(paste('verano/verano',year,aoi,'.tif',sep=''))}  #Cuando sí que existe imagen de verano
        ajustada<-imagen-base   
        fpar<-1.26*ajustada-0.19
         fpar<-reclassify(fpar,reclass_0)  #ajusta a 0 los valores de fpar negativos
        if(i==1){fpar_brick<-brick(fpar)
                names(fpar_brick)[i]<-paste('fpar_',names(imagen),sep='')} 
        else{fpar_brick<-addLayer(fpar_brick,fpar)  #Lo transforma en stack pero como brick es mucho mÃ¡s lento
                names(fpar_brick)[i]<-paste('fpar_',names(imagen),sep='')}
        NPP<-10*fpar*meteo$PAR[i]*meteo$epsilon[i]
        NPP<-reclassify(NPP,reclass_0)  #ajusta a 0 los valores de NPP negativos          
        if(i==1){NPP_brick<-brick(NPP)
                names(NPP_brick)[i]<-paste('NPP_',names(imagen),sep='')
                acumulado<-reclassify(NPP,matrix(c(-Inf,0,0),ncol=3))
                NPPa_brick<-brick(acumulado)
                names(NPPa_brick)[i]<-paste('NPPa_',names(imagen),sep='')
                } 
        else{NPP_brick<-addLayer(NPP_brick,NPP)
                names(NPP_brick)[i]<-paste('NPP_',names(imagen),sep='')
                acumulado<-reclassify(acumulado,matrix(c(-Inf,0,0),ncol=3))
                if (substr(names(imagen),9,12)=='0831'){acumulado<-acumulado*0}else{acumulado<-acumulado+NPP}
                NPPa_brick<-addLayer(NPPa_brick,acumulado)
                names(NPPa_brick)[i]<-paste('NPPa_',names(imagen),sep='')
                }
        print(paste('layer ',i,' over ',nlayers(NDVI_brick_interpoladas),'  ',round(i*100/nlayers(NDVI_brick_interpoladas),2),'%',sep=''))
}


#SE ALMACENAN TODOS LOS BRICK        
directorio<-paste('biomasa/B',
                 format(max(fechas_analisis), "%y%m%d"),
                 '_emax',semax,
                 sep='')
dir.create(directorio,recursive=TRUE)



writeRaster(fpar_brick, 
            filename=paste(directorio,'/fpar_',aoi,'.grd',sep=''), 
            format="raster", 
            overwrite=TRUE)
writeRaster(NPP_brick, 
            filename=paste(directorio,'/NPP_',aoi,'.grd',sep=''), 
            format="raster", 
            overwrite=TRUE)
writeRaster(NPPa_brick, 
            filename=paste(directorio,'/NPPa_',aoi,'.grd',sep=''), 
            format="raster", 
            overwrite=TRUE)
