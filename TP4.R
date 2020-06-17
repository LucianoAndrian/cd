# TP4 Balance de Masa ( E y P)
# trabjar con RDatas guardados en TP4_apertura_y_seleccion.R
source("FUNCIONES.R")

# para pesar los promedios por la latitud
lat = read.table("lat.txt")
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30,10))
lon = read.table("lon.txt")

#### tendencia de P y E  y promedio zonal####


FunAux = function(pp, e, escala2, titulo, nombre.fig){
  
  
  r = length(pp[1,1,1,])
  if(r == 1){
    
    pp = pp[,,,1]
    e = e[,,,1]
    
    lat = read.table("lat.txt")
    lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30))
    lat2 = seq(1, 73, by = 1)
    aux.p = array(NA, c(73,10)); aux.e = array(NA, c(73,10))
    
    for(i in 2:72){
      aux.p[i,1] = mean(pp[,i,])*lats[1,lat2[i],1]/sum((cospi(lat/180))[lat2[i],1])
      aux.e[i,1] = mean(e[,i,])*lats[1,lat2[i],1]/sum((cospi(lat/180))[lat2[i],1])
    }
    
    datos = array(NA, c(73, 2))
    datos[,1] = aux.p[,1]
    datos[,2] = aux.e[,1]
    datos = as.data.frame(cbind(datos, lat = lat[,1]))
  } else {
    
    e = e[,,,1:r]
    
    lat = read.table("lat.txt")
    lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30,r))
    lat2 = seq(1, 73, by = 1)
    aux.p = array(NA, c(73,r+1)); aux.e = array(NA, c(73,r+1))
    
    for(i in 2:72){
      aux.p[i,1:r] = apply(pp[,i,,], c(3), mean, na.rm = T)*lats[1,lat2[i],1,]/sum((cospi(lat/180))[lat2[i],1])
      aux.e[i,1:r] = apply(e[,i,,], c(3), mean, na.rm = T)*lats[1,lat2[i],1,]/sum((cospi(lat/180))[lat2[i],1])
      aux.p[i,r+1] = mean(aux.p[i,1:r])
      aux.e[i,r+1] = mean(aux.e[i,1:r])
    }
      datos = array(NA, c(73, 2))
      datos[,1] = aux.p[,r+1]
      datos[,2] = aux.e[,r+1]
      datos = as.data.frame(cbind(datos, lat = lat[,1]))
    
  }
  
  
  g = ggplot(datos, aes(x = lat)) + theme_minimal() +
    geom_line(aes(y = datos[,1], colour = "P"), size = 1) +
    geom_line(aes(y = datos[,2], colour = "E"), size = 1) +
    scale_colour_manual("", 
                        breaks = c("P", "E"),
                        values = c("steelblue4", "orange3")) +
    scale_y_continuous(breaks = escala2, limits = c(min(escala2), max(escala2)))+
    geom_vline(xintercept = 0, color = "gray", size = 0.2)+
    metR::scale_x_latitude(breaks = seq(-90, 90, by = 15)) +
    ylab("[mm]")+ ggtitle(titulo)+xlab("")+
    theme(axis.text.y   = element_text(size = 14, color = "black"), axis.text.x   = element_text(size = 14, color = "black"), axis.title.y  = element_text("ºC"),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          panel.ontop = F,
          plot.title = element_text(hjust = 0.5, size = 18),
          legend.position = "bottom", legend.key.width = unit(2, "cm"), legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 15)) 
  
  ggsave(paste(getwd(), "/Salidas/TP4/Tend/",nombre.fig,"PE.jpg",sep =""), plot = g, width = 20, height = 10  , units = "cm")
  
  escala2 = seq(-1500,1500, by = 250)
  g = ggplot(datos, aes(x = lat)) + theme_minimal() +
    geom_line(aes(y = datos[,1] - datos[,2], colour = "P - E"), size = 1) +
    scale_colour_manual("", 
                        breaks = c("P - E"),
                        values = c("forestgreen")) +
    scale_y_continuous(breaks = escala2, limits = c(min(escala2), max(escala2)))+
    geom_vline(xintercept = 0, color = "gray", size = 0.2)+
    geom_hline(yintercept = 0, color = "gray", size = 0.2)+
    metR::scale_x_latitude(breaks = seq(-90, 90, by = 15)) +
    ylab("[mm]")+ ggtitle(titulo)+xlab("")+
    theme(axis.text.y   = element_text(size = 14, color = "black"), axis.text.x   = element_text(size = 14, color = "black"), axis.title.y  = element_text("ºC"),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          panel.ontop = F,
          plot.title = element_text(hjust = 0.5, size = 18),
          legend.position = "bottom", legend.key.width = unit(2, "cm"), legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 15)) 
  
  ggsave(paste(getwd(), "/Salidas/TP4/Tend/",nombre.fig,"P-E.jpg",sep =""), plot = g, width = 20, height = 10  , units = "cm")
 
}



#---- CNRM-CM5 ----#
#---- HISTORICAL ----#
load("RDatas/TP4.RDatas/pp5.his.RData"); load("RDatas/TP4.RDatas/evap5.his.RData")
pp = pp5.his[[1]] 
evap = evap5.his[[1]]
FunAux(pp = pp, e = evap, escala2 = seq(0,2500, 500), titulo = "CNRM-CM5 - Historico", nombre.fig = "5his")
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30, length(pp[1,1,1,])))

# p-e
auxx = apply((pp - evap)*lats, c(1,2), mean, na.rm = T) ; titulo = "P - E  CNRM-CM5 Historico"; nombre.fig = "pe5.his.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlBu", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


#---- RCP2.6 2020  2049----#

load("RDatas/TP4.RDatas/pp5.26_49.RData"); load("RDatas/TP4.RDatas/evap5.26_49.RData")

pp = pp5.26_49[[1]]  
evap = evap5.26_49[[1]]
FunAux(pp = pp, e = evap, escala2 = seq(0,2500, 500), titulo = "CNRM-CM5 RCP2.6 2020 - 2049 ", nombre.fig = "5.26_49")

# p-e
pp = apply(pp, c(1,2,3), mean); evap = apply(evap, c(1,2,3), mean)
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30))
auxx = apply((pp - evap)*lats, c(1,2), mean, na.rm = T); titulo = "P - E  CNRM-CM5 RCP2.6 2020 - 2049"; nombre.fig = "pe5.26_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlBu", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")

#---- RCP2.6 2070  2099----#

load("RDatas/TP4.RDatas/pp5.26_99.RData"); load("RDatas/TP4.RDatas/evap5.26_99.RData")

pp = pp5.26_99[[1]] 
evap = evap5.26_99[[1]]
FunAux(pp = pp, e = evap, escala2 = seq(0,2500, 500), titulo = "CNRM-CM5 RCP2.6 2070 - 2099 ", nombre.fig = "5.26_99")

# p-e
pp = apply(pp, c(1,2,3), mean); evap = apply(evap, c(1,2,3), mean)
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30))
auxx = apply((pp - evap)*lats, c(1,2), mean, na.rm = T); titulo = "P - E  CNRM-CM5 RCP2.6 2070 - 2099"; nombre.fig = "pe5.26_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlBu", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")




#---- RCP8.5 2020  2049----#

load("RDatas/TP4.RDatas/pp5.85_49.RData"); load("RDatas/TP4.RDatas/evap5.85_49.RData")

pp = pp5.85_49[[1]] 
evap = evap5.85_49[[1]]
FunAux(pp = pp, e = evap, escala2 = seq(0,2500, 500), titulo = "CNRM-CM5 RCP8.5 2020 - 2049 ", nombre.fig = "5.85_49")

# p-e
pp = apply(pp, c(1,2,3), mean); evap = apply(evap, c(1,2,3), mean)
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30))
auxx = apply((pp - evap)*lats, c(1,2), mean, na.rm = T); titulo = "P - E  CNRM-CM5 RCP8.5 2020 - 2049"; nombre.fig = "pe5.85_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlBu", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")

#---- RCP8.5 2070  2099----#
load("RDatas/TP4.RDatas/pp5.85_99.RData"); load("RDatas/TP4.RDatas/evap5.85_99.RData")

pp = pp5.85_99[[1]]  
evap = evap5.85_99[[1]]
FunAux(pp = pp, e = evap, escala2 = seq(0,2500, 500), titulo = "CNRM-CM5 RCP8.5 2070 - 2099 ", nombre.fig = "5.85_99")

# p-e
pp = apply(pp, c(1,2,3), mean); evap = apply(evap, c(1,2,3), mean)
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30))
auxx = apply((pp - evap)*lats, c(1,2), mean, na.rm = T); titulo = "P - E  CNRM-CM5 RCP8.5 2070 - 2099"; nombre.fig = "pe5.85_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlBu", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")




#---- CNRM-CM6 ----#
#---- HISTORICAL ----#
load("RDatas/TP4.RDatas/pp6.his.RData"); load("RDatas/TP4.RDatas/evap6.his.RData")
pp = pp6.his[[1]] 
evap = evap6.his[[1]]
FunAux(pp = pp, e = evap, escala2 = seq(0,2500, 500), titulo = "CNRM-CM6 - Historico", nombre.fig = "6his")

# p-e
pp = apply(pp, c(1,2,3), mean); evap = apply(evap, c(1,2,3), mean)
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30))
auxx = apply((pp - evap)*lats, c(1,2), mean, na.rm = T) ; titulo = "P - E  CNRM-CM6 Historico"; nombre.fig = "pe6.his.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlBu", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


#---- SSP12.6 2020  2049----#

load("RDatas/TP4.RDatas/pp6.26_49.RData"); load("RDatas/TP4.RDatas/evap6.26_49.RData")
pp = pp6.26_49[[1]] 
evap = evap6.26_49[[1]]
FunAux(pp = pp, e = evap, escala2 = seq(0,2500, 500), titulo = "CNRM-CM6 SSP12.6 2020 - 2049 ", nombre.fig = "6.26_49")

# p-e
pp = apply(pp, c(1,2,3), mean); evap = apply(evap, c(1,2,3), mean)
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30))
auxx = apply((pp - evap)*lats, c(1,2), mean, na.rm = T); titulo = "P - E  CNRM-CM6 SSP12.6 2020 - 2049"; nombre.fig = "pe6.26_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlBu", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")

#---- SSP12.6 2070  2099----#
load("RDatas/TP4.RDatas/pp6.26_99.RData"); load("RDatas/TP4.RDatas/evap6.26_99.RData")
pp = pp6.26_99[[1]]  
evap = evap6.26_99[[1]]
FunAux(pp = pp, e = evap, escala2 = seq(0,2500, 500), titulo = "CNRM-CM6 SSP12.6 2070 - 2099 ", nombre.fig = "6.26_99")

# p-e
pp = apply(pp, c(1,2,3), mean); evap = apply(evap, c(1,2,3), mean)
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30))
auxx = apply((pp - evap)*lats, c(1,2), mean, na.rm = T); titulo = "P - E  CNRM-CM6 SSP12.6 2070 - 2099"; nombre.fig = "pe6.26_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlBu", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


#---- SSP58.5 2020  2049----#
load("RDatas/TP4.RDatas/pp6.85_49.RData"); load("RDatas/TP4.RDatas/evap6.85_49.RData")
pp = pp6.85_49[[1]] 
evap = evap6.85_49[[1]]
FunAux(pp = pp, e = evap, escala2 = seq(0,2500, 500), titulo = "CNRM-CM6 SSP58.5 2020 - 2049 ", nombre.fig = "6.85_49")
# p-e
pp = apply(pp, c(1,2,3), mean); evap = apply(evap, c(1,2,3), mean)
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30))
auxx = apply((pp - evap)*lats, c(1,2), mean, na.rm = T); titulo = "P - E  CNRM-CM6 SSP58.5 2020 - 2049"; nombre.fig = "pe6.85_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlBu", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


#---- SSP58.5 2070  2099----#
load("RDatas/TP4.RDatas/pp6.85_99.RData"); load("RDatas/TP4.RDatas/evap6.85_99.RData")
pp = pp6.85_99[[1]]
evap = evap6.85_99[[1]]
FunAux(pp = pp, e = evap, escala2 = seq(0,2500, 500), titulo = "CNRM-CM6 SSP58.5 2070 - 2099 ", nombre.fig = "6.85_99")

# p-e
pp = apply(pp, c(1,2,3), mean); evap = apply(evap, c(1,2,3), mean)
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30))
auxx = apply((pp - evap)*lats, c(1,2), mean, na.rm = T); titulo = "P - E  CNRM-CM6 SSP58.5 2070 - 2099"; nombre.fig = "pe6.85_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlBu", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")




rm(list = ls())
source("FUNCIONES.R")


#### TABLA 7.1 ####


cm = c("5", "6")
rcp = c("26", "85")

source("FUNCIONES.R")

for( i in 1:2){
  for(j in 1:2){
    
    load(paste("RDatas/TP4.RDatas/", "pp", cm[i],".his.RData", sep = ""), envir = N <- new.env())  #### !!!
    load(paste("RDatas/TP4.RDatas/evap", cm[i],".his.RData", sep = ""), envir = M <- new.env() )   #### !!!
    pp = get(paste("pp", cm[i], ".his", sep = ""), envir =  N)[[1]]
    evap = get(paste("evap", cm[i], ".his", sep = ""), envir =  M)[[1]]
    
    nombre = paste(cm[i], ".his", sep = "")
    tabla = Tabla7.1(pp = pp, evap = evap, nombre = nombre, salida = "/Salidas/TP4/tablas/", r = T)
    
    
    
    load(paste("RDatas/TP4.RDatas/pp",cm[i],".",rcp[j], "_49.RData", sep = ""), envir = N <- new.env())
    load(paste("RDatas/TP4.RDatas/evap",cm[i],".",rcp[j], "_49.RData", sep = ""), envir = M <- new.env())
    
    pp = get(paste("pp", cm[i], ".", rcp[j], "_49", sep = ""), envir =  N)[[1]]
    evap = get(paste("evap", cm[i], ".", rcp[j], "_49", sep = ""), envir =  M)[[1]]
    
    nombre = paste(cm[i], ".", rcp[j], "_49", sep = "")
    tabla2 = Tabla7.1(pp = pp, evap = evap, nombre = nombre, salida = "/Salidas/TP4/tablas/", r = T)
    
    
    
    load(paste("RDatas/TP4.RDatas/pp",cm[i],".",rcp[j], "_99.RData", sep = ""), N <- new.env())
    load(paste("RDatas/TP4.RDatas/evap",cm[i],".",rcp[j], "_99.RData", sep = ""), M <- new.env())
    
    pp = get(paste("pp", cm[i], ".", rcp[j], "_99", sep = ""), envir =  N)[[1]]
    evap = get(paste("evap", cm[i], ".", rcp[j], "_99", sep = ""), envir =  M)[[1]]
    
    nombre = paste(cm[i], ".", rcp[j], "_99", sep = "")
    tabla3 = Tabla7.1(pp = pp, evap = evap, nombre = nombre, salida = "/Salidas/TP4/tablas/", r = T)
    
    
    
    
    titulo = paste("P - E  CNRM-CM", cm[i], " RCP", rcp[j] ,sep = "")
    nombre = paste(cm[i], "_", rcp[j], sep ="")
    
    Tabla7.1Grafico(data.his =  tabla[[1]], data.49 = tabla2[[1]], data.99 = tabla3[[1]], v = 3,limites = c(-900, 900), titulo = titulo, nombre = nombre, salida = "/Salidas/TP4/"
                    , escala2 = T, limites2 = c(-100, 100), global = T, width = 20, height = 15)
    
    Tabla7.1Grafico_Continental(data.his = tabla[[2]], data.49 = tabla2[[2]], data.99 = tabla3[[2]], v = 3, limites = c(-400,400), titulo = titulo, nombre = paste(nombre, "_", sep = ""),  salida = "/Salidas/TP4/"
                                , width = 20, height = 15)
    
    print(paste("RDatas/TP4.RDatas/pp",cm[i],".",rcp[j], "_99.RData", sep = ""))
    print(paste("P - E  CNRM-CM", cm[i], " RCP", rcp[j] ,sep = ""))
    
    rm(N, M)
  }
}



### TENDENCIA  en rcp####
##### revisar estooo#####
lat = read.table("lat.txt")
cm = c("5", "6")
rcp = c("26", "85")
rcp2 = c("RCP", "RCP", "SSP1", "SSP5")
source("FUNCIONES.R")
escala = list(); escala[[1]] = seq(1090, 1130, by = 10); escala[[2]] = seq(1050, 1100, by = 10)
escala2 = list(); escala2[[1]] = seq(1090, 1190, by = 10); escala2[[2]] = seq(1020, 1160, by = 10)
escala3 = list(); escala3[[1]] = seq(-2, 2, by = 0.5); escala3[[2]] = seq(-26, 26, by = 4) 
escala4 = list(); escala4[[1]] = seq(-2, 2, by = 0.5); escala4[[2]] = seq(-6, 6, by = 1) 
for( i in 1:2){
  for(j in 1:2){
    
    load(paste("RDatas/TP4.RDatas/", "pp", cm[i],".his.RData", sep = ""), envir = N <- new.env())  #### !!!
    load(paste("RDatas/TP4.RDatas/evap", cm[i],".his.RData", sep = ""), envir = M <- new.env() )   #### !!!
    pp = get(paste("pp", cm[i], ".his", sep = ""), envir =  N)[[1]]
    evap = get(paste("evap", cm[i], ".his", sep = ""), envir =  M)[[1]]
    
    aux = CorecLat(pp = pp, e = evap)
    PlotTs2(data = aux, titulo = paste("CNRM-CM",cm[i], "  1976-2005", sep = ""), nombre.fig = paste(cm[i], ".his", sep = ""),
           escala = escala[[i]], escala2 = escala3[[i]])
    
    datos = as.data.frame(apply(aux[[1]], c(1), mean))
    datos = cbind(datos, apply(aux[[2]], c(1), mean))
    colnames(datos) = c("p", "e")
  
     
    load(paste("RDatas/TP4.RDatas/pp",cm[i],".",rcp[j], "_49.RData", sep = ""), envir = N <- new.env())
    load(paste("RDatas/TP4.RDatas/evap",cm[i],".",rcp[j], "_49.RData", sep = ""), envir = M <- new.env())
    pp = get(paste("pp", cm[i], ".", rcp[j], "_49", sep = ""), envir =  N)[[1]]
    evap = get(paste("evap", cm[i], ".", rcp[j], "_49", sep = ""), envir =  M)[[1]]

    aux = CorecLat(pp = pp, e = evap)
    
    if((j == 2 & i == 1)|(j == 1 & i == 2)|(j == 2 & i == 2)){ # q forma poco practica...
      datos2 = as.data.frame(apply(aux[[1]], c(1), mean))
      datos2 = cbind(datos2, apply(aux[[2]], c(1), mean))
      colnames(datos2) = c("p", "e")
      datos = rbind(datos, NA, datos2)
    } else {
      datos2 = as.data.frame(aux[[1]])
      datos2 = cbind(datos2, aux[[2]])
      colnames(datos2) = c("p", "e")
      datos = rbind(datos, NA, datos2)
    }
    
    
    
    load(paste("RDatas/TP4.RDatas/pp",cm[i],".",rcp[j], "_99.RData", sep = ""), N <- new.env())
    load(paste("RDatas/TP4.RDatas/evap",cm[i],".",rcp[j], "_99.RData", sep = ""), M <- new.env())
    pp = get(paste("pp", cm[i], ".", rcp[j], "_99", sep = ""), envir =  N)[[1]]
    evap = get(paste("evap", cm[i], ".", rcp[j], "_99", sep = ""), envir =  M)[[1]]
    
    aux = CorecLat(pp = pp, e = evap)
    
    if((j == 2 & i == 1)|(j == 1 & i == 2)|(j == 2 & i == 2)){
      datos2 = as.data.frame(apply(aux[[1]], c(1), mean))
      datos2 = cbind(datos2, apply(aux[[2]], c(1), mean))
      colnames(datos2) = c("p", "e")
      datos = rbind(datos, NA, datos2)
    } else {
      datos2 = as.data.frame(aux[[1]])
      datos2 = cbind(datos2, aux[[2]])
      colnames(datos2) = c("p", "e")
      datos = rbind(datos, NA, datos2)
    }
    
    
    aux = seq(1976, 2005); aux1 = seq(2020,2049); aux2 = seq(2070, 2099)
    años = c(aux, 2010 , aux1, 2060 , aux2)
    
    datos = cbind(datos, años)
    
    PlotTs(datos = datos, escala = escala2[[i]], escala2 = escala4[[i]], titulo = paste("CNRM-CM", cm[i], " ", rcp2[i+j],rcp[j], sep = "")
           , nombre = paste(cm[i], ".", rcp[j], "_49", sep = ""), c = 700)
    
    
  }
}

