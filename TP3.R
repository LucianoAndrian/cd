#TP3 Energia 
library(ncdf4)
source("FUNCIONES.R")
# carga de datos.
#### Observaciones ####

# TEMP
aux = nc_open(paste(getwd(), "/TP3datos.obs/tmp.mon.mean.nc", sep = ""))
t.obs = ncvar_get(aux, "air")
lon.obs = ncvar_get(aux, "lon")
lat.obs = ncvar_get(aux, "lat")
nc_close(aux)

# rotando
t.obs = t.obs[,ncol(t.obs):1,]

t.obs_seasons = open_ncobs.tp3(t.obs)
t.obs_anual = t.obs[,,13:372] # sacando 1975
t.obs_anual.mean = AnualMean(t.obs_anual)

# HR 

aux = nc_open(paste(getwd(), "/TP3datos.obs/rhum.mon.mean.nc", sep = ""))
hu.obs = ncvar_get(aux, "rhum")
nc_close(aux)

# rotando
hu.obs = hu.obs[,ncol(hu.obs):1,]

hu.obs_seasons = open_ncobs.tp3(hu.obs)
hu.obs_anual = hu.obs[,,13:372]
hu.obs_anual.mean = AnualMean(hu.obs_anual)

# PRESION
aux = nc_open(paste(getwd(), "/TP3datos.obs/pl.mon.mean.nc", sep = ""))
pr.obs = ncvar_get(aux, "pres")
nc_close(aux)

# rotando
pr.obs = pr.obs[,ncol(pr.obs):1,]

pr.obs_seasons = open_ncobs.tp3(pr.obs)
pr.obs_anual = pr.obs[,,13:372]
pr.obs_anual.mean = AnualMean(pr.obs_anual)

# q 
q.obs = RhQ(rh = hu.obs, p = pr.obs, t = t.obs)
q.obs_anual = RhQ(rh = hu.obs_anual, p = pr.obs_anual, t = t.obs_anual)
q.obs_anual.mean = RhQ(rh = hu.obs_anual.mean, p = pr.obs_anual.mean, t = t.obs_anual.mean)

#--- TENDENCIAS ----#
#### OBS ####

t.tend = Tendencia(t.obs_anual.mean)
q.tend = Tendencia(q.obs_anual.mean)

#--- mods ---# 
#### CNRM-CM5 #####
#------------------------------------------------------- HISTORICAL ------------------------------------------------------#
load("RDatas/TP3.RDatas/t5.his.RData"); load("RDatas/TP3.RDatas/hu5.his.RData")
t5.his = Tendencia(apply(t5.his[[1]], c(1,2,3), mean, na.rm = T))
q5.his = Tendencia(apply(hu5.his[[1]], c(1,2,3), mean, na.rm = T))
rm(hu5.his)
#-------------------------------------------------------------------------------------------------------------------------#


#------------------------------------------------------- RCP26 2020 2049--------------------------------------------------#
load("RDatas/TP3.RDatas/t5.26_49.RData"); load("RDatas/TP3.RDatas/hu5.26_49.RData")
t5.26_49 = Tendencia(apply(t5.26_49[[1]], c(1,2,3), mean, na.rm = T))
q5.26_49 = Tendencia(apply(hu5.26_49[[1]], c(1,2,3), mean, na.rm = T))
rm(hu5.26_49)
#-------------------------------------------------------------------------------------------------------------------------#


#------------------------------------------------------- RCP26 2070 2099--------------------------------------------------#
load("RDatas/TP3.RDatas/t5.26_99.RData"); load("RDatas/TP3.RDatas/hu5.26_99.RData")
t5.26_99 = Tendencia(apply(t5.26_99[[1]], c(1,2,3), mean, na.rm = T))
q5.26_99 = Tendencia(apply(hu5.26_99[[1]], c(1,2,3), mean, na.rm = T))
rm(hu5.26_99)
#-------------------------------------------------------------------------------------------------------------------------#


#------------------------------------------------------- RCP85 2020 2049--------------------------------------------------#
load("RDatas/TP3.RDatas/t5.85_49.RData"); load("RDatas/TP3.RDatas/hu5.85_49.RData")
t5.85_49 = Tendencia(apply(t5.85_49[[1]], c(1,2,3), mean, na.rm = T))
q5.85_49 = Tendencia(apply(hu5.85_49[[1]], c(1,2,3), mean, na.rm = T))
rm(hu5.85_49)
#-------------------------------------------------------------------------------------------------------------------------#

#------------------------------------------------------- RCP85 2070 2099--------------------------------------------------#
load("RDatas/TP3.RDatas/t5.85_99.RData"); load("RDatas/TP3.RDatas/hu5.85_99.RData")
t5.85_99 = Tendencia(apply(t5.85_99[[1]], c(1,2,3), mean, na.rm = T))
q5.85_99 = Tendencia(apply(hu5.85_99[[1]], c(1,2,3), mean, na.rm = T))
rm(hu5.85_99)
#-------------------------------------------------------------------------------------------------------------------------#

#.rs.restartR() <- ACA ESTA EL COSO PARA RESETEAR!!!

#### CNRM-CM6 ####
#------------------------------------------------------- HISTORICAL ------------------------------------------------------#
load("RDatas/TP3.RDatas/t6.his.RData"); load("RDatas/TP3.RDatas/hu6.his.RData")
t6.his = AnualMean(apply(t6.his[[3]], c(1,2,3), mean, na.rm = T))
hu6.his = AnualMean(apply(hu6.his[[3]], c(1,2,3), mean, na.rm = T))
t6.his = Tendencia(t6.his)
q6.his = Tendencia(hu6.his)
rm(hu6.his)
#-------------------------------------------------------------------------------------------------------------------------#

#------------------------------------------------------- SSP26 2020 2049--------------------------------------------------#
load("RDatas/TP3.RDatas/t6.26_49.RData"); load("RDatas/TP3.RDatas/hu6.26_49.RData")
load("RDatas/TP3.RDatas/etp6.26_49.RData")
t6.26_2049 = AnualMean(apply(t6.26_2049[[3]], c(1,2,3), mean, na.rm = T))
hu6.26_2049 = AnualMean(apply(hu6.26_2049[[3]], c(1,2,3), mean, na.rm = T))
t6.26_49 = Tendencia(t6.26_2049)
q6.26_49 = Tendencia(hu6.26_2049)
rm(hu6.26_2049)
#-------------------------------------------------------------------------------------------------------------------------#


#------------------------------------------------------- SSP26 2070 2099--------------------------------------------------#
load("RDatas/TP3.RDatas/t6.26_99.RData"); load("RDatas/TP3.RDatas/hu6.26_99.RData")
load("RDatas/TP3.RDatas/etp6.26_99.RData")
t6.26_2099 = AnualMean(apply(t6.26_2099[[3]], c(1,2,3), mean, na.rm = T))
hu6.26_2099 = AnualMean(apply(hu6.26_2099[[3]], c(1,2,3), mean, na.rm = T))
t6.26_99 = Tendencia(t6.26_2099)
q6.26_99 = Tendencia(hu6.26_2099)
#-------------------------------------------------------------------------------------------------------------------------#
rm(hu6.26_2099)
#-------------------------------------------------------------------------------------------------------------------------#

#------------------------------------------------------- SSP85 2020 2049--------------------------------------------------#
load("RDatas/TP3.RDatas/t6.85_49.RData"); load("RDatas/TP3.RDatas/hu6.85_49.RData")
t6.85_2049 = AnualMean(apply(t6.85_2049[[3]], c(1,2,3), mean, na.rm = T))
hu6.85_2049 = AnualMean(apply(hu6.85_2049[[3]], c(1,2,3), mean, na.rm = T))
t6.85_49 = Tendencia(t6.85_2049)
q6.85_49 = Tendencia(hu6.85_2049)
rm(hu6.85_2049)
#-------------------------------------------------------------------------------------------------------------------------#


#------------------------------------------------------- SSP85 2070 2099--------------------------------------------------#
load("RDatas/TP3.RDatas/t6.85_99.RData"); load("RDatas/TP3.RDatas/hu6.85_99.RData")
t6.85_2099 = AnualMean(apply(t6.85_2099[[3]], c(1,2,3), mean, na.rm = T))
hu6.85_2099 = AnualMean(apply(hu6.85_2099[[3]], c(1,2,3), mean, na.rm = T))
t6.85_99 = Tendencia(t6.85_2099)
q6.85_99 = Tendencia(hu6.85_2099)
rm(hu6.85_2099)
#-------------------------------------------------------------------------------------------------------------------------#


#### Graficos Tendencia ####
.rs.restartR() # no borra variables, pero se deben volver a cargar funciones y librerias
source("FUNCIONES.R")
library(ggplot2)
#--- OBSERVADO ----#
#---------------------------------------------------------- TEMPERATURA ---------------------------------------------------#
auxx = t.tend; titulo = "Tendencia de T Reanalisis 1976 - 2005"; nombre.fig = "t.r_tend" 
aux = array(auxx[[1]][,ncol(auxx[[1]]):1], c(144,73,1)); mask = array(auxx[[2]][,ncol(auxx[[1]]):1], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux*30, variable.sig = mask, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", niveles = 11, revert = T, escala = seq(-4,4, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.3, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "ºC/30Años", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP3/Tend/", colorbar.pos = "bottom")


auxx = t5.his; titulo = "Tendencia de T CNRM-CM5 - 1976 - 2005"; nombre.fig = "t5.his_tend" 
aux = array(auxx[[1]][,ncol(auxx[[1]]):1], c(144,73,1))
mask = array(auxx[[2]][,ncol(auxx[[1]]):1], c(144,73,1))
mask[which(!is.na(mask))] = 0
mask[which(mask == 1)] = NA # ver esto
mapa_topo3(variable = aux*30, variable.sig = mask, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", niveles = 11, revert = T, escala = seq(-4,4, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.3, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "ºC/30Años", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP3/Tend/", colorbar.pos = "bottom")



auxx = t6.his; titulo = "Tendencia de T CNRM-CM6 - 1976 - 2005"; nombre.fig = "t6.his_tend" 
aux = array(auxx[[1]][,ncol(auxx[[1]]):1], c(144,73,1))
mask = array(auxx[[2]][,ncol(auxx[[1]]):1], c(144,73,1))
mask[which(!is.na(mask))] = 0
mask[which(mask == 1)] = NA # ver esto
mapa_topo3(variable = aux*30, variable.sig = mask, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", niveles = 11, revert = T, escala = seq(-4,4, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.3, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "ºC/30Años", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP3/Tend/", colorbar.pos = "bottom")




auxx = t5.26_49; titulo = "Tendencia de T CNRM-CM5 RCP2.6 - 2020 - 2049"; nombre.fig = "t5.26_49_tend" 
aux = array(auxx[[1]][,ncol(auxx[[1]]):1], c(144,73,1))
mask = array(auxx[[2]][,ncol(auxx[[1]]):1], c(144,73,1))
mask[which(!is.na(mask))] = 0
mask[which(mask == 1)] = NA # ver esto
mapa_topo3(variable = aux*30, variable.sig = mask, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", niveles = 11, revert = T, escala = seq(-4,4, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.3, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "ºC/30Años", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP3/Tend/", colorbar.pos = "bottom")


auxx = t5.26_99; titulo = "Tendencia de T CNRM-CM5 RCP2.6 - 2070 - 2099"; nombre.fig = "t5.26_99_tend" 
aux = array(auxx[[1]][,ncol(auxx[[1]]):1], c(144,73,1))
mask = array(auxx[[2]][,ncol(auxx[[1]]):1], c(144,73,1))
mask[which(!is.na(mask))] = 0
mask[which(mask == 1)] = NA # ver esto
mapa_topo3(variable = aux*30, variable.sig = mask, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", niveles = 11, revert = T, escala = seq(-4,4, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.3, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "ºC/30Años", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP3/Tend/", colorbar.pos = "bottom")



auxx = t5.85_49; titulo = "Tendencia de T CNRM-CM5 RCP8.5 - 2020 - 2049"; nombre.fig = "t5.85_49_tend" 
aux = array(auxx[[1]][,ncol(auxx[[1]]):1], c(144,73,1))
mask = array(auxx[[2]][,ncol(auxx[[1]]):1], c(144,73,1))
mask[which(!is.na(mask))] = 0
mask[which(mask == 1)] = NA # ver esto
mapa_topo3(variable = aux*30, variable.sig = mask, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", niveles = 11, revert = T, escala = seq(-4,4, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.3, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "ºC/30Años", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP3/Tend/", colorbar.pos = "bottom")


auxx = t5.85_99; titulo = "Tendencia de T CNRM-CM5 RCP8.5 - 2070 - 2099"; nombre.fig = "t5.85_99_tend" 
aux = array(auxx[[1]][,ncol(auxx[[1]]):1], c(144,73,1))
mask = array(auxx[[2]][,ncol(auxx[[1]]):1], c(144,73,1))
mask[which(!is.na(mask))] = 0
mask[which(mask == 1)] = NA # ver esto
mapa_topo3(variable = aux*30, variable.sig = mask, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", niveles = 11, revert = T, escala = seq(-4,4, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.3, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "ºC/30Años", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP3/Tend/", colorbar.pos = "bottom")


auxx = t6.26_49; titulo = "Tendencia de T CNRM-CM6 SSP126 - 2020 - 2049"; nombre.fig = "t6.26_49_tend" 
aux = array(auxx[[1]][,ncol(auxx[[1]]):1], c(144,73,1))
mask = array(auxx[[2]][,ncol(auxx[[1]]):1], c(144,73,1))
mask[which(!is.na(mask))] = 0
mask[which(mask == 1)] = NA # ver esto
mapa_topo3(variable = aux*30, variable.sig = mask, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", niveles = 11, revert = T, escala = seq(-4,4, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.3, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "ºC/30Años", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP3/Tend/", colorbar.pos = "bottom")


auxx = t6.26_99; titulo = "Tendencia de T CNRM-CM6 SSP126 - 2070 - 2099"; nombre.fig = "t6.26_99_tend" 
aux = array(auxx[[1]][,ncol(auxx[[1]]):1], c(144,73,1))
mask = array(auxx[[2]][,ncol(auxx[[1]]):1], c(144,73,1))
mask[which(!is.na(mask))] = 0
mask[which(mask == 1)] = NA # ver esto
mapa_topo3(variable = aux*30, variable.sig = mask, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", niveles = 11, revert = T, escala = seq(-4,4, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.3, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "ºC/30Años", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP3/Tend/", colorbar.pos = "bottom")



auxx = t6.85_49; titulo = "Tendencia de T CNRM-CM6 SSP585 - 2020 - 2049"; nombre.fig = "t6.85_49_tend" 
aux = array(auxx[[1]][,ncol(auxx[[1]]):1], c(144,73,1))
mask = array(auxx[[2]][,ncol(auxx[[1]]):1], c(144,73,1))
mask[which(!is.na(mask))] = 0
mask[which(mask == 1)] = NA # ver esto
mapa_topo3(variable = aux*30, variable.sig = mask, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", niveles = 11, revert = T, escala = seq(-4,4, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.3, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "ºC/30Años", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP3/Tend/", colorbar.pos = "bottom")


auxx = t6.85_99; titulo = "Tendencia de T CNRM-CM6 SSP585 - 2070 - 2099"; nombre.fig = "t6.85_99_tend" 
aux = array(auxx[[1]][,ncol(auxx[[1]]):1], c(144,73,1))
mask = array(auxx[[2]][,ncol(auxx[[1]]):1], c(144,73,1))
mask[which(!is.na(mask))] = 0
mask[which(mask == 1)] = NA # ver esto
mapa_topo3(variable = aux*30, variable.sig = mask, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", niveles = 11, revert = T, escala = seq(-4,4, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.3, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "ºC/30Años", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP3/Tend/", colorbar.pos = "bottom")
#-------------------------------------------------------------------------------------------------------------------------#

#-------------------------------------------------------- HUMEDAD --------------------------------------------------------#

auxx = q.tend; titulo = "Tendencia de q Reanalisis 1976 - 2005"; nombre.fig = "q.r_tend" 
aux = array(auxx[[1]][,ncol(auxx[[1]]):1], c(144,73,1)); mask = array(auxx[[2]][,ncol(auxx[[1]]):1], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux*30*1000, variable.sig = mask, lon = lon.obs, lat = lat.obs, colorbar = "BrBG", niveles = 11, revert = F, escala = seq(-2, 2, by = 0.25)
           , color.vsig = "black", alpha.vsig = 0.3, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "kg/kg/30Años", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP3/Tend/", colorbar.pos = "bottom")


auxx = q5.his; titulo = "Tendencia de q CNRM-CM5 - 1976 - 2005"; nombre.fig = "q5.his_tend" 
aux = array(auxx[[1]][,ncol(auxx[[1]]):1], c(144,73,1))
mask = array(auxx[[2]][,ncol(auxx[[1]]):1], c(144,73,1))
mask[which(!is.na(mask))] = 0
mask[which(mask == 1)] = NA # ver esto
mapa_topo3(variable = aux*30*1000, variable.sig = mask, lon = lon.obs, lat = lat.obs, colorbar = "BrBG", niveles = 11, revert = F, escala = seq(-2, 2, by = 0.25)
           , color.vsig = "black", alpha.vsig = 0.3, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "kg/kg/30Años", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP3/Tend/", colorbar.pos = "bottom")



auxx = q6.his; titulo = "Tendencia de q CNRM-CM6 - 1976 - 2005"; nombre.fig = "q6.his_tend" 
aux = array(auxx[[1]][,ncol(auxx[[1]]):1], c(144,73,1))
mask = array(auxx[[2]][,ncol(auxx[[1]]):1], c(144,73,1))
mask[which(!is.na(mask))] = 0
mask[which(mask == 1)] = NA # ver esto
mapa_topo3(variable = aux*30*1000, variable.sig = mask, lon = lon.obs, lat = lat.obs, colorbar = "BrBG", niveles = 11, revert = F, escala = seq(-2, 2, by = 0.25)
           , color.vsig = "black", alpha.vsig = 0.3, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "kg/kg/30Años", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP3/Tend/", colorbar.pos = "bottom")




auxx = q5.26_49; titulo = "Tendencia de q CNRM-CM5 RCP2.6 - 2020 - 2049"; nombre.fig = "q5.26_49_tend" 
aux = array(auxx[[1]][,ncol(auxx[[1]]):1], c(144,73,1))
mask = array(auxx[[2]][,ncol(auxx[[1]]):1], c(144,73,1))
mask[which(!is.na(mask))] = 0
mask[which(mask == 1)] = NA # ver esto
mapa_topo3(variable = aux*30*1000, variable.sig = mask, lon = lon.obs, lat = lat.obs, colorbar = "BrBG", niveles = 11, revert = F, escala = seq(-2, 2, by = 0.25)
           , color.vsig = "black", alpha.vsig = 0.3, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "kg/kg/30Años", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP3/Tend/", colorbar.pos = "bottom")


auxx = q5.26_99; titulo = "Tendencia de q CNRM-CM5 RCP2.6 - 2070 - 2099"; nombre.fig = "q5.26_99_tend" 
aux = array(auxx[[1]][,ncol(auxx[[1]]):1], c(144,73,1))
mask = array(auxx[[2]][,ncol(auxx[[1]]):1], c(144,73,1))
mask[which(!is.na(mask))] = 0
mask[which(mask == 1)] = NA # ver esto
mapa_topo3(variable = aux*30*1000, variable.sig = mask, lon = lon.obs, lat = lat.obs, colorbar = "BrBG", niveles = 11, revert = F, escala = seq(-2, 2, by = 0.25)
           , color.vsig = "black", alpha.vsig = 0.3, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "kg/kg/30Años", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP3/Tend/", colorbar.pos = "bottom")



auxx = q5.85_49; titulo = "Tendencia de q CNRM-CM5 RCP8.5 - 2020 - 2049"; nombre.fig = "q5.85_49_tend" 
aux = array(auxx[[1]][,ncol(auxx[[1]]):1], c(144,73,1))
mask = array(auxx[[2]][,ncol(auxx[[1]]):1], c(144,73,1))
mask[which(!is.na(mask))] = 0
mask[which(mask == 1)] = NA # ver esto
mapa_topo3(variable = aux*30*1000, variable.sig = mask, lon = lon.obs, lat = lat.obs, colorbar = "BrBG", niveles = 11, revert = F, escala = seq(-2, 2, by = 0.25)
           , color.vsig = "black", alpha.vsig = 0.3, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "kg/kg/30Años", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP3/Tend/", colorbar.pos = "bottom")


auxx = q5.85_99; titulo = "Tendencia de q CNRM-CM5 RCP8.5 - 2070 - 2099"; nombre.fig = "q5.85_99_tend" 
aux = array(auxx[[1]][,ncol(auxx[[1]]):1], c(144,73,1))
mask = array(auxx[[2]][,ncol(auxx[[1]]):1], c(144,73,1))
mask[which(!is.na(mask))] = 0
mask[which(mask == 1)] = NA # ver esto
mapa_topo3(variable = aux*30*1000, variable.sig = mask, lon = lon.obs, lat = lat.obs, colorbar = "BrBG", niveles = 11, revert = F, escala = seq(-2, 2, by = 0.25)
           , color.vsig = "black", alpha.vsig = 0.3, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "kg/kg/30Años", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP3/Tend/", colorbar.pos = "bottom")


auxx = q6.26_49; titulo = "Tendencia de q CNRM-CM6 SSP126 - 2020 - 2049"; nombre.fig = "q6.26_49_tend" 
aux = array(auxx[[1]][,ncol(auxx[[1]]):1], c(144,73,1))
mask = array(auxx[[2]][,ncol(auxx[[1]]):1], c(144,73,1))
mask[which(!is.na(mask))] = 0
mask[which(mask == 1)] = NA # ver esto
mapa_topo3(variable = aux*30*1000, variable.sig = mask, lon = lon.obs, lat = lat.obs, colorbar = "BrBG", niveles = 11, revert = F, escala = seq(-2, 2, by = 0.25)
           , color.vsig = "black", alpha.vsig = 0.3, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "kg/kg/30Años", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP3/Tend/", colorbar.pos = "bottom")


auxx = q6.26_99; titulo = "Tendencia de q CNRM-CM6 SSP126 - 2070 - 2099"; nombre.fig = "q6.26_99_tend" 
aux = array(auxx[[1]][,ncol(auxx[[1]]):1], c(144,73,1))
mask = array(auxx[[2]][,ncol(auxx[[1]]):1], c(144,73,1))
mask[which(!is.na(mask))] = 0
mask[which(mask == 1)] = NA # ver esto
mapa_topo3(variable = aux*30*1000, variable.sig = mask, lon = lon.obs, lat = lat.obs, colorbar = "BrBG", niveles = 11, revert = F, escala = seq(-2, 2, by = 0.25)
           , color.vsig = "black", alpha.vsig = 0.3, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "kg/kg/30Años", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP3/Tend/", colorbar.pos = "bottom")



auxx = q6.85_49; titulo = "Tendencia de q CNRM-CM6 SSP585 - 2020 - 2049"; nombre.fig = "q6.85_49_tend" 
aux = array(auxx[[1]][,ncol(auxx[[1]]):1], c(144,73,1))
mask = array(auxx[[2]][,ncol(auxx[[1]]):1], c(144,73,1))
mask[which(!is.na(mask))] = 0
mask[which(mask == 1)] = NA # ver esto
mapa_topo3(variable = aux*30*1000, variable.sig = mask, lon = lon.obs, lat = lat.obs, colorbar = "BrBG", niveles = 11, revert = F, escala = seq(-2, 2, by = 0.25)
           , color.vsig = "black", alpha.vsig = 0.3, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "kg/kg/30Años", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP3/Tend/", colorbar.pos = "bottom")


auxx = q6.85_99; titulo = "Tendencia de q CNRM-CM6 SSP585 - 2070 - 2099"; nombre.fig = "q6.85_99_tend" 
aux = array(auxx[[1]][,ncol(auxx[[1]]):1], c(144,73,1))
mask = array(auxx[[2]][,ncol(auxx[[1]]):1], c(144,73,1))
mask[which(!is.na(mask))] = 0
mask[which(mask == 1)] = NA # ver esto
mapa_topo3(variable = aux*30*1000, variable.sig = mask, lon = lon.obs, lat = lat.obs, colorbar = "BrBG", niveles = 11, revert = F, escala = seq(-2, 2, by = 0.25)
           , color.vsig = "black", alpha.vsig = 0.3, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "kg/kg/30Años", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP3/Tend/", colorbar.pos = "bottom")


#---------------------------------------------------------------------------------------------------------#




#### ENTALPIA ####

# obs
H.his_an = EntalpiaHR(t = t.obs_anual.mean, hr = hu.obs_anual.mean, p = pr.obs_anual.mean)


# mods
H5.his = EntalpiaQ(t = "t5.his"); H5.26_49 = EntalpiaQ(t = "t5.26_49"); H5.26_99 = EntalpiaQ(t = "t5.26_99")
H5.85_49 = EntalpiaQ(t = "t5.85_49"); H5.85_99 = EntalpiaQ(t = "t5.85_99")

H6.his = EntalpiaQ(t = "t6.his"); H6.26_49 = EntalpiaQ(t = "t6.26_49"); H6.26_99 = EntalpiaQ(t = "t6.26_99")
H6.85_49 = EntalpiaQ(t = "t6.85_49"); H6.85_99 = EntalpiaQ(t = "t6.85_99")


#### GRAFICOS ####
#### CAMPO MEDIO OBSERVADO ####
mask = H.his_an[[3]]
mask[which(!is.na(mask))] = 0
mask[which(is.na(mask))] = 1
mask[which(mask == 1)] = NA
mapa_topo3(variable = H.his_an[[3]], lon = lon.obs, lat = lat.obs, colorbar = "Spectral", niveles = 11, escala = seq(20, 70, by = 5), revert = T
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "white"
           , alpha.vsig = 1, sig = T
           , titulo = "Entalpía Reanalisis  1976 - 2005",  nombre.fig = "H.his", width = 35, x.label = NULL, y.label = NULL, label.escala = "KJ/kg")

mapa_topo3(variable = H.his_an[[2]], lon = lon.obs, lat = lat.obs, colorbar = "PuBuGn", niveles = 9, escala = seq(0, 100, by = 10), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "white"
           , alpha.vsig = 1, sig = T
           , titulo = "Aporte de q  1976 - 2005 Reanalisis",  nombre.fig = "H.hisP", width = 35, x.label = NULL, y.label = NULL, label.escala = "%")


# Bias CNRM-CM5
mask = H5.his[[3]] - H.his_an[[3]]
mask[which(!is.na(mask))] = 0
mask[which(is.na(mask))] = 1
mask[which(mask == 1)] = NA
mapa_topo3(variable = H5.his[[3]] - H.his_an[[3]], lon = lon.obs, lat = lat.obs, colorbar = "RdBu", niveles = 9, escala = seq(-8, 8, by = 2), revert = T
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "white"
           , alpha.vsig = 1, sig = T
           , titulo = "Bias Entalpía CNRM-CM5",  nombre.fig = "H5.bias", width = 35, x.label = NULL, y.label = NULL, label.escala = "KJ/kg")

mapa_topo3(variable = H5.his[[2]], lon = lon.obs, lat = lat.obs, colorbar = "PuBuGn", niveles = 9, escala = seq(0, 100, by = 10), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "white"
           , alpha.vsig = 1, sig = T
           , titulo = "Aporte de q 1976 - 2005 CNRM-CM6",  nombre.fig = "H5.hisP", width = 35, x.label = NULL, y.label = NULL, label.escala = "%")

# Bias CNRM-CM6
mask = H6.his[[3]] - H.his_an[[3]]
mask[which(!is.na(mask))] = 0
mask[which(is.na(mask))] = 1
mask[which(mask == 1)] = NA
mapa_topo3(variable = H6.his[[3]] - H.his_an[[3]], lon = lon.obs, lat = lat.obs, colorbar = "RdBu", niveles = 9, escala = seq(-8, 8, by = 2), revert = T
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "white"
           , alpha.vsig = 1, sig = T
           , titulo = "Bias H CNRM-CM6",  nombre.fig = "H6.bias", width = 35, x.label = NULL, y.label = NULL, label.escala = "KJ/kg")

mapa_topo3(variable = H6.his[[2]], lon = lon.obs, lat = lat.obs, colorbar = "PuBuGn", niveles = 9, escala = seq(0, 100, by = 10), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "white"
           , alpha.vsig = 1, sig = T
           , titulo = "Aporte de q 1976 - 2005 CNRM-CM6",  nombre.fig = "H6.hisP", width = 35, x.label = NULL, y.label = NULL, label.escala = "%")



#### CNRM-CM5 ####
# 2.6
mask = H5.26_49[[3]] - H5.his[[3]]
mask[which(!is.na(mask))] = 0
mask[which(is.na(mask))] = 1
mask[which(mask == 1)] = NA
mapa_topo3(variable = H5.26_49[[3]] - H5.his[[3]], lon = lon.obs, lat = lat.obs, colorbar = "YlOrRd", niveles = 9, escala = seq(0, 10, by = 1), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "white"
           , alpha.vsig = 1, sig = T
           , titulo = "Diferencia Entalpía Futuro Cercano  - CNRM-CM5 RCP2.6",  nombre.fig = "H5.dif.26_49", width = 35, x.label = NULL, y.label = NULL, label.escala = "KJ/kg")

mapa_topo3(variable = H5.26_49[[2]], lon = lon.obs, lat = lat.obs, colorbar = "PuBuGn", niveles = 9, escala = seq(0, 100, by = 10), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "white"
           , alpha.vsig = 1, sig = T
           , titulo = "Aporte de q Futuro Cercano CNRM-CM5 RCP2.6",  nombre.fig = "H5P.26_49", width = 35, x.label = NULL, y.label = NULL, label.escala = "%")


mask = H5.26_99[[3]] - H5.his[[3]]
mask[which(!is.na(mask))] = 0
mask[which(is.na(mask))] = 1
mask[which(mask == 1)] = NA
mapa_topo3(variable = H5.26_99[[3]] - H5.his[[3]], lon = lon.obs, lat = lat.obs, colorbar = "YlOrRd", niveles = 9, escala = seq(0, 10, by = 1), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "white"
           , alpha.vsig = 1, sig = T
           , titulo = "Diferencia Entalpía Futuro Lejano  - CNRM-CM5 RCP2.6",  nombre.fig = "H5.dif.26_99", width = 35, x.label = NULL, y.label = NULL, label.escala = "KJ/kg")

mapa_topo3(variable = H5.26_99[[2]], lon = lon.obs, lat = lat.obs, colorbar = "PuBuGn", niveles = 9, escala = seq(0, 100, by = 10), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "white"
           , alpha.vsig = 1, sig = T
           , titulo = "Aporte de q Futuro Lejano  CNRM-CM5 RCP2.6",  nombre.fig = "H5P.26_99", width = 35, x.label = NULL, y.label = NULL, label.escala = "%")

# 8.5
mask = H5.85_49[[3]] - H5.his[[3]]
mask[which(!is.na(mask))] = 0
mask[which(is.na(mask))] = 1
mask[which(mask == 1)] = NA
mapa_topo3(variable = H5.85_49[[3]] - H5.his[[3]], lon = lon.obs, lat = lat.obs, colorbar = "YlOrRd", niveles = 9, escala = seq(0, 10, by = 1), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "white"
           , alpha.vsig = 1, sig = T
           , titulo = "Diferencia Entalpía Futuro Cercano  - CNRM-CM5 RCP8.5",  nombre.fig = "H5.dif.85_49", width = 35, x.label = NULL, y.label = NULL, label.escala = "KJ/kg")

mapa_topo3(variable = H5.85_49[[2]], lon = lon.obs, lat = lat.obs, colorbar = "PuBuGn", niveles = 9, escala = seq(0, 100, by = 10), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "white"
           , alpha.vsig = 1, sig = T
           , titulo = "Aporte de q  Futuro Cercano CNRM-CM5 RCP8.5",  nombre.fig = "H5.P.85_49", width = 35, x.label = NULL, y.label = NULL, label.escala = "%")


mask = H5.85_99[[3]] - H5.his[[3]]
mask[which(!is.na(mask))] = 0
mask[which(is.na(mask))] = 1
mask[which(mask == 1)] = NA
mapa_topo3(variable = H5.85_99[[3]] - H5.his[[3]], lon = lon.obs, lat = lat.obs, colorbar = "YlOrRd", niveles = 9, escala = seq(0, 10, by = 1), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "white"
           , alpha.vsig = 1, sig = T
           , titulo = "Diferencia Entalpía Futuro Lejano  - CNRM-CM5 RCP8.5",  nombre.fig = "H5.dif.85_99", width = 35, x.label = NULL, y.label = NULL, label.escala = "KJ/kg")

mapa_topo3(variable = H5.85_99[[2]], lon = lon.obs, lat = lat.obs, colorbar = "PuBuGn", niveles = 9, escala = seq(0, 100, by = 10), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "white"
           , alpha.vsig = 1, sig = T
           , titulo = "Aporte de q  Futuro Lejano CNRM-CM5 RCP8.5",  nombre.fig = "H5.P.85_99", width = 35, x.label = NULL, y.label = NULL, label.escala = "%")

#### CNRM-CM6 ####
#126
mask = H6.26_49[[3]] - H6.his[[3]]
mask[which(!is.na(mask))] = 0
mask[which(is.na(mask))] = 1
mask[which(mask == 1)] = NA
mapa_topo3(variable = H6.26_49[[3]] - H6.his[[3]], lon = lon.obs, lat = lat.obs, colorbar = "YlOrRd", niveles = 9, escala = seq(0, 10, by = 1), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "white"
           , alpha.vsig = 1, sig = T
           , titulo = "Diferencia Entalpía Futuro Cercano  - CNRM-CM6 SSP126",  nombre.fig = "H6.dif.26_49", width = 35, x.label = NULL, y.label = NULL, label.escala = "KJ/kg")

mapa_topo3(variable = H6.26_49[[2]], lon = lon.obs, lat = lat.obs, colorbar = "PuBuGn", niveles = 9, escala = seq(0, 100, by = 10), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "white"
           , alpha.vsig = 1, sig = T
           , titulo = "Aporte de q  Futuro Cercano  CNRM-CM6 SSP126",  nombre.fig = "H6.P.26_49", width = 35, x.label = NULL, y.label = NULL, label.escala = "%")


mask = H6.26_99[[3]] - H6.his[[3]]
mask[which(!is.na(mask))] = 0
mask[which(is.na(mask))] = 1
mask[which(mask == 1)] = NA
mapa_topo3(variable = H6.26_99[[3]] - H6.his[[3]], lon = lon.obs, lat = lat.obs, colorbar = "YlOrRd", niveles = 9, escala = seq(0, 10, by = 1), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "white"
           , alpha.vsig = 1, sig = T
           , titulo = "Diferencia Entalpía Futuro Lejano  - CNRM-CM6 SSP126",  nombre.fig = "H6.dif.26_99", width = 35, x.label = NULL, y.label = NULL, label.escala = "KJ/kg")

mapa_topo3(variable = H6.26_99[[2]], lon = lon.obs, lat = lat.obs, colorbar = "PuBuGn", niveles = 9, escala = seq(0, 100, by = 10), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "white"
           , alpha.vsig = 1, sig = T
           , titulo = "Aporte de q Futuro Lejano  CNRM-CM6 SSP126",  nombre.fig = "H6.P.26_99", width = 35, x.label = NULL, y.label = NULL, label.escala = "%")

#858
mask = H6.85_49[[3]] - H6.his[[3]]
mask[which(!is.na(mask))] = 0
mask[which(is.na(mask))] = 1
mask[which(mask == 1)] = NA
mapa_topo3(variable = H6.85_49[[3]] - H6.his[[3]], lon = lon.obs, lat = lat.obs, colorbar = "YlOrRd", niveles = 9, escala = seq(0, 10, by = 1), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "white"
           , alpha.vsig = 1, sig = T
           , titulo = "Diferencia Entalpía Futuro Cercano  - CNRM-CM6 SSP585",  nombre.fig = "H6.dif.85_49", width = 35, x.label = NULL, y.label = NULL, label.escala = "KJ/kg")

mapa_topo3(variable = H6.85_49[[2]], lon = lon.obs, lat = lat.obs, colorbar = "PuBuGn", niveles = 9, escala = seq(0, 100, by = 10), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "white"
           , alpha.vsig = 1, sig = T
           , titulo = "Aporte de q Futuro Cercano  CNRM-CM6 SSP585",  nombre.fig = "H6.P.85_49", width = 35, x.label = NULL, y.label = NULL, label.escala = "%")

                               

mask = H6.85_99[[3]] - H6.his[[3]]
mask[which(!is.na(mask))] = 0
mask[which(is.na(mask))] = 1
mask[which(mask == 1)] = NA
mapa_topo3(variable = H6.85_99[[3]] - H6.his[[3]], lon = lon.obs, lat = lat.obs, colorbar = "YlOrRd", niveles = 9, escala = seq(0, 10, by = 1), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "white"
           , alpha.vsig = 1, sig = T
           , titulo = "Diferencia Entalpía Futuro Lejano  - CNRM-CM6 SSP585",  nombre.fig = "H6.dif.85_99", width = 35, x.label = NULL, y.label = NULL, label.escala = "KJ/kg")

mapa_topo3(variable = H6.85_99[[2]], lon = lon.obs, lat = lat.obs, colorbar = "PuBuGn", niveles = 9, escala = seq(0, 100, by = 10), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "white"
           , alpha.vsig = 1, sig = T
           , titulo = "Aporte de q Futuro Lejano  CNRM-CM6 SSP585",  nombre.fig = "H6.P.85_99", width = 35, x.label = NULL, y.label = NULL, label.escala = "%")

#### promedios zonales de H ####

H.his_lon = apply(H.his_an[[3]], c(2), mean, na.rm = T)

H5.his_lon = apply(H5.his[[3]], c(2), mean, na.rm = T)
H5.26_49_lon = apply(H5.26_49[[3]], c(2), mean, na.rm = T); H5.26_99_lon = apply(H5.26_99[[3]], c(2), mean, na.rm = T)
H5.85_49_lon = apply(H5.85_49[[3]], c(2), mean, na.rm = T); H5.85_99_lon = apply(H5.85_99[[3]], c(2), mean, na.rm = T)

H6.his_lon = apply(H6.his[[3]], c(2), mean, na.rm = T)
H6.26_49_lon = apply(H6.26_49[[3]], c(2), mean, na.rm = T); H6.26_99_lon = apply(H6.26_99[[3]], c(2), mean, na.rm = T)
H6.85_49_lon = apply(H6.85_49[[3]], c(2), mean, na.rm = T); H6.85_99_lon = apply(H6.85_99[[3]], c(2), mean, na.rm = T)

datos = as.data.frame(H.his_lon); datos = cbind(lat.obs, datos, H5.his_lon, H6.his_lon); colnames(datos) = c("lat", "obs", "cm5", "cm6")
g = ggplot(datos, aes(x = lat)) + theme_minimal()+
  geom_line(aes(y = obs, colour = "Observado"), size = 1, show.legend = T) + 
  geom_line(aes(y = cm5, colour = "CNRM-CM5"),linetype = 1, size = 1, show.legend = T)  +
  geom_line(aes(y = cm6, colour = "CNRM-CM6"),linetype = 1, size = 1, show.legend = T) +
  scale_colour_manual("", 
                      breaks = c("Observado", "CNRM-CM5", "CNRM-CM6"),
                      values = c("black","springgreen2", "tomato1")) +
  scale_x_latitude(breaks = seq(-90, 90, by = 20)) + scale_y_continuous(breaks = seq(10, 80, by = 10), limits = c(10, 80)) +
  geom_vline(xintercept = 0, alpha = 0.3)+
  ggtitle("Promedio H Zonal Historico") +
  ylab("kJ/kg") +
  theme(axis.text.y   = element_text(size = 14, color = "black"), axis.text.x   = element_text(size = 14, color = "black"), axis.title.y  = element_text("ºC"),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.ontop = F,
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "bottom", legend.key.width = unit(3, "cm"), legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 14)) 

ggsave(paste(getwd(), "/Salidas/TP3/", "H.his_lon", ".jpg",sep =""), plot = g, width = 20, height = 10  , units = "cm")

  
HLonMean(serie1 = H5.his_lon, serie2 = H5.26_49_lon, serie3 = H5.26_99_lon, lat = lat.obs, titulo = "Promedio Zonal H  CNRM-CM5 RCP2.6", nombre.fig = "H5.lon26_49")
HLonMean(serie1 = H5.his_lon, serie2 = H5.85_49_lon, serie3 = H5.85_99_lon, lat = lat.obs, titulo = "Promedio Zonal H  CNRM-CM5 RCP8.5", nombre.fig = "H5.lon85_49")
HLonMean(serie1 = H6.his_lon, serie2 = H6.26_49_lon, serie3 = H6.26_99_lon, lat = lat.obs, titulo = "Promedio Zonal H  CNRM-CM5 SSP126", nombre.fig = "H6.lon26_49")
HLonMean(serie1 = H6.his_lon, serie2 = H6.85_49_lon, serie3 = H6.85_99_lon, lat = lat.obs, titulo = "Promedio Zonal H  CNRM-CM5 SSP585", nombre.fig = "H6.lon85_49")




#### radacion ####
# solo CM6

#------------------------------------------------------- HISTORICAL ------------------------------------------------------#
load("RDatas/TP3.RDatas/t6.his.RData"); load("RDatas/TP3.RDatas/hu6.his.RData"); load("RDatas/TP3.RDatas/etp6.his_an.RData")
load("RDatas/TP3.RDatas/u6.his.RData"); load("RDatas/TP3.RDatas/v6.his.RData")
#-------------------------------------------------------------------------------------------------------------------------#
.rs.restartR() 
source("FUNCIONES.R")
load("RDatas/TP3.RDatas/etp5.his.RData")
etp5 = etp5.his[[1]]/(86400*365/12)/12
RT5 = etp5*2500000
fields::image.plot(RT5[,,1,1])


t6.his = AnualMeanR(t6.his[[3]])
hu6.his = AnualMeanR(hu6.his[[3]])
u6.his = AnualMeanR(u6.his[[3]])
v6.his = AnualMeanR(v6.his[[3]])

etp = etp6.his_an[[1]]/(2628002.88)/(12)/10

RT6 = etp[,,,1:length(t6.his[1,1,1,])]*Lv(t6.his)  # radiacion neta [W/m2]

RT6[which(RT6 >200)]=NA

fields::image.plot(RT6[,,1,1])

h = S(t6.his) + Lv(t6.his)*hu6.his #[kJ/kg]*1000-->J/kg


aux = array(prueba, dim = c(144,73,1))
aux = aux[,ncol(aux):1,]
aux = array(aux, dim = c(144,73,1))
mask = aux
mask[which(!is.na(mask))] = 0
mask[which(is.na(mask))] = 1
mask[which(mask == 1)] = NA
mapa_topo3(variable = aux/100, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", niveles = 9, escala = seq(-40, 40, by = 10), mapa = "mundo", revert = T
           , nombre.fig = "preuba", na.fill = 0, salida = "/Salidas/TP3/"
           , variable.sig = mask, color.vsig = "white", alpha.vsig = 1, sig = T)



##### Variabilidad interanual ####


source("FUNCIONES.R")
#### CNRM-CM5 #####
#
load("RDatas/TP3.RDatas/t5.his.RData")
load("RDatas/TP3.RDatas/hu5.his.RData")
load("RDatas/TP3.RDatas/etp5.his.RData")

t5 = t5.his[[1]]
q5 = hu5.his[[1]]
H5.his_an = EntalpiaQ2(t = t5, q = q5, seasons = F)

etp = etp5.his[[1]]/(2628002.88)/(12)
Rt5.his = etp*Lv(t5[,,,1:length(t5[1,1,1,])])*1000
Rt5z_his = apply(Rt5.his, c(2), mean, na.rm = T)
aux = apply(H5.his_an[[4]], c(1,2,3), mean, na.rm = T)
aux2 = apply(Rt5.his, c(1,2,3), mean, na.rm = T)
RtH5.his = corr(mod = aux, obs = aux2, lon = 144, lat = 73, cf = 0.95 )

t5.his = t5
q5.his = q5 
#------------------------------------------------------------------------#
load("RDatas/TP3.RDatas/t5.85_49.RData")
load("RDatas/TP3.RDatas/hu5.85_49.RData")
load("RDatas/TP3.RDatas/etp5.85_49.RData")

t5 = t5.85_49[[1]]
q5 = hu5.85_49[[1]]
H5.85_49_an = EntalpiaQ2(t = t5, q = q5, seasons = F)

etp = etp5.85_49[[1]]/(2628002.88)/(12)
Rt5.85_49 = etp[,,,1]*Lv(t5[,,,1])*1000
Rt5z_85_49 = apply(Rt5.85_49, c(2), mean, na.rm = T)
aux = apply(H5.85_49_an[[4]], c(1,2,3), mean, na.rm = T)
aux2 = apply(Rt5.85_49, c(1,2,3), mean, na.rm = T)
RtH5.85_49 = corr(mod = aux, obs = aux2, lon = 144, lat = 73, cf = 0.95 )

t5.85_49 = t5
q5.85_49 = q5 

#-------------------------------------------------------------------------#
load("RDatas/TP3.RDatas/t5.26_49.RData")
load("RDatas/TP3.RDatas/hu5.26_49.RData")
load("RDatas/TP3.RDatas/etp5.26_49.RData")

t5 = t5.26_49[[1]]
q5 = hu5.26_49[[1]]
H5.26_49_an = EntalpiaQ2(t = t5, q = q5, seasons = F)

etp = etp5.26_49[[1]]/(2628002.88)/(12)
Rt5.26_49 = etp[,,,1]*Lv(t5[,,,1])*1000
Rt5z_26_49 = apply(Rt5.26_49, c(2), mean, na.rm = T)
aux = apply(H5.26_49_an[[4]], c(1,2,3), mean, na.rm = T)
aux2 = apply(Rt5.26_49, c(1,2,3), mean, na.rm = T)
RtH5.26_49 = corr2.0(mod = aux, obs = aux2, lon = 144, lat = 73, cf = 0.95 )

t5.26_49 = t5
q5.26_49 = q5 

#-------------------------------------------------------------------------#
load("RDatas/TP3.RDatas/t5.85_99.RData")
load("RDatas/TP3.RDatas/hu5.85_99.RData")
load("RDatas/TP3.RDatas/etp5.85_99.RData")

t5 = t5.85_99[[1]]
q5 = hu5.85_99[[1]]
H5.85_99_an = EntalpiaQ2(t = t5, q = q5, seasons = F)

etp = etp5.85_99[[1]]/(2628002.88)/(12)
Rt5.85_99 = etp*Lv(t5[,,,1:length(etp[1,1,1,])])*1000
Rt5z_85_99 = apply(Rt5.85_99, c(2), mean, na.rm = T)
aux = apply(H5.85_99_an[[4]], c(1,2,3), mean, na.rm = T)
aux2 = apply(Rt5.85_99, c(1,2,3), mean, na.rm = T)
RtH5.85_99 = corr(mod = aux, obs = aux2, lon = 144, lat = 73, cf = 0.95 )

t5.85_99 = t5
q5.85_99 = q5 
#-------------------------------------------------------------------------#
load("RDatas/TP3.RDatas/t5.26_99.RData")
load("RDatas/TP3.RDatas/hu5.26_99.RData")
load("RDatas/TP3.RDatas/etp5.26_99.RData")

t5 = t5.26_99[[1]]
q5 = hu5.26_99[[1]]
H5.26_99_an = EntalpiaQ2(t = t5, q = q5, seasons = F)

etp = etp5.26_99[[1]]/(2628002.88)/(12)
Rt5.26_99 = etp[,,,1]*Lv(t5[,,,1])*1000
Rt5z_26_99 = apply(Rt5.26_99, c(2), mean, na.rm = T)
aux = apply(H5.26_99_an[[4]], c(1,2,3), mean, na.rm = T)
aux2 = apply(Rt5.26_99, c(1,2,3), mean, na.rm = T)
RtH5.26_99 = corr2.0(mod = aux, obs = aux2, lon = 144, lat = 73, cf = 0.95 )
t5.26_99 = t5
q5.26_99 = q5 



##### CNRM-CM6 #####

#---- Historical ----#
load("RDatas/TP3.RDatas/t6.his.RData")
load("RDatas/TP3.RDatas/hu6.his.RData")
load("RDatas/TP3.RDatas/etp6.his_an.RData")

t6 = AnualMeanR(t6.his[[3]])
q6 = AnualMeanR(hu6.his[[3]])
H.his_an = EntalpiaQ2(t = t6, q = q6, seasons = F)

etp = etp6.his_an[[1]]/(2628002.88)/(12)/10 # este es un 10 es magico
RT6 = etp[,,,1:length(t6[1,1,1,])]*Lv(t6)*1000
RT6z = apply(RT6, c(2), mean, na.rm = T)

t6.his_seasons = t6.his[[1]]
hu6.his_seasons = hu6.his[[1]]
H.his_seasons = EntalpiaQ2(t = t6.his_seasons, q = hu6.his_seasons)
  
t6.his = AnualMonthR(t6.his[[3]])
hu6.his = AnualMonthR(hu6.his[[3]])

H.his = EntalpiaQ2(t = t6.his, q = hu6.his, seasons = F)


H.his_seasonsZonal = apply(H.his_seasons[[1]], c(2,3), mean, na.rm = T)
H.his_seasonsZonal2 = apply(H.his_seasons[[2]], c(2,3), mean, na.rm = T)
H.his_seasonsZonal3 = apply(H.his_seasons[[3]], c(2,3), mean, na.rm = T)
aux = apply(H.his_an[[4]], c(1,2,3), mean, na.rm = T)
aux2 = apply(RT6, c(1,2,3), mean, na.rm = T)
RtH.his =  corr(mod = aux, obs = aux2, lon = 144, lat = 73, cf =0.95 )

#------- 126 2049 ------#
load("RDatas/TP3.RDatas/t6.26_49.RData")
load("RDatas/TP3.RDatas/hu6.26_49.RData")
load("RDatas/TP3.RDatas/etp6.26_49.RData")

t6 = AnualMeanR(t6.26_2049[[3]])
q6 = AnualMeanR(hu6.26_2049[[3]])
H.26_49_an = EntalpiaQ2(t = t6, q = q6, seasons = F)

t6 = AnualMeanR(t6.26_2049[[3]])
etp = etp6.26_49[[1]]/(2628002.88)/(12)/10
RT6.26_49 = etp[,,,1:length(t6[1,1,1,])]*Lv(t6)*1000
RT6z_26_49 = apply(RT6.26_49, c(2), mean, na.rm = T)

t6.26_2049_seasons = t6.26_2049[[1]]
hu6.26_2049_seasons = hu6.26_2049[[1]]


t6.26_2049 = AnualMonthR(t6.26_2049[[3]])
hu6.26_2049 = AnualMonthR(hu6.26_2049[[3]])

H.26_49_seasons = EntalpiaQ2(t = t6.26_2049_seasons, q = hu6.26_2049_seasons)
H.26_49 = EntalpiaQ2(t = t6.26_2049, q = hu6.26_2049, seasons = F)

H.26_49_seasonsZonal = apply(H.26_49_seasons[[1]], c(2,3), mean, na.rm = T)
H.26_49_seasonsZonal2 = apply(H.26_49_seasons[[2]], c(2,3), mean, na.rm = T)
H.26_49_seasonsZonal3 = apply(H.26_49_seasons[[3]], c(2,3), mean, na.rm = T)

aux = apply(H.26_49_an[[4]], c(1,2,3), mean, na.rm = T)
aux2 = apply(RT6.26_49, c(1,2,3), mean, na.rm = T)
RtH6.26_49 = corr(mod = aux, obs = aux2, lon = 144, lat = 73, cf = 0.95 )

#------- 126 2099 ------#
load("RDatas/TP3.RDatas/t6.26_99.RData")
load("RDatas/TP3.RDatas/hu6.26_99.RData")
load("RDatas/TP3.RDatas/etp6.26_99.RData")

t6 = AnualMeanR(t6.26_2099[[3]])
q6 = AnualMeanR(hu6.26_2099[[3]])
H.26_99_an = EntalpiaQ2(t = t6, q = q6, seasons = F)

t6 = AnualMeanR(t6.26_2099[[3]])
etp = etp6.26_99[[1]]/(2628002.88)/(12)/10
RT6.26_99 = etp[,,,1:length(t6[1,1,1,])]*Lv(t6)*1000
RT6z_26_99 = apply(RT6.26_99, c(2), mean, na.rm = T)

t6.26_2099_seasons = t6.26_2099[[1]]
hu6.26_2099_seasons = hu6.26_2099[[1]]

t6.26_2099 = AnualMonthR(t6.26_2099[[3]])
hu6.26_2099 = AnualMonthR(hu6.26_2099[[3]])

H.26_99_seasons = EntalpiaQ2(t = t6.26_2099_seasons, q = hu6.26_2099_seasons)
H.26_99 = EntalpiaQ2(t = t6.26_2099, q = hu6.26_2099, seasons = F)

H.26_99_seasonsZonal = apply(H.26_99_seasons[[1]], c(2,3), mean, na.rm = T)
H.26_99_seasonsZonal2 = apply(H.26_99_seasons[[2]], c(2,3), mean, na.rm = T)
H.26_99_seasonsZonal3 = apply(H.26_99_seasons[[3]], c(2,3), mean, na.rm = T)


aux = apply(H.26_99_an[[4]], c(1,2,3), mean, na.rm = T)
aux2 = apply(RT6.26_99, c(1,2,3), mean, na.rm = T)
RtH6.26_99 = corr(mod = aux, obs = aux2, lon = 144, lat = 73, cf = 0.95 )

#------- 185 2049 ------#
load("RDatas/TP3.RDatas/t6.85_49.RData")
load("RDatas/TP3.RDatas/hu6.85_49.RData")
load("RDatas/TP3.RDatas/etp6.85_49.RData")

t6 = AnualMeanR(t6.85_2049[[3]])
q6 = AnualMeanR(hu6.85_2049[[3]])
H.85_49_an = EntalpiaQ2(t = t6, q = q6, seasons = F)


t6 = AnualMeanR(t6.85_2049[[3]])
etp = etp6.85_49[[1]]/(2628002.88)/(12)/10
RT6.85_49 = etp[,,,1:length(t6[1,1,1,])]*Lv(t6)*1000
RT6z_85_49 = apply(RT6.85_49, c(2), mean, na.rm = T)

t6.85_2049_seasons = t6.85_2049[[1]]
hu6.85_2049_seasons = hu6.85_2049[[1]]

t6.85_2049 = AnualMonthR(t6.85_2049[[3]])
hu6.85_2049 = AnualMonthR(hu6.85_2049[[3]])

H.85_49_seasons = EntalpiaQ2(t = t6.85_2049_seasons, q = hu6.85_2049_seasons)
H.85_49 = EntalpiaQ2(t = t6.85_2049, q = hu6.85_2049, seasons = F)

H.85_49_seasonsZonal = apply(H.85_49_seasons[[1]], c(2,3), mean, na.rm = T)
H.85_49_seasonsZonal2 = apply(H.85_49_seasons[[2]], c(2,3), mean, na.rm = T)
H.85_49_seasonsZonal3 = apply(H.85_49_seasons[[3]], c(2,3), mean, na.rm = T)

aux = apply(H.85_49_an[[4]], c(1,2,3), mean, na.rm = T)
aux2 = apply(RT6.85_49, c(1,2,3), mean, na.rm = T)
RtH6.85_49 = corr(mod = aux, obs = aux2, lon = 144, lat = 73, cf = 0.95 )

#------- 185 2099 ------#
load("RDatas/TP3.RDatas/t6.85_99.RData")
load("RDatas/TP3.RDatas/hu6.85_99.RData")
load("RDatas/TP3.RDatas/etp6.85_99.RData")

t6 = AnualMeanR(t6.85_2099[[3]])
q6 = AnualMeanR(hu6.85_2099[[3]])
H.85_99_an = EntalpiaQ2(t = t6, q = q6, seasons = F)

t6 = AnualMeanR(t6.85_2099[[3]])
etp = etp6.85_99[[1]]/(2628002.88)/(12)/10
RT6.85_99 = etp[,,,1:length(t6[1,1,1,])]*Lv(t6)*1000
RT6z_85_99 = apply(RT6.85_99, c(2), mean, na.rm = T)

t6.85_2099_seasons = t6.85_2099[[1]]
hu6.85_2099_seasons = hu6.85_2099[[1]]

t6.85_2099 = AnualMonthR(t6.85_2099[[3]])
hu6.85_2099 = AnualMonthR(hu6.85_2099[[3]])

H.85_99_seasons = EntalpiaQ2(t = t6.85_2099_seasons, q = hu6.85_2099_seasons)
H.85_99 = EntalpiaQ2(t = t6.85_2099, q = hu6.85_2099, seasons = F)

H.85_99_seasonsZonal = apply(H.85_99_seasons[[1]], c(2,3), mean, na.rm = T)
H.85_99_seasonsZonal2 = apply(H.85_99_seasons[[2]], c(2,3), mean, na.rm = T)
H.85_99_seasonsZonal3 = apply(H.85_99_seasons[[3]], c(2,3), mean, na.rm = T)

aux = apply(H.85_99_an[[4]], c(1,2,3), mean, na.rm = T)
aux2 = apply(RT6.85_99, c(1,2,3), mean, na.rm = T)
RtH6.85_99 = corr(mod = aux, obs = aux2, lon = 144, lat = 73, cf = 0.95 )

#### Graficos #####
#### Rt ####
aux1 = aux = ComMT3(Rt5.his)
mapa_topo3(variable = aux[[1]], variable.sig = mask, sig = T, lon = lon.obs, lat = lat.obs, colorbar = "YlOrRd", escala = seq(0, 200, by = 20)
           , niveles = 9, revert = F, color.vsig = "white", alpha.vsig = 1, titulo = "Rt CNRR-CM5 - Historico", colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = 0, width = 30, nombre.fig = "Rt5.his", salida = "/Salidas/TP3/Rt/", label.escala = "W/m2")



aux = ComMT3(Rt5.26_49)
mapa_topo3(variable = aux[[1]] - aux1[[1]], variable.sig = aux[[2]], sig = T, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", escala = seq(-10, 10, by = 1)
           , niveles = 9, revert = T, color.vsig = "white", alpha.vsig = 1, titulo = "Diferencia Rt CNRR-CM5 F. Cercano vs Historico RCP2.6", colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = 0, width = 30, nombre.fig = "Rt5.dif26_49", salida = "/Salidas/TP3/Rt/", label.escala = "W/m2")


aux = ComMT3(Rt5.26_99)
mapa_topo3(variable = aux[[1]] - aux1[[1]], variable.sig = aux[[2]], sig = T, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", escala = seq(-10, 10, by = 1)
           , niveles = 9, revert = T, color.vsig = "white", alpha.vsig = 1, titulo = "Diferencia Rt CNRR-CM5 F. Lejano vs Historico RCP2.6", colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = 0, width = 30, nombre.fig = "Rt5.dif26_99", salida = "/Salidas/TP3/Rt/", label.escala = "W/m2")


aux = ComMT3(Rt5.85_49)
mapa_topo3(variable = aux[[1]] - aux1[[1]], variable.sig = aux[[2]], sig = T, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", escala = seq(-10, 10, by = 1)
           , niveles = 9, revert = T, color.vsig = "white", alpha.vsig = 1, titulo = "Diferencia Rt CNRR-CM5 F. Cercano vs Historico RCP8.5", colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = 0, width = 30, nombre.fig = "Rt5.dif85_49", salida = "/Salidas/TP3/Rt/", label.escala = "W/m2")


aux = ComMT3(Rt5.85_99)
mapa_topo3(variable = aux[[1]] - aux1[[1]], variable.sig = aux[[2]], sig = T, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", escala = seq(-10, 10, by = 1)
           , niveles = 9, revert = T, color.vsig = "white", alpha.vsig = 1, titulo = "Diferencia Rt CNRR-CM5 F. Lejano vs Historico RCP8.5", colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = 0, width = 30, nombre.fig = "Rt5.dif85_99", salida = "/Salidas/TP3/Rt/", label.escala = "W/m2")

#--------------------------------------CNRM-CM6 -----------------------------------------------#
aux1 = aux = ComMT3(RT6)
mapa_topo3(variable = aux[[1]], variable.sig = mask, sig = T, lon = lon.obs, lat = lat.obs, colorbar = "YlOrRd", escala = seq(0, 200, by = 20)
           , niveles = 9, revert = F, color.vsig = "white", alpha.vsig = 1, titulo = "Rt CNRR-CM6 - Historico", colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = 0, width = 30, nombre.fig = "Rt6.his", salida = "/Salidas/TP3/Rt/", label.escala = "W/m2")



aux = ComMT3(RT6.26_49)
mapa_topo3(variable = aux[[1]] - aux1[[1]], variable.sig = aux[[2]], sig = T, lon = lon.obs, lat = lat.obs, colorbar = "YlOrRd", escala = seq(0, 20, by = 2.5)
           , niveles = 9, revert = F, color.vsig = "white", alpha.vsig = 1, titulo = "Diferencia Rt CNRR-CM6 F. Cercano vs Historico SSP126", colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = 0, width = 30, nombre.fig = "Rt6.dif26_49", salida = "/Salidas/TP3/Rt/", label.escala = "W/m2")


aux = ComMT3(RT6.26_99)
mapa_topo3(variable = aux[[1]] - aux1[[1]], variable.sig = aux[[2]], sig = T, lon = lon.obs, lat = lat.obs, colorbar = "YlOrRd", escala = seq(0, 20, by = 2.5)
           , niveles = 9, revert = F, color.vsig = "white", alpha.vsig = 1, titulo = "Diferencia Rt CNRR-CM6 F. Lejano vs Historico SSP126", colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = 0, width = 30, nombre.fig = "Rt6.dif26_99", salida = "/Salidas/TP3/Rt/", label.escala = "W/m2")


aux = ComMT3(RT6.85_49)
mapa_topo3(variable = aux[[1]] - aux1[[1]], variable.sig = aux[[2]], sig = T, lon = lon.obs, lat = lat.obs, colorbar = "YlOrRd", escala = seq(0, 20, by = 2.5)
           , niveles = 9, revert = F, color.vsig = "white", alpha.vsig = 1, titulo = "Diferencia Rt CNRR-CM6 F. Cercano vs Historico SSP585", colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = 0, width = 30, nombre.fig = "Rt6.dif85_49", salida = "/Salidas/TP3/Rt/", label.escala = "W/m2")


aux = ComMT3(RT6.85_99)
mapa_topo3(variable = aux[[1]] - aux1[[1]], variable.sig = aux[[2]], sig = T, lon = lon.obs, lat = lat.obs, colorbar = "YlOrRd", escala = seq(0, 100, by = 20)
           , niveles = 9, revert = F, color.vsig = "white", alpha.vsig = 1, titulo = "Diferencia Rt CNRR-CM6 F. Lejano vs Historico SSP585", colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = 0, width = 30, nombre.fig = "Rt6.dif85_99", salida = "/Salidas/TP3/Rt/", label.escala = "W/m2")
#----------------------------------------------------------------------------------------------------#

#### Corr ####

aux1 = ComMT3_2_2(RtH5.his)
mapa_topo3(variable = aux1[[1]], variable.sig = aux1[[2]], sig = T, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", escala = seq(-1, 1, by = 0.2)
           , niveles = 9, revert = T, color.vsig = "white", alpha.vsig = 1, titulo = "Correlación entre Rt y Entalpia CNRR-CM5 - Historico", colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = 0, width = 30, nombre.fig = "corr5RH", salida = "/Salidas/TP3/corr/", label.escala = "W/m2")



aux = ComMT3_2(RtH5.26_49)
mapa_topo3(variable = aux[[1]], variable.sig = aux[[2]], sig = T, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", escala = seq(-1, 1, by = 0.2)
           , niveles = 9, revert = T, color.vsig = "white", alpha.vsig = 1, titulo = "Correlación entre Rt y Entalpia CNRR-CM5 - F. Cercano RCP2.6", colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = 0, width = 30, nombre.fig = "corr5RH.26_49", salida = "/Salidas/TP3/corr/", label.escala = "W/m2")


aux = ComMT3_2(RtH5.26_99)
mapa_topo3(variable = aux[[1]], variable.sig = aux[[2]], sig = T, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", escala = seq(-1, 1, by = 0.2)
           , niveles = 9, revert = T, color.vsig = "white", alpha.vsig = 1, titulo = "Correlación entre Rt y Entalpia CNRR-CM5  - F. Lejano RCP2.6", colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = 0, width = 30, nombre.fig = "corr5RH.26_99", salida = "/Salidas/TP3/corr/", label.escala = "W/m2")


aux = ComMT3_2(RtH5.85_49)
mapa_topo3(variable = aux[[1]], variable.sig = aux[[2]], sig = T, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", escala = seq(-1, 1, by = 0.2)
           , niveles = 9, revert = T, color.vsig = "white", alpha.vsig = 1, titulo = "Correlación entre Rt y Entalpia CNRR-CM5 - F. Cercano RCP8.5", colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = 0, width = 30, nombre.fig = "corr5RH.85_49", salida = "/Salidas/TP3/corr/", label.escala = "W/m2")


aux = ComMT3_2(RtH5.85_99)
mapa_topo3(variable = aux[[1]], variable.sig = aux[[2]], sig = T, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", escala = seq(-1, 1, by = 0.2)
           , niveles = 9, revert = T, color.vsig = "white", alpha.vsig = 1, titulo = "Correlación entre Rt y Entalpia CNRR-CM5 - F. Lejano RCP8.5", colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = 0, width = 30, nombre.fig = "corr5RH.85_99", salida = "/Salidas/TP3/corr/", label.escala = "W/m2")

#--------------------------------------CNRM-CM6 -----------------------------------------------#
aux1 = aux = ComMT3_2(RtH6)
mapa_topo3(variable = aux[[1]], variable.sig = mask, sig = T, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", escala = seq(-1, 1, by = 0.2)
           , niveles = 9, revert = T, color.vsig = "white", alpha.vsig = 1, titulo = "Rt CNRR-CM6 - Historico", colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = 0, width = 30, nombre.fig = "corr6RH", salida = "/Salidas/TP3/corr/", label.escala = "W/m2")



aux = ComMT3_2(RtH6.26_49)
mapa_topo3(variable = aux[[1]], variable.sig = aux[[2]], sig = T, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", escala = seq(-1, 1, by = 0.2)
           , niveles = 9, revert = T, color.vsig = "white", alpha.vsig = 1, titulo = "Correlación entre Rt y Entalpia CNRR-CM6 - F. Cercano SSP126", colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = 0, width = 30, nombre.fig = "corr6RH.26_49", salida = "/Salidas/TP3/corr/", label.escala = "W/m2")


aux = ComMT3_2(RtH6.26_99)
mapa_topo3(variable = aux[[1]], variable.sig = aux[[2]], sig = T, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", escala = seq(-1, 1, by = 0.2)
           , niveles = 9, revert = T, color.vsig = "white", alpha.vsig = 1, titulo = "Correlación entre Rt y Entalpia CNRR-CM6 - F. Lejano SSP126", colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = 0, width = 30, nombre.fig = "corr6RH.26_99", salida = "/Salidas/TP3/corr/", label.escala = "W/m2")


aux = ComMT3_2(RtH6.85_49)
mapa_topo3(variable = aux[[1]], variable.sig = aux[[2]], sig = T, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", escala = seq(-1, 1, by = 0.2)
           , niveles = 9, revert = T, color.vsig = "white", alpha.vsig = 1, titulo = "Correlación entre Rt y Entalpia CNRR-CM6 - F. Cercano SSP585", colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = 0, width = 30, nombre.fig = "corr6RH.85_49", salida = "/Salidas/TP3/corr/", label.escala = "W/m2")


aux = ComMT3_2(RtH6.85_99)
mapa_topo3(variable = aux[[1]], variable.sig = aux[[2]], sig = T, lon = lon.obs, lat = lat.obs, colorbar = "RdBu", escala = seq(-1, 1, by = 0.2)
           , niveles = 9, revert = T, color.vsig = "white", alpha.vsig = 1, titulo = "Correlación entre Rt y Entalpia CNRR-CM6 - F. Lejano SSP585", colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = 0, width = 30, nombre.fig = "corr6RH.85_99", salida = "/Salidas/TP3/corr/", label.escala = "W/m2")
#----------------------------------------------------------------------------------------------------#






#### Entalpia5  y LH , SH #####
aux1 = H.his_an[[1]]; titulo = "Entalpia CNRM-CM5 Historico"; nombre.fig = "H5.his"; escala = seq(0,70, by = 10)
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "_total", sep = ""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")
aux1 = H.his_an[[2]]; titulo = "SH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "SH", sep =""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")
aux1 = H.his_an[[3]]; titulo = "LH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "LH", sep = ""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")



aux1 = H5.26_49_an[[1]]; titulo = "Entalpia CNRM-CM5 2020-2049 - RCP2.6"
nombre.fig = "H5.26_49"; escala = seq(0,70, by = 10)
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "_total", sep = ""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")
aux1 = H5.26_49_an[[2]]; titulo = "SH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "SH", sep =""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")
aux1 = H5.26_49_an[[3]]; titulo = "LH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "LH", sep = ""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")



aux1 = H5.26_99_an[[1]]; titulo = "Entalpia CNRM-CM5 2070-2099 RCP2.6"
nombre.fig = "H5.26_99"; escala = seq(0,70, by = 10)
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "_total", sep = ""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")
aux1 = H5.26_99_an[[2]]; titulo = "SH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "SH", sep =""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")
aux1 = H5.26_99_an[[3]]; titulo = "LH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "LH", sep = ""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")



aux1 = H5.85_49_an[[1]]; titulo = "Entalpia CNRM-CM5 2020-2049 - RCP8.5"
nombre.fig = "H5.85_49"; escala = seq(0,70, by = 10)
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "_total", sep = ""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")
aux1 = H5.85_49_an[[2]]; titulo = "SH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "SH", sep =""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")
aux1 = H5.85_49_an[[3]]; titulo = "LH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "LH", sep = ""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")



aux1 = H5.85_99_an[[1]]; titulo = "Entalpia CNRM-CM5 2070-2099 RCP8.5"
nombre.fig = "H5.85_99"; escala = seq(0,70, by = 10)
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "_total", sep = ""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")
aux1 = H5.85_99_an[[2]]; titulo = "SH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "SH", sep =""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")
aux1 = H5.85_99_an[[3]]; titulo = "LH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "LH", sep = ""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")
#### Entalpia6 y SH y LH #####
aux1= H.his_an[[1]]; titulo = "Entalpia CNRM-CM6 Historico"; nombre.fig = "H6.his"; escala = seq(0,70, by = 10)
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "_total", sep = ""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")
aux1= H.his_an[[2]]; titulo = "SH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "SH", sep =""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")
aux1= H.his_an[[3]]; titulo = "LH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "LH", sep = ""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")



aux1= H.26_49_an[[1]]; titulo = "Entalpia CNRM-CM6 2020-2049 - SSP126"
nombre.fig = "H6.26_49"; escala = seq(0,70, by = 10)
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "_total", sep = ""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")
aux1= H.26_49_an[[2]]; titulo = "SH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "SH", sep =""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")
aux1= H.26_49_an[[3]]; titulo = "LH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "LH", sep = ""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")



aux1= H.26_99_an[[1]]; titulo = "Entalpia CNRM-CM6 2070-2099 SSP126"
nombre.fig = "H6.26_99"; escala = seq(0,70, by = 10)
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "_total", sep = ""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")
aux1= H.26_99_an[[2]]; titulo = "SH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "SH", sep =""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")
aux1= H.26_99_an[[3]]; titulo = "LH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "LH", sep = ""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")



aux1= H.85_49_an[[1]]; titulo = "Entalpia CNRM-CM6 2020-2049 - SSP585"
nombre.fig = "H6.85_49"; escala = seq(0,70, by = 10)
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "_total", sep = ""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")
aux1= H.85_49_an[[2]]; titulo = "SH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "SH", sep =""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")
aux1= H.85_49_an[[3]]; titulo = "LH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "LH", sep = ""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")



aux1= H.85_99_an[[1]]; titulo = "Entalpia CNRM-CM6 2070-2099 SSP585"
nombre.fig = "H6.85_99"; escala = seq(0,70, by = 10)
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "_total", sep = ""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")
aux1= H.85_99_an[[2]]; titulo = "SH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "SH", sep =""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")
aux1= H.85_99_an[[3]]; titulo = "LH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "LH", sep = ""), salida = "/Salidas/TP3/H/", label.escala = "kJ/kg")
##### entalpia 6 seasons campos ####


aux1= H.26_49_seasons[[1]]; titulo = "Entalpia CNRM-CM6 2020-2049 - SSP126"
nombre.fig = "H6.26_49"; escala = seq(0,70, by = 10)
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "_total", sep = ""), salida = "/Salidas/TP3/H_seas/", label.escala = "kJ/kg", r = 4)
aux1= H.26_49_seasons[[2]]; titulo = "SH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "SH", sep =""), salida = "/Salidas/TP3/H_seas/", label.escala = "kJ/kg", r = 4)
aux1= H.26_49_seasons[[3]]; titulo = "LH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "LH", sep = ""), salida = "/Salidas/TP3/H_seas/", label.escala = "kJ/kg", r = 4)



aux1= H.26_99_seasons[[1]]; titulo = "Entalpia CNRM-CM6 2070-2099 SSP126"
nombre.fig = "H6.26_99"; escala = seq(0,70, by = 10)
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "_total", sep = ""), salida = "/Salidas/TP3/H_seas/", label.escala = "kJ/kg", r = 4)
aux1= H.26_99_seasons[[2]]; titulo = "SH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "SH", sep =""), salida = "/Salidas/TP3/H_seas/", label.escala = "kJ/kg", r = 4)
aux1= H.26_99_seasons[[3]]; titulo = "LH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "LH", sep = ""), salida = "/Salidas/TP3/H_seas/", label.escala = "kJ/kg", r = 4)



aux1= H.85_49_seasons[[1]]; titulo = "Entalpia CNRM-CM6 2020-2049 - SSP585"
nombre.fig = "H6.85_49"; escala = seq(0,70, by = 10)
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "_total", sep = ""), salida = "/Salidas/TP3/H_seas/", label.escala = "kJ/kg", r = 4)
aux1= H.85_49_seasons[[2]]; titulo = "SH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "SH", sep =""), salida = "/Salidas/TP3/H_seas/", label.escala = "kJ/kg", r = 4)
aux1= H.85_49_seasons[[3]]; titulo = "LH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "LH", sep = ""), salida = "/Salidas/TP3/H_seas/", label.escala = "kJ/kg", r = 4)



aux1= H.85_99_seasons[[1]]; titulo = "Entalpia CNRM-CM6 2070-2099 SSP585"
nombre.fig = "H6.85_99"; escala = seq(0,70, by = 10)
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "_total", sep = ""), salida = "/Salidas/TP3/H_seas/", label.escala = "kJ/kg", r = 4)
aux1= H.85_99_seasons[[2]]; titulo = "SH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "SH", sep =""), salida = "/Salidas/TP3/H_seas/", label.escala = "kJ/kg", r = 4)
aux1= H.85_99_seasons[[3]]; titulo = "LH"
mapa_topo3(variable = aux1, lon = lon.obs, lat = rev(lat.obs), colorbar = "YlOrRd", escala = escala
           , niveles = 9, revert = F, titulo = titulo, colorbar.pos = "bottom"
           , mapa = "mundo", na.fill = -1000, width = 30, nombre.fig = paste(nombre.fig, "LH", sep = ""), salida = "/Salidas/TP3/H_seas/", label.escala = "kJ/kg", r = 4)


##### Distribucion zonal de lo de arriba ####
titulo = "Promedio Zonal"
estaciones = c("MAM", "JJA", "SON", "DJF")

for(i in 1:4){
  HLatMean2(serie1 = H.his_seasonsZonal[,i], serie2 = H.his_seasonsZonal2[,i], serie3 = H.his_seasonsZonal3[,i], 
            lat = rev(lat.obs), titulo = paste(titulo, " CNRM-CM5 - Historico - ", estaciones[i], sep = ""), nombre.fig = paste("H6.his_", estaciones[i], sep = ""))
  
  HLatMean2(serie1 = H.26_49_seasonsZonal[,i], serie2 = H.26_49_seasonsZonal2[,i], serie3 = H.26_49_seasonsZonal3[,i], 
            lat = rev(lat.obs), titulo = paste(titulo, " CNRM-CM5 SSP126 - 2020-2049 - ", estaciones[i], sep = ""), nombre.fig = paste("H6.26_49_", estaciones[i], sep = ""))
  
  HLatMean2(serie1 = H.26_99_seasonsZonal[,i], serie2 = H.26_99_seasonsZonal2[,i], serie3 = H.26_99_seasonsZonal3[,i], 
            lat = rev(lat.obs), titulo = paste(titulo, " CNRM-CM5 SSP126 - 2070-2099 - ", estaciones[i], sep = ""), nombre.fig = paste("H6.26_99_", estaciones[i], sep = ""))
  
  HLatMean2(serie1 = H.85_49_seasonsZonal[,i], serie2 = H.26_49_seasonsZonal2[,i], serie3 = H.85_49_seasonsZonal3[,i], 
            lat = rev(lat.obs), titulo = paste(titulo, " CNRM-CM5 SSP585 - 2020-2049 - ", estaciones[i], sep = ""), nombre.fig = paste("H6.85_49_", estaciones[i], sep = ""))
  
  HLatMean2(serie1 = H.85_99_seasonsZonal[,i], serie2 = H.26_99_seasonsZonal2[,i], serie3 = H.85_99_seasonsZonal3[,i], 
            lat = rev(lat.obs), titulo = paste(titulo, " CNRM-CM5 SSP585 - 2070-2099 - ", estaciones[i], sep = ""), nombre.fig = paste("H6.85_99_", estaciones[i], sep = ""))
  
}


##### "Marcha anual" promedio ####
h = list(); h[[1]] = seq(1,37, by = 1); h[[2]] = seq(37, 73, by = 1)
H = c("H. Sur", "H. Norte")
H.fig = c("HS", "HN")

for(i in 1:2){
  
  HLatMean3(serie1 = apply(H.his[[4]][,h[[i]],],c(3), mean, na.rm = T), serie2 = apply(H.his[[5]][,h[[i]],], c(3), mean, na.rm = T)
            , serie3 = apply(H.his[[6]][,h[[i]],], c(3),mean, na.rm = T), lat = seq(1, 12, by = 1)
            , titulo = paste("CNRM-CM6 SSP126 - 1976 - 2005 ", H[i], sep = ""),nombre.fig = paste("H.m.a_his_", H.fig[i], sep = ""))
  
  HLatMean3(serie1 = apply(H.26_49[[4]][,h[[i]],],c(3), mean, na.rm = T), serie2 = apply(H.26_49[[5]][,h[[i]],], c(3), mean, na.rm = T)
            , serie3 = apply(H.26_49[[6]][,h[[i]],], c(3),mean, na.rm = T), lat = seq(1, 12, by = 1)
            , titulo = paste("CNRM-CM6 SSP126 - 2020 - 2049 ", H[i], sep = ""),nombre.fig = paste("H.m.a_26_49_", H.fig[i], sep = ""))
  
  HLatMean3(serie1 = apply(H.26_99[[4]][,h[[i]],],c(3), mean, na.rm = T), serie2 = apply(H.26_99[[5]][,h[[i]],], c(3), mean, na.rm = T)
            , serie3 = apply(H.26_99[[6]][,h[[i]],], c(3),mean, na.rm = T), lat = seq(1, 12, by = 1)
            , titulo = paste("CNRM-CM6 SSP126 - 2070 - 2099 ", H[i], sep = ""),nombre.fig = paste("H.m.a_26_99_", H.fig[i], sep = ""))
  
  
  HLatMean3(serie1 = apply(H.85_49[[4]][,h[[i]],],c(3), mean, na.rm = T), serie2 = apply(H.85_49[[5]][,h[[i]],], c(3), mean, na.rm = T)
            , serie3 = apply(H.85_49[[6]][,h[[i]],], c(3),mean, na.rm = T), lat = seq(1, 12, by = 1)
            , titulo = paste("CNRM-CM6 SSP585 - 2020 - 2049 ", H[i], sep = ""),nombre.fig = paste("H.m.a_85_49_", H.fig[i], sep = ""))
  
  
  HLatMean3(serie1 = apply(H.85_99[[4]][,h[[i]],],c(3), mean, na.rm = T), serie2 = apply(H.85_99[[5]][,h[[i]],], c(3), mean, na.rm = T)
            , serie3 = apply(H.85_99[[6]][,h[[i]],], c(3),mean, na.rm = T), lat = seq(1, 12, by = 1)
            , titulo = paste("CNRM-CM6 SSP585 - 2070 - 2099 ", H[i], sep = ""),nombre.fig = paste("H.m.a_85_99_", H.fig[i], sep = ""))
}


