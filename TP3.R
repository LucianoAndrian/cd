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

t.tend_an = Tendencia(apply(t.obs_anual.mean, c(3), mean, na.rm = T))
t.tend_anS = Tendencia(apply(t.obs_anual.mean[,2:37,], c(3), mean, na.rm = T))
t.tend_anN = Tendencia(apply(t.obs_anual.mean[,37:73,], c(3), mean, na.rm = T))

q.tend_an = Tendencia(apply(q.obs_anual.mean, c(3), mean, na.rm = T))
q.tend_anS = Tendencia(apply(q.obs_anual.mean[,1:37,], c(3), mean, na.rm = T))
q.tend_anN = Tendencia(apply(q.obs_anual.mean[,37:73,], c(3), mean, na.rm = T))

#--- mods ---# 
#### CNRM-CM5 #####
#------------------------------------------------------- HISTORICAL ------------------------------------------------------#
load("RDatas/TP3.RDatas/t5.his.RData"); load("RDatas/TP3.RDatas/hu5.his.RData"); load("RDatas/TP3.RDatas/etp5.his.RData")
#-------------------------------------------------------------------------------------------------------------------------#

t5.hisG_tend_an = Tendencia(apply(t5.his[[1]], c(3), mean, na.rm = T))
t5.hisN_tend_an = Tendencia(apply(t5.his[[1]][,37:73,,], c(3), mean, na.rm = T))
t5.hisS_tend_an = Tendencia(apply(t5.his[[1]][,1:37,,], c(3), mean, na.rm = T))

q5.hisG_tend_an = Tendencia(apply(hu5.his[[1]], c(3), mean, na.rm = T))
q5.hisN_tend_an = Tendencia(apply(hu5.his[[1]][,37:73,,], c(3), mean, na.rm = T))
q5.hisS_tend_an = Tendencia(apply(hu5.his[[1]][,1:37,,], c(3), mean, na.rm = T))

etp5.hisG_tend_an = Tendencia(apply(etp5.his[[1]], c(3), mean, na.rm = T))
etp5.hisN_tend_an = Tendencia(apply(etp5.his[[1]][,37:73,,], c(3), mean, na.rm = T))
etp5.hisS_tend_an = Tendencia(apply(etp5.his[[1]][,1:37,,], c(3), mean, na.rm = T))

#-------------------------------------------------------------------------------------------------------------------------#
rm(t5.his, hu5.his, etp5.his)
#-------------------------------------------------------------------------------------------------------------------------#

#------------------------------------------------------- RCP26 2020 2049--------------------------------------------------#
load("RDatas/TP3.RDatas/t5.26_49.RData"); load("RDatas/TP3.RDatas/hu5.26_49.RData")
load("RDatas/TP3.RDatas/etp5.26_49.RData")
#-------------------------------------------------------------------------------------------------------------------------#

t5.26_49G_tend_an = Tendencia(apply(t5.26_49[[1]], c(3), mean, na.rm = T))
t5.26_49N_tend_an = Tendencia(apply(t5.26_49[[1]][,37:73,,], c(3), mean, na.rm = T))
t5.26_49S_tend_an = Tendencia(apply(t5.26_49[[1]][,1:37,,], c(3), mean, na.rm = T))

q5.26_49G_tend_an = Tendencia(apply(hu5.26_49[[1]], c(3), mean, na.rm = T))
q5.26_49N_tend_an = Tendencia(apply(hu5.26_49[[1]][,37:73,,], c(3), mean, na.rm = T))
q5.26_49S_tend_an = Tendencia(apply(hu5.26_49[[1]][,1:37,,], c(3), mean, na.rm = T))

etp5.26_49G_tend_an = Tendencia(apply(etp5.26_49[[1]], c(3), mean, na.rm = T))
etp5.26_49N_tend_an = Tendencia(apply(etp5.26_49[[1]][,37:73,,], c(3), mean, na.rm = T))
etp5.26_49S_tend_an = Tendencia(apply(etp5.26_49[[1]][,1:37,,], c(3), mean, na.rm = T))
#-------------------------------------------------------------------------------------------------------------------------#
rm(t5.26_49, hu5.26_49, etp5.26_49)
#-------------------------------------------------------------------------------------------------------------------------#

#.rs.restartR() <- ACA ESTA EL COSO PARA RESETEAR!!!

#------------------------------------------------------- RCP26 2070 2099--------------------------------------------------#
load("RDatas/TP3.RDatas/t5.26_99.RData"); load("RDatas/TP3.RDatas/hu5.26_99.RData")
load("RDatas/TP3.RDatas/etp5.26_99.RData")
#-------------------------------------------------------------------------------------------------------------------------#

t5.26_99G_tend_an = Tendencia(apply(t5.26_99[[1]], c(3), mean, na.rm = T))
t5.26_99N_tend_an = Tendencia(apply(t5.26_99[[1]][,37:73,,], c(3), mean, na.rm = T))
t5.26_99S_tend_an = Tendencia(apply(t5.26_99[[1]][,1:37,,], c(3), mean, na.rm = T))

q5.26_99G_tend_an = Tendencia(apply(hu5.26_99[[1]], c(3), mean, na.rm = T))
q5.26_99N_tend_an = Tendencia(apply(hu5.26_99[[1]][,37:73,,], c(3), mean, na.rm = T))
q5.26_99S_tend_an = Tendencia(apply(hu5.26_99[[1]][,1:37,,], c(3), mean, na.rm = T))

etp5.26_99G_tend_an = Tendencia(apply(etp5.26_99[[1]], c(3), mean, na.rm = T))
etp5.26_99N_tend_an = Tendencia(apply(etp5.26_99[[1]][,37:73,,], c(3), mean, na.rm = T))
etp5.26_99S_tend_an = Tendencia(apply(etp5.26_99[[1]][,1:37,,], c(3), mean, na.rm = T))
#-------------------------------------------------------------------------------------------------------------------------#
rm(t5.26_99, hu5.26_99, etp5.26_99)
#-------------------------------------------------------------------------------------------------------------------------#

#------------------------------------------------------- RCP85 2020 2049--------------------------------------------------#
load("RDatas/TP3.RDatas/t5.85_49.RData"); load("RDatas/TP3.RDatas/hu5.85_49.RData")
load("RDatas/TP3.RDatas/etp5.85_49.RData")
#-------------------------------------------------------------------------------------------------------------------------#

t5.85_49G_tend_an = Tendencia(apply(t5.85_49[[1]], c(3), mean, na.rm = T))
t5.85_49N_tend_an = Tendencia(apply(t5.85_49[[1]][,37:73,,], c(3), mean, na.rm = T))
t5.85_49S_tend_an = Tendencia(apply(t5.85_49[[1]][,1:37,,], c(3), mean, na.rm = T))

q5.85_49G_tend_an = Tendencia(apply(hu5.85_49[[1]], c(3), mean, na.rm = T))
q5.85_49N_tend_an = Tendencia(apply(hu5.85_49[[1]][,37:73,,], c(3), mean, na.rm = T))
q5.85_49S_tend_an = Tendencia(apply(hu5.85_49[[1]][,1:37,,], c(3), mean, na.rm = T))

etp5.85_49G_tend_an = Tendencia(apply(etp5.85_49[[1]], c(3), mean, na.rm = T))
etp5.85_49N_tend_an = Tendencia(apply(etp5.85_49[[1]][,37:73,,], c(3), mean, na.rm = T))
etp5.85_49S_tend_an = Tendencia(apply(etp5.85_49[[1]][,1:37,,], c(3), mean, na.rm = T))

#-------------------------------------------------------------------------------------------------------------------------#
rm(t5.85_49, hu5.85_49, etp5.85_49)
#-------------------------------------------------------------------------------------------------------------------------#

#------------------------------------------------------- RCP85 2070 2099--------------------------------------------------#
load("RDatas/TP3.RDatas/t5.85_99.RData"); load("RDatas/TP3.RDatas/hu5.85_99.RData")
load("RDatas/TP3.RDatas/etp5.85_99.RData")
#-------------------------------------------------------------------------------------------------------------------------#

t5.85_99G_tend_an = Tendencia(apply(t5.85_99[[1]], c(3), mean, na.rm = T))
t5.85_99N_tend_an = Tendencia(apply(t5.85_99[[1]][,37:73,,], c(3), mean, na.rm = T))
t5.85_99S_tend_an = Tendencia(apply(t5.85_99[[1]][,1:37,,], c(3), mean, na.rm = T))

q5.85_99G_tend_an = Tendencia(apply(hu5.85_99[[1]], c(3), mean, na.rm = T))
q5.85_99N_tend_an = Tendencia(apply(hu5.85_99[[1]][,37:73,,], c(3), mean, na.rm = T))
q5.85_99S_tend_an = Tendencia(apply(hu5.85_99[[1]][,1:37,,], c(3), mean, na.rm = T))

etp5.85_99G_tend_an = Tendencia(apply(etp5.85_99[[1]], c(3), mean, na.rm = T))
etp5.85_99N_tend_an = Tendencia(apply(etp5.85_99[[1]][,37:73,,], c(3), mean, na.rm = T))
etp5.85_99S_tend_an = Tendencia(apply(etp5.85_99[[1]][,1:37,,], c(3), mean, na.rm = T))

#-------------------------------------------------------------------------------------------------------------------------#
rm(t5.85_99, hu5.85_99, etp5.85_99)
#-------------------------------------------------------------------------------------------------------------------------#

#.rs.restartR() <- ACA ESTA EL COSO PARA RESETEAR!!!

#### CNRM-CM6 ####
#------------------------------------------------------- HISTORICAL ------------------------------------------------------#
load("RDatas/TP3.RDatas/t6.his.RData"); load("RDatas/TP3.RDatas/hu6.his.RData"); load("RDatas/TP3.RDatas/etp6.his_an.RData")
t6.his = AnualMean(apply(t6.his[[3]], c(1,2,3), mean, na.rm = T))
hu6.his = AnualMean(apply(hu6.his[[3]], c(1,2,3), mean, na.rm = T))
#-------------------------------------------------------------------------------------------------------------------------#

t6.hisG_tend_an = Tendencia(apply(t6.his, c(3), mean, na.rm = T))
t6.hisN_tend_an = Tendencia(apply(t6.his[,37:73,], c(3), mean, na.rm = T))
t6.hisS_tend_an = Tendencia(apply(t6.his[,1:37,], c(3), mean, na.rm = T))

q6.hisG_tend_an = Tendencia(apply(hu6.his, c(3), mean, na.rm = T))
q6.hisN_tend_an = Tendencia(apply(hu6.his[,37:73,], c(3), mean, na.rm = T))
q6.hisS_tend_an = Tendencia(apply(hu6.his[,1:37,], c(3), mean, na.rm = T))

etp6.hisG_tend_an = Tendencia(apply(etp6.his_an[[1]], c(3), mean, na.rm = T))
etp6.hisN_tend_an = Tendencia(apply(etp6.his_an[[1]][,37:73,,], c(3), mean, na.rm = T))
etp6.hisS_tend_an = Tendencia(apply(etp6.his_an[[1]][,1:37,,], c(3), mean, na.rm = T))

#-------------------------------------------------------------------------------------------------------------------------#
rm(t6.his,hu6.his,etp6.his_an)
#-------------------------------------------------------------------------------------------------------------------------#

#------------------------------------------------------- SSP26 2020 2049--------------------------------------------------#
load("RDatas/TP3.RDatas/t6.26_49.RData"); load("RDatas/TP3.RDatas/hu6.26_49.RData")
load("RDatas/TP3.RDatas/etp6.26_49.RData")
t6.26_2049 = AnualMean(apply(t6.26_2049[[3]], c(1,2,3), mean, na.rm = T))
hu6.26_2049 = AnualMean(apply(hu6.26_2049[[3]], c(1,2,3), mean, na.rm = T))

#-------------------------------------------------------------------------------------------------------------------------#

t6.26_49G_tend_an = Tendencia(apply(t6.26_2049, c(3), mean, na.rm = T))
t6.26_49N_tend_an = Tendencia(apply(t6.26_2049[,37:73,], c(3), mean, na.rm = T))
t6.26_49S_tend_an = Tendencia(apply(t6.26_2049[,1:37,], c(3), mean, na.rm = T))

q6.26_49G_tend_an = Tendencia(apply(hu6.26_2049, c(3), mean, na.rm = T))
q6.26_49N_tend_an = Tendencia(apply(hu6.26_2049[,37:73,], c(3), mean, na.rm = T))
q6.26_49S_tend_an = Tendencia(apply(hu6.26_2049[,1:37,], c(3), mean, na.rm = T))

etp6.26_49G_tend_an = Tendencia(apply(etp6.26_49[[1]], c(3), mean, na.rm = T))
etp6.26_49N_tend_an = Tendencia(apply(etp6.26_49[[1]][,37:73,,], c(3), mean, na.rm = T))
etp6.26_49S_tend_an = Tendencia(apply(etp6.26_49[[1]][,1:37,,], c(3), mean, na.rm = T))
#-------------------------------------------------------------------------------------------------------------------------#
rm(t6.26_2049, hu6.26_2049, etp6.26_49)
#-------------------------------------------------------------------------------------------------------------------------#



#------------------------------------------------------- SSP26 2070 2099--------------------------------------------------#
load("RDatas/TP3.RDatas/t6.26_99.RData"); load("RDatas/TP3.RDatas/hu6.26_99.RData")
load("RDatas/TP3.RDatas/etp6.26_99.RData")
t6.26_2099 = AnualMean(apply(t6.26_2099[[3]], c(1,2,3), mean, na.rm = T))
hu6.26_2099 = AnualMean(apply(hu6.26_2099[[3]], c(1,2,3), mean, na.rm = T))
#-------------------------------------------------------------------------------------------------------------------------#

t6.26_99G_tend_an = Tendencia(apply(t6.26_2099, c(3), mean, na.rm = T))
t6.26_99N_tend_an = Tendencia(apply(t6.26_2099[,37:73,], c(3), mean, na.rm = T))
t6.26_99S_tend_an = Tendencia(apply(t6.26_2099[,1:37,], c(3), mean, na.rm = T))

q6.26_99G_tend_an = Tendencia(apply(hu6.26_2099, c(3), mean, na.rm = T))
q6.26_99N_tend_an = Tendencia(apply(hu6.26_2099[,37:73,], c(3), mean, na.rm = T))
q6.26_99S_tend_an = Tendencia(apply(hu6.26_2099[,1:37,], c(3), mean, na.rm = T))

etp6.26_99G_tend_an = Tendencia(apply(etp6.26_99[[1]], c(3), mean, na.rm = T))
etp6.26_99N_tend_an = Tendencia(apply(etp6.26_99[[1]][,37:73,,], c(3), mean, na.rm = T))
etp6.26_99S_tend_an = Tendencia(apply(etp6.26_99[[1]][,1:37,,], c(3), mean, na.rm = T))
#-------------------------------------------------------------------------------------------------------------------------#
rm(t6.26_2099, hu6.26_2099, etp6.26_99)
#-------------------------------------------------------------------------------------------------------------------------#

#------------------------------------------------------- SSP85 2020 2049--------------------------------------------------#
load("RDatas/TP3.RDatas/t6.85_49.RData"); load("RDatas/TP3.RDatas/hu6.85_49.RData")
load("RDatas/TP3.RDatas/etp6.85_49.RData")
t6.85_2049 = AnualMean(apply(t6.85_2049[[3]], c(1,2,3), mean, na.rm = T))
hu6.85_2049 = AnualMean(apply(hu6.85_2049[[3]], c(1,2,3), mean, na.rm = T))
#-------------------------------------------------------------------------------------------------------------------------#

t6.85_49G_tend_an = Tendencia(apply(t6.85_2049, c(3), mean, na.rm = T))
t6.85_49N_tend_an = Tendencia(apply(t6.85_2049[,37:73,], c(3), mean, na.rm = T))
t6.85_49S_tend_an = Tendencia(apply(t6.85_2049[,1:37,], c(3), mean, na.rm = T))

q6.85_49G_tend_an = Tendencia(apply(hu6.85_2049, c(3), mean, na.rm = T))
q6.85_49N_tend_an = Tendencia(apply(hu6.85_2049[,37:73,], c(3), mean, na.rm = T))
q6.85_49S_tend_an = Tendencia(apply(hu6.85_2049[,1:37,], c(3), mean, na.rm = T))

etp6.85_49G_tend_an = Tendencia(apply(etp6.85_49[[1]], c(3), mean, na.rm = T))
etp6.85_49N_tend_an = Tendencia(apply(etp6.85_49[[1]][,37:73,,], c(3), mean, na.rm = T))
etp6.85_49S_tend_an = Tendencia(apply(etp6.85_49[[1]][,1:37,,], c(3), mean, na.rm = T))

#-------------------------------------------------------------------------------------------------------------------------#
rm(t6.85_2049, hu6.85_2049, etp6.85_49)
#-------------------------------------------------------------------------------------------------------------------------#

#------------------------------------------------------- SSP85 2070 2099--------------------------------------------------#
load("RDatas/TP3.RDatas/t6.85_99.RData"); load("RDatas/TP3.RDatas/hu6.85_99.RData")
load("RDatas/TP3.RDatas/etp6.85_99.RData")
t6.85_2099 = AnualMean(apply(t6.85_2099[[3]], c(1,2,3), mean, na.rm = T))
hu6.85_2099 = AnualMean(apply(hu6.85_2099[[3]], c(1,2,3), mean, na.rm = T))
#-------------------------------------------------------------------------------------------------------------------------#

t6.85_99G_tend_an = Tendencia(apply(t6.85_2099, c(3), mean, na.rm = T))
t6.85_99N_tend_an = Tendencia(apply(t6.85_2099[,37:73,], c(3), mean, na.rm = T))
t6.85_99S_tend_an = Tendencia(apply(t6.85_2099[,1:37,], c(3), mean, na.rm = T))

q6.85_99G_tend_an = Tendencia(apply(hu6.85_2099, c(3), mean, na.rm = T))
q6.85_99N_tend_an = Tendencia(apply(hu6.85_2099[,37:73,], c(3), mean, na.rm = T))
q6.85_99S_tend_an = Tendencia(apply(hu6.85_2099[,1:37,], c(3), mean, na.rm = T))

etp6.85_99G_tend_an = Tendencia(apply(etp6.85_99[[1]], c(3), mean, na.rm = T))
etp6.85_99N_tend_an = Tendencia(apply(etp6.85_99[[1]][,37:73,,], c(3), mean, na.rm = T))
etp6.85_99S_tend_an = Tendencia(apply(etp6.85_99[[1]][,1:37,,], c(3), mean, na.rm = T))

#-------------------------------------------------------------------------------------------------------------------------#
rm(t6.85_2099, hu6.85_2099, etp6.85_99)
#-------------------------------------------------------------------------------------------------------------------------#




#### Graficos Tendencia ####
library(ggplot2)
#--- OBSERVADO ----#

PlotTsTend(global = t.tend_an, hn = t.tend_anN, hs = t.tend_anS, titulo = "T Observada", y.label = "ºC", y.breaks = seq(2, 8, by = 1), nombre.fig = "t.obs_tend")
PlotTsTend(global = q.tend_an, hn = q.tend_anN, hs = q.tend_anS, titulo = "q Observada", y.label = "Kg/Kg", y.breaks = seq(0.000103, 0.000108, by = 0.000001), nombre.fig = "q.obs_tend")


#---- CNRCM-CM5 ----#
# temp
PlotTsTend(global = t5.hisG_tend_an, hn = t5.hisN_tend_an, hs = t5.hisS_tend_an, titulo = "T CNRM-CM5 - Historico", y.label = "ºC", y.breaks = seq(2, 14, by = 1), nombre.fig = "t5.his_tend", cent = T, anios = c(1976, 2005))
PlotTsTend(global = t5.26_49G_tend_an, hn = t5.26_49N_tend_an, hs = t5.26_49S_tend_an, titulo = "T CNRM-CM5 - RCP26 2020 - 2050", y.label = "ºC", y.breaks = seq(2, 14, by = 1), nombre.fig = "t5.26_49", cent = T, anios = c(2020, 2049))
PlotTsTend(global = t5.26_99G_tend_an, hn = t5.26_49N_tend_an, hs = t5.26_99S_tend_an, titulo = "T CNRM-CM5 - RCP26 2070 - 2100", y.label = "ºC", y.breaks = seq(2, 14, by = 1), nombre.fig = "t5.26_99", cent = T, anios = c(2020, 2049))
PlotTsTend(global = t5.85_49G_tend_an, hn = t5.85_49N_tend_an, hs = t5.85_49S_tend_an, titulo = "T CNRM-CM5 - RCP85 2020 - 2050", y.label = "ºC", y.breaks = seq(2, 14, by = 1), nombre.fig = "t5.85_49", cent = T, anios = c(2070, 2099))
PlotTsTend(global = t5.85_99G_tend_an, hn = t5.85_99N_tend_an, hs = t5.85_99S_tend_an, titulo = "T CNRM-CM5 - RCP85 2070 - 2100", y.label = "ºC", y.breaks = seq(2, 14, by = 1), nombre.fig = "t5.85_99", cent = T, anios = c(2070, 2099))

# q
PlotTsTend(global = q5.hisG_tend_an, hn = q5.hisN_tend_an, hs = q5.hisS_tend_an, titulo = "q CNRM-CM5 - Historico", y.label = "Kg/Kg", y.breaks = seq(0.007, 0.01, by = 0.0001), nombre.fig = "q5.his_tend", cent = F, anios = c(1976, 2005))
PlotTsTend(global = q5.26_49G_tend_an, hn = q5.26_49N_tend_an, hs = q5.26_49S_tend_an, titulo = "q CNRM-CM5 - RCP26 2020 - 2050", y.label = "Kg/Kg", y.breaks = seq(0.007, 0.01, by = 0.0001), nombre.fig = "q5.26_49", cent = F, anios = c(2020, 2049))
PlotTsTend(global = q5.26_99G_tend_an, hn = q5.26_99N_tend_an, hs = q5.26_99S_tend_an, titulo = "q CNRM-CM5 - RCP26 2070 - 2100", y.label = "Kg/Kg", y.breaks = seq(0.007, 0.01, by = 0.0001), nombre.fig = "q5.26_99", cent = F, anios = c(2020, 2049))
PlotTsTend(global = q5.85_49G_tend_an, hn = q5.85_49N_tend_an, hs = q5.85_49S_tend_an, titulo = "q CNRM-CM5 - RCP85 2020 - 2050", y.label = "Kg/Kg", y.breaks = seq(0.007, 0.01, by = 0.0001), nombre.fig = "q5.85_49", cent = F, anios = c(2070, 2099))
PlotTsTend(global = q5.85_99G_tend_an, hn = q5.85_99N_tend_an, hs = q5.85_99S_tend_an, titulo = "q CNRM-CM5 - RCP85 2070 - 2100", y.label = "Kg/Kg", y.breaks = seq(0.007, 0.01, by = 0.0002), nombre.fig = "q5.85_99", cent = F, anios = c(2070, 2099))

#--- CNRM-CM6 ---#
PlotTsTend(global = t6.hisG_tend_an, hn = t6.hisN_tend_an, hs = t6.hisS_tend_an, titulo = "T CNRM-CM6 - Historico", y.label = "ºC", y.breaks = seq(2, 14, by = 1), nombre.fig = "t6.his_tend", cent = T, anios = c(1976, 2005))
PlotTsTend(global = t6.26_49G_tend_an, hn = t6.26_49N_tend_an, hs = t6.26_49S_tend_an, titulo = "T CNRM-CM6 - SSP126 2020 - 2050", y.label = "ºC", y.breaks = seq(2, 14, by = 1), nombre.fig = "t6.26_49", cent = T, anios = c(2020, 2049))
PlotTsTend(global = t6.26_99G_tend_an, hn = t6.26_99N_tend_an, hs = t6.26_99S_tend_an, titulo = "T CNRM-CM6 - SSP126 2070 - 2100", y.label = "ºC", y.breaks = seq(2, 14, by = 1), nombre.fig = "t6.26_99", cent = T,anios = c(2020, 2049))
PlotTsTend(global = t6.85_49G_tend_an, hn = t6.85_49N_tend_an, hs = t6.85_49S_tend_an, titulo = "T CNRM-CM6 - SSP858 2020 - 2050", y.label = "ºC", y.breaks = seq(2, 14, by = 1), nombre.fig = "t6.85_49", cent = T, anios = c(2070, 2099))
PlotTsTend(global = t6.85_99G_tend_an, hn = t6.85_99N_tend_an, hs = t6.85_99S_tend_an, titulo = "T CNRM-CM6 - SSP858 2070 - 2100", y.label = "ºC", y.breaks = seq(2, 14, by = 1), nombre.fig = "t6.85_99", cent = T, anios = c(2070, 2099))

# q
PlotTsTend(global = q6.hisG_tend_an, hn = q6.hisN_tend_an, hs = q6.hisS_tend_an, titulo = "q CNRM-CM6 - Historico", y.label = "Kg/Kg", y.breaks = seq(0.0065, 0.01, by = 0.0001), nombre.fig = "q6.his_tend", cent = F,anios = c(1976, 2005))
PlotTsTend(global = q6.26_49G_tend_an, hn = q6.26_49N_tend_an, hs = q6.26_49S_tend_an, titulo = "q CNRM-CM6 - SSP126 2020 - 2050", y.label = "Kg/Kg", y.breaks = seq(0.007, 0.01, by = 0.0001), nombre.fig = "q6.26_49", cent = F, anios = c(2020, 2049))
PlotTsTend(global = q6.26_99G_tend_an, hn = q6.26_99N_tend_an, hs = q6.26_99S_tend_an, titulo = "q CNRM-CM6 - SSP126 2070 - 2100", y.label = "Kg/Kg", y.breaks = seq(0.007, 0.01, by = 0.0001), nombre.fig = "q6.26_99", cent = F, anios = c(2020, 2049))
PlotTsTend(global = q6.85_49G_tend_an, hn = q6.85_49N_tend_an, hs = q6.85_49S_tend_an, titulo = "q CNRM-CM6 - SSP585 2020 - 2050", y.label = "Kg/Kg", y.breaks = seq(0.007, 0.01, by = 0.0002), nombre.fig = "q6.85_49", cent = F,  anios = c(2070, 2099))
PlotTsTend(global = q6.85_99G_tend_an, hn = q6.85_99N_tend_an, hs = q6.85_99S_tend_an, titulo = "q CNRM-CM6 - SSP585 2070 - 2100", y.label = "Kg/Kg", y.breaks = seq(0.007, 0.012, by = 0.0005), nombre.fig = "q6.85_99", cent = F, anios = c(2070, 2099))

##


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
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "grey"
           , alpha.vsig = 1, sig = T
           , titulo = "H observada  1976 - 2005",  nombre.fig = "H.his", width = 35, x.label = NULL, y.label = NULL, label.escala = "KJ/kg")

mapa_topo3(variable = H.his_an[[2]], lon = lon.obs, lat = lat.obs, colorbar = "PuBuGn", niveles = 9, escala = seq(0, 100, by = 10), revert = T
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "grey"
           , alpha.vsig = 1, sig = T
           , titulo = "Aporte de q en H  1976 - 2005",  nombre.fig = "H.hisP", width = 35, x.label = NULL, y.label = NULL, label.escala = "%")


# Bias CNRM-CM5
mask = H5.his[[3]] - H.his_an[[3]]
mask[which(!is.na(mask))] = 0
mask[which(is.na(mask))] = 1
mask[which(mask == 1)] = NA
mapa_topo3(variable = H5.his[[3]] - H.his_an[[3]], lon = lon.obs, lat = lat.obs, colorbar = "RdBu", niveles = 9, escala = seq(-8, 8, by = 2), revert = T
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "grey"
           , alpha.vsig = 1, sig = T
           , titulo = "Bias H CNRM-CM5",  nombre.fig = "H5.bias", width = 35, x.label = NULL, y.label = NULL, label.escala = "KJ/kg")

mapa_topo3(variable = H5.his[[2]] - H.his_an[[2]], lon = lon.obs, lat = lat.obs, colorbar = "BrBG", niveles = 9, escala = seq(-10, 10, by = 2), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "grey"
           , alpha.vsig = 1, sig = T
           , titulo = "Aporte de q en H  1976 - 2005",  nombre.fig = "H5.biasP", width = 35, x.label = NULL, y.label = NULL, label.escala = "%")


# Bias CNRM-CM6
mask = H6.his[[3]] - H.his_an[[3]]
mask[which(!is.na(mask))] = 0
mask[which(is.na(mask))] = 1
mask[which(mask == 1)] = NA
mapa_topo3(variable = H6.his[[3]] - H.his_an[[3]], lon = lon.obs, lat = lat.obs, colorbar = "RdBu", niveles = 9, escala = seq(-8, 8, by = 2), revert = T
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "grey"
           , alpha.vsig = 1, sig = T
           , titulo = "Bias H CNRM-CM6",  nombre.fig = "H6.bias", width = 35, x.label = NULL, y.label = NULL, label.escala = "KJ/kg")

mapa_topo3(variable = H6.his[[2]] - H.his_an[[2]], lon = lon.obs, lat = lat.obs, colorbar = "BrBG", niveles = 9, escala = seq(-10, 10, by = 2), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "grey"
           , alpha.vsig = 1, sig = T
           , titulo = "Aporte de q en H  1976 - 2005",  nombre.fig = "H6.biasP", width = 35, x.label = NULL, y.label = NULL, label.escala = "%")



#### CNRM-CM5 ####
# 2.6
mask = H5.26_49[[3]] - H5.his[[3]]
mask[which(!is.na(mask))] = 0
mask[which(is.na(mask))] = 1
mask[which(mask == 1)] = NA
mapa_topo3(variable = H5.26_49[[3]] - H5.his[[3]], lon = lon.obs, lat = lat.obs, colorbar = "YlOrRd", niveles = 9, escala = seq(0, 10, by = 1), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "grey"
           , alpha.vsig = 1, sig = T
           , titulo = "Diferencia H Futuro Cercano vs Historico - CNRM-CM5 RCP2.6",  nombre.fig = "H5.dif.26_49", width = 35, x.label = NULL, y.label = NULL, label.escala = "KJ/kg")

mapa_topo3(variable = H5.26_49[[2]] - H5.his[[2]], lon = lon.obs, lat = lat.obs, colorbar = "BrBG", niveles = 9, escala = seq(-3, 3, by = 0.5), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "grey"
           , alpha.vsig = 1, sig = T
           , titulo = "Diferencia Aporte de q en H Futuro Cercano vs Historico CNRM-CM5 RCP2.6",  nombre.fig = "H5.difP.26_49", width = 35, x.label = NULL, y.label = NULL, label.escala = "%")


mask = H5.26_99[[3]] - H5.his[[3]]
mask[which(!is.na(mask))] = 0
mask[which(is.na(mask))] = 1
mask[which(mask == 1)] = NA
mapa_topo3(variable = H5.26_99[[3]] - H5.his[[3]], lon = lon.obs, lat = lat.obs, colorbar = "YlOrRd", niveles = 9, escala = seq(0, 10, by = 1), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "grey"
           , alpha.vsig = 1, sig = T
           , titulo = "Diferencia H Futuro Lejano vs Historico - CNRM-CM5 RCP2.6",  nombre.fig = "H5.dif.26_99", width = 35, x.label = NULL, y.label = NULL, label.escala = "KJ/kg")

mapa_topo3(variable = H5.26_99[[2]] - H5.his[[2]], lon = lon.obs, lat = lat.obs, colorbar = "BrBG", niveles = 9, escala = seq(-3, 3, by = 0.5), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "grey"
           , alpha.vsig = 1, sig = T
           , titulo = "Diferencia Aporte de q en H Futuro Lejano vs Historico CNRM-CM5 RCP2.6",  nombre.fig = "H5.difP.26_99", width = 35, x.label = NULL, y.label = NULL, label.escala = "%")

# 8.5
mask = H5.85_49[[3]] - H5.his[[3]]
mask[which(!is.na(mask))] = 0
mask[which(is.na(mask))] = 1
mask[which(mask == 1)] = NA
mapa_topo3(variable = H5.85_49[[3]] - H5.his[[3]], lon = lon.obs, lat = lat.obs, colorbar = "YlOrRd", niveles = 9, escala = seq(0, 10, by = 1), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "grey"
           , alpha.vsig = 1, sig = T
           , titulo = "Diferencia H Futuro Cercano vs Historico - CNRM-CM5 RCP8.5",  nombre.fig = "H5.dif.85_49", width = 35, x.label = NULL, y.label = NULL, label.escala = "KJ/kg")

mapa_topo3(variable = H5.85_49[[2]] - H5.his[[2]], lon = lon.obs, lat = lat.obs, colorbar = "BrBG", niveles = 9, escala = seq(-3, 3, by = 0.5), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "grey"
           , alpha.vsig = 1, sig = T
           , titulo = "Diferencia Aporte de q en H Futuro Cercano vs Historico CNRM-CM5 RCP8.5",  nombre.fig = "H5.difP.85_49", width = 35, x.label = NULL, y.label = NULL, label.escala = "%")


mask = H5.85_99[[3]] - H5.his[[3]]
mask[which(!is.na(mask))] = 0
mask[which(is.na(mask))] = 1
mask[which(mask == 1)] = NA
mapa_topo3(variable = H5.85_99[[3]] - H5.his[[3]], lon = lon.obs, lat = lat.obs, colorbar = "YlOrRd", niveles = 9, escala = seq(0, 10, by = 1), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "grey"
           , alpha.vsig = 1, sig = T
           , titulo = "Diferencia H Futuro Lejano vs Historico - CNRM-CM5 RCP8.5",  nombre.fig = "H5.dif.85_99", width = 35, x.label = NULL, y.label = NULL, label.escala = "KJ/kg")

mapa_topo3(variable = H5.85_99[[2]] - H5.his[[2]], lon = lon.obs, lat = lat.obs, colorbar = "BrBG", niveles = 9, escala = seq(-3, 3, by = 0.5), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "grey"
           , alpha.vsig = 1, sig = T
           , titulo = "Diferencia Aporte de q en H Futuro Lejano vs Historico CNRM-CM5 RCP8.5",  nombre.fig = "H5.difP.85_99", width = 35, x.label = NULL, y.label = NULL, label.escala = "%")

#### CNRM-CM6 ####
#126
mask = H6.26_49[[3]] - H6.his[[3]]
mask[which(!is.na(mask))] = 0
mask[which(is.na(mask))] = 1
mask[which(mask == 1)] = NA
mapa_topo3(variable = H6.26_49[[3]] - H6.his[[3]], lon = lon.obs, lat = lat.obs, colorbar = "YlOrRd", niveles = 9, escala = seq(0, 10, by = 1), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "grey"
           , alpha.vsig = 1, sig = T
           , titulo = "Diferencia H Futuro Cercano vs Historico - CNRM-CM6 SSP126",  nombre.fig = "H6.dif.26_49", width = 35, x.label = NULL, y.label = NULL, label.escala = "KJ/kg")

mapa_topo3(variable = H6.26_49[[2]] - H6.his[[2]], lon = lon.obs, lat = lat.obs, colorbar = "BrBG", niveles = 9, escala = seq(-3, 3, by = 0.5), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "grey"
           , alpha.vsig = 1, sig = T
           , titulo = "Diferencia Aporte de q en H Futuro Cercano vs Historico CNRM-CM6 SSP126",  nombre.fig = "H6.difP.26_49", width = 35, x.label = NULL, y.label = NULL, label.escala = "%")


mask = H6.26_99[[3]] - H6.his[[3]]
mask[which(!is.na(mask))] = 0
mask[which(is.na(mask))] = 1
mask[which(mask == 1)] = NA
mapa_topo3(variable = H6.26_99[[3]] - H6.his[[3]], lon = lon.obs, lat = lat.obs, colorbar = "YlOrRd", niveles = 9, escala = seq(0, 10, by = 1), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "grey"
           , alpha.vsig = 1, sig = T
           , titulo = "Diferencia H Futuro Lejano vs Historico - CNRM-CM6 SSP126",  nombre.fig = "H6.dif.26_99", width = 35, x.label = NULL, y.label = NULL, label.escala = "KJ/kg")

mapa_topo3(variable = H6.26_99[[2]] - H6.his[[2]], lon = lon.obs, lat = lat.obs, colorbar = "BrBG", niveles = 9, escala = seq(-3, 3, by = 0.5), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "grey"
           , alpha.vsig = 1, sig = T
           , titulo = "Diferencia Aporte de q en H Futuro Lejano vs Historico CNRM-CM6 SSP126",  nombre.fig = "H6.difP.26_99", width = 35, x.label = NULL, y.label = NULL, label.escala = "%")

#858
mask = H6.85_49[[3]] - H6.his[[3]]
mask[which(!is.na(mask))] = 0
mask[which(is.na(mask))] = 1
mask[which(mask == 1)] = NA
mapa_topo3(variable = H6.85_49[[3]] - H6.his[[3]], lon = lon.obs, lat = lat.obs, colorbar = "YlOrRd", niveles = 9, escala = seq(0, 10, by = 1), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "grey"
           , alpha.vsig = 1, sig = T
           , titulo = "Diferencia H Futuro Cercano vs Historico - CNRM-CM6 SSP585",  nombre.fig = "H6.dif.85_49", width = 35, x.label = NULL, y.label = NULL, label.escala = "KJ/kg")

mapa_topo3(variable = H6.85_49[[2]] - H6.his[[2]], lon = lon.obs, lat = lat.obs, colorbar = "BrBG", niveles = 9, escala = seq(-3, 3, by = 0.5), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "grey"
           , alpha.vsig = 1, sig = T
           , titulo = "Diferencia Aporte de q en H Futuro Cercano vs Historico CNRM-CM6 SSP585",  nombre.fig = "H6.difP.85_49", width = 35, x.label = NULL, y.label = NULL, label.escala = "%")


mask = H6.85_99[[3]] - H6.his[[3]]
mask[which(!is.na(mask))] = 0
mask[which(is.na(mask))] = 1
mask[which(mask == 1)] = NA
mapa_topo3(variable = H6.85_99[[3]] - H6.his[[3]], lon = lon.obs, lat = lat.obs, colorbar = "YlOrRd", niveles = 9, escala = seq(0, 10, by = 1), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "grey"
           , alpha.vsig = 1, sig = T
           , titulo = "Diferencia H Futuro Lejano vs Historico - CNRM-CM6 SSP585",  nombre.fig = "H6.dif.85_99", width = 35, x.label = NULL, y.label = NULL, label.escala = "KJ/kg")

mapa_topo3(variable = H6.85_99[[2]] - H6.his[[2]], lon = lon.obs, lat = lat.obs, colorbar = "BrBG", niveles = 9, escala = seq(-3, 3, by = 0.5), revert = F
           , mapa = "mundo", na.fill = 0 ,salida = "/Salidas/TP3/", variable.sig = mask, color.vsig = "grey"
           , alpha.vsig = 1, sig = T
           , titulo = "Diferencia Aporte de q en H Futuro Lejano vs Historico CNRM-CM6 SSP585",  nombre.fig = "H6.difP.85_99", width = 35, x.label = NULL, y.label = NULL, label.escala = "%")

#### promedios zonales de H 
#### radacion ####
q.obs = RhQ(rh = hu.obs_seasons, p = pr.obs_seasons, t = t.obs_seasons)

etp = etp5.his_an[[1]]/(365/12)

RT = etp/Lv(t5.his_an[[1]])
h = S(t.obs_seasons) + Lv(t.obs_seasons)*RhQ(rh = hu.obs_seasons, p = pr.obs_seasons, t = t.obs_seasons) # si es datos obs, --> q(hr)
Fa = h.V

RhQ(rh = hu.obs_seasons, p = pr.obs_seasons, t = t.obs_seasons)

