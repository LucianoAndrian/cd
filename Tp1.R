# TP1
#rm(list = ls())
library(ncdf4)
library(fields)
library(ggplot2)

source("FUNCIONES.R")

lon = read.table("lon.txt")[,1]
lat = read.table("lat.txt")[,1]

# esto no anda ahora... porque? no hay porque
#topo = metR::GetTopography(1, 358, 89,  -89, resolution = 1) # mapa topografia
#topo2 = topo #
#topo2[which(topo2$h<1500)]=NA # altura para la cual tapa el grafico

mask = as.matrix(read.table("mask.txt"))
mask_arr = array(NA, dim = c(length(lon), length(lat), 9))
for(i in 1:9){
  mask_arr[,,i] = mask
}

#### Apertura de archivos ####
### Observaciones ###
## Anual
#-------------------------------- temp - historico --------------------------------# 
ruta = getwd()
aux = nc_open(paste(ruta, "/Datos_Obs1/tmp_cru_ts3.20_197601-200512_2.5_anu.nc", sep = ""))
tas_obs = ncvar_get(aux, "tmp")
nc_close(aux)

#-------------------------------- sst - historico ---------------------------------#

aux = nc_open("Datos_Obs1/sst.mnmean.v4_197601-200512_2.5_anu.nc")
sst_obs = ncvar_get(aux, "sst")

nc_close(aux)
#-------------------------------- precip - historico ------------------------------#
aux = nc_open(paste(ruta, "/Datos_Obs1/precip.mon.total.v7_197601-200512_2.5_anu.nc", sep = ""))
pp_obs = ncvar_get(aux, "precip")
nc_close(aux)

## Mensual 

aux = nc_open(paste(ruta, "/Datos_Obs1/precip.mon.total.v7.2.5-197601-200512.nc", sep = ""))
pp_obs_m = ncvar_get(aux, "precip")
nc_close(aux) # lat y lons corridas 0.25 --> interpolar


### Modelos ###

### CNRM-CM5 ###

## Anual

#-------------------------------- tas - historical  -------------------------------#
tas5_an = open_nc(file_pattern = "tas_Amon_CNRM-CM5_historical_r*_197601-200512_2.5_anu.nc"
                 , model = "cnrm-cm5", members = 9, variable = "tas", mes_anual = "anual")

#-------------------------------- pr - historical - -------------------------------#
pp5_an = open_nc(file_pattern = "pr_Amon_CNRM-CM5_historical_r*_197601-200512_2.5_anu.nc"
                    , model = "cnrm-cm5", members = 9, variable = "pr", mes_anual = "anual") 


## Mensual

#-------------------------------- pr - historical ---------------------------------#
pp5_m = open_nc(file_pattern = "pr_Amon_CNRM-CM5_historical_r*_197601-200512_2.5_mes.nc"
                , model = "cnrm-cm5", members = 9, variable = "pr", mes_anual = "mes")


### CNRM-CM6 ###

## Anual

#-------------------------------- tas - historical  -------------------------------#
tas6_an = open_nc(file_pattern = "tas_Amon_CNRM-CM6-1_historical_r*_2.5_anu.nc"
                  , model = "cnrm-cm6", members = 10, variable = "tas", mes_anual = "anual")

#-------------------------------- pr - historical ---------------------------------#
pp6_an = open_nc(file_pattern = "pr_Amon_CNRM-CM6-1_historical_r*_2.5_anu.nc"
                 , model = "cnrm-cm6", members = 10, variable = "pr", mes_anual = "anual" )

## Mensual

#-------------------------------- pr - historical ---------------------------------#
pp6_m = open_nc(file_pattern = "pr_Amon_CNRM-CM6-1_historical_r*_2.5_mes.nc"
                        , model = "cnrm-cm6", members = 10, variable = "pr", mes_anual = "mes")


#### Uniendo TAS y SST ####
# es necesario enmascarar los datos de sst ya que el oceano ocupa partes de los continentes
mask_arr = array(NA, dim = c(length(lon), length(lat), 30))
#invierto mascara
mask2 = mask
mask2[which(is.na(mask2))] = 2 ; mask2[which(mask2 == 1)] = NA ; mask2[which(mask2 == 2)] = 1

mask_arr2 = array(NA, dim = c(length(lon), length(lat), 30))
for(i in 1:30){
  mask_arr2[,,i] = mask2
}

sst_obs = sst_obs*mask_arr2 ; sst_obs[which(is.na(sst_obs))] = 1
tas_obs[which(is.na(tas_obs))]= 1  # aun quedan puntos que no se anularon

obs_t = tas_obs*sst_obs; obs_t[which(obs_t > 50)] = NA # datos obs. continentes y oceanos
obs_t[which(obs_t == 1)] = NA 

## > pp es solamente contienental < ##


##### Calculos #####

# campos medioas r y ensamble

mods = list()
mods[[1]] = tas5_an[[1]]; mods[[2]] = tas6_an[[1]]; mods[[3]] = pp5_an[[1]]; mods[[4]] = pp6_an[[1]]
mods[[5]] = apply(tas5_an[[1]], c(1,2,3), mean, na.rm = T); mods[[6]] = apply(tas6_an[[1]], c(1,2,3), mean, na.rm = T)
mods[[7]] = apply(pp5_an[[1]], c(1,2,3), mean, na.rm = T); mods[[8]] = apply(pp6_an[[1]], c(1,2,3), mean, na.rm = T)

calc_means = list()

for(i in 0:3){

    calc_means[[i + 1 + i*1]] = apply(mods[[i+1]], c(1,2), mean, na.rm = T) # promedio del ensamble
    calc_means[[i + 2 + i*1]] = apply(mods[[i+1]], c(1,2,4), mean, na.rm = T) # promedio para cada miembro
  
}

sd_s = list()
for(i in 0:3){
  
  sd_s[[i + 1 + i*1]] = apply(mods[[i+5]], c(1,2), sd, na.rm = T) # sd del ensamble en funcion de los años

  sd_s[[i + 2 + i*1]] = apply(calc_means[[i+ 2 + i*1]], c(1,2), sd, na.rm = T) # sd del ensamble entre miembros
  
}

#calc[[1 - 8]]          sd_s[[1 - 8]]
#tas5_an_mean  (1)         #sd5_ens_tas 
#tas5_an_r_means (2)       #sd5_r_tas 
#tas6_an_mean (3)          #sd6_ens_tas
#tas6_an_r_means (4)        #sd6_r_tas 
#pp5_an_mean  (5)          #sd5_ens_pp
#pp5_an_r_means (6)        #sd5_r_pp 
#pp6_an_mean  (7)          #sd6_ens_pp 
#pp6_an_r_means (8)        #sd6_r_pp  

# Bias
# CNRM5 # ensamble y miembros
## T ##
bias5_t_ens = (calc_means[[1]] - 273) - apply(obs_t, c(1,2), mean, na.rm = T)

bias5_t_r = array(data = NA, dim = c(144, 73, 9))
for(i in 1:9){
  
  bias5_t_r[,,i] = (calc_means[[2]][,,i] - 273) - apply(obs_t, c(1,2), mean, na.rm = T)
  
}

sd_bias5_t_r = apply(bias5_t_r, c(1,2), sd, na.rm = T)

## pp ##
bias5_pp_ens =  (calc_means[[5]]*mask)/(apply(pp_obs, c(1,2), mean, na.rm = T))*100-100

bias5_pp_r = array(data = NA, dim = c(144, 73, 9))
for(i in 1:9){
  
  bias5_pp_r[,,i] = ((calc_means[[6]][,,i]*mask)/apply(pp_obs, c(1,2), mean, na.rm = T))*100 - 100
  
}

sd_bias5_pp_r = apply(bias5_pp_r, c(1,2), sd, na.rm = T)


# CNRM6 # ensamble y miembros
## T ##
bias6_t_ens = (calc_means[[3]] - 273) - apply(obs_t, c(1,2), mean, na.rm = T)

bias6_t_r = array(data = NA, dim = c(144, 73, 10))
for(i in 1:10){
  
  bias6_t_r[,,i] = (calc_means[[4]][,,i] - 273) - apply(obs_t, c(1,2), mean, na.rm = T)
  
}

sd_bias6_t_r = apply(bias6_t_r, c(1,2), sd, na.rm = T)


## pp ##
bias6_pp_ens =  (calc_means[[7]]*mask)/(apply(pp_obs, c(1,2), mean, na.rm = T))*100-100

bias6_pp_r = array(data = NA, dim = c(144, 73, 10))

for(i in 1:10){
  
  bias6_pp_r[,,i] = ((calc_means[[8]][,,i]*mask)/apply(pp_obs, c(1,2), mean, na.rm = T))*100 -100
  
}

sd_bias6_pp_r = apply(bias6_pp_r, c(1,2), sd, na.rm = T)



# Corr
# CNRM5
aux = apply(tas5_an[[1]], c(1,2,3), mean, na.rm = T)
cor5_t = corr(mod = aux, obs = tas_obs,lon = 144, lat = 73, cf = 0.95)

mask_arr = array(NA, dim = c(144, 73, 30))
for(i in 1:30){
  mask_arr[,,i] = mask
}

aux = apply(pp5_an[[1]], c(1,2,3), mean, na.rm = T)*mask_arr
cor5_pp = corr(mod = aux, obs = pp_obs, lon = 144, lat = 73, cf = 0.95)

# CNRM6
aux = apply(tas6_an[[1]], c(1,2,3), mean, na.rm = T)
cor6_t = corr(mod = aux, obs = tas_obs,lon = 144, lat = 73, cf = 0.95)


aux = apply(pp6_an[[1]], c(1,2,3), mean, na.rm = T)*mask_arr
cor6_pp = corr(mod = aux, obs = pp_obs, lon = 144, lat = 73, cf = 0.95)

# subregiones
#SAn 73.75-71.25,.51.25-36.75 ---> 75 - 70; -50 - -35

aux_lats = list(); aux_lons = list()
aux_lons[[1]] = which((lon>=(-75+360))&(lon<=(-70+360))); aux_lats[[1]] = which((lat>=-50)&(lat<=-35))
aux_lons[[2]] = which((lon>=(-60+360))&(lon<=(-50+360))); aux_lats[[2]] = which((lat>=-35)&(lat<=-22.5))
aux_lons[[3]] = which((lon>=(-170+360))&(lon<=(-120+360))); aux_lats[[3]] = which((lat>=-5)&(lat<=5))

regiones_means_t5 = list(); regiones_means_t6 = list()
regiones_means_pp5 = list(); regiones_means_pp6 = list()

regiones_sd_t5 = list(); regiones_sd_t6 = list()
regiones_sd_pp5 = list(); regiones_sd_pp6 = list()

regiones_bias_t5 = list(); regiones_bias_t6 = list()
regiones_bias_pp5 = list(); regiones_bias_pp6 = list()

for(i in 1:3){
  
  regiones_means_t5[[i]] = calc_means[[1]][aux_lons[[i]], aux_lats[[i]]] - 273
  regiones_means_t6[[i]] = calc_means[[3]][aux_lons[[i]], aux_lats[[i]]] - 273
  
  regiones_means_pp5[[i]] = calc_means[[5]][aux_lons[[i]], aux_lats[[i]]] 
  regiones_means_pp6[[i]] = calc_means[[7]][aux_lons[[i]], aux_lats[[i]]] 
  
  regiones_sd_t5[[i]] = sd_s[[1]][aux_lons[[i]], aux_lats[[i]]]
  regiones_sd_t6[[i]] = sd_s[[3]][aux_lons[[i]], aux_lats[[i]]]
  
  regiones_sd_pp5[[i]] = sd_s[[5]][aux_lons[[i]], aux_lats[[i]]]
  regiones_sd_pp5[[i]] = sd_s[[7]][aux_lons[[i]], aux_lats[[i]]]
  
  regiones_bias_t5[[i]] = (calc_means[[1]][aux_lons[[i]], aux_lats[[i]]] - 273) - apply(obs_t[aux_lons[[i]], aux_lats[[i]],], c(1,2), mean, na.rm = T)
  regiones_bias_t6[[i]] = (calc_means[[3]][aux_lons[[i]], aux_lats[[i]]] - 273) - apply(obs_t[aux_lons[[i]], aux_lats[[i]],], c(1,2), mean, na.rm = T)
  
  regiones_bias_pp5[[i]] = (calc_means[[5]][aux_lons[[i]], aux_lats[[i]]])/apply(pp_obs[aux_lons[[i]], aux_lats[[i]],], c(1,2), mean, na.rm = T)*100 - 100
  regiones_bias_pp6[[i]] = (calc_means[[7]][aux_lons[[i]], aux_lats[[i]]])/apply(pp_obs[aux_lons[[i]], aux_lats[[i]],], c(1,2), mean, na.rm = T)*100 - 100
  
  
}

# series temporales
# Anomalia

mask_arr = array(NA, dim = c(144, 73, 30, 10))
for(i in 1:30){
  for(j in 1:10){
    mask_arr[,,i,j] = mask
  }
}

mask_arr2 = array(NA, dim = c(length(lon), length(lat), 30, 10))
for(i in 1:30){
  for(j in 1:10){
    mask_arr2[,,i,j] = mask2
  }
  
}

# globales

anom_t_obs =  apply(obs_t,c(3), mean, na.rm = T) - mean(obs_t, na.rm = T)
anom_t_obs_cont = apply(tas_obs,c(3), mean, na.rm = T) - mean(tas_obs, na.rm = T)
amon_t_obs_oce = apply(sst_obs*mask_arr2[,,,1],c(3), mean, na.rm = T) - mean(sst_obs*mask_arr2[,,,1], na.rm = T)

anom_pp_obs = apply(pp_obs,c(3), mean, na.rm = T) - mean(pp_obs, na.rm = T)

anom_t5 = apply(tas5_an[[1]], c(3), mean, na.rm = T) - mean(tas5_an[[1]], na.rm = T)
anom_t5_cont = apply(tas5_an[[1]]*mask_arr[,,,1:9], c(3), mean, na.rm = T) - mean(tas5_an[[1]]*mask_arr[,,,1:9], na.rm = T)
anom_t5_oce = apply(tas5_an[[1]]*mask_arr2[,,,1:9], c(3), mean, na.rm = T) - mean(tas5_an[[1]]*mask_arr2[,,,1:9], na.rm = T)
anom_t5_r = apply(tas5_an[[1]], c(3,4), mean, na.rm = T) - apply(tas5_an[[1]], c(4),mean, na.rm = T)

anom_pp5 = apply(pp5_an[[1]]*mask_arr[,,,1:9], c(3), mean, na.rm = T) - mean(pp5_an[[1]]*mask_arr[,,,1:9], na.rm = T)
anom_pp6 = apply(pp6_an[[1]]*mask_arr, c(3), mean, na.rm = T) - mean(pp6_an[[1]]*mask_arr, na.rm = T)


anom_t6 = apply(tas6_an[[1]], c(3), mean, na.rm = T)- mean(tas6_an[[1]], na.rm = T)
anom_t6_cont = apply(tas6_an[[1]]*mask_arr, c(3), mean, na.rm = T) - mean(tas6_an[[1]]*mask_arr, na.rm = T)
anom_t6_oce = apply(tas6_an[[1]]*mask_arr2, c(3), mean, na.rm = T) - mean(tas6_an[[1]]*mask_arr2, na.rm = T)
anom_t6_r = apply(tas6_an[[1]], c(3,4), mean, na.rm = T) - apply(tas6_an[[1]], c(4),mean, na.rm = T)


# en las regiones, solo SESA

anom_t_obs_sesa =  apply(obs_t[aux_lons[[2]], aux_lats[[2]],],c(3), mean, na.rm = T) - mean(obs_t[aux_lons[[2]], aux_lats[[2]],], na.rm = T)
anom_t5_sesa = apply(tas5_an[[1]][aux_lons[[2]], aux_lats[[2]],,], c(3), mean, na.rm = T) - mean(tas5_an[[1]][aux_lons[[2]], aux_lats[[2]],,], na.rm = T)
anom_t6_sesa = apply(tas6_an[[1]][aux_lons[[2]], aux_lats[[2]],,], c(3), mean, na.rm = T) - mean(tas6_an[[1]][aux_lons[[2]], aux_lats[[2]],,], na.rm = T)

anom_pp_obs_sesa = apply(pp_obs[aux_lons[[2]], aux_lats[[2]],],c(3), mean, na.rm = T) - mean(pp_obs[aux_lons[[2]], aux_lats[[2]],], na.rm = T)
anom_pp5_sesa = apply(pp5_an[[1]][aux_lons[[2]], aux_lats[[2]],,]*mask_arr[aux_lons[[2]],aux_lats[[2]],,1:9], c(3), mean, na.rm = T) - mean(pp5_an[[1]][aux_lons[[2]], aux_lats[[2]],,]*mask_arr[aux_lons[[2]],aux_lats[[2]],,1:9], na.rm = T)
anom_pp6_sesa = apply(pp6_an[[1]][aux_lons[[2]], aux_lats[[2]],,]*mask_arr[aux_lons[[2]],aux_lats[[2]],,], c(3), mean, na.rm = T) - mean(pp6_an[[1]][aux_lons[[2]], aux_lats[[2]],,]*mask_arr[aux_lons[[2]],aux_lats[[2]],,], na.rm = T)



# estaciones pp
# para esto hay q ver si es posoible interpolar la grilla, o mas bien "trasladar"


#### GRAFICOS ####
# T5 ensamble
aux = array(data = calc_means[[1]], c(144,73,1)) 
mapa(lista = aux, titulo = "Temperatura media ensamble CNRM-CM5 1975 - 2005", nombre_fig = "tas5.ens.mean"
     , escala = c(-30, 30), label_escala = "ºC", resta = 273, brewer = "RdBu", revert = "si", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(-30, 30, by = 5), breaks_c_f = seq(-30, 30, by = 2.5), r = 1,na_fill = -10000, salida = "/Salidas/TP1/")

# r.s
mapa(lista = calc_means[[2]], titulo = "Temperatura media CNRM-CM5 1975 - 2005", nombre_fig = "tas5.mean.r"
     , escala = c(-30, 30), label_escala = "ºC", resta = 273, brewer = "RdBu", revert = "si", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(-30, 30, by = 5), breaks_c_f = seq(-30, 30, by = 2.5), r = 9, salida = "/Salidas/TP1/")

# sd
aux = array(data = sd_s[[1]], c(144,73,1)) 
mapa(lista = aux, titulo = "Temperatura desvio ensamble CNRM-CM5 1975 - 2005", nombre_fig = "sd_tas5.ens.mean"
     , escala = c(0, 1), label_escala = "", resta = 0, brewer = "YlOrRd", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 1, by = 0.1), breaks_c_f = seq(0, 1, by = 0.1), r = 1, salida = "/Salidas/TP1/")

# sd emtre miembros de ensambles T
#aux = array(data = sd_s[[2]], c(144,73,1)) 
#mapa(lista = aux, titulo = "Temperatura desvio ensamble CNRM-CM5 1975 - 2005", nombre_fig = "prueba"
#     , escala = c(0, 1), label_escala = "", resta = 0, brewer = "YlOrRd", revert = "no", niveles = 9
#     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 1, by = 0.1), breaks_c_f = seq(0, 1, by = 0.1), r = 1, salida = "/Salidas/TP1/")

# bias
aux = array(data = bias5_t_ens, c(144,73,1)) 
aux[which(abs(aux)>7)] = NA
mapa(lista = aux, titulo = "Temperatura Bias ensamble CNRM-CM5 1975 - 2005", nombre_fig = "bias5_t.ens"
     , escala = c(-5, 5), label_escala = "ºC", resta = 0, brewer = "RdBu", revert = "si", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(-5, 5, by = 1), breaks_c_f = seq(-5, 5, by = 0.5), r = 1, na_fill = 0, salida = "/Salidas/TP1/")

# correlaciones
aux = array(data = cor5_t[,,1]*cor5_t[,,2], c(144,73,1)) 
mapa(lista = aux, titulo = "Correlaciòn temperatura CNRM-CM5 vs observaciones  1975 - 2005", nombre_fig = "cor5_t.ens"
     , escala = c(0, 1), label_escala = "R", resta = 0, brewer = "YlOrRd", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 1, by = 0.1), breaks_c_f = seq(0, 1, by = 0.1), r = 1, na_fill = -10000, salida = "/Salidas/TP1/")



# pp5 emsamble
aux = array(data = calc_means[[5]], c(144,73,1)) 
mapa(lista = aux, titulo = "Precipitacíon media ensamble CNRM-CM5 1975 - 2005", nombre_fig = "pp5.ens.mean"
     , escala = c(0, 3500), label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 3500, by = 250), breaks_c_f = seq(0, 3500, by = 250), r = 1, salida = "/Salidas/TP1/")
#r.s
mapa(lista = calc_means[[6]], titulo = "Precipitación media CNRM-CM5 1975 - 2005", nombre_fig = "pp5.mean.r"
     , escala = c(0, 3500), label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 3500, by = 250), breaks_c_f = seq(0, 3500, by = 250), r = 9, salida = "/Salidas/TP1/")

aux = array(data = sd_s[[3]], c(144,73,1)) 
mapa(lista = aux, titulo = "Precipitación desvio ensamble CNRM-CM5 1975 - 2005", nombre_fig = "sd_pp5.ens.mean"
     , escala = c(0, 1), label_escala = "", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 1, by = 0.1), breaks_c_f = seq(0, 1, by = 0.1), r = 1, salida = "/Salidas/TP1/")

#aux = array(data = sd_s[[4]], c(144,73,1)) 
#mapa(lista = aux, titulo = "Temperatura desvio ensamble CNRM-CM5 1975 - 2005", nombre_fig = "prueba"
#     , escala = c(0, 1), label_escala = "", resta = 0, brewer = "YlOrRd", revert = "no", niveles = 9
#     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 1, by = 0.1), breaks_c_f = seq(0, 1, by = 0.1), r = 1, salida = "/Salidas/TP1/")


aux = array(data = bias5_pp_ens, c(144,73,1)) 
aux[which(aux > 100)] = 105;aux[which(aux < -100)] = -105;  
mapa(lista = aux, titulo = "Precipitación Bias % ensamble CNRM-CM5 1975 - 2005", nombre_fig = "bias5_pp.ens"
     , escala = c(-100, 100), label_escala = "mm", resta = 0, brewer = "BrBG", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(-100, 100, by = 20), breaks_c_f = seq(-100, 100, by = 10), r = 1, na_fill = -10000, salida = "/Salidas/TP1/")

aux = array(data = cor5_pp[,,1]*cor5_pp[,,2], c(144,73,1)) 
mapa(lista = aux, titulo = "Correlaciòn precipitación CNRM-CM5 vs observaciones  1975 - 2005", nombre_fig = "cor5_pp.ens"
     , escala = c(-1, 1), label_escala = "R", resta = 0, brewer = "RdBu", revert = "si", niveles = 9
     , contour = "no", lon = lon, lat = lat, escala_dis = seq(-1, 1, by = 0.2), breaks_c_f = seq(-1, 1, by = 0.2), r = 1, na_fill = 0, salida = "/Salidas/TP1/")


# subregion

aux = array(data = regiones_bias_t5[[1]], dim = c(3, 7, 1))
aux2 = array(data = regiones_bias_t5[[2]], dim = c(5, 6, 1))
aux3 = array(data = regiones_bias_t5[[3]], dim = c(21, 5, 1))

mapa_reg(lista = aux, lista2 = aux2, lista3 = aux3, titulo = "probando", nombre_fig = "prueba"
     , escala = c(-4, 4), label_escala = "m", resta = 0,revert = "si", brewer = "RdBu", niveles = 9
     ,  lons = lon, lats = lat, aux_lons = aux_lons, aux_lats = aux_lats, 
     escala_dis = seq(-4, 4, by = 0.5), r = 1, na_fill = 0, salida = "/Salidas/TP1/")

