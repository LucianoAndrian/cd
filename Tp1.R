# TP1 Simulacion del clima 2020

library(ncdf4)
library(fields)
library(ggplot2)

source("FUNCIONES.R")

lon = read.table("lon.txt")[,1]
lat = read.table("lat.txt")[,1]



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



##### Campos medios y desvio #####

## OBSERVADO ##
t.mean.obs = apply(obs_t, c(1,2), mean, na.rm = T)
t.sd.obs = apply(obs_t, c(1,2), sd, na.rm = T)

aux = array(t.mean.obs, c(144,73,1))
mapa(lista = aux, titulo = "Temperatura media observada GPCC 1975 - 2005", nombre_fig = "t.mean.obs"
     , escala = c(-30,30), label_escala = "ºC", resta = 0, brewer = "RdBu", revert = "si", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(-30, 30, by = 5), breaks_c_f = seq(-30, 30, by = 5), r = 1, na_fill = 0, salida = "/Salidas/TP1/")

aux = array(t.sd.obs, c(144,73,1))
mapa(lista = aux, titulo = "SD Temperatura media observada GPCC 1975 - 2005", nombre_fig = "t.sd.obs"
     , escala = c(0,1), label_escala = "ºC", resta = 0, brewer = "YlOrRd", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 1, by = 0.1), breaks_c_f = seq(0, 1, by = 0.1), r = 1, na_fill = 0, salida = "/Salidas/TP1/")



pp.mean.obs = apply(pp_obs, c(1,2), mean, na.rm = T)
pp.sd.obs = apply(pp_obs, c(1,2), sd, na.rm = T)

aux = array(pp.mean.obs, c(144,73,1))
mapa(lista = aux, titulo = "Precipitacíon media observada GPCC 1975 - 2005", nombre_fig = "pp.mean.obs"
     , escala = c(0, 3500), label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 3500, by = 250), breaks_c_f = seq(0, 3500, by = 250), r = 1, na_fill = -10000, salida = "/Salidas/TP1/")

aux = array(pp.sd.obs, c(144,73,1))
mapa(lista = aux, titulo = "SD Precipitación media observado GPCC 1975 - 2005", nombre_fig = "pp.sd.obs"
     , escala = c(0, 250), label_escala = "", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 250, by = 25), breaks_c_f = seq(0, 250, by = 25), r = 1, na_fill = -10000, salida = "/Salidas/TP1/")



## ENSAMBLES DE MODELOS CON LOS 30 AÑOS ##
t5.ens = apply(tas5_an[[1]], c(1,2,3), mean, na.rm = T) - 273
t6.ens = apply(tas6_an[[1]], c(1,2,3), mean, na.rm = T) - 273
pp5.ens = apply(pp5_an[[1]], c(1,2,3), mean, na.rm = T) 
pp6.ens = apply(pp6_an[[1]], c(1,2,3), mean, na.rm = T)


## MEDIAS Y DESVIOS DE LOS MODELOS ##
# TEMPERATURA
t5.med = apply(t5.ens, c(1,2), mean, na.rm = T)  
t6.med = apply(t6.ens, c(1,2), mean, na.rm = T)

aux = array(t5.med, c(144,73,1))
mapa(lista = aux, titulo = "Temperatura media CNRM-CM5 1975 - 2005", nombre_fig = "t5.med"
     , escala = c(-30,30), label_escala = "ºC", resta = 0, brewer = "RdBu", revert = "si", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(-30, 30, by = 5), breaks_c_f = seq(-30, 30, by = 5), r = 1, na_fill = 0, salida = "/Salidas/TP1/")

aux = array(t6.med, c(144,73,1))
mapa(lista = aux, titulo = "Temperatura media CNRM-CM6 1975 - 2005", nombre_fig = "t6.med"
     , escala = c(-30,30), label_escala = "ºC", resta = 0, brewer = "RdBu", revert = "si", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(-30, 30, by = 5), breaks_c_f = seq(-30, 30, by = 5), r = 1, na_fill = 0, salida = "/Salidas/TP1/")


t5.sd = apply(t5.ens, c(1,2), sd, na.rm = T)
t6.sd = apply(t6.ens, c(1,2), sd, na.rm = T)

aux = array(t5.sd, c(144,73,1))
mapa(lista = aux, titulo = "SD Temperatura media CNRM-CM5 1975 - 2005", nombre_fig = "t5.sd"
     , escala = c(0,1), label_escala = "ºC", resta = 0, brewer = "YlOrRd", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 1, by = 0.1), breaks_c_f = seq(0, 1, by = 0.1), r = 1, na_fill = 0, salida = "/Salidas/TP1/")


aux = array(t6.sd, c(144,73,1))
mapa(lista = aux, titulo = "SD Temperatura media CNRM-CM6 1975 - 2005", nombre_fig = "t6.sd"
     , escala = c(0,1), label_escala = "ºC", resta = 0, brewer = "YlOrRd", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 1, by = 0.1), breaks_c_f = seq(0, 1, by = 0.1), r = 1, na_fill = 0, salida = "/Salidas/TP1/")


# PP

pp5.med = apply(pp5_an[[1]], c(1,2), mean, na.rm = T) 
pp6.med = apply(pp6_an[[1]], c(1,2), mean, na.rm = T) 

aux = array(pp5.med, c(144,73,1))
mapa(lista = aux, titulo = "Precipitacíon media CNRM-CM5 1975 - 2005", nombre_fig = "pp5.med"
     , escala = c(0, 3500), label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 3500, by = 250), breaks_c_f = seq(0, 3500, by = 250), r = 1, na_fill = -10000, salida = "/Salidas/TP1/")

aux = array(pp6.med, c(144,73,1))
mapa(lista = aux, titulo = "Precipitacíon media CNRM-CM6 1975 - 2005", nombre_fig = "pp6.med"
     , escala = c(0, 3500), label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 3500, by = 250), breaks_c_f = seq(0, 3500, by = 250), r = 1, na_fill = -10000, salida = "/Salidas/TP1/")


pp5.sd = apply(pp5.ens, c(1,2), sd, na.rm = T)
pp6.sd = apply(pp6.ens, c(1,2), sd, na.rm = T)

aux = array(pp5.sd, c(144,73,1))
mapa(lista = aux, titulo = "SD Precipitación media CNRM-CM5 1975 - 2005", nombre_fig = "pp5.sd"
     , escala = c(0, 250), label_escala = "", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 250, by = 25), breaks_c_f = seq(0, 250, by = 25), r = 1, na_fill = -10000, salida = "/Salidas/TP1/")


aux = array(pp6.sd, c(144,73,1))
mapa(lista = aux, titulo = "SD Precipitación media CNRM-CM6 1975 - 2005", nombre_fig = "pp6.sd"
     , escala = c(0, 250), label_escala = "", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 250, by = 25), breaks_c_f = seq(0, 250, by = 25), r = 1, na_fill = -10000, salida = "/Salidas/TP1/")


##### Bias Modelos #####
# temp
t5.bias = apply(t5.ens - obs_t, c(1,2), sum, na.rm = T)/30
t6.bias = apply(t6.ens - obs_t, c(1,2), sum, na.rm = T)/30

aux = array(t5.bias, c(144,73,1)) 
aux[which(abs(aux)>7)] = NA
mapa(lista = aux, titulo = "Bias Temperatura ensamble CNRM-CM5 1975 - 2005", nombre_fig = "t5.bias"
     , escala = c(-5, 5), label_escala = "ºC", resta = 0, brewer = "RdBu", revert = "si", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(-5, 5, by = 1), breaks_c_f = seq(-5, 5, by = 0.5), r = 1, na_fill = 0, salida = "/Salidas/TP1/")


aux = array(t6.bias, c(144,73,1)) 
aux[which(abs(aux)>7)] = NA
mapa(lista = aux, titulo = "Bias Temperatura ensamble CNRM-CM6 1975 - 2005", nombre_fig = "t6.bias"
     , escala = c(-5, 5), label_escala = "ºC", resta = 0, brewer = "RdBu", revert = "si", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(-5, 5, by = 1), breaks_c_f = seq(-5, 5, by = 0.5), r = 1, na_fill = 0, salida = "/Salidas/TP1/")


# pp (bias %, ojo que aveces da pa la mierda)

aux = (pp5.ens/pp_obs)*100-100
pp.bias5 = apply(aux, c(1,2), sum, na.rm = T)/30

aux = array(pp.bias5, c(144,73,1))
mapa(lista = aux, titulo = "Bias % Precipitación ensamble CNRM-CM5 1975 - 2005", nombre_fig = "pp5.bias"
     , escala = c(-150, 150), label_escala = "", resta = 0, brewer = "BrBG", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(-150, 150, by = 25), breaks_c_f = seq(-150, 150, by = 25), r = 1, na_fill = -10000, salida = "/Salidas/TP1/")



aux = (pp6.ens/pp_obs)*100 - 100
pp.bias6 = apply(aux, c(1,2), sum, na.rm = T)/30

aux = array(pp.bias6, c(144,73,1))
mapa(lista = aux, titulo = "Bias % Precipitación ensamble CNRM-CM6 1975 - 2005", nombre_fig = "pp6.bias"
     , escala = c(-150, 150), label_escala = "", resta = 0, brewer = "BrBG", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(-150, 150, by = 25), breaks_c_f = seq(-150, 150, by = 25), r = 1, na_fill = -10000, salida = "/Salidas/TP1/")


#### Correlacions temporales ####

# temp
aux = apply(tas5_an[[1]], c(1,2,3), mean, na.rm = T)
t5.cor = corr(mod = aux, obs = tas_obs,lon = 144, lat = 73, cf = 0.95)
aux = apply(tas6_an[[1]], c(1,2,3), mean, na.rm = T)
t6.cor = corr(mod = aux, obs = tas_obs,lon = 144, lat = 73, cf = 0.95)


aux = array(t5.cor[,,1]*t5.cor[,,2], c(144,73,1)) 
mapa(lista = aux, titulo = "Correlaciòn temperatura CNRM-CM5 vs observaciones  1975 - 2005", nombre_fig = "t5.cor"
     , escala = c(0, 1), label_escala = "R", resta = 0, brewer = "YlOrRd", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 1, by = 0.1), breaks_c_f = seq(0, 1, by = 0.1), r = 1, na_fill = -10000, salida = "/Salidas/TP1/")


aux = array(t6.cor[,,1]*t6.cor[,,2], c(144,73,1)) 
mapa(lista = aux, titulo = "Correlaciòn temperatura CNRM-CM6 vs observaciones  1975 - 2005", nombre_fig = "t6.cor"
     , escala = c(0, 1), label_escala = "R", resta = 0, brewer = "YlOrRd", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 1, by = 0.1), breaks_c_f = seq(0, 1, by = 0.1), r = 1, na_fill = -10000, salida = "/Salidas/TP1/")



# pp
mask_arr = array(NA, dim = c(144, 73, 30))
for(i in 1:30){
  mask_arr[,,i] = mask
}

aux = apply(pp5_an[[1]], c(1,2,3), mean, na.rm = T)*mask_arr
pp5.cor = corr(mod = aux, obs = pp_obs, lon = 144, lat = 73, cf = 0.95)
aux = apply(pp6_an[[1]], c(1,2,3), mean, na.rm = T)*mask_arr
pp6.cor = corr(mod = aux, obs = pp_obs, lon = 144, lat = 73, cf = 0.95)

aux = array(pp5.cor[,,1]*pp5.cor[,,2], c(144,73,1)) 
mapa(lista = aux, titulo = "Correlaciòn precipitación CNRM-CM5 vs observaciones  1975 - 2005", nombre_fig = "pp5.cor"
     , escala = c(-1, 1), label_escala = "R", resta = 0, brewer = "RdBu", revert = "si", niveles = 9
     , contour = "no", lon = lon, lat = lat, escala_dis = seq(-1, 1, by = 0.2), breaks_c_f = seq(-1, 1, by = 0.2), r = 1, na_fill = 0, salida = "/Salidas/TP1/")

aux = array(pp6.cor[,,1]*pp6.cor[,,2], c(144,73,1)) 
mapa(lista = aux, titulo = "Correlaciòn precipitación CNRM-CM6 vs observaciones  1975 - 2005", nombre_fig = "pp6.cor"
     , escala = c(-1, 1), label_escala = "R", resta = 0, brewer = "RdBu", revert = "si", niveles = 9
     , contour = "no", lon = lon, lat = lat, escala_dis = seq(-1, 1, by = 0.2), breaks_c_f = seq(-1, 1, by = 0.2), r = 1, na_fill = 0, salida = "/Salidas/TP1/")



#### Subregiones ####


aux_lats = list(); aux_lons = list()
aux_lons[[1]] = which((lon>=(-75+360))&(lon<=(-70+360))); aux_lats[[1]] = which((lat>=-50)&(lat<=-35))
aux_lons[[2]] = which((lon>=(-60+360))&(lon<=(-50+360))); aux_lats[[2]] = which((lat>=-35)&(lat<=-22.5))
aux_lons[[3]] = which((lon>=(-170+360))&(lon<=(-120+360))); aux_lats[[3]] = which((lat>=-5)&(lat<=5))

#dejo los nombres horribles

regiones_means_t5 = list(); regiones_means_t6 = list()
regiones_means_pp5 = list(); regiones_means_pp6 = list()

regiones_sd_t5 = list(); regiones_sd_t6 = list()
regiones_sd_pp5 = list(); regiones_sd_pp6 = list()

regiones_bias_t5 = list(); regiones_bias_t6 = list()
regiones_bias_pp5 = list(); regiones_bias_pp6 = list()

for(i in 1:3){
  
  regiones_means_t5[[i]] = t5.med[aux_lons[[i]], aux_lats[[i]]] 
  regiones_means_t6[[i]] = t6.med[aux_lons[[i]], aux_lats[[i]]] 
  
  regiones_means_pp5[[i]] = pp5.med[aux_lons[[i]], aux_lats[[i]]] 
  regiones_means_pp6[[i]] = pp6.med[aux_lons[[i]], aux_lats[[i]]] 
  
  regiones_sd_t5[[i]] = t5.sd[aux_lons[[i]], aux_lats[[i]]]
  regiones_sd_t6[[i]] = t6.sd[aux_lons[[i]], aux_lats[[i]]]
  
  regiones_sd_pp5[[i]] = pp5.sd[aux_lons[[i]], aux_lats[[i]]]
  regiones_sd_pp6[[i]] = pp6.sd[aux_lons[[i]], aux_lats[[i]]]
  
  regiones_bias_t5[[i]] = (t5.med[aux_lons[[i]], aux_lats[[i]]]) - apply(obs_t[aux_lons[[i]], aux_lats[[i]],], c(1,2), mean, na.rm = T)
  regiones_bias_t6[[i]] = (t6.med[aux_lons[[i]], aux_lats[[i]]]) - apply(obs_t[aux_lons[[i]], aux_lats[[i]],], c(1,2), mean, na.rm = T)
  
  regiones_bias_pp5[[i]] = apply((pp5.med[aux_lons[[i]], aux_lats[[i]]])/apply(pp_obs[aux_lons[[i]], aux_lats[[i]],], c(1,2), mean, na.rm = T)*100 - 100, c(1,2), sum, na.rm = T)/30
  regiones_bias_pp6[[i]] = apply((pp6.med[aux_lons[[i]], aux_lats[[i]]])/apply(pp_obs[aux_lons[[i]], aux_lats[[i]],], c(1,2), mean, na.rm = T)*100 - 100, c(1,2), sum, na.rm = T)/30
  
  
}

aux = array(data = regiones_means_t6[[1]], dim = c(3, 7, 1))
aux2 = array(data = regiones_means_t6[[2]], dim = c(5, 6, 1))
aux3 = array(data = regiones_means_t6[[3]], dim = c(21, 5, 1))

mapa_reg(lista = aux, lista2 = aux2, lista3 = aux3, titulo = "Temperatura media ensamble CNRM-CM6 1975 - 2005", nombre_fig = "t6_regiones_means"
         , escala = c(0, 30), label_escala = "ºC", resta = 0,revert = "no", brewer = "YlOrRd", niveles = 9
         ,  lons = lon, lats = lat, aux_lons = aux_lons, aux_lats = aux_lats, 
         escala_dis = seq(0, 30, by = 2 ), r = 1, na_fill = 0, salida = "/Salidas/TP1/")


aux = array(data = regiones_sd_t6[[1]], dim = c(3, 7, 1))
aux2 = array(data = regiones_sd_t6[[2]], dim = c(5, 6, 1))
aux3 = array(data = regiones_sd_t6[[3]], dim = c(21, 5, 1))

mapa_reg(lista = aux, lista2 = aux2, lista3 = aux3, titulo = "Desvio T ensamble CNRM-CM6 1975 - 2005", nombre_fig = "t6.sd_regiones"
         , escala = c(0, 0.4), label_escala = "", resta = 0,revert = "no", brewer = "YlOrRd", niveles = 9
         ,  lons = lon, lats = lat, aux_lons = aux_lons, aux_lats = aux_lats, 
         escala_dis = seq(0, 0.4, by = 0.05), r = 1, na_fill = 0, salida = "/Salidas/TP1/")



aux = array(data = regiones_bias_t6[[1]], dim = c(3, 7, 1))
aux2 = array(data = regiones_bias_t6[[2]], dim = c(5, 6, 1))
aux3 = array(data = regiones_bias_t6[[3]], dim = c(21, 5, 1))

mapa_reg(lista = aux, lista2 = aux2, lista3 = aux3, titulo = "Bias T ensamble CNRM-CM6 1975 - 2005", nombre_fig = "t6.bias_regiones"
         , escala = c(-2, 2), label_escala = "", resta = 0,revert = "si", brewer = "RdBu", niveles = 9
         ,  lons = lon, lats = lat, aux_lons = aux_lons, aux_lats = aux_lats, 
         escala_dis = seq(-2, 2, by = 0.5), r = 1, na_fill = 0, salida = "/Salidas/TP1/")

aux = array(data = regiones_means_pp6[[1]], dim = c(3, 7, 1))
aux2 = array(data = regiones_means_pp6[[2]], dim = c(5, 6, 1))
aux3 = array(data = regiones_means_pp6[[3]], dim = c(21, 5, 1))

mapa_reg(lista = aux, lista2 = aux2, lista3 = aux3, titulo = "Precipitacíón media ensamble CNRM-CM6 1975 - 2005", nombre_fig = "pp6_regiones_means"
         , escala = c(500, 3000), label_escala = "ºC", resta = 0,revert = "no", brewer = "YlGnBu", niveles = 9
         ,  lons = lon, lats = lat, aux_lons = aux_lons, aux_lats = aux_lats, 
         escala_dis = seq(500, 3000, by = 200 ), r = 1, na_fill = 0, salida = "/Salidas/TP1/")

aux = array(data = regiones_sd_pp6[[1]], dim = c(3, 7, 1))
aux2 = array(data = regiones_sd_pp6[[2]], dim = c(5, 6, 1))
aux3 = array(data = regiones_sd_pp6[[3]], dim = c(21, 5, 1))

mapa_reg(lista = aux, lista2 = aux2, lista3 = aux3, titulo = "Desvio PP ensamble CNRM-CM6 1975 - 2005", nombre_fig = "pp6.sd_regiones"
         , escala = c(0, 250), label_escala = "", resta = 0,revert = "no", brewer = "YlGnBu", niveles = 9
         ,  lons = lon, lats = lat, aux_lons = aux_lons, aux_lats = aux_lats, 
         escala_dis = seq(0, 2510, by = 25), r = 1, na_fill = 0, salida = "/Salidas/TP1/")


#### series temporales ####
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
anom_t_obs_oce = apply(sst_obs*mask_arr2[,,,1],c(3), mean, na.rm = T) - mean(sst_obs*mask_arr2[,,,1], na.rm = T)

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


# en las regiones
anom_t_obs_reg = list()
anom_t5_reg = list()
anom_t6_reg = list()

anom_pp5_reg = list()
anom_pp6_reg = list()
anom_pp_obs_reg = list()

# mantengo la notacion horrible
for(i in 1:3){
  
  anom_t_obs_reg[[i]] =  apply(obs_t[aux_lons[[i]], aux_lats[[i]],],c(3), mean, na.rm = T) - mean(obs_t[aux_lons[[i]], aux_lats[[i]],], na.rm = T)
  anom_t5_reg[[i]] = apply(tas5_an[[1]][aux_lons[[i]], aux_lats[[i]],,], c(3), mean, na.rm = T) - mean(tas5_an[[1]][aux_lons[[i]], aux_lats[[i]],,], na.rm = T)
  anom_t6_reg[[i]] = apply(tas6_an[[1]][aux_lons[[i]], aux_lats[[i]],,], c(3), mean, na.rm = T) - mean(tas6_an[[1]][aux_lons[[i]], aux_lats[[i]],,], na.rm = T)
  
  anom_pp_obs_reg[[i]] = apply(pp_obs[aux_lons[[i]], aux_lats[[i]],],c(3), mean, na.rm = T) - mean(pp_obs[aux_lons[[i]], aux_lats[[i]],], na.rm = T)
  anom_pp5_reg[[i]] = apply(pp5_an[[1]][aux_lons[[i]], aux_lats[[i]],,]*mask_arr[aux_lons[[i]],aux_lats[[i]],,1:9], c(3), mean, na.rm = T) - mean(pp5_an[[1]][aux_lons[[i]], aux_lats[[i]],,]*mask_arr[aux_lons[[i]],aux_lats[[i]],,1:9], na.rm = T)
  anom_pp6_reg[[i]] = apply(pp6_an[[1]][aux_lons[[i]], aux_lats[[i]],,]*mask_arr[aux_lons[[i]],aux_lats[[i]],,], c(3), mean, na.rm = T) - mean(pp6_an[[1]][aux_lons[[i]], aux_lats[[i]],,]*mask_arr[aux_lons[[i]],aux_lats[[i]],,], na.rm = T)
  
  
  # ej
  
  #anom_pp_obs_sesa 
  #anom_pp5_sesa
  #anom_pp6_sesa 
}


grafico_ts(data1 = anom_t_obs, data2 = anom_t5, data3 = anom_t6, nombre_fig = "/home/auri/Facultad/Materias/c-dinamica/TPs/Salidas/TP1/anom.t.ts.png",
           titulo = "Anomalía T 1975 - 2005", ylab = "ºC")

grafico_ts(data1 = anom_t_obs_cont, data2 = anom_t5_cont, data3 = anom_t6_cont, nombre_fig = "/home/auri/Facultad/Materias/c-dinamica/TPs/Salidas/TP1/anom.t_cont.ts.png",
           titulo = "Anomalía T continental 1975 - 2005", ylab = "ºC")

grafico_ts(data1 = anom_t_obs_oce, data2 = anom_t5_oce, data3 = anom_t6_oce, nombre_fig = "/home/auri/Facultad/Materias/c-dinamica/TPs/Salidas/TP1/anom.t_oce.ts.png",
           titulo = "Anomalía T oceanica 1975 - 2005", ylab = "ºC")

grafico_ts(data1 = anom_pp_obs, data2 = anom_pp5, data3 = anom_pp6, nombre_fig = "/home/auri/Facultad/Materias/c-dinamica/TPs/Salidas/TP1/anom.pp.ts.png",
           titulo = "Anomalía PP 1975 - 2005", ylab = "mm")

grafico_ts(data1 = anom_pp_obs, data2 = anom_pp5, data3 = anom_pp6, nombre_fig = "/home/auri/Facultad/Materias/c-dinamica/TPs/Salidas/TP1/anom.pp.ts.png",
           titulo = "Anomalía PP continental 1975 - 2005", ylab = "mm")


titulos = c("Anomalia T SAn", "Anomalía T SSESA", "Anomalia T Niño3.4")
nombres_figs = c("anom.t.ts_SAn.png", "anom.t.ts_SESA.png", "anom.t.ts_Niño3.4.png")

titulos_pp = c("Anomalia PP SAn", "Anomalía PP SSESA", "Anomalia PP Niño3.4")
nombres_figs_pp = c("anom.pp.ts_SAn.png", "anom.pp.ts_SESA.png", "anom.pp.ts_Niño3.4.png")

for(i in 1:3){
  
  grafico_ts(data1 = anom_t_obs_reg[[i]], data2 = anom_t5_reg[[i]], data3 = anom_t6_reg[[i]],
             nombre_fig = paste("/home/auri/Facultad/Materias/c-dinamica/TPs/Salidas/TP1/",nombres_figs[i], sep = ""), titulo = titulos[i], ylab = "ºC")
  
  grafico_ts(data1 = anom_pp_obs_reg[[i]], data2 = anom_pp5_reg[[i]], data3 = anom_pp6_reg[[i]], 
             nombre_fig =  paste("/home/auri/Facultad/Materias/c-dinamica/TPs/Salidas/TP1/",nombres_figs_pp[i], sep = ""), titulo = titulos_pp[i], ylab = "mm")
  
}




#### MAE ####
# solo ensambles
# (apply(abs(prom_est_mods_t - prom_est_obs[,,,,1]), c(1,2,4), sum, na.rm = T)/29) 

t5.mae = apply(abs(t5.ens - obs_t), c(1,2), sum, na.rm = T)/30
t6.mae = apply(abs(t6.ens - obs_t), c(1,2), sum, na.rm = T)/30


aux = array(data = t5.mae, c(144, 73, 1))
mapa(lista = aux, titulo = "Error absoluto medio T CNRM-CM5 1975 - 2005", nombre_fig = "t5.mae_escala1"
     , escala = c(0, 5), label_escala = "ºC", resta = 0, brewer = "Reds", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 5, by = 1), breaks_c_f = seq(0, 5, by = 1), r = 1,na_fill = -10000, salida = "/Salidas/TP1/")


aux = array(data = t6.mae, c(144, 73, 1))
mapa(lista = aux, titulo = "Error absoluto medio T CNRM-CM6 1975 - 2005", nombre_fig = "t6.mae_escala1"
     , escala = c(0, 5), label_escala = "ºC", resta = 0, brewer = "Reds", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 5, by = 1), breaks_c_f = seq(0, 5, by = 1), r = 1,na_fill = -10000, salida = "/Salidas/TP1/")



# MAPE PP # da muy raro, valores muuy chicos

pp5.mae = ((apply((abs(pp5.ens - pp_obs)/abs(pp_obs))*100, c(1,2), sum, na.rm = T))/30)*mask
pp6.mae = ((apply((abs(pp6.ens - pp_obs)/abs(pp_obs))*100, c(1,2), sum, na.rm = T))/30)*mask

aux = array(data = pp5.mae, c(144, 73, 1))
mapa(lista = aux, titulo = "Error absoluto medio % pp CNRM-CM5 1975 - 2005", nombre_fig = "pp5.mape"
     , escala = c(0, 150), label_escala = "", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 7
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 150, by = 25), breaks_c_f = seq(0, 150, by = 25), r = 1,na_fill = -10000, salida = "/Salidas/TP1/")

aux = array(data = pp6.mae, c(144, 73, 1))
mapa(lista = aux, titulo = "Error absoluto medio % pp CNRM-CM6 1975 - 2005", nombre_fig = "pp6.mape"
     , escala = c(0, 150), label_escala = "", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 7
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 150, by = 25), breaks_c_f = seq(0, 150, by = 25), r = 1,na_fill = -10000, salida = "/Salidas/TP1/")

#### Promedios longitudinales ####
t.obs_prom.lat = apply(obs_t, c(2), mean, na.rm = T)

t.5_prom.lat = apply(t5.ens, c(2), mean, na.rm = T)

t.6_prom.lat = apply(t6.ens, c(2), mean, na.rm = T)


aux = matrix(data = NA, nrow = 73, 3)

aux[,1] = t.obs_prom.lat
aux[,2] = t.5_prom.lat
aux[,3] = t.6_prom.lat
aux2 = data.frame(seq(-90,90, by = 2.5), aux)

colnames(aux2) = c("Latitud", "Observado", "CNRM-CM5", "CNRM-CM6") # esto puede no ser necesario.

df.ts = ts(aux2[-1], start = -90, end = 90, frequency = 0.4)

png(filename = "/home/auri/Facultad/Materias/c-dinamica/TPs/Salidas/TP1/t.prom_lat.png", width = 850, height = 520, units = "px")
plot(df.ts, plot.type = "single", col = 1:ncol(df.ts), lwd = 3, ylab = "ºC", main = "Promedio Zonal T", xlab = "Latitud")
abline(v = -75, lwd = 0.5)
legend("bottomright", colnames(df.ts), col=1:ncol(aux2), lty=1, cex=2, lwd = 3)
dev.off()

###########\m/########### (2hs)