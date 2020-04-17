# TP1
#rm(list = ls())
library(ncdf4)
library(fields)

source("FUNCIONES.R")

lon = read.table("lon.txt")[,1]
lat = read.table("lat.txt")[,1]


mask = as.matrix(read.table("mask.txt"))
mask_arr = array(NA, dim = c(length(lon), length(lat), 9))
for(i in 1:9){
  mask_arr[,,i] = mask
}


### Observaciones ###
#-------------------------------- temp - historico --------------------------------# 
ruta = getwd()
aux = nc_open(paste(ruta, "/Datos_Obs1/tmp_cru_ts3.20_197601-200512_2.5_anu.nc", sep = ""))
tas_obs = ncvar_get(aux, "tmp")
nc_close(aux)

# campo medio T observado 1975 - 2005
tas_obs_mean_an = apply(tas_obs, c(1,2), mean, na.rm = T)

# sd
sd_tas_obs_anu = array(NA, dim = c(144, 73,1))
sd_tas_obs_anu[,,1] = apply(tas_obs, c(1,2), sd, na.rm = T)


############################################ GRAFICOS ############################################
aux = array(data = NA, dim = c(144, 73, 1)) # solo para evitar modificar funcion
aux[,,1] = tas_obs_mean_an
mapa(lista = aux, titulo = "Promedio T obs anual", nombre_fig = "prom_t_obs_an", escala = c(-30, 30)
     ,label_escala = "C", resta = 0, brewer = "RdBu", revert = "si", niveles = 11
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(-30, 30, by = 5)
     , breaks_c_f = seq(-30, 30, by = 5), r = 1, salida = "/Salidas/TP1/")
##################################################################################################


#-------------------------------- precip - historico --------------------------------#
aux = nc_open(paste(ruta, "/Datos_Obs1/precip.mon.total.v7_197601-200512_2.5_anu.nc", sep = ""))
pp_obs = ncvar_get(aux, "precip")
nc_close(aux)

pp_obs_mean_an = apply(pp_obs, c(1,2), mean, na.rm = T)
sd_pp_obs_anual = array(data = NA, dim = c(144, 73, 1))
sd_pp_obs_anual = apply(pp_obs, c(1,2), sd, na.rm = T)


############################################ GRAFICOS ############################################
aux = array(data = NA, dim = c(144, 73, 1))
aux[,,1] = pp_obs_mean_an
mapa(lista = precip_mean_an*mask_arr, titulo = "Promedio PP obs anual", nombre_fig = "prom_pp_obs_an", escala = c(0, 2500)
     ,label_escala = "C", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 2500, by = 250)
     , breaks_c_f = seq(0, 2500, by = 150), r = 1, salida = "/Salidas/TP1/")
##################################################################################################







### CNRM-CM5 ###

#-------------------------------- tas - historical - anual --------------------------------#

tas_an = open_nc(file_pattern = "tas_Amon_CNRM-CM5_historical_r*_197601-200512_2.5_anu.nc"
                 , model = "cnrm-cm5", members = 9, variable = "tas", mes_anual = "anual")

# campo medio T 1976 - 2005 por miembros
tas_mean_an = apply(tas_an, c(1,2,4), mean, na.rm = T)

# sd - por miembros
sd_tas = array(NA, dim = c(144, 73,1))
sd_tas[,,1] = apply(tas_an, c(1,2), sd, na.rm = T)


############################################ GRAFICOS ############################################
mapa(lista = tas_mean_an*mask_arr, titulo = "Promedio T anual", nombre_fig = "prom_t_an", escala = c(-30, 30)
     ,label_escala = "C", resta = 273, brewer = "RdBu", revert = "si", niveles = 11
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(-30, 30, by = 5)
     , breaks_c_f = seq(-30, 30, by = 5), r = 9, salida = "/Salidas/TP1/")


mapa(lista = sd_tas, titulo = "sd T anual 1976 - 2005", nombre_fig = "sd_t_an", escala = c(0, 2.5)
     ,label_escala = "ºC", resta = 0, brewer = "YlOrRd", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 2.5, by = 0.25)
     , breaks_c_f = seq(0, 2.5, by = 0.5), r = 1, salida = "/Salidas/TP1/")
##################################################################################################



#-------------------------------- pr - historical - anual --------------------------------#

precip_an = open_nc(file_pattern = "pr_Amon_CNRM-CM5_historical_r*_197601-200512_2.5_anu.nc"
                    , model = "cnrm-cm5", members = 9, variable = "pr", mes_anual = "anual") 
precip_mean_an = apply(precip_an[[1]], c(1,2,4), mean, na.rm = T)

sd_precip_an = array(NA, dim = c(144, 73,1))
sd_precip_an[,,1] = apply(precip_an[[1]], c(1,2), sd, na.rm = T)*mask


############################################ GRAFICOS ############################################
# !! eleccion de escala... valores muy altos
mapa(lista = precip_mean_an*mask_arr, titulo = "Promedio PP anual", nombre_fig = "prom_pp_an", escala = c(0, 4600)
     ,label_escala = "C", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 4500, by = 250)
     , breaks_c_f = seq(0, 4500, by = 150), r = 9, salida = "/Salidas/TP1/")


# ver. escala.
mapa(lista = sd_precip_an, titulo = "sd PP anual", nombre_fig = "sd_pp_an", escala = c(0, 500)
     ,label_escala = "C", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 500, by = 100)
     , breaks_c_f = seq(0, 500, by = 50), r = 1, salida = "/Salidas/TP1/")
##################################################################################################



#-------------------------------- pr - historical - mensual --------------------------------#
pp_mm = open_nc(file_pattern = "pr_Amon_CNRM-CM5_historical_r*_197601-200512_2.5_mes.nc"
                 , model = "cnrm-cm5", members = 9, variable = "pr", mes_anual = "mes")



pp_prom_est = array(data = NA, dim = c(144, 73 ,4))
for( i in 1:4){
  pp_prom_est[,,i] = apply(pp_mm[[1]][,,i,], c(1,2), mean)
}


