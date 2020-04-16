# TP1
#rm(list = ls())
library(ncdf4)

source("FUNCIONES.R")

lon = read.table("lon.txt")[,1]
lat = read.table("lat.txt")[,1]

# cnrm5
# anual

# determinar el file_pattern "a mano"
tas_an = open_nc(file_pattern = "tas_Amon_CNRM-CM5_historical_r*_197601-200512_2.5_anu.nc"
                 , model = "cnrm-cm5", members = 9, variable = "tas", mes_anual = "anual")

## CORRIDAS ANUALES
# campo medio T
tas_mean_an = apply(tas_an, c(1,2,4), mean, na.rm = T)

mask = as.matrix(read.table("mask.txt"))
mask_arr = array(NA, dim = c(length(lon), length(lat), 9))
for(i in 1:9){
  mask_arr[,,i] = mask
}

mapa(lista = tas_mean_an*mask_arr, titulo = "Promedio T anual", nombre_fig = "prom_t_an", escala = c(-30, 30)
     ,label_escala = "C", resta = 273, brewer = "RdBu", revert = "si", niveles = 11
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(-30, 30, by = 5)
     , breaks_c_f = seq(-30, 30, by = 5), r = 9, salida = "/Salidas/TP1/")


# campo medio pp 

precip_an = open_nc(file_pattern = "pr_Amon_CNRM-CM5_historical_r*_197601-200512_2.5_anu.nc"
                    , model = "cnrm-cm5", members = 9, variable = "pr", mes_anual = "anual") 
precip_mean_an = apply(precip_an, c(1,2,4), mean, na.rm = T)

# !! eleccion de escala... valores muy altos
mapa(lista = precip_mean_an*mask_arr, titulo = "Promedio PP anual", nombre_fig = "prom_pp_an", escala = c(0, 4600)
     ,label_escala = "C", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 4500, by = 250)
     , breaks_c_f = seq(0, 4500, by = 150), r = 9, salida = "/Salidas/TP1/")


# mensual
# VER: DISTINTA CANTIDAD DE MIEMBROS Y NOMBRES REPETIDOS ?


## Observaciones ##
# temp
ruta = getwd()
aux = nc_open(paste(ruta, "/Datos_Obs1/tmp_cru_ts3.20_197601-200512_2.5_anu.nc", sep = ""))
tas_obs = ncvar_get(aux, "tmp")
nc_close(aux)

tas_obs_mean_an = apply(tas_obs, c(1,2), mean, na.rm = T)

# solo para evitar modificar funcion
aux = array(data = NA, dim = c(144, 73, 1))
aux[,,1] = tas_obs_mean_an

mapa(lista = aux, titulo = "Promedio T obs anual", nombre_fig = "prom_t_obs_an", escala = c(-30, 30)
     ,label_escala = "C", resta = 0, brewer = "RdBu", revert = "si", niveles = 11
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(-30, 30, by = 5)
     , breaks_c_f = seq(-30, 30, by = 5), r = 1, salida = "/Salidas/TP1/")


# pp
aux = nc_open(paste(ruta, "/Datos_Obs1/precip.mon.total.v7_197601-200512_2.5_anu.nc", sep = ""))
pp_obs = ncvar_get(aux, "precip")
nc_close(aux)

pp_obs_mean_an = apply(pp_obs, c(1,2), mean, na.rm = T)
aux = array(data = NA, dim = c(144, 73, 1))
aux[,,1] = pp_obs_mean_an

mapa(lista = precip_mean_an*mask_arr, titulo = "Promedio PP obs anual", nombre_fig = "prom_pp_obs_an", escala = c(0, 2500)
     ,label_escala = "C", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 2500, by = 250)
     , breaks_c_f = seq(0, 2500, by = 150), r = 1, salida = "/Salidas/TP1/")


