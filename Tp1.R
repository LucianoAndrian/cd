# TP1
#rm(list = ls())
library(ncdf4)
library(fields)

source("FUNCIONES.R")

lon = read.table("lon.txt")[,1]
lat = read.table("lat.txt")[,1]

# esto no anda ahora... porque? no hay porque
#topo = metR::GetTopography(0, 180, 90,  -90, resolution = 1/2) # mapa topografia
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
tas_obs[which(is.na(tas_obs))]=1  # aun quedan puntos que no se anularon

obs = tas_obs*sst_obs; obs[which(obs > 50)] = NA # datos obs. continentes y oceanos

## > pp es solamente contienental < ##


##### Calculos #####

ensamble_tas5_an = apply(tas5_an[[1]], c(1,2,3), mean, na.rm = T)
ensamble_pp5_an = apply(pp5_an[[1]], c(1,2,3), mean, na.rm = T)
ensamble_tas6_an = apply(tas6_an[[1]], c(1,2,3), mean, na.rm = T)
ensamble_pp6_an = apply(pp6_an[[1]], c(1,2,3), mean, na.rm = T)

# campos medioas r y ensamble

mods = list()
mods[[1]] = tas5_an[[1]]; mods[[2]] = tas6_an[[1]]; mods[[3]] = pp5_an[[1]]; mods[[4]] = pp6_an[[1]]
mods[[5]] = ensamble_tas5_an; mods[[6]] = ensamble_tas6_an
mods[[7]] = ensamble_pp5_an; mods[[8]] = ensamble_pp6_an

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

#tas5_an_mean = apply(tas5_an[[1]], c(1,2), mean, na.rm = T) 
#tas5_an_r_means = apply(tas5_an[[1]], c(1,2,4), mean, na.rm = T)
#tas6_an_mean = apply(tas6_an[[1]], c(1,2), mean, na.rm = T) 
#tas6_an_r_means = apply(tas6_an[[1]], c(1,2,4), mean, na.rm = T) 
#pp5_an_mean = apply(pp5_an[[1]], c(1,2), mean, na.rm = T)
#pp5_an_r_means = apply(pp5_an[[1]], c(1,2,4), mean, na.rm = T)
#pp6_an_mean = apply(pp6_an[[1]], c(1,2), mean, na.rm = T)
#pp6_an_r_means = apply(pp6_an[[1]], c(1,2,4), mean, na.rm = T)

# SD
#sd5_ens_tas = apply(ensamble_tas5_an, c(1,2), sd, na.rm = T)
#sd5_r_tas = apply(tas5_an_r_means, c(1,2), sd, na.rm = T)
#sd6_ens_tas = apply(ensamble_tas6_an, c(1,2), sd, na.rm = T)
#sd6_r_tas = apply(tas6_an_r_means, c(1,2), sd, na.rm = T)
#sd5_ens_pp = apply(ensamble_pp5_an, c(1,2), sd, na.rm = T)
#sd5_r_pp = apply(pp5_an_r_means, c(1,2), sd, na.rm = T)
#sd6_ens_pp = apply(ensamble_pp6_an, c(1,2), sd, na.rm = T)
#sd6_r_pp = apply(pp6_an_r_means, c(1,2), sd, na.rm = T)


# Bias
# Corr
# subregiones

#yapa
# estaciones pp
# para esto hay q ver si es posoible interpolar la grilla, o mas bien "trasladar"





# esto puede servir despues.
lat=which((latitud>=-60)&(latitud<=15))
lon=which((longitud>=275)&(longitud<=330))

