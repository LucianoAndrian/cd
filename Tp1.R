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
tas_obs[which(is.na(tas_obs))]= 1  # aun quedan puntos que no se anularon

obs_t = tas_obs*sst_obs; obs_t[which(obs_t > 50)] = NA # datos obs. continentes y oceanos
obs_t[which(obs_t == 1)] = NA 

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
bias5_pp_ens =  (calc_means[[5]]*mask) - apply(pp_obs, c(1,2), mean, na.rm = T)

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
bias6_pp_ens =  (calc_means[[7]]*mask) - apply(pp_obs, c(1,2), mean, na.rm = T)

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


#yapa
# estaciones pp
# para esto hay q ver si es posoible interpolar la grilla, o mas bien "trasladar"





# esto puede servir despues.
lat=which((latitud>=-60)&(latitud<=15))
lon=which((longitud>=275)&(longitud<=330))

