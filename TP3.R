#TP3 Energia 
library(ncdf4)
# carga de datos.
#### Observaciones ####

# Temperatura
aux = nc_open(paste(getwd(), "/TP3datos.obs/tmp.mon.mean.nc", sep = ""))
t.obs = ncvar_get(aux, "air")
lon.obs = ncvar_get(aux, "lon")
lat.obs = ncvar_get(aux, "lat")
nc_close(aux)

# rotando
t.obs = t.obs[,ncol(t.obs):1,]

t.obs_seasons = open_ncobs.tp3(t.obs)
t.obs_anual = t.obs[,,13:372] # sacando 1975

# humedad 

aux = nc_open(paste(getwd(), "/TP3datos.obs/rhum.mon.mean.nc", sep = ""))
hu.obs = ncvar_get(aux, "rhum")
nc_close(aux)

# rotando
hu.obs = hu.obs[,ncol(hu.obs):1,]

hu.obs_seasons = open_ncobs.tp3(hu.obs)
hu.obs_anual = hu.obs[,,13:372]

# hay presion.. nse para que.

##### modelos: temperatura, humedad y etp ####
ruta_mods = "RDatas/TP3.RDatas/"
# los RData son listas:
# dim(v[[1]]) = [,,4,r] (seasons y miembros de ensamble, 10 o 6); 
# dim(v[[2]]) = [,,4,29,r] (seasons, anios y miembros de ensamble)
# dim(v[[3]]) = [,,360] ( todos los meses en anios completos)

# CARGAR CADA UNO CUANDO SEA NECESARIO, TODOS LOS RDATAS JUNTOS SON MUY PESADOS. 
#--- HISTORICO ----#
# Temperatura #
load("RDatas/TP3.RDatas/t6.his.RData")
# Humedad #
load("RDatas/TP3.RDatas/hu6.his.RData")


#--- RCP2.6 ---#
# Temperatura #
# CNRM-CM6
load("RDatas/TP3.RDatas/t6.26_2049.RData")
load("RDatas/TP3.RDatas/t6.26_2099.RData")
# Humedad #
# CNRM-CM6
load("RDatas/TP3.RDatas/hu6.26_2049.RData")
load("RDatas/TP3.RDatas/hu6.26_2099.RData")

#--- RCP8.5 ---#
# Temperatura #
load("RDatas/TP3.RDatas/t6.85_2049.RData")
load("RDatas/TP3.RDatas/t6.85_2099.RData")

# Humedad #
load("RDatas/TP3.RDatas/hu6.85_2049.RData")
load("RDatas/TP3.RDatas/hu6.85_2099.RData")

# etp #
# solo datos anuales!, a priori no hay hacer nada. luego hacer los ensambles
load("RDatas/TP3.RDatas/etp6.his_an.RData")
load("RDatas/TP3.RDatas/etp6.26_49.RData")
load("RDatas/TP3.RDatas/etp6.26_99.RData")
load("RDatas/TP3.RDatas/etp6.85_49.RData")
load("RDatas/TP3.RDatas/etp6.85_99.RData")

#### entalpia ####
# segun el coso..
# H = Ha + Hv  
# Ha = 1.007*T[ºC] - 0.026
# Hv = q[kg/kg]*(2050[kJ/kg] - 0.538*T[ºC]) ¿¿¿??? es un 0.538 magico
# H = (1.007*T[ºC] - 0.026) + q[kg/kg]*(2050[kJ/kg] - 0.538*T[ºC])

#Ha.
# q unidad? dice "1" por la magnitud parece gr/kg, aunque hay valores bastante altos 
load("RDatas/TP3.RDatas/t6.his.RData")
load("RDatas/TP3.RDatas/hu6.his.RData")
t.aux = t6.his[[1]]
hu.aux = hu6.his[[1]]
rm(t6.his, hu6.his)

Ha = 1.007*(t.aux-273) - 0.026 
Hv = hu.aux[,,,1:10]*(2502 - 0.538*(t.aux-273))





