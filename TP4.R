# TP4 Balance de Masa ( E y P)
# trabjar con RDatas guardados en TP4_apertura_y_seleccion.R
source("FUNCIONES.R")

# para pesar los promedios por la latitud
lat = read.table("lat.txt")
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30))

#### TABLA 7.1 ####

# CNRM-CM5
# Historical
load("RDatas/TP4.RDatas/pp5.his.RData"); load("RDatas/TP4.RDatas/evap5.his.RData") 
pp = pp5.his[[1]]  # prueba...(ram)
evap = evap5.his[[1]]
pp5.his_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap5.his_ens = apply(evap, c(1,2,3), mean, na.rm = T)

# serie temporales (solo para ver el balance global)
pp5.his.ts = apply(pp5.his_ens*lats, c(3), mean, na.rm = T)
evap5.his.ts = apply(evap5.his_ens*lats, c(3), mean, na.rm = T)

Tabla7.1(pp = pp, evap = evap, nombre = "5his", salida = "/Salidas/TP4/")

# RCP2.6
# 2020 - 2049
load("RDatas/TP4.RDatas/pp5.26_49.RData"); load("RDatas/TP4.RDatas/evap5.26_49.RData") 

pp = pp5.26_49[[1]]  # prueba...(ram)
evap = evap5.26_49[[1]]

pp5.26_49_ens = apply(pp, c(1,2,3), mean, na.rm = T)   
evap5.26_49_ens = apply(evap, c(1,2,3), mean, na.rm = T)

pp5.26_49.ts = apply(pp5.26_49_ens*lats, c(3), mean, na.rm = T)
evap5.26_49.ts = apply(evap5.26_49_ens*lats, c(3), mean, na.rm = T)

Tabla7.1(pp = pp, evap = evap, nombre = "5.26_49", salida = "/Salidas/TP4/")

# 2070 - 2099
load("RDatas/TP4.RDatas/pp5.26_99.RData"); load("RDatas/TP4.RDatas/evap5.26_99.RData") 

pp = pp5.26_99[[1]]  # prueba...(ram)
evap = evap5.26_99[[1]]

pp5.26_99_ens = apply(pp, c(1,2,3), mean, na.rm = T)   
evap5.26_99_ens = apply(evap, c(1,2,3), mean, na.rm = T)

pp5.26_99.ts = apply(pp5.26_99_ens*lats, c(3), mean, na.rm = T)
evap5.26_99.ts = apply(evap5.26_99_ens*lats, c(3), mean, na.rm = T)

Tabla7.1(pp = pp, evap = evap, nombre = "5.26_99", salida = "/Salidas/TP4/")


# RCP8.5
# 2020 - 2049
load("RDatas/TP4.RDatas/pp5.85_49.RData"); load("RDatas/TP4.RDatas/evap5.85_49.RData") 

pp = pp5.85_49[[1]]  # prueba...(ram)
evap = evap5.85_49[[1]]

pp5.85_49_ens = apply(pp, c(1,2,3), mean, na.rm = T)   
evap5.85_49_ens = apply(evap, c(1,2,3), mean, na.rm = T)

pp5.85_49.ts = apply(pp5.85_49_ens*lats, c(3), mean, na.rm = T)
evap5.85_49.ts = apply(evap5.85_49_ens*lats, c(3), mean, na.rm = T)

Tabla7.1(pp = pp, evap = evap, nombre = "5.85_49", salida = "/Salidas/TP4/")

# 2070 - 2099
load("RDatas/TP4.RDatas/pp5.85_99.RData"); load("RDatas/TP4.RDatas/evap5.85_99.RData") 

pp = pp5.85_99[[1]]  # prueba...(ram)
evap = evap5.85_99[[1]]

pp5.85_99_ens = apply(pp, c(1,2,3), mean, na.rm = T)  
evap5.85_99_ens = apply(evap, c(1,2,3), mean, na.rm = T)

pp5.85_99.ts = apply(pp5.85_99_ens*lats, c(3), mean, na.rm = T)
evap5.85_99.ts = apply(evap5.85_99_ens*lats, c(3), mean, na.rm = T)

Tabla7.1(pp = pp, evap = evap, nombre = "5.85_99", salida = "/Salidas/TP4/")



# CNRM-CM6
# Historical
load("RDatas/TP4.RDatas/pp6.his.RData"); load("RDatas/TP4.RDatas/evap6.his.RData") 
pp = pp6.his[[1]]  # prueba...(ram)
evap = evap6.his[[1]]
pp6.his_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap6.his_ens = apply(evap, c(1,2,3), mean, na.rm = T)

# serie temporales (solo para ver el balance global)
pp6.his.ts = apply(pp5.his_ens*lats, c(3), mean, na.rm = T)
evap6.his.ts = apply(evap5.his_ens*lats, c(3), mean, na.rm = T)

Tabla7.1(pp = pp, evap = evap, nombre = "6his", salida = "/Salidas/TP4/")

# SSP126
# 2020 - 2049
load("RDatas/TP4.RDatas/pp6.26_49.RData"); load("RDatas/TP4.RDatas/evap6.26_49.RData") 

pp = pp6.26_49[[1]]  # prueba...(ram)
evap = evap6.26_49[[1]]

pp6.26_49_ens = apply(pp, c(1,2,3), mean, na.rm = T)   
evap6.26_49_ens = apply(evap, c(1,2,3), mean, na.rm = T)

pp6.26_49.ts = apply(pp6.26_49_ens*lats, c(3), mean, na.rm = T)
evap6.26_49.ts = apply(evap6.26_49_ens*lats, c(3), mean, na.rm = T)

Tabla7.1(pp = pp, evap = evap, nombre = "6.26_49", salida = "/Salidas/TP4/")

# 2070 - 2099
load("RDatas/TP4.RDatas/pp6.26_99.RData"); load("RDatas/TP4.RDatas/evap6.26_99.RData") 

pp = pp6.26_99[[1]]  # prueba...(ram)
evap = evap6.26_99[[1]]

pp6.26_99_ens = apply(pp, c(1,2,3), mean, na.rm = T)   
evap6.26_99_ens = apply(evap, c(1,2,3), mean, na.rm = T)

pp6.26_99.ts = apply(pp6.26_99_ens*lats, c(3), mean, na.rm = T)
evap6.26_99.ts = apply(evap6.26_99_ens*lats, c(3), mean, na.rm = T)

Tabla7.1(pp = pp, evap = evap, nombre = "6.26_99", salida = "/Salidas/TP4/")


# SSP585
# 2020 - 2049
load("RDatas/TP4.RDatas/pp6.85_49.RData"); load("RDatas/TP4.RDatas/evap6.85_49.RData") 

pp = pp6.85_49[[1]]  # prueba...(ram)
evap = evap6.85_49[[1]]

pp6.85_49_ens = apply(pp, c(1,2,3), mean, na.rm = T)   
evap6.85_49_ens = apply(evap, c(1,2,3), mean, na.rm = T)

pp6.85_49.ts = apply(pp6.85_49_ens*lats, c(3), mean, na.rm = T)
evap6.85_49.ts = apply(evap6.85_49_ens*lats, c(3), mean, na.rm = T)

Tabla7.1(pp = pp, evap = evap, nombre = "6.85_49", salida = "/Salidas/TP4/")

# 2070 - 2099
load("RDatas/TP4.RDatas/pp6.85_99.RData"); load("RDatas/TP4.RDatas/evap6.85_99.RData") 

pp = pp6.85_99[[1]]  # prueba...(ram)
evap = evap6.85_99[[1]]

pp6.85_99_ens = apply(pp, c(1,2,3), mean, na.rm = T)   
evap6.85_99_ens = apply(evap, c(1,2,3), mean, na.rm = T)

pp6.85_99.ts = apply(pp6.85_99_ens*lats, c(3), mean, na.rm = T)
evap6.85_99.ts = apply(evap6.85_99_ens*lats, c(3), mean, na.rm = T)

Tabla7.1(pp = pp, evap = evap, nombre = "6.85_99", salida = "/Salidas/TP4/")