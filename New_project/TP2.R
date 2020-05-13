# Title     : TODO
# Objective : TODO
# Created by: auri
# Created on: 11/5/20
# TP2 Simulacion del clima 2020 - sistemas monzonicos
# apertura y guardado de datos ya organizados con Apertura_datos_mensuales.R

# source("Apertura_datos_mensuales.R") #tarda 1:30hr...

# NO cargar todos los archivos consume todo la ram.
lon = read.table("lon.txt")[,1]
lat = read.table("lat.txt")[,1]

source("FUNCIONES.R")

mask = as.matrix(read.table("mask.txt"))


# seleccion de latitudes para Asia y SA
lats = list()
lons = list()
lats[[1]] = seq(which(lat == -10),which(lat == 55)); lons[[1]] = seq(which(lon == 40), which(lon == 140))
lats[[2]] = seq(which(lat == -60),which(lat == 20)); lons[[2]] = seq(which(lon == 250), which(lon == 355))


#--- HISTORICO ----#
#### Temperatura ####
#CNRM-CM5 no tiene datos mensuales

# CNRM-CM6


load("RDatas/t6.his.RData")

# usando t6.his[[2]] ya se encuentran promediados los anios (esto da lo mismo q hacerlo sobre cada miembro)
t6.his_seasons = array(data = NA, c(144,73,2))
t6.his_seasons[,,1] = apply(t6.his[[1]][,,2,], c(1,2), mean, na.rm = T) #JJA
t6.his_seasons[,,2] = apply(t6.his[[1]][,,4,], c(1,2), mean, na.rm = T) #DJF


rm(t6.his)



#### Viento ####
# CNRM-CM6
load("RDatas/u6.his.RData")
load("RDatas/v6.his.RData")

# JJA
V.his.jja = array(data = NA, dim = c(144,73,2))
V.his.jja[,,1] = apply(u6.his[[1]][,,2,], c(1,2), mean, na.rm = T)  # u850
V.his.jja[,,2] = apply(v6.his[[1]][,,2,], c(1,2), mean, na.rm = T)  # v850

# DJF
V.his.djf = array(data = NA, dim = c(144,73,2))
V.his.djf[,,1] = apply(u6.his[[1]][,,4,], c(1,2), mean, na.rm = T)  # u850
V.his.djf[,,2] = apply(v6.his[[1]][,,4,], c(1,2), mean, na.rm = T)  # v850

rm(u6.his, v6.his)

##### Precipitacion ####

# CNRM-CM5
load("RDatas/pp5.his.RData")

pp5.his_seasons = array(data = NA, c(144,73,2))
pp5.his_seasons[,,1] = apply(pp5.his[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
pp5.his_seasons[,,2] = apply(pp5.his[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

# CNRM-CM6
load("RDatas/pp6.his.RData")
pp6.his_seasons = array(data = NA, c(144,73,2))
pp6.his_seasons[,,1] = apply(pp6.his[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
pp6.his_seasons[,,2] = apply(pp6.his[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

rm(pp5.his, pp6.his)


#### Humedad ####

# CNRM-CM6
load("RDatas/hu6.his.RData")

hu6.his_seasons = array(data = NA, c(144,73,2))
hu6.his_seasons[,,1] = apply(hu6.his[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
hu6.his_seasons[,,2] = apply(hu6.his[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

rm(hu6.his)


#### GRAFICOS  ####

# esto esta asi nomas...

##--- HISTORICO --##
#--- Asia ---#
#### CNRM-CM5 ####


#--- Temperatura ---#
# no hay


#--- Precipitacion ---#

aux = array(pp5.his_seasons[lons[[1]], lats[[1]],1], c(length(lons[[1]]),length(lats[[1]]),1))
mapa_topo2(lista = aux, titulo = "Precipitación media 1975 - 2005  - JJA", nombre_fig = "probando_pycharm", escala = c(0,600)
           , label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si"
           , lon = lon[lons[[1]]], lat = lat[lats[[1]]], escala_dis = seq(0, 600, by = 50), breaks_c_f = seq(0, 600, by = 50), r = 1, na_fill = -1000
           , topo = "topo1", altura = 1500, salida = "/Salidas/TP2/")

aux = array(pp5.his_seasons[lons[[1]], lats[[1]],2], c(length(lons[[1]]),length(lats[[1]]),1))
mapa_topo2(lista = aux, titulo = "Precipitación media 1975 - 2005  - DFJ", nombre_fig = "pp5.his_asia.djf", escala = c(0,600)
           , label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si"
           , lon = lon[lons[[1]]], lat = lat[lats[[1]]], escala_dis = seq(0, 600, by = 50), breaks_c_f = seq(0, 600, by = 50), r = 1, na_fill = -1000
           , topo = "topo1", altura = 1500, salida = "/Salidas/TP2/")


#### CNRM-CM6 ####
#--- Temperatura ---#
auxv = array(V.his.jja[lons[[1]], lats[[1]],2], c(length(lons[[1]]),length(lats[[1]]),1))
auxu = array(V.his.jja[lons[[1]], lats[[1]],1], c(length(lons[[1]]),length(lats[[1]]),1))
aux = array(t6.his_seasons[lons[[1]], lats[[1]],1], c(length(lons[[1]]),length(lats[[1]]),1))
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Temperatura media CNRM-CM6 1975 - 2005  - JJA", nombre_fig = "t6.his_asia.jja", escala = c(0,35)
          , label_escala = "mm", resta = 273, brewer = "Spectral", revert = "si", niveles = 11, contour = "si"
          , lon = lon[lons[[1]]], lat = lat[lats[[1]]], escala_dis = seq(0, 35, by = 2.5), breaks_c_f = seq(0, 35, by = 2.5), r = 1, na_fill = -1000
          , topo = "topo1", altura = 1500, salida = "/Salidas/TP2/")

auxv = array(V.his.djf[lons[[1]], lats[[1]],2], c(length(lons[[1]]),length(lats[[1]]),1))
auxu = array(V.his.djf[lons[[1]], lats[[1]],1], c(length(lons[[1]]),length(lats[[1]]),1))
aux = array(t6.his_seasons[lons[[1]], lats[[1]],2], c(length(lons[[1]]),length(lats[[1]]),1))
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Temperatura media CNRM-CM6 1975 - 2005  - DJF", nombre_fig = "t6.his_asia.djf", escala = c(0,35)
          , label_escala = "mm", resta = 273, brewer = "Spectral", revert = "si", niveles = 11, contour = "si"
          , lon = lon[lons[[1]]], lat = lat[lats[[1]]], escala_dis = seq(0, 35, by = 2.5), breaks_c_f = seq(0, 35, by = 2.5), r = 1, na_fill = -1000
          , topo = "topo1", altura = 1500, salida = "/Salidas/TP2/")

#--- Precipitacion ---#

auxv = array(V.his.jja[lons[[1]], lats[[1]],2], c(length(lons[[1]]),length(lats[[1]]),1))
auxu = array(V.his.jja[lons[[1]], lats[[1]],1], c(length(lons[[1]]),length(lats[[1]]),1))
aux = array(pp6.his_seasons[lons[[1]], lats[[1]],1], c(length(lons[[1]]),length(lats[[1]]),1))
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Precipitación media CNRM-CM6 1975 - 2005  - JJA", nombre_fig = "pp6.his_asia.jja", escala = c(0,600)
          , label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si"
          , lon = lon[lons[[1]]], lat = lat[lats[[1]]], escala_dis = seq(0, 600, by = 50), breaks_c_f = seq(0, 600, by = 50), r = 1, na_fill = -1000
          , topo = "topo1", altura = 1500, salida = "/Salidas/TP2/")

auxv = array(V.his.djf[lons[[1]], lats[[1]],2], c(length(lons[[1]]),length(lats[[1]]),1))
auxu = array(V.his.djf[lons[[1]], lats[[1]],1], c(length(lons[[1]]),length(lats[[1]]),1))
aux = array(pp6.his_seasons[lons[[1]], lats[[1]],2], c(length(lons[[1]]),length(lats[[1]]),1))
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Precipitación media CNRM-CM6 1975 - 2005  - DJF", nombre_fig = "pp6.his_asia.djf", escala = c(0,600)
          , label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si"
          , lon = lon[lons[[1]]], lat = lat[lats[[1]]], escala_dis = seq(0, 600, by = 50), breaks_c_f = seq(0, 600, by = 50), r = 1, na_fill = -1000
          , topo = "topo1", altura = 1500, salida = "/Salidas/TP2/")

#--- Humedad ---#
auxv = array(V.his.jja[lons[[1]], lats[[1]],2], c(length(lons[[1]]),length(lats[[1]]),1))
auxu = array(V.his.jja[lons[[1]], lats[[1]],1], c(length(lons[[1]]),length(lats[[1]]),1))
aux = array(hu6.his_seasons[lons[[1]], lats[[1]],1], c(length(lons[[1]]),length(lats[[1]]),1))
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Humedad específica media CNRM-CM6 1975 - 2005  - JJA", nombre_fig = "hu6.his_asia.jja", escala = c(0,0.02)
          , label_escala = "mm", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 9, contour = "si"
          , lon = lon[lons[[1]]], lat = lat[lats[[1]]], escala_dis = seq(0, 0.02, by = 0.002), breaks_c_f = seq(0, 0.02, by = 0.002), r = 1, na_fill = -1000
          , topo = "topo1", altura = 1500, salida = "/Salidas/TP2/")


auxv = array(V.his.djf[lons[[1]], lats[[1]],2], c(length(lons[[1]]),length(lats[[1]]),1))
auxu = array(V.his.djf[lons[[1]], lats[[1]],1], c(length(lons[[1]]),length(lats[[1]]),1))
aux = array(hu6.his_seasons[lons[[1]], lats[[1]],2], c(length(lons[[1]]),length(lats[[1]]),1))
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Humedad específica media CNRM-CM6 1975 - 2005  - DJF", nombre_fig = "hu6.his_asia.djf", escala = c(0,0.02)
          , label_escala = "mm", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 9, contour = "si"
          , lon = lon[lons[[1]]], lat = lat[lats[[1]]], escala_dis = seq(0, 0.02, by = 0.002), breaks_c_f = seq(0, 0.02, by = 0.002), r = 1, na_fill = -1000
          , topo = "topo1", altura = 1500, salida = "/Salidas/TP2/")



#####

#--- SA ---#
#### CNRM-CM5 ####
#--- Temperatura ---#
# no hay
#--- Precipitacion ---#

aux = array(pp5.his_seasons[lons[[2]], lats[[2]],1], c(length(lons[[2]]),length(lats[[2]]),1))
mapa_topo2(lista = aux, titulo = "Precipitación media 1975 - 2005  - JJA", nombre_fig = "pp5.his_sa.jja", escala = c(0,600)
           , label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si"
           , lon = lon[lons[[2]]], lat = lat[lats[[2]]], escala_dis = seq(0, 600, by = 50), breaks_c_f = seq(0, 600, by = 50), r = 1, na_fill = -1000
           , topo = "topo2", altura = 1500, salida = "/Salidas/TP2/")

aux = array(pp5.his_seasons[lons[[2]], lats[[2]],2], c(length(lons[[2]]),length(lats[[2]]),1))
mapa_topo2(lista = aux, titulo = "Precipitación media 1975 - 2005  - DJF", nombre_fig = "pp5.his_sa.djf", escala = c(0,600)
           , label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si"
           , lon = lon[lons[[2]]], lat = lat[lats[[2]]], escala_dis = seq(0, 600, by = 50), breaks_c_f = seq(0, 600, by = 50), r = 1, na_fill = -1000
           , topo = "topo2", altura = 1500, salida = "/Salidas/TP2/")


#### CNRM-CM6 ####
#--- Temperatura ---#
auxv = array(V.his.jja[lons[[2]], lats[[2]],2], c(length(lons[[2]]),length(lats[[2]]),1))
auxu = array(V.his.jja[lons[[2]], lats[[2]],1], c(length(lons[[2]]),length(lats[[2]]),1))
aux = array(t6.his_seasons[lons[[2]], lats[[2]],1], c(length(lons[[2]]),length(lats[[2]]),1))
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Temperatura media CNRM-CM6 1975 - 2005  - JJA", nombre_fig = "t6.his_sa.jja", escala = c(0,35)
          , label_escala = "mm", resta = 273, brewer = "Spectral", revert = "si", niveles = 11, contour = "si"
          , lon = lon[lons[[2]]], lat = lat[lats[[2]]], escala_dis = seq(0, 35, by = 2.5), breaks_c_f = seq(0, 35, by = 2.5), r = 1, na_fill = -1000
          , topo = "topo2", altura = 1500, salida = "/Salidas/TP2/")


auxv = array(V.his.djf[lons[[2]], lats[[2]],2], c(length(lons[[2]]),length(lats[[2]]),1))
auxu = array(V.his.djf[lons[[2]], lats[[2]],1], c(length(lons[[2]]),length(lats[[2]]),1))
aux = array(t6.his_seasons[lons[[2]], lats[[2]],2], c(length(lons[[2]]),length(lats[[2]]),1))
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Temperatura media CNRM-CM6 1975 - 2005  - DFJ", nombre_fig = "t6.his_sa.djf", escala = c(0,35)
          , label_escala = "mm", resta = 273, brewer = "Spectral", revert = "si", niveles = 11, contour = "si"
          , lon = lon[lons[[2]]], lat = lat[lats[[2]]], escala_dis = seq(0, 35, by = 2.5), breaks_c_f = seq(0, 35, by = 2.5), r = 1, na_fill = -1000
          , topo = "topo2", altura = 1500, salida = "/Salidas/TP2/")



#--- Precipitacion ---#
auxv = array(V.his.jja[lons[[2]], lats[[2]],2], c(length(lons[[2]]),length(lats[[2]]),1))
auxu = array(V.his.jja[lons[[2]], lats[[2]],1], c(length(lons[[2]]),length(lats[[2]]),1))
aux = array(pp6.his_seasons[lons[[2]], lats[[2]],1], c(length(lons[[2]]),length(lats[[2]]),1))
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Precipitación media CNRM-CM6 1975 - 2005  - JJA", nombre_fig = "pp6.his_sa.jja", escala = c(0,600)
          , label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si"
          , lon = lon[lons[[2]]], lat = lat[lats[[2]]], escala_dis = seq(0, 600, by = 50), breaks_c_f = seq(0, 600, by = 50), r = 1, na_fill = -1000
          , topo = "topo2", altura = 1500, salida = "/Salidas/TP2/")

auxv = array(V.his.djf[lons[[2]], lats[[2]],2], c(length(lons[[2]]),length(lats[[2]]),1))
auxu = array(V.his.djf[lons[[2]], lats[[2]],1], c(length(lons[[2]]),length(lats[[2]]),1))
aux = array(pp6.his_seasons[lons[[2]], lats[[2]],2], c(length(lons[[2]]),length(lats[[2]]),1))
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Precipitación media CNRM-CM6 1975 - 2005  - DJF", nombre_fig = "pp6.his_sa.djf", escala = c(0,600)
          , label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si"
          , lon = lon[lons[[2]]], lat = lat[lats[[2]]], escala_dis = seq(0, 600, by = 50), breaks_c_f = seq(0, 600, by = 50), r = 1, na_fill = -1000
          , topo = "topo2", altura = 1500, salida = "/Salidas/TP2/")


#--- Humedad ---#
auxv = array(V.his.jja[lons[[2]], lats[[2]],2], c(length(lons[[2]]),length(lats[[2]]),1))
auxu = array(V.his.jja[lons[[2]], lats[[2]],1], c(length(lons[[2]]),length(lats[[2]]),1))
aux = array(hu6.his_seasons[lons[[2]], lats[[2]],1], c(length(lons[[2]]),length(lats[[2]]),1))
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Humedad específica media CNRM-CM6 1975 - 2005  - JJA", nombre_fig = "hu6.his_sa.jja", escala = c(0,0.02)
          , label_escala = "mm", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 9, contour = "si"
          , lon = lon[lons[[2]]], lat = lat[lats[[2]]], escala_dis = seq(0, 0.02, by = 0.002), breaks_c_f = seq(0, 0.02, by = 0.002), r = 1, na_fill = -1000
          , topo = "topo2", altura = 1500, salida = "/Salidas/TP2/")

auxv = array(V.his.djf[lons[[2]], lats[[2]],2], c(length(lons[[2]]),length(lats[[2]]),1))
auxu = array(V.his.djf[lons[[2]], lats[[2]],1], c(length(lons[[2]]),length(lats[[2]]),1))
aux = array(hu6.his_seasons[lons[[2]], lats[[2]],2], c(length(lons[[2]]),length(lats[[2]]),1))
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Humedad específica media CNRM-CM6 1975 - 2005  - DJF", nombre_fig = "hu6.his_sa.djf", escala = c(0,0.02)
          , label_escala = "?", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 9, contour = "si"
          , lon = lon[lons[[2]]], lat = lat[lats[[2]]], escala_dis = seq(0, 0.02, by = 0.002), breaks_c_f = seq(0, 0.02, by = 0.002), r = 1, na_fill = -1000
          , topo = "topo2", altura = 1500, salida = "/Salidas/TP2/")







#####


#--- RCP2.6 ---#

#### Temperatura ####
# CNRM - CM5
# no tiene datos mensuales

# CNRM-CM6
load("RDatas/t6.26_2049.RData")
load("RDatas/t6.26_2099.RData")

t6.26_49.seasons = array(data = NA, dim = c(144,73,2))
t6.26_49.seasons[,,1] = apply(t6.26_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
t6.26_49.seasons[,,2] = apply(t6.26_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

t6.26_99.seasons = array(data = NA, dim = c(144,73,2))
t6.26_99.seasons[,,1] = apply(t6.26_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
t6.26_99.seasons[,,2] = apply(t6.26_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

rm(t6.26_2049, t6.26_2099)

#### Precipitacion ####
# CNRM-CM5
load("RDatas/pp5.26_2049.RData")
load("RDatas/pp5.26_2099.RData")

pp5.26_49.seasons = array(data = NA, dim = c(144,73,2))
pp5.26_49.seasons[,,1] = apply(pp5.26_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
pp5.26_49.seasons[,,2] = apply(pp5.26_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

pp5.26_99.seasons = array(data = NA, dim = c(144,73,2))
pp5.26_99.seasons[,,1] = apply(pp5.26_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
pp5.26_99.seasons[,,2] = apply(pp5.26_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

rm(pp5.26_2049, pp5.26_2099)

# CNRM-CM6
load("RDatas/pp6.26_2049.RData")
load("RDatas/pp6.26_2099.RData")

pp6.26_49.seasons = array(data = NA, dim = c(144,73,2))
pp6.26_49.seasons[,,1] = apply(pp6.26_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
pp6.26_49.seasons[,,2] = apply(pp6.26_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

pp6.26_99.seasons = array(data = NA, dim = c(144,73,2))
pp6.26_99.seasons[,,1] = apply(pp6.26_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
pp6.26_99.seasons[,,2] = apply(pp6.26_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF


rm(pp6.26_2049, pp6.26_2099)

#### Viento ####
#CNRM-CM6
load("RDatas/u6.26_2049.RData")
load("RDatas/u6.26_2099.RData")
load("RDatas/v6.26_2049.RData")
load("RDatas/v6.26_2099.RData")
load("RDatas/u6.85_2049.RData")
load("RDatas/u6.85_2099.RData")
load("RDatas/v6.85_2049.RData")
load("RDatas/v6.85_2099.RData")

u6.26_49.seasons = array(data = NA, dim = c(144,73,2))
u6.26_49.seasons[,,1] = apply(u6.26_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
u6.26_49.seasons[,,2] = apply(u6.26_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF


u6.26_99.seasons = array(data = NA, dim = c(144,73,2))
u6.26_99.seasons[,,1] = apply(u6.26_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
u6.26_99.seasons[,,2] = apply(u6.26_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

u6.85_49.seasons = array(data = NA, dim = c(144,73,2))
u6.85_49.seasons[,,1] = apply(u6.85_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
u6.85_49.seasons[,,2] = apply(u6.85_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF


u6.85_99.seasons = array(data = NA, dim = c(144,73,2))
u6.85_99.seasons[,,1] = apply(u6.85_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
u6.85_99.seasons[,,2] = apply(u6.85_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF



# v
v6.26_49.seasons = array(data = NA, dim = c(144,73,2))
v6.26_49.seasons[,,1] = apply(v6.26_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
v6.26_49.seasons[,,2] = apply(v6.26_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF


v6.26_99.seasons = array(data = NA, dim = c(144,73,2))
v6.26_99.seasons[,,1] = apply(v6.26_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
v6.26_99.seasons[,,2] = apply(v6.26_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

v6.85_49.seasons = array(data = NA, dim = c(144,73,2))
v6.85_49.seasons[,,1] = apply(v6.85_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
v6.85_49.seasons[,,2] = apply(v6.85_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF


v6.85_99.seasons = array(data = NA, dim = c(144,73,2))
v6.85_99.seasons[,,1] = apply(v6.85_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
v6.85_99.seasons[,,2] = apply(v6.85_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF







rm(u6.26_2049, u6.26_2099, v6.26_2049, v6.26_2099, u6.85_2049, u6.85_2099, v6.85_2049, v6.85_2099)

#### Humedad ####
# CNRM-CM6
load("RDatas/hu6.26_2049.RData")
load("RDatas/hu6.26_2099.RData")

hu6.26_49.seasons = array(data = NA, dim = c(144,73,2))
hu6.26_49.seasons[,,1] = apply(hu6.26_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
hu6.26_49.seasons[,,2] = apply(hu6.26_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

hu6.26_99.seasons = array(data = NA, dim = c(144,73,2))
hu6.26_99.seasons[,,1] = apply(hu6.26_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
hu6.26_99.seasons[,,2] = apply(hu6.26_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

rm(hu6.26_2049, hu6.26_2099)


#####
#--- RCP8.5 ---#

#### Temperatura ####
# CNRM - CM5
# no tiene datos mensuales

# CNRM-CM6
load("RDatas/t6.85_2049.RData")
load("RDatas/t6.85_2099.RData")

t6.85_49.seasons = array(data = NA, dim = c(144,73,2))
t6.85_49.seasons[,,1] = apply(t6.85_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
t6.85_49.seasons[,,2] = apply(t6.85_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

t6.85_99.seasons = array(data = NA, dim = c(144,73,2))
t6.85_99.seasons[,,1] = apply(t6.85_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
t6.85_99.seasons[,,2] = apply(t6.85_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

rm(t6.85_2049, t6.85_2099)

#### Precipitacion ####
# CNRM-CM5
load("RDatas/pp5.85_2049.RData")
load("RDatas/pp5.85_2099.RData")

pp5.85_49.seasons = array(data = NA, dim = c(144,73,2))
pp5.85_49.seasons[,,1] = apply(pp5.85_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
pp5.85_49.seasons[,,2] = apply(pp5.85_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

pp5.85_99.seasons = array(data = NA, dim = c(144,73,2))
pp5.85_99.seasons[,,1] = apply(pp5.85_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
pp5.85_99.seasons[,,2] = apply(pp5.85_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

rm(pp5.85_2049, pp5.85_2099)

# CNRM-CM6
load("RDatas/pp6.85_2049.RData")
load("RDatas/pp6.85_2099.RData")

pp6.85_49.seasons = array(data = NA, dim = c(144,73,2))
pp6.85_49.seasons[,,1] = apply(pp6.85_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
pp6.85_49.seasons[,,2] = apply(pp6.85_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

pp6.85_99.seasons = array(data = NA, dim = c(144,73,2))
pp6.85_99.seasons[,,1] = apply(pp6.85_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
pp6.85_99.seasons[,,2] = apply(pp6.85_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF


rm(pp6.85_2049, pp6.85_2099)

#### Viento ####
#CNRM-CM6
load("RDatas/u6.85_2049.RData")
load("RDatas/u6.85_2099.RData")
load("RDatas/v6.85_2049.RData")
load("RDatas/v6.85_2099.RData")

u6.85_49.seasons = array(data = NA, dim = c(144,73,2))
u6.85_49.seasons[,,1] = apply(u6.85_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
u6.85_49.seasons[,,2] = apply(u6.85_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF


u6.85_99.seasons = array(data = NA, dim = c(144,73,2))
u6.85_99.seasons[,,1] = apply(u6.85_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
u6.85_99.seasons[,,2] = apply(u6.85_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF


# v
v6.85_49.seasons = array(data = NA, dim = c(144,73,2))
v6.85_49.seasons[,,1] = apply(v6.85_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
v6.85_49.seasons[,,2] = apply(v6.85_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF


v6.85_99.seasons = array(data = NA, dim = c(144,73,2))
v6.85_99.seasons[,,1] = apply(v6.85_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
v6.85_99.seasons[,,2] = apply(v6.85_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

rm(u6.85_2049, u6.85_2099, v6.85_2049, v6.85_2099)

#### Humedad ####
# CNRM-CM6
load("RDatas/hu6.85_2049.RData")
load("RDatas/hu6.85_2099.RData")

hu6.85_49.seasons = array(data = NA, dim = c(144,73,2))
hu6.85_49.seasons[,,1] = apply(hu6.85_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
hu6.85_49.seasons[,,2] = apply(hu6.85_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

hu6.85_99.seasons = array(data = NA, dim = c(144,73,2))
hu6.85_99.seasons[,,1] = apply(hu6.85_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
hu6.85_99.seasons[,,2] = apply(hu6.85_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

rm(hu6.85_2049, hu6.85_2099)


#####

#### Array para graficar ####
# esto no es muy practico... pero es mejor que hacer cada grafico de a uno

M5 = array(data = NA, dim = c(144,73,2,1,2,2))
M5[,,1,1,1,1] = pp5.26_49.seasons[,,1]; M5[,,1,1,1,2] = pp5.26_49.seasons[,,2]
M5[,,1,1,2,1] = pp5.26_99.seasons[,,1]; M5[,,1,1,2,2] = pp5.26_99.seasons[,,2]
M5[,,2,1,1,1] = pp5.85_49.seasons[,,1]; M5[,,2,1,1,2] = pp5.85_49.seasons[,,2]
M5[,,2,1,2,1] = pp5.85_99.seasons[,,1]; M5[,,2,1,2,2] = pp5.85_99.seasons[,,2]

save(M5, file = "RDatas/M5.RData")


M6 = array(data = NA, dim = c(144,73,2,3,2,2))
M6[,,1,1,1,1] = pp6.26_49.seasons[,,1]; M6[,,1,1,1,2] = pp6.26_49.seasons[,,2]
M6[,,1,1,2,1] = pp6.26_99.seasons[,,1]; M6[,,1,1,2,2] = pp6.26_99.seasons[,,2]
M6[,,2,1,1,1] = pp6.85_49.seasons[,,1]; M6[,,2,1,1,2] = pp6.85_49.seasons[,,2]
M6[,,2,1,2,1] = pp6.85_99.seasons[,,1]; M6[,,2,1,2,2] = pp6.85_99.seasons[,,2]

M6[,,1,2,1,1] = t6.26_49.seasons[,,1]; M6[,,1,2,1,2] = t6.26_49.seasons[,,2]
M6[,,1,2,2,1] = t6.26_99.seasons[,,1]; M6[,,1,2,2,2] = t6.26_99.seasons[,,2]
M6[,,2,2,1,1] = t6.85_49.seasons[,,1]; M6[,,2,2,1,2] = t6.85_49.seasons[,,2]
M6[,,2,2,2,1] = t6.85_99.seasons[,,1]; M6[,,2,2,2,2] = t6.85_99.seasons[,,2]

M6[,,1,3,1,1] = hu6.26_49.seasons[,,1]; M6[,,1,3,1,2] = hu6.26_49.seasons[,,2]
M6[,,1,3,2,1] = hu6.26_99.seasons[,,1]; M6[,,1,3,2,2] = hu6.26_99.seasons[,,2]
M6[,,2,3,1,1] = hu6.85_49.seasons[,,1]; M6[,,2,3,1,2] = hu6.85_49.seasons[,,2]
M6[,,2,3,2,1] = hu6.85_99.seasons[,,1]; M6[,,2,3,2,2] = hu6.85_99.seasons[,,2]

save(M6, file = "RDatas/M6.RData")


V6 = array(data = NA, c(144,73, 2, 2, 2, 2))
V6[,,1,1,1,1] = u6.26_49.seasons[,,1]; V6[,,1,1,1,2] = u6.26_49.seasons[,,2]
V6[,,1,1,2,1] = u6.26_99.seasons[,,1]; V6[,,1,1,2,2] = u6.26_99.seasons[,,2]
V6[,,1,2,1,1] = v6.26_49.seasons[,,1]; V6[,,1,2,1,2] = v6.26_49.seasons[,,2]
V6[,,1,2,2,1] = v6.26_99.seasons[,,1]; V6[,,1,2,2,2] = v6.26_99.seasons[,,2]
V6[,,2,1,1,1] = u6.85_49.seasons[,,1]; V6[,,2,1,1,2] = u6.85_49.seasons[,,2]
V6[,,2,1,2,1] = u6.85_99.seasons[,,1]; V6[,,2,1,2,2] = u6.85_99.seasons[,,2]
V6[,,2,2,1,1] = v6.85_49.seasons[,,1]; V6[,,2,2,1,2] = v6.85_49.seasons[,,2]
V6[,,2,2,2,1] = v6.85_99.seasons[,,1]; V6[,,2,2,2,2] = v6.85_99.seasons[,,2]

save(V6, file = "RDatas/V6.RData")

#### Graficos RCP ####
DrawMonsoon("cnrm-cm5")
DrawMonsoon("cnrm-cm6")

#### Dominio ####
# Dominio calculado con el criterio de ....


#--- historico ---#

area5.his = AreaMonsoon(variable = "pp5.his")
area6.his = AreaMonsoon(variable = "pp6.his")

# 2.6

area5.26_49 = AreaMonsoon(variable = "pp5.26_49")
area5.26_99 = AreaMonsoon(variable = "pp5.26_99")

area6.26_49 = AreaMonsoon(variable = "pp6.26_49")
area6.26_99 = AreaMonsoon(variable = "pp6.26_99")


# 8.5

area5.85_49 = AreaMonsoon(variable = "pp5.85_49")
area5.85_99 = AreaMonsoon(variable = "pp5.85_99")

area6.85_49 = AreaMonsoon(variable = "pp6.85_49")
area6.85_99 = AreaMonsoon(variable = "pp6.85_99")

#### GRAFICOS ####
#--- ASIA ---#
aux = array(area5.his[[1]][lons[[1]], lats[[1]]], c(length(lons[[1]]),length(lats[[1]]),1))
aux2 = array(area5.26_49[[1]][lons[[1]], lats[[1]]], c(length(lons[[1]]),length(lats[[1]]),1))
aux3 = array(area5.26_99[[1]][lons[[1]], lats[[1]]], c(length(lons[[1]]),length(lats[[1]]),1))
mapa_cont(lista = aux, lista2 = aux2, lista3 = aux3, titulo = "Area Monzonica CNRM-CM5 RCP2.6", nombre_fig = "area5.26_asia",  lon = lon[lons[[1]]], lat = lat[lats[[1]]], r = 1
          , topo = "topo1", altura = 1500, salida = "/Salidas/TP2/")

aux = array(area6.his[[1]][lons[[1]], lats[[1]]], c(length(lons[[1]]),length(lats[[1]]),1))
aux2 = array(area6.26_49[[1]][lons[[1]], lats[[1]]], c(length(lons[[1]]),length(lats[[1]]),1))
aux3 = array(area6.26_99[[1]][lons[[1]], lats[[1]]], c(length(lons[[1]]),length(lats[[1]]),1))
mapa_cont(lista = aux, lista2 = aux2, lista3 = aux3, titulo = "Area Monzonica CNRM-CM5 SSP1-2.6", nombre_fig = "area6.26_asia",  lon = lon[lons[[1]]], lat = lat[lats[[1]]], r = 1
          , topo = "topo1", altura = 1500, salida = "/Salidas/TP2/")


aux = array(area5.his[[1]][lons[[1]], lats[[1]]], c(length(lons[[1]]),length(lats[[1]]),1))
aux2 = array(area5.85_49[[1]][lons[[1]], lats[[1]]], c(length(lons[[1]]),length(lats[[1]]),1))
aux3 = array(area5.85_99[[1]][lons[[1]], lats[[1]]], c(length(lons[[1]]),length(lats[[1]]),1))
mapa_cont(lista = aux, lista2 = aux2, lista3 = aux3, titulo = "Area Monzonica CNRM-CM5 RCP8.5", nombre_fig = "area5.85_asia",  lon = lon[lons[[1]]], lat = lat[lats[[1]]], r = 1
          , topo = "topo1", altura = 1500, salida = "/Salidas/TP2/")


aux = array(area6.his[[1]][lons[[1]], lats[[1]]], c(length(lons[[1]]),length(lats[[1]]),1))
aux2 = array(area6.85_49[[1]][lons[[1]], lats[[1]]], c(length(lons[[1]]),length(lats[[1]]),1))
aux3 = array(area6.85_99[[1]][lons[[1]], lats[[1]]], c(length(lons[[1]]),length(lats[[1]]),1))
mapa_cont(lista = aux, lista2 = aux2, lista3 = aux3, titulo = "Area Monzonica CNRM-CM5 SS5-8.5", nombre_fig = "area6.85_asia",  lon = lon[lons[[1]]], lat = lat[lats[[1]]], r = 1
          , topo = "topo1", altura = 1500, salida = "/Salidas/TP2/")



#--- SA ---#
aux = array(area6.his[[2]][lons[[2]], lats[[2]]], c(length(lons[[2]]),length(lats[[2]]),1))
aux2 = array(area6.26_49[[2]][lons[[2]], lats[[2]]], c(length(lons[[2]]),length(lats[[2]]),1))
aux3 = array(area6.26_99[[2]][lons[[2]], lats[[2]]], c(length(lons[[2]]),length(lats[[2]]),1))
mapa_cont(lista = aux, lista2 = aux2, lista3 = aux3, titulo = "Area Monzonica CNRM-CM SSP1-2.6", nombre_fig = "area6.26_SA",  lon = lon[lons[[2]]], lat = lat[lats[[2]]], r = 1
          , topo = "topo2", altura = 1500, salida = "/Salidas/TP2/")


aux = array(area5.his[[2]][lons[[2]], lats[[2]]], c(length(lons[[2]]),length(lats[[2]]),1))
aux2 = array(area5.26_49[[2]][lons[[2]], lats[[2]]], c(length(lons[[2]]),length(lats[[2]]),1))
aux3 = array(area5.26_99[[2]][lons[[2]], lats[[2]]], c(length(lons[[2]]),length(lats[[2]]),1))
mapa_cont(lista = aux, lista2 = aux2, lista3 = aux3, titulo = "Area Monzonica CNRM-CM RCP2.6", nombre_fig = "area5.26_SA",  lon = lon[lons[[2]]], lat = lat[lats[[2]]], r = 1
          , topo = "topo2", altura = 1500, salida = "/Salidas/TP2/")


aux = array(area6.his[[2]][lons[[2]], lats[[2]]], c(length(lons[[2]]),length(lats[[2]]),1))
aux2 = array(area6.85_49[[2]][lons[[2]], lats[[2]]], c(length(lons[[2]]),length(lats[[2]]),1))
aux3 = array(area6.85_99[[2]][lons[[2]], lats[[2]]], c(length(lons[[2]]),length(lats[[2]]),1))
mapa_cont(lista = aux, lista2 = aux2, lista3 = aux3, titulo = "Area Monzonica CNRM-CM SSP5-8.5", nombre_fig = "area6.85_SA",  lon = lon[lons[[2]]], lat = lat[lats[[2]]], r = 1
          , topo = "topo2", altura = 1500, salida = "/Salidas/TP2/")

aux = array(area5.his[[2]][lons[[2]], lats[[2]]], c(length(lons[[2]]),length(lats[[2]]),1))
aux2 = array(area5.85_49[[2]][lons[[2]], lats[[2]]], c(length(lons[[2]]),length(lats[[2]]),1))
aux3 = array(area5.85_99[[2]][lons[[2]], lats[[2]]], c(length(lons[[2]]),length(lats[[2]]),1))
mapa_cont(lista = aux, lista2 = aux2, lista3 = aux3, titulo = "Area Monzonica CNRM-CM RCP8.5", nombre_fig = "area5.85_SA",  lon = lon[lons[[2]]], lat = lat[lats[[2]]], r = 1
          , topo = "topo2", altura = 1500, salida = "/Salidas/TP2/")
#### Intensidad ####

# ver km... la grilla es regular? (seria km ~ 555)
# la suma esta bien? -->(ojo con puntos de grilla q cumplen el criterio pero no forman parte de la zona ppal, solo pasa en Asia)

i5.26 = MonsoonIntensity(area.his = area5.his,area.49 = area5.26_49, area.99 = area5.26_99, lons, lats, km = 1)
i5.85 = MonsoonIntensity(area.his = area5.his,area.49 = area5.85_49, area.99 = area5.85_99, lons, lats, km = 1)

i6.26 = MonsoonIntensity(area.his = area6.his,area.49 = area6.26_49, area.99 = area6.26_99, lons, lats, km = 1)
i6.85 = MonsoonIntensity(area.his = area6.his,area.49 = area6.85_49, area.99 = area6.85_99, lons, lats, km = 1)

#### Analisis por mes ####







































