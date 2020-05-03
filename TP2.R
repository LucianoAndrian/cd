# TP2 Simulacion del clima 2020 - sistemas monzonicos
# apertura y guardado de datos ya organizados con Apertura_datos_mensuales.R

# source("Apertura_datos_mensuales.R") #tarda 1:30hr...

# NO cargar todos los archivos consume todo la ram. 
lon = read.table("lon.txt")[,1]
lat = read.table("lat.txt")[,1]

mask = as.matrix(read.table("mask.txt"))


# seleccion de latitudes para Asia y SA
lats = list()
lons = list()
lats[[1]] = seq(which(lat == -10),which(lat == 35)); lons[[1]] = seq(which(lon == 40), which(lon == 102.5))
lats[[2]] = seq(which(lat == -35),which(lat == 20)); lons[[2]] = seq(which(lon == 270), which(lon == 330))

# crear arrays para guardarlos ya que al cargar mas RData comsume cada vez mas ram, en especial con los de vientos (los mas pesados)

#--- HISTORICO ----#
#### Temperatura #### 
#CNRM-CM5 no tiene datos mensuales

# CNRM-CM6


#load("RDatas/t6.his.RData")

# usando t6.his[[2]] ya se encuentran promediados los anios (esto da lo mismo q hacerlo sobre cada miembro)
t6.his_seasons = array(data = NA, c(144,73,2))
t6.his_seasons[,,1] = apply(t6.his[[1]][,,2,], c(1,2), mean, na.rm = T) #JJA
t6.his_seasons[,,2] = apply(t6.his[[1]][,,4,], c(1,2), mean, na.rm = T) #DJF

#save(t6.his_seasons, file = "RDatas/t6.his_seasons.RData")
load("RDatas/t6.his_seasons.RData")



#### Viento ####
# CNRM-CM6
#load("RDatas/u6.his.RData")
#load("RDatas/v6.his.RData")

# JJA
V.his.jja = array(data = NA, dim = c(144,73,2))
V.his.jja[,,1] = apply(u6.his[[1]][,,2,], c(1,2), mean, na.rm = T)  # u850
V.his.jja[,,2] = apply(v6.his[[1]][,,2,], c(1,2), mean, na.rm = T)  # v850

#save(V.his.jja, file = "RDatas/V.his.jja.RData")
load("RDatas/V.his.jja.RData")


# DJF
V.his.djf = array(data = NA, dim = c(144,73,2))
V.his.djf[,,1] = apply(u6.his[[1]][,,4,], c(1,2), mean, na.rm = T)  # u850
V.his.djf[,,2] = apply(v6.his[[1]][,,4,], c(1,2), mean, na.rm = T)  # v850

#save(V.his.djf, file = "RDatas/V.his.djf.RData")
load("RDatas/V.his.djf.RData")



##### Precipitacion ####

# CNRM-CM5
#load("RDatas/pp5.his.RData")

pp5.his_seasons = array(data = NA, c(144,73,2))
pp5.his_seasons[,,1] = apply(pp5.his[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
pp5.his_seasons[,,2] = apply(pp5.his[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

#save(pp5.his_seasons, file = "RDatas/pp5.his_seasons.RData")
load("RDatas/pp5.his_seasons.RData")

# CNRM-CM6
#load("RDatas/pp6.his.RData")
pp6.his_seasons = array(data = NA, c(144,73,2))
pp6.his_seasons[,,1] = apply(pp6.his[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
pp6.his_seasons[,,2] = apply(pp6.his[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

#save(pp6.his_seasons, file = "RDatas/pp6.his_seasons.RData")
load("RDatas/pp6.his_seasons.RData")


#### Humedad ####

# CNRM-CM6
#load("RDatas/hu6.his.RData")

hu6.his_seasons = array(data = NA, c(144,73,2))
hu6.his_seasons[,,1] = apply(hu6.his[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
hu6.his_seasons[,,2] = apply(hu6.his[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

#save(hu6.his_seasons, file = "RDatas/hu6.his_seasons.RData")
load("RDatas/hu6.his_seasons.RData")


#### GRAFICOS  ####
source("FUNCIONES.R")
#--- Asia ---#
#### CNRM-CM5 ####


#--- Temperatura ---#
# no hay


#--- Precipitacion ---#

aux = array(pp5.his_seasons[lons[[1]], lats[[1]],1], c(length(lons[[1]]),length(lats[[1]]),1))
mapa_topo2(lista = aux, titulo = "Precipitación media 1975 - 2005  - JJA", nombre_fig = "pp5.his_asia.jja", escala = c(0,600)
          , label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si"
          , lon = lon[lons[[1]]], lat = lat[lats[[1]]], escala_dis = seq(0, 600, by = 50), breaks_c_f = seq(0, 600, by = 50), r = 1, na_fill = -1000
          , topo = "topo1", altura = 1000, salida = "/Salidas/TP2/")

aux = array(pp5.his_seasons[lons[[1]], lats[[1]],2], c(length(lons[[1]]),length(lats[[1]]),1))
mapa_topo2(lista = aux, titulo = "Precipitación media 1975 - 2005  - DFJ", nombre_fig = "pp5.his_asia.djf", escala = c(0,600)
           , label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si"
           , lon = lon[lons[[1]]], lat = lat[lats[[1]]], escala_dis = seq(0, 600, by = 50), breaks_c_f = seq(0, 600, by = 50), r = 1, na_fill = -1000
           , topo = "topo1", altura = 1000, salida = "/Salidas/TP2/")


#### CNRM-CM6 ####
#--- Temperatura ---#
auxv = array(V.his.jja[lons[[1]], lats[[1]],2], c(length(lons[[1]]),length(lats[[1]]),1))
auxu = array(V.his.jja[lons[[1]], lats[[1]],1], c(length(lons[[1]]),length(lats[[1]]),1))
aux = array(t6.his_seasons[lons[[1]], lats[[1]],1], c(length(lons[[1]]),length(lats[[1]]),1))
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Temperatura media CNRM-CM6 1975 - 2005  - JJA", nombre_fig = "t6.his_asia.jja", escala = c(0,35)
          , label_escala = "mm", resta = 273, brewer = "Spectral", revert = "si", niveles = 11, contour = "si"
          , lon = lon[lons[[1]]], lat = lat[lats[[1]]], escala_dis = seq(0, 35, by = 2.5), breaks_c_f = seq(0, 35, by = 2.5), r = 1, na_fill = -1000
          , topo = "topo1", altura = 1000, salida = "/Salidas/TP2/")

auxv = array(V.his.djf[lons[[1]], lats[[1]],2], c(length(lons[[1]]),length(lats[[1]]),1))
auxu = array(V.his.djf[lons[[1]], lats[[1]],1], c(length(lons[[1]]),length(lats[[1]]),1))
aux = array(t6.his_seasons[lons[[1]], lats[[1]],2], c(length(lons[[1]]),length(lats[[1]]),1))
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Temperatura media CNRM-CM6 1975 - 2005  - DJF", nombre_fig = "t6.his_asia.djf", escala = c(0,35)
          , label_escala = "mm", resta = 273, brewer = "Spectral", revert = "si", niveles = 11, contour = "si"
          , lon = lon[lons[[1]]], lat = lat[lats[[1]]], escala_dis = seq(0, 35, by = 2.5), breaks_c_f = seq(0, 35, by = 2.5), r = 1, na_fill = -1000
          , topo = "topo1", altura = 1000, salida = "/Salidas/TP2/")

#--- Precipitacion ---#

auxv = array(V.his.jja[lons[[1]], lats[[1]],2], c(length(lons[[1]]),length(lats[[1]]),1))
auxu = array(V.his.jja[lons[[1]], lats[[1]],1], c(length(lons[[1]]),length(lats[[1]]),1))
aux = array(pp6.his_seasons[lons[[1]], lats[[1]],1], c(length(lons[[1]]),length(lats[[1]]),1))
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Precipitación media CNRM-CM6 1975 - 2005  - JJA", nombre_fig = "pp6.his_asia.jja", escala = c(0,600)
           , label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si"
           , lon = lon[lons[[1]]], lat = lat[lats[[1]]], escala_dis = seq(0, 600, by = 50), breaks_c_f = seq(0, 600, by = 50), r = 1, na_fill = -1000
           , topo = "topo1", altura = 1000, salida = "/Salidas/TP2/")

auxv = array(V.his.djf[lons[[1]], lats[[1]],2], c(length(lons[[1]]),length(lats[[1]]),1))
auxu = array(V.his.djf[lons[[1]], lats[[1]],1], c(length(lons[[1]]),length(lats[[1]]),1))
aux = array(pp6.his_seasons[lons[[1]], lats[[1]],2], c(length(lons[[1]]),length(lats[[1]]),1))
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Precipitación media CNRM-CM6 1975 - 2005  - DJF", nombre_fig = "pp6.his_asia.djf", escala = c(0,600)
          , label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si"
          , lon = lon[lons[[1]]], lat = lat[lats[[1]]], escala_dis = seq(0, 600, by = 50), breaks_c_f = seq(0, 600, by = 50), r = 1, na_fill = -1000
          , topo = "topo1", altura = 1000, salida = "/Salidas/TP2/")

#--- Humedad ---#
auxv = array(V.his.jja[lons[[1]], lats[[1]],2], c(length(lons[[1]]),length(lats[[1]]),1))
auxu = array(V.his.jja[lons[[1]], lats[[1]],1], c(length(lons[[1]]),length(lats[[1]]),1))
aux = array(hu6.his_seasons[lons[[1]], lats[[1]],1], c(length(lons[[1]]),length(lats[[1]]),1))
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Humedad específica media CNRM-CM6 1975 - 2005  - JJA", nombre_fig = "hu6.his_asia.jja", escala = c(0,0.02)
          , label_escala = "mm", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 9, contour = "si"
          , lon = lon[lons[[1]]], lat = lat[lats[[1]]], escala_dis = seq(0, 0.02, by = 0.002), breaks_c_f = seq(0, 0.02, by = 0.002), r = 1, na_fill = -1000
          , topo = "topo1", altura = 1000, salida = "/Salidas/TP2/")


auxv = array(V.his.djf[lons[[1]], lats[[1]],2], c(length(lons[[1]]),length(lats[[1]]),1))
auxu = array(V.his.djf[lons[[1]], lats[[1]],1], c(length(lons[[1]]),length(lats[[1]]),1))
aux = array(hu6.his_seasons[lons[[1]], lats[[1]],2], c(length(lons[[1]]),length(lats[[1]]),1))
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Humedad específica media CNRM-CM6 1975 - 2005  - DJF", nombre_fig = "hu6.his_asia.djf", escala = c(0,0.02)
          , label_escala = "mm", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 9, contour = "si"
          , lon = lon[lons[[1]]], lat = lat[lats[[1]]], escala_dis = seq(0, 0.02, by = 0.002), breaks_c_f = seq(0, 0.02, by = 0.002), r = 1, na_fill = -1000
          , topo = "topo1", altura = 1000, salida = "/Salidas/TP2/")



#####

#--- SA ---#
#### CNRM-CM5 ####
#--- Temperatura ---#
# no hay
#--- Precipitacion ---#

aux = array(pp5.his_seasons[lons[[2]], lats[[2]],1], c(length(lons[[2]]),length(lats[[2]]),1))
mapa_topo2(lista = aux, titulo = "Precipitación media 1975 - 2005  - JJA", nombre_fig = "pp5.his_sa.jja", escala = c(0,400)
           , label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si"
           , lon = lon[lons[[2]]], lat = lat[lats[[2]]], escala_dis = seq(0, 400, by = 50), breaks_c_f = seq(0, 400, by = 50), r = 1, na_fill = -1000
           , topo = "topo2", altura = 1000, salida = "/Salidas/TP2/")

aux = array(pp5.his_seasons[lons[[2]], lats[[2]],2], c(length(lons[[2]]),length(lats[[2]]),1))
mapa_topo2(lista = aux, titulo = "Precipitación media 1975 - 2005  - DJF", nombre_fig = "pp5.his_sa.djf", escala = c(0,400)
           , label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si"
           , lon = lon[lons[[2]]], lat = lat[lats[[2]]], escala_dis = seq(0, 400, by = 50), breaks_c_f = seq(0, 400, by = 50), r = 1, na_fill = -1000
           , topo = "topo2", altura = 1000, salida = "/Salidas/TP2/")


#### CNRM-CM6 ####
#--- Temperatura ---#
auxv = array(V.his.jja[lons[[2]], lats[[2]],2], c(length(lons[[2]]),length(lats[[2]]),1))
auxu = array(V.his.jja[lons[[2]], lats[[2]],1], c(length(lons[[2]]),length(lats[[2]]),1))
aux = array(t6.his_seasons[lons[[2]], lats[[2]],1], c(length(lons[[2]]),length(lats[[2]]),1))
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Temperatura media CNRM-CM6 1975 - 2005  - JJA", nombre_fig = "t6.his_sa.jja", escala = c(0,30)
          , label_escala = "mm", resta = 273, brewer = "Spectral", revert = "si", niveles = 11, contour = "si"
          , lon = lon[lons[[2]]], lat = lat[lats[[2]]], escala_dis = seq(0, 30, by = 2), breaks_c_f = seq(0, 30, by = 2), r = 1, na_fill = -1000
          , topo = "topo2", altura = 1000, salida = "/Salidas/TP2/")


auxv = array(V.his.djf[lons[[2]], lats[[2]],2], c(length(lons[[2]]),length(lats[[2]]),1))
auxu = array(V.his.djf[lons[[2]], lats[[2]],1], c(length(lons[[2]]),length(lats[[2]]),1))
aux = array(t6.his_seasons[lons[[2]], lats[[2]],2], c(length(lons[[2]]),length(lats[[2]]),1))
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Temperatura media CNRM-CM6 1975 - 2005  - DFJ", nombre_fig = "t6.his_sa.djf", escala = c(0,30)
          , label_escala = "mm", resta = 273, brewer = "Spectral", revert = "si", niveles = 11, contour = "si"
          , lon = lon[lons[[2]]], lat = lat[lats[[2]]], escala_dis = seq(0, 30, by = 2), breaks_c_f = seq(0, 30, by = 2), r = 1, na_fill = -1000
          , topo = "topo2", altura = 1000, salida = "/Salidas/TP2/")



#--- Precipitacion ---#
auxv = array(V.his.jja[lons[[2]], lats[[2]],2], c(length(lons[[2]]),length(lats[[2]]),1))
auxu = array(V.his.jja[lons[[2]], lats[[2]],1], c(length(lons[[2]]),length(lats[[2]]),1))
aux = array(pp6.his_seasons[lons[[2]], lats[[2]],1], c(length(lons[[2]]),length(lats[[2]]),1))
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Precipitación media CNRM-CM6 1975 - 2005  - JJA", nombre_fig = "pp6.his_sa.jja", escala = c(0,500)
          , label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si"
          , lon = lon[lons[[2]]], lat = lat[lats[[2]]], escala_dis = seq(0, 500, by = 50), breaks_c_f = seq(0, 500, by = 50), r = 1, na_fill = -1000
          , topo = "topo2", altura = 1000, salida = "/Salidas/TP2/")

auxv = array(V.his.djf[lons[[2]], lats[[2]],2], c(length(lons[[2]]),length(lats[[2]]),1))
auxu = array(V.his.djf[lons[[2]], lats[[2]],1], c(length(lons[[2]]),length(lats[[2]]),1))
aux = array(pp6.his_seasons[lons[[2]], lats[[2]],2], c(length(lons[[2]]),length(lats[[2]]),1))
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Precipitación media CNRM-CM6 1975 - 2005  - DJF", nombre_fig = "pp6.his_sa.djf", escala = c(0,500)
          , label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si"
          , lon = lon[lons[[2]]], lat = lat[lats[[2]]], escala_dis = seq(0, 500, by = 50), breaks_c_f = seq(0, 500, by = 50), r = 1, na_fill = -1000
          , topo = "topo2", altura = 1000, salida = "/Salidas/TP2/")


#--- Humedad ---#
auxv = array(V.his.jja[lons[[2]], lats[[2]],2], c(length(lons[[2]]),length(lats[[2]]),1))
auxu = array(V.his.jja[lons[[2]], lats[[2]],1], c(length(lons[[2]]),length(lats[[2]]),1))
aux = array(hu6.his_seasons[lons[[2]], lats[[2]],1], c(length(lons[[2]]),length(lats[[2]]),1))
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Humedad específica media CNRM-CM6 1975 - 2005  - JJA", nombre_fig = "hu6.his_sa.jja", escala = c(0,0.02)
          , label_escala = "mm", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 9, contour = "si"
          , lon = lon[lons[[2]]], lat = lat[lats[[2]]], escala_dis = seq(0, 0.02, by = 0.002), breaks_c_f = seq(0, 0.02, by = 0.002), r = 1, na_fill = -1000
          , topo = "topo2", altura = 1000, salida = "/Salidas/TP2/")

auxv = array(V.his.djf[lons[[2]], lats[[2]],2], c(length(lons[[2]]),length(lats[[2]]),1))
auxu = array(V.his.djf[lons[[2]], lats[[2]],1], c(length(lons[[2]]),length(lats[[2]]),1))
aux = array(hu6.his_seasons[lons[[2]], lats[[2]],2], c(length(lons[[2]]),length(lats[[2]]),1))
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Humedad específica media CNRM-CM6 1975 - 2005  - DJF", nombre_fig = "hu6.his_sa.djf", escala = c(0,0.02)
          , label_escala = "?", resta = 0, brewer = "PuBuGn", revert = "no", niveles = 9, contour = "si"
          , lon = lon[lons[[2]]], lat = lat[lats[[2]]], escala_dis = seq(0, 0.02, by = 0.002), breaks_c_f = seq(0, 0.02, by = 0.002), r = 1, na_fill = -1000
          , topo = "topo2", altura = 1000, salida = "/Salidas/TP2/")










#####