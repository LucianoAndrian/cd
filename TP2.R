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

source("TP2_carga.R")

#### GRAFICOS  ####

# esto esta asi nomas...

##--- HISTORICO --##
#--- Asia ---#
#### CNRM-CM5 ####


#--- Temperatura ---#
# no hay


#--- Precipitacion ---#

aux = array(pp5.his_seasons[lons[[1]], lats[[1]],1], c(length(lons[[1]]),length(lats[[1]]),1))
mapa_topo2(lista = aux, titulo = "Precipitación media CNRM-CM5 1975 - 2005  - JJA", nombre_fig = "pp5.his_asia.jja", escala = c(0,600)
           , label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9, contour = "si"
           , lon = lon[lons[[1]]], lat = lat[lats[[1]]], escala_dis = seq(0, 600, by = 50), breaks_c_f = seq(0, 600, by = 50), r = 1, na_fill = -1000
           , topo = "topo1", altura = 1500, salida = "/Salidas/TP2/")

aux = array(pp5.his_seasons[lons[[1]], lats[[1]],2], c(length(lons[[1]]),length(lats[[1]]),1))
mapa_topo2(lista = aux, titulo = "Precipitación media CNRM-CM5 1975 - 2005  - DFJ", nombre_fig = "pp5.his_asia.djf", escala = c(0,600)
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

i5.26 = MonsoonIntensity(area.his = area5.his, area.49 = area5.26_49, area.99 = area5.26_99, lons, lats, km = 1)
i5.85 = MonsoonIntensity(area.his = area5.his, area.49 = area5.85_49, area.99 = area5.85_99, lons, lats, km = 1)

i6.26 = MonsoonIntensity(area.his = area6.his, area.49 = area6.26_49, area.99 = area6.26_99, lons, lats, km = 1)
i6.85 = MonsoonIntensity(area.his = area6.his, area.49 = area6.85_49, area.99 = area6.85_99, lons, lats, km = 1)

## grafico ##
aux = list()
aux[[1]] = data.frame(Periodos = c(2000, 2050, 2100), i5.26)
aux[[2]] = data.frame(Periodos = c(2000, 2050, 2100), i5.85)
aux[[3]] = data.frame(Periodos = c(2000, 2050, 2100), i6.26)
aux[[4]] = data.frame(Periodos = c(2000, 2050, 2100), i6.85)
titulo = c("CNRM-CM5 RCP2.6", "CNRM-CM5 RCP5", "CNRM-CM6 SSP1-2.6", "CNRM-CM6 SSP5-8.5")
png(filename = "intensidades.jpg", width = 850, height = 520, units = "px")
par(mfrow=c(2,2))
for(i in 1:4){
  
  
  df.ts = ts(aux[[i]][-1], start = 2000, deltat = 50)
  plot(df.ts, plot.type = "single", col = 1:ncol(df.ts), lwd = 4, ylab = "mm/p.grilla", main = titulo[i])
  abline(h = 0, lwd = 0.5)
  legend("topleft", colnames(df.ts), col=1:ncol(aux2), lty=1, cex=1.5, lwd = 1)
  
  
  
}
dev.off()


#### Analisis final monzoon ####
# los meses estan en [[3]] de los Rdata, estan promedidaso los miembros y anios de marzo a feb.
 
pp6.sep = PPMeses(modelo = "6", mes = 7)  # asia

pp6.nov = PPMeses(modelo = "6", mes = 9)  # SA
pp6.mar = PPMeses(modelo = "6", mes = 1)  # SA

aux = (pp6.sep[[4]] - pp6.sep[[1]])
aux2 = array(aux[lons[[1]], lats[[1]]], dim = c(144, 73, 1))

mapa_topo2(lista = aux2, titulo = "CM6 Dif. PP 2020 - 2049 vs Periodo Historico - Septiembre", nombre_fig = "pp.dif49_sep", escala = c(-200, 200)
           , label_escala = "mm", resta = 0, brewer = "RdBu", revert = "si", niveles = 9
           , contour = "si", lon = lon[lons[[1]]], lat = lat[lats[[1]]], escala_dis = seq(-200, 200, 25)
           , breaks_c_f = seq(-200, 200, 25), r = 1, na_fill = -1000, topo = "topo1", altura = 1500, salida = "/Salidas/TP2/")




aux = (pp6.sep[[5]] - pp6.sep[[1]])
aux2 = array(aux[lons[[1]], lats[[1]]], dim = c(144, 73, 1))

mapa_topo2(lista = aux2, titulo = "CM6 Dif. PP 2070 - 2099 vs Periodo Historico - Septiembre", nombre_fig = "pp.dif99_sep", escala = c(-200, 200)
           , label_escala = "mm", resta = 0, brewer = "RdBu", revert = "si", niveles = 9
           , contour = "si", lon = lon[lons[[1]]], lat = lat[lats[[1]]], escala_dis = seq(-200, 200, 25)
           , breaks_c_f = seq(-200, 200, 25), r = 1, na_fill = -1000, topo = "topo1", altura = 1500, salida = "/Salidas/TP2/")



###
aux = (pp6.mar[[5]] - pp6.mar[[1]])

aux2 = array(aux[lons[[2]], lats[[2]]], dim = c(144, 73, 1))

mapa_topo2(lista = aux2, titulo = "CM6 Dif. PP 2020 - 2049 vs Periodo Historico - Marzo", nombre_fig = "pp.dif99_mar", escala = c(-100, 100)
           , label_escala = "mm", resta = 0, brewer = "RdBu", revert = "si", niveles = 9
           , contour = "si", lon = lon[lons[[2]]], lat = lat[lats[[2]]], escala_dis = seq(-100, 100, 20)
           , breaks_c_f = seq(-100, 100, 20), r = 1, na_fill = -1000, topo = "topo2", altura = 1500, salida = "/Salidas/TP2/")

#### Advecciones de T y q ####
# solo para CNRM-CM6
#### Historico ####
t6.his = list()
hu6.his = list()
t6.his[[1]] = Adv(variable = t6.his_seasons, u = V.his.jja[,,1], v = V.his.jja[,,2], i = 1)
t6.his[[2]] = Adv(variable = t6.his_seasons, u = V.his.djf[,,1], v = V.his.djf[,,2], i = 2)

hu6.his[[1]] = Adv(variable = hu6.his_seasons, u = V.his.jja[,,1], v = V.his.jja[,,2], i = 1)
hu6.his[[2]] = Adv(variable = hu6.his_seasons, u = V.his.djf[,,1], v = V.his.djf[,,2], i = 2)

topo = c("topo1", "topo2")

aux = array(t6.his[[1]], dim = c(dim(t6.his[[1]]), 1))
auxu = array(V.his.jja[lons[[1]],lats[[1]],1], dim = c(dim(t6.his[[1]]),1 ))
auxv = array(V.his.jja[lons[[1]],lats[[1]],2], dim = c(dim(t6.his[[1]]),1 ))
revert = "si"; color = "RdBu"; breaks = seq(-10, 10, by = 2); escala = c(-10, 10); vnom = "t6"
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Advección T periodo Historico"
          , nombre_fig = "t6.his.adv_asia", escala = escala
          , label_escala = "", resta = 0, brewer = color, revert = revert, niveles = 9
          , contour = "si", lon = lon[lons[[1]]], lat = lat[lats[[1]]], escala_dis = breaks
          , breaks_c_f = breaks, r = 1, na_fill = 0, topo = topo[1], altura = 1500, salida = "/Salidas/TP2/")


aux = array(t6.his[[2]], dim = c(dim(t6.his[[2]]), 1))
auxu = array(V.his.djf[lons[[2]],lats[[2]],1], dim = c(dim(t6.his[[2]]),1 ))
auxv = array(V.his.djf[lons[[2]],lats[[2]],2], dim = c(dim(t6.his[[2]]),1 ))
revert = "si"; color = "RdBu"; breaks = seq(-10, 10, by = 2); escala = c(-10, 10); vnom = "t6"
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Advección T periodo Historico"
          , nombre_fig = "t6.his.adv_sa", escala = escala
          , label_escala = "", resta = 0, brewer = color, revert = revert, niveles = 9
          , contour = "si", lon = lon[lons[[2]]], lat = lat[lats[[2]]], escala_dis = breaks
          , breaks_c_f = breaks, r = 1, na_fill = 0, topo = topo[2], altura = 1500, salida = "/Salidas/TP2/")


aux = array(hu6.his[[1]], dim = c(dim(hu6.his[[1]]), 1))
auxu = array(V.his.jja[lons[[1]],lats[[1]],1], dim = c(dim(hu6.his[[1]]),1 ))
auxv = array(V.his.jja[lons[[1]],lats[[1]],2], dim = c(dim(hu6.his[[1]]),1 ))
tq = "q"; revert = "no"; color = "BrBG"; breaks = seq(-0.010, 0.010, by = 0.002); escala = c(-0.010, 0.010); vnom = "hu6"
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Advección q perido Historico"
          , nombre_fig = "hu6.his.adv_asia", escala = escala
          , label_escala = "", resta = 0, brewer = color, revert = revert, niveles = 9
          , contour = "si", lon = lon[lons[[1]]], lat = lat[lats[[1]]], escala_dis = breaks
          , breaks_c_f = breaks, r = 1, na_fill = 0, topo = topo[1], altura = 1500, salida = "/Salidas/TP2/")


aux = array(hu6.his[[2]], dim = c(dim(hu6.his[[2]]), 1))
auxu = array(V.his.djf[lons[[2]],lats[[2]],1], dim = c(dim(hu6.his[[2]]),1 ))
auxv = array(V.his.djf[lons[[2]],lats[[2]],2], dim = c(dim(hu6.his[[2]]),1 ))
tq = "q"; revert = "no"; color = "BrBG"; breaks = seq(-0.010, 0.010, by = 0.002); escala = c(-0.010, 0.010); vnom = "hu6"
mapa_topo(lista = aux, u = auxu, v = auxv, titulo = "Advección periodo Historico"
          , nombre_fig = "hu6.his.adv_sa", escala = escala
          , label_escala = "", resta = 0, brewer = color, revert = revert, niveles = 9
          , contour = "si", lon = lon[lons[[2]]], lat = lat[lats[[2]]], escala_dis = breaks
          , breaks_c_f = breaks, r = 1, na_fill = 0, topo = topo[2], altura = 1500, salida = "/Salidas/TP2/")

#### rcps #####
adv.sa = array(data = NA, dim = c(43, 33, 8))
adv.asia = array(data = NA, dim = c(41, 27, 8))
ADV =list()
ADV[[2]] = adv.sa
ADV[[1]] = adv.asia


u = list(); v = list()

u[[1]] = u6.26_49.seasons; u[[2]] = u6.26_99.seasons
u[[3]] = u6.85_49.seasons; u[[4]] = u6.85_99.seasons
u[[5]] = u6.26_49.seasons; u[[6]] = u6.26_99.seasons # para no complicarla en el for
u[[7]] = u6.85_49.seasons; u[[8]] = u6.85_99.seasons

v[[1]] = v6.26_49.seasons; v[[2]] = v6.26_99.seasons
v[[3]] = v6.85_49.seasons; v[[4]] = v6.85_99.seasons
v[[5]] = v6.26_49.seasons; v[[6]] = v6.26_99.seasons
v[[7]] = v6.85_49.seasons; v[[8]] = v6.85_99.seasons

for(j in 1:2){
  
  # adv T
  ADV[[j]][,,1] = Adv(variable = t6.26_49.seasons, u = u6.26_49.seasons[,,j], v = v6.26_49.seasons[,,j], i = j)
  ADV[[j]][,,2] = Adv(variable = t6.26_99.seasons, u = u6.26_99.seasons[,,j], v = v6.26_99.seasons[,,j], i = j)
  
  ADV[[j]][,,3] = Adv(variable = t6.85_49.seasons, u = u6.85_49.seasons[,,j], v = v6.85_49.seasons[,,j], i = j)
  
  ADV[[j]][,,4] = Adv(variable = t6.85_99.seasons, u = u6.85_99.seasons[,,j], v = v6.85_99.seasons[,,j], i = j)
  
  ADV[[j]][,,5] = Adv(variable = hu6.26_49.seasons, u = u6.26_49.seasons[,,j], v = v6.26_49.seasons[,,j], i = j)
  ADV[[j]][,,6] = Adv(variable = hu6.26_99.seasons, u = u6.26_99.seasons[,,j], v = v6.26_99.seasons[,,j], i = j)
  
  ADV[[j]][,,7] = Adv(variable = hu6.85_49.seasons, u = u6.85_49.seasons[,,j], v = v6.85_49.seasons[,,j], i = j)
  ADV[[j]][,,8] = Adv(variable = hu6.85_99.seasons, u = u6.85_99.seasons[,,j], v = v6.85_99.seasons[,,j], i = j)
  
}

topo = c("topo1", "topo2")
for(z in 1:2){
 
   for(j in 1:8){
    
    aux = array(ADV[[z]][,,j], dim = c(dim(ADV[[z]][,,1]),1))
    auxu = array(u[[j]][lons[[z]],lats[[z]],z], c(dim(u[[j]])))
    auxv = array(v[[j]][lons[[z]],lats[[z]],z], c(dim(v[[j]])))
    
    anio = ifelse(test = j%%2 == 0, yes = "99", no = "49")
    
    if(j > 4){
      tq = "q"; revert = "no"; color = "BrBG"; breaks = seq(-0.010, 0.010, by = 0.002); escala = c(-0.010, 0.010); vnom = "hu6"
    } else {
      tq = "T"; revert = "si"; color = "RdBu"; breaks = seq(-10, 10, by = 2); escala = c(-10, 10); vnom = "t6"
    }
    
    rcp = ifelse(test = j<=2 | j<=6 & j>=5, yes = "26", no = "85")
    ssp = ifelse(test = j<=2 | j<=6 & j>=5, yes = "SSP1-2.6", no = "SSP5-8.5")
  
    
    
    mapa_topo(lista = aux, u = auxu, v = auxv, titulo = paste("Advección ", tq," y viento en 850hPa 2020 - 20", anio, " ", ssp, sep = "")
              , nombre_fig = paste(vnom, ".", rcp, "_", anio,".adv_", nombre[z], sep = ""), escala = escala
              , label_escala = "", resta = 0, brewer = color, revert = revert, niveles = 9
              , contour = "si", lon = lon[lons[[z]]], lat = lat[lats[[z]]], escala_dis = breaks
              , breaks_c_f = breaks, r = 1, na_fill = 0, topo = topo[z], altura = 1500, salida = "/Salidas/TP2/")
    
    
  }
}



#### Graficos para presentacion ####
# hacer con mapa_topo3, 
# periodos historicos de pp marcando la zona monzonica. 
# (esto mismo se podria hacer para el resto de los graficos q se van a usar de PP(3~4), tambien para los de adveccion)
# ver, si en los de temp y hu, es conveniente marcar los maximos de pp o la zona monzonica de la misma forma