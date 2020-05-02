# TP2 Simulacion del clima 2020 - sistemas monzonicos
# apertura y guardado de datos ya organizados con Apertura_datos_mensuales.R

# source("Apertura_datos_mensuales.R") #tarda 1:30hr...

# NO cargar todos los archivos consume todo la ram. 
lon = read.table("lon.txt")[,1]
lat = read.table("lat.txt")[,1]

mask = as.matrix(read.table("mask.txt"))

# dominios 
# monson asiatico: lats: -5, 37; lons = 37-40, 103
# monson sudamericano:

lats = list()
lons = list()
lats[[1]] = seq(which(lat == -10),which(lat == 35)); lons[[1]] = seq(which(lon == 40), which(lon == 102.5))
lats[[2]] = seq(which(lat == -35),which(lat == 20)); lons[[2]] = seq(which(lon == 270), which(lon == 330))


#### Temperatura #### 
#CNRM-CM5 no tiene datos mensuales

# CNRM-CM6
# campo medio de T para JJA y DJF
# historico #
load("RDatas/t6.his.RData")
# usando t6.his[[2]] ya se encuentran promediados los anios

t6.his.mean_jja = apply(t6.his[[1]][,,2,], c(1,2), mean, na.rm = T) - 273# ensamble de los miembros
t6.his.mean_djf = apply(t6.his[[1]][,,4,], c(1,2), mean, na.rm = T) - 273

# puede ser interezante ver condiciones en estaciones de transicion
t6.his.mean_mam = apply(t6.his[[1]][,,1,], c(1,2), mean, na.rm = T) - 273# ensamble de los miembros
t6.his.mean_son = apply(t6.his[[1]][,,3,], c(1,2), mean, na.rm = T) - 273

# graficar

aux = array(t6.his.mean_djf[lons[[2]], lats[[2]]], c(length(lons[[2]]),length(lats[[2]]),1))
mapa_topo(lista = aux, titulo = "prueba", nombre_fig = "prueba", escala = c(20,30)
          , label_escala = "ºC", resta = 0, brewer = "Spectral", revert = "si", niveles = 11, contour = "si"
          , lon = lon[lons[[2]]], lat = lat[lats[[2]]], escala_dis = seq(20, 30, by = 1), breaks_c_f = seq(20, 30, by = 1), r = 1, na_fill = -1000
          , topo = "topo2", altura = 1000, salida = "/Salidas/TP2/")
