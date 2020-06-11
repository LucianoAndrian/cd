# TP4 Balance de Masa ( E y P)
# trabjar con RDatas guardados en TP4_apertura_y_seleccion.R
source("FUNCIONES.R")

# para pesar los promedios por la latitud
lat = read.table("lat.txt")
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30))

lon = read.table("lon.txt")

#### tendencia de P y E  y promedio zonal####
mask = as.matrix(read.table("mask.txt"))
mask2 = mask; mask2[which(is.na(mask2))] = 2; mask2[which(mask2==1)] = NA; mask2[which(mask2==2)] = 1
mask = array(mask, c(144,73,30)); mask2  = array(mask2, c(144,73,30))

#---- CNRM-CM5 ----#
#---- HÇISTORICAL ----#
load("RDatas/TP4.RDatas/pp5.his.RData"); load("RDatas/TP4.RDatas/evap5.his.RData")
load("RDatas/TP4.RDatas/t5.his.RData")#;  load("RDatas/TP4.RDatas/hu5.his.RData")
pp = pp5.his[[1]]  # prueba...(ram)
evap = evap5.his[[1]]
temp = t5.his[[1]]
# # hu = hu5.his[[1]]

PlotLat(pp = pp, evap = evap, titulo = "CNRM-CM5 - Historico", y.breaks = seq(0, 3000, by = 500), nombre = "5.his", salida = "/Salidas/TP4/Lat/")

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)


pp.ts = apply(pp_ens*lats, c(3), mean, na.rm = T)
evap.ts = apply(evap_ens*lats, c(3), mean, na.rm = T)
temp.ts = apply(temp_ens*lats, c(3), mean, na.rm = T)
# hu.ts = apply(# hu_ens*lats, c(3), mean, na.rm = T)

pp = Tendencia.ts(pp.ts)
evap = Tendencia.ts(evap.ts)
temp = Tendencia.ts(temp.ts)
# hu = Tendencia.ts(# hu.ts)

datos = as.data.frame(pp[[1]])
datos = cbind(datos, Años = seq(1976, 2005, by = 1))
datos = cbind(datos, as.data.frame(evap[[1]]))

PlotTs(datos = datos, escala = seq(690, 725, by = 5), escala2 = seq(-1.5, 1.5, by = 0.25), titulo = "CNRM-CM5 Historical", nombre = "5.his.ts")

pp.t.f = Tendencia(pp_ens, s = 0.9)
evap.t.f = Tendencia(evap_ens, s = 0.9)

auxx = pp.t.f; titulo = "Tendencia de PP - CNRM-CM5 Historico"; nombre.fig = "p5.his.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdBu", niveles = 11, revert = T, escala = seq(-5,5, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


auxx = evap.t.f; titulo = "Tendencia de E - CNRM-CM5 Historico"; nombre.fig = "e5.his.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "PRGn", niveles = 11, revert = T, escala = seq(-5,5, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


# p-e
auxx = apply(pp_ens - evap_ens, c(1,2), mean, na.rm = T) ; titulo = "P - E  CNRM-CM5 Historico"; nombre.fig = "pe5.his.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlGn", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")






#---- RCP2.6 2020  2049----#
mask = as.matrix(read.table("mask.txt"))
mask2 = mask; mask2[which(is.na(mask2))] = 2; mask2[which(mask2==1)] = NA; mask2[which(mask2==2)] = 1
mask = array(mask, c(144,73,30)); mask2  = array(mask2, c(144,73,30))
load("RDatas/TP4.RDatas/pp5.26_49.RData"); load("RDatas/TP4.RDatas/evap5.26_49.RData")
load("RDatas/TP4.RDatas/t5.26_49.RData")#;  load("RDatas/TP4.RDatas/hu5.26_49.RData")
pp = pp5.26_49[[1]]  # prueba...(ram)
evap = evap5.26_49[[1]]
temp = t5.26_49[[1]]
# hu = hu5.26_49[[1]]

PlotLat(pp = pp, evap = evap, titulo = "CNRM-CM5 RCP2.6  2020 - 2049", y.breaks = seq(0, 3000, by = 500), nombre = "5.26_49", salida = "/Salidas/TP4/Lat/")

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)


pp.ts = apply(pp_ens*lats, c(3), mean, na.rm = T)
evap.ts = apply(evap_ens*lats, c(3), mean, na.rm = T)
temp.ts = apply(temp_ens*lats, c(3), mean, na.rm = T)
# hu.ts = apply(# hu_ens*lats, c(3), mean, na.rm = T)

pp = Tendencia.ts(pp.ts)
evap = Tendencia.ts(evap.ts)
temp = Tendencia.ts(temp.ts)
# hu = Tendencia.ts(# hu.ts)

datos = as.data.frame(pp[[1]])
datos = cbind(datos, Años = seq(2020, 2049, by = 1))
datos = cbind(datos, as.data.frame(evap[[1]]))

PlotTs(datos = datos, escala = seq(690, 725, by = 5), escala2 = seq(-1.5, 1.5, by = 0.25), titulo = "CNRM-CM5 RCP2.6  2020 - 2049", nombre = "5.26_49.ts")

pp.t.f = Tendencia(pp_ens, s = 0.9)
evap.t.f = Tendencia(evap_ens, s = 0.9)

auxx = pp.t.f; titulo = "Tendencia de PP - CNRM-CM5 RCP2.6  2020 - 2049 "; nombre.fig = "p5.26_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdBu", niveles = 11, revert = T, escala = seq(-5,5, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


auxx = evap.t.f; titulo = "Tendencia de E - CNRM-CM5 RCP2.6  2020 - 2049"; nombre.fig = "e5.26_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "PRGn", niveles = 11, revert = T, escala = seq(-5,5, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


# p-e
auxx = apply(pp_ens - evap_ens, c(1,2), mean, na.rm = T) ; titulo = "P - E  CNRM-CM5 RCP2.6 2020 - 2049"; nombre.fig = "pe5.26_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlGn", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")





#---- RCP2.6 2070  2099----#
mask = as.matrix(read.table("mask.txt"))
mask2 = mask; mask2[which(is.na(mask2))] = 2; mask2[which(mask2==1)] = NA; mask2[which(mask2==2)] = 1
mask = array(mask, c(144,73,30)); mask2  = array(mask2, c(144,73,30))
load("RDatas/TP4.RDatas/pp5.26_99.RData"); load("RDatas/TP4.RDatas/evap5.26_99.RData")
load("RDatas/TP4.RDatas/t5.26_99.RData")#;  load("RDatas/TP4.RDatas/hu5.26_99.RData")
pp = pp5.26_99[[1]]  # prueba...(ram)
evap = evap5.26_99[[1]]
temp = t5.26_99[[1]]
# hu = hu5.26_99[[1]]

PlotLat(pp = pp, evap = evap, titulo = "CNRM-CM5 RCP2.6  2070 - 2099", y.breaks = seq(0, 3000, by = 500), nombre = "5.26_99", salida = "/Salidas/TP4/Lat/")

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)


pp.ts = apply(pp_ens*lats, c(3), mean, na.rm = T)
evap.ts = apply(evap_ens*lats, c(3), mean, na.rm = T)
temp.ts = apply(temp_ens*lats, c(3), mean, na.rm = T)
# hu.ts = apply(# hu_ens*lats, c(3), mean, na.rm = T)

pp = Tendencia.ts(pp.ts)
evap = Tendencia.ts(evap.ts)
temp = Tendencia.ts(temp.ts)
# hu = Tendencia.ts(# hu.ts)

datos = as.data.frame(pp[[1]])
datos = cbind(datos, Años = seq(2070, 2099, by = 1))
datos = cbind(datos, as.data.frame(evap[[1]]))

PlotTs(datos = datos, escala = seq(690, 725, by = 5), escala2 = seq(-1.5, 1.5, by = 0.25), titulo = "CNRM-CM5 RCP2.6  2070 - 2099", nombre = "5.26_99.ts")

pp.t.f = Tendencia(pp_ens, s = 0.9)
evap.t.f = Tendencia(evap_ens, s = 0.9)

auxx = pp.t.f; titulo = "Tendencia de PP - CNRM-CM5 RCP2.6  2070 - 2099 "; nombre.fig = "p5.26_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdBu", niveles = 11, revert = T, escala = seq(-5,5, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


auxx = evap.t.f; titulo = "Tendencia de E - CNRM-CM5 RCP2.6  2070 - 2099"; nombre.fig = "e5.26_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "PRGn", niveles = 11, revert = T, escala = seq(-5,5, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


# p-e
auxx = apply(pp_ens - evap_ens, c(1,2), mean, na.rm = T) ; titulo = "P - E  CNRM-CM5 RCP2.6 2070 - 2099"; nombre.fig = "pe5.26_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlGn", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")



#---- RCP8.5 2020  2049----#
mask = as.matrix(read.table("mask.txt"))
mask2 = mask; mask2[which(is.na(mask2))] = 2; mask2[which(mask2==1)] = NA; mask2[which(mask2==2)] = 1
mask = array(mask, c(144,73,30)); mask2  = array(mask2, c(144,73,30))
load("RDatas/TP4.RDatas/pp5.85_49.RData"); load("RDatas/TP4.RDatas/evap5.85_49.RData")
load("RDatas/TP4.RDatas/t5.85_49.RData")#;  load("RDatas/TP4.RDatas/hu5.85_49.RData")
pp = pp5.85_49[[1]]  # prueba...(ram)
evap = evap5.85_49[[1]]
temp = t5.85_49[[1]]
# hu = hu5.85_49[[1]]

PlotLat(pp = pp, evap = evap, titulo = "CNRM-CM5 RCP8.5  2020 - 2049", y.breaks = seq(0, 3000, by = 500), nombre = "5.85_49", salida = "/Salidas/TP4/Lat/")

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)


pp.ts = apply(pp_ens*lats, c(3), mean, na.rm = T)
evap.ts = apply(evap_ens*lats, c(3), mean, na.rm = T)
temp.ts = apply(temp_ens*lats, c(3), mean, na.rm = T)
# hu.ts = apply(# hu_ens*lats, c(3), mean, na.rm = T)

pp = Tendencia.ts(pp.ts)
evap = Tendencia.ts(evap.ts)
temp = Tendencia.ts(temp.ts)
# hu = Tendencia.ts(# hu.ts)

datos = as.data.frame(pp[[1]])
datos = cbind(datos, Años = seq(2020, 2049, by = 1))
datos = cbind(datos, as.data.frame(evap[[1]]))

PlotTs(datos = datos, escala = seq(690, 725, by = 5), escala2 = seq(-1.5, 1.5, by = 0.25), titulo = "CNRM-CM5 RCP8.5  2020 - 2049", nombre = "5.85_49.ts")

pp.t.f = Tendencia(pp_ens, s = 0.9)
evap.t.f = Tendencia(evap_ens, s = 0.9)

auxx = pp.t.f; titulo = "Tendencia de PP - CNRM-CM5 RCP8.5  2020 - 2049 "; nombre.fig = "p5.85_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdBu", niveles = 11, revert = T, escala = seq(-5,5, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


auxx = evap.t.f; titulo = "Tendencia de E - CNRM-CM5 RCP8.5  2020 - 2049"; nombre.fig = "e5.85_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "PRGn", niveles = 11, revert = T, escala = seq(-5,5, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


# p-e
auxx = apply(pp_ens - evap_ens, c(1,2), mean, na.rm = T) ; titulo = "P - E  CNRM-CM5 RCP8.5 2020 - 2049"; nombre.fig = "pe5.85_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlGn", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")





#---- RCP8.5 2070  2099----#
mask = as.matrix(read.table("mask.txt"))
mask2 = mask; mask2[which(is.na(mask2))] = 2; mask2[which(mask2==1)] = NA; mask2[which(mask2==2)] = 1
mask = array(mask, c(144,73,30)); mask2  = array(mask2, c(144,73,30))
load("RDatas/TP4.RDatas/pp5.85_99.RData"); load("RDatas/TP4.RDatas/evap5.85_99.RData")
load("RDatas/TP4.RDatas/t5.85_99.RData")#;  load("RDatas/TP4.RDatas/hu5.85_99.RData")
pp = pp5.85_99[[1]]  # prueba...(ram)
evap = evap5.85_99[[1]]
temp = t5.85_99[[1]]
# hu = hu5.85_99[[1]]

PlotLat(pp = pp, evap = evap, titulo = "CNRM-CM5 RCP8.5  2070 - 2099", y.breaks = seq(0, 3000, by = 500), nombre = "5.85_99", salida = "/Salidas/TP4/Lat/")

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)


pp.ts = apply(pp_ens*lats, c(3), mean, na.rm = T)
evap.ts = apply(evap_ens*lats, c(3), mean, na.rm = T)
temp.ts = apply(temp_ens*lats, c(3), mean, na.rm = T)
# hu.ts = apply(# hu_ens*lats, c(3), mean, na.rm = T)

pp = Tendencia.ts(pp.ts)
evap = Tendencia.ts(evap.ts)
temp = Tendencia.ts(temp.ts)
# hu = Tendencia.ts(# hu.ts)

datos = as.data.frame(pp[[1]])
datos = cbind(datos, Años = seq(2070, 2099, by = 1))
datos = cbind(datos, as.data.frame(evap[[1]]))

PlotTs(datos = datos, escala = seq(700, 760, by = 5), escala2 = seq(-1.5, 1.5, by = 0.25), titulo = "CNRM-CM5 RCP8.5  2070 - 2099", nombre = "5.85_99.ts")

pp.t.f = Tendencia(pp_ens, s = 0.9)
evap.t.f = Tendencia(evap_ens, s = 0.9)

auxx = pp.t.f; titulo = "Tendencia de PP - CNRM-CM5 RCP8.5  2070 - 2099 "; nombre.fig = "p5.85_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdBu", niveles = 11, revert = T, escala = seq(-5,5, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


auxx = evap.t.f; titulo = "Tendencia de E - CNRM-CM5 RCP8.5  2070 - 2099"; nombre.fig = "e5.85_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "PRGn", niveles = 11, revert = T, escala = seq(-5,5, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


# p-e
auxx = apply(pp_ens - evap_ens, c(1,2), mean, na.rm = T) ; titulo = "P - E  CNRM-CM5 RCP8.5 2070 - 2099"; nombre.fig = "pe5.85_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlGn", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")




#--- CNRM-CM6 ---#
#---- HÇISTORICAL ----#
mask = as.matrix(read.table("mask.txt"))
mask2 = mask; mask2[which(is.na(mask2))] = 2; mask2[which(mask2==1)] = NA; mask2[which(mask2==2)] = 1
mask = array(mask, c(144,73,30)); mask2  = array(mask2, c(144,73,30))
load("RDatas/TP4.RDatas/pp6.his.RData"); load("RDatas/TP4.RDatas/evap6.his.RData")
load("RDatas/TP4.RDatas/t6.his.RData")#;  load("RDatas/TP4.RDatas/hu6.his.RData")
pp = pp6.his[[1]]  # prueba...(ram)
evap = evap6.his[[1]]
temp = t6.his[[1]]
# hu = hu6.his[[1]]

PlotLat(pp = pp, evap = evap, titulo = "CNRM-CM6 - Historico", y.breaks = seq(0, 3000, by = 500), nombre = "6.his", salida = "/Salidas/TP4/Lat/")

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)


pp.ts = apply(pp_ens*lats, c(3), mean, na.rm = T)
evap.ts = apply(evap_ens*lats, c(3), mean, na.rm = T)
temp.ts = apply(temp_ens*lats, c(3), mean, na.rm = T)
# hu.ts = apply(# hu_ens*lats, c(3), mean, na.rm = T)

pp = Tendencia.ts(pp.ts)
evap = Tendencia.ts(evap.ts)
temp = Tendencia.ts(temp.ts)
# hu = Tendencia.ts(# hu.ts)

datos = as.data.frame(pp[[1]])
datos = cbind(datos, Años = seq(1976, 2005, by = 1))
datos = cbind(datos, as.data.frame(evap[[1]]))

PlotTs(datos = datos, escala = seq(660, 670, by = 1), escala2 = seq(-1.5, 1.5, by = 0.25), titulo = "CNRM-CM6 Historical", nombre = "6.his.ts")

pp.t.f = Tendencia(pp_ens, s = 0.9)
evap.t.f = Tendencia(evap_ens, s = 0.9)

auxx = pp.t.f; titulo = "Tendencia de PP - CNRM-CM6 Historico"; nombre.fig = "p6.his.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdBu", niveles = 11, revert = T, escala = seq(-5,5, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


auxx = evap.t.f; titulo = "Tendencia de E - CNRM-CM6 Historico"; nombre.fig = "e6.his.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "PRGn", niveles = 11, revert = T, escala = seq(-5,5, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


# p-e
auxx = apply(pp_ens - evap_ens, c(1,2), mean, na.rm = T) ; titulo = "P - E  CNRM-CM6 Historico"; nombre.fig = "pe6.his.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlGn", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")






#---- SSP126 2020  2049----#
mask = as.matrix(read.table("mask.txt"))
mask2 = mask; mask2[which(is.na(mask2))] = 2; mask2[which(mask2==1)] = NA; mask2[which(mask2==2)] = 1
mask = array(mask, c(144,73,30)); mask2  = array(mask2, c(144,73,30))
load("RDatas/TP4.RDatas/pp6.26_49.RData"); load("RDatas/TP4.RDatas/evap6.26_49.RData")
load("RDatas/TP4.RDatas/t6.26_49.RData")#;  load("RDatas/TP4.RDatas/hu6.26_49.RData")
pp = pp6.26_49[[1]]  # prueba...(ram)
evap = evap6.26_49[[1]]
temp = t6.26_49[[1]]
# hu = hu6.26_49[[1]]

PlotLat(pp = pp, evap = evap, titulo = "CNRM-CM6 SSP126  2020 - 2049", y.breaks = seq(0, 3000, by = 500), nombre = "6.26_49", salida = "/Salidas/TP4/Lat/")

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)


pp.ts = apply(pp_ens*lats, c(3), mean, na.rm = T)
evap.ts = apply(evap_ens*lats, c(3), mean, na.rm = T)
temp.ts = apply(temp_ens*lats, c(3), mean, na.rm = T)
# hu.ts = apply(# hu_ens*lats, c(3), mean, na.rm = T)

pp = Tendencia.ts(pp.ts)
evap = Tendencia.ts(evap.ts)
temp = Tendencia.ts(temp.ts)
# hu = Tendencia.ts(# hu.ts)

datos = as.data.frame(pp[[1]])
datos = cbind(datos, Años = seq(2020, 2049, by = 1))
datos = cbind(datos, as.data.frame(evap[[1]]))

PlotTs(datos = datos, escala = seq(660, 690, by = 5), escala2 = seq(-1.5, 1.5, by = 0.25), titulo = "CNRM-CM6 SSP126  2020 - 2049", nombre = "6.26_49.ts")

pp.t.f = Tendencia(pp_ens, s = 0.9)
evap.t.f = Tendencia(evap_ens, s = 0.9)

auxx = pp.t.f; titulo = "Tendencia de PP - CNRM-CM6 SSP126  2020 - 2049 "; nombre.fig = "p6.26_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdBu", niveles = 11, revert = T, escala = seq(-5,5, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


auxx = evap.t.f; titulo = "Tendencia de E - CNRM-CM6 SSP126  2020 - 2049"; nombre.fig = "e6.26_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "PRGn", niveles = 11, revert = T, escala = seq(-5,5, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


# p-e
auxx = apply(pp_ens - evap_ens, c(1,2), mean, na.rm = T) ; titulo = "P - E  CNRM-CM6 SSP126 2020 - 2049"; nombre.fig = "pe6.26_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlGn", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")





#---- SSP126 2070  2099----#
mask = as.matrix(read.table("mask.txt"))
mask2 = mask; mask2[which(is.na(mask2))] = 2; mask2[which(mask2==1)] = NA; mask2[which(mask2==2)] = 1
mask = array(mask, c(144,73,30)); mask2  = array(mask2, c(144,73,30))
load("RDatas/TP4.RDatas/pp6.26_99.RData"); load("RDatas/TP4.RDatas/evap6.26_99.RData")
load("RDatas/TP4.RDatas/t6.26_99.RData")#;  load("RDatas/TP4.RDatas/hu6.26_99.RData")
pp = pp6.26_99[[1]]  # prueba...(ram)
evap = evap6.26_99[[1]]
temp = t6.26_99[[1]]
# hu = hu6.26_99[[1]]

PlotLat(pp = pp, evap = evap, titulo = "CNRM-CM6 SSP126  2070 - 2099", y.breaks = seq(0, 3000, by = 500), nombre = "6.26_99", salida = "/Salidas/TP4/Lat/")

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)


pp.ts = apply(pp_ens*lats, c(3), mean, na.rm = T)
evap.ts = apply(evap_ens*lats, c(3), mean, na.rm = T)
temp.ts = apply(temp_ens*lats, c(3), mean, na.rm = T)
# hu.ts = apply(# hu_ens*lats, c(3), mean, na.rm = T)

pp = Tendencia.ts(pp.ts)
evap = Tendencia.ts(evap.ts)
temp = Tendencia.ts(temp.ts)
# hu = Tendencia.ts(# hu.ts)

datos = as.data.frame(pp[[1]])
datos = cbind(datos, Años = seq(2070, 2099, by = 1))
datos = cbind(datos, as.data.frame(evap[[1]]))

PlotTs(datos = datos, escala = seq(660, 690, by = 5), escala2 = seq(-1.5, 1.5, by = 0.25), titulo = "CNRM-CM6 SSP126  2070 - 2099", nombre = "6.26_99.ts")

pp.t.f = Tendencia(pp_ens, s = 0.9)
evap.t.f = Tendencia(evap_ens, s = 0.9)

auxx = pp.t.f; titulo = "Tendencia de PP - CNRM-CM6 SSP126  2070 - 2099 "; nombre.fig = "p6.26_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdBu", niveles = 11, revert = T, escala = seq(-5,5, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


auxx = evap.t.f; titulo = "Tendencia de E - CNRM-CM6 SSP126  2070 - 2099"; nombre.fig = "e6.26_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "PRGn", niveles = 11, revert = T, escala = seq(-5,5, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


# p-e
auxx = apply(pp_ens - evap_ens, c(1,2), mean, na.rm = T) ; titulo = "P - E  CNRM-CM6 SSP126 2070 - 2099"; nombre.fig = "pe6.26_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlGn", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")



#---- SSP585 2020  2049----#
mask = as.matrix(read.table("mask.txt"))
mask2 = mask; mask2[which(is.na(mask2))] = 2; mask2[which(mask2==1)] = NA; mask2[which(mask2==2)] = 1
mask = array(mask, c(144,73,30)); mask2  = array(mask2, c(144,73,30))
load("RDatas/TP4.RDatas/pp6.85_49.RData"); load("RDatas/TP4.RDatas/evap6.85_49.RData")
load("RDatas/TP4.RDatas/t6.85_49.RData")#;  load("RDatas/TP4.RDatas/hu6.85_49.RData")
pp = pp6.85_49[[1]]  # prueba...(ram)
evap = evap6.85_49[[1]]
temp = t6.85_49[[1]]
# hu = hu6.85_49[[1]]

PlotLat(pp = pp, evap = evap, titulo = "CNRM-CM6 SSP585  2020 - 2049", y.breaks = seq(0, 3000, by = 500), nombre = "6.85_49", salida = "/Salidas/TP4/Lat/")

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)


pp.ts = apply(pp_ens*lats, c(3), mean, na.rm = T)
evap.ts = apply(evap_ens*lats, c(3), mean, na.rm = T)
temp.ts = apply(temp_ens*lats, c(3), mean, na.rm = T)
# hu.ts = apply(# hu_ens*lats, c(3), mean, na.rm = T)

pp = Tendencia.ts(pp.ts)
evap = Tendencia.ts(evap.ts)
temp = Tendencia.ts(temp.ts)
# hu = Tendencia.ts(# hu.ts)

datos = as.data.frame(pp[[1]])
datos = cbind(datos, Años = seq(2020, 2049, by = 1))
datos = cbind(datos, as.data.frame(evap[[1]]))

PlotTs(datos = datos, escala = seq(660, 690, by = 5), escala2 = seq(-1.5, 1.5, by = 0.25), titulo = "CNRM-CM6 SSP585  2020 - 2049", nombre = "6.85_49.ts")

pp.t.f = Tendencia(pp_ens, s = 0.9)
evap.t.f = Tendencia(evap_ens, s = 0.9)

auxx = pp.t.f; titulo = "Tendencia de PP - CNRM-CM6 SSP585  2020 - 2049 "; nombre.fig = "p6.85_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdBu", niveles = 11, revert = T, escala = seq(-5,5, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


auxx = evap.t.f; titulo = "Tendencia de E - CNRM-CM6 SSP585  2020 - 2049"; nombre.fig = "e6.85_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "PRGn", niveles = 11, revert = T, escala = seq(-5,5, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


# p-e
auxx = apply(pp_ens - evap_ens, c(1,2), mean, na.rm = T) ; titulo = "P - E  CNRM-CM6 SSP585 2020 - 2049"; nombre.fig = "pe6.85_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlGn", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")





#---- SSP585 2070  2099----#
mask = as.matrix(read.table("mask.txt"))
mask2 = mask; mask2[which(is.na(mask2))] = 2; mask2[which(mask2==1)] = NA; mask2[which(mask2==2)] = 1
mask = array(mask, c(144,73,30)); mask2  = array(mask2, c(144,73,30))
load("RDatas/TP4.RDatas/pp6.85_99.RData"); load("RDatas/TP4.RDatas/evap6.85_99.RData")
load("RDatas/TP4.RDatas/t6.85_99.RData")#;  load("RDatas/TP4.RDatas/hu6.85_99.RData")
pp = pp6.85_99[[1]]  # prueba...(ram)
evap = evap6.85_99[[1]]
temp = t6.85_99[[1]]
# hu = hu6.85_99[[1]]

PlotLat(pp = pp, evap = evap, titulo = "CNRM-CM6 SSP585  2070 - 2099", y.breaks = seq(0, 3000, by = 500), nombre = "6.85_99", salida = "/Salidas/TP4/Lat/")

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)


pp.ts = apply(pp_ens*lats, c(3), mean, na.rm = T)
evap.ts = apply(evap_ens*lats, c(3), mean, na.rm = T)
temp.ts = apply(temp_ens*lats, c(3), mean, na.rm = T)
# hu.ts = apply(# hu_ens*lats, c(3), mean, na.rm = T)

pp = Tendencia.ts(pp.ts)
evap = Tendencia.ts(evap.ts)
temp = Tendencia.ts(temp.ts)
# hu = Tendencia.ts(# hu.ts)

datos = as.data.frame(pp[[1]])
datos = cbind(datos, Años = seq(2070, 2099, by = 1))
datos = cbind(datos, as.data.frame(evap[[1]]))

PlotTs(datos = datos, escala = seq(660, 720, by = 10), escala2 = seq(-1.5, 1.5, by = 0.25), titulo = "CNRM-CM6 SSP585  2070 - 2099", nombre = "6.85_99.ts")

pp.t.f = Tendencia(pp_ens, s = 0.9)
evap.t.f = Tendencia(evap_ens, s = 0.9)

auxx = pp.t.f; titulo = "Tendencia de PP - CNRM-CM6 SSP585  2070 - 2099 "; nombre.fig = "p6.85_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdBu", niveles = 11, revert = T, escala = seq(-5,5, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


auxx = evap.t.f; titulo = "Tendencia de E - CNRM-CM6 SSP585  2070 - 2099"; nombre.fig = "e6.85_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "PRGn", niveles = 11, revert = T, escala = seq(-5,5, by = 0.5)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


# p-e
auxx = apply(pp_ens - evap_ens, c(1,2), mean, na.rm = T) ; titulo = "P - E  CNRM-CM6 SSP585 2070 - 2099"; nombre.fig = "pe6.85_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlGn", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")




rm(list = ls())
source("FUNCIONES.R")


#### TABLA 7.1 ####

# CNRM-CM5
# Historical
load("RDatas/TP4.RDatas/pp5.his.RData"); load("RDatas/TP4.RDatas/evap5.his.RData") 
pp = pp5.his[[1]]  # prueba...(ram)
evap = evap5.his[[1]]

tabla = Tabla7.1(pp = pp, evap = evap, nombre = "5his", salida = "/Salidas/TP4/tablas/")

titulo = "P - E CNRM-CM5 - Historico"
nombre = "pe5.his"
Tabla7.1Grafico(tabla = tabla[[1]], v = 3,limites = c(-900, 900), titulo = titulo, nombre = nombre, salida = "/Salidas/TP4/"
                , escala2 = T, limites2 = c(-60, 60), global = T)

Tabla7.1Grafico_Continental(tabla = tabla[[2]], v = 3, limites = c(-900,900), titulo = titulo, nombre = paste(nombre, "_", sep = ""),  salida = "/Salidas/TP4/")

# RCP2.6
# 2020 - 2049
load("RDatas/TP4.RDatas/pp5.26_49.RData"); load("RDatas/TP4.RDatas/evap5.26_49.RData")

pp = pp5.26_49[[1]]  # prueba...(ram)
evap = evap5.26_49[[1]]

tabla = Tabla7.1(pp = pp, evap = evap, nombre = "5.26_49", salida = "/Salidas/TP4/tablas/")

titulo = "P - E CNRM-CM5 RCP2.6 -  2020 - 2099"
nombre = "pe5.26_49"
Tabla7.1Grafico(tabla = tabla[[1]], v = 3,limites = c(-900, 900), titulo = titulo, nombre = nombre, salida = "/Salidas/TP4/"
                , escala2 = T, limites2 = c(-60, 60), global = T)
Tabla7.1Grafico_Continental(tabla = tabla[[2]], v = 3, limites = c(-900,900), titulo = titulo, nombre = paste(nombre, "_", sep = ""),  salida = "/Salidas/TP4/")


# 2070 - 2099
load("RDatas/TP4.RDatas/pp5.26_99.RData"); load("RDatas/TP4.RDatas/evap5.26_99.RData") 

pp = pp5.26_99[[1]]  # prueba...(ram)
evap = evap5.26_99[[1]]

tabla = Tabla7.1(pp = pp, evap = evap, nombre = "5.26_99", salida = "/Salidas/TP4/tablas/")

titulo = "P - E CNRM-CM5 RCP2.6 -  2070 - 2049"
nombre = "pe5.26_99"
Tabla7.1Grafico(tabla = tabla[[1]], v = 3,limites = c(-900, 900), titulo = titulo, nombre = nombre, salida = "/Salidas/TP4/"
                , escala2 = T, limites2 = c(-60, 60), global = T)
Tabla7.1Grafico_Continental(tabla = tabla[[2]], v = 3, limites = c(-900,900), titulo = titulo, nombre = paste(nombre, "_", sep = ""),  salida = "/Salidas/TP4/")


# RCP8.5
# 2020 - 2049
load("RDatas/TP4.RDatas/pp5.85_49.RData"); load("RDatas/TP4.RDatas/evap5.85_49.RData") 

pp = pp5.85_49[[1]]  # prueba...(ram)
evap = evap5.85_49[[1]]

tabla = Tabla7.1(pp = pp, evap = evap, nombre = "5.85_49", salida = "/Salidas/TP4/tablas/")

titulo = "P - E CNRM-CM5 RCP8.5 -  2020 - 2049"
nombre = "pe5.85_49"
Tabla7.1Grafico(tabla = tabla[[1]], v = 3,limites = c(-900, 900), titulo = titulo, nombre = nombre, salida = "/Salidas/TP4/"
                , escala2 = T, limites2 = c(-60, 60), global = T)
Tabla7.1Grafico_Continental(tabla = tabla[[2]], v = 3, limites = c(-900,900), titulo = titulo, nombre = paste(nombre, "_", sep = ""),  salida = "/Salidas/TP4/")


# 2070 - 2099
load("RDatas/TP4.RDatas/pp5.85_99.RData"); load("RDatas/TP4.RDatas/evap5.85_99.RData") 

pp = pp5.85_99[[1]]  # prueba...(ram)
evap = evap5.85_99[[1]]

tabla = Tabla7.1(pp = pp, evap = evap, nombre = "5.85_99", salida = "/Salidas/TP4/tablas/")

titulo = "P - E CNRM-CM5 RCP8.5 -  2070 - 2099"
nombre = "pe5.85_99"
Tabla7.1Grafico(tabla = tabla[[1]], v = 3,limites = c(-900, 900), titulo = titulo, nombre = nombre, salida = "/Salidas/TP4/"
                , escala2 = T, limites2 = c(-60, 60), global = T)
Tabla7.1Grafico_Continental(tabla = tabla[[2]], v = 3, limites = c(-900,900), titulo = titulo, nombre = paste(nombre, "_", sep = ""),  salida = "/Salidas/TP4/")


# CNRM-CM6
# Historical
load("RDatas/TP4.RDatas/pp6.his.RData"); load("RDatas/TP4.RDatas/evap6.his.RData") 
pp = pp6.his[[1]]  # prueba...(ram)
evap = evap6.his[[1]]

tabla = Tabla7.1(pp = pp, evap = evap, nombre = "6his", salida = "/Salidas/TP4/tablas/")

titulo = "P - E CNRM-CM6 - Historico"
nombre = "pe6.his"
Tabla7.1Grafico(tabla = tabla[[1]], v = 3,limites = c(-900, 900), titulo = titulo, nombre = nombre, salida = "/Salidas/TP4/"
                , escala2 = T, limites2 = c(-60, 60), global = T)
Tabla7.1Grafico_Continental(tabla = tabla[[2]], v = 3, limites = c(-900,900), titulo = titulo, nombre = paste(nombre, "_", sep = ""),  salida = "/Salidas/TP4/")


# SSP126
# 2020 - 2049
load("RDatas/TP4.RDatas/pp6.26_49.RData"); load("RDatas/TP4.RDatas/evap6.26_49.RData") 

pp = pp6.26_49[[1]]  # prueba...(ram)
evap = evap6.26_49[[1]]

tabla = Tabla7.1(pp = pp, evap = evap, nombre = "6.26_49", salida = "/Salidas/TP4/tablas/")

titulo = "P - E CNRM-CM6 SSP126 -  2020 - 2049"
nombre = "pe6.26_49"
Tabla7.1Grafico(tabla = tabla[[1]], v = 3,limites = c(-900, 900), titulo = titulo, nombre = nombre, salida = "/Salidas/TP4/"
                , escala2 = T, limites2 = c(-60, 60), global = T)
Tabla7.1Grafico_Continental(tabla = tabla[[2]], v = 3, limites = c(-900,900), titulo = titulo, nombre = paste(nombre, "_", sep = ""),  salida = "/Salidas/TP4/")


# 2070 - 2099
load("RDatas/TP4.RDatas/pp6.26_99.RData"); load("RDatas/TP4.RDatas/evap6.26_99.RData") 

pp = pp6.26_99[[1]]  # prueba...(ram)
evap = evap6.26_99[[1]]

tabla =Tabla7.1(pp = pp, evap = evap, nombre = "6.26_99", salida = "/Salidas/TP4/tablas/")

titulo = "P - E CNRM-CM6 SSP126 -  2070 - 2099"
nombre = "pe6.26_99"
Tabla7.1Grafico(tabla = tabla[[1]], v = 3,limites = c(-900, 900), titulo = titulo, nombre = nombre, salida = "/Salidas/TP4/"
                , escala2 = T, limites2 = c(-60, 60), global = T)
Tabla7.1Grafico_Continental(tabla = tabla[[2]], v = 3, limites = c(-900,900), titulo = titulo, nombre = paste(nombre, "_", sep = ""),  salida = "/Salidas/TP4/")

# SSP585
# 2020 - 2049
load("RDatas/TP4.RDatas/pp6.85_49.RData"); load("RDatas/TP4.RDatas/evap6.85_49.RData") 

pp = pp6.85_49[[1]]  # prueba...(ram)
evap = evap6.85_49[[1]]

tabla=Tabla7.1(pp = pp, evap = evap, nombre = "6.85_49", salida = "/Salidas/TP4/tablas/")

titulo = "P - E CNRM-CM6 SSP585 -  2020 - 2049"
nombre = "pe6.85_49"
Tabla7.1Grafico(tabla = tabla[[1]], v = 3,limites = c(-900, 900), titulo = titulo, nombre = nombre, salida = "/Salidas/TP4/"
                , escala2 = T, limites2 = c(-60, 60), global = T)
Tabla7.1Grafico_Continental(tabla = tabla[[2]], v = 3, limites = c(-900,900), titulo = titulo, nombre = paste(nombre, "_", sep = ""),  salida = "/Salidas/TP4/")

# 2070 - 2099
load("RDatas/TP4.RDatas/pp6.85_99.RData"); load("RDatas/TP4.RDatas/evap6.85_99.RData") 

pp = pp6.85_99[[1]]  # prueba...(ram)
evap = evap6.85_99[[1]]

tabla = Tabla7.1(pp = pp, evap = evap, nombre = "6.85_99", salida = "/Salidas/TP4/tablas/")

titulo = "P - E CNRM-CM6 SSP585 -  2070 - 2099"
nombre = "pe6.85_99"
Tabla7.1Grafico(tabla = tabla[[1]], v = 3,limites = c(-900, 900), titulo = titulo, nombre = nombre, salida = "/Salidas/TP4/"
                , escala2 = T, limites2 = c(-60, 60), global = T)
Tabla7.1Grafico_Continental(tabla = tabla[[2]], v = 3, limites = c(-900,900), titulo = titulo, nombre = paste(nombre, "_", sep = ""),  salida = "/Salidas/TP4/")

