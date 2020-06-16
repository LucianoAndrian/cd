# TP4 Balance de Masa ( E y P)
# trabjar con RDatas guardados en TP4_apertura_y_seleccion.R
source("FUNCIONES.R")

# para pesar los promedios por la latitud
lat = read.table("lat.txt")
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30))

lats[,73,] = lats[,1,] = 0

lon = read.table("lon.txt")

#### tendencia de P y E  y promedio zonal####
mask = as.matrix(read.table("mask.txt"))
mask2 = mask; mask2[which(is.na(mask2))] = 2; mask2[which(mask2==1)] = NA; mask2[which(mask2==2)] = 1
mask = array(mask, c(144,73,30)); mask2  = array(mask2, c(144,73,30))
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30,9))
#---- CNRM-CM5 ----#
#---- HÇISTORICAL ----#
load("RDatas/TP4.RDatas/pp5.his.RData"); load("RDatas/TP4.RDatas/evap5.his.RData")
load("RDatas/TP4.RDatas/t5.his.RData")#;  load("RDatas/TP4.RDatas/hu5.his.RData")
pp = pp5.his[[1]]  # prueba...(ram)
evap = evap5.his[[1]]
temp = t5.his[[1]]
# # hu = hu5.his[[1]]
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30,length(pp[1,1,1,])))
PlotLat(pp = pp*lats, evap = evap*lats, titulo = "dif", y.breaks = seq(0, 3000, by = 500), nombre = "dif5.his", salida = "/Salidas/TP4/Lat/")

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)

pp.t.f = Tendencia(pp_ens, s = 0.9)
evap.t.f = Tendencia(evap_ens, s = 0.9)

auxx = pp.t.f; titulo = "Tendencia de PP - CNRM-CM5 Historico"; nombre.fig = "p5.his.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "Spectral", niveles = 11, revert = F, escala = seq(-10,10, by = 1)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


auxx = evap.t.f; titulo = "Tendencia de E - CNRM-CM5 Historico"; nombre.fig = "e5.his.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "PuOr", niveles = 9, revert = T, escala = seq(-7,7, by = 1)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


# p-e
auxx = apply(pp_ens - evap_ens, c(1,2), mean, na.rm = T) ; titulo = "P - E  CNRM-CM5 Historico"; nombre.fig = "pe5.his.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlBu", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
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
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30,length(pp[1,1,1,])))
PlotLat(pp = pp*lats, evap = evap*lats, titulo = "CNRM-CM5 RCP2.6  2020 - 2049", y.breaks = seq(0, 3000, by = 500), nombre = "5.26_49", salida = "/Salidas/TP4/Lat/")

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)

pp.t.f = Tendencia(pp_ens, s = 0.9)
evap.t.f = Tendencia(evap_ens, s = 0.9)

auxx = pp.t.f; titulo = "Tendencia de PP - CNRM-CM5 RCP2.6  2020 - 2049 "; nombre.fig = "p5.26_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "Spectral", niveles = 11, revert = F, escala = seq(-10,10, by = 1)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


auxx = evap.t.f; titulo = "Tendencia de E - CNRM-CM5 RCP2.6  2020 - 2049"; nombre.fig = "e5.26_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "PuOr", niveles = 9, revert = T, escala = seq(-7,7, by = 1)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


# p-e
auxx = apply(pp_ens - evap_ens, c(1,2), mean, na.rm = T) ; titulo = "P - E  CNRM-CM5 RCP2.6 2020 - 2049"; nombre.fig = "pe5.26_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlBu", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
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
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30,length(pp[1,1,1,])))
PlotLat(pp = pp*lats, evap = evap*lats, titulo = "CNRM-CM5 RCP2.6  2070 - 2099", y.breaks = seq(0, 3000, by = 500), nombre = "5.26_99", salida = "/Salidas/TP4/Lat/")

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)

pp.t.f = Tendencia(pp_ens, s = 0.9)
evap.t.f = Tendencia(evap_ens, s = 0.9)

auxx = pp.t.f; titulo = "Tendencia de PP - CNRM-CM5 RCP2.6  2070 - 2099 "; nombre.fig = "p5.26_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "Spectral", niveles = 11, revert = F, escala = seq(-10,10, by = 1)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


auxx = evap.t.f; titulo = "Tendencia de E - CNRM-CM5 RCP2.6  2070 - 2099"; nombre.fig = "e5.26_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "PuOr", niveles = 9, revert = T, escala = seq(-7,7, by = 1)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


# p-e
auxx = apply(pp_ens - evap_ens, c(1,2), mean, na.rm = T) ; titulo = "P - E  CNRM-CM5 RCP2.6 2070 - 2099"; nombre.fig = "pe5.26_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlBu", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
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
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30,length(pp[1,1,1,])))
PlotLat(pp = pp*lats, evap = evap*lats, titulo = "CNRM-CM5 RCP8.5  2020 - 2049", y.breaks = seq(0, 3000, by = 500), nombre = "5.85_49", salida = "/Salidas/TP4/Lat/")

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)

pp.t.f = Tendencia(pp_ens, s = 0.9)
evap.t.f = Tendencia(evap_ens, s = 0.9)

auxx = pp.t.f; titulo = "Tendencia de PP - CNRM-CM5 RCP8.5  2020 - 2049 "; nombre.fig = "p5.85_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "Spectral", niveles = 11, revert = F, escala = seq(-10,10, by = 1)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


auxx = evap.t.f; titulo = "Tendencia de E - CNRM-CM5 RCP8.5  2020 - 2049"; nombre.fig = "e5.85_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "PuOr", niveles = 9, revert = T, escala = seq(-7,7, by = 1)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


# p-e
auxx = apply(pp_ens - evap_ens, c(1,2), mean, na.rm = T) ; titulo = "P - E  CNRM-CM5 RCP8.5 2020 - 2049"; nombre.fig = "pe5.85_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlBu", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
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
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30,length(pp[1,1,1,])))
PlotLat(pp = pp*lats, evap = evap*lats, titulo = "CNRM-CM5 RCP8.5  2070 - 2099", y.breaks = seq(0, 3000, by = 500), nombre = "5.85_99", salida = "/Salidas/TP4/Lat/")

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)

pp.t.f = Tendencia(pp_ens, s = 0.9)
evap.t.f = Tendencia(evap_ens, s = 0.9)

auxx = pp.t.f; titulo = "Tendencia de PP - CNRM-CM5 RCP8.5  2070 - 2099 "; nombre.fig = "p5.85_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "Spectral", niveles = 11, revert = F, escala = seq(-10,10, by = 1)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


auxx = evap.t.f; titulo = "Tendencia de E - CNRM-CM5 RCP8.5  2070 - 2099"; nombre.fig = "e5.85_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "PuOr", niveles = 9, revert = T, escala = seq(-7,7, by = 1)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


# p-e
auxx = apply(pp_ens - evap_ens, c(1,2), mean, na.rm = T) ; titulo = "P - E  CNRM-CM5 RCP8.5 2070 - 2099"; nombre.fig = "pe5.85_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlBu", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
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
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30,length(pp[1,1,1,])))
PlotLat(pp = pp*lats, evap = evap*lats, titulo = "CNRM-CM6 - Historico", y.breaks = seq(0, 3000, by = 500), nombre = "6.his", salida = "/Salidas/TP4/Lat/")

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)

pp.t.f = Tendencia(pp_ens, s = 0.9)
evap.t.f = Tendencia(evap_ens, s = 0.9)

auxx = pp.t.f; titulo = "Tendencia de PP - CNRM-CM6 Historico"; nombre.fig = "p6.his.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "Spectral", niveles = 11, revert = F, escala = seq(-10,10, by = 1)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


auxx = evap.t.f; titulo = "Tendencia de E - CNRM-CM6 Historico"; nombre.fig = "e6.his.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "PuOr", niveles = 9, revert = T, escala = seq(-7,7, by = 1)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


# p-e
auxx = apply(pp_ens - evap_ens, c(1,2), mean, na.rm = T) ; titulo = "P - E  CNRM-CM6 Historico"; nombre.fig = "pe6.his.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlBu", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
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
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30,length(pp[1,1,1,])))
PlotLat(pp = pp*lats, evap = evap*lats, titulo = "CNRM-CM6 SSP126  2020 - 2049", y.breaks = seq(0, 3000, by = 500), nombre = "6.26_49", salida = "/Salidas/TP4/Lat/")

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)


pp.t.f = Tendencia(pp_ens, s = 0.9)
evap.t.f = Tendencia(evap_ens, s = 0.9)

auxx = pp.t.f; titulo = "Tendencia de PP - CNRM-CM6 SSP126  2020 - 2049 "; nombre.fig = "p6.26_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "Spectral", niveles = 11, revert = F, escala = seq(-10,10, by = 1)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


auxx = evap.t.f; titulo = "Tendencia de E - CNRM-CM6 SSP126  2020 - 2049"; nombre.fig = "e6.26_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "PuOr", niveles = 9, revert = T, escala = seq(-7,7, by = 1)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


# p-e
auxx = apply(pp_ens - evap_ens, c(1,2), mean, na.rm = T) ; titulo = "P - E  CNRM-CM6 SSP126 2020 - 2049"; nombre.fig = "pe6.26_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlBu", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
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
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30,length(pp[1,1,1,])))
PlotLat(pp = pp*lats, evap = evap*lats, titulo = "CNRM-CM6 SSP126  2070 - 2099", y.breaks = seq(0, 3000, by = 500), nombre = "6.26_99", salida = "/Salidas/TP4/Lat/")

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)

pp.t.f = Tendencia(pp_ens, s = 0.9)
evap.t.f = Tendencia(evap_ens, s = 0.9)

auxx = pp.t.f; titulo = "Tendencia de PP - CNRM-CM6 SSP126  2070 - 2099 "; nombre.fig = "p6.26_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "Spectral", niveles = 11, revert = F, escala = seq(-10,10, by = 1)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


auxx = evap.t.f; titulo = "Tendencia de E - CNRM-CM6 SSP126  2070 - 2099"; nombre.fig = "e6.26_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "PuOr", niveles = 9, revert = T, escala = seq(-7,7, by = 1)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


# p-e
auxx = apply(pp_ens - evap_ens, c(1,2), mean, na.rm = T) ; titulo = "P - E  CNRM-CM6 SSP126 2070 - 2099"; nombre.fig = "pe6.26_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlBu", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
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
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30,length(pp[1,1,1,])))
PlotLat(pp = pp*lats, evap = evap*lats, titulo = "CNRM-CM6 SSP585  2020 - 2049", y.breaks = seq(0, 3000, by = 500), nombre = "6.85_49", salida = "/Salidas/TP4/Lat/")

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)

pp.t.f = Tendencia(pp_ens, s = 0.9)
evap.t.f = Tendencia(evap_ens, s = 0.9)

auxx = pp.t.f; titulo = "Tendencia de PP - CNRM-CM6 SSP585  2020 - 2049 "; nombre.fig = "p6.85_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "Spectral", niveles = 11, revert = F, escala = seq(-10,10, by = 1)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


auxx = evap.t.f; titulo = "Tendencia de E - CNRM-CM6 SSP585  2020 - 2049"; nombre.fig = "e6.85_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "PuOr", niveles = 9, revert = T, escala = seq(-7,7, by = 1)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


# p-e
auxx = apply(pp_ens - evap_ens, c(1,2), mean, na.rm = T) ; titulo = "P - E  CNRM-CM6 SSP585 2020 - 2049"; nombre.fig = "pe6.85_49.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlBu", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
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
lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30,length(pp[1,1,1,])))
PlotLat(pp = pp*lats, evap = evap*lats, titulo = "CNRM-CM6 SSP585  2070 - 2099", y.breaks = seq(0, 3000, by = 500), nombre = "6.85_99", salida = "/Salidas/TP4/Lat/")

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)


pp.t.f = Tendencia(pp_ens, s = 0.9)
evap.t.f = Tendencia(evap_ens, s = 0.9)

auxx = pp.t.f; titulo = "Tendencia de PP - CNRM-CM6 SSP585  2070 - 2099 "; nombre.fig = "p6.85_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat),colorbar = "Spectral", niveles = 11, revert = F, escala = seq(-10,10, by = 1)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


auxx = evap.t.f; titulo = "Tendencia de E - CNRM-CM6 SSP585  2070 - 2099"; nombre.fig = "e6.85_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx[[1]], c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "PuOr", niveles = 9, revert = T, escala = seq(-7,7, by = 1)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")


# p-e
auxx = apply(pp_ens - evap_ens, c(1,2), mean, na.rm = T) ; titulo = "P - E  CNRM-CM6 SSP585 2070 - 2099"; nombre.fig = "pe6.85_99.f_tend" 
mask = as.matrix(read.table("mask.txt"))
aux = array(auxx, c(144,73,1)); mask = array(auxx[[2]], c(144,73,1))
mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
mapa_topo3(variable = aux, variable.sig = mask, lon = as.matrix(lon), lat = as.matrix(lat), colorbar = "RdYlBu", niveles = 11, revert = F, escala = seq(-2000,2000, by = 250)
           , color.vsig = "black", alpha.vsig = 0.2, sig = F, mapa = "mundo", x.label = NULL, y.label = NULL, label.escala = "mm", na.fill = -1000
           , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP4/Tend/", colorbar.pos = "bottom")




rm(list = ls())
source("FUNCIONES.R")


#### TABLA 7.1 ####


cm = c("5", "6")
rcp = c("26", "85")

source("FUNCIONES.R")

for( i in 1:2){
  for(j in 1:2){
    
    load(paste("RDatas/TP4.RDatas/", "pp", cm[i],".his.RData", sep = ""), envir = N <- new.env())  #### !!!
    load(paste("RDatas/TP4.RDatas/evap", cm[i],".his.RData", sep = ""), envir = M <- new.env() )   #### !!!
    pp = get(paste("pp", cm[i], ".his", sep = ""), envir =  N)[[1]]
    evap = get(paste("evap", cm[i], ".his", sep = ""), envir =  M)[[1]]
    
    nombre = paste(cm[i], ".his", sep = "")
    tabla = Tabla7.1(pp = pp, evap = evap, nombre = nombre, salida = "/Salidas/TP4/tablas/", r = T)
    
    
    
    load(paste("RDatas/TP4.RDatas/pp",cm[i],".",rcp[j], "_49.RData", sep = ""), envir = N <- new.env())
    load(paste("RDatas/TP4.RDatas/evap",cm[i],".",rcp[j], "_49.RData", sep = ""), envir = M <- new.env())
    
    pp = get(paste("pp", cm[i], ".", rcp[j], "_49", sep = ""), envir =  N)[[1]]
    evap = get(paste("evap", cm[i], ".", rcp[j], "_49", sep = ""), envir =  M)[[1]]
    
    nombre = paste(cm[i], ".", rcp[j], "_49", sep = "")
    tabla2 = Tabla7.1(pp = pp, evap = evap, nombre = nombre, salida = "/Salidas/TP4/tablas/", r = T)
    
    
    
    load(paste("RDatas/TP4.RDatas/pp",cm[i],".",rcp[j], "_99.RData", sep = ""), N <- new.env())
    load(paste("RDatas/TP4.RDatas/evap",cm[i],".",rcp[j], "_99.RData", sep = ""), M <- new.env())
    
    pp = get(paste("pp", cm[i], ".", rcp[j], "_99", sep = ""), envir =  N)[[1]]
    evap = get(paste("evap", cm[i], ".", rcp[j], "_99", sep = ""), envir =  M)[[1]]
    
    nombre = paste(cm[i], ".", rcp[j], "_99", sep = "")
    tabla3 = Tabla7.1(pp = pp, evap = evap, nombre = nombre, salida = "/Salidas/TP4/tablas/", r = T)
    
    
    
    
    titulo = paste("P - E  CNRM-CM", cm[i], " RCP", rcp[j] ,sep = "")
    nombre = paste(cm[i], "_", rcp[j], sep ="")
    
    Tabla7.1Grafico(data.his =  tabla[[1]], data.49 = tabla2[[1]], data.99 = tabla3[[1]], v = 3,limites = c(-900, 900), titulo = titulo, nombre = nombre, salida = "/Salidas/TP4/"
                    , escala2 = T, limites2 = c(-70, 70), global = T, width = 20, height = 15)
    
    Tabla7.1Grafico_Continental(data.his = tabla[[2]], data.49 = tabla2[[2]], data.99 = tabla3[[2]], v = 3, limites = c(-400,400), titulo = titulo, nombre = paste(nombre, "_", sep = ""),  salida = "/Salidas/TP4/"
                                , width = 20, height = 15)
    
    print(paste("RDatas/TP4.RDatas/pp",cm[i],".",rcp[j], "_99.RData", sep = ""))
    print(paste("P - E  CNRM-CM", cm[i], " RCP", rcp[j] ,sep = ""))
    
    rm(N, M)
  }
}



### TENDENCIA ####
#CNRM-CM5
#### RCP2.6 ####
load("RDatas/TP4.RDatas/pp5.his.RData"); load("RDatas/TP4.RDatas/evap5.his.RData")
load("RDatas/TP4.RDatas/t5.his.RData")#;  load("RDatas/TP4.RDatas/hu5.his.RData")
pp = pp5.his[[1]]  # prueba...(ram)
evap = evap5.his[[1]]
temp = t5.his[[1]]
# # hu = hu5.his[[1]]

##### revisar estooo#####
cm = c("5", "6")
rcp = c("26", "85")
rcp2 = c("RCP", "RCP", "SSP1", "SSP5")
source("FUNCIONES.R")
escala = list(); escala[[1]] = seq(1090, 1130, by = 10); escala[[2]] = seq(1050, 1100, by = 10)
escala2 = list(); escala2[[1]] = seq(1090, 1190, by = 10); escala2[[2]] = seq(1090, 1190, by = 10)

c1 = c(695, 640); c2 = c(1100, 1070)

for( i in 1:2){
  for(j in 1:2){
    
    load(paste("RDatas/TP4.RDatas/", "pp", cm[i],".his.RData", sep = ""), envir = N <- new.env())  #### !!!
    load(paste("RDatas/TP4.RDatas/evap", cm[i],".his.RData", sep = ""), envir = M <- new.env() )   #### !!!
    load(paste("RDatas/TP4.RDatas/t", cm[i],".his.RData", sep = ""), envir = O <- new.env() )   #### !!!
    pp = get(paste("pp", cm[i], ".his", sep = ""), envir =  N)[[1]]
    evap = get(paste("evap", cm[i], ".his", sep = ""), envir =  M)[[1]]
    t = get(paste("t", cm[i], ".his", sep = ""), envir =  O)[[1]]
    
    aux = CorecLat(pp = pp, e = evap, t = t)
   # PlotTs2(data = aux, titulo = paste("CNRM-CM",cm[i], "  1976-2005", sep = ""), nombre.fig = paste(cm[i], ".his", sep = ""),
          #  escala = escala[[i]], c1 = c1[j], c2 = c2[j])
    
    datos = as.data.frame(apply(aux[[1]], c(1), mean))
    datos = cbind(datos, apply(aux[[2]], c(1), mean))
    datos = cbind(datos, apply(aux[[3]], c(1), mean))
    colnames(datos) = c("p", "e", "t")
  
     
    load(paste("RDatas/TP4.RDatas/pp",cm[i],".",rcp[j], "_49.RData", sep = ""), envir = N <- new.env())
    load(paste("RDatas/TP4.RDatas/evap",cm[i],".",rcp[j], "_49.RData", sep = ""), envir = M <- new.env())
    load(paste("RDatas/TP4.RDatas/t",cm[i],".",rcp[j], "_49.RData", sep = ""), envir = O <- new.env())
    
    pp = get(paste("pp", cm[i], ".", rcp[j], "_49", sep = ""), envir =  N)[[1]]
    evap = get(paste("evap", cm[i], ".", rcp[j], "_49", sep = ""), envir =  M)[[1]]
    t = get(paste("t", cm[i], ".", rcp[j], "_49", sep = ""), envir =  O)[[1]]
    
    aux = CorecLat(pp = pp, e = evap, t = t)
    
    if(j == 2 & i == 1){
      datos2 = as.data.frame(apply(aux[[1]], c(1), mean))
      datos2 = cbind(datos2, apply(aux[[2]], c(1), mean))
      datos2 = cbind(datos2, apply(aux[[3]], c(1), mean))
      colnames(datos2) = c("p", "e", "t")
      datos = rbind(datos, NA, datos2)
    } else {
      datos2 = as.data.frame(aux[[1]])
      datos2 = cbind(datos2, aux[[2]])
      datos2 = cbind(datos2, aux[[3]])
      colnames(datos2) = c("p", "e", "t")
      datos = rbind(datos, NA, datos2)
    }
    
    
    
    load(paste("RDatas/TP4.RDatas/pp",cm[i],".",rcp[j], "_99.RData", sep = ""), N <- new.env())
    load(paste("RDatas/TP4.RDatas/evap",cm[i],".",rcp[j], "_99.RData", sep = ""), M <- new.env())
    load(paste("RDatas/TP4.RDatas/t",cm[i],".",rcp[j], "_99.RData", sep = ""), O <- new.env())
    
    pp = get(paste("pp", cm[i], ".", rcp[j], "_99", sep = ""), envir =  N)[[1]]
    evap = get(paste("evap", cm[i], ".", rcp[j], "_99", sep = ""), envir =  M)[[1]]
    t = get(paste("t", cm[i], ".", rcp[j], "_99", sep = ""), envir =  O)[[1]]
    
    aux = CorecLat(pp = pp, e = evap, t = t)
    
    
    if(j == 2){
      datos2 = as.data.frame(apply(aux[[1]], c(1), mean))
      datos2 = cbind(datos2, apply(aux[[2]], c(1), mean))
      datos2 = cbind(datos2, apply(aux[[3]], c(1), mean))
      colnames(datos2) = c("p", "e", "t")
      datos = rbind(datos, NA, datos2)
    } else {
      datos2 = as.data.frame(aux[[1]])
      datos2 = cbind(datos2, aux[[2]])
      datos2 = cbind(datos2, aux[[3]])
      colnames(datos2) = c("p", "e", "t")
      datos = rbind(datos, NA, datos2)
    }
    
    
    aux = seq(1976, 2005); aux1 = seq(2020,2049); aux2 = seq(2070, 2099)
    años = c(aux, 2010 , aux1, 2060 , aux2)
    
    datos = cbind(datos, años)
    


    aux1 =  mean(datos[32:61,3]) - mean(datos[1:30,3])
    aux2 = mean(datos[63:92,3]) - mean(datos[1:30,3])
    
    datos[1:30,3] = datos[1:30,3] - mean(datos[1:30,3])
    datos[32:61,3] = datos[32:61,3] - mean(datos[32:61,3]) + aux1
    datos[63:92,3] = datos[63:92,3] - mean(datos[63:92,3]) + aux2
    
    PlotTs(datos = datos, escala = escala2[[i]], escala2 = seq(-1, 1, by = 0.2), titulo = paste("CNRM-CM", cm[i], " ", rcp2[i+j],rcp[j], sep = "")
           , nombre = paste(cm[i], ".", rcp[j], "_49", sep = ""), c = 700)
    
    
  }
}

############
aux = CorecLat(pp = pp, e = evap, t = temp)

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)


pp.ts = apply(pp_ens*lats, c(3), mean, na.rm = T)
evap.ts = apply(evap_ens*lats, c(3), mean, na.rm = T)
temp.ts = apply(temp_ens, c(3), mean, na.rm = T)
# hu.ts = apply(# hu_ens*lats, c(3), mean, na.rm = T)

pp = Tendencia.ts(pp.ts)
evap = Tendencia.ts(evap.ts)
temp = Tendencia.ts(temp.ts)
# hu = Tendencia.ts(# hu.ts)

datos = as.data.frame(pp[[1]])
#datos = cbind(datos, Años = seq(1976, 2005, by = 1))
datos = cbind(datos, as.data.frame(evap[[1]]))
datos = cbind(datos, as.data.frame(temp[[1]]))


load("RDatas/TP4.RDatas/pp5.26_49.RData"); load("RDatas/TP4.RDatas/evap5.26_49.RData")
load("RDatas/TP4.RDatas/t5.26_49.RData")#;  load("RDatas/TP4.RDatas/hu5.26_49.RData")
pp = pp5.26_49[[1]]  # prueba...(ram)
evap = evap5.26_49[[1]]
temp = t5.26_49[[1]]


pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)


pp.ts = apply(pp_ens*lats, c(3), mean, na.rm = T)
evap.ts = apply(evap_ens*lats, c(3), mean, na.rm = T)
temp.ts = apply(temp_ens, c(3), mean, na.rm = T)
# hu.ts = apply(# hu_ens*lats, c(3), mean, na.rm = T)

pp = Tendencia.ts(pp.ts)
evap = Tendencia.ts(evap.ts)
temp = Tendencia.ts(temp.ts)
# hu = Tendencia.ts(# hu.ts)

datos2 = as.data.frame(pp[[1]])
#datos = cbind(datos, Años = seq(1976, 2005, by = 1))
datos2 = cbind(datos2, as.data.frame(evap[[1]]))
datos2 = cbind(datos2, as.data.frame(temp[[1]]))
datos = rbind(datos, NA, datos2)

load("RDatas/TP4.RDatas/pp5.26_99.RData"); load("RDatas/TP4.RDatas/evap5.26_99.RData")
load("RDatas/TP4.RDatas/t5.26_99.RData")#;  load("RDatas/TP4.RDatas/hu5.26_99.RData")
pp = pp5.26_99[[1]]  # prueba...(ram)
evap = evap5.26_99[[1]]
temp = t5.26_99[[1]]
# hu = hu5.26_99[[1]]

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)


pp.ts = apply(pp_ens*lats, c(3), mean, na.rm = T)
evap.ts = apply(evap_ens*lats, c(3), mean, na.rm = T)
temp.ts = apply(temp_ens, c(3), mean, na.rm = T)
# hu.ts = apply(# hu_ens*lats, c(3), mean, na.rm = T)

pp = Tendencia.ts(pp.ts)
evap = Tendencia.ts(evap.ts)
temp = Tendencia.ts(temp.ts)
# hu = Tendencia.ts(# hu.ts)

datos2 = as.data.frame(pp[[1]])
#datos2 = cbind(datos2, Años = seq(2070, 2099, by = 1))
datos2 = cbind(datos2, as.data.frame(evap[[1]]))
datos2 = cbind(datos2, as.data.frame(temp[[1]]))

datos = rbind(datos, NA, datos2)

aux = seq(1976, 2005); aux1 = seq(2020,2049); aux2 = seq(2070, 2099)
años = c(aux, 2010 , aux1, 2060 , aux2)

datos = cbind(datos, años)

aux = datos


datos = aux
aux1 =  mean(datos[32:61,5]) - mean(datos[1:30,5])
aux2 = mean(datos[63:92,5]) - mean(datos[1:30,5])

datos[1:30,5] = datos[1:30,5] - mean(datos[1:30,5])
datos[32:61,5] = datos[32:61,5] - mean(datos[32:61,5]) + aux1
datos[63:92,5] = datos[63:92,5] - mean(datos[63:92,5]) + aux2

PlotTs(datos = datos, escala = seq(690, 740, by = 10), escala2 = seq(-1, 1, by = 0.2), titulo = "CNRM-CM5 RCP2.6", nombre = "cm5.26", c = 700)


#### RCP8.5 ####
load("RDatas/TP4.RDatas/pp5.his.RData"); load("RDatas/TP4.RDatas/evap5.his.RData")
load("RDatas/TP4.RDatas/t5.his.RData")#;  load("RDatas/TP4.RDatas/hu5.his.RData")
pp = pp5.his[[1]]  # prueba...(ram)
evap = evap5.his[[1]]
temp = t5.his[[1]]
# # hu = hu5.his[[1]]


pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)


pp.ts = apply(pp_ens*lats, c(3), mean, na.rm = T)
evap.ts = apply(evap_ens*lats, c(3), mean, na.rm = T)
temp.ts = apply(temp_ens, c(3), mean, na.rm = T)
# hu.ts = apply(# hu_ens*lats, c(3), mean, na.rm = T)

pp = Tendencia.ts(pp.ts)
evap = Tendencia.ts(evap.ts)
temp = Tendencia.ts(temp.ts)
# hu = Tendencia.ts(# hu.ts)

datos = as.data.frame(pp[[1]])
#datos = cbind(datos, Años = seq(1976, 2005, by = 1))
datos = cbind(datos, as.data.frame(evap[[1]]))
datos = cbind(datos, as.data.frame(temp[[1]]))


load("RDatas/TP4.RDatas/pp5.85_49.RData"); load("RDatas/TP4.RDatas/evap5.85_49.RData")
load("RDatas/TP4.RDatas/t5.85_49.RData")#;  load("RDatas/TP4.RDatas/hu5.85_49.RData")
pp = pp5.85_49[[1]]  # prueba...(ram)
evap = evap5.85_49[[1]]
temp = t5.85_49[[1]]


pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)


pp.ts = apply(pp_ens*lats, c(3), mean, na.rm = T)
evap.ts = apply(evap_ens*lats, c(3), mean, na.rm = T)
temp.ts = apply(temp_ens, c(3), mean, na.rm = T)
# hu.ts = apply(# hu_ens*lats, c(3), mean, na.rm = T)

pp = Tendencia.ts(pp.ts)
evap = Tendencia.ts(evap.ts)
temp = Tendencia.ts(temp.ts)
# hu = Tendencia.ts(# hu.ts)

datos2 = as.data.frame(pp[[1]])
#datos = cbind(datos, Años = seq(1976, 2005, by = 1))
datos2 = cbind(datos2, as.data.frame(evap[[1]]))
datos2 = cbind(datos2, as.data.frame(temp[[1]]))
datos = rbind(datos, NA, datos2)

load("RDatas/TP4.RDatas/pp5.85_99.RData"); load("RDatas/TP4.RDatas/evap5.85_99.RData")
load("RDatas/TP4.RDatas/t5.85_99.RData")#;  load("RDatas/TP4.RDatas/hu5.85_99.RData")
pp = pp5.85_99[[1]]  # prueba...(ram)
evap = evap5.85_99[[1]]
temp = t5.85_99[[1]]
# hu = hu5.85_99[[1]]

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)


pp.ts = apply(pp_ens*lats, c(3), mean, na.rm = T)
evap.ts = apply(evap_ens*lats, c(3), mean, na.rm = T)
temp.ts = apply(temp_ens, c(3), mean, na.rm = T)
# hu.ts = apply(# hu_ens*lats, c(3), mean, na.rm = T)

pp = Tendencia.ts(pp.ts)
evap = Tendencia.ts(evap.ts)
temp = Tendencia.ts(temp.ts)
# hu = Tendencia.ts(# hu.ts)

datos2 = as.data.frame(pp[[1]])
#datos2 = cbind(datos2, Años = seq(2070, 2099, by = 1))
datos2 = cbind(datos2, as.data.frame(evap[[1]]))
datos2 = cbind(datos2, as.data.frame(temp[[1]]))

datos = rbind(datos, NA, datos2)

aux = seq(1976, 2005); aux1 = seq(2020,2049); aux2 = seq(2070, 2099)
años = c(aux, 2010 , aux1, 2060 , aux2)

datos = cbind(datos, años)

aux = datos


datos = aux
aux1 =  mean(datos[32:61,5]) - mean(datos[1:30,5])
aux2 = mean(datos[63:92,5]) - mean(datos[1:30,5])

datos[1:30,5] = datos[1:30,5] - mean(datos[1:30,5])
datos[32:61,5] = datos[32:61,5] - mean(datos[32:61,5]) + aux1
datos[63:92,5] = datos[63:92,5] - mean(datos[63:92,5]) + aux2

PlotTs(datos = datos, escala = seq(690, 760, by = 10), escala2 = seq(-1, 1, by = 0.2), titulo = "CNRM-CM5 RCP8.5", nombre = "cm5.85", c = 700)


#CNRM-CM6
####SSP126 ####
load("RDatas/TP4.RDatas/pp6.his.RData"); load("RDatas/TP4.RDatas/evap6.his.RData")
load("RDatas/TP4.RDatas/t6.his.RData")#;  load("RDatas/TP4.RDatas/hu5.his.RData")
pp = pp6.his[[1]]  # prueba...(ram)
evap = evap6.his[[1]]
temp = t6.his[[1]]
# # hu = hu5.his[[1]]


pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)


pp.ts = apply(pp_ens*lats, c(3), mean, na.rm = T)
evap.ts = apply(evap_ens*lats, c(3), mean, na.rm = T)
temp.ts = apply(temp_ens, c(3), mean, na.rm = T)
# hu.ts = apply(# hu_ens*lats, c(3), mean, na.rm = T)

pp = Tendencia.ts(pp.ts)
evap = Tendencia.ts(evap.ts)
temp = Tendencia.ts(temp.ts)
# hu = Tendencia.ts(# hu.ts)

datos = as.data.frame(pp[[1]])
#datos = cbind(datos, Años = seq(1976, 2005, by = 1))
datos = cbind(datos, as.data.frame(evap[[1]]))
datos = cbind(datos, as.data.frame(temp[[1]]))


load("RDatas/TP4.RDatas/pp6.26_49.RData"); load("RDatas/TP4.RDatas/evap6.26_49.RData")
load("RDatas/TP4.RDatas/t6.26_49.RData")#;  load("RDatas/TP4.RDatas/hu5.26_49.RData")
pp = pp6.26_49[[1]]  # prueba...(ram)
evap = evap6.26_49[[1]]
temp = t6.26_49[[1]]


pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)


pp.ts = apply(pp_ens*lats, c(3), mean, na.rm = T)
evap.ts = apply(evap_ens*lats, c(3), mean, na.rm = T)
temp.ts = apply(temp_ens, c(3), mean, na.rm = T)
# hu.ts = apply(# hu_ens*lats, c(3), mean, na.rm = T)

pp = Tendencia.ts(pp.ts)
evap = Tendencia.ts(evap.ts)
temp = Tendencia.ts(temp.ts)
# hu = Tendencia.ts(# hu.ts)

datos2 = as.data.frame(pp[[1]])
#datos = cbind(datos, Años = seq(1976, 2005, by = 1))
datos2 = cbind(datos2, as.data.frame(evap[[1]]))
datos2 = cbind(datos2, as.data.frame(temp[[1]]))
datos = rbind(datos, NA, datos2)

load("RDatas/TP4.RDatas/pp6.26_99.RData"); load("RDatas/TP4.RDatas/evap6.26_99.RData")
load("RDatas/TP4.RDatas/t6.26_99.RData")#;  load("RDatas/TP4.RDatas/hu5.26_99.RData")
pp = pp6.26_99[[1]]  # prueba...(ram)
evap = evap6.26_99[[1]]
temp = t6.26_99[[1]]
# hu = hu5.26_99[[1]]

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)


pp.ts = apply(pp_ens*lats, c(3), mean, na.rm = T)
evap.ts = apply(evap_ens*lats, c(3), mean, na.rm = T)
temp.ts = apply(temp_ens, c(3), mean, na.rm = T)
# hu.ts = apply(# hu_ens*lats, c(3), mean, na.rm = T)

pp = Tendencia.ts(pp.ts)
evap = Tendencia.ts(evap.ts)
temp = Tendencia.ts(temp.ts)
# hu = Tendencia.ts(# hu.ts)

datos2 = as.data.frame(pp[[1]])
#datos2 = cbind(datos2, Años = seq(2070, 2099, by = 1))
datos2 = cbind(datos2, as.data.frame(evap[[1]]))
datos2 = cbind(datos2, as.data.frame(temp[[1]]))

datos = rbind(datos, NA, datos2)

aux = seq(1976, 2005); aux1 = seq(2020,2049); aux2 = seq(2070, 2099)
años = c(aux, 2010 , aux1, 2060 , aux2)

datos = cbind(datos, años)

aux = datos


datos = aux
aux1 =  mean(datos[32:61,5]) - mean(datos[1:30,5])
aux2 = mean(datos[63:92,5]) - mean(datos[1:30,5])

datos[1:30,5] = datos[1:30,5] - mean(datos[1:30,5])
datos[32:61,5] = datos[32:61,5] - mean(datos[32:61,5]) + aux1
datos[63:92,5] = datos[63:92,5] - mean(datos[63:92,5]) + aux2

PlotTs(datos = datos, escala = seq(650, 725, by = 15), escala2 = seq(-1, 1, by = 0.2), titulo = "CNRM-CM6 SSP126", nombre = "CM6.26", c = 665)


#### SSP585 ####
load("RDatas/TP4.RDatas/pp6.his.RData"); load("RDatas/TP4.RDatas/evap6.his.RData")
load("RDatas/TP4.RDatas/t6.his.RData")#;  load("RDatas/TP4.RDatas/hu5.his.RData")
pp = pp6.his[[1]]  # prueba...(ram)
evap = evap6.his[[1]]
temp = t6.his[[1]]
# # hu = hu5.his[[1]]


pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)


pp.ts = apply(pp_ens*lats, c(3), mean, na.rm = T)
evap.ts = apply(evap_ens*lats, c(3), mean, na.rm = T)
temp.ts = apply(temp_ens, c(3), mean, na.rm = T)
# hu.ts = apply(# hu_ens*lats, c(3), mean, na.rm = T)

pp = Tendencia.ts(pp.ts)
evap = Tendencia.ts(evap.ts)
temp = Tendencia.ts(temp.ts)
# hu = Tendencia.ts(# hu.ts)

datos = as.data.frame(pp[[1]])
#datos = cbind(datos, Años = seq(1976, 2005, by = 1))
datos = cbind(datos, as.data.frame(evap[[1]]))
datos = cbind(datos, as.data.frame(temp[[1]]))


load("RDatas/TP4.RDatas/pp6.85_49.RData"); load("RDatas/TP4.RDatas/evap6.85_49.RData")
load("RDatas/TP4.RDatas/t6.85_49.RData")#;  load("RDatas/TP4.RDatas/hu5.85_49.RData")
pp = pp6.85_49[[1]]  # prueba...(ram)
evap = evap6.85_49[[1]]
temp = t6.85_49[[1]]


pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)


pp.ts = apply(pp_ens*lats, c(3), mean, na.rm = T)
evap.ts = apply(evap_ens*lats, c(3), mean, na.rm = T)
temp.ts = apply(temp_ens, c(3), mean, na.rm = T)
# hu.ts = apply(# hu_ens*lats, c(3), mean, na.rm = T)

pp = Tendencia.ts(pp.ts)
evap = Tendencia.ts(evap.ts)
temp = Tendencia.ts(temp.ts)
# hu = Tendencia.ts(# hu.ts)

datos2 = as.data.frame(pp[[1]])
#datos = cbind(datos, Años = seq(1976, 2005, by = 1))
datos2 = cbind(datos2, as.data.frame(evap[[1]]))
datos2 = cbind(datos2, as.data.frame(temp[[1]]))
datos = rbind(datos, NA, datos2)

load("RDatas/TP4.RDatas/pp6.85_99.RData"); load("RDatas/TP4.RDatas/evap6.85_99.RData")
load("RDatas/TP4.RDatas/t6.85_99.RData")#;  load("RDatas/TP4.RDatas/hu5.85_99.RData")
pp = pp6.85_99[[1]]  # prueba...(ram)
evap = evap6.85_99[[1]]
temp = t6.85_99[[1]]
# hu = hu5.85_99[[1]]

pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   # este paso puede q este al dope... 
evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
temp_ens = apply(temp, c(1,2,3), mean, na.rm = T)
# hu_ens = apply(hu, c(1,2,3), mean, na.rm = T)


pp.ts = apply(pp_ens*lats, c(3), mean, na.rm = T)
evap.ts = apply(evap_ens*lats, c(3), mean, na.rm = T)
temp.ts = apply(temp_ens, c(3), mean, na.rm = T)
# hu.ts = apply(# hu_ens*lats, c(3), mean, na.rm = T)

pp = Tendencia.ts(pp.ts)
evap = Tendencia.ts(evap.ts)
temp = Tendencia.ts(temp.ts)
# hu = Tendencia.ts(# hu.ts)

datos2 = as.data.frame(pp[[1]])
#datos2 = cbind(datos2, Años = seq(2070, 2099, by = 1))
datos2 = cbind(datos2, as.data.frame(evap[[1]]))
datos2 = cbind(datos2, as.data.frame(temp[[1]]))

datos = rbind(datos, NA, datos2)

aux = seq(1976, 2005); aux1 = seq(2020,2049); aux2 = seq(2070, 2099)
años = c(aux, 2010 , aux1, 2060 , aux2)

datos = cbind(datos, años)

aux = datos


datos = aux
aux1 =  mean(datos[32:61,5]) - mean(datos[1:30,5])
aux2 = mean(datos[63:92,5]) - mean(datos[1:30,5])

datos[1:30,5] = datos[1:30,5] - mean(datos[1:30,5])
datos[32:61,5] = datos[32:61,5] - mean(datos[32:61,5]) + aux1
datos[63:92,5] = datos[63:92,5] - mean(datos[63:92,5]) + aux2

PlotTs(datos = datos, escala = seq(650, 725, by = 15), escala2 = seq(-1, 1, by = 0.2), titulo = "CNRM-CM6 SSP585", nombre = "CM6.85", c = 665)



