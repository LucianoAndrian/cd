#### GRAFICOS ####
source("Tp1.R")

# CNRM5
aux = array(data = calc_means[[1]], c(144,73,1)) 
mapa(lista = aux, titulo = "Temperatura media ensamble CNRM-CM5 1975 - 2005", nombre_fig = "tas5.ens.mean"
     , escala = c(-30, 30), label_escala = "ºC", resta = 273, brewer = "RdBu", revert = "si", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(-30, 30, by = 5), breaks_c_f = seq(-30, 30, by = 2.5), r = 1,na_fill = -10000, salida = "/Salidas/TP1/")

# r.s
mapa(lista = calc_means[[2]], titulo = "Temperatura media CNRM-CM5 1975 - 2005", nombre_fig = "tas5.mean.r"
     , escala = c(-30, 30), label_escala = "ºC", resta = 273, brewer = "RdBu", revert = "si", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(-30, 30, by = 5), breaks_c_f = seq(-30, 30, by = 2.5), r = 9, salida = "/Salidas/TP1/")

# sd
aux = array(data = sd_s[[1]], c(144,73,1)) 
mapa(lista = aux, titulo = "Temperatura desvio ensamble CNRM-CM5 1975 - 2005", nombre_fig = "sd_tas5.ens.mean"
     , escala = c(0, 1), label_escala = "", resta = 0, brewer = "YlOrRd", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 1, by = 0.1), breaks_c_f = seq(0, 1, by = 0.1), r = 1, salida = "/Salidas/TP1/")

# sd emtre miembros de ensambles T
#aux = array(data = sd_s[[2]], c(144,73,1)) 
#mapa(lista = aux, titulo = "Temperatura desvio ensamble CNRM-CM5 1975 - 2005", nombre_fig = "prueba"
#     , escala = c(0, 1), label_escala = "", resta = 0, brewer = "YlOrRd", revert = "no", niveles = 9
#     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 1, by = 0.1), breaks_c_f = seq(0, 1, by = 0.1), r = 1, salida = "/Salidas/TP1/")

# bias
aux = array(data = bias5_t_ens, c(144,73,1)) 
aux[which(abs(aux)>7)] = NA
mapa(lista = aux, titulo = "Temperatura Bias ensamble CNRM-CM5 1975 - 2005", nombre_fig = "bias5_t.ens"
     , escala = c(-5, 5), label_escala = "ºC", resta = 0, brewer = "RdBu", revert = "si", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(-5, 5, by = 1), breaks_c_f = seq(-5, 5, by = 0.5), r = 1, na_fill = 0, salida = "/Salidas/TP1/")

# correlaciones
aux = array(data = cor5_t[,,1]*cor5_t[,,2], c(144,73,1)) 
mapa(lista = aux, titulo = "Correlaciòn temperatura CNRM-CM5 vs observaciones  1975 - 2005", nombre_fig = "cor5_t.ens"
     , escala = c(0, 1), label_escala = "R", resta = 0, brewer = "YlOrRd", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 1, by = 0.1), breaks_c_f = seq(0, 1, by = 0.1), r = 1, na_fill = -10000, salida = "/Salidas/TP1/")



# pp5 emsamble
aux = array(data = calc_means[[5]], c(144,73,1)) 
mapa(lista = aux, titulo = "Precipitacíon media ensamble CNRM-CM5 1975 - 2005", nombre_fig = "pp5.ens.mean"
     , escala = c(0, 3500), label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 3500, by = 250), breaks_c_f = seq(0, 3500, by = 250), r = 1, salida = "/Salidas/TP1/")
#r.s
mapa(lista = calc_means[[6]], titulo = "Precipitación media CNRM-CM5 1975 - 2005", nombre_fig = "pp5.mean.r"
     , escala = c(0, 3500), label_escala = "mm", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 3500, by = 250), breaks_c_f = seq(0, 3500, by = 250), r = 9, salida = "/Salidas/TP1/")

aux = array(data = sd_s[[3]], c(144,73,1)) 
mapa(lista = aux, titulo = "Precipitación desvio ensamble CNRM-CM5 1975 - 2005", nombre_fig = "sd_pp5.ens.mean"
     , escala = c(0, 1), label_escala = "", resta = 0, brewer = "YlGnBu", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 1, by = 0.1), breaks_c_f = seq(0, 1, by = 0.1), r = 1, salida = "/Salidas/TP1/")

#aux = array(data = sd_s[[4]], c(144,73,1)) 
#mapa(lista = aux, titulo = "Temperatura desvio ensamble CNRM-CM5 1975 - 2005", nombre_fig = "prueba"
#     , escala = c(0, 1), label_escala = "", resta = 0, brewer = "YlOrRd", revert = "no", niveles = 9
#     , contour = "si", lon = lon, lat = lat, escala_dis = seq(0, 1, by = 0.1), breaks_c_f = seq(0, 1, by = 0.1), r = 1, salida = "/Salidas/TP1/")


aux = array(data = bias5_pp_ens, c(144,73,1)) 
aux[which(aux > 100)] = 105;aux[which(aux < -100)] = -105;  
mapa(lista = aux, titulo = "Precipitación Bias % ensamble CNRM-CM5 1975 - 2005", nombre_fig = "bias5_pp.ens"
     , escala = c(-100, 100), label_escala = "mm", resta = 0, brewer = "BrBG", revert = "no", niveles = 9
     , contour = "si", lon = lon, lat = lat, escala_dis = seq(-100, 100, by = 20), breaks_c_f = seq(-100, 100, by = 10), r = 1, na_fill = -10000, salida = "/Salidas/TP1/")

aux = array(data = cor5_pp[,,1]*cor5_pp[,,2], c(144,73,1)) 
mapa(lista = aux, titulo = "Correlaciòn precipitación CNRM-CM5 vs observaciones  1975 - 2005", nombre_fig = "cor5_pp.ens"
     , escala = c(-1, 1), label_escala = "R", resta = 0, brewer = "RdBu", revert = "si", niveles = 9
     , contour = "no", lon = lon, lat = lat, escala_dis = seq(-1, 1, by = 0.2), breaks_c_f = seq(-1, 1, by = 0.2), r = 1, na_fill = 0, salida = "/Salidas/TP1/")


# subregion

aux = array(data = regiones_means_t5[[1]], dim = c(3, 7, 1))
aux2 = array(data = regiones_means_t5[[2]], dim = c(5, 6, 1))
aux3 = array(data = regiones_means_t5[[3]], dim = c(21, 5, 1))

mapa_reg(lista = aux, lista2 = aux2, lista3 = aux3, titulo = "Temperatura media ensamble CNRM-CM5 1975 - 2005", nombre_fig = "t5_regiones_means"
         , escala = c(0, 30), label_escala = "ºC", resta = 0,revert = "no", brewer = "YlOrRd", niveles = 9
         ,  lons = lon, lats = lat, aux_lons = aux_lons, aux_lats = aux_lats, 
         escala_dis = seq(0, 30, by = 2 ), r = 1, na_fill = 0, salida = "/Salidas/TP1/")


aux = array(data = regiones_sd_t5[[1]], dim = c(3, 7, 1))
aux2 = array(data = regiones_sd_t5[[2]], dim = c(5, 6, 1))
aux3 = array(data = regiones_sd_t5[[3]], dim = c(21, 5, 1))

mapa_reg(lista = aux, lista2 = aux2, lista3 = aux3, titulo = "Desvio T ensamble CNRM-CM5 1975 - 2005", nombre_fig = "t5.sd_regiones"
         , escala = c(0, 0.4), label_escala = "", resta = 0,revert = "no", brewer = "YlOrRd", niveles = 9
         ,  lons = lon, lats = lat, aux_lons = aux_lons, aux_lats = aux_lats, 
         escala_dis = seq(0, 0.4, by = 0.05), r = 1, na_fill = 0, salida = "/Salidas/TP1/")



aux = array(data = regiones_bias_t5[[1]], dim = c(3, 7, 1))
aux2 = array(data = regiones_bias_t5[[2]], dim = c(5, 6, 1))
aux3 = array(data = regiones_bias_t5[[3]], dim = c(21, 5, 1))

mapa_reg(lista = aux, lista2 = aux2, lista3 = aux3, titulo = "Bias T ensamble CNRM-CM5 1975 - 2005", nombre_fig = "t5.bias_regiones"
         , escala = c(-2, 2), label_escala = "", resta = 0,revert = "si", brewer = "RdBu", niveles = 9
         ,  lons = lon, lats = lat, aux_lons = aux_lons, aux_lats = aux_lats, 
         escala_dis = seq(-2, 2, by = 0.5), r = 1, na_fill = 0, salida = "/Salidas/TP1/")

aux = array(data = regiones_means_pp5[[1]], dim = c(3, 7, 1))
aux2 = array(data = regiones_means_pp5[[2]], dim = c(5, 6, 1))
aux3 = array(data = regiones_means_pp5[[3]], dim = c(21, 5, 1))

mapa_reg(lista = aux, lista2 = aux2, lista3 = aux3, titulo = "Precipitacíón media ensamble CNRM-CM5 1975 - 2005", nombre_fig = "pp5_regiones_means"
         , escala = c(500, 3000), label_escala = "ºC", resta = 0,revert = "no", brewer = "YlGnBu", niveles = 9
         ,  lons = lon, lats = lat, aux_lons = aux_lons, aux_lats = aux_lats, 
         escala_dis = seq(500, 3000, by = 200 ), r = 1, na_fill = 0, salida = "/Salidas/TP1/")

aux = array(data = regiones_sd_pp5[[1]], dim = c(3, 7, 1))
aux2 = array(data = regiones_sd_pp5[[2]], dim = c(5, 6, 1))
aux3 = array(data = regiones_sd_pp5[[3]], dim = c(21, 5, 1))

mapa_reg(lista = aux, lista2 = aux2, lista3 = aux3, titulo = "Desvio T ensamble CNRM-CM5 1975 - 2005", nombre_fig = "pp5.sd_regiones"
         , escala = c(0, 0.4), label_escala = "", resta = 0,revert = "no", brewer = "YlOrRd", niveles = 9
         ,  lons = lon, lats = lat, aux_lons = aux_lons, aux_lats = aux_lats, 
         escala_dis = seq(0, 0.4, by = 0.05), r = 1, na_fill = 0, salida = "/Salidas/TP1/")

#bias pp no --> escalas muuy distitnas yu la cordillera caga todo ( y solo 2 px en n34)

### idem con cnrm6



# series temporales

grafico_ts(data1 = anom_t_obs, data2 = anom_t5, data3 = anom_t6, nombre_fig = "/home/auri/Facultad/Materias/c-dinamica/TPs/Salidas/TP1/anom.t.ts.png",
           titulo = "Anomalía T 1975 - 2005", ylab = "ºC")

grafico_ts(data1 = anom_t_obs_cont, data2 = anom_t5_cont, data3 = anom_t6_cont, nombre_fig = "/home/auri/Facultad/Materias/c-dinamica/TPs/Salidas/TP1/anom.t_cont.ts.png",
           titulo = "Anomalía T continental 1975 - 2005", ylab = "ºC")

grafico_ts(data1 = anom_t_obs_oce, data2 = anom_t5_oce, data3 = anom_t6_oce, nombre_fig = "/home/auri/Facultad/Materias/c-dinamica/TPs/Salidas/TP1/anom.t_oce.ts.png",
           titulo = "Anomalía T oceanica 1975 - 2005", ylab = "ºC")

grafico_ts(data1 = anom_pp_obs, data2 = anom_pp5, data3 = anom_pp6, nombre_fig = "/home/auri/Facultad/Materias/c-dinamica/TPs/Salidas/TP1/anom.pp.ts.png",
           titulo = "Anomalía PP 1975 - 2005", ylab = "mm")

grafico_ts(data1 = anom_pp_obs, data2 = anom_pp5, data3 = anom_pp6, nombre_fig = "/home/auri/Facultad/Materias/c-dinamica/TPs/Salidas/TP1/anom.pp.ts.png",
           titulo = "Anomalía PP continental 1975 - 2005", ylab = "mm")


titulos = c("Anomalia T SAn", "Anomalía T SSESA", "Anomalia T Niño3.4")
nombres_figs = c("anom.t.ts_SAn.png", "anom.t.ts_SESA.png", "anom.t.ts_Niño3.4.png")

titulos_pp = c("Anomalia PP SAn", "Anomalía PP SSESA", "Anomalia PP Niño3.4")
nombres_figs_pp = c("anom.pp.ts_SAn.png", "anom.pp.ts_SESA.png", "anom.pp.ts_Niño3.4.png")

for(i in 1:3){
  
  grafico_ts(data1 = anom_t_obs_reg[[i]], data2 = anom_t5_reg[[i]], data3 = anom_t6_reg[[i]],
             nombre_fig = paste("/home/auri/Facultad/Materias/c-dinamica/TPs/Salidas/TP1/",nombres_figs[i], sep = ""), titulo = titulos[i], ylab = "ºC")
  
  grafico_ts(data1 = anom_pp_obs_reg[[i]], data2 = anom_pp5_reg[[i]], data3 = anom_pp6_reg[[i]], 
             nombre_fig =  paste("/home/auri/Facultad/Materias/c-dinamica/TPs/Salidas/TP1/",nombres_figs_pp[i], sep = ""), titulo = titulos_pp[i], ylab = "mm")
  
}
