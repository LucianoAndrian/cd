#TP final 
library(ncdf4)
library(ggplot2)

source("FUNCIONES.R")

ruta_nc = "/home/auri/Facultad/Materias/c-dinamica/TPs/NC_TPfinal/"

#### Funciones ####
Fig7.3 = function(data){
  #Funcion para calcular las anomalias de P 
  # dim de data de entrada c(lon, lat, meses, miembros de ensamble)
  
  r.dim = length(data[1,1,1,]) # cantidad de miembros de ensamble
  
  global_mean = apply(data, c(4), mean, na.rm = T)
  SH_mean = apply(data[,1:37,,], c(4), mean, na.rm = T)
  NH_mean = apply(data[,37:73,,], c(4), mean, na.rm = T)
  
  
  aux1 = apply(data, c(3,4), mean, na.rm = T)
  aux2 = apply(data[,1:37,,], c(3,4), mean, na.rm = T)
  aux3 = apply(data[,37:73,,], c(3,4), mean, na.rm = T)
  
  global_anom = array(NA, dim = c(12,r.dim))
  SH_anom = array(NA, dim = c(12,r.dim))
  NH_anom = array(NA, dim = c(12,r.dim))
  for(i in 1:r.dim){
    global_anom[,i] = aux1[,i] - global_mean[i]
    SH_anom[,i] = aux2[,i] - SH_mean[i]
    NH_anom[,i] = aux3[,i] - NH_mean[i]
  }
  
  V = list()
  V[[1]] = global_anom; V[[2]] = SH_anom; V[[3]] = NH_anom
  return(V)
  
}

PlotMonthsTS = function(data, titulo, nombre, tl = F, data2049 = NULL, data2099 = NULL){
  
  # graficado de salida de Fig7.3
  
  # todo esto es para poder graficar el spread (ggplot::geom_ribbon)
  datos = array(NA, c(12,9))
  datos[,1] = apply(data[[1]], c(1), mean)
  datos[,2] = apply(data[[1]], c(1), max)
  datos[,3] = apply(data[[1]], c(1), min)
  datos[,4] = apply(data[[2]], c(1), mean)
  datos[,5] = apply(data[[2]], c(1), max)
  datos[,6] = apply(data[[2]], c(1), min)
  datos[,7] = apply(data[[3]], c(1), mean)
  datos[,8] = apply(data[[3]], c(1), max)
  datos[,9] = apply(data[[3]], c(1), min)
  

  
  if(tl == T){
    datos = array(NA, c(12,9))
    datos[,1] = apply(data[[1]], c(1), mean)
    datos[,2] = apply(data[[1]], c(1), max)
    datos[,3] = apply(data[[1]], c(1), min)
    datos[,4] = apply(data[[2]], c(1), mean)
    datos[,5] = apply(data[[2]], c(1), max)
    datos[,6] = apply(data[[2]], c(1), min)
    datos[,7] = apply(data[[3]], c(1), mean)
    datos[,8] = apply(data[[3]], c(1), max)
    datos[,9] = apply(data[[3]], c(1), min)
    
 if(is.null(data2049[[1]])){
   print("Sin datos!")
 }  else {
   # esto al final no se usa para el spread porque queda todo junto en un grafico y no se entiende
   datos2 = array(NA, c(12,9))
   datos2[,1] = apply(data2049[[1]], c(1), mean)
   datos2[,2] = apply(data2049[[1]], c(1), max)
   datos2[,3] = apply(data2049[[1]], c(1), min)
   datos2[,4] = apply(data2049[[2]], c(1), mean)
   datos2[,5] = apply(data2049[[2]], c(1), max)
   datos2[,6] = apply(data2049[[2]], c(1), min)
   datos2[,7] = apply(data2049[[3]], c(1), mean)
   datos2[,8] = apply(data2049[[3]], c(1), max)
   datos2[,9] = apply(data2049[[3]], c(1), min)
   
   datos3 = array(NA, c(12,9))
   datos3[,1] = apply(data2099[[1]], c(1), mean)
   datos3[,2] = apply(data2099[[1]], c(1), max)
   datos3[,3] = apply(data2099[[1]], c(1), min)
   datos3[,4] = apply(data2099[[2]], c(1), mean)
   datos3[,5] = apply(data2099[[2]], c(1), max)
   datos3[,6] = apply(data2099[[2]], c(1), min)
   datos3[,7] = apply(data2099[[3]], c(1), mean)
   datos3[,8] = apply(data2099[[3]], c(1), max)
   datos3[,9] = apply(data2099[[3]], c(1), min)
   
   datos = cbind(datos, datos2, datos3)
   
 }
}
  
  datos = as.data.frame(rbind(datos/100, datos[1,]/100))
  datos = as.data.frame(cbind(datos, meses = seq(1, 13)))


if(tl == T){
  g = ggplot(datos, aes(x = meses)) + theme_minimal() + 
    geom_line(aes(y = datos[,1], colour = "Historico", linetype = "Global"), size = 1) +
    geom_line(aes(y = datos[,4], colour = "Historico", linetype = "HS"), size = 1) +
    geom_line(aes(y = datos[,7], colour = "Historico", linetype = "HN"), size = 1) +
    geom_line(aes(y = datos[,1+9], colour = "2020 - 2049", linetype = "Global"), size = 1) +
    geom_line(aes(y = datos[,4+9], colour = "2020 - 2049", linetype = "HS"), size = 1) +
    geom_line(aes(y = datos[,7+9], colour = "2020 - 2049", linetype = "HN"), size = 1) +
    geom_line(aes(y = datos[,1+9*2], colour = "2070 - 2099", linetype = "Global"), size = 1) +
    geom_line(aes(y = datos[,4+9*2], colour = "2070 - 2099", linetype = "HS"), size = 1) +
    geom_line(aes(y = datos[,7+9*2], colour = "2070 - 2099", linetype = "HN"), size = 1) + 
    geom_hline(yintercept = 0, color = "black", size = 0.2)+
    scale_colour_manual("", 
                        breaks = c("Historico", "2020 - 2049", "2070 - 2099"),
                        values = c("black", "turquoise2", "coral")) +
    scale_linetype_manual("",
                          breaks = c("Global", "HS", "HN"),
                          values = c(1, 2, 3))+
    scale_x_continuous(labels = c(month.abb, month.abb[1]),
                       breaks = seq(1, 13, by = 1)) +
    scale_y_continuous(breaks = seq(-2,2, by = 1), limits = c(-2,2))
  
  nombre = paste(nombre, "_futuro", sep = "")
    
} else {
  
  g = ggplot(datos, aes(x = meses)) + theme_minimal() + 
    geom_ribbon(aes(x = meses, ymin = datos[,3], ymax = datos[,2]), fill="snow4", alpha = .4) +
    geom_ribbon(aes(x = meses, ymin = datos[,6], ymax = datos[,5]), fill="steelblue3", alpha = .3) +
    geom_ribbon(aes(x = meses, ymin = datos[,9], ymax = datos[,8]), fill="springgreen3", alpha = .3) +
    geom_line(aes(y = datos[,1], colour = "Global"), size = 1, linetype = 1) +
    geom_line(aes(y = datos[,4], colour = "HS"), size = .8, linetype = 1) +
    geom_line(aes(y = datos[,7], colour = "HN"), size = .8, linetype = 1) +
    geom_hline(yintercept = 0, color = "black", size = 0.2)+
    scale_color_manual("", 
                      breaks = c("Global", "HS", "HN"),
                      values = c("black", "steelblue3", "springgreen3")) +
    scale_x_continuous(labels = c(month.abb, month.abb[1]),
                       breaks = seq(1, 13, by = 1)) +
    scale_y_continuous(breaks = seq(-2,2, by = 1), limits = c(-2,2))
  
}
  g =  g + ylab("[hPa]") + ggtitle(titulo) + xlab("") +
    theme(axis.text.y   = element_text(size = 14, color = "black"), axis.text.x   = element_text(size = 14, color = "black"), axis.title.y  = element_text("ºC"),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          panel.ontop = F,
          plot.title = element_text(hjust = 0.5, size = 18),
          legend.position = "bottom", legend.key.width = unit(1, "cm"), legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 15)) 
  
  ggsave(paste(getwd(), "/Salidas/TP_FINAL/",nombre,".jpg",sep =""), plot = g, width = 25, height = 15  , units = "cm")
  
}

Vinf = function(data){
  # velocidad inferida a partir de las diferencias de presion
  
  R = 6371000
  num = 2*pi*R**2
  den = 86400*30*1013*100*2*pi*R # la entrada es Pa, aca tambien mantengo Pa. P0 = 1013*100
  
  x = vector()
  
  for(i in 0:11){
    # calculo del delta de P entre un mes y otro
    if((i + 1) == 1){
      aux = data[12]-data[1]
      x[i+1] = (num*aux)/den
    } else {
      aux = data[1+i]-data[0+i]
      x[i+1] = (num*aux)/den
    }
  }
  
  return(x*1000) # pasa a mm/s
}  

####----------------------------- Difrencias de Presion --------------------------------- ####

SSP = c("126", "585")

for(ssp in 1:2){
  
  
  ncs = c("PSL_CNRM-CM6.nc", paste("PSL_2049_", SSP[ssp], ".CNRM-CM6.nc", sep = ""), paste("PSL_2099_", SSP[ssp],".CNRM-CM6.nc", sep = ""))
  # estos nc tienen el promedio de cada mes para todo el periodo,
  # fueron creados previamente porque consumia todo el RAM en leer los nc solamente
  # y trabajar con arrays muy grandes que ademas de los meses abarquen todos los años,
  # implicaba tener arrays de 1gb ***. q sumando todo me dejaba 
  # muy poco espacio en la RAM para hacer las cuentas
  # estos NC tienen estas dimenciones (lon, lat, meses, miembros de ensamble)
  psl = list()
  
  for(i in 1:3){
    
    aux = nc_open(paste(ruta_nc, ncs[i], sep = ""))
    psl[[i]] = ncvar_get(aux, "psl")
    nc_close(aux)
    
    if(i>1){ # peso por las latitudes
      psl[[i]] = psl[[i]]*lats[,,,1:6] # en lo proyectado tiene solo 6 miembros 
    } else {
      lat = seq(-90, 90, by = 2.5)
      lats =  array(data = t(array(data = cos((lat*pi)/180), c(73,144))), c(dim(psl[[i]]))) # solo lo va hacer una vez y ya queda lats.
      psl[[i]] = psl[[i]]*lats
    }
  }
  
  # aca usa las dos funciones juntas PlotMonthsTS y Fig7.3
  nombre = paste("PSL_ANOM_", SSP[ssp], sep ="")
  PlotMonthsTS(data = Fig7.3(psl[[1]]), titulo = "Anomalías de presión en superficie", nombre = nombre,
               tl = T, data2049 = Fig7.3(psl[[2]]), data2099 = Fig7.3(psl[[3]]))
  
  
  PlotMonthsTS(data = Fig7.3(psl[[1]]), titulo = "Anomalías de presión en superficie", nombre = nombre,
               tl = F, data2049 = Fig7.3(psl[[2]]), data2099 = Fig7.3(psl[[3]]))
  
}



#####---------------------TRANSPORTE MERID---------------------------- #####
### HACER PARA EL RESTO DE LOS TIEMPOS ###


# INFERIDO
aux = Fig7.3(psl[[1]])
psl_mean_hn = apply(aux[[3]], c(1), mean)
  
x1 = Vinf(psl_mean_hn)
x1[13]=x1[1]  # para que vaya de enereo a enero

# grafico
data = as.data.frame(matrix(data = NA, nrow = 13, ncol = 2)) # REC--> como se creaba un dataframe!!!?
data[,1] = x1; data[,2] = seq(1, 13, by = 1)
colnames(data) = c("v", "meses")

titulo = "Viento meridional inferiod a partir de las diferencias de PSL [mm/s] ¿?"
nombre = "v_inf"
g = ggplot(data) + theme_minimal()+
  geom_line(aes(y = v, x = meses)) +
  geom_hline(yintercept = 0, color = "black", alpha = .2)+
  scale_x_continuous("", 
                     breaks = seq(1, 13), labels = c(month.abb, month.abb[1])) +
  scale_y_continuous("[mm/s]", breaks = seq(-3,3), limits = c(-3,3)) +
  ggtitle(titulo) +
  theme(axis.text.y   = element_text(size = 14, color = "black"), axis.text.x   = element_text(size = 14, color = "black"), axis.title.y  = element_text("ºC"),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.ontop = F,
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.position = "bottom", legend.key.width = unit(1, "cm"), legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 15)) 

ggsave(paste(getwd(), "/Salidas/TP_FINAL/",nombre,".jpg",sep =""), plot = g, width = 25, height = 15  , units = "cm")
  



# CON V DEL MODELO
aux = nc_open(paste(ruta_nc, "V_CNRM-CM6.nc", sep = ""))
# este NC, tambien creado antes, tiene dimenciones c(lon, lat, niveles(9), meses, miemrbos de ensamble, u y v ) ***
v = ncvar_get(aux, "viento")[,37,,,,2] # tomo solo sobre el ecuador y la componente v
nc_close(aux)



t = 297 # calculada a partir de los datos de TAS del modelo, sobre el ecuador
res_t = -c(1.06, 7.44, 14.3, 29.5, 54, 70.3, 89, 115.3, 154) # valores que se deben restar a T media en sfc calculados a partir de formula
                                                  # de grad. adiabatico seco y altitud segun presion.

x = t+res_t
niveles = c(100000, 92500, 85000, 70000, 50000, 40000, 30000, 20000, 10000) # Pa

pesos = (niveles/(286.9*x)) # Kg m-3
# print(pesos)  
#[1] 1,1763534 1,1037726 1,0301477 0,8776527 0,6677270 0,5554637
#[7] 0,4393532 0,3154330 0,1787454

v2 = array(NA, c(dim(v))) # v tiene dimenciones c(lon, niveles, meses, miembros de ensamble)
for(i in 1:9){
  v2[,i,,] = v[,i,,]*pesos[i] # pesando. unidades Kg m-3 m s-1
}


## forma 2
v_mean = apply(v2, c(2,4), mean, na.rm = T) # media para cada nivel y miembro de ensamble
aux1 = apply(v2, c(2,3,4), mean, na.rm = T) # media zonal para cada nivel, mes y miembro

global_anom = array(NA, dim = c(9,12,29)) # va guardar, anomalia de cada nivel, para cada mes y miembro
for(i in 1:12){
  global_anom[,i,] = aux1[,i,] - v_mean
  
}

#y2 = apply(apply(global_anom, c(2,3), sum, na.rm = T)/sum(pesos),c(1), mean)
aux = apply(global_anom, c(2,3), sum, na.rm = T)/sum(pesos) # promedio pesado. unidades m s-1
y2 = apply(aux, c(1), mean) # promedio de los miembros

y2[13] = y2[1]

data = as.data.frame(matrix(data = NA, nrow = 13, ncol = 3))
data[,1] = y; data[,2] = y2; data[,3] = seq(1, 13)
colnames(data) = c("Forma1", "Forma2", "meses")

titulo = "Promedio vertical de anomalia de V sobre el ecuador [m/s], valores esperados del orden 0.002 m/s"
nombre = "v2"
g = ggplot(data) + theme_minimal()+
  geom_line(aes(y = Forma1, x = meses, colour = "Forma 1")) +
  geom_line(aes(y = Forma2, x = meses, colour = "Forma 2")) +
  geom_hline(yintercept = 0, color = "black", alpha = .2)+
  scale_color_manual("", breaks = c("Forma 1", "Forma 2"), values = c("black", "firebrick"))+
  scale_x_continuous("", 
                     breaks = seq(1, 13), labels = c(month.abb, month.abb[1])) +
  scale_y_continuous("[m/s]", breaks = seq(-2,2, by = .5), limits = c(-1.5,1.5)) +
  ggtitle(titulo) +
  theme(axis.text.y   = element_text(size = 14, color = "black"), axis.text.x   = element_text(size = 14, color = "black"), axis.title.y  = element_text("ºC"),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.ontop = F,
        plot.title = element_text(hjust = 0.5, size = 15),
        legend.position = "bottom", legend.key.width = unit(1, "cm"), legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 15)) 

ggsave(paste(getwd(), "/Salidas/TP_FINAL/",nombre,".jpg",sep =""), plot = g, width = 25, height = 15  , units = "cm")


#### PSL ANOM LONS ####
# Presion por lon.
#TP final 
library(ncdf4)
library(ggplot2)

source("FUNCIONES.R")

ruta_nc = "/home/auri/Facultad/Materias/c-dinamica/TPs/NC_TPfinal/"

Fig7.3Lons = function(data){
  lon = seq(1,360, by = 2.5)
  lons = list()
  lons[[1]] = seq(which(lon == 51), which(lon == 151)); lons[[2]] = seq(which(lon == 151), which(lon == 281))
  aux1 = seq(which(lon == 1), which(lon == 31)); aux2 = seq(which(lon == 281), which(lon == 358.5))
  lons[[3]] = c(aux1, aux2)
  
  r.dim = length(data[1,1,1,]) # cantidad de miembros de ensamble
  
  global_mean = apply(data, c(4), mean, na.rm = T)
  SH_mean = apply(data[,1:37,,], c(4), mean, na.rm = T)
  NH_mean = apply(data[,37:73,,], c(4), mean, na.rm = T)
  
  aux1 = apply(data, c(3,4), mean, na.rm = T)
  aux2 = apply(data[,1:37,,], c(1,3,4), mean, na.rm = T)
  aux3 = apply(data[,37:73,,], c(1,3,4), mean, na.rm = T)
  
  global_anom = array(NA, dim = c(12,r.dim))
  SH_anom = array(NA, dim = c(144,12,r.dim))
  NH_anom = array(NA, dim = c(144,12,r.dim))
  
  for(i in 1:r.dim){ # puede no ser necesario.
    global_anom[,i] = aux1[,i] - global_mean[i]
    SH_anom[,,i] = aux2[,,i] - SH_mean[i]
    NH_anom[,,i] = aux3[,,i] - NH_mean[i]
  }
  
  # por lons
  SH_anom_global = apply(SH_anom, c(2,3), mean); NH_anom_global = apply(NH_anom, c(2,3), mean)
  
  SH_anom_lons = array(NA, dim = c(12,3,r.dim)); NH_anom_lons = array(NA, dim = c(12,3,r.dim))
  for(l in 1:3){
    SH_anom_lons[,l,] = apply(SH_anom[lons[[l]],,], c(2,3), mean)
    NH_anom_lons[,l,] = apply(NH_anom[lons[[l]],,], c(2,3), mean)
  }
  
  
  V = list()
  V[[1]] = global_anom; V[[2]] = SH_anom_global; V[[3]] = NH_anom_global
  V[[4]] = SH_anom_lons; V[[5]] = NH_anom_lons
  return(V)
  
}

PlotMonthsTSLons = function(data, titulo, nombre, lons.rm = c(1, 1, 1)){
  # graficado de salida de Fig7.3
  
  datos = array(NA, c(12,8))
  datos[,1] = apply(data[[2]], c(1), mean)
  datos[,2] = apply(data[[3]], c(1), mean)
  datos[,3:5] = apply(data[[4]], c(1,2), mean)
  datos[,6:8] = apply(data[[5]], c(1,2), mean)
  
  datos = as.data.frame(rbind(datos/100, datos[1,]/100))
  datos = as.data.frame(cbind(datos, meses = seq(1, 13))) 
  
  breaks = c("50º - 120º", "150º - 280º", "280º - 30º")
  values = c("firebrick", "steelblue3", "springgreen3")
  
  breaks = breaks[which(!is.na(lons.rm))] 
  values = values[which(!is.na(lons.rm))] 
  
  g =  ggplot(datos, aes(x = meses)) + theme_minimal() + 
    geom_line(aes(y = datos[,1], linetype = "HS"), size = 1, alpha = .5, color = "black") +
    geom_line(aes(y = datos[,2], linetype = "HN"), size = 1, alpha = .5, color = "black") 
  
  if(is.na(lons.rm[1] & !is.na(lons.rm[2]) & !is.na(lons.rm[3]))){
    
    g = g + geom_line(aes(y = datos[,4], linetype = "HS", colour = "150º - 280º"), size = 1) + 
      geom_line(aes(y = datos[,5], linetype = "HS", colour = "280º - 30º"), size = 1) + 
      geom_line(aes(y = datos[,7], linetype = "HN", colour = "150º - 280º"), size = 1) + 
      geom_line(aes(y = datos[,8], linetype = "HN", colour = "280º - 30º"), size = 1) +
      scale_color_manual("", 
                         breaks = breaks,
                         values = values)
    
  } else if(is.na(lons.rm[2] & !is.na(lons.rm[1]) & !is.na(lons.rm[3]))){
    
    g = g + geom_line(aes(y = datos[,3], linetype = "HS", colour = "50º - 120º"), size = 1) +
      geom_line(aes(y = datos[,5], linetype = "HS", colour = "280º - 30º"), size = 1) +
      geom_line(aes(y = datos[,6], linetype = "HN", colour = "50º - 120º"), size = 1) +
      geom_line(aes(y = datos[,8], linetype = "HN", colour = "280º - 30º"), size = 1) +
      scale_color_manual("", 
                         breaks = breaks,
                         values = values)
    
  } else if(is.na(lons.rm[3] & !is.na(lons.rm[2]) & !is.na(lons.rm[1]))) {
    
    g = g + geom_line(aes(y = datos[,3], linetype = "HS", colour = "50º - 120º"), size = 1) +
      geom_line(aes(y = datos[,4], linetype = "HS", colour = "150º - 280º"), size = 1) + 
      geom_line(aes(y = datos[,6], linetype = "HN", colour = "50º - 120º"), size = 1) + 
      geom_line(aes(y = datos[,7], linetype = "HN", colour = "150º - 280º"), size = 1) + 
      scale_color_manual("", 
                         breaks = breaks,
                         values = values)
  } else if(is.na(lons.rm[1] & is.na(lons.rm[2]) & !is.na(lons.rm[3]))){
    
    g = g + geom_line(aes(y = datos[,5], linetype = "HS", colour = "280º - 30º"), size = 1) +
      geom_line(aes(y = datos[,8], linetype = "HN", colour = "280º - 30º"), size = 1) +
      
      scale_color_manual("", 
                         breaks = breaks,
                         values = values)
    
  } else if(is.na(lons.rm[1] & is.na(lons.rm[3]) & !is.na(lons.rm[2]))){
    
    g = g + geom_line(aes(y = datos[,4], linetype = "HS", colour = "150º - 280º"), size = 1) + 
      geom_line(aes(y = datos[,7], linetype = "HN", colour = "150º - 280º"), size = 1) + 
      
      scale_color_manual("", 
                         breaks = breaks,
                         values = values)
  } else if(is.na(lons.rm[2] & is.na(lons.rm[3]) & !is.na(lons.rm[1]))){
    
    g = g + geom_line(aes(y = datos[,3], linetype = "HS", colour = "50º - 120º"), size = 1) +
      geom_line(aes(y = datos[,6], linetype = "HN", colour = "50º - 120º"), size = 1) +
      
      scale_color_manual("", 
                         breaks = breaks,
                         values = values)
    
  } else {
    
    g = g + geom_line(aes(y = datos[,3], linetype = "HS", colour = "50º - 120º"), size = 1) +
      geom_line(aes(y = datos[,4], linetype = "HS", colour = "150º - 280º"), size = 1) + 
      geom_line(aes(y = datos[,5], linetype = "HS", colour = "280º - 30º"), size = 1) + 
      geom_line(aes(y = datos[,6], linetype = "HN", colour = "50º - 120º"), size = 1) + 
      geom_line(aes(y = datos[,7], linetype = "HN", colour = "150º - 280º"), size = 1) + 
      geom_line(aes(y = datos[,8], linetype = "HN", colour = "280º - 30º"), size = 1) +
      scale_color_manual("", 
                         breaks = breaks,
                         values = values)
    
  }
  
  g = g + geom_hline(yintercept = 0, color = "black", size = .5, alpha = 1) + 
    scale_linetype("") + 
    scale_x_continuous(labels = c(month.abb, month.abb[1]),
                       breaks = seq(1, 13, by = 1)) +
    scale_y_continuous(breaks = seq(-4,4, by = 1), limits = c(-4,4)) + 
    ylab("[hPa]") + ggtitle(titulo) + xlab("") +
    theme(axis.text.y   = element_text(size = 14, color = "black"), axis.text.x   = element_text(size = 14, color = "black"), axis.title.y  = element_text("ºC"),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          panel.ontop = F,
          plot.title = element_text(hjust = 0.5, size = 18),
          legend.position = "bottom", legend.key.width = unit(1, "cm"), legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 15)) 
  
  ggsave(paste(getwd(), "/Salidas/TP_FINAL/lons/",nombre,".jpg",sep =""), plot = g, width = 25, height = 15  , units = "cm")
  
}

SSP = c("126", "585")
nom = c("_hist", "_49", "_99")
for(ssp in 1:2){
  
  
  ncs = c("PSL_CNRM-CM6.nc", paste("PSL_2049_", SSP[ssp], ".CNRM-CM6.nc", sep = ""), paste("PSL_2099_", SSP[ssp],".CNRM-CM6.nc", sep = ""))
  print(ssp)
  for(i in 1:3){
    print(i)
    aux = nc_open(paste(ruta_nc, ncs[i], sep = ""))
    psl = ncvar_get(aux, "psl")
    nc_close(aux)
    
    if(i>1){ 
      psl = psl*lats[,,,1:6] # en lo proyectado tiene solo 6 miembros 
    } else {
      lat = seq(-90, 90, by = 2.5)
      lats =  array(data = t(array(data = cos((lat*pi)/180), c(73,144))), c(dim(psl))) # solo lo va hacer una vez y ya queda lats.
      psl = psl*lats
    }
    
    nombre = paste("PSL_Lon_anom",  nom[i], "_" ,SSP[ssp], sep ="")
    PlotMonthsTSLons(data = Fig7.3Lons(psl), titulo = "Anomalías de presión en superficie", nombre = nombre)
    
    
  }
}

##### V C-E lons ####

VCrossEq_Lons = function(nc, year, ssp){
  
  library(ncdf4)
  library(ggplot2)
  
  ruta_nc = "/home/auri/Facultad/Materias/c-dinamica/TPs/NC_TPfinal/"
  
  aux = nc_open(paste(ruta_nc, nc, sep = ""))
  aux1 = ncvar_get(aux, "viento")[,37,,,,2] 
  nc_close(aux)
  
  r.dim = length(aux1[1,1,1,]) 
  
  Pesos = function(){
    t = 297 # calculada a partir de los datos de TAS del modelo, sobre el ecuador
    res_t = -c(1.06, 7.44, 14.3, 29.5, 54, 70.3, 89, 115.3, 154) # valores que se deben restar a T media en sfc calculados a partir de formula
    # de grad. adiabatico seco y altitud segun presion.
    x = t+res_t
    niveles = c(100000, 92500, 85000, 70000, 50000, 40000, 30000, 20000, 10000) # Pa
    
    pesos = (niveles/(286.9*x)) # Kg m-3
    
    return(pesos)
  }
  
  #> Pesos()
  #[1] 1,1777844 1,1134549 1,0480031 0,9121026 0,7171883 0,6150040 0,5027214
  #[8] 0,3836582 0,2437437
  
  v2 = array(NA, c(dim(aux1))) # v tiene dimenciones c(lon, niveles, meses, miembros de ensamble)
  for(i in 1:9){
    v2[,i,,] = aux1[,i,,]*Pesos()[i] # pesando. unidades Kg m-3 m s-1
  }
  
  lon = seq(1,360, by = 2.5)
  lons = list()
  lons[[1]] = seq(which(lon == 51), which(lon == 151)); lons[[2]] = seq(which(lon == 151), which(lon == 281))
  aux1 = seq(which(lon == 1), which(lon == 31)); aux2 = seq(which(lon == 281), which(lon == 358.5))
  lons[[3]] = c(aux1, aux2)
  
  
  V = list()
  for(i in 1:3){
    
    v_anual.mean = apply(v2[lons[[i]],,,], c(1,2,4), mean, na.rm = T) 
    
    month.anom = array(NA, dim = c(length(lons[[i]]),9,12,r.dim))
    for(mes in 1:12){
      month.anom[,,mes,] =  v2[lons[[i]],,mes,] - v_anual.mean
    }
    
    aux = apply(month.anom, c(1,3,4), sum, na.rm = T)/sum(Pesos())
    m.anom_ens = apply(aux, c(2), mean, na.rm = T)
    m.anom_ens[13] = m.anom_ens[1]
    
    
    V[[i]] = m.anom_ens
    
  }
  
  
  
  data = as.data.frame(matrix(data = NA, nrow = 13, ncol = 4))
  data[,1] = V[[1]]; data[,2] = V[[2]]; data[,3] = V[[3]]; data[,4] = seq(1, 13)
  colnames(data) = c("Indico", "Pacifico", "Amer_Atla", "meses")
  
  titulo = paste("Promedio vertical de anomalìa de V sobre el ecuador [m/s] - ", year, ssp)
  nombre = paste("v_mod_lons.", year, ssp, sep = "")
  g = ggplot(data) + theme_minimal()+
    geom_line(aes(y = Indico, x = meses, colour = "50º-120º"), size = 1) +
    geom_line(aes(y = Pacifico, x = meses, colour = "150º-280º"), size = 1) +
    geom_line(aes(y = Amer_Atla, x = meses, colour = "280º-30º"), size = 1) +
    geom_hline(yintercept = 0, color = "black", alpha = .2)+
    scale_color_manual("", breaks = c("50º-120º", "150º-280º", "280º-30º"),  values = c("firebrick", "steelblue3", "springgreen3")) +
    scale_x_continuous("", 
                       breaks = seq(1, 13), labels = c(month.abb, month.abb[1])) +
    scale_y_continuous("[m/s]", breaks = seq(-2,2, by = .5), limits = c(-1.5,1.5)) +
    ggtitle(titulo) +
    theme(axis.text.y   = element_text(size = 14, color = "black"), axis.text.x   = element_text(size = 14, color = "black"), axis.title.y  = element_text("ºC"),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          panel.ontop = F,
          plot.title = element_text(hjust = 0.5, size = 15),
          legend.position = "bottom", legend.key.width = unit(1, "cm"), legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 15)) 
  
  ggsave(paste(getwd(), "/Salidas/TP_FINAL/c_ce/",nombre,".jpg",sep =""), plot = g, width = 25, height = 15  , units = "cm")
  
  rm(list = ls())
  
}

VCrossEq_Lons(nc = "V_CNRM-CM6.nc", year = "Historico", "")
VCrossEq_Lons(nc = "V_2049_126.CNRM-CM6.nc", year = "2020-2049", ssp = "SSP126")
VCrossEq_Lons(nc = "V_2099_126.CNRM-CM6.nc", year = "2070-2099", ssp = "SSP126")
VCrossEq_Lons(nc = "V_2049_585.CNRM-CM6.nc", year = "2020-2049", ssp = "SSP585")
VCrossEq_Lons(nc = "V_2099_585.CNRM-CM6.nc", year = "2070-2099", ssp = "SSP585")


rm(list = ls())
.rs.restartR()
