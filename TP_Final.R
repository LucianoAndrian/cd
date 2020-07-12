#TP final 
library(ncdf4)
library(ggplot2)

source("FUNCIONES.R")

ruta_nc = "/home/auri/Facultad/Materias/c-dinamica/TPs/NC_TPfinal/"


Fig7.3 = function(data){
  
  r.dim = length(data[1,1,1,])
  
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


PlotMonthsTS = function(data, titulo, nombre){
  
  datos = array(NA, c(12,10))
  datos[,1] = apply(data[[1]], c(1), mean)
  datos[,2] = apply(data[[1]], c(1), max)
  datos[,3] = apply(data[[1]], c(1), min)
  datos[,4] = apply(data[[2]], c(1), mean)
  datos[,5] = apply(data[[2]], c(1), max)
  datos[,6] = apply(data[[2]], c(1), min)
  datos[,8] = apply(data[[3]], c(1), mean)
  datos[,9] = apply(data[[3]], c(1), max)
  datos[,10] = apply(data[[3]], c(1), min)
  
  datos = as.data.frame(cbind(datos/100,meses = seq(1,12)))
  
  
g = ggplot(datos, aes(x = meses)) + theme_minimal() + 
    geom_ribbon(aes(x = meses, ymin = datos[,3], ymax = datos[,2]), fill="black", alpha = .4) +
    geom_ribbon(aes(x = meses, ymin = datos[,6], ymax = datos[,5]), fill="steelblue2", alpha = .4) +
    geom_ribbon(aes(x = meses, ymin = datos[,10], ymax = datos[,9]), fill="springgreen", alpha = .4) +
    geom_hline(yintercept = 0, color = "black", size = 0.2)+
    geom_line(aes(y = datos[,1], colour = "Global"), size = 0.5) +
    geom_line(aes(y = datos[,4], colour = "HS"), size = 1) +
    geom_line(aes(y = datos[,8], colour = "HN"), size = 1) +
    scale_colour_manual("", 
                        breaks = c("Global", "HS", "HN"),
                        values = c("black", "steelblue4", "springgreen")) +
    
    scale_x_continuous(labels = month.abb,
                       breaks = seq(1, 12, by = 1)) +
    scale_y_continuous(breaks = seq(-2,2, by = 1), limits = c(-2,2))+
    
    ylab("[hPa]") + ggtitle(titulo) + xlab("") +
    theme(axis.text.y   = element_text(size = 14, color = "black"), axis.text.x   = element_text(size = 14, color = "black"), axis.title.y  = element_text("ºC"),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          panel.ontop = F,
          plot.title = element_text(hjust = 0.5, size = 18),
          legend.position = "bottom", legend.key.width = unit(2, "cm"), legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 15)) 
  
  ggsave(paste(getwd(), "/Salidas/TP_FINAL/",nombre,".jpg",sep =""), plot = g, width = 20, height = 10  , units = "cm")
  
}

Vinf = function(data){
  # velocidad inferida a partir de las diferencias de presion
  
  R = 6371000
  num = 2*pi*R**2
  den = 86400*30*1013*100*2*pi*R
  
  x = vector()
  
  for(i in 0:11){
    if((i + 1) == 1){
      aux = data[12]-data[1]
      x[i+1] = (num*aux)/den
    } else {
      aux = data[1+i]-data[0+i]
      x[i+1] = (num*aux)/den
    }
  }
  
  return(x*1000) # mm/s
}  


####

aux = nc_open(paste(ruta_nc, "PSL_2099_585.CNRM-CM6.nc", sep = ""))
psl = ncvar_get(aux, "psl")
nc_close(aux)

lat = seq(-90, 90, by = 2.5)
lats =  array(data = t(array(data = cos((lat*pi)/180), c(73,144))), c(dim(psl)))
psl = psl*lats

PlotMonthsTS(data = Fig7.3(psl), titulo = "probandp", nombre = "prueba")

aux = Fig7.3(psl)

psl_mean_hn = apply(aux[[3]], c(1), mean)
  
x1 = Vinf(psl_mean_hn)
plot.ts(x1)



##--------------------#
aux = nc_open(paste(ruta_nc, "V_CNRM-CM6.nc", sep = ""))
v = ncvar_get(aux, "viento")[,37,,,,2]
nc_close(aux)
v_mean = apply(v[,3:4,,], c(4), mean, na.rm = T)
aux1 = apply(v, c(3,4), mean, na.rm = T)
global_anom = array(NA, dim = c(12,29))
for(i in 1:29){
  global_anom[,i] = aux1[,i] - v_mean[i]
  
}
y = apply(global_anom,c(1), mean)
plot.ts(y*1000, col = "blue")
