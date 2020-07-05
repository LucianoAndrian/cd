#TP5
# 1era parte.
library(ncdf4)
source("FUNCIONES.R")

mask = as.matrix(read.table("mask.txt"))
m = as.numeric(readline(("CM5 (1) o CM6(2): ")))

modelos = c("cm5", "cm6")
rs = c(9, 10)

lat = seq(-90, 90, by = 2.5)
lats =  array(data = t(array(data = cos((lat*pi)/180), c(73,144))), c(144,73,30,rs[m],3,2,5))

lon = seq(0, 360, by = 2.5)

nc = nc_open(paste("cnrm-", modelos[m],".nc", sep = ""))
v = ncvar_get(nc, "var2")
nc = 1
rm(nc)

v = v*lats*array(mask, dim = c(dim(v))) # pesa todas las variable por la latitud

v.mean = apply(v, c(1,2,4,5,6,7), mean)  # promedio de años



#rec v = 144,73,30,r,3,2,5
# 3 periodos
# 2 escenarios (el periodo his es el mismo en los dos)
# 5 variables- 1 temp, 2 huss, 3 pp, 4 et, 5 etp

EP = apply((v.mean[,,,,,4]/v.mean[,,,,,3]), c(1,2,4,5), mean, na.rm = T)

c = ifelse(test = m == 1, yes = 1, no = 10)

EPP = apply(((v.mean[,,,,,5]/c)/v.mean[,,,,,3]), c(1,2,4,5), mean, na.rm = T)


lat2 = as.matrix(read.table("lat.txt"))
lon2 = as.matrix(read.table("lon.txt"))
rcp.name = c("RCP2.6", "RCP8.5")
periodo = c("F. Cercano", "F. Lejano")
periodo.fig = c("49", "99")
variable = c("E/P", "EP/P")
colorbar = c("YlOrBr", "YlGnBu")
modelo = c("CNRM-CM5", "CNRM-CM6")
#escala = list(); escala[[1]] = seq(0, 1, 0.1); escala[[2]] = seq(-150, 150, by = 25); escala[[3]] = seq(-150, 150, by = 25)
revert = c(FALSE, FALSE, FALSE)


aux = array(EP[,,1,1], dim = c(144,73,1))
mapa_topo3(variable = aux, lon = lon2, lat = lat2, colorbar = "Spectral", escala = seq(0, 2, by = 0.2)
           , revert = T, titulo = paste("E/P -  Historico", " - ", modelo[m]), niveles = 11
           , mapa = "mundo", colorbar.pos = "bottom", label.escala = "", na.fill = -1000
           , nombre.fig = paste("EP.", modelos[m], "_historico", sep = "") , width = 30, height = 20
           , salida = "/Salidas/TP5/2daparte/" )  

  
aux = array(EPP[,,1,1], dim = c(144,73,1))
mapa_topo3(variable = aux, lon = lon2, lat = lat2, colorbar = "Spectral", escala = seq(0, 3, by = 0.25)
           , revert = T, titulo = paste("Ep/P -  Historico", " - ", modelo[m]), niveles = 11
           , mapa = "mundo", colorbar.pos = "bottom", label.escala = "", na.fill = -1000
           , nombre.fig = paste("EPP.", modelos[m], "_historico", sep = ""), width = 30, height = 20
           , salida = "/Salidas/TP5/2daparte/" )  


# diferencias 

aux2 = array(mask, dim = c(144, 73, 1))

for(rcp in 1:2){
  for(p in 2:3){
    
    aux = array(EP[,,p,rcp] - EP[,,1,1], dim = c(144,73,1))
    mapa_topo3(variable = aux, lon = lon2, lat = lat2, colorbar = "RdBu", escala = seq(-0.2, 0.2, by = 0.02)
               , revert = T, titulo = paste("E/P Dif.", periodo[p-1], "-  Historico ", rcp.name[rcp], " - ", modelo[m]), niveles = 11
               , mapa = "mundo", colorbar.pos = "bottom", label.escala = "", na.fill = 0
               , nombre.fig = paste("Dif_EP.", modelos[m], "_",  rcp.name[rcp], "_", periodo.fig[p-1], sep = "") , width = 30, height = 20
               , salida = "/Salidas/TP5/2daparte/"
               , variable.sig = aux2, sig = T, alpha.vsig = 1, color.vsig = "white")  
    
    
    aux = array(EPP[,,p,rcp] - EPP[,,1,1], dim = c(144,73,1))
    mapa_topo3(variable = aux, lon = lon2, lat = lat2, colorbar = "RdBu", escala = seq(-0.5, 0.5, by = 0.05)
               , revert = T, titulo = paste("Ep/P Dif.", periodo[p-1], "-  Historico ", rcp.name[rcp], " - ", modelo[m]), niveles = 11
               , mapa = "mundo", colorbar.pos = "bottom", label.escala = "", na.fill = 0
               , nombre.fig = paste("Dif_EPP.", modelos[m], "_",  rcp.name[rcp], "_", periodo.fig[p-1], sep = ""), width = 30, height = 20
               , salida = "/Salidas/TP5/2daparte/" 
               , variable.sig = aux2, sig = T, alpha.vsig = 1, color.vsig = "white")  
    
    
  }
}
  


#Budiko

lat.sa = seq(which(lat2 == -60),which(lat2 == 20)); lon.sa = seq(which(lon2 == 270), which(lon2 == 335))
aux2 = array(mask[lon.sa,lat.sa], dim = c(length(lon.sa),length(lat.sa),1))

aux = array(EP[lon.sa,lat.sa,1,1], dim = c(length(lon.sa),length(lat.sa),1))

aux3 = aux2

aux3[seq(which(lon.sa == 119), which(lon.sa == 123)), seq(which(lat.sa == 24), which(lat.sa == 30)) ,1] = 4
aux3[seq(which(lon.sa == 119), which(lon.sa == 126)), seq(which(lat.sa == 30), which(lat.sa == 36)) ,1] = 4

mask.budiko = aux3
mask.budiko[which(mask.budiko != 4)] = NA
mask.budiko[which(mask.budiko == 4)] = 1

mapa_topo3(variable = aux, lon = lon2[lon.sa], lat = lat2[lat.sa], colorbar = "Spectral", escala = seq(0.9, 1.4, by = 0.1)
           , revert = T, titulo = paste("E/P -  Historico", " - ", modelo[m]), niveles = 4
           , mapa = "SA", label.escala = "", na.fill = -1000, contour.fill = F
           , nombre.fig = paste("SA_EP.", modelos[m], "_historico", sep = "") , width = 20, height = 20
           , salida = "/Salidas/TP5/2daparte/", sig = T, variable.sig = aux2, alpha.vsig = 1, color.vsig = "white"
           , contour = T, variable.cont = aux3, nivel.vcont =  2, color.vcont = "red")  



# Colores en la region

mask.budiko_colores = mask.budiko
mask.budiko_colores[seq(which(lon.sa == 119), which(lon.sa == 123)), seq(which(lat.sa == 24), which(lat.sa == 27)) ,] = 2
mask.budiko_colores[seq(which(lon.sa == 119), which(lon.sa == 123)), seq(which(lat.sa == 27), which(lat.sa == 30)) ,] = 3
mask.budiko_colores[seq(which(lon.sa == 119), which(lon.sa == 126)), seq(which(lat.sa == 30), which(lat.sa == 33)) ,] = 4
mask.budiko_colores[seq(which(lon.sa == 119), which(lon.sa == 126)), seq(which(lat.sa == 33), which(lat.sa == 36)) ,] = 5

mapa_topo3(variable = mask.budiko_colores, lon = lon2[lon.sa], lat = lat2[lat.sa], colorbar = "Spectral", escala = seq(1, 5, by = 1)
           , revert = T, titulo = paste("Región Seleccionada"), niveles = 4
           , mapa = "SA", label.escala = "", na.fill = -1000, contour.fill = F
           , nombre.fig = "Regiones", width = 20, height = 20
           , salida = "/Salidas/TP5/2daparte/", sig = T, variable.sig = aux2, alpha.vsig = 1, color.vsig = "white"
           , contour = T, variable.cont = aux3, nivel.vcont =  2, color.vcont = "red")  




curva = as.data.frame(matrix(NA, ncol = 2, nrow = 31))
x = seq(0, 3, by = 0.1)
omega = c(2.6, 3.05)
for(o in 1:2){
  for(i in 0:30){
    
    curva[i,o] = 1 + x[i] - (1+x[i]**omega[o])**(1/omega[o])
    
    
  }
}

curva = cbind(curva, x)

colnames(curva) = c("omega1", "omega2", "x")





rcp.name = c(NULL, "RCP2.6", "RCP8.5")
periodo.fig = c(NA, "49", "99")
periodo = c("Historico", "2020-2049", "2070-2099")
for(rcp in 1:2){
  for(p in 1:3){
    
    titulo = paste(periodo[p], rcp.name[rcp], modelo[m], sep = " - ")
    if(p == 1){
      nombre = paste("Bko.his.", modelos[m], sep = "")
    } else {
      nombre = paste("Bko.", modelos[m], ".", periodo.fig[p], "_", rcp.name[rcp], sep = "")
    }
    
    
    
    EP.sa = array(EP[lon.sa,lat.sa,p,rcp], dim = c(length(lon.sa),length(lat.sa),1))*mask.budiko
    EPP.sa = array(EPP[lon.sa,lat.sa,p,rcp], dim = c(length(lon.sa),length(lat.sa),1))*mask.budiko
    
    puntos.EP_1 = array(EP.sa[seq(which(lon.sa == 119), which(lon.sa == 123)), seq(which(lat.sa == 24), which(lat.sa == 27)) ,]
                        , dim = c(length(seq(which(lon.sa == 119), which(lon.sa == 123)))*length(seq(which(lat.sa == 24), which(lat.sa == 27)))))
    
    puntos.EP_2 = array(EP.sa[seq(which(lon.sa == 119), which(lon.sa == 123)), seq(which(lat.sa == 27), which(lat.sa == 30)) ,]
                        , dim = c(length(seq(which(lon.sa == 119), which(lon.sa == 123)))*length(seq(which(lat.sa == 27), which(lat.sa == 30)))))
    
    puntos.EP_3 = array(EP.sa[seq(which(lon.sa == 119), which(lon.sa == 126)), seq(which(lat.sa == 30), which(lat.sa == 33)) ,]
                        , dim = c(length(seq(which(lon.sa == 119), which(lon.sa == 126)))*length(seq(which(lat.sa == 30), which(lat.sa == 33)))))
    
    puntos.EP_4 = array(EP.sa[seq(which(lon.sa == 119), which(lon.sa == 126)), seq(which(lat.sa == 33), which(lat.sa == 36)) ,]
                        , dim = c(length(seq(which(lon.sa == 119), which(lon.sa == 126)))*length(seq(which(lat.sa == 33), which(lat.sa == 36)))))
    
    
    puntos.EPP_1 = array(EPP.sa[seq(which(lon.sa == 119), which(lon.sa == 123)), seq(which(lat.sa == 24), which(lat.sa == 27)) ,]
                         , dim = c(length(seq(which(lon.sa == 119), which(lon.sa == 123)))*length(seq(which(lat.sa == 24), which(lat.sa == 27)))))
    
    puntos.EPP_2 = array(EPP.sa[seq(which(lon.sa == 119), which(lon.sa == 123)), seq(which(lat.sa == 27), which(lat.sa == 30)) ,]
                         , dim = c(length(seq(which(lon.sa == 119), which(lon.sa == 123)))*length(seq(which(lat.sa == 27), which(lat.sa == 30)))))
    
    puntos.EPP_3 = array(EPP.sa[seq(which(lon.sa == 119), which(lon.sa == 126)), seq(which(lat.sa == 30), which(lat.sa == 33)) ,]
                         , dim = c(length(seq(which(lon.sa == 119), which(lon.sa == 126)))*length(seq(which(lat.sa == 30), which(lat.sa == 33)))))
    
    puntos.EPP_4 = array(EPP.sa[seq(which(lon.sa == 119), which(lon.sa == 126)), seq(which(lat.sa == 33), which(lat.sa == 36)) ,]
                         , dim = c(length(seq(which(lon.sa == 119), which(lon.sa == 126)))*length(seq(which(lat.sa == 33), which(lat.sa == 36)))))
    
    datos = matrix(NA, nrow = 20, ncol = 3)
    datos[,1] = puntos.EPP_1; datos[,2] = puntos.EP_1; datos[,3] = 1
    
    datos2 = matrix(NA, nrow = 20, ncol = 3)
    datos2[,1] = puntos.EPP_2; datos2[,2] = puntos.EP_2; datos2[,3] = 2
    
    datos3 = matrix(NA, nrow = 32, ncol = 3)
    datos3[,1] = puntos.EPP_3; datos3[,2] = puntos.EP_3; datos3[,3] = 3
    
    datos4 = matrix(NA, nrow = 32, ncol = 3)
    datos4[,1] = puntos.EPP_4; datos4[,2] = puntos.EP_4; datos4[,3] = 4
    
    puntos = as.data.frame(rbind(datos, datos2, datos3, datos4))
    
    colnames(puntos)= c("x", "y", "zona")
    
    puntos$zona = as.factor(puntos$zona)
  
    dataline = matrix(data = NA, nrow = 3, ncol = 2)
    dataline[,1] = c(0,1,3)
    dataline[,2] = c(0,1,1)
    dataline = as.data.frame(dataline)
    colnames(dataline) = c("XX", "YY")
    
    g = ggplot(data = puntos, mapping = aes(x = x, y = y)) + theme_minimal()+
      geom_point(aes(color = zona),show.legend = F, size = 3)+
      geom_line(data = dataline, aes(x = XX, y = YY))+
      geom_line(data = curva, aes(y = omega1, x = x), color = "grey2")+
      geom_line(data = curva, aes(y = omega2, x = x), color = "tomato3")+
      
      
      scale_x_continuous(limits = c(0,3), breaks = seq(0,3,by = 0.25), name = "EP/P")+
      scale_y_continuous(limits = c(0,1.5),breaks = seq(0,1.5,by = 0.25), name = "E/P")+
      scale_color_manual(values = c("steelblue3", "seagreen3", "sandybrown", "tomato2"))+
      
      ggtitle(paste(titulo, sep = ""))+
      theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
            axis.title.x  = element_text(size = 14),
            panel.border = element_rect(colour = "black", fill = NA, size = 3),
            panel.ontop = F,
            plot.title = element_text(hjust = 0.5))
    
    ggsave(paste(getwd(), "/Salidas/TP5/2daparte/", nombre,".jpg",sep =""), plot = g, width = 20, height = 15  , units = "cm")
    
      
    
  }
}

rm(list = ls())
.rs.restartR()

