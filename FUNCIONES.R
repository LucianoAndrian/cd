#### OPEN_NC ####
# abrir NCs en un array
open_nc = function(file_pattern, model, variable, mes_anual){
  
  # queda basica dado lo irregular de los patrones de los archivos (mas en cnrm-cm5)
  
  library(ncdf4)
  model = toupper(model)
  
  
  
  ruta = paste('/home/auri/Facultad/Materias/c-dinamica/TPs/', model, "/",sep = "")
  
  t = Sys.glob(paste(ruta, file_pattern, sep = ""))
  
  r = length(t)
  
  if(mes_anual == "anual"){
    V = array(data = NA, dim = c(144, 73, 30, r))  
    
    for(i in 1:r){
      
      v_nc = nc_open(t[i])
      
      V[,,,i] = ncvar_get(v_nc, variable)
      
      nc_close(v_nc)
    }             
    
    W = list()
    W[[1]] = V
  
    return(W)
    
  } else {
    
    if(r > 10){
      r  = 10 # los historical que tiene r30 los RData pesan mas de 1gb... 
    }
    
    V =  array(data = NA, dim = c(144, 73, 4, r)) # --> quedan los miembros. (se puede hacer el ensamble en el futuro, operando sobre V (o cambiar todo..))
    V2 = array(data = NA, dim = c(144, 73, 4, 29, r))  ## --> por si es necesario
    V3 = array(data = NA, dim = c(144, 73, 360, r))
    for(m in 1:r){
      
      v_nc = nc_open(t[m])
      v_v = ncvar_get(v_nc, variable)
      nc_close(v_nc)
      v = v_v[,, 3:350] 
      V3[,,,m] = v_v
      v_anu_3_2 = array(NA, dim = c(144, 73, 29, 12)) 
      
      # años de marzo a feb
      for(j in 1:12){
        for (i in 0:28){
          v_anu_3_2[,,1+i,j] = v[ , , j+12*i]
        }
      }
      
      # Separa promediando Estaciones en orden MAM, JJA, SON, DJF
      
      v_estaciones = array(NA, dim = c(144, 73, 4, 29))
      i=1
      while(i<=4){
        v_estaciones[,,i,] = apply(v_anu_3_2[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean)
        
        i = i + 1
      }
      V2[,,,,m] = v_estaciones # <--- ##
     
      v_estaciones_prom = array(NA, dim = c(144, 73, 4))
      
      for( i in 1:4){
        v_estaciones_prom[,,i] = apply(v_estaciones[,,i,], c(1,2), mean)
      }
      
      V[,,,m] =   v_estaciones_prom 
      print(m)
    }
    
    W = list()
    W[[1]] = V
    W[[2]] = V2
    W[[3]] = V3
    return(W)

    
  }
  
  
}
    


#### MAPA ####
mapa = function(lista, titulo, nombre_fig, escala, label_escala, resta, brewer, revert, niveles, contour, lon, lat, escala_dis, breaks_c_f, r, na_fill, salida){
  
  library(ncdf4)
  library(maps)
  library(ncdf4)
  require(fields)
  require(mapdata)
  library(ggplot2)
  library(RColorBrewer)
  library(mapproj)
  library(metR)
  ruta = getwd()
  
  #topo = metR::GetTopography(lon.west = 0.5, lon.east = 357.5, lat.north = 90,  lat.south = -90,  resolution = 1/res) # mapa topografia
  #topo2 = topo #
  #topo2[which(topo2$h<altura)]=NA
  
  
  
  num = seq(1, r, by = 1)
  
  g = list()
  for(i in 1:r){
    value = array(lista[,,i], dim = length(lon)*length(lat))
    data = matrix(data = NA, nrow = length(lon)*length(lat), ncol = 3)
    
    l=0
    while(l<length(lon)*length(lat)){
      data[seq(l:l+length(lon)),1]<-lon
      l=l+length(lon)
    }
    
    for(j in 1:length(lat)){
      lat_v = array(lat[j],dim=length(lon))
      data[(length(lon)*j-(length(lon)-1)):(j*length(lon)),2]<-lat_v
    } 
    
    
    data[,3]<-value -resta
    
    data<-as.data.frame(data)
    
    colnames(data)<-c("lon", "lat", "temp")
    
    
    mapa <- map_data("world2", colour = "black")
    
    title = ifelse(test = r>1, yes = paste(titulo, " r" , num[i], sep = ""), no = titulo)
    
    if(revert == "si"){
      if(contour == "si"){
        #g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
        g = ggplot() + theme_minimal()+
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
          
          geom_tile(data = data, aes(x = lon, y = lat, fill = temp), alpha=0.8, na.rm = T) +
          
          geom_contour_fill(data = data, aes(x = lon, y = lat, z = temp),alpha = 1, na.fill = na_fill , breaks = breaks_c_f) +
          
          #geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
          
          scale_fill_stepsn(limits = escala, name = label_escala, colours = rev(brewer.pal(n=niveles,brewer)), na.value = "white", breaks = escala_dis,
                            guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
          
          
          #scale_fill_gradientn(limits = escala, name = label_escala, colours = rev(brewer.pal(n=niveles,brewer)), na.value = "white", guide = "legend", breaks = escala_dis) +
          
          
          #guides(fill = guide_legend(reverse = TRUE)) +
          
          geom_polygon(data = mapa, aes(x = long,y = lat, group =group),fill = NA, color = "black") +
          
          ggtitle(title)+
          scale_x_discrete(limits = seq(0,360, by = 60))+
          scale_y_discrete(limits = seq(-90, 90, by = 30))+
          theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill = NA, size = 3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust = 0.5))
        
        
        ggsave(paste(ruta, salida, nombre_fig, num[i], ".jpg", sep = ""), plot = g, width = 30, height = 15  , units = "cm")
      } else {
        g = ggplot() + theme_minimal() +
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank())+
          
          geom_tile(data=data,aes(x = lon, y = lat,fill = temp),alpha=0.9, na.rm = T) +
          
          #geom_contour_fill(data=data, aes(x = lon, y= lat, z = temp),alpha=1, na.fill = -10000, breaks = breaks_c_f)
          
          scale_fill_stepsn(limits = escala, name = label_escala, colours = rev(brewer.pal(n=niveles,brewer)), na.value = "white", breaks = escala_dis,
                            guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
          
          geom_polygon(data = mapa, aes(x = long,y = lat, group =group),fill = NA, color = "black") +
          
          ggtitle(title)+
          scale_x_discrete(limits = seq(0,360, by = 60))+
          scale_y_discrete(limits = seq(-90, 90, by = 30))+
          theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill = NA, size = 3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust = 0.5))
        
        ggsave(paste(ruta, salida, nombre_fig, num[i], ".jpg", sep = ""), plot = g, width = 30, height = 15  , units = "cm")
        
      }
      
    } else {
      if(contour == "si"){
        
        g = ggplot() + theme_minimal() +
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
          
          geom_tile(data = data,aes(x = lon, y = lat, fill = temp), alpha = 0.9, na.rm = T) +
          
          geom_contour_fill(data = data, aes(x = lon, y = lat, z = temp), alpha = 1, na.fill = na_fill, breaks = breaks_c_f) +
          #geom_contour(data = data, aes(x = lon, y = lat, z = temp), breaks = c(5), color = "white" )+
          
          #stat_subset(data=data, aes(x = lon, y = lat, z = temp, subset = temp <= rc), shape = 20, size = 1, color = "black", alpha = 0.3, geom = "point")+
          #geom_contour(data = data, aes(x = lon, y = lat, z = temp), color = "blue", size = 0.666, breaks = rc )+
          
          scale_fill_stepsn(limits = escala, name = label_escala, colours = brewer.pal(n=niveles,brewer), na.value = "white", breaks = escala_dis,
                            guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
          
          geom_polygon(data = mapa, aes(x = long,y = lat, group =group),fill = NA, color = "black") +
          
          ggtitle(title)+
          scale_x_discrete(limits = seq(0,360, by = 60))+
          scale_y_discrete(limits = seq(-90, 90, by = 30))+
          theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill = NA, size = 3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust = 0.5))
        
        
        ggsave(paste(ruta, salida, nombre_fig, num[i], ".jpg", sep = ""), plot = g, width = 30, height = 15  , units = "cm")
      } else {
        g = ggplot() + theme_minimal() +
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
          
          geom_tile(data = data,aes(x = lon, y = lat,fill = temp),alpha = 0.9, na.rm = T) +
          
          #geom_contour_fill(data=data,aes(x = lon, y= lat, z = temp),alpha=1, na.fill = -10000)+
          
          scale_fill_stepsn(limits = escala, name = label_escala, colours = brewer.pal(n=niveles,brewer), na.value = "white", breaks = escala_dis,
                            guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
          
          geom_polygon(data = mapa, aes(x = long,y = lat, group =group),fill = NA, color = "black") +
          
          ggtitle(title)+
          scale_x_discrete(limits = seq(0,360, by = 60))+
          scale_y_discrete(limits = seq(-90, 90, by = 30))+
          theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill = NA, size = 3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust = 0.5))
        
        
        ggsave(paste(ruta, salida, nombre_fig, num[i], ".jpg", sep = ""), plot = g, width = 30, height = 15  , units = "cm")
        
      }
      
    }
  }
  
}







  
  

#### CORR ####
corr = function(mod, obs, lon, lat, cf){
  corr =  array(data = NA, dim = c(lon, lat,2))
  a = 1- cf
  for(i in 1:lon){
    for(j in 1:lat){
      
      # si cada serie [i,j,] no tiene una cantidad de al menos 3 valores no NA, cor.test da error y corta el ciclo
      if(is.na(obs[i,j,1])){
        aux = obs[i,j,]
        l_NA = length(aux[which(is.na(aux))]) 
        corr[i,j,2] = NA
        
      } else {
        
        l = cor.test(mod[i,j,], obs[i,j,], method = "pearson", conf.level = cf, alternative = "two.sided")
        
        corr[i,j,1] = l$estimate
        
        # esto con lo de arriba deberia no hacer falta, pero en caso de que dos valores sean iguales el resultado de p.value es NA
        # seria muy raro que pase.
        if(is.na(l$p.value)){
          
          corr[i,j,2] = NA
          
        } else if(l$p.value < a){
          
          corr[i,j,2] = 1
        }
        
      }
      
    }
  }
  
  return(corr)
  
}


corr2.0 = function(mod, obs, lon, lat, cf){
  corr =  array(data = NA, dim = c(lon, lat,2))
  a = 1- cf
  for(i in 1:lon){
    for(j in 1:lat){
      
      # si cada serie [i,j,] no tiene una cantidad de al menos 3 valores no NA, cor.test da error y corta el ciclo
      aux = obs[i,j,]
      l_NA = length(aux[which(is.na(aux))]) 
      if(l_NA>3){
        
        corr[i,j,2] = NA
        
      } else {
        
        l = cor.test(mod[i,j,], obs[i,j,], method = "pearson", conf.level = cf, alternative = "two.sided")
        
        corr[i,j,1] = l$estimate
        
        # esto con lo de arriba deberia no hacer falta, pero en caso de que dos valores sean iguales el resultado de p.value es NA
        # seria muy raro que pase.
        if(is.na(l$p.value)){
          
          corr[i,j,2] = NA
          
        } else if(l$p.value < a){
          
          corr[i,j,2] = 1
        }
        
      }
      
    }
  }
  
  return(corr)
  
}
#### MAPA_SA ####
mapa_reg = function(lista, lista2, lista3, titulo, nombre_fig, escala, label_escala, resta, revert, brewer, niveles,  lons, lats, aux_lons, aux_lats, escala_dis, r, na_fill, salida){
  
  # por ahora atada con alambre
  
  # grafica subregiones (lista, lista2....)
  # requiere las lat y lon de esas regiones dadas en lista
  # para agregar mas regiones, por ahora hay q editar la funcion.
  
  # dependiendo de como se haya seleccionado las region previo a la funcion, puede haber problemas en la conversion a data frame para el ggplot
  
  library(ncdf4)
  library(maps)
  library(ncdf4)
  require(fields)
  require(mapdata)
  library(ggplot2)
  library(RColorBrewer)
  library(mapproj)
  library(metR)
  ruta = getwd()
  
 
  lon = lons[aux_lons[[1]]]
  lat = lats[aux_lats[[1]]]
  
  lon2 = lons[aux_lons[[2]]]
  lat2 = lats[aux_lats[[2]]]
  
  lon3 = lons[aux_lons[[3]]]
  lat3 = lats[aux_lats[[3]]]
  
  num = seq(1, r, by = 1)
  
  g = list()
  for(i in 1:r){
    
    # esto para las todas las listas.. a lo loco (se puede definir una lista para cada data frame)

    value = array(lista[,,i], dim = length(lon)*length(lat))
    data = matrix(data = NA, nrow = length(lon)*length(lat), ncol = 3)
    
    l=0
    while(l<length(lon)*length(lat)){
      data[seq(l:l+length(lon)),1]<-lon
      l=l+length(lon)
    }
    
    for(j in 1:length(lat)){
      lat_v = array(lat[j],dim=length(lon))
      data[(length(lon)*j-(length(lon)-1)):(j*length(lon)),2]<-lat_v
    } 
    
    
    data[,3]<-value -resta
    
    data<-as.data.frame(data)
    colnames(data)<-c("lon", "lat", "temp")
    
    
    ### lista2 ###
    value = array(lista2[,,i], dim = length(lon2)*length(lat2))
    data2 = matrix(data = NA, nrow = length(lon2)*length(lat2), ncol = 3)
    
    l=0
    while(l<length(lon2)*length(lat2)){
      data2[seq(l:l+length(lon2)),1]<-lon2
      l=l+length(lon2)
    }
    
    for(j in 1:length(lat2)){
      lat2_v = array(lat2[j],dim=length(lon2))
      data2[(length(lon2)*j-(length(lon2)-1)):(j*length(lon2)),2]<-lat2_v
    } 
    
    
    data2[,3]<-value -resta
    
    data2<-as.data.frame(data2)
    
    colnames(data2)<-c("lon", "lat", "temp")
    
   
    
    
    ### lista3 ###
    value = array(lista3[,,i], dim = length(lon3)*length(lat3))
    data3 = matrix(data = NA, nrow = length(lon3)*length(lat3), ncol = 3)
    
    l=0
    while(l<length(lon3)*length(lat3)){
      data3[seq(l:l+length(lon3)),1]<-lon3
      l=l+length(lon3)
    }
    
    for(j in 1:length(lat3)){
      lat3_v = array(lat3[j],dim=length(lon3))
      data3[(length(lon3)*j-(length(lon3)-1)):(j*length(lon3)),2]<-lat3_v
    } 
    
    
    data3[,3]<-value -resta
    
    data3<-as.data.frame(data3)
    
    colnames(data3)<-c("lon", "lat", "temp")
    
    
    mapa <- map_data("world", regions = c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname", "Colombia", "Venezuela",
                                          "Bolivia", "Ecuador", "Chile", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica", "Nicaragua", "falkland islands",
                                          "Martinique", "Kiribati"), 
                     colour = "black")
    
    title = ifelse(test = r>1, yes = paste(titulo, " r" , num[i], sep = ""), no = titulo)
    
    if(revert == "si"){
      
      g = ggplot() + theme_minimal() +
        xlab("Longitud") + ylab("Latitud") + 
        theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank())+
        
        geom_tile(data=data,aes(x = lon , y = lat,fill = temp),alpha=0.9, na.rm = T) +
        geom_tile(data=data2,aes(x = lon , y = lat,fill = temp),alpha=0.9, na.rm = T) +
        geom_tile(data=data3,aes(x = lon , y = lat,fill = temp),alpha=0.9, na.rm = T) +
        
        
        scale_fill_stepsn(limits = escala, name = label_escala, colours = rev(brewer.pal(n=niveles,brewer)), na.value = "white", breaks = escala_dis,
                          guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
        
        geom_polygon(data = mapa, aes(x = long + 360, y = lat, group =group),fill = NA, color = "black") + # + 360  el mapa( o las data's - 360) 
        
        ggtitle(title)+
        scale_x_continuous(limits = c(180,327))+ # seq?
        #scale_y_discrete(limits = seq(-90, 90, by = 30))
        #scale_x_discrete(limits = seq(0,360, by = 60))+
        #scale_y_continuous(limits = seq(-90, 20, by = 30))+
        theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
              axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill = NA, size = 3),
              panel.ontop = TRUE,
              plot.title = element_text(hjust = 0.5))
      
      ggsave(paste(ruta, salida, nombre_fig, num[i], ".jpg", sep = ""), plot = g, width = 25, height = 15  , units = "cm")
      
    } else {
      
      g = ggplot() + theme_minimal() +
        xlab("Longitud") + ylab("Latitud") + 
        theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank())+
        
        geom_tile(data=data,aes(x = lon , y = lat,fill = temp),alpha=0.9, na.rm = T) +
        geom_tile(data=data2,aes(x = lon , y = lat,fill = temp),alpha=0.9, na.rm = T) +
        geom_tile(data=data3,aes(x = lon , y = lat,fill = temp),alpha=0.9, na.rm = T) +
        
        
        scale_fill_stepsn(limits = escala, name = label_escala, colours = brewer.pal(n=niveles,brewer), na.value = "white", breaks = escala_dis,
                          guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
        
        geom_polygon(data = mapa, aes(x = long + 360, y = lat, group =group),fill = NA, color = "black") + # + 360  el mapa( o las data's - 360) 
        
        ggtitle(title)+
        scale_x_continuous(limits = c(180,327))+ # seq?
        #scale_y_discrete(limits = seq(-90, 90, by = 30))
        #scale_x_discrete(limits = seq(0,360, by = 60))+
        #scale_y_continuous(limits = seq(-90, 20, by = 30))+
        theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
              axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill = NA, size = 3),
              panel.ontop = TRUE,
              plot.title = element_text(hjust = 0.5))
      
      ggsave(paste(ruta, salida, nombre_fig, num[i], ".jpg", sep = ""), plot = g, width = 25, height = 15  , units = "cm")
    }

    
  }
  
}


#### GRAFICO_TS ####
grafico_ts = function(data1, data2, data3, data4, nombre_fig, titulo, ylab){
 
  
  # se puede agregar la posicion del legend como parametro si hace falta
    
    aux = matrix(data = NA, nrow = 30, 3)
    
    aux[,1] = data1
    aux[,2] = data2
    aux[,3] = data3
    aux2 = data.frame(1976:2005, aux)
    
    colnames(aux2) = c("Year", "Observado", "CNRM-CM5", "CNRM-CM6") # esto puede no ser necesario.
    
    df.ts = ts(aux2[-1], start = 1976, frequency = 1)
    png(filename = nombre_fig, width = 850, height = 520, units = "px")
    plot(df.ts, plot.type = "single", col = 1:ncol(df.ts), lwd = 4, ylab = ylab, main = titulo)
    abline(h = 0, lwd = 0.5)
    legend("topleft", colnames(df.ts), col=1:ncol(aux2), lty=1, cex=2, lwd = 3)
    dev.off()
    
    

  
}
#### MAPA_TOPO ####
mapa_topo = function(lista, u, v, titulo, nombre_fig, escala, label_escala, resta, brewer, revert, niveles, contour
                     , lon, lat, escala_dis, breaks_c_f, r, na_fill, topo, altura, salida){
  
  library(ncdf4)
  library(maps)
  library(ncdf4)
  require(fields)
  require(mapdata)
  library(ggplot2)
  library(RColorBrewer)
  library(mapproj)
  library(metR)
  ruta = getwd()
  
  load("RDatas/topo_india.RData")
  load("RDatas/topo_sa.RData")
  
  if(topo == "topo1"){
    
    topo2 = topo_india
    
    topo2[which(topo2$h<altura)]=NA
    
    num = seq(1, r, by = 1)
    
    g = list()
    for(i in 1:r){
      
      u = array(u[,,i], dim = length(lon)*length(lat))
      
      v = array(v[,,i], dim = length(lon)*length(lat))
      
      value = array(lista[,,i], dim = length(lon)*length(lat))
      data = matrix(data = NA, nrow = length(lon)*length(lat), ncol = 3)
      
      l=0
      while(l<length(lon)*length(lat)){
        data[seq(l:l+length(lon)),1]<-lon
        l=l+length(lon)
      }
      
      for(j in 1:length(lat)){
        lat_v = array(lat[j],dim=length(lon))
        data[(length(lon)*j-(length(lon)-1)):(j*length(lon)),2]<-lat_v
      } 
      
      
      data[,3]<-value -resta
      
      data<-as.data.frame(data)
      
      data = cbind(data, u)
      data = cbind(data, v)
      
      colnames(data)<-c("lon", "lat", "temp", "u", "v")
      
  
        
        mapa <- map_data("world2", region = c("India", "Sri Lanka", "Bangladesh", "Nepal", "Bhutan", "Pakistan"
                                                 ,"Oman", "Yemen", "Somalia", "Eriopia", "Birmania"
                                                 , "Malasya", "United Arab Emirates", "Singapur", "Myanmar", "Iran", 
                                                 "Turkmenistan", "Afghanistan", "Tajikistan", "Uzbekistan", "Kyrgyzstan", "China", "Mongolia", 
                                                 "Bangladesh", "North Korea", "South Korea",  "Taiwan", "Laos", "Thailand", "Vietnam", "Cambodia", 
                                                 "Malasya", "Indonesia", "Philippines"), colour = "black")
        
     
      title = ifelse(test = r>1, yes = paste(titulo, " r" , num[i], sep = ""), no = titulo)
      
      if(revert == "si"){
        if(contour == "si"){
          g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
            #g = ggplot() + theme_minimal()+
            xlab("Longitud") + ylab("Latitud") + 
            theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
            
            geom_tile(data = data, aes(x = lon, y = lat, fill = temp), alpha=0.8, na.rm = T) +
            
            geom_contour_fill(data = data, aes(x = lon, y = lat, z = temp),alpha = 1, na.fill = na_fill , breaks = breaks_c_f) +
            
            geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
            
            geom_arrow(data = data, aes(x = lon, y = lat, dx = u*2, dy = v*2), color = "black", skip.x = 0, skip.y = 0, arrow.length = 0.5, na.rm = T, show.legend = F)+
            
            scale_fill_stepsn(limits = escala, name = label_escala, colours = rev(brewer.pal(n=niveles,brewer)), na.value = "white", breaks = escala_dis,
                              guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +

           
            
            geom_polygon(data = mapa, aes(x = long ,y = lat, group =group),fill = NA, color = "black") +
            
            ggtitle(title)+
            scale_x_longitude(breaks = seq(40, 140, by = 10), name = "longitud")+
            scale_y_latitude(breaks = seq(-10, 55, by = 10), name = "Latitud")+
            theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                  axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                  panel.border = element_rect(colour = "black", fill = NA, size = 3),
                  panel.ontop = TRUE,
                  plot.title = element_text(hjust = 0.5)) + geom_hline(yintercept = 0, color = "black")
          
          
          ggsave(paste(ruta, salida, nombre_fig, num[i], ".jpg", sep = ""), plot = g, width = 30, height = 20  , units = "cm")
          
        } else {
          g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
            #g = ggplot() + theme_minimal() +
            xlab("Longitud") + ylab("Latitud") + 
            theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank())+
            
            geom_tile(data=data,aes(x = lon, y = lat,fill = temp),alpha=0.9, na.rm = T) +
            
            geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
            
            geom_arrow(data = data, aes(x = lon, y = lat, dx = u*2, dy = v*2), color = "black", skip.x = 0, skip.y = 0, arrow.length = 0.5, na.rm = T, show.legend = F)+
            
            scale_fill_stepsn(limits = escala, name = label_escala, colours = rev(brewer.pal(n=niveles,brewer)), na.value = "white", breaks = escala_dis,
                              guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
            
           
            geom_polygon(data = mapa, aes(x = long,y = lat, group =group),fill = NA, color = "black") +
            
            ggtitle(title)+
            scale_x_longitude(breaks = seq(40, 140, by = 10), name = "longitud")+
            scale_y_latitude(breaks = seq(-10, 55, by = 10), name = "Latitud")+
            theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                  axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                  panel.border = element_rect(colour = "black", fill = NA, size = 3),
                  panel.ontop = TRUE,
                  plot.title = element_text(hjust = 0.5))+ geom_hline(yintercept = 0, color = "black")
          
          ggsave(paste(ruta, salida, nombre_fig, num[i], ".jpg", sep = ""), plot = g, width = 30, height = 20  , units = "cm")
          
        }
        
      } else {
        if(contour == "si"){
          g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
            #g = ggplot() + theme_minimal() +
            xlab("Longitud") + ylab("Latitud") + 
            theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
            
            geom_tile(data = data,aes(x = lon, y = lat, fill = temp), alpha = 0.9, na.rm = T) +
            
            geom_contour_fill(data = data, aes(x = lon, y = lat, z = temp), alpha = 1, na.fill = na_fill, breaks = breaks_c_f) +
          
            geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
            
            geom_arrow(data = data, aes(x = lon, y = lat, dx = u*2, dy = v*2), color = "black", skip.x = 0, skip.y = 0, arrow.length = 0.5, na.rm = T, show.legend = F)+
            
            scale_fill_stepsn(limits = escala, name = label_escala, colours = brewer.pal(n=niveles,brewer), na.value = "white", breaks = escala_dis,
                              guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
            
           
            geom_polygon(data = mapa, aes(x = long,y = lat, group =group),fill = NA, color = "black") +
            
            ggtitle(title)+
            scale_x_longitude(breaks = seq(40, 140, by = 10), name = "longitud")+
            scale_y_latitude(breaks = seq(-10, 55, by = 10), name = "Latitud")+
            theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                  axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                  panel.border = element_rect(colour = "black", fill = NA, size = 3),
                  panel.ontop = TRUE,
                  plot.title = element_text(hjust = 0.5)) + geom_hline(yintercept = 0, color = "black")
          
          
          ggsave(paste(ruta, salida, nombre_fig, num[i], ".jpg", sep = ""), plot = g, width = 30, height = 20  , units = "cm")
          
        } else {
          g = ggplot() + theme_minimal() +
            xlab("Longitud") + ylab("Latitud") + 
            theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
            
            geom_tile(data = data,aes(x = lon, y = lat,fill = temp),alpha = 0.9, na.rm = T) +
            
            geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
            
            geom_arrow(data = data, aes(x = lon, y = lat, dx = u*2, dy = v*2), color = "black", skip.x = 0, skip.y = 0, arrow.length = 0.5, na.rm = T, show.legend = F)+
            
            scale_fill_stepsn(limits = escala, name = label_escala, colours = brewer.pal(n=niveles,brewer), na.value = "white", breaks = escala_dis,
                              guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
            
            
            geom_polygon(data = mapa, aes(x = long,y = lat, group =group),fill = NA, color = "black") +
            
            ggtitle(title)+
            scale_x_longitude(breaks = seq(40, 140, by = 10), name = "longitud")+
            scale_y_latitude(breaks = seq(-10, 55, by = 10), name = "Latitud")+
            theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                  axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                  panel.border = element_rect(colour = "black", fill = NA, size = 3),
                  panel.ontop = TRUE,
                  plot.title = element_text(hjust = 0.5)) + geom_hline(yintercept = 0, color = "black")
          
          
          ggsave(paste(ruta, salida, nombre_fig, num[i], ".jpg", sep = ""), plot = g, width = 30, height = 20  , units = "cm")
          
        }
        
      }
    }
    
  } else {
    topo2 = topo_sa
    
    topo2[which(topo2$h<altura)]=NA
    
    num = seq(1, r, by = 1)
    
    g = list()
    for(i in 1:r){
      
      u = array(u[,,i], dim = length(lon)*length(lat))
      
      v = array(v[,,i], dim = length(lon)*length(lat))
      
      value = array(lista[,,i], dim = length(lon)*length(lat))
      data = matrix(data = NA, nrow = length(lon)*length(lat), ncol = 3)
      
      l=0
      while(l<length(lon)*length(lat)){
        data[seq(l:l+length(lon)),1]<-lon
        l=l+length(lon)
      }
      
      for(j in 1:length(lat)){
        lat_v = array(lat[j],dim=length(lon))
        data[(length(lon)*j-(length(lon)-1)):(j*length(lon)),2]<-lat_v
      } 
      
      
      data[,3]<-value -resta
      
      data<-as.data.frame(data)
      data = cbind(data, u)
      data = cbind(data, v)
      
      colnames(data)<-c("lon", "lat", "temp", "u", "v")
      
        
      mapa <- map_data("world2", region = c("Brazil", "French Guiana", "Suriname", "Colombia", "Venezuela","Argentina", "Chile", "Uruguay",
                                            "Bolivia", "Ecuador", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica", "Nicaragua",
                                            "Martinique"), colour = "black")
      
      
      title = ifelse(test = r>1, yes = paste(titulo, " r" , num[i], sep = ""), no = titulo)
      
      if(revert == "si"){
        if(contour == "si"){
          g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
            #g = ggplot() + theme_minimal()+
            xlab("Longitud") + ylab("Latitud") + 
            theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
            
            geom_tile(data = data, aes(x = lon, y = lat, fill = temp), alpha=0.8, na.rm = T) +
            
            geom_contour_fill(data = data, aes(x = lon, y = lat, z = temp),alpha = 1, na.fill = na_fill , breaks = breaks_c_f) +
            
            geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
            
            geom_arrow(data = data, aes(x = lon, y = lat, dx = u*2, dy = v*2), color = "black", skip.x = 0, skip.y = 0, arrow.length = 0.5, na.rm = T, show.legend = F)+
            
            scale_fill_stepsn(limits = escala, name = label_escala, colours = rev(brewer.pal(n=niveles,brewer)), na.value = "white", breaks = escala_dis,
                              guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
            
            geom_hline(yintercept = 0, color = "black")+
            geom_polygon(data = mapa, aes(x = long ,y = lat, group =group),fill = NA, color = "black") +
            
            ggtitle(title)+
            scale_x_longitude(breaks = seq(250, 350, by = 10), name = "longitud")+
            scale_y_latitude(breaks = seq(-60, 20, by = 10), name = "Latitud")+
            theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                  axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                  panel.border = element_rect(colour = "black", fill = NA, size = 3),
                  panel.ontop = TRUE,
                  plot.title = element_text(hjust = 0.5))
          
          
          ggsave(paste(ruta, salida, nombre_fig, num[i], ".jpg", sep = ""), plot = g, width = 25, height = 20  , units = "cm")
          
        } else {
          g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
            #g = ggplot() + theme_minimal() +
            xlab("Longitud") + ylab("Latitud") + 
            theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank())+
            
            geom_tile(data=data,aes(x = lon, y = lat,fill = temp),alpha=0.9, na.rm = T) +
        
            geom_arrow(data = data, aes(x = lon, y = lat, dx = u*2, dy = v*2), color = "black", skip.x = 0, skip.y = 0, arrow.length = 0.5, na.rm = T, show.legend = F)+
            
            geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
            
            scale_fill_stepsn(limits = escala, name = label_escala, colours = rev(brewer.pal(n=niveles,brewer)), na.value = "white", breaks = escala_dis,
                              guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
            
            geom_hline(yintercept = 0, color = "black")+
            geom_polygon(data = mapa, aes(x = long,y = lat, group =group),fill = NA, color = "black") +
            
            ggtitle(title)+
            scale_x_longitude(breaks = seq(250, 350, by = 10), name = "longitud")+
            scale_y_latitude(breaks = seq(-60, 20, by = 10), name = "Latitud")+
            theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                  axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                  panel.border = element_rect(colour = "black", fill = NA, size = 3),
                  panel.ontop = TRUE,
                  plot.title = element_text(hjust = 0.5))
          
          ggsave(paste(ruta, salida, nombre_fig, num[i], ".jpg", sep = ""), plot = g, width = 25, height = 20  , units = "cm")
          
        }
        
      } else {
        if(contour == "si"){
          g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
            #g = ggplot() + theme_minimal() +
            xlab("Longitud") + ylab("Latitud") + 
            theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
            
            geom_tile(data = data,aes(x = lon, y = lat, fill = temp), alpha = 0.9, na.rm = T) +
            
            geom_contour_fill(data = data, aes(x = lon, y = lat, z = temp), alpha = 1, na.fill = na_fill, breaks = breaks_c_f) +
            
            geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
            
            geom_arrow(data = data, aes(x = lon, y = lat, dx = u*2, dy = v*2), color = "black", skip.x = 0, skip.y = 0, arrow.length = 0.5, na.rm = T, show.legend = F)+
            
            scale_fill_stepsn(limits = escala, name = label_escala, colours = brewer.pal(n=niveles,brewer), na.value = "white", breaks = escala_dis,
                              guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
            
            geom_hline(yintercept = 0, color = "black")+
            geom_polygon(data = mapa, aes(x = long,y = lat, group =group),fill = NA, color = "black") +
            
            ggtitle(title)+
            scale_x_longitude(breaks = seq(250, 350, by = 10), name = "longitud")+
            scale_y_latitude(breaks = seq(-60, 20, by = 10), name = "Latitud")+
            theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                  axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                  panel.border = element_rect(colour = "black", fill = NA, size = 3),
                  panel.ontop = TRUE,
                  plot.title = element_text(hjust = 0.5))
          
          
          ggsave(paste(ruta, salida, nombre_fig, num[i], ".jpg", sep = ""), plot = g, width = 25, height = 20  , units = "cm")
        } else {
          g = ggplot() + theme_minimal() +
            xlab("Longitud") + ylab("Latitud") + 
            theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
            
            geom_tile(data = data,aes(x = lon, y = lat,fill = temp),alpha = 0.9, na.rm = T) +
            
            geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
            
            geom_arrow(data = data, aes(x = lon, y = lat, dx = u*2, dy = v*2), color = "black", skip.x = 0, skip.y = 0, arrow.length = 0.5, na.rm = T, show.legend = F)+
            
            scale_fill_stepsn(limits = escala, name = label_escala, colours = brewer.pal(n=niveles,brewer), na.value = "white", breaks = escala_dis,
                              guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
            
            geom_hline(yintercept = 0, color = "black")+
            geom_polygon(data = mapa, aes(x = long,y = lat, group =group),fill = NA, color = "black") +
            
            ggtitle(title)+
            scale_x_longitude(breaks = seq(250, 350, by = 10), name = "longitud")+
            scale_y_latitude(breaks = seq(-60, 20, by = 10), name = "Latitud")+
            theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                  axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                  panel.border = element_rect(colour = "black", fill = NA, size = 3),
                  panel.ontop = TRUE,
                  plot.title = element_text(hjust = 0.5))
          
          
          ggsave(paste(ruta, salida, nombre_fig, num[i], ".jpg", sep = ""), plot = g, width = 25, height = 20  , units = "cm")
          
        }
        
      }
    }
    
  }
  
}
#### MAPA_TOPO2 ####
mapa_topo2 = function(lista, titulo, nombre_fig, escala, label_escala, resta, brewer, revert, niveles, contour
                     , lon, lat, escala_dis, breaks_c_f, r, na_fill, topo, altura, salida){
  
  library(ncdf4)
  library(maps)
  library(ncdf4)
  require(fields)
  require(mapdata)
  library(ggplot2)
  library(RColorBrewer)
  library(mapproj)
  library(metR)
  ruta = getwd()
  
  load("RDatas/topo_india.RData")
  load("RDatas/topo_sa.RData")
  
  if(topo == "topo1"){
    
    topo2 = topo_india
    
    topo2[which(topo2$h<altura)]=NA
    
    num = seq(1, r, by = 1)
    
    g = list()
    for(i in 1:r){
      
      #u = array(u[,,i], dim = length(lon)*length(lat))
      
      #v = array(v[,,i], dim = length(lon)*length(lat))
      
      value = array(lista[,,i], dim = length(lon)*length(lat))
      data = matrix(data = NA, nrow = length(lon)*length(lat), ncol = 3)
      
      l=0
      while(l<length(lon)*length(lat)){
        data[seq(l:l+length(lon)),1]<-lon
        l=l+length(lon)
      }
      
      for(j in 1:length(lat)){
        lat_v = array(lat[j],dim=length(lon))
        data[(length(lon)*j-(length(lon)-1)):(j*length(lon)),2]<-lat_v
      } 
      
      
      data[,3]<-value -resta
      
      data<-as.data.frame(data)
      
      #data = cbind(data, u)
      #data = cbind(data, v)
      
      #colnames(data)<-c("lon", "lat", "temp", "u", "v")
      colnames(data)<-c("lon", "lat", "temp")
      
      
      mapa <- map_data("world2", region = c("India", "Sri Lanka", "Bangladesh", "Nepal", "Bhutan", "Pakistan"
                                            ,"Oman", "Yemen", "Somalia", "Eriopia", "Birmania"
                                            , "Malasya", "United Arab Emirates", "Singapur", "Myanmar", "Iran", 
                                            "Turkmenistan", "Afghanistan", "Tajikistan", "Uzbekistan", "Kyrgyzstan", "China", "Mongolia", 
                                            "Bangladesh", "North Korea", "South Korea", "Taiwan", "Laos", "Thailand", "Vietnam", "Cambodia", 
                                            "Malasya", "Indonesia", "Philippines"), colour = "black")
      
      title = ifelse(test = r>1, yes = paste(titulo, " r" , num[i], sep = ""), no = titulo)
      
      if(revert == "si"){
        if(contour == "si"){
          g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
            #g = ggplot() + theme_minimal()+
            xlab("Longitud") + ylab("Latitud") + 
            theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
            
            geom_tile(data = data, aes(x = lon, y = lat, fill = temp), alpha=0.8, na.rm = T) +
            
            geom_contour_fill(data = data, aes(x = lon, y = lat, z = temp),alpha = 1, na.fill = na_fill , breaks = breaks_c_f) +
            
            geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
            
            #geom_arrow(data = data, aes(x = lon, y = lat, dx = u*2, dy = v*2), color = "black", skip.x = 0, skip.y = 0, arrow.length = 0.5, na.rm = T, show.legend = F)+
            
            scale_fill_stepsn(limits = escala, name = label_escala, colours = rev(brewer.pal(n=niveles,brewer)), na.value = "white", breaks = escala_dis,
                              guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
            
            
            
            geom_polygon(data = mapa, aes(x = long ,y = lat, group =group),fill = NA, color = "black") +
            
            ggtitle(title)+
            scale_x_longitude(breaks = seq(40, 140, by = 10), name = "longitud")+
            scale_y_latitude(breaks = seq(-10, 55, by = 10), name = "Latitud")+
            theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                  axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                  panel.border = element_rect(colour = "black", fill = NA, size = 3),
                  panel.ontop = TRUE,
                  plot.title = element_text(hjust = 0.5)) + geom_hline(yintercept = 0, color = "black")
          
          
          ggsave(paste(ruta, salida, nombre_fig, num[i], ".jpg", sep = ""), plot = g, width = 30, height = 20  , units = "cm")
          
        } else {
          g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
            #g = ggplot() + theme_minimal() +
            xlab("Longitud") + ylab("Latitud") + 
            theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank())+
            
            geom_tile(data=data,aes(x = lon, y = lat,fill = temp),alpha=0.9, na.rm = T) +
            
            geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
            
            #geom_arrow(data = data, aes(x = lon, y = lat, dx = u*2, dy = v*2), color = "black", skip.x = 0, skip.y = 0, arrow.length = 0.5, na.rm = T, show.legend = F)+
            
            scale_fill_stepsn(limits = escala, name = label_escala, colours = rev(brewer.pal(n=niveles,brewer)), na.value = "white", breaks = escala_dis,
                              guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
            
            geom_hline(yintercept = 0, color = "black")+
            geom_polygon(data = mapa, aes(x = long,y = lat, group =group),fill = NA, color = "black") +
            
            ggtitle(title)+
            scale_x_longitude(breaks = seq(40, 140, by = 10), name = "longitud")+
            scale_y_latitude(breaks = seq(-10, 55, by = 10), name = "Latitud")+
            theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                  axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                  panel.border = element_rect(colour = "black", fill = NA, size = 3),
                  panel.ontop = TRUE,
                  plot.title = element_text(hjust = 0.5))
          
          ggsave(paste(ruta, salida, nombre_fig, num[i], ".jpg", sep = ""), plot = g, width = 30, height = 20  , units = "cm")
          
        }
        
      } else {
        if(contour == "si"){
          g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
            #g = ggplot() + theme_minimal() +
            xlab("Longitud") + ylab("Latitud") + 
            theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
            
            geom_tile(data = data,aes(x = lon, y = lat, fill = temp), alpha = 0.9, na.rm = T) +
            
            geom_contour_fill(data = data, aes(x = lon, y = lat, z = temp), alpha = 1, na.fill = na_fill, breaks = breaks_c_f) +
            
            geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
            
            #geom_arrow(data = data, aes(x = lon, y = lat, dx = u*2, dy = v*2), color = "black", skip.x = 0, skip.y = 0, arrow.length = 0.5, na.rm = T, show.legend = F)+
            
            scale_fill_stepsn(limits = escala, name = label_escala, colours = brewer.pal(n=niveles,brewer), na.value = "white", breaks = escala_dis,
                              guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
            
           
            geom_polygon(data = mapa, aes(x = long,y = lat, group =group),fill = NA, color = "black") +
            
            ggtitle(title)+
            scale_x_longitude(breaks = seq(40, 140, by = 10), name = "longitud")+
            scale_y_latitude(breaks = seq(-10, 55, by = 10), name = "Latitud")+
            theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                  axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                  panel.border = element_rect(colour = "black", fill = NA, size = 3),
                  panel.ontop = TRUE,
                  plot.title = element_text(hjust = 0.5)) + geom_hline(yintercept = 0, color = "black")
          
          
          ggsave(paste(ruta, salida, nombre_fig, num[i], ".jpg", sep = ""), plot = g, width = 30, height = 20  , units = "cm")
          
        } else {
          g = ggplot() + theme_minimal() +
            xlab("Longitud") + ylab("Latitud") + 
            theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
            
            geom_tile(data = data,aes(x = lon, y = lat,fill = temp),alpha = 0.9, na.rm = T) +
            
            geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
            
            #geom_arrow(data = data, aes(x = lon, y = lat, dx = u*2, dy = v*2), color = "black", skip.x = 0, skip.y = 0, arrow.length = 0.5, na.rm = T, show.legend = F)+
            
            scale_fill_stepsn(limits = escala, name = label_escala, colours = brewer.pal(n=niveles,brewer), na.value = "white", breaks = escala_dis,
                              guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
            
           
            geom_polygon(data = mapa, aes(x = long,y = lat, group =group),fill = NA, color = "black") +
            
            ggtitle(title)+
            scale_x_longitude(breaks = seq(40, 140, by = 10), name = "longitud")+
            scale_y_latitude(breaks = seq(-10, 55, by = 10), name = "Latitud")+
            theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                  axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                  panel.border = element_rect(colour = "black", fill = NA, size = 3),
                  panel.ontop = TRUE,
                  plot.title = element_text(hjust = 0.5)) + geom_hline(yintercept = 0, color = "black")
          
          
          ggsave(paste(ruta, salida, nombre_fig, num[i], ".jpg", sep = ""), plot = g, width = 30, height = 20  , units = "cm")
          
        }
        
      }
    }
    
  } else {
    topo2 = topo_sa
    
    topo2[which(topo2$h<altura)]=NA
    
    num = seq(1, r, by = 1)
    
    g = list()
    for(i in 1:r){
      
      #u = array(u[,,i], dim = length(lon)*length(lat))
      
      #v = array(v[,,i], dim = length(lon)*length(lat))
      
      value = array(lista[,,i], dim = length(lon)*length(lat))
      data = matrix(data = NA, nrow = length(lon)*length(lat), ncol = 3)
      
      l=0
      while(l<length(lon)*length(lat)){
        data[seq(l:l+length(lon)),1]<-lon
        l=l+length(lon)
      }
      
      for(j in 1:length(lat)){
        lat_v = array(lat[j],dim=length(lon))
        data[(length(lon)*j-(length(lon)-1)):(j*length(lon)),2]<-lat_v
      } 
      
      
      data[,3]<-value -resta
      
      data<-as.data.frame(data)
      #data = cbind(data, u)
      #data = cbind(data, v)
      
      #colnames(data)<-c("lon", "lat", "temp", "u", "v")
      colnames(data)<-c("lon", "lat", "temp")
      
      mapa <- map_data("world2", region = c("Brazil", "French Guiana", "Suriname", "Colombia", "Venezuela","Argentina", "Chile", "Uruguay",
                                            "Bolivia", "Ecuador", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica", "Nicaragua",
                                            "Martinique"), colour = "black")
      
      
      
      title = ifelse(test = r>1, yes = paste(titulo, " r" , num[i], sep = ""), no = titulo)
      
      if(revert == "si"){
        if(contour == "si"){
          g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
            #g = ggplot() + theme_minimal()+
            xlab("Longitud") + ylab("Latitud") + 
            theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
            
            geom_tile(data = data, aes(x = lon, y = lat, fill = temp), alpha=0.8, na.rm = T) +
            
            geom_contour_fill(data = data, aes(x = lon, y = lat, z = temp),alpha = 1, na.fill = na_fill , breaks = breaks_c_f) +
            
            geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
            
            #geom_arrow(data = data, aes(x = lon, y = lat, dx = u*2, dy = v*2), color = "black", skip.x = 0, skip.y = 0, arrow.length = 0.5, na.rm = T, show.legend = F)+
            
            scale_fill_stepsn(limits = escala, name = label_escala, colours = rev(brewer.pal(n=niveles,brewer)), na.value = "white", breaks = escala_dis,
                              guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
            
            geom_hline(yintercept = 0, color = "black")+
            geom_polygon(data = mapa, aes(x = long ,y = lat, group =group),fill = NA, color = "black") +
            
            ggtitle(title)+
            scale_x_longitude(breaks = seq(250, 350, by = 10), name = "longitud")+
            scale_y_latitude(breaks = seq(-60, 20, by = 10), name = "Latitud")+
            theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                  axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                  panel.border = element_rect(colour = "black", fill = NA, size = 3),
                  panel.ontop = TRUE,
                  plot.title = element_text(hjust = 0.5))
          
          
          ggsave(paste(ruta, salida, nombre_fig, num[i], ".jpg", sep = ""), plot = g, width = 25, height = 20  , units = "cm")
          
        } else {
          g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
            #g = ggplot() + theme_minimal() +
            xlab("Longitud") + ylab("Latitud") + 
            theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank())+
            
            geom_tile(data=data,aes(x = lon, y = lat,fill = temp),alpha=0.9, na.rm = T) +
            
            #geom_arrow(data = data, aes(x = lon, y = lat, dx = u*2, dy = v*2), color = "black", skip.x = 0, skip.y = 0, arrow.length = 0.5, na.rm = T, show.legend = F)+
            
            geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black", show.legend = F) +
            
            scale_fill_stepsn(limits = escala, name = label_escala, colours = rev(brewer.pal(n=niveles,brewer)), na.value = "white", breaks = escala_dis,
                              guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
            
            geom_hline(yintercept = 0, color = "black")+
            geom_polygon(data = mapa, aes(x = long,y = lat, group =group),fill = NA, color = "black") +
            
            ggtitle(title)+
            scale_x_longitude(breaks = seq(250, 350, by = 10), name = "longitud")+
            scale_y_latitude(breaks = seq(-60, 20, by = 10), name = "Latitud")+
            theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                  axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                  panel.border = element_rect(colour = "black", fill = NA, size = 3),
                  panel.ontop = TRUE,
                  plot.title = element_text(hjust = 0.5))
          
          ggsave(paste(ruta, salida, nombre_fig, num[i], ".jpg", sep = ""), plot = g, width = 25, height = 20  , units = "cm")
          
        }
        
      } else {
        if(contour == "si"){
          g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
            #g = ggplot() + theme_minimal() +
            xlab("Longitud") + ylab("Latitud") + 
            theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
            
            geom_tile(data = data,aes(x = lon, y = lat, fill = temp), alpha = 0.9, na.rm = T) +
            
            geom_contour_fill(data = data, aes(x = lon, y = lat, z = temp), alpha = 1, na.fill = na_fill, breaks = breaks_c_f) +
            
            geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
            
            #geom_arrow(data = data, aes(x = lon, y = lat, dx = u*2, dy = v*2), color = "black", skip.x = 0, skip.y = 0, arrow.length = 0.5, na.rm = T, show.legend = F)+
            
            scale_fill_stepsn(limits = escala, name = label_escala, colours = brewer.pal(n=niveles,brewer), na.value = "white", breaks = escala_dis,
                              guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
            
            geom_hline(yintercept = 0, color = "black")+
            geom_polygon(data = mapa, aes(x = long,y = lat, group =group),fill = NA, color = "black") +
            
            ggtitle(title)+
            scale_x_longitude(breaks = seq(250, 350, by = 10), name = "longitud")+
            scale_y_latitude(breaks = seq(-60, 20, by = 10), name = "Latitud")+
            theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                  axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                  panel.border = element_rect(colour = "black", fill = NA, size = 3),
                  panel.ontop = TRUE,
                  plot.title = element_text(hjust = 0.5))
          
          
          ggsave(paste(ruta, salida, nombre_fig, num[i], ".jpg", sep = ""), plot = g, width = 25, height = 20  , units = "cm")
        } else {
          g = ggplot() + theme_minimal() +
            xlab("Longitud") + ylab("Latitud") + 
            theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
            
            geom_tile(data = data,aes(x = lon, y = lat,fill = temp),alpha = 0.9, na.rm = T) +
            
            geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black") +
            
            #geom_arrow(data = data, aes(x = lon, y = lat, dx = u*2, dy = v*2), color = "black", skip.x = 0, skip.y = 0, arrow.length = 0.5, na.rm = T, show.legend = F)+
            
            scale_fill_stepsn(limits = escala, name = label_escala, colours = brewer.pal(n=niveles,brewer), na.value = "white", breaks = escala_dis,
                              guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) +
            
            geom_hline(yintercept = 0, color = "black")+
            geom_polygon(data = mapa, aes(x = long,y = lat, group =group),fill = NA, color = "black") +
            
            ggtitle(title)+
            scale_x_longitude(breaks = seq(250, 350, by = 10), name = "longitud")+
            scale_y_latitude(breaks = seq(-60, 20, by = 10), name = "Latitud")+
            theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                  axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                  panel.border = element_rect(colour = "black", fill = NA, size = 3),
                  panel.ontop = TRUE,
                  plot.title = element_text(hjust = 0.5))
          
          
          ggsave(paste(ruta, salida, nombre_fig, num[i], ".jpg", sep = ""), plot = g, width = 25, height = 20  , units = "cm")
          
        }
        
      }
    }
    
  }
  
}
#### DRAWMONSOON ####
# grafica los campos medios de las variables de los monzones asiaticos y sudamericanos a partir de los arrays de tp2
DrawMonsoon = function(modelo){
  
  source("FUNCIONES.R")
  
  modelo = toupper(modelo)
  
  lon = read.table("lon.txt")[,1]
  lat = read.table("lat.txt")[,1]
  
  if(modelo == "CNRM-CM5"){
    
    load("RDatas/M5.RData")
    aux = M5
    rm(M5)
    zona = c("asia", "sa")
    rcp = c("RCP2.6", "RCP8.5")
    rcp.fig = c("26", "85")
    var = c("Precipitacion media", "Temperatura media")
    var.fig = c("pp5", "t5")
    periodo = c("2020 - 2049",  "2070 - 2099")
    periodo.fig = c("49", "99")
    seasons = c("JJA", "DJF")
    topo = c("asia", "sa")
    
    
    
    for(z in 1:2){
      
      for(i in 1:2){
        
        rcp.n = rcp[i] 
        rcp.f = rcp.fig[i]
        
        for(j in 1:length(aux[1,1,1,,1,1])){  # esto por ahora es innecesario ya que siempre sera 1, pero si se agrega T mensual no hay q modificar nada
          
          v = var[j]
          v.f = var.fig[j]
          
          if(j == 1){
            escala = c(0,700)
            resta = 0
            label_escala = "mm"
            colores = "YlGnBu"
            revert = F
            niveles = 9
            breaks = seq(0,700, by = 50)
          } else {
            escala = c(0,35)
            resta = 273
            label_escala = "ºC"
            colores = "Spectral"
            revert = T
            niveles = 11
            breaks = seq(0, 35, by = 2.5)
          }
          
          for(p in 1:2){
            
            per = periodo[p] 
            p.f = periodo.fig[p]
            areaaux = AreaMonsoon(variable = paste(var.fig[j],".", rcp.fig[i], "_", periodo.fig[p], sep = ""))
            
            if(z == 1){
              seas = seasons[1]
              s = 1
            } else {
              seas = seasons[2]
              s = 2
            }
            
            auxa = array(areaaux[[z]][lons[[z]], lats[[z]]], c(length(lons[[1]]),length(lats[[1]]),1))
            
            aux2 = array(data = aux[lons[[z]], lats[[z]], i, j, p, s], dim = c(length(lons[[z]]), length(lats[[z]]),1))
            
            mapa_topo3(variable = aux2, titulo = paste(v, modelo, rcp.n, per, seas, sep = " "), nombre.fig = paste(v.f, ".", rcp.f, "_", p.f, "_", zona[z], ".", seas, sep = "" )
                       , escala = breaks, label.escala = label_escala, resta = resta, colorbar = colores, revert = revert, niveles = niveles
                       , topo = topo[z], lon = lon[lons[[z]]], lat = lat[lats[[z]]], altura.topo = 1500, salida = "/Salidas/TP2/drawmonsoon/"
                       , contour = T, variable2 = auxa, nivel.v2 = 180, color.v2 = "firebrick", x.label = "Longitud", y.label = "Latitud", width = 30)
            
            
            
          }
        }
      }
      
    }
    
  } else {
    
    
    load("RDatas/M6.RData")
    aux = M6
    load("RDatas/V6.RData")
    auxV = V6
    rm(M6, V6)
    zona = c("asia", "sa")
    rcp = c("SSP1-2.6", "SSP5-8.5")
    rcp.fig = c("26", "85")
    var = c("Precipitacion media", "Temperatura media", "Humedad media")
    var.fig = c("pp6", "t6", "hu6")
    periodo = c("2020 - 2049",  "2070 - 2099")
    periodo.fig = c("49", "99")
    seasons = c("JJA", "DJF")
    topo = c("asia", "sa")
    
    for(z in 1:2){
      
      for(i in 1:2){
        
        rcp.n = rcp[i] 
        rcp.f = rcp.fig[i]
        
        for(j in 1:length(aux[1,1,1,,1,1])){  # esto por ahora es innecesario ya que siempre sera 1, pero si se agrega T mensual no hay q modificar nada
          
          v = var[j]
          v.f = var.fig[j]
          
          if(j == 1){
            escala = c(0,700)
            resta = 0
            label_escala = "mm"
            colores = "YlGnBu"
            revert = F
            niveles = 9
            breaks = seq(0,700, by = 50)
          } else if(j == 2 ){
            escala = c(0,35)
            resta = 273
            label_escala = "ºC"
            colores = "Spectral"
            revert = T
            niveles = 11
            if(z == 1){
              breaks = seq(0, 40, by = 2.5)
            } else {
              breaks = seq(0, 35, by = 2.5)
            }
            
          } else {
            if(z == 1){
              escala = c(0,0.03)
              breaks = seq(0, 0.03, by = 0.002)
              
            } else {
              
              escala = c(0,0.03)
              breaks = seq(0, 0.03, by = 0.002)
              
            }
            resta = 0
            label_escala = ""
            colores = "PuBuGn"
            revert = F
            niveles = 9
            
          }
          
          
          for(p in 1:2){
            
            per = periodo[p] 
            p.f = periodo.fig[p]
            areaaux = AreaMonsoon(variable = paste(var.fig[1], ".", rcp.fig[i], "_", periodo.fig[p], sep = ""))
            
            
            if(z == 1){
              seas = seasons[1]
              s = 1
            } else {
              seas = seasons[2]
              s = 2
            }
            
            
            
            auxa = array(areaaux[[z]][lons[[z]], lats[[z]]], c(length(lons[[1]]),length(lats[[1]]),1))
            
            aux2 = array(data = aux[lons[[z]], lats[[z]], i, j, p, s], dim = c(length(lons[[z]]), length(lats[[z]]),1))
            auxu = array(data = auxV[lons[[z]], lats[[z]], i, 1, p, s], dim = c(length(lons[[z]]), length(lats[[z]]),1))
            auxv = array(data = auxV[lons[[z]], lats[[z]], i, 2, p, s], dim = c(length(lons[[z]]), length(lats[[z]]),1))
            
            mapa_topo3(variable = aux2, viento = T, u = auxu, v = auxv
                       , titulo = paste(v, modelo, rcp.n, per, seas, sep = " "), nombre.fig = paste(v.f, ".", rcp.f, "_", p.f, "_", zona[z], ".", seas, sep = "" )
                       , escala = breaks, label.escala = label_escala, resta = resta, colorbar = colores, revert = revert, niveles = niveles
                       , lon = lon[lons[[z]]], lat = lat[lats[[z]]], topo = topo[z], altura.topo = 1500, salida = "/Salidas/TP2/drawmonsoon/"
                       , contour = T, variable2 = auxa, nivel.v2 = 180, color.v2 = "firebrick", x.label = "Longitud", y.label = "Latitud", width = 30)
            
            
            
          }
        }
      }
    }
  }
}
#### MAPA_CONT ####
mapa_cont = function(lista, lista2, lista3, titulo, nombre_fig,
                      lon, lat, r, topo, altura, salida){
  
  library(ncdf4)
  library(maps)
  library(ncdf4)
  require(fields)
  require(mapdata)
  library(ggplot2)
  library(RColorBrewer)
  library(mapproj)
  library(metR)
  ruta = getwd()
  
  load("RDatas/topo_india.RData")
  load("RDatas/topo_sa.RData")
  
  
  if(topo == "topo1"){
    
    topo2 = topo_india
    
    topo2[which(topo2$h<altura)]=NA
    
    num = seq(1, r, by = 1)
    
    g = list()
    for(i in 1:r){
      
      #u = array(u[,,i], dim = length(lon)*length(lat))
      
      #v = array(v[,,i], dim = length(lon)*length(lat))
      
      value = array(lista[,,i], dim = length(lon)*length(lat))
      data = matrix(data = NA, nrow = length(lon)*length(lat), ncol = 3)
      
      l=0
      while(l<length(lon)*length(lat)){
        data[seq(l:l+length(lon)),1]<-lon
        l=l+length(lon)
      }
      
      for(j in 1:length(lat)){
        lat_v = array(lat[j],dim=length(lon))
        data[(length(lon)*j-(length(lon)-1)):(j*length(lon)),2]<-lat_v
      } 
      
      
      data[,3]<-value 
      data<-as.data.frame(data)
      
      #data = cbind(data, u)
      #data = cbind(data, v)
      
      #colnames(data)<-c("lon", "lat", "temp", "u", "v")
      colnames(data)<-c("lon", "lat", "temp")
      
      value = array(lista2[,,i], dim = length(lon)*length(lat))
      data2 = matrix(data = NA, nrow = length(lon)*length(lat), ncol = 3)
      
      l=0
      while(l<length(lon)*length(lat)){
        data2[seq(l:l+length(lon)),1]<-lon
        l=l+length(lon)
      }
      
      for(j in 1:length(lat)){
        lat_v = array(lat[j],dim=length(lon))
        data2[(length(lon)*j-(length(lon)-1)):(j*length(lon)),2]<-lat_v
      } 
      
      
      data2[,3]<-value 
      data2<-as.data.frame(data2)
      
      #data2 = cbind(data2, u)
      #data2 = cbind(data2, v)
      
      #colnames(data2)<-c("lon", "lat", "temp", "u", "v")
      colnames(data2)<-c("lon", "lat", "temp2")
      
      value = array(lista3[,,i], dim = length(lon)*length(lat))
      data3 = matrix(data = NA, nrow = length(lon)*length(lat), ncol = 3)
      
      l=0
      while(l<length(lon)*length(lat)){
        data3[seq(l:l+length(lon)),1]<-lon
        l=l+length(lon)
      }
      
      for(j in 1:length(lat)){
        lat_v = array(lat[j],dim=length(lon))
        data3[(length(lon)*j-(length(lon)-1)):(j*length(lon)),2]<-lat_v
      } 
      
      
      data3[,3]<-value 
      data3<-as.data.frame(data3)
      
      #data3 = cbind(data3, u)
      #data3 = cbind(data3, v)
      
      #colnames(data3)<-c("lon", "lat", "temp", "u", "v")
      colnames(data3)<-c("lon", "lat", "temp3")
      
      
      mapa <- map_data("world2", region = c("India", "Sri Lanka", "Bangladesh", "Nepal", "Bhutan", "Pakistan"
                                            ,"Oman", "Yemen", "Somalia", "Eriopia", "Birmania"
                                            , "Malasya", "United Arab Emirates", "Singapur", "Myanmar", "Iran", 
                                            "Turkmenistan", "Afghanistan", "Tajikistan", "Uzbekistan", "Kyrgyzstan", "China", "Mongolia", 
                                            "Bangladesh", "North Korea", "South Korea",  "Taiwan", "Laos", "Thailand", "Vietnam", "Cambodia", 
                                            "Malasya", "Indonesia", "Philippines"), colour = "black")
      
      title = ifelse(test = r>1, yes = paste(titulo, " r" , num[i], sep = ""), no = titulo)
      g =ggplot(topo2, aes(lon, lat)) + theme_minimal()+
        #g = ggplot() + theme_minimal()+
        xlab("Longitud") + ylab("Latitud") + 
        theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
        
        geom_polygon(data = mapa, aes(x = long ,y = lat, group =group),fill = "forestgreen", alpha = 0.5, color = "black") +
        
        geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black", show.legend = F) +
        
        stat_contour(data = data, aes(x = lon, y = lat, z = temp), color = "yellow1", size = 1, breaks = 180 )+
        stat_contour(data = data2, aes(x = lon, y = lat, z = temp2), color = "orange1", size = 1, breaks = 180 )+
        stat_contour(data = data3, aes(x = lon, y = lat, z = temp3), color = "red1", size = 1, breaks = 180 )+
        
        ggtitle(title)+
        scale_x_longitude(breaks = seq(40, 140, by = 10), name = "longitud")+
        scale_y_latitude(breaks = seq(-10, 55, by = 10), name = "Latitud")+
        theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
              axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill = NA, size = 3),
              panel.ontop = TRUE,
              plot.title = element_text(hjust = 0.5)) + geom_hline(yintercept = 0, color = "black")
      
      
      
      
      ggsave(paste(ruta, salida, nombre_fig, num[i], ".jpg", sep = ""), plot = g, width = 30, height = 20  , units = "cm")
    } 
    
  } else {
    
    topo2 = topo_sa
    
    topo2[which(topo2$h<altura)]=NA
    
    num = seq(1, r, by = 1)
    
    g = list()
    for(i in 1:r){
      
      #u = array(u[,,i], dim = length(lon)*length(lat))
      
      #v = array(v[,,i], dim = length(lon)*length(lat))
      
      value = array(lista[,,i], dim = length(lon)*length(lat))
      data = matrix(data = NA, nrow = length(lon)*length(lat), ncol = 3)
      
      l=0
      while(l<length(lon)*length(lat)){
        data[seq(l:l+length(lon)),1]<-lon
        l=l+length(lon)
      }
      
      for(j in 1:length(lat)){
        lat_v = array(lat[j],dim=length(lon))
        data[(length(lon)*j-(length(lon)-1)):(j*length(lon)),2]<-lat_v
      } 
      
      
      data[,3]<-value 
      
      data<-as.data.frame(data)
      #data = cbind(data, u)
      #data = cbind(data, v)
      
      #colnames(data)<-c("lon", "lat", "temp", "u", "v")
      colnames(data)<-c("lon", "lat", "temp")
      
      
      value = array(lista2[,,i], dim = length(lon)*length(lat))
      data2 = matrix(data = NA, nrow = length(lon)*length(lat), ncol = 3)
      
      l=0
      while(l<length(lon)*length(lat)){
        data2[seq(l:l+length(lon)),1]<-lon
        l=l+length(lon)
      }
      
      for(j in 1:length(lat)){
        lat_v = array(lat[j],dim=length(lon))
        data2[(length(lon)*j-(length(lon)-1)):(j*length(lon)),2]<-lat_v
      } 
      
      
      data2[,3]<-value 
      data2<-as.data.frame(data2)
      
      #data2 = cbind(data2, u)
      #data2 = cbind(data2, v)
      
      #colnames(data2)<-c("lon", "lat", "temp", "u", "v")
      colnames(data2)<-c("lon", "lat", "temp2")
      
      value = array(lista3[,,i], dim = length(lon)*length(lat))
      data3 = matrix(data = NA, nrow = length(lon)*length(lat), ncol = 3)
      
      l=0
      while(l<length(lon)*length(lat)){
        data3[seq(l:l+length(lon)),1]<-lon
        l=l+length(lon)
      }
      
      for(j in 1:length(lat)){
        lat_v = array(lat[j],dim=length(lon))
        data3[(length(lon)*j-(length(lon)-1)):(j*length(lon)),2]<-lat_v
      } 
      
      
      data3[,3]<-value 
      data3<-as.data.frame(data3)
      
      #data3 = cbind(data3, u)
      #data3 = cbind(data3, v)
      
      #colnames(data3)<-c("lon", "lat", "temp", "u", "v")
      colnames(data3)<-c("lon", "lat", "temp3")
      
      
      mapa <- map_data("world2", region = c("Brazil", "French Guiana", "Suriname", "Colombia", "Venezuela","Argentina", "Chile", "Uruguay",
                                            "Bolivia", "Ecuador", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica", "Nicaragua",
                                            "Martinique"), colour = "black")
      
      
      title = ifelse(test = r>1, yes = paste(titulo, " r" , num[i], sep = ""), no = titulo)
      
      g = ggplot(topo2, aes(lon, lat)) + theme_minimal()+
        #g = ggplot() + theme_minimal()+
        xlab("Longitud") + ylab("Latitud") + 
        theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
        
        geom_polygon(data = mapa, aes(x = long ,y = lat, group =group),fill = "forestgreen", alpha = 0.5, color = "black") +
        
        geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black", show.legend = F) +
        
        stat_contour(data = data, aes(x = lon, y = lat, z = temp), color = "yellow1", size = 1, breaks = 180 )+
        stat_contour(data = data2, aes(x = lon, y = lat, z = temp2), color = "orange1", size = 1, breaks = 180 )+
        stat_contour(data = data3, aes(x = lon, y = lat, z = temp3), color = "red1", size = 1, breaks = 180 )+
        
        geom_hline(yintercept = 0, color = "black")+
        
        ggtitle(title)+
        scale_x_longitude(breaks = seq(250, 330, by = 10), name = "longitud")+
        scale_y_latitude(breaks = seq(-35, 20, by = 10), name = "Latitud")+
        theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
              axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill = NA, size = 3),
              panel.ontop = TRUE,
              plot.title = element_text(hjust = 0.5))
      
      
      ggsave(paste(ruta, salida, nombre_fig, num[i], ".jpg", sep = ""), plot = g, width = 25, height = 20  , units = "cm")
      
      
      
    }
  }
  
}
#### AREAMONSOON ####
AreaMonsoon = function(variable){
  
  if( variable == "pp5.his"){
    
    load("RDatas/pp5.his.RData")
    data = pp5.his
    rm(pp5.his)
    
  } else if( variable == "pp6.his"){
    
    load("RDatas/pp6.his.RData")
    data = pp6.his
    rm(pp6.his)
    
  } else if(variable == "pp5.26_49"){
    # tiene una sola corrida!!
    
    load("RDatas/pp5.26_2049.RData")
    data = pp5.26_2049
    rm(pp5.26_2049)
    
    pp_anual = apply(data[[1]], c(1,2,4), mean, na.rm = T)
    
    
    d = dim(data[[1]])[4]
    
    pp_seasons_r = array(data = NA, c(144,73,2,d))
    pp_seasons_r[,,1,] = apply(data[[1]][,,2,], c(1,2), mean, na.rm = T) #JJA
    pp_seasons_r[,,2,] = apply(data[[1]][,,4,], c(1,2), mean, na.rm = T) #DJF
    
    aux = abs( pp_seasons_r[,,2,] - pp_seasons_r[,,1,])
    aux2 = pp_seasons_r[,,1,]/pp_anual[,,1]
    aux3 = pp_seasons_r[,,2,]/pp_anual[,,1]
    
    aux2[which(aux2<0.55)] = NA # modifico un poco el criterio. uso el de ...
    aux2[which(!is.na(aux2))] = 1
    aux3[which(aux3<0.55)] = NA
    aux3[which(!is.na(aux3))] = 1
    
    areas = list()
    areas[[1]] = apply(aux*aux2, c(1,2), mean, na.rm = T)
    areas[[2]] = apply(aux*aux3, c(1,2), mean, na.rm = T)
    
    return(areas)
    break()
    
  } else if(variable == "pp5.26_99"){
    # tiene una sola corrida 
    
    load("RDatas/pp5.26_2099.RData")
    data = pp5.26_2099
    rm(pp5.26_2099)
    
    pp_anual = apply(data[[1]], c(1,2,4), mean, na.rm = T)
    
    
    d = dim(data[[1]])[4]
    
    pp_seasons_r = array(data = NA, c(144,73,2,d))
    pp_seasons_r[,,1,] = apply(data[[1]][,,2,], c(1,2), mean, na.rm = T) #JJA
    pp_seasons_r[,,2,] = apply(data[[1]][,,4,], c(1,2), mean, na.rm = T) #DJF
    
    aux = abs( pp_seasons_r[,,2,] - pp_seasons_r[,,1,])
    aux2 = pp_seasons_r[,,1,]/pp_anual[,,1]
    aux3 = pp_seasons_r[,,2,]/pp_anual[,,1]
    
    aux2[which(aux2<0.55)] = NA # modifico un poco el criterio. uso el de ...
    aux2[which(!is.na(aux2))] = 1
    aux3[which(aux3<0.55)] = NA
    aux3[which(!is.na(aux3))] = 1
    
    areas = list()
    areas[[1]] = apply(aux*aux2, c(1,2), mean, na.rm = T)
    areas[[2]] = apply(aux*aux3, c(1,2), mean, na.rm = T)
    
    return(areas)
    break()
    
  } else if(variable == "pp6.26_49"){
    
    load("RDatas/pp6.26_2049.RData")
    data = pp6.26_2049
    rm(pp6.26_2049)
    
  } else if(variable == "pp6.26_99"){
    
    load("RDatas/pp6.26_2099.RData")
    data = pp6.26_2099
    rm(pp6.26_2099)
    
  } else if(variable == "pp5.85_49"){
    
    load("RDatas/pp5.85_2049.RData")
    data = pp5.85_2049
    rm(pp5.85_2049)
    
  } else if(variable == "pp5.85_99"){
    
    load("RDatas/pp5.85_2099.RData")
    data = pp5.85_2099
    rm(pp5.85_2099)
    
  } else if(variable == "pp6.85_49"){
    
    load("RDatas/pp6.85_2049.RData")
    data = pp6.85_2049
    rm(pp6.85_2049)
    
  } else if(variable == "pp6.85_99"){
    
    load("RDatas/pp6.85_2099.RData")
    data = pp6.85_2099
    rm(pp6.85_2099)
    
  }
  
  pp_anual = apply(data[[1]], c(1,2,4), mean, na.rm = T)
  
  
  d = dim(data[[1]])[4]
  
  pp_seasons_r = array(data = NA, c(144,73,2,d))
  pp_seasons_r[,,1,] = apply(data[[1]][,,2,], c(1,2,3), mean, na.rm = T) #JJA
  pp_seasons_r[,,2,] = apply(data[[1]][,,4,], c(1,2,3), mean, na.rm = T) #DJF
  
  aux = abs( pp_seasons_r[,,2,] - pp_seasons_r[,,1,])
  aux2 = pp_seasons_r[,,1,]/pp_anual
  aux3 = pp_seasons_r[,,2,]/pp_anual
  
  aux2[which(aux2<0.55)] = NA # modifico un poco el criterio. uso el de ...
  aux2[which(!is.na(aux2))] = 1
  aux3[which(aux3<0.55)] = NA
  aux3[which(!is.na(aux3))] = 1
  
  areas = list()
  areas[[1]] = apply(aux*aux2, c(1,2), mean, na.rm = T)
  areas[[2]] = apply(aux*aux3, c(1,2), mean, na.rm = T)
  
  return(areas)
  
}
#### MONSOONINTENSITY ####
MonsoonIntensity = function(area.his, area.49, area.99, lons, lats, km){
  
  # experimental...
  # funca con salidas de AreaMonsoon
  
  zona = list()
  zona[[1]] = area.his[[1]][lons[[1]], lats[[1]]] 
  zona[[2]] = area.his[[2]][lons[[2]], lats[[2]]]
  zona[[3]] = area.49[[1]][lons[[1]], lats[[1]]] 
  zona[[4]] = area.49[[2]][lons[[2]], lats[[2]]] 
  zona[[5]] = area.99[[1]][lons[[1]], lats[[1]]] 
  zona[[6]] = area.99[[2]][lons[[2]], lats[[2]]] 
  
  intensidad = matrix(data = NA, nrow = 3, ncol = 2)
  colnames(intensidad) = c("Asia", "SA")
  row.names(intensidad) = c("His", "2049", "2099")
  i = 1
  j = 1
  k = 1
  while(i <= 6){
    zona[[i]][which(zona[[i]]<180)] = NA
    if( (i/2)%%1 == 0){
      
      intensidad[j,2] = sum(zona[[i]], na.rm = T)/(length(which(!is.na(zona[[i]])))*km) # aca agregar km2 --> es grilla regular?
      j = j + 1
      
    } else {
      
      intensidad[k,1] = sum(zona[[i]], na.rm = T)/(length(which(!is.na(zona[[i]])))*km) # aca agregar km2 --> es grilla regular?
      k = k + 1
    }
    
    i = i + 1
  }
  
  return(as.data.frame(intensidad))
  
} 

#### PPMeses ####
PPMeses = function(modelo, mes){
  
  m = mes
  
  if(modelo == "5"){
    
    load("RDatas/pp5.his.RData")
    pp.his.mes = pp5.his[[3]]  # era dim anios, 4ta meses.
    pp.his = apply(pp.his.mes[,,,mes], c(1,2), mean, na.rm = T) 
    #aux = apply(pp_his, c(3), mean)
    
    
    load("RDatas/pp5.26_2049.RData")
    pp.26_49.men = pp5.26_2049[[3]]  # era dim anios, 4ta meses.
    pp.26_49 = apply(pp.26_49.men[,,,m], c(1,2), mean, na.rm = T) 
    
    load("RDatas/pp5.26_2099.RData")
    pp.26_99.men = pp5.26_2099[[3]]  # era dim anios, 4ta meses.
    pp.26_99 = apply(pp.26_99.men[,,,m], c(1,2), mean, na.rm = T) 
    
    
    load("RDatas/pp5.85_2049.RData")
    pp.85_49.men = pp5.85_2049[[3]]  # era dim anios, 4ta meses.
    pp.85_49 = apply(pp.85_49.men[,,,m], c(1,2), mean, na.rm = T) 
    
    load("RDatas/pp5.85_2099.RData")
    pp.85_99.men = pp5.85_2099[[3]]  # era dim anios, 4ta meses.
    pp.85_99 = apply(pp.85_99.men[,,,m], c(1,2), mean, na.rm = T) 
    
    rm(pp5.his, pp5.26_2049, pp5.26_2099, pp5.85_2049, pp5.85_2099)
    
    V = list()
    V[[1]] = pp.his
    V[[2]] = pp.26_49; V[[3]] = pp.26_99
    V[[4]] = pp.85_49; V[[5]] = pp.85_99
    
    return(V)
    
  } else {
    
    load("RDatas/pp6.his.RData")
    pp.his.mes = pp6.his[[3]]  # era dim anios, 4ta meses.
    pp.his = apply(pp.his.mes[,,,mes], c(1,2), mean, na.rm = T) 
    #aux = apply(pp_his, c(3), mean)
    
    
    load("RDatas/pp6.26_2049.RData")
    pp.26_49.men = pp6.26_2049[[3]]  # era dim anios, 4ta meses.
    pp.26_49 = apply(pp.26_49.men[,,,m], c(1,2), mean, na.rm = T) 
    
    load("RDatas/pp6.26_2099.RData")
    pp.26_99.men = pp6.26_2099[[3]]  # era dim anios, 4ta meses.
    pp.26_99 = apply(pp.26_99.men[,,,m], c(1,2), mean, na.rm = T) 
    
    
    load("RDatas/pp6.85_2049.RData")
    pp.85_49.men = pp6.85_2049[[3]]  # era dim anios, 4ta meses.
    pp.85_49 = apply(pp.85_49.men[,,,m], c(1,2), mean, na.rm = T) 
    
    load("RDatas/pp6.85_2099.RData")
    pp.85_99.men = pp6.85_2099[[3]]  # era dim anios, 4ta meses.
    pp.85_99 = apply(pp.85_99.men[,,,m], c(1,2), mean, na.rm = T) 
    
    rm(pp6.his, pp6.26_2049, pp6.26_2099, pp6.85_2049, pp6.85_2099)
    
    V = list()
    V[[1]] = pp.his
    V[[2]] = pp.26_49; V[[3]] = pp.26_99
    V[[4]] = pp.85_49; V[[5]] = pp.85_99
    
    return(V)
    
  }
  
}
#### Derivada ####
# derivadas por algo asi como diferencias finitas
Dy = function(arr, dy){
  
  dUy = arr*0
  a = length(arr[,1])
  b = length(arr[1,])
  i = 1
  
  while(i<a){
    j = 1
    while(j<b){
      if(j == b-1){
        dUy[i,j]=(arr[i,b]-arr[i,j])/dy
      } else {
        dUy[i,j]=(arr[i,j+1]-arr[i,j])/dy
      }
      j=j+1
    }
    i=i+1
    
  }
  return(dUy)
}


Dx = function(arr,dx){
  
    # este es por las dudas.
    dUx = arr*0
    a = length(arr[,1])
    b = length(arr[1,])
    i = 1
    
    while(i<a){
      j = 1
      while(j<b){
        if(i == a-1){
          dUx[i,j]=(arr[a,j]-arr[i,j])/(dx*cos(lat[j]*pi/180))
        } else {
          dUx[i,j]=(arr[i+1,j]-arr[i,j])/(dx*cos(lat[j]*pi/180))/2
        }
        j=j+1
      }
      i=i+1
      
    }
    return(dUx) 
  
}



#### Adveccion ####
Adv = function(variable, u, v, i){
  
  v.dx = Dx(arr = variable[lons[[i]], lats[[i]], i], dx = 2.5, es = "no")
  v.dy = Dy(arr = variable[lons[[i]], lats[[i]], i], dy = 2.5)
    
  adv = -1*(u[lons[[i]],lats[[i]]]*v.dx + v[lons[[i]],lats[[i]]]*v.dy)
    
  return(adv)
}
#### MAPA_TOPO3.0 ####
mapa_topo3 = function(variable, variable.sig = NULL, variable.cont = NULL, u = NULL, v = NULL, lon, lat, contour.fill = T, contour = F, viento = F
                      , colorbar = "Spectral", niveles = 9, revert = F, escala = NULL, resta = 0, resta.vsig = 0, resta.vcont = 0, nivel.vcont = NULL, color.vsig = "black", color.vcont = "red", alpha.vsig, sig = F
                      , titulo = NULL, label.escala = "value", x.label = NULL, y.label = NULL, fill.mapa = F, colorbar.pos = "right"
                      , mapa = NULL, altura.topo = 0, r = 1, na.fill = NULL, nombre.fig = "fig", width = 25, height = 20, salida = NULL){
  
  library(maps)
  library(ncdf4)
  require(fields)
  require(mapdata)
  library(ggplot2)
  library(RColorBrewer)
  library(mapproj)
  library(metR)
  
  ruta = getwd()
  
  
  limites = c(min(escala), max(escala))
  
  if(mapa == "asia"){
    
    #load("RDatas/topo_india.RData")
    #topo2 = topo_india
    #rm(topo_india)
    
    #topo2[which(topo2$h<altura.topo)]=NA
    
    map <- map_data("world2", region = c("India", "Sri Lanka", "Bangladesh", "Nepal", "Bhutan", "Pakistan"
                                         ,"Oman", "Yemen", "Somalia", "Eriopia", "Birmania"
                                         , "Malasya", "United Arab Emirates", "Singapur", "Myanmar", "Iran", 
                                         "Turkmenistan", "Afghanistan", "Tajikistan", "Uzbekistan", "Kyrgyzstan", "China", "Mongolia", 
                                         "Bangladesh", "North Korea", "South Korea",  "Taiwan", "Laos", "Thailand", "Vietnam", "Cambodia", 
                                         "Malasya", "Indonesia", "Philippines"), colour = "black")
    
    breaks.lon = seq(40, 140, by = 10)
    breaks.lat = seq(-10, 55, by = 10)
    
  } else if(mapa == "mundo"){
    
    map = map_data("world2", colour = "black")
    
    breaks.lon = seq(0, 360, by = 30)
    breaks.lat = seq(-90, 90, by = 20)
    
    
  } else if(mapa == "sa") {
    
    load("topo.RData")
    topo2 = topo
    rm(topo)
    
    topo2[which(topo2$h<altura.topo)]=NA
    
    map <- map_data("world2", region = c("Brazil", "French Guiana", "Suriname", "Colombia", "Venezuela","Argentina", "Chile", "Uruguay",
                                         "Bolivia", "Ecuador", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica", "Nicaragua",
                                         "Martinique"), colour = "black")
    
    
    breaks.lon = seq(250, 350, by = 10)
    breaks.lat = seq(-60, 20, by = 10)
    
  }
  
  g = list()
  num = seq(1, r, by = 1)
  
  for(i in 1:r){
    
    data = expand.grid(lon = lon, lat = lat)
    
    data[,3] = array(variable[,,i], dim = length(lon)*length(lat)) - resta
    
    colnames(data)<-c("lon", "lat", "var")
    
    if(mapa == "SA"){
      
      g = ggplot(topo2, aes(lon, lat)) + theme_minimal() +
        xlab(x.label) + ylab(y.label) + 
        theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
        geom_tile(data = data, aes(x = lon, y = lat, fill = var), alpha = 0.8, na.rm = T) 
      
      
    } else {
      
      g = ggplot() + theme_minimal() +
        xlab(x.label) + ylab(y.label) + 
        theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
        geom_tile(data = data, aes(x = lon, y = lat, fill = var), alpha = 0.8, na.rm = T) 
      
    }
    
    if(colorbar.pos == "right"){
      
      if(contour.fill == T & revert == T ){
        
        g = g +  geom_contour_fill(data = data, aes(x = lon, y = lat, z = var),alpha = 1, na.fill = na.fill , breaks = escala) +
          scale_fill_stepsn(limits = limites, name = label.escala, colours = rev(brewer.pal(n=niveles , colorbar)), na.value = "white", breaks = escala,
                            guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T))
      } else if(contour.fill == T & revert == F ){
        g = g +  geom_contour_fill(data = data, aes(x = lon, y = lat, z = var),alpha = 1, na.fill = na.fill , breaks = escala) +
          scale_fill_stepsn(limits = limites, name = label.escala, colours = brewer.pal(n=niveles , colorbar), na.value = "white", breaks = escala,
                            guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) 
      } else if(contour.fill == F & revert == T){
        g = g + scale_fill_stepsn(limits = limites, name = label.escala, colours = rev(brewer.pal(n=niveles , colorbar)), na.value = "white", breaks = escala,
                                  guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) 
      } else {
        g + scale_fill_stepsn(limits = limites, name = label.escala, colours = brewer.pal(n=niveles , colorbar), na.value = "white", breaks = escala,
                              guide = guide_colorbar(barwidth = 1, barheight = 20, title.position = "top", title.hjust = 0.5, raster = F, ticks = T)) 
      }
      
      
    } else {
      
      if(contour.fill == T & revert == T ){
        
        g = g +  geom_contour_fill(data = data, aes(x = lon, y = lat, z = var),alpha = 1, na.fill = na.fill , breaks = escala) +
          scale_fill_stepsn(limits = limites, name = label.escala, colours = rev(brewer.pal(n=niveles , colorbar)), na.value = "white", breaks = escala,
                            guide = guide_colorbar(barwidth = 35, barheight = 0.8, title.position = "left", title.hjust = 0.5, raster = F, ticks = T, direction = "horizontal")) 
      } else if(contour.fill == T & revert == F ){
        g = g +  geom_contour_fill(data = data, aes(x = lon, y = lat, z = var),alpha = 1, na.fill = na.fill , breaks = escala) +
          scale_fill_stepsn(limits = limites, name = label.escala, colours = brewer.pal(n=niveles , colorbar), na.value = "white", breaks = escala,
                            guide = guide_colorbar(barwidth = 35, barheight = 0.8, title.position = "left", title.hjust = 0.5, raster = F, ticks = T, direction = "horizontal")) 
      } else if(contour.fill == F & revert == T){
        g = g + scale_fill_stepsn(limits = limites, name = label.escala, colours = rev(brewer.pal(n=niveles , colorbar)), na.value = "white", breaks = escala,
                                  guide = guide_colorbar(barwidth = 35, barheight = 0.8, title.position = "left", title.hjust = 0.5, raster = F, ticks = T, direction = "horizontal")) 
      } else {
        g + scale_fill_stepsn(limits = limites, name = label.escala, colours = brewer.pal(n=niveles , colorbar), na.value = "white", breaks = escala,
                              guide = guide_colorbar(barwidth = 35, barheight = 0.8, title.position = "left", title.hjust = 0.5, raster = F, ticks = T, direction = "horizontal")) 
      }
      
      
    }
    
    if(sig == T){
      
      data2 = expand.grid(lon = lon, lat = lat)
      data2[,3] = array(variable.sig[,,i], dim = length(lon)*length(lat)) - resta.vsig
      colnames(data2)<-c("lon", "lat", "var")
      
      g = g +  geom_tile(data = subset(data2, is.na(var)),aes(x = lon, y = lat, fill = is.na(var)), alpha = alpha.vsig, fill = color.vsig, show.legend = F)
      
      
    }
    
    
    if(mapa == "SA"){
      
      g = g + geom_tile(aes(fill = h ), na.rm = T, alpha = 0.1, color = "black", show.legend = F) 
      
    }
    
    if(viento == T){
      
      u = array(u[,,i], dim = length(lon)*length(lat))
      v = array(v[,,i], dim = length(lon)*length(lat))
      
      data = cbind(data, u)
      data = cbind(data, v)
      
      colnames(data)<-c("lon", "lat", "temp", "u", "v")
      
      g = g +  geom_arrow(data = data, aes(x = lon, y = lat, dx = u*2, dy = v*2), color = "black"
                          , skip.x = 0, skip.y = 0, arrow.length = 0.5, na.rm = T, show.legend = F)
    } 
    
    
    if(fill.mapa == T){
      g = g + geom_polygon(data = map, aes(x = long ,y = lat, group = group),fill = "black", color = "black", alpha = 0.3) 
    } else {
      g = g + geom_polygon(data = map, aes(x = long ,y = lat, group = group),fill = NA, color = "black") 
    }
    
    
    
    if(contour == T){
      
      data2 = expand.grid(lon = lon, lat = lat)
      data2[,3] = array(variable.cont[,,i], dim = length(lon)*length(lat)) - resta.vcont
      colnames(data2)<-c("lon", "lat", "cont")
      
      g = g +  stat_contour(data = data2, aes(x = lon, y = lat, z = cont), color = color.vcont, size = 1, breaks = nivel.vcont )
      
    }
    
    g = g + ggtitle(titulo) +
      scale_x_longitude(breaks = breaks.lon, name = x.label)+
      scale_y_latitude(breaks = breaks.lat, name = y.label)+
      theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
            axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            panel.border = element_rect(colour = "black", fill = NA, size = 3),
            panel.ontop = TRUE,
            plot.title = element_text(hjust = 0.5)) + geom_hline(yintercept = 0, color = "black") 
    if(colorbar.pos == "bottom"){
      g = g + theme(legend.position = "bottom")
    }
    
    
    ggsave(paste(ruta, salida, nombre.fig, num[i], ".jpg", sep = ""), plot = g, width = width, height = height, units = "cm")
    
  }
}  
#### Auxs ####
open_ncobs.tp3 = function(variable){
  
  # separando en estaciones, años de marzo a febrero, empezando en marzo del '76 hasta febrero del '05
  aux = variable[,,(3+12):(2+12*30)] # tiene un año mas, +12
  
  v.obs_anio_mf = array(NA, dim = c(144, 73, 29, 12)) 
  
  for(j in 1:12){
    for (i in 0:28){
      v.obs_anio_mf[,,1+i,j] = aux[ , , j+12*i]
    }
  }
  
  #promediando Estaciones en orden MAM, JJA, SON, DJF
  v.obs_seasons = array(NA, dim = c(144, 73, 4, 29))
  i=1
  while(i<=4){
    v.obs_seasons[,,i,] = apply(v.obs_anio_mf[,,,(i + 2*i - 2):(i+2*i)], c(1,2,3), mean)
    
    i = i + 1
  }
  
  return(v.obs_seasons)
  
}

AnualMean = function(v){
  
  W = array(data = NA, dim = c(dim(v[,,1]),30))
  
  for(i in 0:29){
    aux = v[,,(1+12*i):(12+12*i)]
    W[,,i+1] = apply(aux, c(1,2), mean, na.rm = T)
  }
  
  return(W)
  
}



AnualMeanR = function(v){
  
  W = array(data = NA, dim = c(dim(v[,,1,1]),30, length(v[1,1,1,])))
  
  for(i in 0:29){
    aux = v[,,(1+12*i):(12+12*i),]
    W[,,i+1,] = apply(aux, c(1,2,4), mean, na.rm = T)
  }
  
  return(W)
  
}


AnualMonthR = function(v){
  
  W = array(data = NA, dim = c(dim(v[,,1,1]), 12, 30))
  
  for(i in 0:29){
    aux = v[,,(1+12*i):(12+12*i),]
    W[,,,i+1] = apply(aux, c(1,2,3), mean, na.rm = T)
  }
  
  W = apply(W, c(1,2,3), mean, na.rm = T)
  return(W)
  
}




Tendencia<-function(data, s){
  
  aux = array(NA, c(144,73)); aux2 = array(NA, c(144,73))
  
  for(i in 1:144){
    for(j in 1:73){
      
      datos = data[i,j,]
     
      datos = cbind(seq(length(datos)), datos)
      
      serie<-data.frame(datos[which(!is.na(datos[,2])),1],datos[which(!is.na(datos[,2])),2]) 
      serie[,1]<-seq(1:length(serie[,1])) 
      ajuste1<-lm(serie[,2]~serie[,1],data=serie) #Y , X!!
      tendencia<-ajuste1$coefficients[1]+ajuste1$coefficients[2]*serie[,1]  #contruccion de la recta y=ax+b... coef[2]x+coef[1]
      b = ajuste1$coefficients[2]
      attributes(b) = NULL
      
      f = summary(ajuste1)$fstatistic
      p.value = pf(f[1],f[2],f[3],lower.tail=F); attributes(p.value) = NULL
      
      serie_tendencia = data.frame(Serie = serie[,2], Tendencia = tendencia)
      
      
      p = 1 - s
      aux[i,j] = b
      aux2[i,j] = ifelse(test = p.value < p, yes = 1, no = NA)
      
     
    }
  }
  V = list()
  V[[1]] = aux; V[[2]] = aux2
  
  return(V)
}


Tendencia.ts<-function(datos){
  
  datos = cbind(seq(length(datos)), datos)
  
  serie<-data.frame(datos[which(!is.na(datos[,2])),1],datos[which(!is.na(datos[,2])),2]) 
  serie[,1]<-seq(1:length(serie[,1])) 
  ajuste1<-lm(serie[,2]~serie[,1],data=serie) #Y , X!!
  tendencia<-ajuste1$coefficients[1]+ajuste1$coefficients[2]*serie[,1]  #contruccion de la recta y=ax+b... coef[2]x+coef[1]
  b = ajuste1$coefficients[2]
  attributes(b) = NULL
  
  f = summary(ajuste1)$fstatistic
  p.value = pf(f[1],f[2],f[3],lower.tail=F); attributes(p.value) = NULL
  
  serie_tendencia = data.frame(Serie = serie[,2], Tendencia = tendencia)
  
  V = list()
  V[[1]] = serie_tendencia
  V[[2]] = p.value
  V[[3]] = b
  
  ifelse(test = p.value < 0.05, yes = print(paste("Tendencia Significatica ", " p-value", sprintf("%.5f",p.value)))
         , no = print(paste("Tendencia NO Significatica ", " p-value", sprintf("%.5f",p.value))))
  
  return(V)
}


PlotTsTend = function(global, hn, hs, titulo, y.label, y.breaks, anios, nombre.fig, cent = F){
  
  anios = seq(min(anios), max(anios), by = 1)
  limite = c(min(y.breaks), max(y.breaks))
  if(cent == T){
    datos = as.data.frame(global[[1]]-273); datos = cbind(datos, hn[[1]]-273, hs[[1]]-273, anios); colnames(datos) = c("Global", "GTend", "HN", "HNTend", "HS", "HSTend", "Años")
  } else {
    datos = as.data.frame(global[[1]]); datos = cbind(datos, hn[[1]], hs[[1]], anios); colnames(datos) = c("Global", "GTend", "HN", "HNTend", "HS", "HSTend", "Años")
  }
  

  g = ggplot(datos, aes(x = Años)) + theme_minimal()
  
  if(global[[2]]<0.05){
    g = g + geom_line(aes(y = Global, colour = "Global"), size = 0.8, show.legend = T) + geom_line(aes(y = GTend, colour = "Global"),linetype = 1, size = 0.5, show.legend = F) 
  } else {
    g =g + geom_line(aes(y = Global, colour = "Global"), size = 0.8, show.legend = T) + geom_line(aes(y = GTend, colour = "Global"),linetype = 4, size = 0.5, show.legend = F)  
  }
  
  if(hn[[2]]<0.05){
    g = g +  geom_line(aes(y = HN, colour = "HN"), size = 0.8, show.legend = T) + geom_line(aes(y = HNTend, colour = "HN"),linetype = 1, size = 0.5, show.legend = F)  
  } else {
    g = g +  geom_line(aes(y = HN, colour = "HN"), size = 0.8, show.legend = T) + geom_line(aes(y = HNTend, colour = "HN"),linetype = 4, size = 0.5, show.legend = F) 
  }
  
  if(hs[[2]]<0.05){
    g = g +   geom_line(aes(y = HS, colour = "HS"), size = 0.8, show.legend = T) + geom_line(aes(y = HSTend, colour = "HS"), linetype = 1, size = 0.5, show.legend = F) 
  } else {
    g =g +   geom_line(aes(y = HS, colour = "HS"), size = 0.8, show.legend = T) + geom_line(aes(y = HSTend, colour = "HS"), linetype = 4, size = 0.5, show.legend = F) 
  }
  
  anios.breaks = seq(min(anios), max(anios), by = 5)
  
  g = g + scale_colour_manual("", 
                              breaks = c("Global", "HN", "HS"),
                              values = c("Black", "forestgreen", "royalblue")) +
    scale_x_continuous(breaks = anios.breaks)+
    scale_y_continuous(breaks = y.breaks, limits = limite) +
    ggtitle(titulo) +
    ylab(y.label) +
    theme(axis.text.y   = element_text(size = 14, color = "black"), axis.text.x   = element_text(size = 14, color = "black"), axis.title.y  = element_text("ºC"),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          panel.ontop = F,
          plot.title = element_text(hjust = 0.5, size = 18),
          legend.position = "right", legend.key.width = unit(1, "cm"), legend.key.height = unit(2, "cm"), legend.text = element_text(size = 15)) 
  
  ggsave(paste(getwd(), "/Salidas/TP3/",nombre.fig,".jpg",sep =""), plot = g, width = 20, height = 10  , units = "cm")
  
  
}



HLatMean = function(serie1, serie2, serie3, lat,  titulo, nombre.fig){
  
  datos = as.data.frame(serie1); datos = cbind(lat, datos, serie2, serie3); colnames(datos) = c("lat", "Historico", "F.Cercano", "F.Lejano")
  
  g = ggplot(datos, aes(x = lat)) + theme_minimal()+
    geom_line(aes(y = Historico, colour = "Historico"), size = 1, show.legend = T) + 
    geom_line(aes(y = F.Cercano, colour = "2020-2049"),linetype = 1, size = 1, show.legend = T)  +
    geom_line(aes(y = F.Lejano, colour = "2070-2099"),linetype = 1, size = 1, show.legend = T) +
    scale_colour_manual("", 
                        breaks = c("Historico", "2020-2049", "2070-2099"),
                        values = c("forestgreen","orange2", "firebrick")) +
    scale_x_latitude(breaks = seq(-90, 90, by = 20)) + scale_y_continuous(breaks = seq(10, 80, by = 10), limits = c(10, 80)) +
    geom_vline(xintercept = 0, alpha = 0.3)+
    ggtitle(titulo) +
    ylab("kJ/kg") +
    theme(axis.text.y   = element_text(size = 14, color = "black"), axis.text.x   = element_text(size = 14, color = "black"), axis.title.y  = element_text("ºC"),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          panel.ontop = F,
          plot.title = element_text(hjust = 0.5, size = 18),
          legend.position = "bottom", legend.key.width = unit(2, "cm"), legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 12)) 
  
  ggsave(paste(getwd(), "/Salidas/TP3/",nombre.fig,".jpg",sep =""), plot = g, width = 20, height = 10  , units = "cm")
  
}


HLatMean2 = function(serie1, serie2, serie3, lat,  titulo, nombre.fig){
  
  datos = as.data.frame(serie1); datos = cbind(lat, datos, serie2, serie3); colnames(datos) = c("lat", "Entalpia", "SH", "LH")
  
  g = ggplot(datos, aes(x = lat)) + theme_minimal()+
    geom_line(aes(y = Entalpia, colour = "Entalpia"), size = 1, show.legend = T) + 
    geom_line(aes(y = SH, colour = "SH"),linetype = 1, size = 1, show.legend = T)  +
    geom_line(aes(y = LH, colour = "LH"),linetype = 1, size = 1, show.legend = T) +
    scale_colour_manual("", 
                        breaks = c("Entalpia","SH", "LH"),
                        values = c("firebrick","orange2","forestgreen")) +
    scale_x_latitude(breaks = seq(-90, 90, by = 20)) + scale_y_continuous(breaks = seq(0, 80, by = 10), limits = c(0, 80)) +
    geom_vline(xintercept = 0, alpha = 0.3)+
    ggtitle(titulo) +
    ylab("kJ/kg") +
    theme(axis.text.y   = element_text(size = 14, color = "black"), axis.text.x   = element_text(size = 14, color = "black"), axis.title.y  = element_text("ºC"),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          panel.ontop = F,
          plot.title = element_text(hjust = 0.5, size = 18),
          legend.position = "bottom", legend.key.width = unit(2, "cm"), legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 12)) 
  
  ggsave(paste(getwd(), "/Salidas/TP3/Hlon_seas/",nombre.fig,".jpg",sep =""), plot = g, width = 20, height = 10  , units = "cm")
  
}



HLatMean3 = function(serie1, serie2, serie3, lat,  titulo, nombre.fig){
  
  datos = as.data.frame(serie1); datos = cbind(lat, datos, serie2, serie3); colnames(datos) = c("meses", "Entalpia", "SH", "LH")
  
  g = ggplot(datos, aes(x = meses)) + theme_minimal()+
    geom_line(aes(y = Entalpia, colour = "Entalpia"), size = 1, show.legend = T) + 
    geom_line(aes(y = SH, colour = "SH"),linetype = 1, size = 1, show.legend = T)  +
    geom_line(aes(y = LH, colour = "LH"),linetype = 1, size = 1, show.legend = T) +
    scale_colour_manual("", 
                        breaks = c("Entalpia","SH", "LH"),
                        values = c("firebrick","orange2","forestgreen")) +
    scale_x_continuous(labels=c("1" = "Mar", "2" = "Abr", "3" = "May", "4" = "Jun", "5" = "Jul", "6" = "Ago", "7" = "Sep", "8" =  "Oct", "9" = "Nov", "10" = "Dic", "11" = "Ene", "12" = "Feb"), breaks = seq(1, 12, by = 1)) +
    scale_y_continuous(breaks = seq(0, 60, by = 10), limits = c(0, 60)) +
    
    ggtitle(titulo) +
    ylab("kJ/kg") + xlab("")+
    theme(axis.text.y   = element_text(size = 14, color = "black"), axis.text.x   = element_text(size = 14, color = "black"), axis.title.y  = element_text("ºC"),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          panel.ontop = F,
          plot.title = element_text(hjust = 0.5, size = 18),
          legend.position = "bottom", legend.key.width = unit(2, "cm"), legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 12)) 
  
  ggsave(paste(getwd(), "/Salidas/TP3/Hlon_an/",nombre.fig,".jpg",sep =""), plot = g, width = 20, height = 10  , units = "cm")
  
                         
}



RhQ = function(rh, p, t){
  
  #t[which(t<0 | t >40)] = NA
  p = p*100
  
  #q = 0.622/((p)/((rh/100)*10**(((0.7859 + 0.03477*t)/(1+0.00412*t))+2))-0.378)
  
  #algo falla de la de arriba. nse que
  # vamos por partes...
  e.sat = 10**(((0.7859 + 0.03477*t)/(1+0.00412*t))+2)
  den = (rh/100)*e.sat
  q = 0.622/((p/den)-0.378)
  return(q) 
  
}


ComMT3 = function(data){
  
  aux = array(apply(data, c(1,2), mean, na.rm = T), dim = c(144,73,1))
  aux = array(aux, dim = c(144,73,r)); aux = aux[,ncol(aux):1,]
  aux = array(aux, dim = c(144,73,1))
  mask = aux
  mask[which(!is.na(mask))] = 0
  mask[which(is.na(mask))] = 1
  mask[which(mask == 1)] = NA
  
  
  V= list(); V[[1]] = aux; V[[2]] = mask
  return(V)
  
}


ComMT3_2 = function(data){
  
  aux = array(data, dim = c(144,73,2))
  aux = array(aux, dim = c(144,73,2)); aux = aux[,ncol(aux):1,]
  aux = array(aux, dim = c(144,73,2))
  aux1 = aux[,,1]
  mask = aux[,,2]
  mask[which(!is.na(mask))] = 0
  mask[which(is.na(mask))] = 1
  mask[which(mask == 1)] = NA
  
  
  V= list(); 
  V[[1]] = array(aux1, dim = c(dim(aux1),1))
  V[[2]] = array(mask, dim = c(dim(mask),1))
  return(V)
  
}


#### ENTALPIAs #####
EntalpiaHR = function(t, hr, p){
  
  t[which(t<0 | t > 40)] = NA
  # OBS, usando HR
  p = p*100
  q = 0.622/((p)/((hr/100)*10**(((0.7859 + 0.03477*t)/(1+0.00412*t))+2))-0.378)
  H.obs = (1.007*t + 0.026) + (2502 - 0.538*t)*q
  q.porc = ((2502 - 0.538*t)*q/H.obs)*100
  V = list()
  V[[1]] = H.obs
  V[[2]] = array(apply(q.porc, c(1,2), mean, na.rm = T), dim = c(dim(H.obs[,,1]), 1))
  V[[3]] = array(apply(H.obs, c(1,2), mean, na.rm = T), dim = c(dim(H.obs[,,1]), 1))
  return(V)
  
}

EntalpiaQ = function(t){
  if(t == "t6.his"){
    load("RDatas/TP3.RDatas/t6.his.RData")
    load("RDatas/TP3.RDatas/hu6.his.RData")
    t.aux = AnualMeanR(t6.his[[3]])
    hu.aux = AnualMeanR(hu6.his[[3]])
    rm(t6.his, hu6.his)
    
  } else if(t == "t6.26_49") {
    load("RDatas/TP3.RDatas/t6.26_49.RData")
    load("RDatas/TP3.RDatas/hu6.26_49.RData")
    t.aux = AnualMeanR(t6.26_2049[[3]])
    hu.aux = AnualMeanR(hu6.26_2049[[3]])
    rm(t6.26_2049, hu6.26_2049)
    
  } else if(t == "t6.26_99") {
    load("RDatas/TP3.RDatas/t6.26_99.RData")
    load("RDatas/TP3.RDatas/hu6.26_99.RData")
    t.aux = AnualMeanR(t6.26_2099[[3]])
    hu.aux = AnualMeanR(hu6.26_2099[[3]])
    rm(t6.26_2099, hu6.26_2099)
    
  } else if(t == "t6.85_49") {
    load("RDatas/TP3.RDatas/t6.85_49.RData")
    load("RDatas/TP3.RDatas/hu6.85_49.RData")
    t.aux = AnualMeanR(t6.85_2049[[3]])
    hu.aux = AnualMeanR(hu6.85_2049[[3]])
    rm(t6.85_2049, hu6.85_2049)
    
  } else if(t == "t6.85_99") {
    load("RDatas/TP3.RDatas/t6.85_99.RData")
    load("RDatas/TP3.RDatas/hu6.85_99.RData")
    t.aux = AnualMeanR(t6.85_2099[[3]])
    hu.aux = AnualMeanR(hu6.85_2099[[3]])
    rm(t6.85_2099, hu6.85_2099)
    
  } else if(t == "t5.26_49") {
    load("RDatas/TP3.RDatas/t5.26_49.RData"); load("RDatas/TP3.RDatas/hu5.26_49.RData")
    
    t.aux = t5.26_49[[1]]
    hu.aux = hu5.26_49[[1]]
    rm(t5.26_49, hu5.26_49)
    
  } else if(t == "t5.26_99") {
    load("RDatas/TP3.RDatas/t5.26_99.RData");load("RDatas/TP3.RDatas/hu5.26_99.RData")
    
    t.aux = t5.26_99[[1]]
    hu.aux = hu5.26_99[[1]]
    rm(t5.26_99, hu5.26_99)
    
  } else if(t == "t5.85_99") {
    load("RDatas/TP3.RDatas/t5.85_99.RData"); load("RDatas/TP3.RDatas/hu5.85_99.RData")
    
    t.aux = t5.85_99[[1]]
    hu.aux = hu5.85_99[[1]]
    rm(t5.85_99, hu5.85_99)
    
  } else if(t == "t5.his"){
    load("RDatas/TP3.RDatas/t5.his.RData"); load("RDatas/TP3.RDatas/hu5.his.RData")
    
    t.aux = t5.his[[1]]
    hu.aux = hu5.his[[1]]
    rm(t5.his, hu5.his)
    
  } else if(t == "t5.85_49"){
    load("RDatas/TP3.RDatas/t5.85_49.RData"); load("RDatas/TP3.RDatas/hu5.85_49.RData")
    
    t.aux = t5.85_49[[1]]
    hu.aux = hu5.85_49[[1]]
    rm(t5.85_49, hu5.85_49)
  }
  
  t.aux = t.aux-273
  t.aux[which(t.aux<0 | t.aux > 40)] = NA
  Ha = 1.007*(t.aux) - 0.026
  if(length(hu.aux) == 1 ){
    Hv = 0
  } else {
    if(length(t.aux[1,1,1,]) == 1){
      Hv = array((hu.aux[,,,1])*(2502 - 0.538*(t.aux[,,,1])), dim = c(dim(Ha)))
    } else {
      Hv = (hu.aux[,,,1:length(t.aux[1,1,1,])])*(2502 - 0.538*(t.aux))
    }
  }
  
  H.mods = Ha + Hv
  q.porc = (Hv/H.mods)*100
  rm(Ha, Hv)
  V = list()
  V[[1]] = H.mods
  V[[2]] = array(apply(q.porc, c(1,2), mean, na.rm = T), dim = c(dim(H.mods[,,1,1]),1)) #ensamble, en array para mapa_topo3
  V[[3]] = array(apply(H.mods, c(1,2), mean, na.rm = T), dim = c(dim(H.mods[,,1,1]),1)) #ensamble, en array para mapa_topo3
  return(V)
  
}



EntalpiaQ2 = function(t, q, seasons = T){
  
  if(seasons){
    t= t-273
    t[which(t<0 | t > 40)] = NA
    
    Ha = 1.007*(t) - 0.026
    
    Hv = array(q[,,,1:length(t[1,1,1,])]*(2502 - 0.538*t), dim = c(dim(Ha)))
    
    H = Ha + Hv
    V = list()
    V[[1]] = array(apply(H, c(1,2,3), mean, na.rm = T), dim = c(144,73,4))
    V[[2]] = array(apply(Ha, c(1,2,3), mean, na.rm = T), dim = c(144,73,4))
    V[[3]] = array(apply(Hv, c(1,2,3), mean, na.rm = T), dim = c(144,73,4))
    return(V)
  } else {
    
    t= t-273
    t[which(t<0 | t > 40)] = NA
    
    Ha = 1.007*(t) - 0.026
    
    Hv = array(q*(2502 - 0.538*t), dim = c(dim(Ha)))
    
    H = Ha + Hv
    V = list()
    V[[1]] = array(H, dim = c(144,73,1))
    V[[2]] = array(Ha, dim = c(144,73,1))
    V[[3]] = array(Hv, dim = c(144,73,1))
    V[[4]] = H
    V[[5]] = Ha
    V[[6]] = Hv
    return(V)
  }
  
  
}

#### Energia ####


Lv= function(t){
  # "L" = Lv (original, Henderson - Sellers 1984)
  if(mean(t)>273){
    t = t - 273
  }
  
  t[which(t<0 | t >40)] = NA
  
  Lv = 2502 - 2.378*t
  return(Lv)
  
}

S = function(t){
  # s = cp.T + geopot, si geopot ~ 0 --> cp.T ~ 1.007*T - 0.0026
  if(mean(t)>273){
    t = t - 273
  }
  
  t[which(t<0 | t >40)] = NA
  
  s = 1.007*t - 0.026
  
  return(s)
  
}
#### Tabla7.1 ####


Tabla7.1 = function(pp, evap, nombre, salida){
  #Tabla 7.1 Pexioto.
  
  lat = read.table("lat.txt") # o seq(-90,90, by = 2.5).. es lo mismo
  lat.breaks = seq(-90, 90, by = 10)
  lat2 = array(data = NA, c(length(lat.breaks)-1,5))
  for(i in 1:(length(lat.breaks)-1)){
    lat2[i,]  = seq(which(lat == lat.breaks[i]),which(lat == lat.breaks[i+1]))
  }
  
  lat = read.table("lat.txt")
  lats = array(data = t(array(data = cos((lat*pi)/180)[,1], c(73,144))), c(144,73,30))
  
  pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)*lats
  evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)*lats
  
  p_e = apply(pp_ens - evap_ens, c(1,2), mean, na.rm = T)
  ep = apply(evap_ens/pp_ens, c(1,2), mean, na.rm = T)
  p_ep = apply((pp_ens - evap_ens)/pp_ens, c(1,2), mean, na.rm = T)
  p = apply(pp_ens, c(1,2), mean, na.rm = T)
  e = apply(evap_ens, c(1,2), mean, na.rm = T)
  
  mask = as.matrix(read.table("mask.txt"))
  mask2 = mask; mask2[which(is.na(mask2))] = 2; mask2[which(mask2==1)] = NA; mask2[which(mask2==2)] = 1
  p_c = p*mask
  e_c = e*mask
  p_ep_c = p_ep*mask
  p_e_c = p_e*mask
  ep_c = ep*mask
  
  p_e_o = p_e*mask2
  
  
  # por latitudes
  aux = c("80-90S", "70-80S", "60-70S", "50-60S", "40-50S", "30-40S", "20-30S", "10-20S", "0-10S"
          , "0-10N", "10-20N", "20-30N", "30-40N", "40-50N", "50-60N", "60-70N", "70-80N", "80-90N" )
  tabla = data.frame(P = seq(1, length(lat.breaks)-1), E = NA, P_E = NA, EP = NA, P_EP = NA, row.names = aux)
  
  for(i in 1:(length(lat.breaks)-1)){
    tabla[i,1] = round(mean(p[,lat2[i,]]), digits = 2) 
    tabla[i,2] = round(mean(e[,lat2[i,]]), digits = 2) 
    tabla[i,3] = round(mean(p_e[,lat2[i,]]), digits = 2)
    tabla[i,4] = round(mean(ep[,lat2[i,]]), digits = 2)
    tabla[i,5] = round(mean(p_ep[,lat2[i,]]), digits = 2)
    
  }
  
  
  # Por hemisferio y global
  tabla2 = data.frame(P = c(1,2,3), E = NA, P_E = NA, EP = NA, P_EP = NA, row.names = c("HS", "HN", "Global"))
  
  tabla2[1,1] = round(mean(p[,1:37]), digits = 2) 
  tabla2[1,2] = round(mean(e[,1:37]), digits = 2) 
  tabla2[1,3] = round(mean(p_e[,1:37]), digits = 2)
  tabla2[1,4] = round(mean(ep[,1:37]), digits = 2)
  tabla2[1,5] = round(mean(p_ep[,1:37]), digits = 2)
  
  tabla2[2,1] = round(mean(p[,37:73]), digits = 2) 
  tabla2[2,2] = round(mean(e[,37:73]), digits = 2) 
  tabla2[2,3] = round(mean(p_e[,37:73]), digits = 2)
  tabla2[2,4] = round(mean(ep[,37:73]), digits = 2)
  tabla2[2,5] = round(mean(p_ep[,37:73]), digits = 2)
  
  tabla2[3,1] = round(mean(p), digits = 2) 
  tabla2[3,2] = round(mean(e), digits = 2) 
  tabla2[3,3] = round(mean(p_e), digits = 2)
  tabla2[3,4] = round(mean(ep), digits = 2)
  tabla2[3,5] = round(mean(p_ep), digits = 2)
  
  tabla3 = data.frame(P = c(1,2,3,4,5), E = NA, P_E = NA, EP = NA, P_EP = NA, row.names = c("SA", "NA", "EA", "AF", "OC"))
  tabla3[1,1] = round(mean(p_c[110:132,1:45], na.rm = T), digits = 2) 
  tabla3[1,2] = round(mean(e_c[110:132,1:45], na.rm = T), digits = 2) 
  tabla3[1,3] = round(mean(p_e_c[110:132,1:45], na.rm = T),  digits = 2)
  tabla3[1,4] = round(mean(ep_c[110:132,1:45], na.rm = T),  digits = 2)
  tabla3[1,5] = round(mean(p_ep_c[110:132,1:45], na.rm = T),  digits = 2)
  
  tabla3[2,1] = round(mean(p_c[75:130,45:73], na.rm = T),  digits = 2) 
  tabla3[2,2] = round(mean(e_c[75:130,45:73], na.rm = T),  digits = 2) 
  tabla3[2,3] = round(mean(p_e_c[75:130,45:73], na.rm = T),  digits = 2)
  tabla3[2,4] = round(mean(ep_c[75:130,45:73], na.rm = T),  digits = 2)
  tabla3[2,5] = round(mean(p_ep_c[75:130,45:73], na.rm = T),  digits = 2)
  
  tabla3[3,1] = round(mean(p_c[1:75,40:73], na.rm = T),  digits = 2) 
  tabla3[3,2] = round(mean(e_c[1:75,40:73], na.rm = T),  digits = 2) 
  tabla3[3,3] = round(mean(p_e_c[1:75,40:73], na.rm = T),  digits = 2)
  tabla3[3,4] = round(mean(ep_c[1:75,40:73], na.rm = T),  digits = 2)
  tabla3[3,5] = round(mean(p_ep_c[1:75,40:73], na.rm = T),  digits = 2)
  
  tabla3[4,1] = round(mean(p_c[1:20,1:55], na.rm = T),  digits = 2) 
  tabla3[4,2] = round(mean(e_c[1:20,1:55], na.rm = T),  digits = 2) 
  tabla3[4,3] = round(mean(p_e_c[1:20,1:55], na.rm = T),  digits = 2)
  tabla3[4,4] = round(mean(ep_c[1:20,1:55], na.rm = T),  digits = 2)
  tabla3[4,5] = round(mean(p_ep_c[1:20,1:55], na.rm = T),  digits = 2)
  
  tabla3[5,1] = round(mean(p_c[40:80,1:50], na.rm = T),  digits = 2) 
  tabla3[5,2] = round(mean(e_c[40:80,1:50], na.rm = T),  digits = 2) 
  tabla3[5,3] = round(mean(p_e_c[40:80,1:50], na.rm = T),  digits = 2)
  tabla3[5,4] = round(mean(ep_c[40:80,1:50], na.rm = T),  digits = 2)
  tabla3[5,5] = round(mean(p_ep_c[40:80,1:50], na.rm = T),  digits = 2)
  
  
  
  V = list()
  V[[1]] = rbind(tabla2, 0, tabla)
  V[[2]] = tabla3
  V[[3]] = p_e_c
  V[[4]] = p_e_o
  V[[5]] = p_e
  
  colnames(tabla) = c("P", "E", "P-E", "E/P", "(P-E)/P")
  colnames(tabla2) = c("P", "E", "P-E", "E/P", "(P-E)/P")
  colnames(tabla3) = c("P", "E", "P-E", "E/P", "(P-E)/P")
  tabla = as.table(as.matrix(tabla))
  tabla2 = as.table(as.matrix(tabla2))
  tabla3 = as.table(as.matrix(tabla3))
  write.table(x = tabla, file = paste(getwd(), salida, nombre,".csv", sep = ""), sep = "  ")
  write.table(x = tabla2, file = paste(getwd(), salida, nombre,"global.csv", sep = ""), sep = "  ")
  write.table(x = tabla3, file = paste(getwd(), salida, nombre,"continental.csv", sep = ""), sep = "  ")
  
  print(paste("Tablas guardadas como", paste(nombre,".csv", sep = ""), ", ", paste(nombre,"global.csv", sep =""), " y ", paste(nombre,"continental.csv", sep =""), "en ", salida), sep = "")
  
  return(V)
  
}




Tabla7.1Grafico = function(tabla,v = 3, limites, global = F, limites2 = NULL, escala2 = F, titulo, nombre, salida, width = 20, height = 20){
  library(ggplot2)
  ruta = getwd()
 
  l = vector(length = 20)
  i = 0
  
  while(length(l)>=15){
    stp = 50+50*i
    breaks.mm =  seq(limites[1], limites[2], by = stp)
    l = breaks.mm
    i = i + 1
    print(length(l))
  }

  

  aux = cbind(tabla, lats =  seq(-13, 8, by = 1))
  g = ggplot(data = aux, aes(x = aux[,v], y = lats, fill = aux[,v] > 0)) +
    geom_bar(stat = "identity", show.legend = F) +
    scale_fill_manual(values = c("tan3", "steelblue3" )) +
    scale_y_continuous(labels=c("-13" = "HS", "-12" = "HN", "-11" = "Global", "-10" = "" ,"-9" = "80-90S", "-8" = "70-80S", "-7" = "60-70S", "-6" = "50-60S", "-5" = "40-50S",
                                "-4" = "30-40S", "-3" = "20-30S", "-2" = "10-20S", "-1" = "0-10S", 
                                "0" = "0-10N", "1" = "10-20N", "2" = "20-30N", "3" = "30-40N", "4" = "40-50N", "5" = "50-60N", 
                                "6" = "60-70N", "7" = "70-80N", "8" = "80-90N"),breaks = seq(-13, 8, by = 1))+
  scale_x_continuous(breaks = breaks.mm, limits = limites) +
    
    theme_minimal() +
    ylab("")+xlab("mm")+ ggtitle(titulo)+
  theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
        axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.ontop = F,
        plot.title = element_text(hjust = 0.5))
  
  ggsave(paste(ruta, salida, nombre, ".jpg", sep = ""), plot = g, width = width, height = height, units = "cm")
  
 
  if(global == T){
    
    aux = aux[1:3,]
    # para la parte global
    g = ggplot(data = aux, aes(x = aux[,v], y = lats, fill = aux[,v] > 0)) +
      geom_bar(stat = "identity", show.legend = F) +
      scale_fill_manual(values = c("tan3", "steelblue3" )) +
      scale_y_continuous(labels=c("-13" = "HS", "-12" = "HN", "-11" = "Global", "-10" = "" ,"-9" = "80-90S", "-8" = "70-80S", "-7" = "60-70S", "-6" = "50-60S", "-5" = "40-50S",
                                  "-4" = "30-40S", "-3" = "20-30S", "-2" = "10-20S", "-1" = "0-10S", 
                                  "0" = "0-10N", "1" = "10-20N", "2" = "20-30N", "3" = "30-40N", "4" = "40-50N", "5" = "50-60N", 
                                  "6" = "60-70N", "7" = "70-80N", "8" = "80-90N"),breaks = seq(-13, 8, by = 1)) 
    if(escala2 == T){
      
      l = vector(length = 20)
      i = 0
      
      while(length(l)>15){
        stp = 10+10*i
        breaks.mm2 =  seq(limites2[1], limites2[2], by = stp)
        l = breaks.mm2
        i = i + 1
      }
      
      g = g + scale_x_continuous(breaks = breaks.mm2, limits = limites2) 
    } else {
      
      g = g + scale_x_continuous(breaks = breaks.mm, limits = limites) 
    }
    
    g = g +  theme_minimal() +
      ylab("")+xlab("mm")+ ggtitle(titulo) +
      theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
            axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            panel.border = element_rect(colour = "black", fill = NA, size = 1),
            panel.ontop = F,
            plot.title = element_text(hjust = 0.5))
    ggsave(paste(ruta, salida, nombre, "2", ".jpg", sep = ""), plot = g, width = width, height = height, units = "cm")
  } 
  
} 


Tabla7.1Grafico_Continental = function(tabla,v = 3, limites, titulo, nombre, salida, width = 20, height = 20){
  library(ggplot2)
  ruta = getwd()
  
  l = vector(length = 20)
  i = 0
  
  while(length(l)>=15){
    stp = 50+50*i
    breaks.mm =  seq(limites[1], limites[2], by = stp)
    l = breaks.mm
    i = i + 1
    print(length(l))
  }
  
  
  
  aux = cbind(tabla, lats =  seq(1, 5, by = 1))
  g = ggplot(data = aux, aes(x = aux[,v], y = lats, fill = aux[,v] > 0)) +
    geom_bar(stat = "identity", show.legend = F) +
    scale_fill_manual(values = c("steelblue3", "tan3" )) +
    scale_y_continuous(labels=c( "1" = "Sudamerica", "2" = "Norte America", "3" = "Eurasia", "4" = "Africa", "5" = "Oceania"),breaks = seq(1, 5, by = 1))+
    scale_x_continuous(breaks = breaks.mm, limits = limites) +
    
    theme_minimal() +
    ylab("")+xlab("mm")+ ggtitle(titulo)+
    theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
          axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          panel.ontop = F,
          plot.title = element_text(hjust = 0.5))
  
  ggsave(paste(ruta, salida, nombre, "continental.jpg", sep = ""), plot = g, width = width, height = height, units = "cm")
  
}  




#### PlotLat ####

PlotLat = function(pp, evap, titulo, y.breaks, nombre, salida){
  
  library(ggplot2)
  library(metR)
  
  limite = c(min(y.breaks), max(y.breaks))
  
  pp_ens = apply(pp, c(1,2,3), mean, na.rm = T)   
  evap_ens = apply(evap, c(1,2,3), mean, na.rm = T)
  
  datos = array(NA, c(73,5))
  
  pp.lat = apply(pp_ens, c(2), mean, na.rm = T)
  evap.lat = apply(evap_ens, c(2), mean, na.rm = T)   
  
  datos = cbind(pp.lat ,evap.lat)
  datos = cbind(datos,  apply(pp_ens*mask, c(2), mean, na.rm = T)); datos = cbind(datos,  apply(evap_ens*mask, c(2), mean, na.rm = T))
  datos = cbind(datos,  apply(pp_ens*mask2, c(2), mean, na.rm = T)); datos = cbind(datos,  apply(evap_ens*mask2, c(2), mean, na.rm = T))
  datos = cbind(datos, lat)
  
  colnames(datos) = c("P", "E", "P_C", "E_C", "P_O", "E_O", "Lat")
  
  g = ggplot(data = datos, aes(x = Lat)) + 
    geom_line(aes(y = P, colour = "Global"), size = 1) + 
    geom_line(aes(y = P_O, colour = "Oceanico"), size = 1) + 
    geom_line(aes(y = P_C, colour = "Continental"), size = 1)+
    scale_colour_manual("", 
                        breaks = c("Global", "Oceanico", "Continental"),
                        values = c("Black", "royalblue", "forestgreen")) +
    scale_x_latitude(breaks = seq(-90, 90, by = 15)) +
    scale_y_continuous(breaks = y.breaks, limits = limite) +
    ggtitle(paste("Precipitación", titulo)) +
    ylab("") +
    theme_minimal() +
    theme(axis.text.y   = element_text(size = 14, color = "black"), axis.text.x   = element_text(size = 14, color = "black"), axis.title.y  = element_text("ºC"),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          panel.ontop = F,
          plot.title = element_text(hjust = 0.5, size = 18),
          legend.position = "bottom", legend.key.width = unit(1, "cm"), legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 15)) 
  
  ggsave(paste(getwd(), salida,"P", nombre,".jpg",sep =""), plot = g, width = 20, height = 10  , units = "cm")
  
  g = ggplot(data = datos, aes(x = Lat)) + 
    geom_line(aes(y = E, colour = "Global"), size = 1) + 
    geom_line(aes(y = E_O, colour = "Oceanico"), size = 1) + 
    geom_line(aes(y = E_C, colour = "Continental"), size = 1)+
    scale_colour_manual("", 
                        breaks = c("Global", "Oceanico", "Continental"),
                        values = c("Black", "royalblue", "forestgreen")) +
    scale_x_latitude(breaks = seq(-90, 90, by = 15)) +
    scale_y_continuous(breaks = y.breaks, limits = limite) +
    ggtitle(paste("Evaporación", titulo)) +
    ylab("") +
    theme_minimal() +
    theme(axis.text.y   = element_text(size = 14, color = "black"), axis.text.x   = element_text(size = 14, color = "black"), axis.title.y  = element_text("ºC"),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          panel.ontop = F,
          plot.title = element_text(hjust = 0.5, size = 18),
          legend.position = "bottom", legend.key.width = unit(1, "cm"), legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 15)) 
  
  ggsave(paste(getwd(), salida,"E", nombre,".jpg",sep =""), plot = g, width = 20, height = 10  , units = "cm")
  
  
}
#### PlotTs ####
PlotTs = function(datos, escala,escala2,  titulo, nombre){
  library(ggplot2)
  
  colnames(datos) = c("PP", "TendenciaP", "Años", "E", "TendenciaE")
  g = ggplot(datos, aes(x = Años))+ theme_minimal()+
    geom_line(aes(y = PP, colour = "P"), size = 1) +
    geom_line(aes(y = TendenciaP), show.legend = F, color = "steelblue3") + 
    geom_line(aes(y = E, colour = "E"), size = 1) +
    geom_line(aes(y = TendenciaE), show.legend = F, color = "orange2")+
    scale_colour_manual("", 
                        breaks = c("P", "Tend P", "E", "Tend E"),
                        values = c("steelblue3","steelblue3", "orange2", "orange2"))+
    scale_y_continuous(breaks = escala, limits = c(min(escala), max(escala)))+
  ylab("mm")+ ggtitle(titulo)+
    theme(axis.text.y   = element_text(size = 14, color = "black"), axis.text.x   = element_text(size = 14, color = "black"), axis.title.y  = element_text("ºC"),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          panel.ontop = F,
          plot.title = element_text(hjust = 0.5, size = 18),
          legend.position = "bottom", legend.key.width = unit(1, "cm"), legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 15)) 
  
  ggsave(paste(getwd(), "/Salidas/TP4/Tend/",nombre,".jpg",sep =""), plot = g, width = 20, height = 10  , units = "cm")
  
g = ggplot(datos, aes(x = Años))+ theme_minimal()+
    geom_line(aes(y = PP-E, colour = "P-E"), size = 1) +
    geom_hline(yintercept = 0) +
    scale_colour_manual("", 
                        breaks = c("P-E"),
                        values = c("darkorchid4")) +
    scale_y_continuous(breaks = escala2, limits = c(min(escala2), max(escala2)))+
    ylab("mm")+ ggtitle(titulo)+
    theme(axis.text.y   = element_text(size = 14, color = "black"), axis.text.x   = element_text(size = 14, color = "black"), axis.title.y  = element_text("ºC"),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          panel.ontop = F,
          plot.title = element_text(hjust = 0.5, size = 18),
          legend.position = "bottom", legend.key.width = unit(1, "cm"), legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 15)) 
  
  ggsave(paste(getwd(), "/Salidas/TP4/Tend/",nombre,"resta.jpg",sep =""), plot = g, width = 20, height = 10  , units = "cm")
  
}  