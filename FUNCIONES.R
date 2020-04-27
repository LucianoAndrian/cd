#### OPEN_NC ####
# abrir NCs en un array
open_nc = function(file_pattern, model, variable, members, mes_anual){
  
  # queda basica dado lo irregular de los patrones de los archivos (mas en cnrm-cm5)
  
  library(ncdf4)
  model = toupper(model)
  
  r = members
  
  ruta = paste('/home/auri/Facultad/Materias/c-dinamica/TPs/', model, "/",sep = "")
  
  t = Sys.glob(paste(ruta, file_pattern, sep = ""))
  
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
    
    
    V =  array(data = NA, dim = c(144, 73, 4, r)) # --> quedan los miembros. (se puede hacer el ensamble en el futuro, operando sobre V (o cambiar todo..))
    V2 = array(data = NA, dim = c(144, 73, 4, 29, r))  ## --> por si es necesario
    for(m in 1:r){
      
      v_nc = nc_open(t[m])
      v_v = ncvar_get(v_nc, variable)
      nc_close(v_nc)
      v = v_v[,, 3:350] 
      
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
      
    }
    
    W = list()
    W[[1]] = V
    W[[2]] = V2
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