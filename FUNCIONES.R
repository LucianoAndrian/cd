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
    
  } else {
    
    
    V =  array(data = NA, dim = c(144, 73, 4, r)) # --> quedan los miembros. (se puede hacer el ensamble en el futuro, operando sobre V (o cambiar todo..))
    V2 = array(data = NA, dim = c(144, 73, 4, 29, r))  ## --> por si es necesario mas adelante.
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
    
  }
  
  W = list()
  W[[1]] = V
  W[[2]] = V2
  return(W)
}
    


#### MAPA ####
mapa = function(lista, titulo, nombre_fig, escala, label_escala, resta, brewer, revert, niveles, contour, lon, lat, escala_dis, breaks_c_f, r, salida){
  
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
    
    if(revert == "si"){
      if(contour == "si"){
        g = ggplot() + theme_minimal()+
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
          
          geom_tile(data = data, aes(x = lon, y = lat, fill = temp), alpha=0.8, na.rm = T) +
          
          geom_contour_fill(data = data, aes(x = lon, y = lat, z = temp),alpha = 1, na.fill = -10000, breaks = breaks_c_f) +
          
          scale_fill_gradientn(limits = escala, name = label_escala, colours = rev(brewer.pal(n=niveles,brewer)), na.value = "white", guide = "legend", breaks = escala_dis) +
          
          guides(fill = guide_legend(reverse = TRUE)) +
          
          geom_polygon(data = mapa, aes(x = long,y = lat, group =group),fill = NA, color = "black") +
          ggtitle(paste(titulo, " r" , num[i], sep = ""))+
          scale_x_discrete(limits = seq(0,360, by = 60))+
          scale_y_discrete(limits = seq(-90, 90, by = 30))+
          theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill = NA, size = 3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust = 0.5))
        
        
        ggsave(paste(ruta, salida, nombre_fig, "_r", num[i], ".jpg", sep = ""), plot = g, width = 30, height = 15  , units = "cm")
      } else {
        g = ggplot() + theme_minimal() +
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank())+
          
          geom_tile(data=data,aes(x = lon, y = lat,fill = temp),alpha=0.9, na.rm = T) +
          
          #geom_contour_fill(data=data, aes(x = lon, y= lat, z = temp),alpha=1, na.fill = -10000, breaks = breaks_c_f)
          
          scale_fill_gradientn(limits = escala, name = label_escala, colours = rev(brewer.pal(n = niveles, brewer)), na.value = "white", guide = "legend", breaks = escala_dis) +
          
          guides(fill = guide_legend(reverse = TRUE)) +
          
          geom_polygon(data = mapa, aes(x = long,y = lat, group =group),fill = NA, color = "black") +
          ggtitle(paste(titulo, " r" , num[i], sep = ""))+
          scale_x_discrete(limits = seq(0,360, by = 60))+
          scale_y_discrete(limits = seq(-90, 90, by = 30))+
          theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill = NA, size = 3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust = 0.5))
        
        
        ggsave(paste(ruta, salida, nombre_fig, "_r", num[i], ".jpg", sep = ""), plot = g, width = 30, height = 15  , units = "cm")
        
      }
      
    } else {
      if(contour == "si"){
        
        g = ggplot() + theme_minimal() +
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
          
          geom_tile(data = data,aes(x = lon, y = lat, fill = temp), alpha = 0.9, na.rm = T) +
          
          geom_contour_fill(data = data, aes(x = lon, y = lat, z = temp), alpha = 1, na.fill = -10000, breaks = breaks_c_f) +
          
          #stat_subset(data=data, aes(x = lon, y = lat, z = temp, subset = temp <= rc), shape = 20, size = 1, color = "black", alpha = 0.3, geom = "point")+
          #geom_contour(data = data, aes(x = lon, y = lat, z = temp), color = "blue", size = 0.666, breaks = rc )+
          
          scale_fill_gradientn(limits = escala, name = label_escala, colours = brewer.pal(n = niveles,brewer), na.value = "white", guide = "legend",breaks = escala_dis) +
          
          guides(fill = guide_legend(reverse = TRUE)) +
          
          geom_polygon(data = mapa, aes(x = long,y = lat, group =group),fill = NA, color = "black") +
          ggtitle(paste(titulo, " r" , num[i], sep = ""))+
          scale_x_discrete(limits = seq(0,360, by = 60))+
          scale_y_discrete(limits = seq(-90, 90, by = 30))+
          theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill = NA, size = 3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust = 0.5))
        
        
        ggsave(paste(ruta, salida, nombre_fig, "_r", num[i], ".jpg", sep = ""), plot = g, width = 30, height = 15  , units = "cm")
      } else {
        g = ggplot() + theme_minimal() +
          xlab("Longitud") + ylab("Latitud") + 
          theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank()) +
          
          geom_tile(data = data,aes(x = lon, y = lat,fill = temp),alpha = 0.9, na.rm = T) +
          
          #geom_contour_fill(data=data,aes(x = lon, y= lat, z = temp),alpha=1, na.fill = -10000)+
          
          scale_fill_gradientn(limits = escala,name = label_escala, colours = brewer.pal(n = niveles, brewer), na.value = "white", guide = "legend",breaks = escala_dis)+
          
          guides(fill = guide_legend(reverse = TRUE))+
          
          geom_polygon(data = mapa, aes(x = long,y = lat, group =group),fill = NA, color = "black") +
          ggtitle(paste(titulo, " r" , num[i], sep = ""))+
          scale_x_discrete(limits = seq(0,360, by = 60))+
          scale_y_discrete(limits = seq(-90, 90, by = 30))+
          theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
                axis.title.x  = element_text(size = 14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill = NA, size = 3),
                panel.ontop = TRUE,
                plot.title = element_text(hjust = 0.5))
        
        
        ggsave(paste(ruta, salida, nombre_fig, "_r", num[i], ".jpg", sep = ""), plot = g, width = 30, height = 15  , units = "cm")
        
      }
      
    }
  }
  
}



  
  