
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

v = v*lats # pesa todas las variable por la latitud


SpatialCorr = function(v1, v2){
  
  v1.mean = mean(v1, na.rm = T)
  v2.mean = mean(v2, na.rm = T)
  
  aux1 = v1-v1.mean
  aux2 = v2-v2.mean
  n = 144*73
  num = sum(aux1*aux2, na.rm = T)
  den = n*sqrt(sum(aux1**2, na.rm = T)/n)*sqrt(sum(aux2**2, na.rm = T)/n)
  num/den
  
  return(num/den)
  
}


# corr. espacial

sc.t1 = matrix(NA, nrow = 30, ncol = 2); sc.p1 = matrix(NA, nrow = 30, ncol = 2)   # esto se podria hacer todo en una sola matriz
sc.t2 = matrix(NA, nrow = 30, ncol = 2); sc.p2 = matrix(NA, nrow = 30, ncol = 2)
sc.t3 = matrix(NA, nrow = 30, ncol = 2); sc.p3 = matrix(NA, nrow = 30, ncol = 2)

r.aux.t = vector(); r.aux.p = vector()

for(a in 1:30){
  for(rcp in 1:2){
    for(p in 1:3){
      for(r in 1:rs){
        
        r.aux.t[r] =  SpatialCorr(v1 = v[,,a,r,p,rcp,1], v2 = v[,,a,r,p,rcp,4]) 
        r.aux.p[r] =  SpatialCorr(v1 = v[,,a,r,p,rcp,3], v2 = v[,,a,r,p,rcp,4]) 
        
      } 
      
      R.t = mean(r.aux.t, na.rm = T)
      R.p = mean(r.aux.p, na.rm = T)
      
      if(p == 1){
        sc.t1[a,rcp] = R.t
        sc.p1[a,rcp] = R.p
      } else if(p == 2){
        sc.t2[a,rcp] = R.t
        sc.p2[a,rcp] = R.p
      } else {
        sc.t3[a,rcp] = R.t
        sc.p3[a,rcp] = R.p
      }
      
      
      
    }
  }
}

aux = seq(1976, 2005); aux1 = seq(2020,2049); aux2 = seq(2070, 2099)
años = c(aux, 2010 , aux1, 2060 , aux2)

temp = as.data.frame(sc.t1)
temp = rbind(temp, NA, sc.t2)
temp = rbind(temp, NA, sc.t3)
temp = cbind(temp, años)

pp = as.data.frame(sc.p1)
pp = rbind(pp, NA, sc.p2)
pp = rbind(pp, NA, sc.p3)
pp = cbind(pp, años)


PlotTsCorr = function(datos, datos2,  escala, escala2, titulo, titulo2, nombre){
  library(ggplot2)
  
  colnames(datos) = c("RCP2.6", "RCP8.5", "Años")
  colnames(datos2) = c("RCP2.6", "RCP8.5", "Años")
  
  g =  ggplot(datos, aes(x = Años))+ theme_minimal()+
    geom_line(aes(y = RCP2.6, colour = "RCP2.6"), size = 1) +
    geom_line(aes(y = RCP8.5, colour = "RCP8.5"), size = 1) +
    
    scale_colour_manual("", 
                        breaks = c("RCP2.6", "RCP8.5"),
                        values = c("turquoise3", "firebrick3")) +
    scale_y_continuous(breaks = escala, limits = c(min(escala), max(escala)))+
    
    scale_x_continuous(breaks = seq(1976, 2099, by = 20))+
    ylab("R")+ ggtitle(titulo)+xlab("")+
    theme(axis.text.y   = element_text(size = 14, color = "black"), axis.text.x   = element_text(size = 14, color = "black"), axis.title.y  = element_text("ºC"),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          panel.ontop = F,
          plot.title = element_text(hjust = 0.5, size = 18),
          legend.position = "bottom", legend.key.width = unit(2, "cm"), legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 15)) 
  
  ggsave(paste(getwd(), "/Salidas/TP5/tend/",nombre,".CorrET.jpg",sep =""), plot = g, width = 20, height = 10  , units = "cm")
  
  
  g =  ggplot(datos2, aes(x = Años)) + theme_minimal()+
    geom_line(aes(y = RCP2.6, colour = "RCP2.6"), size = 1) +
    geom_line(aes(y = RCP8.5, colour = "RCP8.5"), size = 1) +
    
    scale_colour_manual("", 
                        breaks = c("RCP2.6", "RCP8.5"),
                        values = c("springgreen", "orange2")) +
    scale_y_continuous(breaks = escala2, limits = c(min(escala2), max(escala2)))+
    
    scale_x_continuous(breaks = seq(1976, 2099, by = 20))+
    ylab("R")+ ggtitle(titulo2)+xlab("")+
    theme(axis.text.y   = element_text(size = 14, color = "black"), axis.text.x   = element_text(size = 14, color = "black"), axis.title.y  = element_text("ºC"),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          panel.ontop = F,
          plot.title = element_text(hjust = 0.5, size = 18),
          legend.position = "bottom", legend.key.width = unit(2, "cm"), legend.key.height = unit(0.5, "cm"), legend.text = element_text(size = 15)) 
  
  ggsave(paste(getwd(), "/Salidas/TP5/tend/",nombre,".CorrEP.jpg",sep =""), plot = g, width = 20, height = 10  , units = "cm") 

}  

titulo = paste("Correlación espacial E vs T - CNRM-", toupper(modelos[m]), sep = "")
titulo2 = paste("Correlación espacial E vs P - CNRM-", toupper(modelos[m]), sep = "")

escala = list(); escala[[1]] = seq(0.82, 0.85, by = 0.01); escala[[2]] = seq(0.80, 0.83, by = 0.01)
escala2 = list(); escala2[[1]] = seq(0.68, 0.75, by = 0.01); escala2[[2]] = seq(0.59, 0.66, by = 0.01)

PlotTsCorr(datos = temp, datos2 = pp, titulo = titulo, titulo2 = titulo2
           , escala = escala[[m]], escala2 = escala2[[m]], nombre = modelos[m])



Tendencia<-function(data, s,rs){
  
  aux = array(NA, c(144,73,rs,3,2,5)); aux2 = array(NA, c(144,73,rs,3,2,5))
  for(rcp in 1:2){
    for(p in 1:3){
      for(var in 1:5){
        for(i in 1:144){
          for(j in 1:73){
            for(r in 1:rs){
              
              datos = data[i,j,,r,p,rcp,var]
              
              if(length(datos[which(is.na(datos))])<5){
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
                
                
                p.val = 1 - s
                aux[i,j,r,p,rcp,var] = b
                aux2[i,j,r,p,rcp,var] = ifelse(test = p.value < p.val, yes = 1, no = NA)
                
              }
            }
          }
          V = list()
          V[[1]] = aux; V[[2]] = aux2
         }
      }
    }
  }
  
  return(V) 

}


tend = Tendencia(v,0.95,9)




aux = tend[[2]]

sig = array(data = NA, dim = c(144,73,3,2,5))

for(rcp in 1:2){
  for(p in 1:3){
    for(var in 1:5){
      for(i in 1:144){
        for(j in 1:73){
          
          if(p == 1){
            
            x = aux[i,j,,p,rcp,var]
            if(length( x[which(x== 1)])>6){
              sig[i,j,p,rcp,var] = 1
            } else {
              sig[i,j,p,rcp,var] = NA
            }
            
          } else {
            
            if(m == 1){
              x = aux[i,j,,p,rcp,var]
              if(length( x[which(x== 1)])>=2){
                sig[i,j,p,rcp,var] = 1
              }
            } else {
              x = aux[i,j,,p,rcp,var]
              if(length( x[which(x== 1)])>=4){
                sig[i,j,p,rcp,var] = 1
              }
            }
          }
          

        } 
      }
    }
  }
}






aux1 = apply(tend[[1]], c(1,2,4,5,6), mean, na.rm = T)
aux2 = sig

#> dim(aux1)
#[1] 144  73   3   2   5

lat2 = as.matrix(read.table("lat.txt"))
lon2 = as.matrix(read.table("lon.txt"))
rcp.name = c("RCP2.6", "RCP8.5")
periodo = c("Historico","Futuro Cercano", "Futuro Lejano")
periodo.n = c("his", "49", "99")
modelo = c("CNRM-CM5", "CNRM-CM6")
modelo.fig = c("5","6")
rcp.n = c("26","85")
colorbar = c("YlOrRd", "nada",  "BrBG", "PuOr", "nada")
revert = c(F, T, T, T, F) # no hace falta, F en todas
escala = list(); escala[[1]] = seq(0,2, by = 0.25); escala[[2]] = NULL; escala[[3]] = seq(-200, 200, by = 50)
escala[[4]] = seq(-100, 100, by = 20)
variable = c("Temperatura", "no va", "Precipitación", "Evapotranspiración")
variable.fig = c("T", "nada", "PP", "E", "nada")

for(p in 1:3){
  for(rcp in 1:2){
    for(var in 1:5){
      
      if(var == 2 | var == 5){
        print("no grafica hu y etp")
      } else {
        titulo = paste("Tendencia de ", variable[var], " ", periodo[p], " - ", rcp.name[rcp], "  ", modelo[m], sep = "")
        nombre.fig = paste(variable.fig[var],".", modelo.fig[m], periodo.n[p], "_", rcp.n[rcp], sep = "")
        
        aux = array(aux1[,,p,rcp,var], c(144,73,1))
        mask = array(aux2[,,p,rcp,var], c(144,73,1))
        mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
        mapa_topo3(variable = aux*30, variable.sig = mask, lon = lon2, lat = lat2, colorbar = colorbar[var], revert = F
                   , escala = escala[[var]], color.vsig = "black", alpha.vsig = 0.5, sig = T, mapa = "mundo", x.label = NULL, y.label = NULL
                   , label.escala = "", na.fill = 0, type.sig = "point2"
                   , titulo = titulo, nombre.fig = nombre.fig, width = 30, salida = "/Salidas/TP5/tend/", colorbar.pos = "bottom")
        
      }
    }
  }
}



# corr espacial para las tendencias entre T - E, P - E

et = matrix(data = NA, nrow = 2, ncol = 3)
ep = matrix(data = NA, nrow = 2, ncol = 3)

colnames(et) = c("His", "49", "99")
colnames(ep) = c("His", "49", "99")

row.names(et) = c("2.6", "8.5")
row.names(ep) = c("2.6", "8.5")


for(rcp in 1:2){
  for(p in 1:3){
    
    et[rcp, p] = SpatialCorr(aux1[,,p,rcp,1], aux1[,,p,rcp,4])
    ep[rcp, p] = SpatialCorr(aux1[,,p,rcp,3], aux1[,,p,rcp,4])

  }
}



write.table(x = et, file = paste(getwd(), "/Salidas/TP5/tend/", "ET", modelos[m],".csv", sep = ""), sep = "  ")
write.table(x = ep, file = paste(getwd(), "/Salidas/TP5/tend/", "EP", modelos[m],".csv", sep = ""), sep = "  ")



rm(list = ls())
.rs.restartR()
