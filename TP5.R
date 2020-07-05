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

v = v*lats # pesa todas las variable por la latitud

v.mean = apply(v, c(1,2,4,5,6,7), mean)

variable = c(1,3,4)

v.mean.g = array(NA, c(144,73,rs[m],2,3,2))
for(rcp in 1:2){
  for(i in 1:3){
    
    v.mean.g[,,,rcp,i,1] = v.mean[,,,2,rcp,variable[i]] - v.mean[,,,1,rcp,variable[i]]
    v.mean.g[,,,rcp,i,2] = v.mean[,,,3,rcp,variable[i]] - v.mean[,,,1,rcp,variable[i]]
    
  }
}


lat2 = as.matrix(read.table("lat.txt"))
lon2 = as.matrix(read.table("lon.txt"))
rcp.name = c("RCP2.6", "RCP8.5")
periodo = c("F. Cercano - P. Historico", "F. Lejano - P. Historico")
periodo.fig = c("49", "99")
variable = c("T", "PP", "ET")
colorbar = c("YlOrRd", "BrBG", "PRGn")
escala = list(); escala[[1]] = seq(0, 4, 0.5); escala[[2]] = seq(-150, 150, by = 25); escala[[3]] = seq(-150, 150, by = 25)
label.escala = c("ºC", "mm", "mm")
revert = c(FALSE, FALSE, FALSE)
na.fill = c(-1000, 0, 0)

mask = as.matrix(read.table("mask.txt"))
mask.arr = array(mask, c(dim(v.mean.g)))
v.mean.g = apply(v.mean.g, c(1,2,4,5,6), mean, na.rm = T)

v.mean.g = v.mean.g*array(mask, c(dim(v.mean.g)))

for(i in 1:3){
  for(rcp in 1:2){
    for(p in 1:2){
      
      aux = array(v.mean.g[,,rcp,i,p], dim = c(144,73,1))
      
      mapa_topo3(variable = aux, lon = lon2, lat = lat2, colorbar = colorbar[i], revert = revert[i],
                 escala = escala[[i]], label.escala = label.escala[i],
                 titulo = paste(variable[i], " - Diferencia ", periodo[p]," "," CNRM-", toupper(modelos[m]), " ", rcp.name[rcp], sep = ""), colorbar.pos = "bottom",
                 mapa = "mundo", na.fill = na.fill[i], nombre.fig = paste(variable[i], toupper(modelos[m]), periodo.fig[p], "_", rcp.name[rcp], sep = ""),
                 salida = "/Salidas/TP5/dif/", width = 30, height = 20)
      
      
    }
  }
}



#rec v = 144,73,30,r,3,2,5
# 3 periodos
# 2 escenarios (el periodo his es el mismo en los dos)
# 5 variables- 1 temp, 2 huss, 3 pp, 4 et, 5 etp

aux.corr = array(data = NA, dim = c(144,73,rs[m],2,3,2))
for(p in 1:3){
  for( r in 1:rs[m]){
    for(rcp in 1:2){
      
      aux.corr[,,r,rcp,p,] = corr2.0(mod = v[,,,r,p,rcp,4], obs = v[,,,r,p,rcp,1], lon = 144, lat = 73, cf = 0.95)*array(mask, dim = c(dim(mask), 2))
      
    }
  }
}


aux.corr2 = array(data = NA, dim = c(144,73,rs[m],2,3,2))
for(p in 1:3){
  for( r in 1:rs[m]){
    for(rcp in 1:2){
      
      aux.corr2[,,r,rcp,p,] = corr2.0(mod = v[,,,r,p,rcp,4], obs = v[,,,r,p,rcp,3], lon = 144, lat = 73, cf = 0.95)*array(mask, dim = c(dim(mask), 2))
      
    }
  }
}


# da muy raro... en cnrm-cm6.




aux = aux.corr[,,,,,2]

SigRs = function(aux){
  
  sig = array(data = NA, dim = c(144,73,3,2))
  
  for(rcp in 1:2){
    for(p in 1:3){
      for(i in 1:144){
        for(j in 1:73){
          
          if(p == 1){
            
            x = aux[i,j,,rcp,p]
            if(length( x[which(x== 1)])>6){
              sig[i,j,p,rcp] = 1
            } else {
              sig[i,j,p,rcp] = NA
            }
            
          } else {
            
            if(m == 1){
              x = aux[i,j,,rcp,p]
              if(length( x[which(x== 1)])>=2){
                sig[i,j,p,rcp] = 1
              }
            } else {
              x = aux[i,j,,rcp,p]
              if(length( x[which(x== 1)])>=4){
                sig[i,j,p,rcp] = 1
              }
            }
          }
          
          
        } 
      }
    }
  }
  
  return(sig)
}

sig =  SigRs(aux = aux.corr[,,,,,2])
sig2=  SigRs(aux = aux.corr2[,,,,,2])


# config esto
lat2 = as.matrix(read.table("lat.txt"))
lon2 = as.matrix(read.table("lon.txt"))
rcp.name = c("RCP2.6", "RCP8.5")
periodo = c("Historico","Futuro Cercano", "Futuro Lejano")
periodo.n = c("his", "49", "99")

ens.corr = apply(aux.corr, c(1,2,4,5,6), mean, na.rm = T)

for(rcp in 1:2){
  for(p in 1:3){
    
    aux = array(ens.corr[,,rcp,p,1], dim = c(dim(ens.corr[,,rcp,p,1]),1))
    mask = array(sig[,,p,rcp], dim = c(144,73,1))
    mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
    mapa_topo3(variable = aux, variable.sig = mask, lon = lon2, lat = lat2
               , titulo = paste("Correlación E vs T - CNRM-", toupper(modelos[m]), " ", rcp.name[rcp], " ", periodo[p], sep = "")
               , colorbar = "RdBu", escala = seq(-1,1, by = 0.1), sig = T, color.vsig = "black", contour.fill = T, type.sig = "point2"
               , mapa = "mundo", salida = "/Salidas/TP5/", nombre.fig = paste("corrETT.", toupper(modelos[m]),"_", periodo.n[p],"_", rcp.name[rcp], sep = "")
               , alpha.vsig = 0.5, na.fill = 0, revert = T, colorbar.pos = "bottom", label.escala = ""
               , width = 30, height = 20)
  }
}




ens.corr2 = apply(aux.corr2, c(1,2,4,5,6), mean, na.rm = T)

for(rcp in 1:2){
  for(p in 1:3){
    
    aux = array(ens.corr2[,,rcp,p,1], dim = c(dim(ens.corr2[,,rcp,p,1]),1))
    mask = array(sig2[,,p,rcp], dim = c(144,73,1))
    mask[which(!is.na(mask))] = 0; mask[which(mask == 1)] = NA 
    mapa_topo3(variable = aux, variable.sig = mask, lon = lon2, lat = lat2
               , titulo = paste("Correlación E vs PP - CNRM-", toupper(modelos[m]), " ", rcp.name[rcp], " ", periodo[p], sep = "")
               , colorbar = "RdBu", escala = seq(-1,1, by = 0.1), sig = T, color.vsig = "black", contour.fill = T, type.sig = "point2"
               , mapa = "mundo", salida = "/Salidas/TP5/", nombre.fig = paste("corrETP.", toupper(modelos[m]),"_", periodo.n[p],"_", rcp.name[rcp], sep = "")
               , alpha.vsig = 0.5, na.fill = 0, revert = F, colorbar.pos = "bottom", label.escala = ""
               , width = 30, height = 20)
  }
}





aux1 = ens.corr[,,,,1]#*ens.corr[,,,,2]
aux2 = ens.corr2[,,,,1]#*ens.corr2[,,,,2]

for(rcp in 1:2){
  for(p in 1:3){
    for(i in 1:73){
      for(j in 1:144){
        x = aux1[j,i,rcp,p]>=aux2[j,i,rcp,p]
        if(!is.na(x)){
          if(x == T){
            aux2[j,i,rcp,p] = 3
          } else {
            aux2[j,i,rcp,p] = 1
          } 
        }
      }
    }
  }
}


for(rcp in 1:2){
  for(p in 1:3){
    
    aux = array(aux2[,,rcp,p], dim = c(dim(aux2[,,rcp,p]),1))
    
    
    mapa_topo3(variable = aux, variable.sig = aux2, lon = lon2, lat = lat2
               , colorbar = "RdBu", escala = seq(1.2,2.8, by = 0.8), contour.fill = T
               , titulo = paste("CNRM-", toupper(modelos[m]), " ",rcp.name[rcp]," ", periodo[p], sep = "")
               , mapa = "mundo", salida = "/Salidas/TP5/"
               , nombre.fig = paste("zonas.",toupper(modelos[m]), "_", periodo.n[p],"_", rcp.name[rcp], sep = ""), niveles = 9
               , na.fill = 0, revert = T, colorbar.pos = "bottom"
               , width = 30, height = 20)
  }
}


rm(list = ls())
.rs.restartR()



















