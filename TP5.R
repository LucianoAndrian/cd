#TP5
# 1era parte.
library(ncdf4)
source("FUNCIONES.R")

mask = as.matrix(read.table("mask.txt"))

nc = nc_open("cnrm-cm5.nc")
v = ncvar_get(nc, "var2")
rm(nc)

#rec v = 144,73,30,r,3,2,5
# 3 periodos
# 2 escenarios (el periodo his es el mismo en los dos)
# 5 variables- 1 temp, 2 huss, 3 pp, 4 et, 5 etp

aux.corr = array(data = NA, dim = c(144,73,10,2,3,2))
for(p in 1:3){
  for( r in 1:10){
    for(rcp in 1:2){
      
      aux.corr[,,r,rcp,p,] = corr2.0(mod = v[,,,r,p,rcp,4], obs = v[,,,r,p,rcp,1], lon = 144, lat = 73, cf = 0.95)*array(mask, dim = c(dim(mask), 2))
      
    }
  }
}


aux.corr2 = array(data = NA, dim = c(144,73,10,2,3,2))
for(p in 1:3){
  for( r in 1:9){
    for(rcp in 1:2){
      
      aux.corr2[,,r,rcp,p,] = corr2.0(mod = v[,,,r,p,rcp,4], obs = v[,,,r,p,rcp,3], lon = 144, lat = 73, cf = 0.95)*array(mask, dim = c(dim(mask), 2))
      
    }
  }
}


# da muy raro... en cnrm-cm6.





# config esto
ens.corr = apply(aux.corr, c(1,2,4,5,6), mean, na.rm = T)
ens.corr2 = apply(aux.corr2, c(1,2,4,5,6), mean, na.rm = T)
lat = seq(-90,90, by = 2.5); lon = seq(1, 360, by = 2.5)


rcp.name = c("RCP2.6", "RCP8.5")
periodo = c("Futuro Cercano", "Futuro Lejano")

ens.corr[which(ens.corr>0)] = 1
ens.corr[which(ens.corr<0)] = NA

aux = array(ens.corr2[,,rcp,p,1], dim = c(dim(ens.corr[,,rcp,p,1]),1))
aux2 = array(ens.corr2[,,rcp,p,2], dim = c(dim(ens.corr[,,rcp,p,2]),1))


mapa_topo3(variable = aux, variable.sig = aux2, lon = lon, lat = lat
           , colorbar = "RdBu", escala = seq(-1,1, by = 0.1), sig = F, color.vsig = "white", contour.fill = T
           , mapa = "mundo", salida = "/Salidas/"
           , alpha.vsig = 1, na.fill = 0, revert = F, colorbar.pos = "bottom")
