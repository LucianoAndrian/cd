# nc para tp final.
# periodo historico va aparte ya que r = 30 
# viento separado del resto de las variables --> 9 niveles 

# datos mensuales en cada periodo. 
# para empezar--> promedio de los 30 años para cada mes

#### funciones ####
library(ncdf4)

Pattern = function(model, file_pattern){
  model = toupper(model)
  
  ruta = paste('/home/auri/Facultad/Materias/c-dinamica/TPs/', model, "/",sep = "")
  
  t = Sys.glob(paste(ruta, file_pattern, sep = ""))
  
  return(t)
  
}





MeanMonth = function(t, variable, niveles = T){
  
  if(niveles){
    e2  = new.env(parent = baseenv()) 
    assign("meses", array(NA, dim = c(144,73,9,12,length(t))), envir = e2)
    
    for(r in 1:length(t)){
      print(paste("miembro = ", r ))
      for(m in 1:12){
        x = array(NA, dim = c(144,73,9,30)) 
        
        v_nc = nc_open(t[r])
        v = ncvar_get(v_nc, variable)
        for(a in 0:29){  
          
          
          x[,,,a+1] = v[,,,m+12*a]
          
        }
        
        nc_close(v_nc)
        
        
        e2$meses[,,,m,r] = apply(x, c(1,2,3), mean, na.rm = T)
        
        
      }
      
    }
    
  } else {
    
    e2  = new.env(parent = baseenv()) 
    assign("meses", array(NA, dim = c(144,73,12,length(t))), envir = e2)
    
    for(r in 1:length(t)){
      print(paste("miembro = ", r ))
      for(m in 1:12){
        
        x = array(NA, dim = c(144,73,30)) 
        
        v_nc = nc_open(t[r])
        v = ncvar_get(v_nc, variable)
        for(a in 0:29){  
          
          
          x[,,a+1] = v[,,m+12*a]
          
        }
        
        nc_close(v_nc)
        
        
        e2$meses[,,m,r] = apply(x, c(1,2), mean, na.rm = T)
        
        
      }
      
    }
    
  }
  
  return(e2$meses); print("Return")
  rm(e2); print("Eliminando 2do Environment")
}




var = as.numeric(readline("viento(1), temp(2), presion(3) "))
periodo = as.numeric(readline("Historico(1), 2020-2049(2), 2070-2099(3) "))



####
if(var == 1 & periodo == 1){
  # rec, r = 29. en pr r = 23 no esta.
  #-- VIENTO --#
  
  t = Pattern(model = "cnrm-cm6", file_pattern = "ua_Amon_CNRM-CM6-1_historical_r*_2.5.nc")
  aux = MeanMonth(t, "ua")
  
  
  t = Pattern(model = "cnrm-cm6", file_pattern = "va_Amon_CNRM-CM6-1_historical_r*_2.5.nc")
  aux2 = MeanMonth(t, "va")
  
  
  #meses  
  V = array(data = NA, dim = c(144, 73, 9, 12, 29, 2))   #519.7Mb
  #niv    #r   #u v
  V[,,,,,1] = aux
  V[,,,,,2] = aux2
  
  
  lon = seq(1,144); lat = seq(1,73)
  meses = seq(1,12)
  r = 29
  niveles = seq(1,9)
  londim = ncdim_def("lon", "grados_este", as.double(lon))
  latdim = ncdim_def("lat", "grados_norte", as.double(lat))
  mesesdim = ncdim_def("time", "meses", as.double(meses))
  rdim = ncdim_def("r","miembros", as.double(1:r))
  nivdim = ncdim_def("niv","niveles", as.double(niveles))
  vardim = ncdim_def("V","viento", as.double(1:2))
  fillvalue = NA
  dlname = "V_CNRM-CM6"
  var_def = ncvar_def("viento", "viento", list(londim, latdim, nivdim, mesesdim, rdim, vardim), fillvalue, dlname, prec = "single")
  ncfname = "V_CNRM-CM6.nc"
  ncout = nc_create(ncfname, list(var_def), force_v4=T)
  ncvar_put(ncout, var_def, V)
  nc_close(ncout)
  
  
} else if(var == 1 & periodo >1){
  rcp = as.numeric(readline("RCP: 2.6(1), 8.5(2)"))
  
  if(periodo == 2 & rcp == 1){
    ut = Pattern(model = "cnrm-cm6", file_pattern = "ua_Amon_CNRM-CM6-1_ssp126_r*_2020-2049_2.5.nc")
    vt = Pattern(model = "cnrm-cm6", file_pattern = "va_Amon_CNRM-CM6-1_ssp126_r*_2020-2049_2.5.nc")
  } else if(periodo == 2 & rcp == 2) {
    ut = Pattern(model = "cnrm-cm6", file_pattern = "ua_Amon_CNRM-CM6-1_ssp585_r*_2020-2049_2.5.nc")
    vt = Pattern(model = "cnrm-cm6", file_pattern = "va_Amon_CNRM-CM6-1_ssp585_r*_2020-2049_2.5.nc")
  } else if(periodo == 3 & rcp == 1){
    ut = Pattern(model = "cnrm-cm6", file_pattern = "ua_Amon_CNRM-CM6-1_ssp126_r*_2070-2099_2.5.nc")
    vt = Pattern(model = "cnrm-cm6", file_pattern = "va_Amon_CNRM-CM6-1_ssp126_r*_2070-2099_2.5.nc")
  } else if(periodo == 3 & rcp == 2){
    ut = Pattern(model = "cnrm-cm6", file_pattern = "ua_Amon_CNRM-CM6-1_ssp585_r*_2070-2099_2.5.nc")
    vt = Pattern(model = "cnrm-cm6", file_pattern = "va_Amon_CNRM-CM6-1_ssp585_r*_2070-2099_2.5.nc")
  }
  
  
  aux = MeanMonth(ut, "ua")
  aux2 = MeanMonth(vt, "va")
  
  
  #meses  
  V = array(data = NA, dim = c(144, 73, 9, 12, length(ut), 2))   #519.7Mb
  #niv    #r   #u v
  V[,,,,,1] = aux
  V[,,,,,2] = aux2
  
  periodo_nc = c("lalal","2049", "2099"); rcp_nc = c("126", "585")
  lon = seq(1,144); lat = seq(1,73)
  meses = seq(1,12)
  r = length(ut)
  niveles = seq(1,9)
  londim = ncdim_def("lon", "grados_este", as.double(lon))
  latdim = ncdim_def("lat", "grados_norte", as.double(lat))
  mesesdim = ncdim_def("time", "meses", as.double(meses))
  rdim = ncdim_def("r","miembros", as.double(1:r))
  nivdim = ncdim_def("niv","niveles", as.double(niveles))
  vardim = ncdim_def("V","viento", as.double(1:2))
  fillvalue = NA
  dlname = paste("V_", periodo_nc[periodo],"_", rcp_nc[rcp],".CNRM-CM6", sep = "")
  var_def = ncvar_def("viento", "viento", list(londim, latdim, nivdim, mesesdim, rdim, vardim), fillvalue, dlname, prec = "single")
  ncfname = paste("V_", periodo_nc[periodo],"_", rcp_nc[rcp],".CNRM-CM6.nc", sep = "")
  ncout = nc_create(ncfname, list(var_def), force_v4=T)
  ncvar_put(ncout, var_def, V)
  nc_close(ncout)
  
  
  
  
} else if(var == 2 & periodo == 1){
  t = Pattern(model = "cnrm-cm6", file_pattern = "tas_Amon_CNRM-CM6-1_historical_r*_2.5.nc")
  V = MeanMonth(t, "tas", niveles = F)

  lon = seq(1,144); lat = seq(1,73)
  meses = seq(1,12)
  r = 29
  niveles = seq(1,9)
  londim = ncdim_def("lon", "grados_este", as.double(lon))
  latdim = ncdim_def("lat", "grados_norte", as.double(lat))
  mesesdim = ncdim_def("time", "meses", as.double(meses))
  rdim = ncdim_def("r","miembros", as.double(1:r))
  fillvalue = NA
  dlname = "T_CNRM-CM6"
  var_def = ncvar_def("tas", "tas", list(londim, latdim, mesesdim, rdim), fillvalue, dlname, prec = "single")
  ncfname = "T_CNRM-CM6.nc"
  ncout = nc_create(ncfname, list(var_def), force_v4=T)
  ncvar_put(ncout, var_def, V)
  nc_close(ncout)
  
  
} else if(var == 2 & periodo > 1){
  rcp = as.numeric(readline("RCP: 2.6(1), 8.5(2)"))
  
  if(periodo == 2 & rcp == 1){
    t = Pattern(model = "cnrm-cm6", file_pattern = "tas_Amon_CNRM-CM6-1_ssp126_r*_2020-2049_2.5.nc")
  } else if(periodo == 2 & rcp == 2) {
    t = Pattern(model = "cnrm-cm6", file_pattern = "tas_Amon_CNRM-CM6-1_ssp585_r*_2020-2049_2.5.nc")
  } else if(periodo == 3 & rcp == 1){
    t = Pattern(model = "cnrm-cm6", file_pattern = "tas_Amon_CNRM-CM6-1_ssp126_r*_2070-2099_2.5.nc")
  } else if(periodo == 3 & rcp == 2){
    t = Pattern(model = "cnrm-cm6", file_pattern = "tas_Amon_CNRM-CM6-1_ssp585_r*_2070-2099_2.5.nc")
  }
  
  V = MeanMonth(t, "tas", niveles = F)
  
  
  periodo_nc = c("lalal","2049", "2099"); rcp_nc = c("126", "585")
  lon = seq(1,144); lat = seq(1,73)
  meses = seq(1,12)
  r = length(t)
  niveles = seq(1,9)
  londim = ncdim_def("lon", "grados_este", as.double(lon))
  latdim = ncdim_def("lat", "grados_norte", as.double(lat))
  mesesdim = ncdim_def("time", "meses", as.double(meses))
  rdim = ncdim_def("r","miembros", as.double(1:r))
  fillvalue = NA
  dlname = paste("T_", periodo_nc[periodo],"_", rcp_nc[rcp],".CNRM-CM6", sep = "")
  var_def = ncvar_def("tas", "tas", list(londim, latdim, mesesdim, rdim), fillvalue, dlname, prec = "single")
  ncfname = paste("T_", periodo_nc[periodo],"_", rcp_nc[rcp],".CNRM-CM6.nc", sep = "")
  ncout = nc_create(ncfname, list(var_def), force_v4=T)
  ncvar_put(ncout, var_def, V)
  nc_close(ncout)
  
  
  
} else if(var == 3 & periodo == 1){
  t = Pattern(model = "cnrm-cm6", file_pattern = "psl_Amon_CNRM-CM6-1_historical_r*_2.5.nc")
  V = MeanMonth(t, "psl", niveles = F)
  
  lon = seq(1,144); lat = seq(1,73)
  meses = seq(1,12)
  r = 29
  niveles = seq(1,9)
  londim = ncdim_def("lon", "grados_este", as.double(lon))
  latdim = ncdim_def("lat", "grados_norte", as.double(lat))
  mesesdim = ncdim_def("time", "meses", as.double(meses))
  rdim = ncdim_def("r","miembros", as.double(1:r))
  fillvalue = NA
  dlname = "PSL_CNRM-CM6"
  var_def = ncvar_def("psl", "psl", list(londim, latdim, mesesdim, rdim), fillvalue, dlname, prec = "single")
  ncfname = "PSL_CNRM-CM6.nc"
  ncout = nc_create(ncfname, list(var_def), force_v4=T)
  ncvar_put(ncout, var_def, V)
  nc_close(ncout)
  
  
} else if(var == 3 & periodo > 1){
  rcp = as.numeric(readline("RCP: 2.6(1), 8.5(2)"))
  
  if(periodo == 2 & rcp == 1){
    t = Pattern(model = "cnrm-cm6", file_pattern = "psl_Amon_CNRM-CM6-1_ssp126_r*_2020-2049_2.5.nc")
  } else if(periodo == 2 & rcp == 2) {
    t = Pattern(model = "cnrm-cm6", file_pattern = "psl_Amon_CNRM-CM6-1_ssp585_r*_2020-2049_2.5.nc")
  } else if(periodo == 3 & rcp == 1){
    t = Pattern(model = "cnrm-cm6", file_pattern = "psl_Amon_CNRM-CM6-1_ssp126_r*_2070-2099_2.5.nc")
  } else if(periodo == 3 & rcp == 2){
    t = Pattern(model = "cnrm-cm6", file_pattern = "psl_Amon_CNRM-CM6-1_ssp585_r*_2070-2099_2.5.nc")
  }
  
  V = MeanMonth(t, "psl", niveles = F)
  
  
  periodo_nc = c("lalal","2049", "2099"); rcp_nc = c("126", "585")
  lon = seq(1,144); lat = seq(1,73)
  meses = seq(1,12)
  r = length(t)
  niveles = seq(1,9)
  londim = ncdim_def("lon", "grados_este", as.double(lon))
  latdim = ncdim_def("lat", "grados_norte", as.double(lat))
  mesesdim = ncdim_def("time", "meses", as.double(meses))
  rdim = ncdim_def("r","miembros", as.double(1:r))
  fillvalue = NA
  dlname = paste("PSL_", periodo_nc[periodo],"_", rcp_nc[rcp],".CNRM-CM6", sep = "")
  var_def = ncvar_def("psl", "psl", list(londim, latdim, mesesdim, rdim), fillvalue, dlname, prec = "single")
  ncfname = paste("PSL_", periodo_nc[periodo],"_", rcp_nc[rcp],".CNRM-CM6.nc", sep = "")
  ncout = nc_create(ncfname, list(var_def), force_v4=T)
  ncvar_put(ncout, var_def, V)
  nc_close(ncout)
  
  
  
}


rm(list = ls())
.rs.restartR()
