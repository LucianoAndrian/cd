# mask

# necesito mascara global
# la contruyo a partir de los datos obs de gpcc
aux = nc_open("/home/auri/Facultad/Materias/c-dinamica/TPs/Datos_Obs1/precip.mon.total.v7_197601-200512_2.5_anu.nc")
v_aux = ncvar_get(aux, "precip")
mask = v_aux[,,2]
mask[which(!is.na(mask))] = 1
mask[which(mask != 1)] = NA
image.plot(mask)
write.table(mask, "mask.txt")

# la de prip tiene poco detalle de tierra, esta demasiado tal vez


aux = nc_open("/home/auri/Facultad/Materias/c-dinamica/TPs/CNRM-CM5/tas_Amon_CNRM-CM5_historical_r1_197601-200512_2.5_anu.nc")
lon = ncvar_get(aux, "lon")
lat = ncvar_get(aux, "lat")
nc_close(aux)

# al guardar esto auri pasa puntos a comas!!! (cambiar a mano con "buscar y remmplazar", sino ggplot2 se enloquece)
write.table(lon, "lon.txt")
write.table(lat, "lat.txt")



