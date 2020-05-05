#### Apertura de datos  y guardado de datos mensuales para evitar cargarlos cada vez (tarda bastante) ####
source("FUNCIONES.R")
# CNRM-CM5 - solo datos mensuales
# tas solo hay anual ---> preguntar si esto es correcto

# PRECIPITACION
#-------------------------------- pr - historical ---------------------------------#
pp5.his = open_nc(file_pattern = "pr_Amon_CNRM-CM5_historical_r*_197601-200512_2.5_mes.nc"
                , model = "cnrm-cm5", variable = "pr", mes_anual = "mes")
save(pp5.his, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/pp5.his.RData")
rm(pp5.his)



#--- RCP 2.6 --- #
#-------------------------------- pr - 2020 - 2049 ---------------------------------#
# HAY UNA SOLA CORRIDA
pp5.26_2049 = open_nc(file_pattern = "pr_Amon_CNRM-CM5_rcp26_r*_202001-204912_2.5_mes.nc"
                , model = "cnrm-cm5", variable = "pr", mes_anual = "mes")
save(pp5.26_2049, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/pp5.26_2049.RData")
rm(pp5.26_2049)

#-------------------------------- pr - 2070 - 2099 ---------------------------------#
# HAY UNA SOLA CORRIDA
pp5.26_2099 = open_nc(file_pattern = "pr_Amon_CNRM-CM5_rcp26_r*_207001-209912_2.5_mes.nc"
                   , model = "cnrm-cm5", variable = "pr", mes_anual = "mes")
save(pp5.26_2099, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/pp5.26_2099.RData")
rm(pp5.26_2099)



#--- RCP 8.5 ----#
#-------------------------------- pr - 2020 - 2049 ---------------------------------#
pp5.85_2049 = open_nc(file_pattern = "pr_Amon_CNRM-CM5_rcp85_r*_202001-204912_2.5_mes.nc"
                       , model = "cnrm-cm5", variable = "pr", mes_anual = "mes")
save(pp5.85_2049, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/pp5.85_2049.RData")
rm(pp5.85_2049)
#-------------------------------- pr - 2070 - 2099 ---------------------------------#
pp5.85_2099 = open_nc(file_pattern = "pr_Amon_CNRM-CM5_rcp85_r*_207001-209912_2.5_mes.nc"
                       , model = "cnrm-cm5", variable = "pr", mes_anual = "mes")
save(pp5.85_2099, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/pp5.85_2099.RData")
rm(pp5.85_2099)




# CNRM-CM6 - solo datos mensuales
# TEMPERATURA
#-------------------------------- tas - historical ---------------------------------#
t6.his = open_nc(file_pattern = "tas_Amon_CNRM-CM6-1_historical_r*_2.5.nc"
                  , model = "cnrm-cm6", variable = "tas", mes_anual = "mes")
save(t6.his, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/t6.his.RData")
rm(t6.his)



#--- SSP126 ---#
#-------------------------------- tas - 2020 - 2049 ---------------------------------#
t6.26_2049 = open_nc(file_pattern = "tas_Amon_CNRM-CM6-1_ssp126_r*_2020-2049_2.5.nc"
                 , model = "cnrm-cm6", variable = "tas", mes_anual = "mes")
save(t6.26_2049, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/t6.26_2049.RData")
rm(t6.26_2049)

#-------------------------------- tas - 2070 - 2099 ---------------------------------#
t6.26_2099 = open_nc(file_pattern = "tas_Amon_CNRM-CM6-1_ssp126_r*_2070-2099_2.5.nc"
                     , model = "cnrm-cm6", variable = "tas", mes_anual = "mes")
save(t6.26_2099, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/t6.26_2099.RData")
rm(t6.26_2099)



#--- SSP5858 ---#
#-------------------------------- tas - 2020 - 2049 ---------------------------------#
t6.85_2049 = open_nc(file_pattern = "tas_Amon_CNRM-CM6-1_ssp585_r*_2020-2049_2.5.nc"
                     , model = "cnrm-cm6", variable = "tas", mes_anual = "mes")
save(t6.85_2049, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/t6.85_2049.RData")
rm(t6.85_2049)

#-------------------------------- tas - 2070 - 2099 ---------------------------------#
t6.85_2099 = open_nc(file_pattern = "tas_Amon_CNRM-CM6-1_ssp585_r*_2070-2099_2.5.nc"
                     , model = "cnrm-cm6", variable = "tas", mes_anual = "mes")
save(t6.85_2099, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/t6.85_2099.RData")
rm(t6.85_2099)


# PRECIPITACION
#-------------------------------- pr - historical ---------------------------------#
pp6.his = open_nc(file_pattern = "pr_Amon_CNRM-CM6-1_historical_r*_2.5_mes.nc"
                  , model = "cnrm-cm6", variable = "pr", mes_anual = "mes")
save(pp6.his, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/pp6.his.RData")
rm(pp6.his)



#--- SSP126 ---#
#-------------------------------- pr - 2020 - 2049 ---------------------------------#
pp6.26_2049 = open_nc(file_pattern = "pr_Amon_CNRM-CM6-1_ssp126_r*_2020-2049_2.5_mes.nc"
                       , model = "cnrm-cm6", variable = "pr", mes_anual = "mes")
save(pp6.26_2049, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/pp6.26_2049.RData")
rm(pp6.26_2049)

#-------------------------------- pr - 2070 - 2099 ---------------------------------#
pp6.26_2099 = open_nc(file_pattern = "pr_Amon_CNRM-CM6-1_ssp126_r*_2070-2099_2.5_mes.nc"
                      , model = "cnrm-cm6", variable = "pr", mes_anual = "mes")
save(pp6.26_2099, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/pp6.26_2099.RData")
rm(pp6.26_2099)




#--- SSP585 ---#
#-------------------------------- pr - 2020 - 2049 ---------------------------------#
pp6.85_2049 = open_nc(file_pattern = "pr_Amon_CNRM-CM6-1_ssp585_r*_2020-2049_2.5_mes.nc"
                      , model = "cnrm-cm6", variable = "pr", mes_anual = "mes")
save(pp6.85_2049, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/pp6.85_2049.RData")
rm(pp6.85_2049)

#-------------------------------- pr - 2070 - 2099 ---------------------------------#
pp6.85_2099 = open_nc(file_pattern = "pr_Amon_CNRM-CM6-1_ssp585_r*_2070-2099_2.5_mes.nc"
                      , model = "cnrm-cm6", variable = "pr", mes_anual = "mes")
save(pp6.85_2099, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/pp6.85_2099.RData")
rm(pp6.85_2089)


# HUMEDAD DEL SUELO
#-------------------------------- huss - historical ---------------------------------#
hu6.his = open_nc(file_pattern = "huss_Amon_CNRM-CM6-1_historical_r*_2.5.nc"
                  , model = "cnrm-cm6", variable = "huss", mes_anual = "mes")
save(hu6.his, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/hu6.his.RData")
rm(hu6.his)



#--- SSP126 ---#
#-------------------------------- huss - 2020 - 2049 ---------------------------------#
hu6.26_2049 = open_nc(file_pattern = "huss_Amon_CNRM-CM6-1_ssp126_r*_2020-2049_2.5.nc"
                  , model = "cnrm-cm6", variable = "huss", mes_anual = "mes")
save(hu6.26_2049, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/hu6.26_2049.RData")
rm(hu6.26_2049)

#-------------------------------- huss - 2070 - 2099 ---------------------------------#
hu6.26_2099 = open_nc(file_pattern = "huss_Amon_CNRM-CM6-1_ssp126_r*_2070-2099_2.5.nc"
                      , model = "cnrm-cm6", variable = "huss", mes_anual = "mes")
save(hu6.26_2099, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/hu6.26_2099.RData")
rm(hu6.26_2099)




#--- SSP585 ---#
#-------------------------------- huss - 2020 - 2049 ---------------------------------#
hu6.85_2049 = open_nc(file_pattern = "huss_Amon_CNRM-CM6-1_ssp585_r*_2020-2049_2.5.nc"
                      , model = "cnrm-cm6", variable = "huss", mes_anual = "mes")
save(hu6.85_2049, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/hu6.85_2049.RData")
rm(hu6.85_2049)

#-------------------------------- huss - 2070 - 2099 ---------------------------------#
hu6.85_2099 = open_nc(file_pattern = "huss_Amon_CNRM-CM6-1_ssp585_r*_2070-2099_2.5.nc"
                      , model = "cnrm-cm6", variable = "huss", mes_anual = "mes")
save(hu6.85_2099, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/hu6.85_2099.RData")
rm(hu6.85_2099)


#--- U850 ---#
#-------------------------------- ua - historical ---------------------------------#
u6.his = open_nc(file_pattern = "ua850_Amon_CNRM-CM6-1_historical_r*_2.5.nc"
                 , model = "cnrm-cm6", variable = "ua", mes_anual = "mes")
save(u6.his, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/u6.his.RData")
rm(u6.his)


#--- SSP126 ---#
#-------------------------------- ua - 2020 - 2049 ---------------------------------#
u6.26_2049 = open_nc(file_pattern = "ua850_Amon_CNRM-CM6-1_ssp126_r*_2020-2049_2.5.nc"
                 , model = "cnrm-cm6", variable = "ua", mes_anual = "mes")
save(u6.26_2049, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/u6.26_2049.RData")
rm(u6.26_2049)

#-------------------------------- ua - 2070 - 2099 ---------------------------------#
u6.26_2099 = open_nc(file_pattern = "ua850_Amon_CNRM-CM6-1_ssp126_r*_2070-2099_2.5.nc"
                     , model = "cnrm-cm6", variable = "ua", mes_anual = "mes")
save(u6.26_2099, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/u6.26_2099.RData")
rm(u6.26_2099)



#--- SSP585 ---#
#-------------------------------- ua - 2020 - 2049 ---------------------------------#
u6.85_2049 = open_nc(file_pattern = "ua850_Amon_CNRM-CM6-1_ssp585_r*_2020-2049_2.5.nc"
                     , model = "cnrm-cm6", variable = "ua", mes_anual = "mes")
save(u6.85_2049, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/u6.85_2049.RData")
rm(u6.85_2049)


#-------------------------------- ua - 2070 - 2099 ---------------------------------#
u6.85_2099 = open_nc(file_pattern = "ua850_Amon_CNRM-CM6-1_ssp585_r*_2070-2099_2.5.nc"
                     , model = "cnrm-cm6", variable = "ua", mes_anual = "mes")
save(u6.85_2099, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/u6.85_2099.RData")
rm(u6.85_2099)


#--- V850 ---#
#-------------------------------- va - historical ---------------------------------#
v6.his = open_nc(file_pattern = "va850_Amon_CNRM-CM6-1_historical_r*_2.5.nc"
                 , model = "cnrm-cm6", variable = "va", mes_anual = "mes")
save(v6.his, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/v6.his.RData")
rm(v6.his)


#--- SSP126 ---#
#-------------------------------- va - 2020 - 2049 ---------------------------------#
v6.26_2049 = open_nc(file_pattern = "va850_Amon_CNRM-CM6-1_ssp126_r*_2020-2049_2.5.nc"
                     , model = "cnrm-cm6", variable = "va", mes_anual = "mes")
save(v6.26_2049, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/v6.26_2049.RData")
rm(v6.26_2049)

#-------------------------------- va - 2070 - 2099 ---------------------------------#
v6.26_2099 = open_nc(file_pattern = "va850_Amon_CNRM-CM6-1_ssp126_r*_2070-2099_2.5.nc"
                     , model = "cnrm-cm6", variable = "va", mes_anual = "mes")
save(v6.26_2099, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/v6.26_2099.RData")
rm(v6.26_2099)



#--- SSP585 ---#
#-------------------------------- va - 2020 - 2049 ---------------------------------#
v6.85_2049 = open_nc(file_pattern = "va850_Amon_CNRM-CM6-1_ssp585_r*_2020-2049_2.5.nc"
                     , model = "cnrm-cm6", variable = "va", mes_anual = "mes")
save(v6.85_2049, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/v6.85_2049.RData")
rm(v6.85_2049)


#-------------------------------- va - 2070 - 2099 ---------------------------------#
v6.85_2099 = open_nc(file_pattern = "va850_Amon_CNRM-CM6-1_ssp585_r*_2070-2099_2.5.nc"
                     , model = "cnrm-cm6", variable = "va", mes_anual = "mes")
save(v6.85_2099, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/v6.85_2099.RData")
rm(v6.85_2099)