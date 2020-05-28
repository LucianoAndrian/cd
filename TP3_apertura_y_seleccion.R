#### Apertura de datos y guardado de datos mensuales para evitar cargarlos cada vez (tarda bastante) ####
rm(list = ls())
source("FUNCIONES.R")


# CNRM-CM6 - solo datos mensuales
# TEMPERATURA
#-------------------------------- tas - historical ---------------------------------#
t6.his = open_nc(file_pattern = "tas_Amon_CNRM-CM6-1_historical_r*_2.5.nc"
                 , model = "cnrm-cm6", variable = "tas", mes_anual = "mes")
save(t6.his, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/t6.his.RData")
rm(t6.his)

#--- SSP126 ---#
#-------------------------------- tas - 2020 - 2049 ---------------------------------#
t6.26_2049 = open_nc(file_pattern = "tas_Amon_CNRM-CM6-1_ssp126_r*_2020-2049_2.5.nc"
                     , model = "cnrm-cm6", variable = "tas", mes_anual = "mes")
save(t6.26_2049, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/t6.26_2049.RData")
rm(t6.26_2049)

#-------------------------------- tas - 2070 - 2099 ---------------------------------#
t6.26_2099 = open_nc(file_pattern = "tas_Amon_CNRM-CM6-1_ssp126_r*_2070-2099_2.5.nc"
                     , model = "cnrm-cm6", variable = "tas", mes_anual = "mes")
save(t6.26_2099, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/t6.26_2099.RData")
rm(t6.26_2099)


#--- SSP5858 ---#
#-------------------------------- tas - 2020 - 2049 ---------------------------------#
t6.85_2049 = open_nc(file_pattern = "tas_Amon_CNRM-CM6-1_ssp585_r*_2020-2049_2.5.nc"
                     , model = "cnrm-cm6", variable = "tas", mes_anual = "mes")
save(t6.85_2049, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/t6.85_2049.RData")
rm(t6.85_2049)

#-------------------------------- tas - 2070 - 2099 ---------------------------------#
t6.85_2099 = open_nc(file_pattern = "tas_Amon_CNRM-CM6-1_ssp585_r*_2070-2099_2.5.nc"
                     , model = "cnrm-cm6", variable = "tas", mes_anual = "mes")
save(t6.85_2099, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/t6.85_2099.RData")
rm(t6.85_2099)


# HUMEDAD
#-------------------------------- huss - historical ---------------------------------#
hu6.his = open_nc(file_pattern = "huss_Amon_CNRM-CM6-1_historical_r*_2.5.nc"
                  , model = "cnrm-cm6", variable = "huss", mes_anual = "mes")
save(hu6.his, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/hu6.his.RData")
rm(hu6.his)


#--- SSP126 ---#
#-------------------------------- huss - 2020 - 2049 ---------------------------------#
hu6.26_2049 = open_nc(file_pattern = "huss_Amon_CNRM-CM6-1_ssp126_r*_2020-2049_2.5.nc"
                      , model = "cnrm-cm6", variable = "huss", mes_anual = "mes")
save(hu6.26_2049, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/hu6.26_2049.RData")
rm(hu6.26_2049)

#-------------------------------- huss - 2070 - 2099 ---------------------------------#
hu6.26_2099 = open_nc(file_pattern = "huss_Amon_CNRM-CM6-1_ssp126_r*_2070-2099_2.5.nc"
                      , model = "cnrm-cm6", variable = "huss", mes_anual = "mes")
save(hu6.26_2099, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/hu6.26_2099.RData")
rm(hu6.26_2099)


#--- SSP585 ---#
#-------------------------------- huss - 2020 - 2049 ---------------------------------#
hu6.85_2049 = open_nc(file_pattern = "huss_Amon_CNRM-CM6-1_ssp585_r*_2020-2049_2.5.nc"
                      , model = "cnrm-cm6", variable = "huss", mes_anual = "mes")
save(hu6.85_2049, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/hu6.85_2049.RData")
rm(hu6.85_2049)

#-------------------------------- huss - 2070 - 2099 ---------------------------------#
hu6.85_2099 = open_nc(file_pattern = "huss_Amon_CNRM-CM6-1_ssp585_r*_2070-2099_2.5.nc"
                      , model = "cnrm-cm6", variable = "huss", mes_anual = "mes")
save(hu6.85_2099, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/hu6.85_2099.RData")
rm(hu6.85_2099)


#---- ETP ----# hay solo anual
#---- historical ----#
#-------------------------------- etp - his - anual ---------------------------------#
etp6.his_an = open_nc(file_pattern = "evspsblpot_Emon_CNRM-CM6-1_historical_r*_2.5_anu.nc"
                      , model = "cnrm-cm6", variable = "evspsblpot", mes_anual = "anual")

save(etp6.his_an, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/etp6.his_an.RData")
rm(etp6.his_an)

#---- SSP126 ----#
#-------------------------------- etp - 2049 - anual ---------------------------------#
etp6.26_49 = open_nc(file_pattern = "evspsblpot_Emon_CNRM-CM6-1_ssp126_r*_2020-2049_2.5_anu.nc"
                     , model = "cnrm-cm6", variable = "evspsblpot", mes_anual = "anual")

save(etp6.26_49, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/etp6.26_49.RData")
rm(etp6.26_49)

#-------------------------------- etp - 2099 - anual ---------------------------------#
etp6.26_99 = open_nc(file_pattern = "evspsblpot_Emon_CNRM-CM6-1_ssp126_r*_2070-2099_2.5_anu.nc"
                     , model = "cnrm-cm6", variable = "evspsblpot", mes_anual = "anual")

save(etp6.26_99, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/etp6.26_99.RData")
rm(etp6.26_99)


#---- ssp585 ----#
#-------------------------------- etp - 2049 - anual ---------------------------------#
etp6.85_49 = open_nc(file_pattern = "evspsblpot_Emon_CNRM-CM6-1_ssp585_r*_2020-2049_2.5_anu.nc"
                     , model = "cnrm-cm6", variable = "evspsblpot", mes_anual = "anual")

save(etp6.85_49, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/etp6.85_49.RData")
rm(etp6.85_49)

#-------------------------------- etp - 2099 - anual ---------------------------------#
etp6.85_99 = open_nc(file_pattern = "evspsblpot_Emon_CNRM-CM6-1_ssp585_r*_2070-2099_2.5_anu.nc"
                     , model = "cnrm-cm6", variable = "evspsblpot", mes_anual = "anual")

save(etp6.85_99, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/etp6.85_99.RData")
rm(etp6.85_99)




















