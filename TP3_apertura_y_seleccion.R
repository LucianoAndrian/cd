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

# anual#
t6.his_an = open_nc(file_pattern = "tas_Amon_CNRM-CM6-1_historical_r*_2.5_anu.nc"
                    , model = "cnrm-cm6", variable = "tas", mes_anual = "anual")
save(t6.his_an, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/t6.his_an.RData")
rm(t6.his_an)

#--- SSP126 ---#
#-------------------------------- tas - 2020 - 2049 ---------------------------------#
t6.26_2049_an = open_nc(file_pattern = "tas_Amon_CNRM-CM6-1_ssp126_r*_2020-2049_2.5_anu.nc"
                        , model = "cnrm-cm6", variable = "tas", mes_anual = "anual")
save(t6.26_2049_an, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/t6.26_2049_an.RData")
rm(t6.26_2049_an)

#-------------------------------- tas - 2070 - 2099 ---------------------------------#
t6.26_2099_an = open_nc(file_pattern = "tas_Amon_CNRM-CM6-1_ssp126_r*_2070-2099_2.5_anu.nc"
                        , model = "cnrm-cm6", variable = "tas", mes_anual = "anual")
save(t6.26_2099_an, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/t6.26_2099_an.RData")
rm(t6.26_2099_an)


#--- SSP5858 ---#
#-------------------------------- tas - 2020 - 2049 ---------------------------------#
t6.85_2049_an = open_nc(file_pattern = "tas_Amon_CNRM-CM6-1_ssp585_r*_2020-2049_2.5_anu.nc"
                        , model = "cnrm-cm6", variable = "tas", mes_anual = "anual")
save(t6.85_2049_an, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/t6.85_2049_an.RData")
rm(t6.85_2049_an)

#-------------------------------- tas - 2070 - 2099 ---------------------------------#
t6.85_2099_an = open_nc(file_pattern = "tas_Amon_CNRM-CM6-1_ssp585_r*_2070-2099_2.5_anu.nc"
                        , model = "cnrm-cm6", variable = "tas", mes_anual = "anual")
save(t6.85_2099_an, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/t6.85_2099_an.RData")
rm(t6.85_2099_an)



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


# etp de CNRM-CM5#
#---- historical ----#
#-------------------------------- etp - his - anual ---------------------------------#
etp5.his_an = open_nc(file_pattern = "etp_Amon_CNRM-CM5_historical_r*_197601-200512_2.5_anu.nc"
                      , model = "cnrm-cm5", variable = "etp", mes_anual = "anual")

save(etp5.his_an, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/etp5.his_an.RData")
rm(etp5.his_an)

#---- SSP126 ----#
#-------------------------------- etp - 2049 - anual ---------------------------------#
etp5.26_49 = open_nc(file_pattern = "etp_Amon_CNRM-CM5_rcp26_r*_202001-204912_2.5_anu.nc"
                     , model = "cnrm-cm5", variable = "etp", mes_anual = "anual")

save(etp5.26_49, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/etp5.26_49.RData")
rm(etp5.26_49)

#-------------------------------- etp - 2099 - anual ---------------------------------#
etp5.26_99 = open_nc(file_pattern = "etp_Amon_CNRM-CM5_rcp26_r*_207001-209912_2.5_anu.nc"
                     , model = "cnrm-cm5", variable = "etp", mes_anual = "anual")

save(etp5.26_99, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/etp5.26_99.RData")
rm(etp5.26_99)


#---- ssp585 ----#
#-------------------------------- etp - 2049 - anual ---------------------------------#
etp5.85_49 = open_nc(file_pattern =  "etp_Amon_CNRM-CM5_rcp26_r*_207001-209912_2.5_anu.nc"
                     , model = "cnrm-cm5", variable = "etp", mes_anual = "anual")

save(etp5.85_49, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/etp5.85_49.RData")
rm(etp5.85_49)

#-------------------------------- etp - 2099 - anual ---------------------------------#
etp5.85_99 = open_nc(file_pattern = "etp_Amon_CNRM-CM5_rcp85_r*_202001-204912_2.5_anu.nc"
                     , model = "cnrm-cm5", variable = "etp", mes_anual = "anual")

save(etp5.85_99, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/etp5.85_99.RData")
rm(etp5.85_99)



# TEMP  CNRM-CM5  




#---- historical ----#
#-------------------------------- t - his - anual ---------------------------------#
t5.his_an = open_nc(file_pattern = "tas_Amon_CNRM-CM5_historical_r*_197601-200512_2.5_anu.nc"
                    , model = "cnrm-cm5", variable = "tas", mes_anual = "anual")

save(t5.his_an, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/t5.his_an.RData")
rm(t5.his_an)

#---- SSP126 ----#
#-------------------------------- t - 2049 - anual ---------------------------------#
t5.26_49 = open_nc(file_pattern = "tas_Amon_CNRM-CM5_rcp26_r*_202001-204912_2.5_anu.nc"
                   , model = "cnrm-cm5", variable = "tas", mes_anual = "anual")

save(t5.26_49, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/t5.26_49.RData")
rm(t5.26_49)

#-------------------------------- t - 2099 - anual ---------------------------------#
t5.26_99 = open_nc(file_pattern = "tas_Amon_CNRM-CM5_rcp26_r*_207001-209912_2.5_anu.nc"
                   , model = "cnrm-cm5", variable = "tas", mes_anual = "anual")

save(t5.26_99, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/t5.26_99.RData")
rm(t5.26_99)


#---- ssp585 ----#
#-------------------------------- t - 2049 - anual ---------------------------------#
t5.85_49 = open_nc(file_pattern =  "tas_Amon_CNRM-CM5_rcp85_r*_202001-204912_2.5_anu.nc"
                   , model = "cnrm-cm5", variable = "tas", mes_anual = "anual")

save(t5.85_49, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/t5.85_49.RData")
rm(t5.85_49)

#-------------------------------- t - 2099 - anual ---------------------------------#
t5.85_99 = open_nc(file_pattern = "tas_Amon_CNRM-CM5_rcp85_r*_207001-209912_2.5_anu.nc"
                   , model = "cnrm-cm5", variable = "tas", mes_anual = "anual")

save(t5.85_99, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/t5.85_99.RData")
rm(t5.85_99)

#### Viento ####

#--- U850 ---#
#-------------------------------- ua - historical ---------------------------------#
u6.his = open_nc(file_pattern = "ua850_Amon_CNRM-CM6-1_historical_r*_2.5.nc"
                 , model = "cnrm-cm6", variable = "ua", mes_anual = "mes")
save(u6.his, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/u6.his.RData")
rm(u6.his)


#--- SSP126 ---#
#-------------------------------- ua - 2020 - 2049 ---------------------------------#
u6.26_2049 = open_nc(file_pattern = "ua850_Amon_CNRM-CM6-1_ssp126_r*_2020-2049_2.5.nc"
                     , model = "cnrm-cm6", variable = "ua", mes_anual = "mes")
save(u6.26_2049, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/u6.26_2049.RData")
rm(u6.26_2049)

#-------------------------------- ua - 2070 - 2099 ---------------------------------#
u6.26_2099 = open_nc(file_pattern = "ua850_Amon_CNRM-CM6-1_ssp126_r*_2070-2099_2.5.nc"
                     , model = "cnrm-cm6", variable = "ua", mes_anual = "mes")
save(u6.26_2099, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/u6.26_2099.RData")
rm(u6.26_2099)



#--- SSP585 ---#
#-------------------------------- ua - 2020 - 2049 ---------------------------------#
u6.85_2049 = open_nc(file_pattern = "ua850_Amon_CNRM-CM6-1_ssp585_r*_2020-2049_2.5.nc"
                     , model = "cnrm-cm6", variable = "ua", mes_anual = "mes")
save(u6.85_2049, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/u6.85_2049.RData")
rm(u6.85_2049)


#-------------------------------- ua - 2070 - 2099 ---------------------------------#
u6.85_2099 = open_nc(file_pattern = "ua850_Amon_CNRM-CM6-1_ssp585_r*_2070-2099_2.5.nc"
                     , model = "cnrm-cm6", variable = "ua", mes_anual = "mes")
save(u6.85_2099, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/u6.85_2099.RData")
rm(u6.85_2099)


#--- V850 ---#
#-------------------------------- va - historical ---------------------------------#
v6.his = open_nc(file_pattern = "va850_Amon_CNRM-CM6-1_historical_r*_2.5.nc"
                 , model = "cnrm-cm6", variable = "va", mes_anual = "mes")
save(v6.his, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/v6.his.RData")
rm(v6.his)


#--- SSP126 ---#
#-------------------------------- va - 2020 - 2049 ---------------------------------#
v6.26_2049 = open_nc(file_pattern = "va850_Amon_CNRM-CM6-1_ssp126_r*_2020-2049_2.5.nc"
                     , model = "cnrm-cm6", variable = "va", mes_anual = "mes")
save(v6.26_2049, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/v6.26_2049.RData")
rm(v6.26_2049)

#-------------------------------- va - 2070 - 2099 ---------------------------------#
v6.26_2099 = open_nc(file_pattern = "va850_Amon_CNRM-CM6-1_ssp126_r*_2070-2099_2.5.nc"
                     , model = "cnrm-cm6", variable = "va", mes_anual = "mes")
save(v6.26_2099, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/v6.26_2099.RData")
rm(v6.26_2099)



#--- SSP585 ---#
#-------------------------------- va - 2020 - 2049 ---------------------------------#
v6.85_2049 = open_nc(file_pattern = "va850_Amon_CNRM-CM6-1_ssp585_r*_2020-2049_2.5.nc"
                     , model = "cnrm-cm6", variable = "va", mes_anual = "mes")
save(v6.85_2049, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/v6.85_2049.RData")
rm(v6.85_2049)


#-------------------------------- va - 2070 - 2099 ---------------------------------#
v6.85_2099 = open_nc(file_pattern = "va850_Amon_CNRM-CM6-1_ssp585_r*_2070-2099_2.5.nc"
                     , model = "cnrm-cm6", variable = "va", mes_anual = "mes")
save(v6.85_2099, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP3.RDatas/v6.85_2099.RData")
rm(v6.85_2099)




