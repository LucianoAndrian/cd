#TP4_apertura_y_seleccion modelos
# Anual #
# Apertura de datos y guardado de datos mensuales para evitar cargarlos cada vez (tarda bastante) ####
rm(list = ls())
source("FUNCIONES.R")

#---- CNRM-CM5 ----#
#### TEMPERATURA ####

#---- historical ----#
#-------------------------------- t - his - anual ---------------------------------#
t5.his = open_nc(file_pattern = "tas_Amon_CNRM-CM5_historical_r*_197601-200512_2.5_anu.nc"
                 , model = "cnrm-cm5", variable = "tas", mes_anual = "anual")

save(t5.his, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/t5.his.RData")
rm(t5.his)

#---- SSP126 ----#
#-------------------------------- t - 2049 - anual ---------------------------------#
t5.26_49 = open_nc(file_pattern = "tas_Amon_CNRM-CM5_rcp26_r*_202001-204912_2.5_anu.nc"
                   , model = "cnrm-cm5", variable = "tas", mes_anual = "anual")

save(t5.26_49, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/t5.26_49.RData")
rm(t5.26_49)

#-------------------------------- t - 2099 - anual ---------------------------------#
t5.26_99 = open_nc(file_pattern = "tas_Amon_CNRM-CM5_rcp26_r*_207001-209912_2.5_anu.nc"
                   , model = "cnrm-cm5", variable = "tas", mes_anual = "anual")

save(t5.26_99, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/t5.26_99.RData")
rm(t5.26_99)


#---- ssp585 ----#
#-------------------------------- t - 2049 - anual ---------------------------------#
t5.85_49 = open_nc(file_pattern =  "tas_Amon_CNRM-CM5_rcp85_r*_202001-204912_2.5_anu.nc"
                   , model = "cnrm-cm5", variable = "tas", mes_anual = "anual")

save(t5.85_49, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/t5.85_49.RData")
rm(t5.85_49)

#-------------------------------- t - 2099 - anual ---------------------------------#
t5.85_99 = open_nc(file_pattern = "tas_Amon_CNRM-CM5_rcp85_r*_207001-209912_2.5_anu.nc"
                   , model = "cnrm-cm5", variable = "tas", mes_anual = "anual")

save(t5.85_99, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/t5.85_99.RData")
rm(t5.85_99)

#### HUMEDAD ####
#-------------------------------- hu - his - anual ---------------------------------#
hu5.his = open_nc(file_pattern = "huss_annual_CNRM-CM5_historical_r*_197601-200512_2.5.nc"
                  , model = "cnrm-cm5", variable = "huss", mes_anual = "anual")

save(hu5.his, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/hu5.his.RData")
rm(hu5.his)

#---- RCP126 ----#
#-------------------------------- hu - 2049 - anual ---------------------------------#
hu5.26_49 = open_nc(file_pattern = "huss_annual_CNRM-CM5_rcp26_r*_202001-204912_2.5.nc"
                    , model = "cnrm-cm5", variable = "huss", mes_anual = "anual")

save(hu5.26_49, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/hu5.26_49.RData")
rm(hu5.26_49)


#-------------------------------- hu - 2099 - anual ---------------------------------#
hu5.26_99 = open_nc(file_pattern = "huss_annual_CNRM-CM5_rcp26_r*_207001-209912_2.5.nc"
                    , model = "cnrm-cm5", variable = "huss", mes_anual = "anual")

save(hu5.26_99, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/hu5.26_99.RData")
rm(hu5.26_99)


#---- RCP85 ----#
#-------------------------------- hu - 2049 - anual ---------------------------------#
hu5.85_49 = open_nc(file_pattern = "huss_annual_CNRM-CM5_rcp85_r*_202001-204912_2.5.nc"
                    , model = "cnrm-cm5", variable = "huss", mes_anual = "anual")

save(hu5.85_49, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/hu5.85_49.RData")
rm(hu5.85_49)


#-------------------------------- hu - 2099 - anual ---------------------------------#
hu5.85_99 = open_nc(file_pattern = "huss_annual_CNRM-CM5_rcp85_r*_207001-209912_2.5.nc"
                    , model = "cnrm-cm5", variable = "huss", mes_anual = "anual")

save(hu5.85_99, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/hu5.85_99.RData")
rm(hu5.85_99)

#### PP ####
#-------------------------------- pp - his - anual ---------------------------------#
pp5.his = open_nc(file_pattern = "pr_Amon_CNRM-CM5_historical_r*_197601-200512_2.5_anu.nc"
                  , model = "cnrm-cm5", variable = "pr", mes_anual = "anual")

save(pp5.his, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/pp5.his.RData")
rm(pp5.his)

#---- RCP126 ----#
#-------------------------------- pp - 2049 - anual ---------------------------------#
pp5.26_49 = open_nc(file_pattern = "pr_Amon_CNRM-CM5_rcp26_r*_202001-204912_2.5_anu.nc"
                   , model = "cnrm-cm5", variable = "pr", mes_anual = "anual")

save(pp5.26_49, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/pp5.26_49.RData")
rm(pp5.26_49)

#-------------------------------- pp - 2099 - anual ---------------------------------#
pp5.26_99 = open_nc(file_pattern = "pr_Amon_CNRM-CM5_rcp26_r*_207001-209912_2.5_anu.nc"
                    , model = "cnrm-cm5", variable = "pr", mes_anual = "anual")

save(pp5.26_99, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/pp5.26_99.RData")
rm(pp5.26_99)


#---- RCP585 ----#
#-------------------------------- pp - 2049 - anual ---------------------------------#
pp5.85_49 = open_nc(file_pattern = "pr_Amon_CNRM-CM5_rcp85_r*_202001-204912_2.5_anu.nc"
                    , model = "cnrm-cm5", variable = "pr", mes_anual = "anual")

save(pp5.85_49, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/pp5.85_49.RData")
rm(pp5.85_49)

#-------------------------------- pp - 2099 - anual ---------------------------------#
pp5.85_99 = open_nc(file_pattern = "pr_Amon_CNRM-CM5_rcp85_r*_207001-209912_2.5_anu.nc"
                    , model = "cnrm-cm5", variable = "pr", mes_anual = "anual")

save(pp5.85_99, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/pp5.85_99.RData")
rm(pp5.85_99)


#### Evap ####
#-------------------------------- evap - his - anual ---------------------------------#
evap5.his = open_nc(file_pattern = "evspsbl_Amon_CNRM-CM5_historical_r*_197601-200512_2.5_anu.nc"
                    , model = "cnrm-cm5", variable = "evspsbl", mes_anual = "anual")

save(evap5.his, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/evap5.his.RData")
rm(evap5.his)

#---- RCP26 ----#
#-------------------------------- evap - 2049 - anual ---------------------------------#
evap5.26_49 = open_nc(file_pattern = "evspsbl_Amon_CNRM-CM5_rcp26_r*_202001-204912_2.5_anu.nc"
                      , model = "cnrm-cm5", variable = "evspsbl", mes_anual = "anual")

save(evap5.26_49, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/evap5.26_49.RData")
rm(evap5.26_49)

#-------------------------------- evap - 2099 - anual ---------------------------------#
evap5.26_99 = open_nc(file_pattern = "evspsbl_Amon_CNRM-CM5_rcp26_r*_207001-209912_2.5_anu.nc"
                      , model = "cnrm-cm5", variable = "evspsbl", mes_anual = "anual")

save(evap5.26_99, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/evap5.26_99.RData")
rm(evap5.26_99)


#---- RCP85 ----#
#-------------------------------- evap - 2049 - anual ---------------------------------#
evap5.85_49 = open_nc(file_pattern = "evspsbl_Amon_CNRM-CM5_rcp85_r*_202001-204912_2.5_anu.nc"
                      , model = "cnrm-cm5", variable = "evspsbl", mes_anual = "anual")

save(evap5.85_49, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/evap5.85_49.RData")
rm(evap5.85_49)

#-------------------------------- evap - 2099 - anual ---------------------------------#
evap5.85_99 = open_nc(file_pattern = "evspsbl_Amon_CNRM-CM5_rcp85_r*_207001-209912_2.5_anu.nc"
                      , model = "cnrm-cm5", variable = "evspsbl", mes_anual = "anual")

save(evap5.85_99, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/evap5.85_99.RData")
rm(evap5.85_99)

#### ETP ####
#---- historical ----#
#-------------------------------- etp - his - anual ---------------------------------#
etp5.his = open_nc(file_pattern = "etp_Amon_CNRM-CM5_historical_r*_197601-200512_2.5_anu.nc"
                   , model = "cnrm-cm5", variable = "etp", mes_anual = "anual")

save(etp5.his, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/etp5.his.RData")
rm(etp5.his)

#---- SSP126 ----#
#-------------------------------- etp - 2049 - anual ---------------------------------#
etp5.26_49 = open_nc(file_pattern = "etp_Amon_CNRM-CM5_rcp26_r*_202001-204912_2.5_anu.nc"
                     , model = "cnrm-cm5", variable = "etp", mes_anual = "anual")

save(etp5.26_49, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/etp5.26_49.RData")
rm(etp5.26_49)

#-------------------------------- etp - 2099 - anual ---------------------------------#
etp5.26_99 = open_nc(file_pattern = "etp_Amon_CNRM-CM5_rcp26_r*_207001-209912_2.5_anu.nc"
                     , model = "cnrm-cm5", variable = "etp", mes_anual = "anual")

save(etp5.26_99, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/etp5.26_99.RData")
rm(etp5.26_99)


#---- ssp585 ----#
#-------------------------------- etp - 2049 - anual ---------------------------------#
etp5.85_49 = open_nc(file_pattern =  "etp_Amon_CNRM-CM5_rcp26_r*_207001-209912_2.5_anu.nc"
                     , model = "cnrm-cm5", variable = "etp", mes_anual = "anual")

save(etp5.85_49, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/etp5.85_49.RData")
rm(etp5.85_49)

#-------------------------------- etp - 2099 - anual ---------------------------------#
etp5.85_99 = open_nc(file_pattern = "etp_Amon_CNRM-CM5_rcp85_r*_202001-204912_2.5_anu.nc"
                     , model = "cnrm-cm5", variable = "etp", mes_anual = "anual")

save(etp5.85_99, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/etp5.85_99.RData")
rm(etp5.85_99)




#####

rm(list = ls())
source("FUNCIONES.R")

#---- CNRM-CM6 ----#
#### TEMPERATURA ####

#---- historical ----#
#-------------------------------- t - his - anual ---------------------------------#
t6.his = open_nc(file_pattern = "tas_Amon_CNRM-CM6-1_historical_r*_2.5_anu.nc"
                 , model = "CNRM-CM6", variable = "tas", mes_anual = "anual")

save(t6.his, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/t6.his.RData")
rm(t6.his)

#---- SSP126 ----#
#-------------------------------- t - 2049 - anual ---------------------------------#
t6.26_49 = open_nc(file_pattern = "tas_Amon_CNRM-CM6-1_ssp126_r*_2020-2049_2.5_anu.nc"
                   , model = "CNRM-CM6", variable = "tas", mes_anual = "anual")

save(t6.26_49, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/t6.26_49.RData")
rm(t6.26_49)

#-------------------------------- t - 2099 - anual ---------------------------------#
t6.26_99 = open_nc(file_pattern = "tas_Amon_CNRM-CM6-1_ssp126_r*_2070-2099_2.5_anu.nc"
                   , model = "CNRM-CM6", variable = "tas", mes_anual = "anual")

save(t6.26_99, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/t6.26_99.RData")
rm(t6.26_99)


#---- ssp585 ----#
#-------------------------------- t - 2049 - anual ---------------------------------#
t6.85_49 = open_nc(file_pattern =  "tas_Amon_CNRM-CM6-1_ssp585_r*_2020-2049_2.5_anu.nc"
                   , model = "CNRM-CM6", variable = "tas", mes_anual = "anual")

save(t6.85_49, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/t6.85_49.RData")
rm(t6.85_49)

#-------------------------------- t - 2099 - anual ---------------------------------#
t6.85_99 = open_nc(file_pattern = "tas_Amon_CNRM-CM6-1_ssp585_r*_2070-2099_2.5_anu.nc"
                   , model = "CNRM-CM6", variable = "tas", mes_anual = "anual")

save(t6.85_99, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/t6.85_99.RData")
rm(t6.85_99)

#### HUMEDAD ####
#-------------------------------- hu - his - anual ---------------------------------#
hu6.his_m = open_nc(file_pattern = "huss_Amon_CNRM-CM6-1_historical_r*_2.5.nc"
                    , model = "CNRM-CM6", variable = "huss", mes_anual = "mes")

save(hu6.his_m, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/hu6.his_m.RData")
rm(hu6.his_m)

#---- RCP126 ----#
#-------------------------------- hu - 2049 - anual ---------------------------------#
hu6.26_49_m = open_nc(file_pattern = "huss_Amon_CNRM-CM6-1_ssp126_r*_2020-2049_2.5.nc"
                      , model = "CNRM-CM6", variable = "huss", mes_anual = "mes")

save(hu6.26_49_m, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/hu6.26_49_m.RData")
rm(hu6.26_49_m)


#-------------------------------- hu - 2099 - anual ---------------------------------#
hu6.26_99_m = open_nc(file_pattern = "huss_Amon_CNRM-CM6-1_ssp126_r*_2070-2099_2.5.nc"
                      , model = "CNRM-CM6", variable = "huss", mes_anual = "mes")

save(hu6.26_99_m, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/hu6.26_99_m.RData")
rm(hu6.26_99_m)


#---- ssp585 ----#
#-------------------------------- hu - 2049 - anual ---------------------------------#
hu6.85_49_m = open_nc(file_pattern = "huss_Amon_CNRM-CM6-1_ssp585_r*_2020-2049_2.5.nc"
                      , model = "CNRM-CM6", variable = "huss", mes_anual = "mes")

save(hu6.85_49_m, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/hu6.85_49_m.RData")
rm(hu6.85_49_m)


#-------------------------------- hu - 2099 - anual ---------------------------------#
hu6.85_99_m = open_nc(file_pattern = "huss_Amon_CNRM-CM6-1_ssp585_r*_2070-2099_2.5.nc"
                      , model = "CNRM-CM6", variable = "huss", mes_anual = "mes")

save(hu6.85_99_m, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/hu6.85_99_m.RData")
rm(hu6.85_99_m)

#### PP ####
#-------------------------------- pp - his - anual ---------------------------------#
pp6.his = open_nc(file_pattern = "pr_Amon_CNRM-CM6-1_historical_r*_2.5_anu.nc"
                  , model = "CNRM-CM6", variable = "pr", mes_anual = "anual")

save(pp6.his, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/pp6.his.RData")
rm(pp6.his)

#---- RCP126 ----#
#-------------------------------- pp - 2049 - anual ---------------------------------#
pp6.26_49 = open_nc(file_pattern = "pr_Amon_CNRM-CM6-1_ssp126_r*_2020-2049_2.5_anu.nc"
                    , model = "CNRM-CM6", variable = "pr", mes_anual = "anual")

save(pp6.26_49, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/pp6.26_49.RData")
rm(pp6.26_49)

#-------------------------------- pp - 2099 - anual ---------------------------------#
pp6.26_99 = open_nc(file_pattern = "pr_Amon_CNRM-CM6-1_ssp126_r*_2070-2099_2.5_anu.nc"
                    , model = "CNRM-CM6", variable = "pr", mes_anual = "anual")

save(pp6.26_99, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/pp6.26_99.RData")
rm(pp6.26_99)


#---- RCP585 ----#
#-------------------------------- pp - 2049 - anual ---------------------------------#
pp6.85_49 = open_nc(file_pattern = "pr_Amon_CNRM-CM6-1_ssp585_r*_2020-2049_2.5_anu.nc"
                    , model = "CNRM-CM6", variable = "pr", mes_anual = "anual")

save(pp6.85_49, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/pp6.85_49.RData")
rm(pp6.85_49)

#-------------------------------- pp - 2099 - anual ---------------------------------#
pp6.85_99 = open_nc(file_pattern = "pr_Amon_CNRM-CM6-1_ssp585_r*_2070-2099_2.5_anu.nc"
                    , model = "CNRM-CM6", variable = "pr", mes_anual = "anual")

save(pp6.85_99, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/pp6.85_99.RData")
rm(pp6.85_99)


#### Evap ####
#-------------------------------- evap - his - anual ---------------------------------#
evap6.his = open_nc(file_pattern = "evspsbl_Amon_CNRM-CM6-1_historical_r*_2.5_anu.nc"
                    , model = "CNRM-CM6", variable = "evspsbl", mes_anual = "anual")

save(evap6.his, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/evap6.his.RData")
rm(evap6.his)

#---- ssp126 ----#
#-------------------------------- evap - 2049 - anual ---------------------------------#
evap6.26_49 = open_nc(file_pattern = "evspsbl_Amon_CNRM-CM6-1_ssp126_r*_2020-2049_2.5_anu.nc"
                      , model = "CNRM-CM6", variable = "evspsbl", mes_anual = "anual")

save(evap6.26_49, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/evap6.26_49.RData")
rm(evap6.26_49)

#-------------------------------- evap - 2099 - anual ---------------------------------#
evap6.26_99 = open_nc(file_pattern = "evspsbl_Amon_CNRM-CM6-1_ssp126_r*_2070-2099_2.5_anu.nc"
                      , model = "CNRM-CM6", variable = "evspsbl", mes_anual = "anual")

save(evap6.26_99, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/evap6.26_99.RData")
rm(evap6.26_99)


#---- ssp585 ----#
#-------------------------------- evap - 2049 - anual ---------------------------------#
evap6.85_49 = open_nc(file_pattern = "evspsbl_Amon_CNRM-CM6-1_ssp585_r*_2020-2049_2.5_anu.nc"
                      , model = "CNRM-CM6", variable = "evspsbl", mes_anual = "anual")

save(evap6.85_49, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/evap6.85_49.RData")
rm(evap6.85_49)

#-------------------------------- evap - 2099 - anual ---------------------------------#
evap6.85_99 = open_nc(file_pattern = "evspsbl_Amon_CNRM-CM6-1_ssp585_r*_2070-2099_2.5_anu.nc"
                      , model = "CNRM-CM6", variable = "evspsbl", mes_anual = "anual")

save(evap6.85_99, file = "/home/auri/Facultad/Materias/c-dinamica/TPs/RDatas/TP4.RDatas/evap6.85_99.RData")
rm(evap6.85_99)

