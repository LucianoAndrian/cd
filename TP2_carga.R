#Carga de datos TP2
#--- HISTORICO ----#
#### Temperatura #### 
#CNRM-CM5 no tiene datos mensuales

# CNRM-CM6


load("RDatas/t6.his.RData")

# usando t6.his[[2]] ya se encuentran promediados los anios (esto da lo mismo q hacerlo sobre cada miembro)
t6.his_seasons = array(data = NA, c(144,73,2))
t6.his_seasons[,,1] = apply(t6.his[[1]][,,2,], c(1,2), mean, na.rm = T) #JJA
t6.his_seasons[,,2] = apply(t6.his[[1]][,,4,], c(1,2), mean, na.rm = T) #DJF


rm(t6.his)



#### Viento ####
# CNRM-CM6
load("RDatas/u6.his.RData")
load("RDatas/v6.his.RData")

# JJA
V.his.jja = array(data = NA, dim = c(144,73,2))
V.his.jja[,,1] = apply(u6.his[[1]][,,2,], c(1,2), mean, na.rm = T)  # u850
V.his.jja[,,2] = apply(v6.his[[1]][,,2,], c(1,2), mean, na.rm = T)  # v850

# DJF
V.his.djf = array(data = NA, dim = c(144,73,2))
V.his.djf[,,1] = apply(u6.his[[1]][,,4,], c(1,2), mean, na.rm = T)  # u850
V.his.djf[,,2] = apply(v6.his[[1]][,,4,], c(1,2), mean, na.rm = T)  # v850

rm(u6.his, v6.his)

##### Precipitacion ####

# CNRM-CM5
load("RDatas/pp5.his.RData")

pp5.his_seasons = array(data = NA, c(144,73,2))
pp5.his_seasons[,,1] = apply(pp5.his[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
pp5.his_seasons[,,2] = apply(pp5.his[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

# CNRM-CM6
load("RDatas/pp6.his.RData")
pp6.his_seasons = array(data = NA, c(144,73,2))
pp6.his_seasons[,,1] = apply(pp6.his[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
pp6.his_seasons[,,2] = apply(pp6.his[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

rm(pp5.his, pp6.his)


#### Humedad ####

# CNRM-CM6
load("RDatas/hu6.his.RData")

hu6.his_seasons = array(data = NA, c(144,73,2))
hu6.his_seasons[,,1] = apply(hu6.his[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
hu6.his_seasons[,,2] = apply(hu6.his[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

rm(hu6.his)



#--- RCP2.6 ---#

#### Temperatura ####
# CNRM - CM5
# no tiene datos mensuales

# CNRM-CM6
load("RDatas/t6.26_2049.RData")
load("RDatas/t6.26_2099.RData")

t6.26_49.seasons = array(data = NA, dim = c(144,73,2)) 
t6.26_49.seasons[,,1] = apply(t6.26_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
t6.26_49.seasons[,,2] = apply(t6.26_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

t6.26_99.seasons = array(data = NA, dim = c(144,73,2)) 
t6.26_99.seasons[,,1] = apply(t6.26_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
t6.26_99.seasons[,,2] = apply(t6.26_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

rm(t6.26_2049, t6.26_2099)

#### Precipitacion ####
# CNRM-CM5
load("RDatas/pp5.26_2049.RData")
load("RDatas/pp5.26_2099.RData")

pp5.26_49.seasons = array(data = NA, dim = c(144,73,2)) 
pp5.26_49.seasons[,,1] = apply(pp5.26_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
pp5.26_49.seasons[,,2] = apply(pp5.26_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

pp5.26_99.seasons = array(data = NA, dim = c(144,73,2)) 
pp5.26_99.seasons[,,1] = apply(pp5.26_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
pp5.26_99.seasons[,,2] = apply(pp5.26_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

rm(pp5.26_2049, pp5.26_2099)

# CNRM-CM6
load("RDatas/pp6.26_2049.RData")
load("RDatas/pp6.26_2099.RData")

pp6.26_49.seasons = array(data = NA, dim = c(144,73,2)) 
pp6.26_49.seasons[,,1] = apply(pp6.26_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
pp6.26_49.seasons[,,2] = apply(pp6.26_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

pp6.26_99.seasons = array(data = NA, dim = c(144,73,2)) 
pp6.26_99.seasons[,,1] = apply(pp6.26_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
pp6.26_99.seasons[,,2] = apply(pp6.26_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF


rm(pp6.26_2049, pp6.26_2099)

#### Viento ####
#CNRM-CM6
load("RDatas/u6.26_2049.RData")
load("RDatas/u6.26_2099.RData")
load("RDatas/v6.26_2049.RData")
load("RDatas/v6.26_2099.RData")
load("RDatas/u6.85_2049.RData")
load("RDatas/u6.85_2099.RData")
load("RDatas/v6.85_2049.RData")
load("RDatas/v6.85_2099.RData")

u6.26_49.seasons = array(data = NA, dim = c(144,73,2)) 
u6.26_49.seasons[,,1] = apply(u6.26_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
u6.26_49.seasons[,,2] = apply(u6.26_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF


u6.26_99.seasons = array(data = NA, dim = c(144,73,2)) 
u6.26_99.seasons[,,1] = apply(u6.26_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
u6.26_99.seasons[,,2] = apply(u6.26_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

u6.85_49.seasons = array(data = NA, dim = c(144,73,2)) 
u6.85_49.seasons[,,1] = apply(u6.85_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
u6.85_49.seasons[,,2] = apply(u6.85_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF


u6.85_99.seasons = array(data = NA, dim = c(144,73,2)) 
u6.85_99.seasons[,,1] = apply(u6.85_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
u6.85_99.seasons[,,2] = apply(u6.85_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF



# v
v6.26_49.seasons = array(data = NA, dim = c(144,73,2)) 
v6.26_49.seasons[,,1] = apply(v6.26_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
v6.26_49.seasons[,,2] = apply(v6.26_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF


v6.26_99.seasons = array(data = NA, dim = c(144,73,2)) 
v6.26_99.seasons[,,1] = apply(v6.26_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
v6.26_99.seasons[,,2] = apply(v6.26_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

v6.85_49.seasons = array(data = NA, dim = c(144,73,2)) 
v6.85_49.seasons[,,1] = apply(v6.85_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
v6.85_49.seasons[,,2] = apply(v6.85_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF


v6.85_99.seasons = array(data = NA, dim = c(144,73,2)) 
v6.85_99.seasons[,,1] = apply(v6.85_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
v6.85_99.seasons[,,2] = apply(v6.85_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF







rm(u6.26_2049, u6.26_2099, v6.26_2049, v6.26_2099, u6.85_2049, u6.85_2099, v6.85_2049, v6.85_2099)

#### Humedad ####
# CNRM-CM6
load("RDatas/hu6.26_2049.RData")
load("RDatas/hu6.26_2099.RData")

hu6.26_49.seasons = array(data = NA, dim = c(144,73,2)) 
hu6.26_49.seasons[,,1] = apply(hu6.26_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
hu6.26_49.seasons[,,2] = apply(hu6.26_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

hu6.26_99.seasons = array(data = NA, dim = c(144,73,2)) 
hu6.26_99.seasons[,,1] = apply(hu6.26_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
hu6.26_99.seasons[,,2] = apply(hu6.26_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

rm(hu6.26_2049, hu6.26_2099)


#####
#--- RCP8.5 ---#

#### Temperatura ####
# CNRM - CM5
# no tiene datos mensuales

# CNRM-CM6
load("RDatas/t6.85_2049.RData")
load("RDatas/t6.85_2099.RData")

t6.85_49.seasons = array(data = NA, dim = c(144,73,2)) 
t6.85_49.seasons[,,1] = apply(t6.85_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
t6.85_49.seasons[,,2] = apply(t6.85_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

t6.85_99.seasons = array(data = NA, dim = c(144,73,2)) 
t6.85_99.seasons[,,1] = apply(t6.85_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
t6.85_99.seasons[,,2] = apply(t6.85_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

rm(t6.85_2049, t6.85_2099)

#### Precipitacion ####
# CNRM-CM5
load("RDatas/pp5.85_2049.RData")
load("RDatas/pp5.85_2099.RData")

pp5.85_49.seasons = array(data = NA, dim = c(144,73,2)) 
pp5.85_49.seasons[,,1] = apply(pp5.85_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
pp5.85_49.seasons[,,2] = apply(pp5.85_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

pp5.85_99.seasons = array(data = NA, dim = c(144,73,2)) 
pp5.85_99.seasons[,,1] = apply(pp5.85_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
pp5.85_99.seasons[,,2] = apply(pp5.85_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

rm(pp5.85_2049, pp5.85_2099)

# CNRM-CM6
load("RDatas/pp6.85_2049.RData")
load("RDatas/pp6.85_2099.RData")

pp6.85_49.seasons = array(data = NA, dim = c(144,73,2)) 
pp6.85_49.seasons[,,1] = apply(pp6.85_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
pp6.85_49.seasons[,,2] = apply(pp6.85_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

pp6.85_99.seasons = array(data = NA, dim = c(144,73,2)) 
pp6.85_99.seasons[,,1] = apply(pp6.85_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
pp6.85_99.seasons[,,2] = apply(pp6.85_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF


rm(pp6.85_2049, pp6.85_2099)

#### Viento ####
#CNRM-CM6
load("RDatas/u6.85_2049.RData")
load("RDatas/u6.85_2099.RData")
load("RDatas/v6.85_2049.RData")
load("RDatas/v6.85_2099.RData")

u6.85_49.seasons = array(data = NA, dim = c(144,73,2)) 
u6.85_49.seasons[,,1] = apply(u6.85_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
u6.85_49.seasons[,,2] = apply(u6.85_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF


u6.85_99.seasons = array(data = NA, dim = c(144,73,2)) 
u6.85_99.seasons[,,1] = apply(u6.85_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
u6.85_99.seasons[,,2] = apply(u6.85_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF


# v
v6.85_49.seasons = array(data = NA, dim = c(144,73,2)) 
v6.85_49.seasons[,,1] = apply(v6.85_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
v6.85_49.seasons[,,2] = apply(v6.85_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF


v6.85_99.seasons = array(data = NA, dim = c(144,73,2)) 
v6.85_99.seasons[,,1] = apply(v6.85_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
v6.85_99.seasons[,,2] = apply(v6.85_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

rm(u6.85_2049, u6.85_2099, v6.85_2049, v6.85_2099)

#### Humedad ####
# CNRM-CM6
load("RDatas/hu6.85_2049.RData")
load("RDatas/hu6.85_2099.RData")

hu6.85_49.seasons = array(data = NA, dim = c(144,73,2)) 
hu6.85_49.seasons[,,1] = apply(hu6.85_2049[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
hu6.85_49.seasons[,,2] = apply(hu6.85_2049[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

hu6.85_99.seasons = array(data = NA, dim = c(144,73,2)) 
hu6.85_99.seasons[,,1] = apply(hu6.85_2099[[1]][,,2,], c(1,2), mean, na.rm = T) # JJA
hu6.85_99.seasons[,,2] = apply(hu6.85_2099[[1]][,,4,], c(1,2), mean, na.rm = T) # DJF

rm(hu6.85_2049, hu6.85_2099)

