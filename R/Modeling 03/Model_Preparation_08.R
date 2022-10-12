
library(raster)
library(ggplot2) ## For plotting
library(spdep) ## For neighborhood structure




rm(list=ls())
# Read in data ------------------------------------------------------------
pse.dat = read.csv("../Data/FSW Spreadsheets/SSA_FSW_PSE_03.csv")
gadm.dhs = readRDS("../Data/Shapefiles/gadm36_adm1_africa_dhs.rds")
gadm.spatial = readRDS("../Data/gadm36_adm1_with_spatial.rds")
gadm.spatial$NAME_1 = gadm.dhs$NAME_1
gadm.cov = gadm.dhs

gadm.cov = sp::merge(gadm.cov, gadm.spatial@data)
gadm.cov$ID = 1:nrow(gadm.cov)
nightlight.cov = readRDS("../Data/Shapefiles/gadm36_adm1_nightlight.rds")


# Stage 0 (Separate areas) ------------------------------------------------
table(pse.dat$Validated_area, useNA = "always")
pse.national = subset(pse.dat, Validated_area %in% c("National"))
pse.dat = subset(pse.dat, Validated_area %in%
                   c("Adm1", "City", "Multi-city"))
pse.dat$City = 1
pse.dat$City[pse.dat$Validated_area == "Adm1"] = 0

pse.dat.full = merge(pse.dat, gadm.cov@data,
                     by.x = c("ISO", "Validated_adm1"),
                     by.y = c("ISO", "NAME_1"),
                     all.x = TRUE)
pse.dat.full = pse.dat.full[,
                            -which(names(pse.dat.full) %in% paste("PD", c(2000:2020),
                                                                  sep = "_"))]
pse.dat.full = pse.dat.full[,
                            -which(names(pse.dat.full) %in% paste0("GDP_X", c(1990:2015)))]

pse.dat.full = pse.dat.full[,
                            -which(names(pse.dat.full) %in% paste("Stunting", c(2000:2019),
                                                                  sep = "_"))]
pse.dat.full = pse.dat.full[,
                            -which(names(pse.dat.full) %in% paste("Stunting", c(2000:2019), "popweighted",
                                                                  sep = "_"))]
pse.dat.full = pse.dat.full[,
                            -which(names(pse.dat.full) %in% paste("Severe_wasting", c(2000:2019),
                                                                  sep = "_"))]
pse.dat.full = pse.dat.full[,
                            -which(names(pse.dat.full) %in% paste("Severe_wasting", c(2000:2019), "popweighted",
                                                                  sep = "_"))]
pse.dat.full = pse.dat.full[,
                            -which(names(pse.dat.full) %in% paste("Underweight", c(2000:2019),
                                                                  sep = "_"))]
pse.dat.full = pse.dat.full[,
                            -which(names(pse.dat.full) %in% paste("Underweight", c(2000:2019), "popweighted",
                                                                  sep = "_"))]
pse.dat.full = pse.dat.full[,
                            -which(names(pse.dat.full) %in% paste("Wasting", c(2000:2019),
                                                                  sep = "_"))]
pse.dat.full = pse.dat.full[,
                            -which(names(pse.dat.full) %in% paste("Wasting", c(2000:2019), "popweighted",
                                                                  sep = "_"))]
which(is.na(pse.dat.full$g6pdd_frequency_popweighted))

## Add nightlight too, every month in the same year
ind.2013 = ind.2014 = ind.2015 = ind.2016 =
  ind.2017 = ind.2018 = rep(NA, 48)
start.year = sapply(names(nightlight.cov),
                    function(x){substr(x, 1, 4)})
ind.2013 = which(start.year == "2013")
ind.2014 = which(start.year == "2014")
ind.2015 = which(start.year == "2015")
ind.2016 = which(start.year == "2016")
ind.2017 = which(start.year == "2017")
ind.2018 = which(start.year == "2018")

nightlight.add = matrix(NA, nrow = nrow(pse.dat.full),
                        ncol = length(ind.2013))
for(i in 1:nrow(pse.dat.full)){
  if(!is.na(pse.dat.full$data_year[i])){
    ref.year = pse.dat.full$data_year[i]
  }else{
    ref.year = pse.dat.full$year[i]
  }
  if(is.na(ref.year)){
    next
  }
  if(ref.year < 2013){
    ref.year = 2013
  }else if(ref.year > 2018){
    ref.year = 2018
  }
  
  if(!is.na(pse.dat.full$Validated_adm1[i])){
    gadm.match = which(nightlight.cov$ISO == pse.dat.full$ISO[i] &
                         nightlight.cov$NAME_1 == pse.dat.full$Validated_adm1[i])
    if(length(gadm.match) == 0){
      print(as.character(pse.dat.full$fsw_sizeestimate_region[i]))
      print(i)
    }
    if(ref.year == 2013){
      nightlight.add[i,] = as.numeric(nightlight.cov@data[gadm.match,ind.2013])
    }else if(ref.year == 2014){
      nightlight.add[i,] = as.numeric(nightlight.cov@data[gadm.match,ind.2014])
    }else if(ref.year == 2015){
      nightlight.add[i,] = as.numeric(nightlight.cov@data[gadm.match,ind.2015])
    }else if(ref.year == 2016){
      nightlight.add[i,] = as.numeric(nightlight.cov@data[gadm.match,ind.2016])
    }else if(ref.year == 2017){
      nightlight.add[i,] = as.numeric(nightlight.cov@data[gadm.match,ind.2017])
    }else if(ref.year == 2018){
      nightlight.add[i,] = as.numeric(nightlight.cov@data[gadm.match,ind.2018])
    }else{
      print(paste0("Missing ref.year:",i))
    }
  }
}
nightlight.add = as.data.frame(nightlight.add)
## Average mean, quant05, quant50, and quant95
NL.mean = rowMeans(nightlight.add[,seq(1, ncol(nightlight.add), by = 4)])
NL.quant05 = rowMeans(nightlight.add[,seq(2, ncol(nightlight.add), by = 4)])
NL.quant50 = rowMeans(nightlight.add[,seq(3, ncol(nightlight.add), by = 4)])
NL.quant95 = rowMeans(nightlight.add[,seq(4, ncol(nightlight.add), by = 4)])

pse.dat.full$NL_mean = NL.mean
pse.dat.full$NL_quant05 = NL.quant05
pse.dat.full$NL_quant50 = NL.quant50
pse.dat.full$NL_quant95 = NL.quant95


## Add population density
PD.add = matrix(NA, nrow = nrow(pse.dat.full), ncol = 1)
for(i in 1:nrow(pse.dat.full)){
  if(!is.na(pse.dat.full$data_year[i])){
    ref.year = pse.dat.full$data_year[i]
  }else{
    ref.year = pse.dat.full$year[i]
  }
  if(is.na(ref.year)){
    next
  }
  
  if(!is.na(pse.dat.full$Validated_adm1[i])){
    gadm.match = which(gadm.cov$ISO == pse.dat.full$ISO[i] &
                         gadm.cov$NAME_1 == pse.dat.full$Validated_adm1[i])
    if(length(gadm.match) == 0){
      print(as.character(pse.dat.full$fsw_sizeestimate_region[i]))
      print(i)
      next
    }
    
    col.name = paste("PD", ref.year, sep = "_")
    PD.add[i] = gadm.cov@data[gadm.match,col.name]
  }
}

pse.dat.full$PD = as.numeric(PD.add)


## Add GDP
GDP.add = matrix(NA, nrow = nrow(pse.dat.full), ncol = 1)
for(i in 1:nrow(pse.dat.full)){
  if(!is.na(pse.dat.full$data_year[i])){
    ref.year = pse.dat.full$data_year[i]
  }else{
    ref.year = pse.dat.full$year[i]
  }
  if(is.na(ref.year)){
    next
  }
  
  if(ref.year > 2015){
    ref.year = 2015
  }
  
  if(!is.na(pse.dat.full$Validated_adm1[i])){
    gadm.match = which(gadm.cov$ISO == pse.dat.full$ISO[i] &
                         gadm.cov$NAME_1 == pse.dat.full$Validated_adm1[i])
    if(length(gadm.match) == 0){
      print(as.character(pse.dat.full$fsw_sizeestimate_region[i]))
      print(i)
      next
    }
    
    col.name = paste0("GDP_X", ref.year)
    GDP.add[i] = gadm.cov@data[gadm.match,col.name]
  }
}

pse.dat.full$GDP = as.numeric(GDP.add)



## Add stunting
stunting.add = matrix(NA, nrow = nrow(pse.dat.full), ncol = 8)
for(i in 1:nrow(pse.dat.full)){
  if(!is.na(pse.dat.full$data_year[i])){
    ref.year = pse.dat.full$data_year[i]
  }else{
    ref.year = pse.dat.full$year[i]
  }
  if(is.na(ref.year)){
    next
  }
  
  if(!is.na(pse.dat.full$Validated_adm1[i])){
    gadm.match = which(gadm.cov$ISO == pse.dat.full$ISO[i] &
                         gadm.cov$NAME_1 == pse.dat.full$Validated_adm1[i])
    if(length(gadm.match) == 0){
      print(as.character(pse.dat.full$fsw_sizeestimate_region[i]))
      print(i)
      next
    }
    
    stunting.col = paste("Stunting", ref.year, sep = "_")
    severe.wasting.col = paste("Severe_wasting", ref.year, sep = "_")
    underweight.col = paste("Underweight", ref.year, sep = "_")
    wasting.col = paste("Wasting", ref.year, sep = "_")
    stunting.popweighted.col = paste("Stunting", ref.year, "popweighted", sep = "_")
    severe.wasting.popweighted.col = paste("Severe_wasting", ref.year, "popweighted", sep = "_")
    underweight.popweighted.col = paste("Underweight", ref.year, "popweighted", sep = "_")
    wasting.popweighted.col = paste("Wasting", ref.year, "popweighted", sep = "_")
    stunting.add[i,] = as.numeric(gadm.cov@data[gadm.match,
                                                c(stunting.col,stunting.popweighted.col,
                                                  severe.wasting.col,severe.wasting.popweighted.col,
                                                  underweight.col,underweight.popweighted.col,
                                                  wasting.col,wasting.popweighted.col)])
  }
}
colnames(stunting.add) = c("stunting", "stunting_popweighted", "severe_wasting", "severe_wasting_popweighted",
                           "underweight", "underweight_popweighted",
                           "wasting", "wasting_popweighted")
pse.dat.full = cbind(pse.dat.full, stunting.add)





# Also add nightlight for shapefile --------------------------------------
## Use 2015
nightlight.gadm.add = nightlight.cov@data[,ind.2015]
## Average mean, quant05, quant50, and quant95
NL.mean = rowMeans(nightlight.gadm.add[,seq(1, ncol(nightlight.gadm.add), by = 4)])
NL.quant05 = rowMeans(nightlight.gadm.add[,seq(2, ncol(nightlight.gadm.add), by = 4)])
NL.quant50 = rowMeans(nightlight.gadm.add[,seq(3, ncol(nightlight.gadm.add), by = 4)])
NL.quant95 = rowMeans(nightlight.gadm.add[,seq(4, ncol(nightlight.gadm.add), by = 4)])
gadm.cov@data$NL_mean = NL.mean
gadm.cov@data$NL_quant05 = NL.quant05
gadm.cov@data$NL_quant50 = NL.quant50
gadm.cov@data$NL_quant95 = NL.quant95


gadm.cov$PD = gadm.cov$PD_2015
gadm.cov@data = gadm.cov@data[,
                              -which(names(gadm.cov@data) %in% paste("PD", c(2000:2020),
                                                                     sep = "_"))]
gadm.cov$GDP = gadm.cov$GDP_X2015
gadm.cov@data = gadm.cov@data[,
                              -which(names(gadm.cov@data) %in% paste0("GDP_X", c(1990:2015)))]
gadm.cov$stunting = gadm.cov$Stunting_2015
gadm.cov$stunting_popweighted = gadm.cov$Stunting_2015_popweighted
gadm.cov@data = gadm.cov@data[,
                              -which(names(gadm.cov@data) %in% paste("Stunting", c(2000:2019),
                                                                     sep = "_"))]
gadm.cov@data = gadm.cov@data[,
                              -which(names(gadm.cov@data) %in% paste("Stunting", c(2000:2019), "popweighted",
                                                                     sep = "_"))]
gadm.cov$severe_wasting = gadm.cov$Severe_wasting_2015
gadm.cov$severe_wasting_popweighted = gadm.cov$Severe_wasting_2015_popweighted
gadm.cov@data = gadm.cov@data[,
                              -which(names(gadm.cov@data) %in% paste("Severe_wasting", c(2000:2019),
                                                                     sep = "_"))]
gadm.cov@data = gadm.cov@data[,
                              -which(names(gadm.cov@data) %in% paste("Severe_wasting", c(2000:2019), "popweighted",
                                                                     sep = "_"))]
gadm.cov$underweight = gadm.cov$Underweight_2015
gadm.cov$underweight_popweighted = gadm.cov$Underweight_2015_popweighted
gadm.cov@data = gadm.cov@data[,
                              -which(names(gadm.cov@data) %in% paste("Underweight", c(2000:2019),
                                                                     sep = "_"))]
gadm.cov@data = gadm.cov@data[,
                              -which(names(gadm.cov@data) %in% paste("Underweight", c(2000:2019), "popweighted",
                                                                     sep = "_"))]
gadm.cov$wasting = gadm.cov$Wasting_2015
gadm.cov$wasting_popweighted = gadm.cov$Wasting_2015_popweighted
gadm.cov@data = gadm.cov@data[,
                              -which(names(gadm.cov@data) %in% paste("Wasting", c(2000:2019),
                                                                     sep = "_"))]
gadm.cov@data = gadm.cov@data[,
                              -which(names(gadm.cov@data) %in% paste("Wasting", c(2000:2019), "popweighted",
                                                                     sep = "_"))]


###########################
## Full spatial model
###########################
pse.dat.fit = subset(pse.dat.full, !is.na(pse) &
                       !is.na(ref_pop_joined) &
                       !is.na(Validated_adm1))


## Get spatial info
nb.RK = poly2nb(gadm.cov, queen=TRUE, row.names=gadm.cov$ID,
                snap = 1e-4)
n.dist = nrow(gadm.cov)
nb.mat <- nb2mat(nb.RK, zero.policy = TRUE, style = "B")

## Match district to shapefile


pse.dat.fit$fsw_sizeestimate_region[is.na(pse.dat.fit$ID)]
pse.dat.fit = subset(pse.dat.fit, !is.na(ID))
## Remove the data that doesn't have any shapefile attached to it


## Add reference population to gadm
gadm.36.adm1 = readRDS("../Data/Shapefiles/gadm36_adm1_africa_fpop.rds")
gadm.cov@data$ref_pop = gadm.36.adm1$worldpop_fcba_pop
gadm.cov@data$worldpop_fcba_ratio = gadm.36.adm1$worldpop_fcba_ratio
gadm.cov@data$worldpop_total_pop = gadm.36.adm1$worldpop_total_pop

## Method re
table(pse.dat.fit$method_std, useNA = "always")
method.re.ind = as.numeric(factor(pse.dat.fit$method_std))

## Country re
n.country = length(unique(gadm.cov@data$ISO))
pse.dat.fit$ISO = factor(pse.dat.fit$ISO,
                         levels = unique(gadm.cov@data$ISO))
country.re.ind = as.numeric(pse.dat.fit$ISO)

pse.dat.fit$Validated_area[pse.dat.fit$Validated_area == "Multi-city"] = "City"

saveRDS(pse.dat.fit, "../Data/PSE_Prep.rds")
saveRDS(gadm.cov, "../Data/Shapefile_Prep.rds")
saveRDS(nb.mat, "../Data/nb_mat_Prep.rds")
