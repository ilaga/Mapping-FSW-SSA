
###########################################
##
## In this file, we get the ratio of child-bearing
## females in every district in sub-Saharan Africa
##
###########################################

## Process:
## (1) Download shapefiles via getData
## (2) Download total worldPop in 2015 at 1km res for each country
## (3) Download child-bearing worldPop at 1km res for each country
## (4) Divide total child-bearing by total total in each district

library(raster)
library(sp)

rm(list=ls())
## Load shapefiles
gadm.36.adm1 = readRDS("../Data/Shapefiles/gadm36_adm1_africa.rds")
gadm.36.country = raster::aggregate(gadm.36.adm1, by = "ISO")

gadm.36.adm1$worldpop_total_pop = NA
gadm.36.adm1$worldpop_fcba_pop = NA
gadm.36.adm1$worldpop_fcba_ratio = NA
gadm.36.country$worldpop_total_pop = NA
gadm.36.country$worldpop_fcba_pop = NA
gadm.36.country$worldpop_fcba_ratio = NA

## Do adm1 and adm2 first
for(iso.ind in unique(c(gadm.36.adm1$ISO))){
  ## Load general population and female population worldPop for ISO
  total.name = paste0("../Data/WorldPop_total_data/",iso.ind,"_total.tif")
  fcba.name = paste0("../Data/WorldPop_fcba_data/",iso.ind,".tif")
  
  total.link = paste0("ftp://ftp.worldpop.org.uk/GIS/Population/Global_2000_2020_1km/2015/",
                    iso.ind,"/",tolower(iso.ind),
                    "_ppp_2015_1km_Aggregated.tif")
  curl::curl_download(total.link, total.name)
  
  fcba.link = paste0("ftp://ftp.worldpop.org/GIS/AgeSex_structures/Women_of_child_bearing_age/15_49_years_old/2015/",
                    iso.ind,"/",tolower(iso.ind),
                    "_f_15_49_2015_1km.tif")
  curl::curl_download(fcba.link, fcba.name)
  
  country.total.worldPop = raster(total.name)
  country.fcba.worldPop = raster(fcba.name)
  
  ## For Adm1
  ## Subset country_shapes inside ISO
  gadm.36.adm1.sub = subset(gadm.36.adm1, ISO == iso.ind)
  gadm.36.adm1.ind = which(gadm.36.adm1$ISO == iso.ind)
  
  if(length(gadm.36.adm1.ind) > 0){
    ## Get worldPop inside the districts
    total.pop.adm1 = fcba.pop.adm1 = fcba.ratio.adm1 = rep(NA, nrow(gadm.36.adm1.sub))
    for(j in 1:nrow(gadm.36.adm1.sub)){
      adm1.sub = gadm.36.adm1.sub[j,]
      image.crop = try(crop(country.total.worldPop, extent(adm1.sub)))
      total.rasterize = rasterize(adm1.sub, image.crop, mask = TRUE)
      total.pop.adm1[j] = sum(getValues(total.rasterize), na.rm = T)
      image.crop = try(crop(country.fcba.worldPop, extent(adm1.sub)))
      fcba.rasterize = rasterize(adm1.sub, image.crop, mask = TRUE)
      fcba.pop.adm1[j] = sum(getValues(fcba.rasterize), na.rm = T)
      fcba.ratio.adm1[j] = fcba.pop.adm1[j] / total.pop.adm1[j]
    }
    gadm.36.adm1$worldpop_total_pop[gadm.36.adm1.ind] = total.pop.adm1
    gadm.36.adm1$worldpop_fcba_pop[gadm.36.adm1.ind] = fcba.pop.adm1
    gadm.36.adm1$worldpop_fcba_ratio[gadm.36.adm1.ind] = fcba.ratio.adm1
  }
  

  print(iso.ind)
}


saveRDS(gadm.36.adm1, "../Data/Shapefiles/gadm36_adm1_africa_fpop.rds")


## Then do country
for(iso.ind in unique(gadm.36.country$ISO)){
  ## Load general population and female population worldPop for ISO
  total.name = paste0("../Data/WorldPop_total_data/",iso.ind,"_total.tif")
  fcba.name = paste0("../Data/WorldPop_fcba_data/",iso.ind,".tif")
  
  total.link = paste0("ftp://ftp.worldpop.org.uk/GIS/Population/Global_2000_2020_1km/2015/",
                      iso.ind,"/",tolower(iso.ind),
                      "_ppp_2015_1km_Aggregated.tif")
  curl::curl_download(total.link, total.name)
  
  fcba.link = paste0("ftp://ftp.worldpop.org/GIS/AgeSex_structures/Women_of_child_bearing_age/15_49_years_old/2015/",
                     iso.ind,"/",tolower(iso.ind),
                     "_f_15_49_2015_1km.tif")
  curl::curl_download(fcba.link, fcba.name)
  
  country.total.worldPop = raster(total.name)
  country.fcba.worldPop = raster(fcba.name)
  
  ## For Country
  ## Subset country_shapes inside ISO
  gadm.36.country.sub = subset(gadm.36.country, ISO == iso.ind)
  gadm.36.country.ind = which(gadm.36.country$ISO == iso.ind)
  
  if(length(gadm.36.country.ind) > 0){
    ## Get worldPop inside the districts
    country.sub = gadm.36.country.sub
    image.crop = try(crop(country.total.worldPop, extent(country.sub)))
    total.rasterize = rasterize(country.sub, image.crop, mask = TRUE)
    total.pop.country = sum(getValues(total.rasterize), na.rm = T)
    image.crop = try(crop(country.fcba.worldPop, extent(country.sub)))
    fcba.rasterize = rasterize(country.sub, image.crop, mask = TRUE)
    fcba.pop.country = sum(getValues(fcba.rasterize), na.rm = T)
    fcba.ratio.country = fcba.pop.country / total.pop.country
    gadm.36.country$worldpop_total_pop[gadm.36.country.ind] = total.pop.country
    gadm.36.country$worldpop_fcba_pop[gadm.36.country.ind] = fcba.pop.country
    gadm.36.country$worldpop_fcba_ratio[gadm.36.country.ind] = fcba.ratio.country
  }
  
  print(iso.ind)
}

saveRDS(gadm.36.country, "../Data/Shapefiles/gadm36_country_africa_fpop.rds")
