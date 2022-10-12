
library(raster)
library(sp) ## For projections
library(rgdal) ## For projections
library(plyr)


rm(list=ls())
# Read in data ------------------------------------------------------------
ssa.iso = readxl::read_excel("../Data/Africa_ISO.xlsx")
ssa.iso = ssa.iso[,1:2]
ssa.iso = subset(ssa.iso, Included == "TRUE")


gadm.36 = readRDS("../Data/Shapefiles/gadm36_adm1_africa_fpop.rds")
gadm.36 = subset(gadm.36, ISO %in% ssa.iso$ISO)
worldpop = raster("../Data/Spatial Data/ppp_2015_1km_Aggregated.tif")
worldpop.ssa = raster::crop(worldpop, extent(gadm.36))


################################################################################
################################################################################
## Add Landcover
################################################################################
################################################################################

landcover.300m <- raster::raster('../Data/LandCover/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif')

landcover.ssa = raster::crop(landcover.300m, extent(gadm.36))

landcover.df = landcover.popweighted.df = matrix(NA, nrow = nrow(gadm.36), ncol = 22)
for(ind in 1:nrow(gadm.36)) {
  ctry.shps.tmp = gadm.36[ind,]
  landcover.crop = try(crop(landcover.ssa, extent(ctry.shps.tmp)))
  worldpop.crop = try(crop(worldpop.ssa, extent(landcover.crop) + 2))
  
  ## Get worldpop points inside raster
  worldpop.resample = resample(worldpop.crop, landcover.crop)
  
  if(class(landcover.crop) == "try-error"){
    next
  }
  ctry.shps.sub.rasterize = rasterize(ctry.shps.tmp, landcover.crop, mask = TRUE)
  worldpop.rasterize = rasterize(ctry.shps.tmp, worldpop.resample, mask = TRUE)
  ext = getValues(ctry.shps.sub.rasterize)
  ext.worldpop = getValues(worldpop.rasterize)
  if(all(is.na(ext)) | all(is.na(ext.worldpop))){
    ext = raster::extract(worldclim.crop, ctry.shps.tmp)[[1]]
    ext.worldpop = raster::extract(worldpop.resample, ctry.shps.tmp)[[1]]
  }
  ## These values are categorical
  ## First round to the nearest 10
  ext = round_any(ext, 10)
  ext[ext == 0] = NA
  ext = as.character(ext)
  mapping = cbind(seq(10, 220, by = 10),
                  c("cropland_rainfed", "cropland_irrigated",
                    "mosaic_cropland", "mosaic_natural_veg",
                    "tree_cover_becto", "tree_cover_bdcto",
                    "tree_cover_necto", "tree_cover_ndcto",
                    "tree_cover_mlt", "mosaic_tree",
                    "mosaic_herbaceous", "shrubland",
                    "grassland", "lichens", "sparse",
                    "tree_cover_fresh", "tree_cover_saline",
                    "shrub_cover", "urban", "bare", "water", "snow_ice"))
  ext = mapping[match(ext, mapping[,1]), 2]
  which.not.na = which(!is.na(ext))
  ext.factor = factor(ext, levels = mapping[,2])
  ext.mm = model.matrix(~ext.factor - 1) ## This removes na by default
  
  landcover.df[ind,] = colMeans(ext.mm)
  ext.worldpop = ext.worldpop[which.not.na] / sum(ext.worldpop[which.not.na], na.rm = T)
  landcover.popweighted.df[ind,] = colMeans(ext.mm * ext.worldpop, na.rm = T)

  print(ind / nrow(gadm.36) * 100)
}
landcover.df = data.frame(landcover.df)
names(landcover.df) <- mapping[,2]
gadm.36@data = cbind(gadm.36@data, landcover.df)






################################################################################
################################################################################
## Add Worldpop and Worldpop Density for all years
################################################################################
################################################################################

## Load all worldpop raster .tif files
worldpop.2000 = raster("../Data/Spatial Data/ppp_2000_1km_Aggregated.tif")
worldpop.2000.ssa = raster::crop(worldpop.2000, extent(gadm.36))
worldpop.2001 = raster("../Data/Spatial Data/ppp_2001_1km_Aggregated.tif")
worldpop.2001.ssa = raster::crop(worldpop.2001, extent(gadm.36))
worldpop.2002 = raster("../Data/Spatial Data/ppp_2002_1km_Aggregated.tif")
worldpop.2002.ssa = raster::crop(worldpop.2002, extent(gadm.36))
worldpop.2003 = raster("../Data/Spatial Data/ppp_2003_1km_Aggregated.tif")
worldpop.2003.ssa = raster::crop(worldpop.2003, extent(gadm.36))
worldpop.2004 = raster("../Data/Spatial Data/ppp_2004_1km_Aggregated.tif")
worldpop.2004.ssa = raster::crop(worldpop.2004, extent(gadm.36))
worldpop.2005 = raster("../Data/Spatial Data/ppp_2005_1km_Aggregated.tif")
worldpop.2005.ssa = raster::crop(worldpop.2005, extent(gadm.36))
worldpop.2006 = raster("../Data/Spatial Data/ppp_2006_1km_Aggregated.tif")
worldpop.2006.ssa = raster::crop(worldpop.2006, extent(gadm.36))
worldpop.2007 = raster("../Data/Spatial Data/ppp_2007_1km_Aggregated.tif")
worldpop.2007.ssa = raster::crop(worldpop.2007, extent(gadm.36))
worldpop.2008 = raster("../Data/Spatial Data/ppp_2008_1km_Aggregated.tif")
worldpop.2008.ssa = raster::crop(worldpop.2008, extent(gadm.36))
worldpop.2009 = raster("../Data/Spatial Data/ppp_2009_1km_Aggregated.tif")
worldpop.2009.ssa = raster::crop(worldpop.2009, extent(gadm.36))
worldpop.2010 = raster("../Data/Spatial Data/ppp_2010_1km_Aggregated.tif")
worldpop.2010.ssa = raster::crop(worldpop.2010, extent(gadm.36))
worldpop.2011 = raster("../Data/Spatial Data/ppp_2011_1km_Aggregated.tif")
worldpop.2011.ssa = raster::crop(worldpop.2011, extent(gadm.36))
worldpop.2012 = raster("../Data/Spatial Data/ppp_2012_1km_Aggregated.tif")
worldpop.2012.ssa = raster::crop(worldpop.2012, extent(gadm.36))
worldpop.2013 = raster("../Data/Spatial Data/ppp_2013_1km_Aggregated.tif")
worldpop.2013.ssa = raster::crop(worldpop.2013, extent(gadm.36))
worldpop.2014 = raster("../Data/Spatial Data/ppp_2014_1km_Aggregated.tif")
worldpop.2014.ssa = raster::crop(worldpop.2014, extent(gadm.36))
worldpop.2015 = raster("../Data/Spatial Data/ppp_2015_1km_Aggregated.tif")
worldpop.2015.ssa = raster::crop(worldpop.2015, extent(gadm.36))
worldpop.2016 = raster("../Data/Spatial Data/ppp_2016_1km_Aggregated.tif")
worldpop.2016.ssa = raster::crop(worldpop.2016, extent(gadm.36))
worldpop.2017 = raster("../Data/Spatial Data/ppp_2017_1km_Aggregated.tif")
worldpop.2017.ssa = raster::crop(worldpop.2017, extent(gadm.36))
worldpop.2018 = raster("../Data/Spatial Data/ppp_2018_1km_Aggregated.tif")
worldpop.2018.ssa = raster::crop(worldpop.2018, extent(gadm.36))
worldpop.2019 = raster("../Data/Spatial Data/ppp_2019_1km_Aggregated.tif")
worldpop.2019.ssa = raster::crop(worldpop.2019, extent(gadm.36))
worldpop.2020 = raster("../Data/Spatial Data/ppp_2020_1km_Aggregated.tif")
worldpop.2020.ssa = raster::crop(worldpop.2020, extent(gadm.36))

worldpop.stack = stack(worldpop.2000.ssa, worldpop.2001.ssa, worldpop.2002.ssa,
                       worldpop.2003.ssa, worldpop.2004.ssa, worldpop.2005.ssa,
                       worldpop.2006.ssa, worldpop.2007.ssa, worldpop.2008.ssa,
                       worldpop.2009.ssa, worldpop.2010.ssa, worldpop.2011.ssa,
                       worldpop.2012.ssa, worldpop.2013.ssa, worldpop.2014.ssa,
                       worldpop.2015.ssa, worldpop.2016.ssa, worldpop.2017.ssa,
                       worldpop.2018.ssa, worldpop.2019.ssa, worldpop.2020.ssa)
district.areas = area(gadm.36)


gadm.pop.density = matrix(NA, nrow = nrow(gadm.36), ncol = dim(worldpop.stack)[3])
for(ind in 1:nrow(gadm.36)) {
  ctry.shps.tmp = gadm.36[ind,]
  pop.density.crop = try(crop(worldpop.stack, extent(ctry.shps.tmp)))
  
  pop.density.rasterize = rasterize(ctry.shps.tmp, pop.density.crop, mask = TRUE)
  ext.pop.density = getValues(pop.density.rasterize)
  if(all(is.na(ext.pop.density))){
    ext.pop.density = raster::extract(pop.density.crop, ctry.shps.tmp)[[1]]
  }
  
  
  gadm.pop.density[ind,] = colSums(ext.pop.density, na.rm = T) /
    district.areas[ind]
 
  print(round(ind / nrow(gadm.36) * 100, digits = 2))
}
gadm.pop.density = as.data.frame(gadm.pop.density)
names(gadm.pop.density) = paste("PD", c(2000:2020), sep = "_")
gadm.36@data = cbind(gadm.36@data, gadm.pop.density)






################################################################################
################################################################################
## Add Stunting, wasting, underweight, and severe wasting
################################################################################
################################################################################

## Load all stunting raster .tif files
stunting.list = severe.wasting.list = underweight.list = wasting.list = list()
j = 1
for(i in 2000:2019){
  name = paste0("../Data/Spatial Data/Stunting_Data/stunting_under_5/",
                "IHME_GLOBAL_CGF_2000_2019_STUNTING_PREV_PERCENT_A1_S3_MEAN_",
                i,"_Y2020M08D31.tif")
  read.dat = raster(name)
  read.crop = raster::crop(read.dat, extent(gadm.36) + 5)
  stunting.list[[j]] = read.crop
  
  name = paste0("../Data/Spatial Data/Stunting_Data/severe_wasting_under_5/",
                "IHME_GLOBAL_CGF_2000_2019_SEVERE_WASTING_PREV_PERCENT_A1_S3_MEAN_",
                i,"_Y2020M08D31.tif")
  read.dat = raster(name)
  read.crop = raster::crop(read.dat, extent(gadm.36) + 5)
  severe.wasting.list[[j]] = read.crop
  
  name = paste0("../Data/Spatial Data/Stunting_Data/underweight_under_5/",
                "IHME_GLOBAL_CGF_2000_2019_UNDERWEIGHT_PREV_PERCENT_A1_S3_MEAN_",
                i,"_Y2020M08D31.tif")
  read.dat = raster(name)
  read.crop = raster::crop(read.dat, extent(gadm.36) + 5)
  underweight.list[[j]] = read.crop
  
  name = paste0("../Data/Spatial Data/Stunting_Data/wasting_under_5/",
                "IHME_GLOBAL_CGF_2000_2019_WASTING_PREV_PERCENT_A1_S3_MEAN_",
                i,"_Y2020M08D31.tif")
  read.dat = raster(name)
  read.crop = raster::crop(read.dat, extent(gadm.36) + 5)
  wasting.list[[j]] = read.crop
  
  
  
  j = j + 1
}
stunting.stack = stack(stunting.list)
severe.wasting.stack = stack(severe.wasting.list)
underweight.stack = stack(underweight.list)
wasting.stack = stack(wasting.list)

worldpop.stack = stack(worldpop.2000.ssa, worldpop.2001.ssa, worldpop.2002.ssa,
                       worldpop.2003.ssa, worldpop.2004.ssa, worldpop.2005.ssa,
                       worldpop.2006.ssa, worldpop.2007.ssa, worldpop.2008.ssa,
                       worldpop.2009.ssa, worldpop.2010.ssa, worldpop.2011.ssa,
                       worldpop.2012.ssa, worldpop.2013.ssa, worldpop.2014.ssa,
                       worldpop.2015.ssa, worldpop.2016.ssa, worldpop.2017.ssa,
                       worldpop.2018.ssa, worldpop.2019.ssa)



gadm.stunting = gadm.stunting.popweight = gadm.severe.wasting = gadm.severe.wasting.popweight =
  gadm.underweight = gadm.underweight.popweight = gadm.wasting = gadm.wasting.popweight =
  matrix(NA, nrow = nrow(gadm.36), ncol = dim(stunting.stack)[3])
for(ind in 1:nrow(gadm.36)) {
  ctry.shps.tmp = gadm.36[ind,]
  stunting.crop = try(crop(stunting.stack, extent(ctry.shps.tmp) + 1))
  severe.wasting.crop = try(crop(severe.wasting.stack, extent(ctry.shps.tmp) + 1))
  underweight.crop = try(crop(underweight.stack, extent(ctry.shps.tmp) + 1))
  wasting.crop = try(crop(wasting.stack, extent(ctry.shps.tmp) + 1))
  worldpop.crop = try(crop(worldpop.stack, extent(stunting.crop) + 2))
  
  ## Get worldpop points inside raster
  worldpop.resample = resample(worldpop.crop, stunting.crop)

  stunting.rasterize = rasterize(ctry.shps.tmp, stunting.crop, mask = TRUE)
  severe.wasting.rasterize = rasterize(ctry.shps.tmp, severe.wasting.crop, mask = TRUE)
  underweight.rasterize = rasterize(ctry.shps.tmp, underweight.crop, mask = TRUE)
  wasting.rasterize = rasterize(ctry.shps.tmp, wasting.crop, mask = TRUE)
  worldpop.rasterize = rasterize(ctry.shps.tmp, worldpop.resample, mask = TRUE)
  ext.stunting = getValues(stunting.rasterize)
  ext.severe.wasting = getValues(severe.wasting.rasterize)
  ext.underweight = getValues(underweight.rasterize)
  ext.wasting = getValues(wasting.rasterize)
  ext.worldpop = getValues(worldpop.rasterize)
  if(all(is.na(ext.stunting)) | all(is.na(ext.severe.wasting)) | all(is.na(ext.underweight))|
     all(is.na(ext.wasting)) | all(is.na(ext.worldpop))){
    ext.stunting = raster::extract(stunting.crop, ctry.shps.tmp)[[1]]
    ext.severe.wasting = raster::extract(severe.wasting.crop, ctry.shps.tmp)[[1]]
    ext.underweight = raster::extract(underweight.crop, ctry.shps.tmp)[[1]]
    ext.wasting = raster::extract(wasting.crop, ctry.shps.tmp)[[1]]
    ext.worldpop = raster::extract(worldpop.resample, ctry.shps.tmp)[[1]]
  }
  
  gadm.stunting[ind,] = colMeans(ext.stunting, na.rm = T)
  gadm.severe.wasting[ind,] = colMeans(ext.severe.wasting, na.rm = T)
  gadm.underweight[ind,] = colMeans(ext.underweight, na.rm = T)
  gadm.wasting[ind,] = colMeans(ext.wasting, na.rm = T)
  
  
  ext.worldpop = t(t(ext.worldpop) / colSums(ext.worldpop, na.rm = T)) ## Standardize by district
  gadm.stunting.popweight[ind,] = colMeans(ext.stunting * ext.worldpop, na.rm = T)
  gadm.severe.wasting.popweight[ind,] = colMeans(ext.severe.wasting * ext.worldpop, na.rm = T)
  gadm.underweight.popweight[ind,] = colMeans(ext.underweight * ext.worldpop, na.rm = T)
  gadm.wasting.popweight[ind,] = colMeans(ext.wasting * ext.worldpop, na.rm = T)
  
  
  
  print(round(ind / nrow(gadm.36) * 100, digits = 2))
}
gadm.stunting = as.data.frame(gadm.stunting)
names(gadm.stunting) = paste("Stunting", c(2000:2019), sep = "_")
gadm.stunting.popweight = as.data.frame(gadm.stunting.popweight)
names(gadm.stunting.popweight) = paste("Stunting", c(2000:2019), "popweighted", sep = "_")

gadm.severe.wasting = as.data.frame(gadm.severe.wasting)
names(gadm.severe.wasting) = paste("Severe_wasting", c(2000:2019), sep = "_")
gadm.severe.wasting.popweight = as.data.frame(gadm.severe.wasting.popweight)
names(gadm.severe.wasting.popweight) = paste("Severe_wasting", c(2000:2019), "popweighted", sep = "_")

gadm.underweight = as.data.frame(gadm.underweight)
names(gadm.underweight) = paste("Underweight", c(2000:2019), sep = "_")
gadm.underweight.popweight = as.data.frame(gadm.underweight.popweight)
names(gadm.underweight.popweight) = paste("Underweight", c(2000:2019), "popweighted", sep = "_")

gadm.wasting = as.data.frame(gadm.wasting)
names(gadm.wasting) = paste("Wasting", c(2000:2019), sep = "_")
gadm.wasting.popweight = as.data.frame(gadm.wasting.popweight)
names(gadm.wasting.popweight) = paste("Wasting", c(2000:2019), "popweighted", sep = "_")

gadm.36@data = cbind(gadm.36@data, gadm.stunting, gadm.stunting.popweight,
                     gadm.severe.wasting, gadm.severe.wasting.popweight,
                     gadm.underweight, gadm.underweight.popweight,
                     gadm.wasting, gadm.wasting.popweight)













################################################################################
################################################################################
## Add WorldClim
################################################################################
################################################################################

wind = raster("../Data/Spatial Data/wc2.1_2.5m_wind/wc2.1_2.5m_wind_01.tif")
vapr = raster("../Data/Spatial Data/wc2.1_2.5m_vapr/wc2.1_2.5m_vapr_01.tif")
srad = raster("../Data/Spatial Data/wc2.1_2.5m_srad/wc2.1_2.5m_srad_01.tif")
prec = raster("../Data/Spatial Data/wc2.1_2.5m_prec/wc2.1_2.5m_prec_01.tif")
elev = raster("../Data/Spatial Data/wc2.1_2.5m_elev/wc2.1_2.5m_elev.tif")

bio.mean.tmp = raster("../Data/Spatial Data/wc2.1_2.5m_bio/wc2.1_2.5m_bio_1.tif")
bio.mean.range = raster("../Data/Spatial Data/wc2.1_2.5m_bio/wc2.1_2.5m_bio_2.tif")
bio.isotherm = raster("../Data/Spatial Data/wc2.1_2.5m_bio/wc2.1_2.5m_bio_3.tif")
bio.seasonality = raster("../Data/Spatial Data/wc2.1_2.5m_bio/wc2.1_2.5m_bio_4.tif")
bio.max = raster("../Data/Spatial Data/wc2.1_2.5m_bio/wc2.1_2.5m_bio_5.tif")
bio.min = raster("../Data/Spatial Data/wc2.1_2.5m_bio/wc2.1_2.5m_bio_6.tif")
bio.ann.range = raster("../Data/Spatial Data/wc2.1_2.5m_bio/wc2.1_2.5m_bio_7.tif")
bio.temp.wet = raster("../Data/Spatial Data/wc2.1_2.5m_bio/wc2.1_2.5m_bio_8.tif")
bio.temp.dry = raster("../Data/Spatial Data/wc2.1_2.5m_bio/wc2.1_2.5m_bio_9.tif")
bio.temp.warm = raster("../Data/Spatial Data/wc2.1_2.5m_bio/wc2.1_2.5m_bio_10.tif")
bio.temp.cold = raster("../Data/Spatial Data/wc2.1_2.5m_bio/wc2.1_2.5m_bio_11.tif")
bio.prec = raster("../Data/Spatial Data/wc2.1_2.5m_bio/wc2.1_2.5m_bio_12.tif")
bio.prec.wet = raster("../Data/Spatial Data/wc2.1_2.5m_bio/wc2.1_2.5m_bio_13.tif")
bio.prec.dry = raster("../Data/Spatial Data/wc2.1_2.5m_bio/wc2.1_2.5m_bio_14.tif")
bio.prec.seas = raster("../Data/Spatial Data/wc2.1_2.5m_bio/wc2.1_2.5m_bio_15.tif")
bio.prec.wet.q = raster("../Data/Spatial Data/wc2.1_2.5m_bio/wc2.1_2.5m_bio_16.tif")
bio.prec.dry.q = raster("../Data/Spatial Data/wc2.1_2.5m_bio/wc2.1_2.5m_bio_17.tif")
bio.prec.warm.q = raster("../Data/Spatial Data/wc2.1_2.5m_bio/wc2.1_2.5m_bio_18.tif")
bio.prec.cold.q = raster("../Data/Spatial Data/wc2.1_2.5m_bio/wc2.1_2.5m_bio_19.tif")

crs(wind)

## Fortunately, all of these are the same dimensions, so do them together
worldclim = stack(wind, vapr, srad, prec, elev, bio.mean.tmp,
                  bio.mean.range, bio.isotherm, bio.seasonality,
                  bio.max, bio.min, bio.ann.range, bio.temp.wet,
                  bio.temp.dry, bio.temp.warm, bio.temp.cold,
                  bio.prec, bio.prec.wet, bio.prec.dry, bio.prec.seas,
                  bio.prec.wet.q, bio.prec.dry.q, bio.prec.warm.q, bio.prec.cold.q)
worldclim.ssa = raster::crop(worldclim, extent(gadm.36))


gadm.worldclim = gadm.worldclim.popweight =
  matrix(NA, nrow = nrow(gadm.36), ncol = dim(worldclim)[3])
for(ind in 1:nrow(gadm.36)) {
  ctry.shps.tmp = gadm.36[ind,]
  worldclim.crop = try(crop(worldclim.ssa, extent(ctry.shps.tmp) + 1))
  worldpop.crop = try(crop(worldpop.ssa, extent(worldclim.crop) + 2))
  
  ## Get worldpop points inside raster
  worldpop.resample = resample(worldpop.crop, worldclim.crop)
  worldclim.rasterize = rasterize(ctry.shps.tmp, worldclim.crop, mask = TRUE)
  worldpop.rasterize = rasterize(ctry.shps.tmp, worldpop.resample, mask = TRUE)
  ext.worldclim = getValues(worldclim.rasterize)
  ext.worldpop = getValues(worldpop.rasterize)
  if(all(is.na(ext.worldclim)) | all(is.na(ext.worldpop))){
    ext.worldclim = raster::extract(worldclim.crop, ctry.shps.tmp)[[1]]
    ext.worldpop = raster::extract(worldpop.resample, ctry.shps.tmp)[[1]]
  }
  
  gadm.worldclim[ind,] = colMeans(ext.worldclim, na.rm = T)
  
  
  ext.worldclim[which(is.na(ext.worldpop)),] = NA
  ext.worldpop[!complete.cases(ext.worldclim)] = NA ## Make sure these match for standardization
  ext.worldpop = ext.worldpop / sum(ext.worldpop, na.rm = T) ## Standardize by district
  gadm.worldclim.popweight[ind,] = colMeans(ext.worldclim * ext.worldpop, na.rm = T)
  
  
  print(round(ind / nrow(gadm.36) * 100, digits = 2))
}
gadm.worldclim = as.data.frame(gadm.worldclim)
names(gadm.worldclim) = names(worldclim)
gadm.worldclim.popweight = as.data.frame(gadm.worldclim.popweight)
names(gadm.worldclim.popweight) = paste0(names(worldclim),"_popweighted")
gadm.36@data = cbind(gadm.36@data, gadm.worldclim, gadm.worldclim.popweight)


## Clean up for memory
rm(gadm.worldclim, worldclim.ssa, worldclim)



################################################################################
################################################################################
## Add Global human footprint
################################################################################
################################################################################

hfp.raster = new("GDALReadOnlyDataset",
                 paste0("../Data/Spatial Data/hfp-africa-geo-grid/hfp_Africa_grid/",
                                               "hfp_africa/hdr.adf"))
hfp.raster = asSGDF_GROD(hfp.raster)
hfp.raster = raster(hfp.raster)
hfp.raster = projectRaster(hfp.raster, crs = crs(gadm.36))


crs(hfp.raster) ## Matches
hfp.ssa = raster::crop(hfp.raster, extent(gadm.36))
plot(hfp.ssa)

gadm.hfp = gadm.hfp.popweight = rep(NA, nrow(gadm.36))
for(ind in 1:nrow(gadm.36)) {
  ctry.shps.tmp = gadm.36[ind,]
  hfp.crop = try(crop(hfp.ssa, extent(ctry.shps.tmp) + 1))
  worldpop.crop = try(crop(worldpop.ssa, extent(hfp.crop) + 2))
  
  ## Get worldpop points inside raster
  worldpop.resample = resample(worldpop.crop, hfp.crop)
  
  if(class(hfp.crop) == "try-error"){
    next
  }
  hfp.rasterize = rasterize(ctry.shps.tmp, hfp.crop, mask = TRUE)
  worldpop.rasterize = rasterize(ctry.shps.tmp, worldpop.resample, mask = TRUE)
  ext.hfp = getValues(hfp.rasterize)
  ext.worldpop = getValues(worldpop.rasterize)
  # ext.worldpop[is.na(ext.worldpop)] = 0
  if(all(is.na(ext.hfp) | all(is.na(ext.worldpop)))){
    ext.hfp = raster::extract(hfp.crop, ctry.shps.tmp)[[1]]
    ext.worldpop = raster::extract(worldpop.resample, ctry.shps.tmp)[[1]]
  }
  gadm.hfp[ind] = mean(ext.hfp, na.rm = T)
  
  ext.hfp[which(is.na(ext.worldpop))] = NA
  ext.worldpop[which(is.na(ext.hfp))] = NA ## Make sure these match for standardization
  ext.worldpop = ext.worldpop / sum(ext.worldpop, na.rm = T) ## Standardize by district
  gadm.hfp.popweight[ind] = mean(ext.hfp * ext.worldpop, na.rm = T)
  
  print(round(ind / nrow(gadm.36) * 100, digits = 2))
}
gadm.hfp = as.data.frame(gadm.hfp)
names(gadm.hfp) = "hfp"
gadm.hfp.popweight = as.data.frame(gadm.hfp.popweight)
names(gadm.hfp.popweight) = "hfp_popweighted"
gadm.36@data = cbind(gadm.36@data, gadm.hfp, gadm.hfp.popweight)





################################################################################
################################################################################
## Add growing season length
################################################################################
################################################################################

## Don't know the initial projection (CRS)
gsl.raster = new("GDALReadOnlyDataset",paste0("../Data/Spatial Data/global_length_growing_period/",
                                "lgp_pl7/hdr.adf"))
gsl.raster = asSGDF_GROD(gsl.raster)
gsl.raster = raster(gsl.raster)
## Appears to be longlat
crs(gsl.raster) = crs(gadm.36)


crs(gsl.raster) ## Matches
gsl.ssa = raster::crop(gsl.raster, extent(gadm.36) + c(0.1))

gadm.gsl = gadm.gsl.popweight = rep(NA, nrow(gadm.36))
for(ind in 1:nrow(gadm.36)) {
  ctry.shps.tmp = gadm.36[ind,]
  gsl.crop = try(crop(gsl.ssa, extent(ctry.shps.tmp) + 1))
  worldpop.crop = try(crop(worldpop.ssa, extent(gsl.crop) + 2))
  
  ## Get worldpop points inside raster
  worldpop.resample = resample(worldpop.crop, gsl.crop)
  
  if(class(gsl.crop) == "try-error"){
    next
  }
  gsl.rasterize = rasterize(ctry.shps.tmp, gsl.crop, mask = TRUE)
  worldpop.rasterize = rasterize(ctry.shps.tmp, worldpop.resample, mask = TRUE)
  ext.gsl = getValues(gsl.rasterize)
  ext.worldpop = getValues(worldpop.rasterize)
  if(all(is.na(ext.gsl) | all(is.na(ext.worldpop)))){
    ext.gsl = raster::extract(gsl.crop, ctry.shps.tmp)[[1]]
    ext.worldpop = raster::extract(worldpop.resample, ctry.shps.tmp)[[1]]
  }

  gadm.gsl[ind] = mean(ext.gsl, na.rm = T)
  
  ext.gsl[which(is.na(ext.worldpop))] = NA
  ext.worldpop[which(is.na(ext.gsl))] = NA ## Make sure these match for standardization
  ext.worldpop = ext.worldpop / sum(ext.worldpop, na.rm = T) ## Standardize by district
  gadm.gsl.popweight[ind] = mean(ext.gsl * ext.worldpop, na.rm = T)
  
  print(round(ind / nrow(gadm.36) * 100, digits = 2))
}
gadm.gsl = as.data.frame(gadm.gsl)
names(gadm.gsl) = "gsl"
gadm.gsl.popweight = as.data.frame(gadm.gsl.popweight)
names(gadm.gsl.popweight) = "gsl_popweighted"
gadm.36@data = cbind(gadm.36@data, gadm.gsl, gadm.gsl.popweight)






################################################################################
################################################################################
## Add Travel times
################################################################################
################################################################################

travel.tif = raster(paste0("../Data/Spatial Data/",
                                 "2015_accessibility_to_cities_v1.0/",
                                 "2015_accessibility_to_cities_v1.0.tif"))

crs(travel.tif)
crs(gadm.36)

travel.ssa = raster::crop(travel.tif, extent(gadm.36))

gadm.travel = gadm.travel.popweight = rep(NA, nrow(gadm.36))
for(ind in 1:nrow(gadm.36)) {
  ctry.shps.tmp = gadm.36[ind,]
  travel.crop = try(crop(travel.ssa, extent(ctry.shps.tmp) + 1))
  worldpop.crop = try(crop(worldpop.ssa, extent(travel.crop) + 2))
  
  ## Get worldpop points inside raster
  worldpop.resample = resample(worldpop.crop, travel.crop)
  
  if(class(travel.crop) == "try-error"){
    next
  }
  travel.rasterize = rasterize(ctry.shps.tmp, travel.crop, mask = TRUE)
  worldpop.rasterize = rasterize(ctry.shps.tmp, worldpop.resample, mask = TRUE)
  ext.travel = getValues(travel.rasterize)
  ext.worldpop = getValues(worldpop.rasterize)
  if(all(is.na(ext.travel) | all(is.na(ext.worldpop)))){
    ext.travel = raster::extract(travel.crop, ctry.shps.tmp)[[1]]
    ext.worldpop = raster::extract(worldpop.resample, ctry.shps.tmp)[[1]]
  }
  gadm.travel[ind] = mean(ext.travel, na.rm = T)
  
  ext.travel[which(is.na(ext.worldpop))] = NA
  ext.worldpop[which(is.na(ext.travel))] = NA ## Make sure these match for standardization
  ext.worldpop = ext.worldpop / sum(ext.worldpop, na.rm = T) ## Standardize by district
  gadm.travel.popweight[ind] = mean(ext.travel * ext.worldpop, na.rm = T)
  
  print(round(ind / nrow(gadm.36) * 100, digits = 2))
}
gadm.travel = as.data.frame(gadm.travel)
names(gadm.travel) = "travel_to_50k"
gadm.travel.popweight = as.data.frame(gadm.travel.popweight)
names(gadm.travel.popweight) = "travel_to_50k_popweighted"
gadm.36@data = cbind(gadm.36@data, gadm.travel, gadm.travel.popweight)





################################################################################
################################################################################
## Add Infant mortality
################################################################################
################################################################################

infant.mort.tif = raster(paste0("../Data/Spatial Data/",
                           "infant_mortality/",
                           "infant_mortality_v2.tif"))

crs(infant.mort.tif)
crs(gadm.36)

infant.mort.ssa = raster::crop(infant.mort.tif, extent(gadm.36))

gadm.infant.mort = gadm.infant.mort.popweight = rep(NA, nrow(gadm.36))
for(ind in 1:nrow(gadm.36)) {
  ctry.shps.tmp = gadm.36[ind,]
  infant.mort.crop = try(crop(infant.mort.ssa, extent(ctry.shps.tmp) + 1))
  worldpop.crop = try(crop(worldpop.ssa, extent(infant.mort.crop) + 2))
  
  ## Get worldpop points inside raster
  worldpop.resample = resample(worldpop.crop, infant.mort.crop)
  
  if(class(infant.mort.crop) == "try-error"){
    next
  }
  infant.mort.rasterize = rasterize(ctry.shps.tmp, infant.mort.crop, mask = TRUE)
  worldpop.rasterize = rasterize(ctry.shps.tmp, worldpop.resample, mask = TRUE)
  ext.infant.mort = getValues(infant.mort.rasterize)
  ext.infant.mort[ext.infant.mort == -7777] = NA
  ext.infant.mort[ext.infant.mort == -9999] = NA
  ext.worldpop = getValues(worldpop.rasterize)
  if(all(is.na(ext.infant.mort) | all(is.na(ext.worldpop)))){
    ext.infant.mort = raster::extract(infant.mort.crop, ctry.shps.tmp)[[1]]
    ext.worldpop = raster::extract(worldpop.resample, ctry.shps.tmp)[[1]]
  }
  ext.infant.mort[ext.infant.mort == -7777] = NA
  ext.infant.mort[ext.infant.mort == -9999] = NA
  gadm.infant.mort[ind] = mean(ext.infant.mort, na.rm = T)
  
  ext.infant.mort[which(is.na(ext.worldpop))] = NA
  ext.worldpop[which(is.na(ext.infant.mort))] = NA ## Make sure these match for standardization
  ext.worldpop = ext.worldpop / sum(ext.worldpop, na.rm = T) ## Standardize by district
  gadm.infant.mort.popweight[ind] = mean(ext.infant.mort * ext.worldpop, na.rm = T)
  
  print(round(ind / nrow(gadm.36) * 100, digits = 2))
}
gadm.infant.mort = as.data.frame(gadm.infant.mort)
names(gadm.infant.mort) = "infant_mort"
gadm.infant.mort.popweight = as.data.frame(gadm.infant.mort.popweight)
names(gadm.infant.mort.popweight) = "infant_mort_popweighted"
gadm.36@data = cbind(gadm.36@data, gadm.infant.mort, gadm.infant.mort.popweight)






################################################################################
################################################################################
## Add Wealth and maternal education
################################################################################
################################################################################

## These are only available in a few countries



################################################################################
################################################################################
## Add Walking to healthcare
################################################################################
################################################################################



walking.raster = raster(paste0("../Data/Spatial Data/",
                                "2020_walking_only_travel_time_to_healthcare/",
                                "2020_walking_only_travel_time_to_healthcare.geotiff"))

crs(walking.raster)
crs(gadm.36)
walking.ssa = raster::crop(walking.raster, extent(gadm.36))

gadm.walking = gadm.walking.popweight = rep(NA, nrow(gadm.36))
for(ind in 1:nrow(gadm.36)) {
  ctry.shps.tmp = gadm.36[ind,]
  walking.crop = try(crop(walking.ssa, extent(ctry.shps.tmp) + 1))
  worldpop.crop = try(crop(worldpop.ssa, extent(walking.crop) + 2))
  
  ## Get worldpop points inside raster
  worldpop.resample = resample(worldpop.crop, walking.crop)
  
  if(class(walking.crop) == "try-error"){
    next
  }
  walking.rasterize = rasterize(ctry.shps.tmp, walking.crop, mask = TRUE)
  worldpop.rasterize = rasterize(ctry.shps.tmp, worldpop.resample, mask = TRUE)
  ext.walking = getValues(walking.rasterize)
  ext.worldpop = getValues(worldpop.rasterize)
  if(all(is.na(ext.walking) | all(is.na(ext.worldpop)))){
    ext.walking = raster::extract(walking.crop, ctry.shps.tmp)[[1]]
    ext.worldpop = raster::extract(worldpop.resample, ctry.shps.tmp)[[1]]
  }
  gadm.walking[ind] = mean(ext.walking, na.rm = T)
  
  ext.walking[which(is.na(ext.worldpop))] = NA
  ext.worldpop[which(is.na(ext.walking))] = NA ## Make sure these match for standardization
  ext.worldpop = ext.worldpop / sum(ext.worldpop, na.rm = T) ## Standardize by district
  gadm.walking.popweight[ind] = mean(ext.walking * ext.worldpop, na.rm = T)
  
  print(round(ind / nrow(gadm.36) * 100, digits = 2))
}
gadm.walking = as.data.frame(gadm.walking)
names(gadm.walking) = "walking_healthcare"
gadm.walking.popweight = as.data.frame(gadm.walking.popweight)
names(gadm.walking.popweight) = "walking_healthcare_popweighted"
gadm.36@data = cbind(gadm.36@data, gadm.walking, gadm.walking.popweight)





################################################################################
################################################################################
## Add duffy negative
################################################################################
################################################################################



duffy.raster = raster(paste0("../Data/Spatial Data/",
                               "2011_Duffy_Negative/",
                               "2011_duffy_negative.geotiff"))

crs(duffy.raster)
crs(gadm.36)
duffy.ssa = raster::crop(duffy.raster, extent(gadm.36))

gadm.duffy = gadm.duffy.popweight = rep(NA, nrow(gadm.36))
for(ind in 1:nrow(gadm.36)) {
  ctry.shps.tmp = gadm.36[ind,]
  duffy.crop = try(crop(duffy.ssa, extent(ctry.shps.tmp) + 1))
  worldpop.crop = try(crop(worldpop.ssa, extent(duffy.crop) + 2))
  
  ## Get worldpop points inside raster
  worldpop.resample = resample(worldpop.crop, duffy.crop)
  
  if(class(duffy.crop) == "try-error"){
    next
  }
  duffy.rasterize = rasterize(ctry.shps.tmp, duffy.crop, mask = TRUE)
  worldpop.rasterize = rasterize(ctry.shps.tmp, worldpop.resample, mask = TRUE)
  ext.duffy = getValues(duffy.rasterize)
  ext.worldpop = getValues(worldpop.rasterize)
  if(all(is.na(ext.duffy) | all(is.na(ext.worldpop)))){
    ext.duffy = raster::extract(duffy.crop, ctry.shps.tmp)[[1]]
    ext.worldpop = raster::extract(worldpop.resample, ctry.shps.tmp)[[1]]
  }
  
  gadm.duffy[ind] = mean(ext.duffy, na.rm = T)
  
  ext.duffy[which(is.na(ext.worldpop))] = NA
  ext.worldpop[which(is.na(ext.duffy))] = NA ## Make sure these match for standardization
  ext.worldpop = ext.worldpop / sum(ext.worldpop, na.rm = T) ## Standardize by district
  gadm.duffy.popweight[ind] = mean(ext.duffy * ext.worldpop, na.rm = T)
  
  print(round(ind / nrow(gadm.36) * 100, digits = 2))
}
gadm.duffy = as.data.frame(gadm.duffy)
names(gadm.duffy) = "duffy_negative"
gadm.duffy.popweight = as.data.frame(gadm.duffy.popweight)
names(gadm.duffy.popweight) = "duffy_negative_popweighted"
gadm.36@data = cbind(gadm.36@data, gadm.duffy, gadm.duffy.popweight)








################################################################################
################################################################################
## Add sickle
################################################################################
################################################################################



sickle.raster = raster(paste0("../Data/Spatial Data/",
                             "2013_Sickle/",
                             "2013_sickle.geotiff"))

crs(sickle.raster)
crs(gadm.36)
sickle.ssa = raster::crop(sickle.raster, extent(gadm.36))

gadm.sickle = gadm.sickle.popweight = rep(NA, nrow(gadm.36))
for(ind in 1:nrow(gadm.36)) {
  ctry.shps.tmp = gadm.36[ind,]
  sickle.crop = try(crop(sickle.ssa, extent(ctry.shps.tmp) + 1))
  worldpop.crop = try(crop(worldpop.ssa, extent(sickle.crop) + 2))
  
  ## Get worldpop points inside raster
  worldpop.resample = resample(worldpop.crop, sickle.crop)
  
  if(class(sickle.crop) == "try-error"){
    next
  }
  sickle.rasterize = rasterize(ctry.shps.tmp, sickle.crop, mask = TRUE)
  worldpop.rasterize = rasterize(ctry.shps.tmp, worldpop.resample, mask = TRUE)
  ext.sickle = getValues(sickle.rasterize)
  ext.worldpop = getValues(worldpop.rasterize)
  if(all(is.na(ext.sickle) | all(is.na(ext.worldpop)))){
    ext.sickle = raster::extract(sickle.crop, ctry.shps.tmp)[[1]]
    ext.worldpop = raster::extract(worldpop.resample, ctry.shps.tmp)[[1]]
  }
  gadm.sickle[ind] = mean(ext.sickle, na.rm = T)
  
  ext.sickle[which(is.na(ext.worldpop))] = NA
  ext.worldpop[which(is.na(ext.sickle))] = NA ## Make sure these match for standardization
  ext.worldpop = ext.worldpop / sum(ext.worldpop, na.rm = T) ## Standardize by district
  gadm.sickle.popweight[ind] = mean(ext.sickle * ext.worldpop, na.rm = T)
  
  print(round(ind / nrow(gadm.36) * 100, digits = 2))
}
gadm.sickle = as.data.frame(gadm.sickle)
names(gadm.sickle) = "sickle_frequency"
gadm.sickle.popweight = as.data.frame(gadm.sickle.popweight)
names(gadm.sickle.popweight) = "sickle_frequency_popweighted"
gadm.36@data = cbind(gadm.36@data, gadm.sickle, gadm.sickle.popweight)










################################################################################
################################################################################
## Add g6pdd
################################################################################
################################################################################



g6pdd.raster = raster(paste0("../Data/Spatial Data/",
                              "2012_G6PDd/",
                              "2012_G6PDd.geotiff"))

crs(g6pdd.raster)
crs(gadm.36)
g6pdd.ssa = raster::crop(g6pdd.raster, extent(gadm.36))

gadm.g6pdd = gadm.g6pdd.popweight = rep(NA, nrow(gadm.36))
for(ind in 1:nrow(gadm.36)) {
  ctry.shps.tmp = gadm.36[ind,]
  g6pdd.crop = try(crop(g6pdd.ssa, extent(ctry.shps.tmp) + 1))
  worldpop.crop = try(crop(worldpop.ssa, extent(g6pdd.crop) + 2))
  
  ## Get worldpop points inside raster
  worldpop.resample = resample(worldpop.crop, g6pdd.crop)
  
  if(class(g6pdd.crop) == "try-error"){
    next
  }
  g6pdd.rasterize = rasterize(ctry.shps.tmp, g6pdd.crop, mask = TRUE)
  worldpop.rasterize = rasterize(ctry.shps.tmp, worldpop.resample, mask = TRUE)
  ext.g6pdd = getValues(g6pdd.rasterize)
  ext.worldpop = getValues(worldpop.rasterize)
  if(all(is.na(ext.g6pdd) | all(is.na(ext.worldpop)))){
    ext.g6pdd = raster::extract(g6pdd.crop, ctry.shps.tmp)[[1]]
    ext.worldpop = raster::extract(worldpop.resample, ctry.shps.tmp)[[1]]
  }
  gadm.g6pdd[ind] = mean(ext.g6pdd, na.rm = T)
  
  ext.g6pdd[which(is.na(ext.worldpop))] = NA
  ext.worldpop[which(is.na(ext.g6pdd))] = NA ## Make sure these match for standardization
  ext.worldpop = ext.worldpop / sum(ext.worldpop, na.rm = T) ## Standardize by district
  gadm.g6pdd.popweight[ind] = mean(ext.g6pdd * ext.worldpop, na.rm = T)
  
  print(round(ind / nrow(gadm.36) * 100, digits = 2))
}
gadm.g6pdd = as.data.frame(gadm.g6pdd)
names(gadm.g6pdd) = "g6pdd_frequency"
gadm.g6pdd.popweight = as.data.frame(gadm.g6pdd.popweight)
names(gadm.g6pdd.popweight) = "g6pdd_frequency_popweighted"
gadm.36@data = cbind(gadm.36@data, gadm.g6pdd, gadm.g6pdd.popweight)





################################################################################
################################################################################
## Add GDP
################################################################################
################################################################################

gdp.raster = raster::brick(paste0("../Data/Spatial Data/",
                                  "GDP/GDP_per_capita_PPP_1990_2015_v2.nc"))


crs(gdp.raster)
crs(gadm.36)
gdp.ssa = raster::crop(gdp.raster, extent(gadm.36))

gadm.gdp = gadm.gdp.popweight = matrix(NA, nrow(gadm.36), ncol = dim(gdp.ssa)[3])
for(ind in 1:nrow(gadm.36)) {
  ctry.shps.tmp = gadm.36[ind,]
  gdp.crop = try(crop(gdp.ssa, extent(ctry.shps.tmp) + 1))

  if(class(gdp.crop) == "try-error"){
    next
  }
  gdp.rasterize = rasterize(ctry.shps.tmp, gdp.crop, mask = TRUE)
  ext.gdp = raster::extract(gdp.crop, ctry.shps.tmp, weights = TRUE,
                            normalizeWeights = TRUE)[[1]]

  if(nrow(ext.gdp) == 0){
    print(c(ind,"Error"))
  }
  if(nrow(ext.gdp) == 1){
    ## Remove NA and renormalize
    ext.gdp = ext.gdp[complete.cases(ext.gdp),]
    ext.gdp[27] = ext.gdp[27] / sum(ext.gdp[27], na.rm = T)
    gadm.gdp[ind,] = mean(ext.gdp[1:26] * ext.gdp[27], na.rm = T)
  }else{
    ## Remove NA and renormalize
    ext.gdp = ext.gdp[complete.cases(ext.gdp),]
    ext.gdp[,27] = ext.gdp[,27] / sum(ext.gdp[,27], na.rm = T)
    gadm.gdp[ind,] = colMeans(ext.gdp[,1:26] * ext.gdp[,27], na.rm = T)
  }
  
  print(round(ind / nrow(gadm.36) * 100, digits = 2))
}
gadm.gdp = as.data.frame(gadm.gdp)
names(gadm.gdp) = paste0("GDP_", names(gdp.raster))
gadm.36@data = cbind(gadm.36@data, gadm.gdp)









####################################################################################################
####################################################################################################
## Save Data
####################################################################################################
####################################################################################################

saveRDS(gadm.36, "../Data/gadm36_adm1_with_spatial.rds")


