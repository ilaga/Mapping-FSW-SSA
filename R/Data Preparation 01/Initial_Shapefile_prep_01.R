library(raster)

rm(list=ls())
## Remove Uganda, Ghana, and Nigeria
## They needs different shapefiles
ssa.iso = readxl::read_excel("../Data/Africa_ISO.xlsx")
ssa.iso = ssa.iso[,1:2]
ssa.iso = subset(ssa.iso, Included == "TRUE")
ssa.iso = ssa.iso[-which(ssa.iso$ISO %in% c("UGA", "GHA", "NGA")),]

## Create an administrative 1 level
district.shape.all = shapefile("../Data/Shapefiles/gadm36_1.shp", encoding = "UTF-8", use_iconv = TRUE)
district.shape.all.adm2 = shapefile("../Data/Shapefiles/gadm36_2.shp", encoding = "UTF-8", use_iconv = TRUE)
district.shape.ssa = subset(district.shape.all, GID_0 %in% ssa.iso$ISO)
nigeria.shape = subset(district.shape.all.adm2, GID_0 == "NGA")
uganda.shape = shapefile("../Data/Shapefiles/uganda_20200824_shp/uga_admbnda_adm2_ubos_20200824.shp")
ghana.shape = shapefile("../Data/Shapefiles/ghana_216/Districts/Map_of_Districts_216.shp")
ghana.shape = spTransform(ghana.shape, crs(district.shape.ssa))
## Nigeria has a few districts with the same name
dup.names = names(which(table(nigeria.shape$NAME_2) > 1))
dup.ind = which(nigeria.shape$NAME_2 %in% dup.names)
nigeria.shape$NAME_2[dup.ind] = paste0(nigeria.shape$NAME_2[dup.ind], "-",
                                       nigeria.shape$NAME_1[dup.ind])

## Merge data
district.shape.ssa@data = district.shape.ssa@data[,c("GID_0", "NAME_0", "NAME_1", "TYPE_1", "ENGTYPE_1", "VARNAME_1")]
names(district.shape.ssa@data)[1] = "ISO"
nigeria.shape@data = nigeria.shape@data[,c("GID_0", "NAME_0", "NAME_2", "TYPE_2", "ENGTYPE_2", "VARNAME_2")]
names(nigeria.shape@data) = names(district.shape.ssa@data)
uganda.shape@data = cbind("UGA", uganda.shape@data[,c("ADM0_EN", "ADM2_EN")])
names(uganda.shape@data) = c("ISO", "NAME_0", "NAME_1")
ghana.shape@data = data.frame(ISO = "GHA", NAME_0 = "Ghana", NAME_1 = ghana.shape@data[,c("LABEL")])
ssa.district = bind(district.shape.ssa, nigeria.shape, uganda.shape, ghana.shape)

## Save
saveRDS(ssa.district, file = "../Data/Shapefiles/gadm36_adm1_africa.rds")

