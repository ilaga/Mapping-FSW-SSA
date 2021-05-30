
library(raster)
library(ggmap)
register_google(key = "")


rm(list=ls())
# Read in data ------------------------------------------------------------

pse.dat = read.csv("../Data/PSE Spreadsheets/SSA_FSW_PSE_00.csv")
gadm.36.adm1 = readRDS("../Data/Shapefiles/gadm36_adm1_africa.rds")


# Get ISO first -----------------------------------------------------------

pse.dat$ISO = NA
iso.match = match(pse.dat$country, gadm.36.adm1$NAME_0)
pse.dat$ISO = gadm.36.adm1$ISO[iso.match]
## Check missing entries
subset(pse.dat, is.na(ISO))[,c("country", "ISO")]
## Missing Congo (Because should be Republic of Congo, ISO "COG")
## Missing Cote D'Ivoire (Because missing accents, ISO "CIV")
## Missing ethiopia (Lowercase, ISO "ETH")

pse.dat$ISO[pse.dat$country == "Congo"] = "COG"
pse.dat$ISO[pse.dat$country == "Cote D'Ivoire"] = "CIV"
pse.dat$ISO[pse.dat$country == "ethiopia"] = "ETH"
subset(pse.dat, is.na(ISO))[,c("country", "ISO")]



# Apply geocode -----------------------------------------------------------
## Get both adm1 and adm2

pse.tmp = pse.dat
pse.tmp$region_area = apply(pse.tmp[,c("fsw_sizeestimate_region","area")],
                            1, paste, collapse=" ")
pse.tmp$Name_no_area = apply(pse.tmp[,c("fsw_sizeestimate_region","country")],
                             1, paste, collapse = ", ")

geocode_more <- geocode(pse.tmp$Name_no_area, output = "more")

saveRDS(geocode_more, file = "../Data/geocode_more_covmatch.rds")
## Geocode_more contains only the first hit.
## Need to validate by hand later.

## Place geocode_more inside gadm.36.adm1
geocode_sp = geocode_more
which.not.na = which(!is.na(geocode_more$lon))
geocode_sp = geocode_sp[which.not.na,]
coordinates(geocode_sp) = ~ lon + lat

crs(geocode_sp) = crs(gadm.36.adm1)
adm1_data = over(geocode_sp, gadm.36.adm1)
pse.dat$adm1_geocode[which.not.na] = adm1_data$NAME_1
pse.dat$iso_geocode[which.not.na] = adm1_data$ISO

write.csv(pse.dat, file = "../Data/PSE Spreadsheets/SSA_FSW_PSE_01.csv", row.names = F)

## Corrections and validations are done by hand
