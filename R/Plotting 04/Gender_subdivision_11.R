

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
library(ggplot2)

rm(list = ls())
## Load shapefiles
gadm.36.adm1 = readRDS("../Data/Shapefiles/gadm36_adm1_africa.rds")

gadm.36.adm1$f15 = NA
gadm.36.adm1$f20 = NA
gadm.36.adm1$f25 = NA
gadm.36.adm1$f30 = NA
gadm.36.adm1$f35 = NA
gadm.36.adm1$f40 = NA
gadm.36.adm1$f45 = NA
gadm.36.adm1$f15to49 = NA

## Do adm1 and adm2 first
for (iso.ind in unique(c(gadm.36.adm1$ISO))) {
  ## Load general population and female population worldPop for ISO
  f15.name = paste0("../Data/WorldPop_data/", iso.ind, "_f15.tif")
  if (!file.exists(f15.name)) {
    f15.link = paste0(
      "https://data.worldpop.org/GIS/AgeSex_structures/Global_2000_2020/2015/",
      iso.ind,
      "/",
      tolower(iso.ind),
      "_f_15_2015.tif"
    )
    curl::curl_download(f15.link, f15.name)
  }
  f15.worldPop = raster(f15.name)
  
  f20.name = paste0("../Data/WorldPop_data/", iso.ind, "_f20.tif")
  if (!file.exists(f20.name)) {
    f20.link = paste0(
      "https://data.worldpop.org/GIS/AgeSex_structures/Global_2000_2020/2015/",
      iso.ind,
      "/",
      tolower(iso.ind),
      "_f_20_2015.tif"
    )
    curl::curl_download(f20.link, f20.name)
  }
  f20.worldPop = raster(f20.name)
  
  f25.name = paste0("../Data/WorldPop_data/", iso.ind, "_f25.tif")
  if (!file.exists(f25.name)) {
    f25.link = paste0(
      "https://data.worldpop.org/GIS/AgeSex_structures/Global_2000_2020/2015/",
      iso.ind,
      "/",
      tolower(iso.ind),
      "_f_25_2015.tif"
    )
    curl::curl_download(f25.link, f25.name)
  }
  f25.worldPop = raster(f25.name)
  
  f30.name = paste0("../Data/WorldPop_data/", iso.ind, "_f30.tif")
  if (!file.exists(f30.name)) {
    f30.link = paste0(
      "https://data.worldpop.org/GIS/AgeSex_structures/Global_2000_2020/2015/",
      iso.ind,
      "/",
      tolower(iso.ind),
      "_f_30_2015.tif"
    )
    curl::curl_download(f30.link, f30.name)
  }
  f30.worldPop = raster(f30.name)
  
  f35.name = paste0("../Data/WorldPop_data/", iso.ind, "_f35.tif")
  if (!file.exists(f35.name)) {
    f35.link = paste0(
      "https://data.worldpop.org/GIS/AgeSex_structures/Global_2000_2020/2015/",
      iso.ind,
      "/",
      tolower(iso.ind),
      "_f_35_2015.tif"
    )
    curl::curl_download(f35.link, f35.name)
  }
  f35.worldPop = raster(f35.name)
  
  f40.name = paste0("../Data/WorldPop_data/", iso.ind, "_f40.tif")
  if (!file.exists(f40.name)) {
    f40.link = paste0(
      "https://data.worldpop.org/GIS/AgeSex_structures/Global_2000_2020/2015/",
      iso.ind,
      "/",
      tolower(iso.ind),
      "_f_40_2015.tif"
    )
    curl::curl_download(f40.link, f40.name)
  }
  f40.worldPop = raster(f40.name)
  
  f45.name = paste0("../Data/WorldPop_data/", iso.ind, "_f45.tif")
  if (!file.exists(f45.name)) {
    f45.link = paste0(
      "https://data.worldpop.org/GIS/AgeSex_structures/Global_2000_2020/2015/",
      iso.ind,
      "/",
      tolower(iso.ind),
      "_f_45_2015.tif"
    )
    curl::curl_download(f45.link, f45.name)
  }
  f45.worldPop = raster(f45.name)
  
  
  ## For Adm1
  ## Subset country_shapes inside ISO
  gadm.36.adm1.sub = subset(gadm.36.adm1, ISO == iso.ind)
  gadm.36.adm1.ind = which(gadm.36.adm1$ISO == iso.ind)
  
  if (length(gadm.36.adm1.ind) > 0) {
    ## Get worldPop inside the districts
    f15.pop.adm1 = f20.pop.adm1 = f25.pop.adm1 = f30.pop.adm1 =
      f35.pop.adm1 = f40.pop.adm1 = f45.pop.adm1 = rep(NA, nrow(gadm.36.adm1.sub))
    for (j in 1:nrow(gadm.36.adm1.sub)) {
      adm1.sub = gadm.36.adm1.sub[j, ]
      image.crop = try(crop(f15.worldPop, extent(adm1.sub)))
      f15.rasterize = rasterize(adm1.sub, image.crop, mask = TRUE)
      f15.pop.adm1[j] = sum(getValues(f15.rasterize), na.rm = T)
      
      image.crop = try(crop(f20.worldPop, extent(adm1.sub)))
      f20.rasterize = rasterize(adm1.sub, image.crop, mask = TRUE)
      f20.pop.adm1[j] = sum(getValues(f20.rasterize), na.rm = T)
      
      image.crop = try(crop(f25.worldPop, extent(adm1.sub)))
      f25.rasterize = rasterize(adm1.sub, image.crop, mask = TRUE)
      f25.pop.adm1[j] = sum(getValues(f25.rasterize), na.rm = T)
      
      image.crop = try(crop(f30.worldPop, extent(adm1.sub)))
      f30.rasterize = rasterize(adm1.sub, image.crop, mask = TRUE)
      f30.pop.adm1[j] = sum(getValues(f30.rasterize), na.rm = T)
      
      image.crop = try(crop(f35.worldPop, extent(adm1.sub)))
      f35.rasterize = rasterize(adm1.sub, image.crop, mask = TRUE)
      f35.pop.adm1[j] = sum(getValues(f35.rasterize), na.rm = T)
      
      image.crop = try(crop(f40.worldPop, extent(adm1.sub)))
      f40.rasterize = rasterize(adm1.sub, image.crop, mask = TRUE)
      f40.pop.adm1[j] = sum(getValues(f40.rasterize), na.rm = T)
      
      image.crop = try(crop(f45.worldPop, extent(adm1.sub)))
      f45.rasterize = rasterize(adm1.sub, image.crop, mask = TRUE)
      f45.pop.adm1[j] = sum(getValues(f45.rasterize), na.rm = T)
    }
    gadm.36.adm1$f15[gadm.36.adm1.ind] = f15.pop.adm1
    gadm.36.adm1$f20[gadm.36.adm1.ind] = f20.pop.adm1
    gadm.36.adm1$f25[gadm.36.adm1.ind] = f25.pop.adm1
    gadm.36.adm1$f30[gadm.36.adm1.ind] = f30.pop.adm1
    gadm.36.adm1$f35[gadm.36.adm1.ind] = f35.pop.adm1
    gadm.36.adm1$f40[gadm.36.adm1.ind] = f40.pop.adm1
    gadm.36.adm1$f45[gadm.36.adm1.ind] = f45.pop.adm1
  }
  
  
  print(iso.ind)
}

gadm.36.adm1$f15to49 = rowSums(gadm.36.adm1@data[, 7:13])

saveRDS(gadm.36.adm1, "../Data/WorldPop_data/worldpop_age_division.rds")


rm(list = ls())
load("../Data/Results/Final_Results.RData")
f15to49.dat = readRDS("../Data/WorldPop_data/worldpop_age_division.rds")
## Convert to ratios
for (i in 1:nrow(f15to49.dat@data)) {
  f15to49.dat@data[i, 7:13] = f15to49.dat@data[i, 7:13] / f15to49.dat@data[i, 14]
}

## Get residuals
resid.dat = data.frame(resid = resid(horseshoe.fit)[, 1])
resid.dat = cbind(resid.dat, matrix(NA, nrow = nrow(resid.dat), ncol = 8))

## Match residuals to proportions
for (i in 1:nrow(pse.obs)) {
  resid.dat[i, 2:8] = f15to49.dat@data[pse.obs$ID[i], 7:13]
  resid.dat[i, 9] = f15to49.dat@data[pse.obs$ID[i], 1]
  if (f15to49.dat@data[pse.obs$ID[i], 1] != pse.obs$ISO[i]) {
    print("Wrong country")
  }
}

names(resid.dat) = c("resid",
                     "f1519",
                     "f2024",
                     "f2529",
                     "f3034",
                     "f3539",
                     "f4044",
                     "f4549",
                     "ISO")

resid.dat.subset = subset(
  resid.dat,
  ISO %in% c(
    "COG",
    "ETH",
    "GHA",
    "KEN",
    "MDG",
    "MWI",
    "NGA",
    "TGO",
    "TZA",
    "UGA",
    "ZAF",
    "ZMB"
  )
)

resid.melt = reshape2::melt(resid.dat, id.vars = "resid")
resid.melt.noiso = subset(resid.melt, variable != "ISO")
resid.melt.noiso$value = as.numeric(resid.melt.noiso$value)

## Create plots

## F15-19
all.15.19 = ggplot(resid.dat, aes(x = f1519, y = resid)) +
  geom_point() +
  geom_smooth(lwd = 2) +
  xlab("Percent of population which are women aged 15-19") + ylab("Residual") + ggtitle("Age 15-19") +
  theme(
    axis.text.x = element_text(size = 20, face = "bold"),
    strip.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 22, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    legend.title = element_text(size = 22, face = "bold"),
    legend.text = element_text(size = 18),
    title = element_text(size = 25, face = "bold"),
    aspect.ratio = 1
  )

iso.15.19 = ggplot(resid.dat.subset, aes(x = f1519, y = resid)) +
  geom_point() +
  geom_smooth(lwd = 2) +
  facet_wrap( ~ ISO, scales = "free") +
  xlab("Percent of population which are women aged 15-19") + ylab("Residual") + ggtitle("Age 15-19") +
  theme(
    axis.text.x = element_text(size = 20, face = "bold"),
    strip.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 22, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    legend.title = element_text(size = 22, face = "bold"),
    legend.text = element_text(size = 18),
    title = element_text(size = 25, face = "bold"),
    aspect.ratio = 1
  )

## F20-24
all.20.24 = ggplot(resid.dat, aes(x = f2024, y = resid)) +
  geom_point() +
  geom_smooth(lwd = 2) +
  xlab("Percent of population which are women aged 20-24") + ylab("Residual") + ggtitle("Age 20-24") +
  theme(
    axis.text.x = element_text(size = 20, face = "bold"),
    strip.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 22, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    legend.title = element_text(size = 22, face = "bold"),
    legend.text = element_text(size = 18),
    title = element_text(size = 25, face = "bold"),
    aspect.ratio = 1
  )

iso.20.24 = ggplot(resid.dat.subset, aes(x = f2024, y = resid)) +
  geom_point() +
  geom_smooth(lwd = 2) +
  facet_wrap( ~ ISO, scales = "free") +
  xlab("Percent of population which are women aged 20-24") + ylab("Residual") + ggtitle("Age 20-24") +
  theme(
    axis.text.x = element_text(size = 20, face = "bold"),
    strip.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 22, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    legend.title = element_text(size = 22, face = "bold"),
    legend.text = element_text(size = 18),
    title = element_text(size = 25, face = "bold"),
    aspect.ratio = 1
  )

# F30-34
all.30.34 = ggplot(resid.dat, aes(x = f3034, y = resid)) +
  geom_point() +
  geom_smooth(lwd = 2) +
  xlab("Percent of population which are women aged 30-34") + ylab("Residual") + ggtitle("Age 30-34") +
  theme(
    axis.text.x = element_text(size = 20, face = "bold"),
    strip.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 22, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    legend.title = element_text(size = 22, face = "bold"),
    legend.text = element_text(size = 18),
    title = element_text(size = 25, face = "bold"),
    aspect.ratio = 1
  )

iso.30.34 = ggplot(resid.dat.subset, aes(x = f3034, y = resid)) +
  geom_point() +
  geom_smooth(lwd = 2) +
  facet_wrap( ~ ISO, scales = "free") +
  xlab("Percent of population which are women aged 30-34") + ylab("Residual") + ggtitle("Age 30-34") +
  theme(
    axis.text.x = element_text(size = 20, face = "bold"),
    strip.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 22, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    legend.title = element_text(size = 22, face = "bold"),
    legend.text = element_text(size = 18),
    title = element_text(size = 25, face = "bold"),
    aspect.ratio = 1
  )


## F45-49
all.45.49 = ggplot(resid.dat, aes(x = f4549, y = resid)) +
  geom_point() +
  geom_smooth(lwd = 2) +
  xlab("Percent of population which are women aged 45-49") + ylab("Residual") + ggtitle("Age 45-49") +
  theme(
    axis.text.x = element_text(size = 20, face = "bold"),
    strip.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 22, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    legend.title = element_text(size = 22, face = "bold"),
    legend.text = element_text(size = 18),
    title = element_text(size = 25, face = "bold"),
    aspect.ratio = 1
  )

iso.45.49 = ggplot(resid.dat.subset, aes(x = f4549, y = resid)) +
  geom_point() +
  geom_smooth(lwd = 2) +
  facet_wrap( ~ ISO, scales = "free") +
  xlab("Percent of population which are women aged 45-49") + ylab("Residual") + ggtitle("Age 45-49") +
  theme(
    axis.text.x = element_text(size = 20, face = "bold"),
    strip.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 22, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    legend.title = element_text(size = 22, face = "bold"),
    legend.text = element_text(size = 18),
    title = element_text(size = 25, face = "bold"),
    aspect.ratio = 1
  )



ggsave("../Figures/age_all_15_19.jpg", all.15.19, width = 10, height = 10)
ggsave("../Figures/age_iso_15_19.jpg", iso.15.19, width = 20, height = 20)
ggsave("../Figures/age_all_20_24.jpg", all.20.24, width = 10, height = 10)
ggsave("../Figures/age_iso_20_24.jpg", iso.20.24, width = 20, height = 20)
ggsave("../Figures/age_all_30_34.jpg", all.30.34, width = 10, height = 10)
ggsave("../Figures/age_iso_30_34.jpg", iso.30.34, width = 20, height = 20)
ggsave("../Figures/age_all_45_49.jpg", all.45.49, width = 10, height = 10)
ggsave("../Figures/age_iso_45_49.jpg", iso.45.49, width = 20, height = 20)

