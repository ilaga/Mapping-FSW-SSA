
library(raster)
library(dplyr)
library(readr)
library(ggplot2)

rm(list=ls())
# Read in data ------------------------------------------------------------

pse.dat = read.csv("../Data/PSE Spreadsheets/SSA_FSW_PSE_02.csv")
pse.dat$pse = as.numeric(pse.dat$pse)

# Fix method --------------------------------------------------------------

pse.dat$method = as.character(pse.dat$method)
pse.dat$method = sapply(pse.dat$method, trimws)
pse.dat = pse.dat %>%
  mutate(method_std = case_when(
    method == "Mapping" ~ "Mapping",
    is.na(method) ~ "NR",
    method == "" ~ "NR",
    method == "not found" ~ "NR",
    method == "geographic mapping" ~ "Mapping",
    method == "Hotspot mapping" ~ "Mapping",
    method == "census" ~ "Enumeration",
    method == "CRC" ~ "CRC",
    method == "NR" ~ "NR",
    method == "Enumeration" ~ "Enumeration",
    method == "Average_CRM_Enumeration" ~ "Multiple",
    method == "Averaged_methods" ~ "Multiple",
    method == "Citation" ~ "Misc",
    method == "Survey" ~ "Misc",
    method == "Wisdom of the crowds, capture recapture, mass event" ~ "Multiple",
    method == "Unique Object Multiplier" ~ "Multiplier",
    method == "Multiplier" ~ "Multiplier",
    method == "census and enumeration" ~ "Enumeration",
    method == "Consensus" ~ "Expert",
    method == "Delphi" ~ "Expert",
    method == "Service multiplier" ~ "Multiplier",
    method == "Census (Seaters)" ~ "Enumeration",
    method == "Census, mapping, key informant interviews" ~ "Multiple",
    method == "CRC (Roamers)" ~ "CRC",
    method == "Roamers (CRC)" ~ "CRC",
    method == "Roamers (CRM)" ~ "CRC",
    method == "Roamers (Delphi)" ~ "Expert",
    method == "Seaters (Census)" ~ "Enumeration",
    method == "Desk Review" ~ "Misc",
    method == "Mapping and Enumeration" ~ "Multiple",
    method == "Modified Delphi" ~ "Expert",
    method == "Multiplier method" ~ "Multiplier",
    method == "Summary" ~ "Misc",
    method == "multiple methods" ~ "Multiple",
    method == "Interviews" ~ "Expert",
    method == "modified Delphi" ~ "Expert",
    method == "SS-PSE*" ~ "Misc",
    method == "Multiple methods averaged" ~ "Multiple",
    method == "Unique object" ~ "Multiplier",
    method == "mapping" ~ "Mapping",
    method == "non-stat Interviews and Observation" ~ "Misc",
    method == "Adjusted mapping and enumeration" ~ "Multiple",
    method == "Literature review" ~ "Multiple",
    method == "Neighborhood mapping" ~ "Mapping",
    method == "Wisdom of the crowds" ~ "Expert",
    method == "citation" ~ "Misc",
    method == "Mapping/Enumeration+Literature Based+Delphi" ~ "Multiple",
    method == "Median" ~ "Misc",
    method == "stakeholder consensus" ~ "Expert",
    method == "Unique Object" ~ "Multiplier",
    method == "Unique object multiplier" ~ "Multiplier",
    method == "Best size estimate" ~ "Misc",
    method == "Literature estimate (5.0%)" ~ "Multiple",
    method == "Median estimate" ~ "Misc",
    method == "Multiple" ~ "Multiple",
    method == "Unique Event" ~ "Multiplier",
    method == "Unique event multiplier" ~ "Multiplier",
    method == "census enumeration or multiplier method" ~ "Multiple",
    method == "Multiple methods" ~ "Multiple",
    method == "multiple methods - unique object multiplier method, social event multiplier method, the wisdom of the masses method and capture recapture method" ~ "Multiple",
    method == "Unique object multiplier, social event multiplier, wisdom of the masses method" ~ "Multiple",
    method == "Capture-recapture" ~ "CRC",
    method == "Census" ~ "Enumeration",
    method == "census enumeration and multiplier methods" ~ "Multiple",
    method == "census enumeration or multiplier methods" ~ "Multiple",
    method == "count" ~ "Enumeration",
    method == "geographic mapping, wisdom of the crowd" ~ "Mapping",
    method == "Interviews and secondary sources" ~ "Expert",
    method == "Mapping (citation)" ~ "Mapping",
    method == "mapping and census" ~ "Multiple",
    method == "Multiple methods -" ~ "Multiple",
    method == "Network scale-up method using known populations approach" ~ "Misc",
    method == "Network scale-up method using summation approach" ~ "Misc",
    method == "Non-scientific" ~ "Misc",
    method == "Proxy respondent method" ~ "Misc"
  ))
table(pse.dat$method_std, useNA = "always")
pse.dat$method[which(is.na(pse.dat$method_std))]


# Find reference populations and calculate prevalence ---------------------

pse.dat$reference_pop = NA
pse.dat$Prevalence_Calc = NA

gadm.36.adm1 = readRDS("../Data/Shapefiles/gadm36_adm1_africa_fpop.rds")
country.shape.0.fratio = readRDS("../Data/Shapefiles/gadm36_country_africa_fpop.rds")

## Get rid of Census_Pop_ with census years less than 2000
cy.1 = pse.dat$Census_Year_1
cy.2 = pse.dat$Census_Year_2
cy.3 = pse.dat$Census_Year_3
cy.4 = pse.dat$Census_Year_4
cy.5 = pse.dat$Census_Year_5
cy.6 = pse.dat$Census_Year_6
pse.dat$Census_Pop_1[cy.1 < 2000] = NA
pse.dat$Census_Pop_2[cy.2 < 2000] = NA
pse.dat$Census_Pop_3[cy.3 < 2000] = NA
pse.dat$Census_Pop_4[cy.4 < 2000] = NA
pse.dat$Census_Pop_5[cy.5 < 2000] = NA
pse.dat$Census_Pop_6[cy.6 < 2000] = NA
pse.dat$Census_Year_1[cy.1 < 2000] = NA
pse.dat$Census_Year_2[cy.2 < 2000] = NA
pse.dat$Census_Year_3[cy.3 < 2000] = NA
pse.dat$Census_Year_4[cy.4 < 2000] = NA
pse.dat$Census_Year_5[cy.5 < 2000] = NA
pse.dat$Census_Year_6[cy.6 < 2000] = NA

pse.dat$data_year = as.numeric(as.character(pse.dat$data_year))
pse.dat$year = as.numeric(as.character(pse.dat$year))
pse.dat$Census_Pop_1 = as.numeric(as.character(pse.dat$Census_Pop_1))
pse.dat$Census_Pop_2 = as.numeric(as.character(pse.dat$Census_Pop_2))
pse.dat$Census_Pop_3 = as.numeric(as.character(pse.dat$Census_Pop_3))
pse.dat$Census_Pop_4 = as.numeric(as.character(pse.dat$Census_Pop_4))
pse.dat$Census_Pop_5 = as.numeric(as.character(pse.dat$Census_Pop_5))
pse.dat$Census_Pop_6 = as.numeric(as.character(pse.dat$Census_Pop_6))

for(i in 1:nrow(pse.dat)){
  if(pse.dat$Validated_area[i] %in% "National"){
    ## f_ratio in country_shape_x_fratio
    ## Just need population in reference year
    district.iso = as.character(pse.dat$ISO[i])
    if(is.na(district.iso)){
      next
    }
    if(!is.na(pse.dat$data_year[i])){
      ref.year = as.numeric(as.character(pse.dat$data_year[i]))
    }else{
      ref.year = as.numeric(as.character(pse.dat$year[i]))
    }
    if(length(ref.year) == 0 | is.na(ref.year)){
      print(c("Check ind",i))
    }else{
      tmp.year.ind = abs(pse.dat[i,c("Census_Year_1", "Census_Year_2",
                                     "Census_Year_3", "Census_Year_4",
                                     "Census_Year_5", "Census_Year_6")] - ref.year)
      tmp.year.ind[is.na(pse.dat[i,c("Census_Pop_1", "Census_Pop_2",
                                                             "Census_Pop_3", "Census_Pop_4",
                                                             "Census_Pop_5", "Census_Pop_6")])] = NA
      year.ind = which.min(tmp.year.ind)
      if(length(year.ind) != 1){
        next
      }
      census.year = pse.dat[i,c("Census_Year_1", "Census_Year_2",
                                "Census_Year_3", "Census_Year_4",
                                "Census_Year_5", "Census_Year_6")][year.ind]
      
      total.pop.census = pse.dat[i,c("Census_Pop_1", "Census_Pop_2",
                                                           "Census_Pop_3", "Census_Pop_4",
                                                           "Census_Pop_5", "Census_Pop_6")][year.ind]
      
      if(is.na(total.pop.census)){
        next
      }
      
      file.name.ref = paste0(district.iso, ref.year, "_total.tif")
      if(!file.exists(paste0("../Data/Worldpop_total_matching", "/", file.name.ref))){
        link_new = paste0("ftp://ftp.worldpop.org.uk/GIS/Population/Global_2000_2020_1km/",ref.year,"/",
                          district.iso,"/",tolower(district.iso),
                          "_ppp_",ref.year,"_1km_Aggregated.tif")
        download.file(link_new, paste0("../Data/Worldpop_total_matching", "/", file.name.ref), mode = "wb")
      }
      
      file.name.census = paste0(district.iso, census.year, "_total.tif")
      if(!file.exists(paste0("../Data/Worldpop_total_matching", "/", file.name.census))){
        link_new = paste0("ftp://ftp.worldpop.org.uk/GIS/Population/Global_2000_2020_1km/",census.year,"/",
                          district.iso,"/",tolower(district.iso),
                          "_ppp_",census.year,"_1km_Aggregated.tif")
        download.file(link_new, paste0("../Data/Worldpop_total_matching", "/", file.name.census), mode = "wb")
      }
      
      
      country.shape.ind = which(country.shape.0.fratio$ISO == district.iso)
      if(length(country.shape.ind) != 1){
        next
      }
      
      worldPop.ref.year = raster(paste0("../Data/Worldpop_total_matching", "/", file.name.ref))
      worldPop.census.year = raster(paste0("../Data/Worldpop_total_matching", "/", file.name.census))
      
      total.pop.ratio = sum(getValues(worldPop.ref.year), na.rm = T) /
        sum(getValues(worldPop.census.year), na.rm = T)
      
      ref.year.pop = total.pop.census * total.pop.ratio * country.shape.0.fratio$worldpop_fcba_ratio[country.shape.ind]
      pse.dat$reference_pop[i] = ref.year.pop
      pse.dat$Prevalence_Calc[i] = pse.dat$pse[i] / ref.year.pop
    }
  }else{ ## Else not national
    ## f_ratio in country_shape_x_fratio
    ## Just need population in reference year
    district.iso = as.character(pse.dat$ISO[i])
    if(is.na(district.iso)){
      next
    }
    if(!is.na(pse.dat$data_year[i])){
      ref.year = pse.dat$data_year[i]
    }else{
      ref.year = pse.dat$year[i]
    }
    if(length(ref.year) == 0 | is.na(ref.year)){
      print(c("Check ind",i))
    }else{
      tmp.year.ind = abs(pse.dat[i,c("Census_Year_1", "Census_Year_2",
                                     "Census_Year_3", "Census_Year_4",
                                     "Census_Year_5", "Census_Year_6")] - ref.year)
      tmp.year.ind[is.na(pse.dat[i,c("Census_Pop_1", "Census_Pop_2",
                                                             "Census_Pop_3", "Census_Pop_4",
                                                             "Census_Pop_5", "Census_Pop_6")])] = NA
      year.ind = which.min(tmp.year.ind)
      if(length(year.ind) != 1){
        next
      }
      census.year = pse.dat[i,c("Census_Year_1", "Census_Year_2",
                                "Census_Year_3", "Census_Year_4",
                                "Census_Year_5", "Census_Year_6")][year.ind]
      
      total.pop.census = as.numeric(as.character(pse.dat[i,c("Census_Pop_1", "Census_Pop_2",
                                                           "Census_Pop_3", "Census_Pop_4",
                                                           "Census_Pop_5", "Census_Pop_6")][year.ind]))
      
      if(is.na(total.pop.census)){
        next
      }
      
      
      file.name.ref = paste0(district.iso, ref.year, "_total.tif")
      if(!file.exists(paste0("../Data/Worldpop_total_matching", "/", file.name.ref))){
        link_new = paste0("ftp://ftp.worldpop.org.uk/GIS/Population/Global_2000_2020_1km/",ref.year,"/",
                          district.iso,"/",tolower(district.iso),
                          "_ppp_",ref.year,"_1km_Aggregated.tif")
        download.file(link_new, paste0("../Data/Worldpop_total_matching", "/", file.name.ref), mode = "wb")
      }
      
      file.name.census = paste0(district.iso, census.year, "_total.tif")
      if(!file.exists(paste0("../Data/Worldpop_total_matching", "/", file.name.census))){
        link_new = paste0("ftp://ftp.worldpop.org.uk/GIS/Population/Global_2000_2020_1km/",census.year,"/",
                          district.iso,"/",tolower(district.iso),
                          "_ppp_",census.year,"_1km_Aggregated.tif")
        download.file(link_new, paste0("../Data/Worldpop_total_matching", "/", file.name.census), mode = "wb")
      }
      
      country.shape.ind = which(gadm.36.adm1$NAME_1 == as.character(pse.dat$Validated_adm1[i]) &
                                  gadm.36.adm1$ISO == district.iso)
      if(length(country.shape.ind) == 0){
        next
      }
      tmp.dist = gadm.36.adm1[country.shape.ind,]
      
      
      worldPop.ref.year = raster(paste0("../Data/Worldpop_total_matching", "/", file.name.ref))
      worldPop.census.year = raster(paste0("../Data/Worldpop_total_matching", "/", file.name.census))
      
      raster.ref = rasterize(tmp.dist, worldPop.ref.year, mask = TRUE)
      census.ref = rasterize(tmp.dist, worldPop.census.year, mask = TRUE)
      total.pop.ratio = sum(getValues(raster.ref), na.rm = T) / sum(getValues(census.ref), na.rm = T)

      ref.year.pop = total.pop.census * total.pop.ratio * gadm.36.adm1$worldpop_fcba_ratio[country.shape.ind]
      pse.dat$reference_pop[i] = ref.year.pop
      pse.dat$Prevalence_Calc[i] = pse.dat$pse[i] / ref.year.pop
    }
    
  }
  print(i)
}
pse.dat$reference_pop = as.numeric(pse.dat$reference_pop)
pse.dat$Prevalence_Calc = as.numeric(pse.dat$Prevalence_Calc)

plot(pse.dat$Prevalence_Calc, as.numeric(as.character(pse.dat$Prevalence)))
abline(0, 1, col = "red")

# Combined calculated prevs and the lit prevs --------

pse.dat$Prev_comb = pse.dat$Prevalence_Calc ## Use our prevalences if available

## Create prevalence column
for(i in 1:nrow(pse.dat)){
  ## Use existing prevalences, if unavailable
  if(is.na(pse.dat$Prev_comb[i])){
    pse.dat$Prev_comb[i] = as.numeric(as.character(pse.dat$Prevalence[i]))
  }
}

## Using prevalence column, calculate reference population
## Ref_pop = pse / prevalence_calc

pse.dat$ref_pop_joined = round(pse.dat$pse / pse.dat$Prev_comb)
write.csv(pse.dat, "../Data/PSE Spreadsheets/SSA_FSW_PSE_03.csv")