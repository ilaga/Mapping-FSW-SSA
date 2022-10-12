# Clean up R environment
rm(list = ls())

# Load in packages
# devtools::install_github("ropensci/rdhs")
library(rdhs)
library(tidyverse)
library(plyr) ## Needed for rbind.fill
library(dplyr)
library(janitor)
library(labelled)
library(stringr)
library(raster)


# create an account on the DHS website, and then using the function  "set_rdhs_config()" to log in your account.
set_rdhs_config(email = "",
                project = "",
                password_prompt = TRUE)

# Enter "1" and then enter password:


## ----------------------------------------------------------------------------------------------------------------------
# look up survey characteristics 
surveychar <- dhs_survey_characteristics() 

# query country names and 2-letter codes
country.all = dhs_countries(returnFields = c("CountryName", "DHS_CountryCode", "RegionName", "ISO3_CountryCode"))
country.ssa = subset(country.all, RegionName == "Sub-Saharan Africa")
dhs.codes = country.ssa$DHS_CountryCode

# request dataset by entering the country code
surveys <- dhs_surveys(surveyCharacteristicIds = NULL,
                       countryIds = dhs.codes)

country.ind = 1
datasets.HR = NA
datasets.MR = NA
datasets.IR = NA
datasets.KR = NA
datasets.BR = NA
datasets.CR = NA
datasets.GC = NA
geographics = NA

need.geo = TRUE

## Order will always be alphabetical by fileType
## BR, CR, HR, IR, KR, MR

for(i in 1:length(dhs.codes)){
  tmp.survey = try(dhs_surveys(surveyCharacteristicIds = NULL,
                               countryIds = dhs.codes[i]),
                   silent = TRUE)
  if(class(tmp.survey) == "try-error"){
    cat("No DHS Survey for:",dhs.codes[i],"\n")
  }else{
    BR.new <- try(dhs_datasets(surveyIds = tmp.survey$SurveyId, 
                                  fileType = "BR", ## Births
                                  fileFormat = "flat"),
                  silent = TRUE)
    CR.new <- try(dhs_datasets(surveyIds = tmp.survey$SurveyId, 
                                   fileType = "CR", ## Couple
                                   fileFormat = "flat"),
                  silent = TRUE)
    HR.new <- try(dhs_datasets(surveyIds = tmp.survey$SurveyId, 
                                     fileType = "HR", ## Household
                                     fileFormat = "flat"),
                  silent = TRUE)
    IR.new <- try(dhs_datasets(surveyIds = tmp.survey$SurveyId, 
                                fileType = "IR", ## Individual
                                fileFormat = "flat"),
                  silent = TRUE)
    KR.new <- try(dhs_datasets(surveyIds = tmp.survey$SurveyId, 
                                  fileType = "KR", ## Children
                                  fileFormat = "flat"),
                  silent = TRUE)
    MR.new <- try(dhs_datasets(surveyIds = tmp.survey$SurveyId, 
                                fileType = "MR", ## Men
                                fileFormat = "flat"),
                  silent = TRUE)
    
    GC.new <- try(dhs_datasets(surveyIds = tmp.survey$SurveyId,
                               fileType = "GC", ## Geographic
                               fileFormat = "flat"),
                  silent = TRUE)
    
    geographic.new <- try(dhs_datasets(surveyIds = tmp.survey$SurveyId,
                                     fileType = "GE", ## Geographic
                                     fileFormat = "flat"),
                          silent = TRUE)
    

    
    if(class(geographic.new) == "try-error"){
      cat(c("No geographic data for:",dhs.codes[i],"\n"))
      next
    }
    
    if(need.geo){
      BR.new = subset(BR.new, SurveyId %in% geographic.new$SurveyId)
      CR.new = subset(CR.new, SurveyId %in% geographic.new$SurveyId)
      HR.new = subset(HR.new, SurveyId %in% geographic.new$SurveyId)
      IR.new = subset(IR.new, SurveyId %in% geographic.new$SurveyId)
      KR.new = subset(KR.new, SurveyId %in% geographic.new$SurveyId)
      MR.new = subset(MR.new, SurveyId %in% geographic.new$SurveyId)
      GC.new = subset(GC.new, SurveyId %in% geographic.new$SurveyId)
    }
    
    if(country.ind == 1){
      datasets.BR = BR.new
      datasets.CR = CR.new
      datasets.HR = HR.new
      datasets.IR = IR.new
      datasets.KR = KR.new
      datasets.MR = MR.new
      datasets.GC = GC.new
      geographics = geographic.new

    }else{
      if(class(BR.new) != "try-error"){
        datasets.BR = rbind.data.frame(datasets.BR, BR.new)
      }
      if(class(CR.new) != "try-error"){
        datasets.CR = rbind.data.frame(datasets.CR, CR.new)
      }
      if(class(HR.new) != "try-error"){
        datasets.HR = rbind.data.frame(datasets.HR, HR.new)
      }
      if(class(IR.new) != "try-error"){
        datasets.IR = rbind.data.frame(datasets.IR, IR.new)
      }
      if(class(KR.new) != "try-error"){
        datasets.KR = rbind.data.frame(datasets.KR, KR.new)
      }
      if(class(MR.new) != "try-error"){
        datasets.MR = rbind.data.frame(datasets.MR, MR.new)
      }
      if(class(GC.new) != "try-error"){
        datasets.GC = rbind.data.frame(datasets.GC, GC.new)
      }

      if(class(geographic.new) != "try-error"){
        geographics = rbind.data.frame(geographics, geographic.new)
      }
    }
    country.ind = country.ind + 1
  }
}


# download datasets
BR.downloads = get_datasets(dataset_filenames = datasets.BR)
# CR.downloads = get_datasets(dataset_filenames = datasets.CR)
HR.downloads = get_datasets(dataset_filenames = datasets.HR)
# IR.downloads = get_datasets(dataset_filenames = datasets.IR)
KR.downloads = get_datasets(dataset_filenames = datasets.KR)
MR.downloads = get_datasets(dataset_filenames = datasets.MR)
GC.downloads = get_datasets(dataset_filenames = datasets.GC)
geo.downloads = get_datasets(dataset_filenames = geographics)
## GC.downloads always match with geo.downloads


## IR and CR have errors. Run them one by one
IR.downloads = list()
IR.downloads[[1]] = unlist(get_datasets(dataset_filenames = datasets.IR[1,]))
j = 2
for(i in 2:nrow(datasets.IR)){
  IR.new = try(unlist(get_datasets(dataset_filenames = datasets.IR[i,])))
  if(class(IR.new) != "try-error"){
    IR.downloads[[j]] = IR.new
    j = j + 1
  }
}

CR.downloads = list()
CR.downloads[[1]] = unlist(get_datasets(dataset_filenames = datasets.CR[1,]))
j = 2
for(i in 2:nrow(datasets.CR)){
  CR.new = try(unlist(get_datasets(dataset_filenames = datasets.CR[i,])))
  if(class(CR.new) != "try-error"){
    CR.downloads[[j]] = CR.new
      j = j + 1
  }
}





## -----------------------------------------------------------------------------
## Load in shapefiles
gadm.36.adm1 = readRDS("../Data/Shapefiles/gadm36_adm1_africa.rds")


# EDA: Tabulate how many countries each name has -------------------------------
dhs.countries = unique(c(datasets.BR$DHS_CountryCode,
                         datasets.CR$DHS_CountryCode,
                         datasets.HR$DHS_CountryCode,
                         datasets.IR$DHS_CountryCode,
                         datasets.KR$DHS_CountryCode,
                         datasets.MR$DHS_CountryCode,
                         datasets.GC$DHS_CountryCode))



all.names = list()
name.map = matrix(NA, nrow = 0, ncol = 2)
for(i in 1:length(BR.downloads)){
  tmp.BR = try(readRDS(BR.downloads[[i]]))
  if("try-error" %in% class(tmp.BR)){
    next
  }
  country.ind = which(dhs.countries == datasets.BR$DHS_CountryCode[i])
  BR.names.lab = sapply(tmp.BR, var_label)
  BR.names = names(tmp.BR)
  if(length(all.names) < country.ind){
    all.names[[country.ind]] = BR.names
  }else{
    all.names[[country.ind]] = union(all.names[[country.ind]], BR.names)
  }
  # all.names = union(all.names, BR.names)
  name.map = rbind(name.map, cbind(BR.names.lab, names(BR.names.lab)))
  print(i / length(BR.downloads))
}
name.map = unique(name.map)
names.BR.vec = unlist(all.names)
names.BR.table = table(names.BR.vec)
names.BR.sorted = sort(names.BR.table, decreasing = TRUE)
BR.table = matrix(NA, nrow = length(names.BR.sorted), ncol = 3)
BR.table[,1] = names(names.BR.sorted)
BR.table[,3] = names.BR.sorted
name.match = match(names(names.BR.sorted), name.map[,2])
BR.table[,2] = name.map[name.match,1]
colnames(BR.table) = c("Code", "Label", "Country Count")
write.csv(BR.table, file = "../Data/DHS/DHS_BR_covariates.csv", row.names = F)



all.names = list()
name.map = matrix(NA, nrow = 0, ncol = 2)
for(i in 1:length(CR.downloads)){
  tmp.CR = try(readRDS(CR.downloads[[i]]))
  if("try-error" %in% class(tmp.CR)){
    next
  }
  country.ind = which(dhs.countries == datasets.CR$DHS_CountryCode[i])
  CR.names.lab = sapply(tmp.CR, var_label)
  CR.names = names(tmp.CR)
  if(length(all.names) < country.ind){
    all.names[[country.ind]] = CR.names
  }else{
    all.names[[country.ind]] = union(all.names[[country.ind]], CR.names)
  }
  # all.names = union(all.names, CR.names)
  name.map = rbind(name.map, cbind(CR.names.lab, names(CR.names.lab)))
  print(i / length(CR.downloads))
}
name.map = unique(name.map)
names.CR.vec = unlist(all.names)
names.CR.table = table(names.CR.vec)
names.CR.sorted = sort(names.CR.table, decreasing = TRUE)
CR.table = matrix(NA, nrow = length(names.CR.sorted), ncol = 3)
CR.table[,1] = names(names.CR.sorted)
CR.table[,3] = names.CR.sorted
name.match = match(names(names.CR.sorted), name.map[,2])
CR.table[,2] = name.map[name.match,1]
colnames(CR.table) = c("Code", "Label", "Country Count")
write.csv(CR.table, file = "../Data/DHS/DHS_CR_covariates.csv", row.names = F)





all.names = list()
name.map = matrix(NA, nrow = 0, ncol = 2)
for(i in 1:length(HR.downloads)){
  tmp.HR = try(readRDS(HR.downloads[[i]]))
  if("try-error" %in% class(tmp.HR)){
    next
  }
  country.ind = which(dhs.countries == datasets.HR$DHS_CountryCode[i])
  HR.names.lab = sapply(tmp.HR, var_label)
  HR.names = names(tmp.HR)
  if(length(all.names) < country.ind){
    all.names[[country.ind]] = HR.names
  }else{
    all.names[[country.ind]] = union(all.names[[country.ind]], HR.names)
  }
  name.map = rbind(name.map, cbind(HR.names.lab, names(HR.names.lab)))
  print(i / length(HR.downloads))
}
name.map = unique(name.map)
names.HR.vec = unlist(all.names)
names.HR.table = table(names.HR.vec)
names.HR.sorted = sort(names.HR.table, decreasing = TRUE)
HR.table = matrix(NA, nrow = length(names.HR.sorted), ncol = 3)
HR.table[,1] = names(names.HR.sorted)
HR.table[,3] = names.HR.sorted
name.match = match(names(names.HR.sorted), name.map[,2])
HR.table[,2] = name.map[name.match,1]
colnames(HR.table) = c("Code", "Label", "Country Count")
write.csv(HR.table, file = "../Data/DHS/DHS_HR_covariates.csv", row.names = F)




all.names = list()
name.map = matrix(NA, nrow = 0, ncol = 2)
for(i in 1:length(IR.downloads)){
  tmp.IR = try(readRDS(IR.downloads[[i]]))
  if("try-error" %in% class(tmp.IR)){
    next
  }
  country.ind = which(dhs.countries == datasets.IR$DHS_CountryCode[i])
  IR.names.lab = sapply(tmp.IR, var_label)
  IR.names = names(tmp.IR)
  if(length(all.names) < country.ind){
    all.names[[country.ind]] = IR.names
  }else{
    all.names[[country.ind]] = union(all.names[[country.ind]], IR.names)
  }
  name.map = rbind(name.map, cbind(IR.names.lab, names(IR.names.lab)))
  print(i / length(IR.downloads))
}
name.map = unique(name.map)
names.IR.vec = unlist(all.names)
names.IR.table = table(names.IR.vec)
names.IR.sorted = sort(names.IR.table, decreasing = TRUE)
IR.table = matrix(NA, nrow = length(names.IR.sorted), ncol = 3)
IR.table[,1] = names(names.IR.sorted)
IR.table[,3] = names.IR.sorted
name.match = match(names(names.IR.sorted), name.map[,2])
IR.table[,2] = name.map[name.match,1]
colnames(IR.table) = c("Code", "Label", "Country Count")
write.csv(IR.table, file = "../Data/DHS/DHS_IR_covariates.csv", row.names = F)





all.names = list()
name.map = matrix(NA, nrow = 0, ncol = 2)
for(i in 1:length(KR.downloads)){
  tmp.KR = try(readRDS(KR.downloads[[i]]))
  if("try-error" %in% class(tmp.KR)){
    next
  }
  country.ind = which(dhs.countries == datasets.KR$DHS_CountryCode[i])
  KR.names.lab = sapply(tmp.KR, var_label)
  KR.names = names(tmp.KR)
  if(length(all.names) < country.ind){
    all.names[[country.ind]] = KR.names
  }else{
    all.names[[country.ind]] = union(all.names[[country.ind]], KR.names)
  }
  name.map = rbind(name.map, cbind(KR.names.lab, names(KR.names.lab)))
  print(i / length(KR.downloads))
}
name.map = unique(name.map)
names.KR.vec = unlist(all.names)
names.KR.table = table(names.KR.vec)
names.KR.sorted = sort(names.KR.table, decreasing = TRUE)
KR.table = matrix(NA, nrow = length(names.KR.sorted), ncol = 3)
KR.table[,1] = names(names.KR.sorted)
KR.table[,3] = names.KR.sorted
name.match = match(names(names.KR.sorted), name.map[,2])
KR.table[,2] = name.map[name.match,1]
colnames(KR.table) = c("Code", "Label", "Country Count")
write.csv(KR.table, file = "../Data/DHS/DHS_KR_covariates.csv", row.names = F)





all.names = list()
name.map = matrix(NA, nrow = 0, ncol = 2)
for(i in 1:length(MR.downloads)){
  tmp.MR = try(readRDS(MR.downloads[[i]]))
  if("try-error" %in% class(tmp.MR)){
    next
  }
  country.ind = which(dhs.countries == datasets.MR$DHS_CountryCode[i])
  MR.names.lab = sapply(tmp.MR, var_label)
  MR.names = names(tmp.MR)
  if(length(all.names) < country.ind){
    all.names[[country.ind]] = MR.names
  }else{
    all.names[[country.ind]] = union(all.names[[country.ind]], MR.names)
  }
  name.map = rbind(name.map, cbind(MR.names.lab, names(MR.names.lab)))
  print(i / length(MR.downloads))
}
name.map = unique(name.map)
names.MR.vec = unlist(all.names)
names.MR.table = table(names.MR.vec)
names.MR.sorted = sort(names.MR.table, decreasing = TRUE)
MR.table = matrix(NA, nrow = length(names.MR.sorted), ncol = 3)
MR.table[,1] = names(names.MR.sorted)
MR.table[,3] = names.MR.sorted
name.match = match(names(names.MR.sorted), name.map[,2])
MR.table[,2] = name.map[name.match,1]
colnames(MR.table) = c("Code", "Label", "Country Count")
write.csv(MR.table, file = "../Data/DHS/DHS_MR_covariates.csv", row.names = F)


all.names = list()
name.map = matrix(NA, nrow = 0, ncol = 1)
for(i in 1:length(GC.downloads)){
  tmp.GC = try(readRDS(GC.downloads[[i]]))
  if("try-error" %in% class(tmp.GC)){
    next
  }
  country.ind = which(dhs.countries == datasets.GC$DHS_CountryCode[i])
  GC.names = names(tmp.GC)
  if(length(all.names) < country.ind){
    all.names[[country.ind]] = GC.names
  }else{
    all.names[[country.ind]] = union(all.names[[country.ind]], GC.names)
  }
  print(i / length(GC.downloads))
}
names.GC.vec = unlist(all.names)
names.GC.table = table(names.GC.vec)
names.GC.sorted = sort(names.GC.table, decreasing = TRUE)
GC.table = matrix(NA, nrow = length(names.GC.sorted), ncol = 2)
GC.table[,1] = names(names.GC.sorted)
GC.table[,2] = names.GC.sorted
colnames(GC.table) = c("Label", "Country Count")
write.csv(GC.table, file = "../Data/DHS/DHS_GC_covariates.csv", row.names = F)



## Grab names with 30 or above countries
BR.names.keep = subset(BR.table, as.numeric(BR.table[,3]) > 30)
CR.names.keep = subset(CR.table, as.numeric(CR.table[,3]) > 30)
HR.names.keep = subset(HR.table, as.numeric(HR.table[,3]) > 30)
IR.names.keep = subset(IR.table, as.numeric(IR.table[,3]) > 30)
KR.names.keep = subset(KR.table, as.numeric(KR.table[,3]) > 30)
MR.names.keep = subset(MR.table, as.numeric(MR.table[,3]) > 30)
GC.names.keep = subset(GC.table, as.numeric(GC.table[,2]) > 30)




# Run through again and process data --------------------------------------

## Clear up some memory
rm(all.names)
rm(BR.table, CR.table, HR.table, IR.table, KR.table, MR.table)
rm(names.BR.sorted, names.BR.table, names.BR.vec,
   names.CR.sorted, names.CR.table, names.CR.vec,
   names.HR.sorted, names.HR.table, names.HR.vec,
   names.IR.sorted, names.IR.table, names.IR.vec,
   names.KR.sorted, names.KR.table, names.KR.vec,
   names.MR.sorted, names.MR.table, names.MR.vec)






# Find numeric columns to find special cases ------------------------------

DHS.numeric.cases = function(datasets, fileType.names.keep, fileType){
  fileType.mat = matrix(NA, nrow = 150, ncol = 0) ## Don't need 150, but just to be safe
  for(i in 1:length(unique(datasets$CountryName))){
    datasets.subset = subset(datasets, CountryName == unique(datasets$CountryName)[i])

    for(j in 1:nrow(datasets.subset)){
      tmp.fileType.sub = try(readRDS(get_datasets(dataset_filenames = datasets.subset[j,])[[1]]))
      if("try-error" %in% class(tmp.fileType.sub)){
        next
      }

      ## Add the ISO, NAME_0, and NAME_1 info to tmp.fileType.sub
      tmp.fileType.keep = which(names(tmp.fileType.sub) %in% fileType.names.keep)
      tmp.fileType.sub = tmp.fileType.sub[,tmp.fileType.keep]
      
      
      if(j == 1){
        tmp.fileType = tmp.fileType.sub
      }else{
        tmp.fileType = rbind.fill(tmp.fileType, tmp.fileType.sub)
      }
    }

    
    fileType.names = names(tmp.fileType)
    fileType.labels = sapply(tmp.fileType, var_label)
    ## First run tmp.fileType and get numeric columns
    numeric.col.ind = rep(FALSE, ncol(tmp.fileType))
    for(j in 1:ncol(tmp.fileType)){
      labels.resp = unique(val_labels(tmp.fileType[,j]))
      poss.resp = unique(tmp.fileType[,j])
      poss.resp = poss.resp[which(!is.na(poss.resp))]
      numeric.col = any(!(poss.resp %in% labels.resp))
      if(numeric.col){
        if(!(fileType.names[j] %in% fileType.mat[1,])){
          fileType.mat = cbind(fileType.mat,
                               c(fileType.names[j],
                                 fileType.labels[j],
                                 val_labels(tmp.fileType[,j]),
                                 rep(NA, 150 - 2 - length(val_labels(tmp.fileType[,j])))),
                               c(fileType.names[j],
                                 fileType.labels[j],
                                 names(val_labels(tmp.fileType[,j])),
                                 rep(NA, 150 - 2 - length(val_labels(tmp.fileType[,j])))))
        }
      }
    }
    cat(c("Iter:",i,"Percent:",i/length(unique(datasets$CountryName)), "\n"))
  }
  return(fileType.mat)
}

BR.df.numeric.cases = DHS.numeric.cases(datasets.BR, BR.names.keep, "BR")
write.csv(BR.df.numeric.cases, file = "../Data/DHS/BR_numeric_cases.csv")

CR.df.numeric.cases = DHS.numeric.cases(datasets.CR, CR.names.keep, "CR")
write.csv(CR.df.numeric.cases, file = "../Data/DHS/CR_numeric_cases.csv")

HR.df.numeric.cases = DHS.numeric.cases(datasets.HR, HR.names.keep, "HR")
write.csv(HR.df.numeric.cases, file = "../Data/DHS/HR_numeric_cases.csv")

IR.df.numeric.cases = DHS.numeric.cases(datasets.IR, IR.names.keep, "IR")
write.csv(IR.df.numeric.cases, file = "../Data/DHS/IR_numeric_cases.csv")

KR.df.numeric.cases = DHS.numeric.cases(datasets.KR, KR.names.keep, "KR")
write.csv(KR.df.numeric.cases, file = "../Data/DHS/KR_numeric_cases.csv")

MR.df.numeric.cases = DHS.numeric.cases(datasets.MR, MR.names.keep, "MR")
write.csv(MR.df.numeric.cases, file = "../Data/DHS/MR_numeric_cases.csv")

GC.df.numeric.cases = DHS.numeric.cases(datasets.GC, GC.names.keep, "GC")
write.csv(GC.df.numeric.cases, file = "../Data/DHS/GC_numeric_cases.csv")





# Now process and summarize the data --------------------------------------


## Special Cases:
## For numeric columns, zeros should always map to zeros

## BR: v152 96 -> 96+ (Just use 96)
##     v464 80 -> 80 (Just use 80)
##     v477 90 -> 90
##     v478 90 -> 90

## CR: v464 80 -> 80

## CR: v464 80 -> 80

## HR: hv105 all and hml16 all 97 -> 97
##     hml11 5 -> 5

## IR: v152 97 -> 97
##     v220 6 -> 6
##     v322 5 -> 5

## KR: v152 97 -> 97
##     v220 6 -> 6
##     v322 5 -> 5

## MR: mv152 95 -> 95
##     mv167 90 -> 90
##     mv478 90 -> 90
##     mv477 90 -> 90
##     mv836 95 -> 95

## GC: No problems



## Checked result of all high p-value after p-value computation




DHS.process.func = function(datasets, shapefile.in, fileType.names.keep, fileType){
  fileType.list = list()
  for(i in 1:length(unique(datasets$CountryName))){
    datasets.subset = subset(datasets, CountryName == unique(datasets$CountryName)[i])
    iso.tmp = country.all$ISO3_CountryCode[which(country.all$DHS_CountryCode == unique(datasets$DHS_CountryCode)[i])]
    country.sub = subset(shapefile.in, ISO == iso.tmp)
    if(!(iso.tmp %in% shapefile.in$ISO)){
      cat(c("Shapefile does not include:", unique(datasets$CountryName)[i], "\n"))
      next
    }
    for(j in 1:nrow(datasets.subset)){
      which.geo = match(datasets.subset$SurveyId[j], geographics$SurveyId)
      
      tmp.fileType.sub = try(readRDS(get_datasets(dataset_filenames = datasets.subset[j,])[[1]]))
      if("try-error" %in% class(tmp.fileType.sub)){
        next
      }
      tmp.geo = readRDS(get_datasets(dataset_filenames = geographics[which.geo,])[[1]])
      
      
      ## Add district names to tmp.fileType.sub
      gadm.crop = crop(country.sub, extent(tmp.geo))
      geo.over = sp::over(tmp.geo, country.sub)
      ## Some clusters are not in the geographic downloads for some reason
      tmp.geo@data = cbind(tmp.geo@data, geo.over)
      
      
      
      ## Add the ISO, NAME_0, and NAME_1 info to tmp.fileType.sub
      tmp.fileType.keep = which(names(tmp.fileType.sub) %in% fileType.names.keep)
      tmp.fileType.sub = tmp.fileType.sub[,tmp.fileType.keep]
      tmp.fileType.sub$ISO = NA
      tmp.fileType.sub$NAME_0 = NA
      tmp.fileType.sub$NAME_1 = NA
      
      
      if(fileType == "HR"){
        tmp.fileType.sub$ISO = tmp.geo@data$ISO[match(tmp.fileType.sub$hv001, tmp.geo@data$DHSCLUST)]
        tmp.fileType.sub$NAME_0 = tmp.geo@data$NAME_0[match(tmp.fileType.sub$hv001, tmp.geo@data$DHSCLUST)]
        tmp.fileType.sub$NAME_1 = tmp.geo@data$NAME_1[match(tmp.fileType.sub$hv001, tmp.geo@data$DHSCLUST)]
      }else if(fileType == "MR"){
        tmp.fileType.sub$ISO = tmp.geo@data$ISO[match(tmp.fileType.sub$mv001, tmp.geo@data$DHSCLUST)]
        tmp.fileType.sub$NAME_0 = tmp.geo@data$NAME_0[match(tmp.fileType.sub$mv001, tmp.geo@data$DHSCLUST)]
        tmp.fileType.sub$NAME_1 = tmp.geo@data$NAME_1[match(tmp.fileType.sub$mv001, tmp.geo@data$DHSCLUST)]
      }else if(fileType == "GC"){
        tmp.fileType.sub$ISO = tmp.geo@data$ISO[match(tmp.fileType.sub$DHSCLUST, tmp.geo@data$DHSCLUST)]
        tmp.fileType.sub$NAME_0 = tmp.geo@data$NAME_0[match(tmp.fileType.sub$DHSCLUST, tmp.geo@data$DHSCLUST)]
        tmp.fileType.sub$NAME_1 = tmp.geo@data$NAME_1[match(tmp.fileType.sub$DHSCLUST, tmp.geo@data$DHSCLUST)]
      }else{
        tmp.fileType.sub$ISO = tmp.geo@data$ISO[match(tmp.fileType.sub$v001, tmp.geo@data$DHSCLUST)]
        tmp.fileType.sub$NAME_0 = tmp.geo@data$NAME_0[match(tmp.fileType.sub$v001, tmp.geo@data$DHSCLUST)]
        tmp.fileType.sub$NAME_1 = tmp.geo@data$NAME_1[match(tmp.fileType.sub$v001, tmp.geo@data$DHSCLUST)]
      }

      
      ## Keep only the complete ones
      tmp.fileType.sub = subset(tmp.fileType.sub, !is.na(ISO))
      
      
      if(j == 1){
        tmp.fileType = tmp.fileType.sub
      }else{
        tmp.fileType = rbind.fill(tmp.fileType, tmp.fileType.sub)
      }
    }
    
    rm(gadm.crop)
    rm(geo.over)
    rm(tmp.geo)
    rm(tmp.fileType.sub)
    
    
    grouped.fileType = tmp.fileType %>%
      dplyr::group_by(NAME_1)
    
    
    tmp.fileType.names = tmp.fileType[,c("ISO", "NAME_0", "NAME_1")]
    tmp.fileType.names1 = tmp.fileType.names[,"NAME_1"]
    tmp.fileType = tmp.fileType[,-which(names(tmp.fileType) == "NAME_1" |
                                          names(tmp.fileType) == "NAME_0" |
                                          names(tmp.fileType) == "ISO")]
    ## First run through and get numeric columns
    numeric.col.ind = rep(FALSE, ncol(tmp.fileType))
    for(j in 1:ncol(tmp.fileType)){
      labels.resp = unique(val_labels(tmp.fileType[,j]))
      ## Remove zero from labels.resp
      if(0 %in% labels.resp){
        labels.resp = labels.resp[-which(labels.resp == 0)]
      }
      ## Catch other special cases
      if(fileType == "BR"){
        if(names(tmp.fileType) == "v152"){
          which.rm = -which(labels.resp == 96)
          if(length(which.rm) > 0){
            labels.resp = labels.resp[-which.rm]
          }
        }
        if(names(tmp.fileType) == "v464"){
          which.rm = -which(labels.resp == 80)
          if(length(which.rm) > 0){
            labels.resp = labels.resp[-which.rm]
          }
        }
        if(names(tmp.fileType) == "v477"){
          which.rm = -which(labels.resp == 90)
          if(length(which.rm) > 0){
            labels.resp = labels.resp[-which.rm]
          }
        }
        if(names(tmp.fileType) == "v478"){
          which.rm = -which(labels.resp == 90)
          if(length(which.rm) > 0){
            labels.resp = labels.resp[-which.rm]
          }
        }
      }
      
      if(fileType == "CR"){
        if(names(tmp.fileType) == "v464"){
          which.rm = -which(labels.resp == 96)
          if(length(which.rm) > 0){
            labels.resp = labels.resp[-which.rm]
          }
        }
      }
      
      if(fileType == "HR"){
        if(names(tmp.fileType) == "hml11"){
          which.rm = -which(labels.resp == 5)
          if(length(which.rm) > 0){
            labels.resp = labels.resp[-which.rm]
          }
        }
      }
      
      if(fileType == "IR" | fileType == "KR"){
        if(names(tmp.fileType) == "v152"){
          which.rm = -which(labels.resp == 97)
          if(length(which.rm) > 0){
            labels.resp = labels.resp[-which.rm]
          }
        }
        
        if(names(tmp.fileType) == "v220"){
          which.rm = -which(labels.resp == 6)
          if(length(which.rm) > 0){
            labels.resp = labels.resp[-which.rm]
          }
        }
        
        if(names(tmp.fileType) == "v322"){
          which.rm = -which(labels.resp == 5)
          if(length(which.rm) > 0){
            labels.resp = labels.resp[-which.rm]
          }
        }
      }
      
      if(fileType == "MR"){
        if(names(tmp.fileType) == "mv152"){
          which.rm = -which(labels.resp == 95)
          if(length(which.rm) > 0){
            labels.resp = labels.resp[-which.rm]
          }
        }
        
        if(names(tmp.fileType) == "mv167" |
           names(tmp.fileType) == "mv478" |
           names(tmp.fileType) == "mv477"){
          which.rm = -which(labels.resp == 90)
          if(length(which.rm) > 0){
            labels.resp = labels.resp[-which.rm]
          }
        }
        
        if(names(tmp.fileType) == "mv836"){
          which.rm = -which(labels.resp == 95)
          if(length(which.rm) > 0){
            labels.resp = labels.resp[-which.rm]
          }
        }
      }
      
      
      
      poss.resp = unique(tmp.fileType[,j])
      poss.resp = poss.resp[which(!is.na(poss.resp))]
      numeric.col = any(!(poss.resp %in% labels.resp))
      if(numeric.col){
        ## Now replace other labels
        tmp.fileType[tmp.fileType[,j] %in% labels.resp,j] = NA
        tmp = try(as.numeric(tmp.fileType[,j]), silent = T)
        if("try-error" %in% class(tmp)){
          next
        }
        tmp.fileType[,j] = as.numeric(tmp.fileType[,j])
        numeric.col.ind[j] = TRUE
      }
    }
    
    tmp.fileType.numeric = tmp.fileType[,which(numeric.col.ind == TRUE)]
    tmp.fileType.numeric = cbind(tmp.fileType.numeric, NAME_1 = tmp.fileType.names1)
    tmp.fileType.nonnumeric = tmp.fileType[,which(numeric.col.ind == FALSE)]
    tmp.fileType.nonnumeric = cbind(tmp.fileType.nonnumeric, NAME_1 = tmp.fileType.names1)
    
    fileType.numeric.grouped = tmp.fileType.numeric %>% dplyr::group_by(NAME_1)
    fileType.nonnumeric.grouped = tmp.fileType.nonnumeric %>% dplyr::group_by(NAME_1)
    
    table.numeric = fileType.numeric.grouped %>%
      dplyr::summarise_all(list(mean = mean, median = median), na.rm = T)

    if(0){ ## Do we want factor variables?
      for(j in 1:ncol(tmp.fileType)) {
        if(!numeric.col.ind[j]){
          ## Else if has label, create table
          table1 <-
            tmp.fileType.nonnumeric %>%
            dplyr::count(NAME_1, across(names[j]), .drop = FALSE) %>%
            pivot_wider(names_from = names[j],
                        values_from = "n") %>%
            adorn_percentages("row") %>%
            adorn_pct_formatting(digits = 2, affix_sign = FALSE)
          table.add = table1[,-1]
          which.names.match = match(names(table.add), val_labels(tmp.fileType[, j]))
          which.not.na = which(!is.na(which.names.match))
          which.na = which(is.na(which.names.match))
          which.names.match = which.names.match[!is.na(which.names.match)]
          
          names(table.add)[which.not.na] = paste0(names[j], "_", names(val_labels(tmp.fileType[, j])))[which.names.match]
          names(table.add)[which.na] = paste0(names[j], "_NA")
          
          data.region = cbind(data.region, table.add)
          table.list = c(table.list, j)
        }
        
        if(j %% 100 == 0){
          print(100 * j / ncol(tmp.fileType))
        }
      }
      
      data.region = cbind(table.numeric, table.nonnumeric)
    }else{
      data.region = table.numeric
    }
    
    data.region$ISO = iso.tmp
    
    print(unique(datasets$CountryName)[i])
    fileType.list[[i]] = data.region
    gc()
  }
  fileType.df.all = rbind.fill(fileType.list)
  return(fileType.df.all)
}


# Do it for GADM.36.adm1 --------------------------------------------------

BR.df.all = DHS.process.func(datasets.BR, gadm.36.adm1, BR.names.keep, "BR")
saveRDS(BR.df.all, file = "../Data/DHS/BR_gadm_36_adm1.rds")
rm(BR.df.all)

CR.df.all = DHS.process.func(datasets.CR, gadm.36.adm1, CR.names.keep, "CR")
saveRDS(CR.df.all, file = "../Data/DHS/CR_gadm_36_adm1.rds")
rm(CR.df.all)

HR.df.all = DHS.process.func(datasets.HR, gadm.36.adm1, HR.names.keep, "HR")
saveRDS(HR.df.all, file = "../Data/DHS/HR_gadm_36_adm1.rds")
rm(HR.df.all)

IR.df.all = DHS.process.func(datasets.IR, gadm.36.adm1, IR.names.keep, "IR")
saveRDS(IR.df.all, file = "../Data/DHS/IR_gadm_36_adm1.rds")
rm(IR.df.all)

KR.df.all = DHS.process.func(datasets.KR, gadm.36.adm1, KR.names.keep, "KR")
saveRDS(KR.df.all, file = "../Data/DHS/KR_gadm_36_adm1.rds")
rm(KR.df.all)

MR.df.all = DHS.process.func(datasets.MR, gadm.36.adm1, MR.names.keep, "MR")
saveRDS(MR.df.all, file = "../Data/DHS/MR_gadm_36_adm1.rds")
rm(MR.df.all)

GC.df.all = DHS.process.func(datasets.GC, gadm.36.adm1, GC.names.keep, "GC")
saveRDS(GC.df.all, file = "../Data/DHS/GC_gadm_36_adm1.rds")
rm(GC.df.all)
