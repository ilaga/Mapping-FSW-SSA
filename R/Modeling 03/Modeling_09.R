library(raster)
library(spdep)
library(plyr)
library(tmap)
library(lme4)
library(ggplot2)
library(brms)
library(optimx)
library(caret)
library(INLA)


rm(list=ls())
pse.dat.fit = readRDS("../Data/PSE_Prep.rds")
gadm.cov = readRDS("../Data/Shapefile_Prep.rds")
nb.mat = readRDS("../Data/nb_mat_Prep.rds")
## Plot average prevalence in each district
gadm.tmp = gadm.cov
gadm.tmp$Prev = NA
for(i in 1:nrow(gadm.tmp)){
  match.ind = which(pse.dat.fit$ID == i)
  if(length(match.ind) > 0){
    gadm.tmp$Prev[i] = weighted.mean(pse.dat.fit$Prev_comb[match.ind],
                                     pse.dat.fit$ref_pop_joined[match.ind],
                                     na.rm = T)
  }
}


## Combined pse and gadm.cov
pse.dat.fit$ref_pop = pse.dat.fit$ref_pop_joined
## Remove large Nairobi estimate
which.rm = which(pse.dat.fit$Prev_comb > 0.6 &
                   pse.dat.fit$fsw_sizeestimate_region == "Nairobi") 
pse.dat.fit = pse.dat.fit[-which.rm,]
n.obs = nrow(pse.dat.fit)

prev.vs.prev.gg = ggplot(pse.dat.fit) + geom_point(aes(y = as.numeric(as.character(Prevalence)),
                                                       x = as.numeric(as.character(Prevalence_Calc))), size = 2) +
  geom_abline(intercept = 0, slope = 1, size = 1.2) +
  xlab("Calculated FSW Proportion") + ylab("Reported Literature FSW Proportion") + 
  theme_grey(base_size = 15) +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        aspect.ratio = 1) +
  xlim(-0.01, 0.71) + ylim(-0.01, 0.71)
prev.vs.prev.gg
ggsave("../Figures/Prev_vs_prev.jpg", prev.vs.prev.gg, width = 10, height = 10)


if(1){ ## Include rest of gadm.cov
  combined = rbind.fill(pse.dat.fit, gadm.cov@data)
  combined$ref_pop = round(combined$ref_pop)
}else{
  combined = pse.dat.fit
  combined$ref_pop = round(combined$ref_pop)
}

pse.dat.fit = combined[,c("pse", "ref_pop", "ID", "method_std",
                          "ISO", "NAME_1", "City",
                          names(combined)[c(286:389)])]
pse.dat.fit = pse.dat.fit[,-which(names(pse.dat.fit) == "ID.1")]

## Remove non-popweighted variables
names(pse.dat.fit)[c(8,9,17,18,24,32,33:56,81,83,85,87,89,91,93,95,103,105,107,109)]
pse.dat.fit = pse.dat.fit[,-c(8,9,17,18,24,32,33:56,81,83,85,87,89,91,93,95,103,105,107,109)]

## Identify variables that need log transform
# par(ask = T)
# par(mfrow = c(1,2))
# for(i in 8:68){
#   if(sum(pse.dat.fit[,i] <= 0, na.rm = T) > 0){
#     print(c(i, "Has non-positive"))
#     next
#   }
#   plot(pse.dat.fit[,i],qlogis(pse.dat.fit$pse / pse.dat.fit$ref_pop),
#        main = i, xlab = names(pse.dat.fit)[i])
#   plot(log(pse.dat.fit[,i]),qlogis(pse.dat.fit$pse / pse.dat.fit$ref_pop),
#        main = i, xlab = names(pse.dat.fit)[i])
# }
# par(ask = F)

## Can't log-transform landcover because many zeros

pse.dat.fit$wc2.1_2.5m_wind_01_popweighted = log(pse.dat.fit$wc2.1_2.5m_wind_01_popweighted)
pse.dat.fit$wc2.1_2.5m_vapr_01_popweighted = log(pse.dat.fit$wc2.1_2.5m_vapr_01_popweighted)
pse.dat.fit$wc2.1_2.5m_srad_01_popweighted = log(pse.dat.fit$wc2.1_2.5m_srad_01_popweighted)
pse.dat.fit$wc2.1_2.5m_elev_popweighted = log(pse.dat.fit$wc2.1_2.5m_elev_popweighted)
pse.dat.fit$wc2.1_2.5m_bio_1_popweighted = log(pse.dat.fit$wc2.1_2.5m_bio_1_popweighted)
pse.dat.fit$wc2.1_2.5m_bio_2_popweighted = log(pse.dat.fit$wc2.1_2.5m_bio_2_popweighted)
pse.dat.fit$wc2.1_2.5m_bio_3_popweighted = log(pse.dat.fit$wc2.1_2.5m_bio_3_popweighted)
pse.dat.fit$wc2.1_2.5m_bio_4_popweighted = log(pse.dat.fit$wc2.1_2.5m_bio_4_popweighted)
pse.dat.fit$wc2.1_2.5m_bio_5_popweighted = log(pse.dat.fit$wc2.1_2.5m_bio_5_popweighted)
pse.dat.fit$wc2.1_2.5m_bio_7_popweighted = log(pse.dat.fit$wc2.1_2.5m_bio_7_popweighted)
pse.dat.fit$wc2.1_2.5m_bio_8_popweighted = log(pse.dat.fit$wc2.1_2.5m_bio_8_popweighted)
pse.dat.fit$wc2.1_2.5m_bio_9_popweighted = log(pse.dat.fit$wc2.1_2.5m_bio_9_popweighted)
pse.dat.fit$wc2.1_2.5m_bio_10_popweighted = log(pse.dat.fit$wc2.1_2.5m_bio_10_popweighted)
pse.dat.fit$wc2.1_2.5m_bio_11_popweighted = log(pse.dat.fit$wc2.1_2.5m_bio_11_popweighted)
pse.dat.fit$wc2.1_2.5m_bio_12_popweighted = log(pse.dat.fit$wc2.1_2.5m_bio_12_popweighted)
pse.dat.fit$wc2.1_2.5m_bio_13_popweighted = log(pse.dat.fit$wc2.1_2.5m_bio_13_popweighted)
pse.dat.fit$wc2.1_2.5m_bio_15_popweighted = log(pse.dat.fit$wc2.1_2.5m_bio_15_popweighted)
pse.dat.fit$wc2.1_2.5m_bio_16_popweighted = log(pse.dat.fit$wc2.1_2.5m_bio_16_popweighted)
pse.dat.fit$wc2.1_2.5m_bio_18_popweighted = log(pse.dat.fit$wc2.1_2.5m_bio_18_popweighted)

pse.dat.fit$hfp_popweighted = log(pse.dat.fit$hfp_popweighted)
pse.dat.fit$infant_mort_popweighted = log(pse.dat.fit$infant_mort_popweighted)
pse.dat.fit$walking_healthcare_popweighted = log(pse.dat.fit$walking_healthcare_popweighted)
pse.dat.fit$duffy_negative_popweighted = log(pse.dat.fit$duffy_negative_popweighted)
pse.dat.fit$sickle_frequency_popweighted = log(pse.dat.fit$sickle_frequency_popweighted)
pse.dat.fit$PD = log(pse.dat.fit$PD)
pse.dat.fit$GDP = log(pse.dat.fit$GDP)
pse.dat.fit$stunting_popweighted = log(pse.dat.fit$stunting_popweighted)
pse.dat.fit$severe_wasting_popweighted = log(pse.dat.fit$severe_wasting_popweighted)
pse.dat.fit$underweight_popweighted = log(pse.dat.fit$underweight_popweighted)
pse.dat.fit$wasting_popweighted = log(pse.dat.fit$wasting_popweighted)
pse.dat.fit$ref_pop_log = log(pse.dat.fit$ref_pop)
pse.dat.fit$urban = log(pse.dat.fit$urban +
                          min(pse.dat.fit$urban[pse.dat.fit$urban > 0]))
pse.dat.fit$g6pdd_frequency_popweighted = log(pse.dat.fit$g6pdd_frequency_popweighted +
                                                min(pse.dat.fit$g6pdd_frequency_popweighted[pse.dat.fit$g6pdd_frequency_popweighted > 0]))



pse.dat.fit$method_std = factor(pse.dat.fit$method_std)
pse.dat.fit$ISO = factor(pse.dat.fit$ISO)


## Remove variables that are too sporadic
pse.dat.fit = pse.dat.fit[,-which(names(pse.dat.fit) %in% c("cropland_irrigated",
                                                            "tree_cover_mlt",
                                                            "mosaic_herbasceous",
                                                            "sparse",
                                                            "tree_cover_fresh",
                                                            "tree_cover_saline",
                                                            "bare",
                                                            "wc2.1_2.5m_prec_01_popweighted",
                                                            "travel_to_50k_popweighted",
                                                            "tree_cover_becto",
                                                            "tree_cover_bdcto",
                                                            "mosaic_tree",
                                                            "water",
                                                            "shrub_cover",
                                                            "mosaic_natural_veg",
                                                            "mosaic_herbaceous",
                                                            "shrubland",
                                                            "grassland",
                                                            "wc2.1_2.5m_bio_14_popweighted",
                                                            "wc2.1_2.5m_bio_19_popweighted",
                                                            "wc2.1_2.5m_bio_17_popweighted",
                                                            "wc2.1_2.5m_bio_6_popweighted"))]


## Now scale
cov.names = names(pse.dat.fit)[c(8:48)]
## Remove correlated variables
rm.var = caret::findCorrelation(cor(pse.dat.fit[,cov.names], use = "pairwise.complete.obs"),
                                cutoff = 0.8, exact = TRUE)
cov.names[rm.var]
cov.names = cov.names[-rm.var]
## Replace NL_quant50 with NL_mean, all NL highly correlated
cov.names[8] = "NL_mean"

single.subnational.ind = which(is.na(pse.dat.fit$pse))
scale.info = scale(pse.dat.fit[single.subnational.ind, cov.names])
for(k in 1:length(cov.names)){
  pse.dat.fit[,cov.names[k]] = (pse.dat.fit[,cov.names[k]] - attributes(scale.info)$`scaled:center`[k]) /
    attributes(scale.info)$`scaled:scale`[k]
}
which.incomplete = which(!complete.cases(pse.dat.fit[,cov.names[-1]]))
pse.dat.fit$NAME_1[which.incomplete]

pse.dat.fit$prev = pse.dat.fit$pse / pse.dat.fit$ref_pop
cov.names = c(cov.names, "City")
pse.dat.fit$logit_prev = qlogis(pse.dat.fit$prev)
pse.sub = subset(pse.dat.fit, !is.na(pse.dat.fit$pse))
pse.pred = subset(pse.dat.fit, is.na(pse.dat.fit$pse))
saveRDS(pse.dat.fit, file = "../Data/Final_pse_dat_fit.rds")
saveRDS(pse.sub, file = "../Data/Final_brms_data_for_fitting.rds")
saveRDS(pse.pred, file = "../Data/Final_brms_data_for_prediction.rds")

pse.obs = subset(pse.dat.fit, !is.na(pse))


# Examine variance of prevalences -----------------------------------------
ISO.vec = unique(pse.obs$ISO)
n.iso = length(ISO.vec)
var.iso = n.iso.vec = rep(NA, n.iso)
k = 1
for(i in ISO.vec){
  dat.iso.sub = subset(pse.obs, ISO == i)
  n.iso.vec[k] = nrow(dat.iso.sub)
  var.iso[k] = var(dat.iso.sub$logit_prev)
  k = k + 1
}

var.df = data.frame(iso = paste0(ISO.vec, " (N = ", n.iso.vec, ")"), var = var.iso, n = n.iso.vec)

gg.iso.var = ggplot(var.df) + geom_point(aes(x = iso, y = var), size = 5) +
  theme_gray(base_size = 15) +
  xlab("Country ISO") + ylab("Variance of logit(FSW Proportion)") +
  theme(legend.title=element_blank(),
        axis.text = element_text(size = 22, face = "bold"),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.text = element_text(size = 22, face = "bold"))
gg.iso.var
ggsave("../Figures/Var_ISO.jpg", gg.iso.var, width = 20, height = 10)


## Repeat for just city
pse.obs.city = subset(pse.obs, City == 1)
ISO.vec = unique(pse.obs.city$ISO)
n.iso = length(ISO.vec)
var.iso = n.iso.vec = rep(NA, n.iso)
k = 1
for(i in ISO.vec){
  dat.iso.sub = subset(pse.obs.city, ISO == i)
  n.iso.vec[k] = nrow(dat.iso.sub)
  var.iso[k] = var(dat.iso.sub$logit_prev)
  k = k + 1
}

var.df = data.frame(iso = paste0(ISO.vec, " (N = ", n.iso.vec, ")"), var = var.iso, n = n.iso.vec)

gg.iso.var = ggplot(var.df) + geom_point(aes(x = iso, y = var), size = 5) +
  theme_gray(base_size = 15) +
  xlab("Country ISO") + ylab("Variance of logit(FSW Proportion)") +
  theme(legend.title=element_blank(),
        axis.text = element_text(size = 22, face = "bold"),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.text = element_text(size = 22, face = "bold"))
gg.iso.var
ggsave("../Figures/Var_ISO_city.jpg", gg.iso.var, width = 20, height = 10)


## Repeat for just subnational
pse.obs.subnational = subset(pse.obs, City == 0)
ISO.vec = unique(pse.obs.subnational$ISO)
n.iso = length(ISO.vec)
var.iso = n.iso.vec = rep(NA, n.iso)
k = 1
for(i in ISO.vec){
  dat.iso.sub = subset(pse.obs.subnational, ISO == i)
  n.iso.vec[k] = nrow(dat.iso.sub)
  var.iso[k] = var(dat.iso.sub$logit_prev)
  k = k + 1
}

var.df = data.frame(iso = paste0(ISO.vec, " (N = ", n.iso.vec, ")"), var = var.iso, n = n.iso.vec)

gg.iso.var = ggplot(var.df) + geom_point(aes(x = iso, y = var), size = 5) +
  theme_gray(base_size = 15) +
  xlab("Country ISO") + ylab("Variance of logit(FSW Proportion)") +
  theme(legend.title=element_blank(),
        axis.text = element_text(size = 22, face = "bold"),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.text = element_text(size = 22, face = "bold"))
gg.iso.var
ggsave("../Figures/Var_ISO_subnational.jpg", gg.iso.var, width = 20, height = 10)



# Stepwise variable selection ---------------------------------------------

pse.obs$row.id = 1:nrow(pse.obs)
spatial.ind = unique(pse.obs$ID)
cov.out = cov.names
cov.in = c("1")
cov.out = cov.names
cov.out = c(cov.out, "City * ref_pop_log")
stop = FALSE
dic.final.vec = rep(NA, length(cov.names))
best.cov = list()
k = 1

## Need to do it using INLA instead of brms for computational necessity
uniq.iso = unique(pse.obs$ISO)
while(stop == FALSE){
  dic.vec = rep(NA, length(cov.out))
  for(i in 1:length(cov.out)){
    include = c(cov.in, cov.out[i])
    
    brms.form = as.formula(paste0("logit_prev ~ ",
                                  paste(include, collapse = " + "),
                                  "+ f(method_std, model = \"iid\") + f(ISO, model = \"iid\")"))
    
    n = sum(!is.na(pse.dat.fit$pse))
    loco.rmse = rep(NA, length(uniq.iso))
    j = 1
    for(ind in uniq.iso){ ## LOCO
      pse.train = pse.obs
      pse.train$logit_prev[pse.train$ISO == ind] = NA
      pse.test = subset(pse.obs, ISO == ind)
      
      brms.fit = inla(brms.form,
                      data = pse.train,
                      family = "gaussian",
                      control.predictor = list(compute = TRUE, link = 1))
      
      lodo.pred.vec = brms.fit$summary.fitted.values$mean[which(is.na(pse.train$logit_prev))]
      
      loco.rmse[j] = sqrt(mean((pse.test$logit_prev - lodo.pred.vec)^2, na.rm = T))
      j = j + 1
    }
    
    dic.vec[i] = mean(loco.rmse) ## Average RMSE over countries
    print(paste0("i/n:",i,"/",length(cov.out)))
  }
  
  
  
  if(min(dic.vec) > min(dic.final.vec, na.rm = T)){
    stop = TRUE
  }else{
    cov.in = c(cov.in,cov.out[which.min(dic.vec)])
    print(paste("Adding:", cov.out[which.min(dic.vec)]))
    cov.out = cov.out[-which.min(dic.vec)]
    best.cov[[j]] = cov.in
  }
  dic.final.vec[j] = min(dic.vec)
  j = j + 1
}

############################################################################
# This can be done using frequentist methods to save time
uniq.iso = unique(pse.obs$ISO)
while(stop == FALSE){
  dic.vec = rep(NA, length(cov.out))
  for(i in 1:length(cov.out)){
    include = c(cov.in, cov.out[i])
    
    brms.form = as.formula(paste0("logit_prev ~ ",
                                  paste(include, collapse = " + "),
                                  "+ (1|method_std) +",
                                  "(1|ISO)"))
    
    n = sum(!is.na(pse.dat.fit$pse))
    loco.rmse = rep(NA, length(uniq.iso))
    j = 1
    for(ind in uniq.iso){ ## LOCO
      pse.train = subset(pse.obs, ISO != ind)
      pse.test = subset(pse.obs, ISO == ind)
      
      brms.fit = lmer(brms.form,
                      data = pse.train,
                      control = lmerControl(
                        optimizer ='optimx', optCtrl=list(method='nlminb')))
      
      lodo.pred.vec = predict(brms.fit, newdata = pse.test,
                              allow.new.levels = TRUE)
      
      loco.rmse[j] = sqrt(mean((pse.test$logit_prev - lodo.pred.vec)^2, na.rm = T))
      j = j + 1
    }
    
    dic.vec[i] = mean(loco.rmse) ## Average RMSE over countries
    print(paste0("i/n:",i,"/",length(cov.out)))
  }
  
  if(min(dic.vec) > min(dic.final.vec, na.rm = T)){
    stop = TRUE
  }else{
    cov.in = c(cov.in,cov.out[which.min(dic.vec)])
    print(paste("Adding:", cov.out[which.min(dic.vec)]))
    cov.out = cov.out[-which.min(dic.vec)]
    best.cov[[k]] = cov.in
  }
  dic.final.vec[k] = min(dic.vec)
  k = k + 1
}


plot(pse.obs$worldpop_fcba_ratio, pse.obs$ref_pop_log)
tmp.city = subset(pse.obs, City == 1)
tmp.subnational = subset(pse.obs, City == 0)

par(mfrow = c(1,2))
plot(tmp.city$worldpop_fcba_ratio, tmp.city$ref_pop_log)
plot(tmp.subnational$worldpop_fcba_ratio, tmp.subnational$ref_pop_log)

#############################################################################
# Also do backwards stepwise regression to compare
cov.in = c(cov.names, "City * ref_pop_log")
uniq.iso = unique(pse.obs$ISO)
stop = FALSE
dic.final.vec.backwards = rep(NA, length(cov.names))
best.cov.backwards = list()
k = 1
while(stop == FALSE){
  include.list = list()
  dic.vec = rep(NA, length(cov.in))
  for(i in 1:length(cov.in)){
    include.list[[i]] = cov.in[-i]
    
    brms.form = as.formula(paste0("logit_prev ~ ",
                                  paste(include.list[[i]], collapse = " + "),
                                  "+ f(method_std, model = \"iid\") + f(ISO, model = \"iid\")"))
    
    n = sum(!is.na(pse.dat.fit$pse))
    loco.rmse = rep(NA, length(uniq.iso))
    j = 1
    for(ind in uniq.iso){ ## LOCO
      pse.train = pse.obs
      pse.train$logit_prev[pse.train$ISO == ind] = NA
      pse.test = subset(pse.obs, ISO == ind)
      
      brms.fit = inla(brms.form,
                      data = pse.train,
                      family = "gaussian",
                      control.predictor = list(compute = TRUE, link = 1))
      
      lodo.pred.vec = brms.fit$summary.fitted.values$mean[which(is.na(pse.train$logit_prev))]
      
      loco.rmse[j] = sqrt(mean((pse.test$logit_prev - lodo.pred.vec)^2, na.rm = T))
      j = j + 1
    }
    
    dic.vec[i] = mean(loco.rmse) ## Average RMSE over countries
    print(paste0("i/n:",i,"/",length(cov.in)))
  }
  
  if(k == 1){
    print(paste("Removing:", cov.in[which.min(dic.vec)]))
    cov.in = include.list[[which.min(dic.vec)]]
    best.cov.backwards[[k]] = cov.in
  }else if(min(dic.vec) > min(dic.final.vec.backwards, na.rm = T)){
    stop = TRUE
  }else{
    print(paste("Removing:", cov.in[which.min(dic.vec)]))
    cov.in = include.list[[which.min(dic.vec)]]
    best.cov.backwards[[k]] = cov.in
  }
  dic.final.vec.backwards[k] = min(dic.vec)
  k = k + 1
}



## Or via frequentist
cov.in = c(cov.names, "City * ref_pop_log")
uniq.iso = unique(pse.obs$ISO)
stop = FALSE
dic.final.vec.backwards = rep(NA, length(cov.names))
best.cov.backwards = list()
k = 1
while(stop == FALSE){
  include.list = list()
  dic.vec = rep(NA, length(cov.in))
  for(i in 1:length(cov.in)){
    include.list[[i]] = cov.in[-i]
    
    brms.form = as.formula(paste0("logit_prev ~ ",
                                  paste(include.list[[i]], collapse = " + "),
                                  "+ (1|method_std) +",
                                  "(1|ISO)"))
    
    n = sum(!is.na(pse.dat.fit$pse))
    loco.rmse = rep(NA, length(uniq.iso))
    j = 1
    for(ind in uniq.iso){ ## LOCO
      pse.train = subset(pse.obs, ISO != ind)
      pse.test = subset(pse.obs, ISO == ind)
      
      brms.fit = lmer(brms.form,
                      data = pse.train,
                      control = lmerControl(
                        optimizer ='optimx', optCtrl=list(method='nlminb')))
      
      lodo.pred.vec = predict(brms.fit, newdata = pse.test,
                              allow.new.levels = TRUE)
      
      loco.rmse[j] = sqrt(mean((pse.test$logit_prev - lodo.pred.vec)^2, na.rm = T))
      j = j + 1
    }
    
    dic.vec[i] = mean(loco.rmse) ## Average RMSE over countries
    print(paste0("i/n:",i,"/",length(cov.in)))
  }
  
  if(k == 1){
    print(paste("Removing:", cov.in[which.min(dic.vec)]))
    cov.in = include.list[[which.min(dic.vec)]]
    best.cov.backwards[[k]] = cov.in
  }else if(min(dic.vec) > min(dic.final.vec.backwards, na.rm = T)){
    stop = TRUE
  }else{
    print(paste("Removing:", cov.in[which.min(dic.vec)]))
    cov.in = include.list[[which.min(dic.vec)]]
    best.cov.backwards[[k]] = cov.in
  }
  dic.final.vec.backwards[k] = min(dic.vec)
  k = k + 1
}




# LODO sequence -----------------------------------------------------------
## Purpose:
# (1) Evaluate LOO Performance
# (2) Check for spatial random effect performance
# MCMC is far too slow for LOO, use INLA

## Model formulas
base.form = as.formula(paste0("logit_prev ~ City * ref_pop_log + urban + cropland_rainfed",
                              " + wc2.1_2.5m_elev_popweighted + walking_healthcare_popweighted +",
                              "f(method_std, model = \"iid\") + f(ISO, model = \"iid\")"))

spatial.form = as.formula(paste0("logit_prev ~ City * ref_pop_log + urban + cropland_rainfed",
                                 " + wc2.1_2.5m_elev_popweighted + walking_healthcare_popweighted +",
                                 "f(method_std, model = \"iid\") + f(ISO, model = \"iid\")",
                                 "+ f(ID, model = \"besag\", graph = nb.mat)"))


n = sum(!is.na(pse.dat.fit$pse))
base.pred = spatial.pred = district.pred = spatial.district.pred = rep(NA, n)
pse.dat.lodo = subset(pse.dat.fit, !is.na(pse))
spatial.ind = unique(pse.dat.lodo$ID)
j = 1
for(i in spatial.ind){
  pse.dat.lodo = subset(pse.dat.fit, !is.na(pse))
  pse.dat.lodo$ID.1 = pse.dat.lodo$ID
  pse.dat.lodo$logit_prev[which(pse.dat.lodo$ID == i)] = NA
  pse.dat.in = subset(pse.dat.lodo, !is.na(logit_prev))
  pse.dat.miss = subset(pse.dat.lodo, is.na(logit_prev))
  
  brms.base = inla(base.form,
                   data = pse.dat.lodo,
                   family = "gaussian",
                   control.predictor = list(compute = TRUE, link = 1))
  brms.spatial = inla(spatial.form,
                      data = pse.dat.lodo,
                      family = "gaussian",
                      control.predictor = list(compute = TRUE, link = 1))
  
  
  base.pred[which(pse.dat.lodo$ID == i)] = brms.base$summary.fitted.values$mean[which(is.na(pse.dat.lodo$logit_prev))]
  spatial.pred[which(pse.dat.lodo$ID == i)] = brms.spatial$summary.fitted.values$mean[which(is.na(pse.dat.lodo$logit_prev))]
  
  
  print(round(j/length(spatial.ind)*100))
  j = j + 1
}

pse.dat.lodo = subset(pse.dat.fit, !is.na(pse))

lodo.df.base = data.frame(pred.link = base.pred,
                          truth.link = pse.dat.lodo$logit_prev,
                          ISO = pse.dat.lodo$ISO,
                          size = pse.dat.lodo$ref_pop)

lodo.df.spatial = data.frame(pred.link = spatial.pred,
                             truth.link = pse.dat.lodo$logit_prev,
                             ISO = pse.dat.lodo$ISO,
                             size = pse.dat.lodo$ref_pop)

lodo.df.base$resid = lodo.df.base$truth.link - lodo.df.base$pred.link
lodo.df.spatial$resid = lodo.df.spatial$truth.link - lodo.df.spatial$pred.link

mean(lodo.df.base$resid^2, na.rm = T)
mean(lodo.df.spatial$resid^2, na.rm = T)

gg1 = ggplot(lodo.df.base) + geom_point(aes(x = pred.link, y = resid)) +
  geom_abline(slope = 0, intercept = 0, col = "black", lwd = 1.5) +
  xlab("Fitted Value (logit scale)") + ylab("Residual (Logit Scale)") +
  theme_grey(base_size = 15) + 
  theme(legend.position = "none") +
  ylim(-7.2, 3.4) + xlim(-7, -1)

gg2 = ggplot(lodo.df.spatial) + geom_point(aes(x = pred.link, y = resid)) +
  geom_abline(slope = 0, intercept = 0, col = "black", lwd = 1.5) +
  xlab("Fitted Value (logit scale)") + ylab("Residual (Logit Scale)") +
  theme_grey(base_size = 15) +
  ylim(-7.2, 3.4) + xlim(-7, -1)


# LOCO --------------------------------------------------------------------
## Do Leave one country out

n = sum(!is.na(pse.dat.fit$pse))
base.pred = spatial.pred = district.pred = spatial.district.pred = rep(NA, n)
pse.dat.loco = subset(pse.dat.fit, !is.na(pse))
spatial.ind = unique(pse.dat.loco$ID)
n.dist = length(unique(pse.dat.loco$ISO))
ISO.vec = unique(pse.dat.loco$ISO)
j = 1
for(i in 1:n.dist){
  pse.dat.loco = subset(pse.dat.fit, !is.na(pse))
  country.ind = which(pse.dat.loco$ISO == ISO.vec[i])
  pse.dat.loco$logit_prev[country.ind] = NA
  pse.dat.in = subset(pse.dat.loco, !is.na(logit_prev))
  pse.dat.miss = subset(pse.dat.loco, is.na(logit_prev))
  
  brms.base = inla(base.form,
                   data = pse.dat.loco,
                   family = "gaussian",
                   control.predictor = list(compute = TRUE, link = 1))
  brms.spatial = inla(spatial.form,
                      data = pse.dat.loco,
                      family = "gaussian",
                      control.predictor = list(compute = TRUE, link = 1))
  
  
  base.pred[country.ind] = brms.base$summary.fitted.values$mean[which(is.na(pse.dat.loco$logit_prev))]
  spatial.pred[country.ind] = brms.spatial$summary.fitted.values$mean[which(is.na(pse.dat.loco$logit_prev))]
  
  print(round(i/n.dist*100))
}

pse.dat.loco = subset(pse.dat.fit, !is.na(pse))

loco.df.base = data.frame(pred.link = base.pred,
                          truth.link = pse.dat.loco$logit_prev,
                          ISO = pse.dat.loco$ISO,
                          size = pse.dat.loco$ref_pop)

loco.df.spatial = data.frame(pred.link = spatial.pred,
                             truth.link = pse.dat.loco$logit_prev,
                             ISO = pse.dat.loco$ISO,
                             size = pse.dat.loco$ref_pop)


loco.df.base$resid = loco.df.base$truth.link - loco.df.base$pred.link
loco.df.spatial$resid = loco.df.spatial$truth.link - loco.df.spatial$pred.link

mean(loco.df.base$resid^2, na.rm = T)
mean(loco.df.spatial$resid^2, na.rm = T)

gg1 = ggplot(loco.df.base) + geom_point(aes(x = pred.link, y = resid, col = ISO)) +
  geom_abline(slope = 0, intercept = 0, col = "black", lwd = 1.5) +
  xlab("Fitted Value (logit scale)") + ylab("Residual (Logit Scale)") +
  theme_grey(base_size = 15) + 
  theme(legend.position = "none") +
  ylim(-8.2, 3.2) + xlim(-6, -1.9)

gg2 = ggplot(loco.df.spatial) + geom_point(aes(x = pred.link, y = resid, col = ISO)) +
  geom_abline(slope = 0, intercept = 0, col = "black", lwd = 1.5) +
  xlab("Reference Population") + ylab("Residual (Logit Scale)") +
  theme_grey(base_size = 15) +
  ylim(-8.2, 3.2) + xlim(-6, -1.9)



# Fit final model ---------------------------------------------------------
## Now we can use MCMC again
base.form = as.formula(paste0("logit_prev ~ City * ref_pop_log + urban + cropland_rainfed",
                              " + wc2.1_2.5m_elev_popweighted + walking_healthcare_popweighted +",
                              "(1|method_std) + (1|ISO)"))


which.not.na = which(!is.na(pse.dat.fit$pse))
which.na = which(is.na(pse.dat.fit$pse))
pse.dat.fit$City[-which.not.na] = 0
pse.dat.fit$ID[-which.not.na] = NA

pse.dat.obs = subset(pse.dat.fit, !is.na(prev))
pse.dat.miss = subset(pse.dat.fit, is.na(prev))

brms.base = brm(base.form, data = pse.dat.obs, cores = 3, chains = 3,
                iter = 6000, warmup = 4000,
                control = list(adapt_delta = 0.95))
brms.fitted = fitted(brms.base)


resid = pse.dat.obs$logit_prev - brms.fitted[,1]
plot(resid ~ brms.fitted[,1])
abline(h = 0, lwd = 2)
qqnorm(resid)
qqline(resid)

resid.df = data.frame(resid = resid, fitted = brms.fitted[,1])

min.val = min(resid.df)-0.2
max.val = max(resid.df)+0.2
## Look at diagnostics
gg.qq = ggplot(resid.df, aes(sample = resid)) + stat_qq(size = 2.5) +
  geom_qq_line(size = 1.5) +
  xlab("Theoretical Quantiles") + ylab("Sample Quantiles") +
  theme_grey(base_size = 15) + ggtitle("Normal distribution") +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        title = element_text(size = 22, face = "bold"),
        aspect.ratio = 1) +
  coord_cartesian(
    xlim = c(min.val, max.val),
    ylim = c(min.val, max.val)
  )
ggsave("../Figures/QQ_plot.jpg", gg.qq, width = 10, height = 10)

gg.resid = ggplot(resid.df) + geom_point(aes(x = fitted, y = resid)) +
  xlab("Fitted Values") + ylab("Residuals") +
  geom_abline(intercept = 0, slope = 0, size = 1) + 
  theme_grey(base_size = 15) +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        title = element_text(size = 22, face = "bold"),
        aspect.ratio = 1)
ggsave("../Figures/Resid_plot.jpg", gg.resid, width = 10, height = 10)

post.fitted = fitted(brms.base, newdata = pse.dat.miss, allow_new_levels = TRUE,
                     sample_new_levels = "gaussian", summary = FALSE,
                     re_formula = ~ (1|ISO)) ## Ignore method
post.transformed = plogis(post.fitted)
post.mean = colMeans(post.transformed)
quant.interval = apply(post.transformed, 2, quantile, probs = c(0.025, 0.975))

gadm.cov = readRDS("../Data/Shapefile_Prep.rds")
gadm.cov$Prev = gadm.cov$Fitted_FSW = gadm.cov$Lower = gadm.cov$Upper =
  gadm.cov$Uncertainty = NA


## Add results to gadm.cov
gadm.cov$Prev = post.mean 
gadm.cov$Fitted_FSW = post.mean * pse.dat.fit$ref_pop[which.na]
gadm.cov$Lower = quant.interval[1,]
gadm.cov$Upper = quant.interval[2,]
gadm.cov$Uncertainty = (gadm.cov$Upper - gadm.cov$Lower) / gadm.cov$Prev
gadm.country = raster::aggregate(gadm.cov, by = c("ISO", "NAME_0"),
                                 sums = list(list(function(x){sum(x, na.rm = T)}, "ref_pop")))

## Find Fitted FSW for country
gadm.country$Fitted_FSW = gadm.country$Lower = gadm.country$Upper = 
  gadm.country$Uncertainty = NA
country.fitted.fsw.mat = matrix(NA, nrow = nrow(gadm.country), ncol = nrow(post.transformed))
for(i in 1:nrow(gadm.country)){
  country.ind = which(gadm.cov$ISO == gadm.country$ISO[i])
  ref.pops = gadm.cov$ref_pop[country.ind]
  tmp.prev = post.transformed[,country.ind]
  for(j in 1:ncol(tmp.prev)){
    tmp.prev[,j] = tmp.prev[,j] * ref.pops[j]
  }
  country.post.samples = rowSums(tmp.prev)
  gadm.country$Fitted_FSW[i] = mean(country.post.samples)
  gadm.country$Lower[i] = quantile(country.post.samples, probs = c(0.025)) / gadm.country$ref_pop[i]
  gadm.country$Upper[i] = quantile(country.post.samples, probs = c(0.975)) / gadm.country$ref_pop[i]
}
gadm.country$Prev = gadm.country$Fitted_FSW / gadm.country$ref_pop
gadm.country$Uncertainty = (gadm.country$Upper - gadm.country$Lower) / gadm.country$Prev
gadm.country$Percent = gadm.country$Prev * 100
gadm.country@data

gadm.country@data[order(gadm.country$Percent),]



## Find FSW estimate for methods
pse.dat.cov.effect = data.frame(City = 0,
                                ref_pop_log = 0,
                                urban = 0,
                                cropland_rainfed = 0,
                                wc2.1_2.5m_elev_popweighted = 0,
                                walking_healthcare_popweighted = 0,
                                method_std = c("CRC", "Enumeration", "Expert", "Mapping",
                                               "Misc", "Multiple", "Multiplier", "NR"))
cov.effect = fitted(brms.base, newdata = pse.dat.cov.effect, allow_new_levels = F,
                    sample_new_levels = "gaussian", summary = FALSE,
                    re_formula = ~ (1 | method_std)) ## Ignore method and ISO

cbind(pse.dat.cov.effect, round(100 * colMeans(plogis(cov.effect)), digits = 2))

cov.effect = fitted(brms.base, newdata = pse.dat.cov.effect, allow_new_levels = F,
                    sample_new_levels = "gaussian", summary = FALSE,
                    re_formula = NA) ## Ignore method and ISO

cbind(pse.dat.cov.effect, round(100 * colMeans(plogis(cov.effect)), digits = 2))

## Find FSW estimate different for covariates
## Create plots for this effect
cov.effect.df = matrix(NA, nrow = 0, ncol = 9)
pse.dat.cov = data.frame(City = 0,
                         ref_pop_log = seq(-2, 2, length.out = 1000),
                         urban = 0,
                         cropland_rainfed = 0,
                         wc2.1_2.5m_elev_popweighted = 0,
                         walking_healthcare_popweighted = 0,
                         value = seq(-2, 2, length.out = 1000),
                         variable = "Log Reference Population")
cov.effect = fitted(brms.base, newdata = pse.dat.cov, allow_new_levels = TRUE,
                    sample_new_levels = "gaussian", summary = FALSE,
                    re_formula = NA) ## Ignore method and ISO
cov.effect.df = rbind(cov.effect.df, cbind(pse.dat.cov, colMeans(plogis(cov.effect))))


pse.dat.cov = data.frame(City = 0,
                         ref_pop_log = 0,
                         urban = seq(-2, 2, length.out = 1000),
                         cropland_rainfed = 0,
                         wc2.1_2.5m_elev_popweighted = 0,
                         walking_healthcare_popweighted = 0,
                         value = seq(-2, 2, length.out = 1000),
                         variable = "Urban")
cov.effect = fitted(brms.base, newdata = pse.dat.cov, allow_new_levels = TRUE,
                    sample_new_levels = "gaussian", summary = FALSE,
                    re_formula = NA) ## Ignore method and ISO
cov.effect.df = rbind(cov.effect.df, cbind(pse.dat.cov, colMeans(plogis(cov.effect))))

pse.dat.cov = data.frame(City = 0,
                         ref_pop_log = 0,
                         urban = 0,
                         cropland_rainfed = seq(-2, 2, length.out = 1000),
                         wc2.1_2.5m_elev_popweighted = 0,
                         walking_healthcare_popweighted = 0,
                         value = seq(-2, 2, length.out = 1000),
                         variable = "Rainfed Cropland")
cov.effect = fitted(brms.base, newdata = pse.dat.cov, allow_new_levels = TRUE,
                    sample_new_levels = "gaussian", summary = FALSE,
                    re_formula = NA) ## Ignore method and ISO
cov.effect.df = rbind(cov.effect.df, cbind(pse.dat.cov, colMeans(plogis(cov.effect))))

pse.dat.cov = data.frame(City = 0,
                         ref_pop_log = 0,
                         urban = 0,
                         cropland_rainfed = 0,
                         wc2.1_2.5m_elev_popweighted = seq(-2, 2, length.out = 1000),
                         walking_healthcare_popweighted = 0,
                         value = seq(-2, 2, length.out = 1000),
                         variable = "Elevation")
cov.effect = fitted(brms.base, newdata = pse.dat.cov, allow_new_levels = TRUE,
                    sample_new_levels = "gaussian", summary = FALSE,
                    re_formula = NA) ## Ignore method and ISO
cov.effect.df = rbind(cov.effect.df, cbind(pse.dat.cov, colMeans(plogis(cov.effect))))

pse.dat.cov = data.frame(City = 0,
                         ref_pop_log = 0,
                         urban = 0,
                         cropland_rainfed = 0,
                         wc2.1_2.5m_elev_popweighted = 0,
                         walking_healthcare_popweighted = seq(-2, 2, length.out = 1000),
                         value = seq(-2, 2, length.out = 1000),
                         variable = "Walking to Healthcare")
cov.effect = fitted(brms.base, newdata = pse.dat.cov, allow_new_levels = TRUE,
                    sample_new_levels = "gaussian", summary = FALSE,
                    re_formula = NA) ## Ignore method and ISO
cov.effect.df = rbind(cov.effect.df, cbind(pse.dat.cov, colMeans(plogis(cov.effect))))

cov.effect.df = data.frame(cov.effect.df)
names(cov.effect.df) = c(names(pse.dat.cov), "Estimate")
cov.effect.df$Estimate = cov.effect.df$Estimate * 100


cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")
gg.cov.eff = ggplot(cov.effect.df) +
  geom_line(aes(x = value, y = Estimate, group = variable, col = variable), size = 2) +
  xlab("Standardized Variable") + ylab("FSW Percent Estimate") +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        title = element_text(size = 22, face = "bold"),
        aspect.ratio = 1) +
  scale_y_continuous(breaks = round(seq(min(cov.effect.df$Estimate), max(cov.effect.df$Estimate), by = 0.1),1)) +
  scale_color_manual(name = "Covariate", values = cbPalette)
gg.cov.eff

ggsave("../Figures/Covariate_effect_lines.jpg", plot = gg.cov.eff, height = 10, width = 15)












# Save Results ------------------------------------------------------------


save.image("../Data/Results/Final_Results.RData")

## Remove some columns from gadm.cov
gadm.cov@data = gadm.cov@data[,c("ID", "ISO", "NAME_0", "NAME_1", "ref_pop",
                                 "Uncertainty", "Upper", "Lower", "Fitted_FSW", "Prev")]
saveRDS(gadm.cov, "../Data/Results/Final_district_est.rds")
saveRDS(gadm.country, "../Data/Results/Final_country_est.rds")
saveRDS(brms.base, "../Data/Results/Final_model_fit.rds")
write.csv(gadm.cov@data, "../Data/Results/Final_district_est.csv")
write.csv(gadm.country@data, "../Data/Results/Final_country_est.csv")







###########################################################################
###########################################################################
## This ends the main results.
## The following code is additional diagnostics and secondary analysis
###########################################################################
###########################################################################


## Calculate Variance Inflation Factor using Frequentist Model (VIF)
library(car)
freq.fit = lmer(base.form, data = pse.dat.obs)
vif(freq.fit)



library(GGally)
# Plot covariates against each other
scaleFUN <- function(x) sprintf("%.0f", x)
gg.cov.pairs = ggpairs(pse.dat.miss[,c("ref_pop_log", "urban", "cropland_rainfed",
                                       "wc2.1_2.5m_elev_popweighted", "walking_healthcare_popweighted")],
                       columnLabels = c("Log Ref Pop", "% Urban", "% Rain Crop",
                                        "Elevation", "Healthcare"),
                       upper = list(continuous = wrap("cor", size=12))) +
  theme(axis.text.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        strip.text.x = element_text(size = 23),
        strip.text.y = element_text(size = 23)) +
  scale_y_continuous(labels=scaleFUN) +
  scale_x_continuous(labels=scaleFUN)


ggsave("../Figures/Covariate_pairwise.jpg", plot = gg.cov.pairs, height = 12, width = 12)



# Get some plots of Method ------------------------------------------------
tmp = pse.dat.obs
tmp$method_std = factor(tmp$method_std)
tmp$method_std = factor(substr(as.character(tmp$method_std), 1, 50))
tmp$method_std = as.character(tmp$method_std)
method.table = table(tmp$method_std)
n.NA = sum(is.na(tmp$method_std))
method.n = as.numeric(table(tmp$method_std))
for(i in 1:nrow(tmp)){
  tmp.ind = which(names(method.table) == as.character(tmp$method_std[i]))
  if(length(tmp.ind) > 0){
    tmp$method_std[i] = paste0(as.character(tmp$method_std[i]), " (N = ", method.table[tmp.ind], ")")
  }else{ ## The NA's
    tmp$method_std[i] = paste0(as.character(tmp$method_std[i]), " (N = ", n.NA, ")")
  }
}
tmp$method_std = factor(tmp$method_std, levels = c("CRC (N = 53)", "Enumeration (N = 82)",
                                                   "Expert (N = 20)", "Mapping (N = 374)",
                                                   "Misc (N = 32)", "Multiple (N = 112)",
                                                   "Multiplier (N = 49)",
                                                   "NR (N = 210)"))
gg1 = ggplot(tmp, aes(x = logit_prev, y = factor(method_std))) + geom_boxplot() +
  ylab("Method") + xlab("Logit-transformed FSW proportion") +
  scale_y_discrete(labels = c("CRC (N = 53)", "Enumeration (N = 82)",
                              "Expert (N = 20)", "Mapping (N = 374)",
                              "Misc (N = 32)", "Multiple (N = 112)",
                              "Multiplier (N = 49)",
                              "NR (N = 210)")) +
  theme_grey(base_size = 15) +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 25, face = "bold"),
        axis.title.y = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        title = element_text(size = 22, face = "bold"))
gg1
ggsave("../Figures/Method_boxplots.jpg", plot = gg1, height = 10, width = 20)






# Rank country level random effects ---------------------------------------

country.ranef = ranef(brms.base)$ISO
country.ranef[order(country.ranef[,1,1]),,1]
hist(country.ranef[,1,1], breaks = 10)


# Rank estimated fixed effects --------------------------------------------
## Here we look at country FSW proportion from the predictors only
post.fixed = fitted(brms.base, newdata = pse.dat.miss, allow_new_levels = TRUE,
                    sample_new_levels = "gaussian", summary = FALSE,
                    re_formula = NA) ## Ignore method
post.fixed.transformed = plogis(post.fixed)
post.fixed.mean = colMeans(post.fixed.transformed)

gadm.cov.fixed = gadm.cov
gadm.cov.fixed$Prev = post.fixed.mean 
gadm.cov.fixed$Fitted_FSW = post.fixed.mean * gadm.cov.fixed$ref_pop
gadm.country.fixed = raster::aggregate(gadm.cov.fixed, by = c("ISO", "NAME_0"),
                                       sums = list(list(function(x){sum(x, na.rm = T)}, "ref_pop")))

## Find Fitted FSW for country
gadm.country.fixed$Fitted_FSW = NA
country.fitted.fsw.mat = matrix(NA, nrow = nrow(gadm.country.fixed), ncol = nrow(post.transformed))
for(i in 1:nrow(gadm.country.fixed)){
  country.ind = which(gadm.cov.fixed$ISO == gadm.country.fixed$ISO[i])
  ref.pops = gadm.cov.fixed$ref_pop[country.ind]
  tmp.prev = post.fixed.transformed[,country.ind]
  for(j in 1:ncol(tmp.prev)){
    tmp.prev[,j] = tmp.prev[,j] * ref.pops[j]
  }
  country.post.samples = rowSums(tmp.prev)
  gadm.country.fixed$Fitted_FSW[i] = mean(country.post.samples)
}
gadm.country.fixed$Prev = gadm.country.fixed$Fitted_FSW / gadm.country.fixed$ref_pop
gadm.country.fixed$Percent = gadm.country.fixed$Prev * 100
gadm.country.fixed@data[order(gadm.country.fixed@data$Percent),]

cbind(gadm.country@data, gadm.country.fixed$Percent,
      rank(gadm.country$Percent), rank(gadm.country.fixed$Percent))

cbind(country.ranef[,1,1],rank(country.ranef[,1,1]))

gadm.country$Predictor_only = gadm.country.fixed$Percent
gadm.country$Predictor_only_rank = rank(gadm.country.fixed$Percent)
write.csv(gadm.country@data, "../Data/Results/Final_country_est.csv")



# Variance decomposition --------------------------------------------------

library(lmerTest) ## Have to load lmerTest for ANOVA Tables for Linear Mixed Models
freq.fit = lmer(logit_prev ~ City * ref_pop_log + urban +
                  cropland_rainfed + walking_healthcare_popweighted +
                  wc2.1_2.5m_elev_popweighted +
                  (1|method_std) + (1|ISO), data = pse.dat.obs)
summary(freq.fit)
anova(freq.fit)
ranova(freq.fit)

library(MuMIn)
r.squaredGLMM(freq.fit)


## Decompose R^2 for each random effect
fitted <- (model.matrix(freq.fit) %*% 
             fixef(freq.fit))[, 1L]
varFE <- var(fitted)

## Fixed effect
varFE / (varFE +
           sum(unlist(summary(freq.fit)$varcor)) +
           summary(freq.fit)$sigma^2)

## Random effects only
sum(unlist(summary(freq.fit)$varcor)) / (varFE +
                                           sum(unlist(summary(freq.fit)$varcor)) +
                                           summary(freq.fit)$sigma^2)

## ISO
unlist(summary(freq.fit)$varcor)[1] / (varFE +
                                         sum(unlist(summary(freq.fit)$varcor)) +
                                         summary(freq.fit)$sigma^2)

## Method
unlist(summary(freq.fit)$varcor)[2] / (varFE +
                                         sum(unlist(summary(freq.fit)$varcor)) +
                                         summary(freq.fit)$sigma^2)

# Look at variance of repeated measures -----------------------------------

repeated.id = as.numeric(names(table(pse.dat.obs$ID)[which(table(pse.dat.obs$ID) >= 3)]))

repeated.var = repeated.n = rep(NA, length(repeated.id))
for(ind in 1:length(repeated.id)){
  tmp = subset(pse.dat.obs, ID == repeated.id[ind])
  repeated.n[ind] = nrow(tmp)
  repeated.var[ind] = var(tmp$logit_prev)
}
summary(repeated.var)
sum((repeated.n - 1) * repeated.var) / (sum(repeated.n - 1))
sum((repeated.n - 1) * repeated.var) / (sum(repeated.n - 1)) /
  summary(freq.fit)$sigma^2


repeated.dat = subset(pse.dat.obs, ID %in% repeated.id)
repeated.fit = lmer(logit_prev ~ City * ref_pop_log + urban + mosaic_cropland + walking_healthcare_popweighted +
                      (1|method_std) + (1|ID),
                    data = repeated.dat)
r.squaredGLMM(repeated.fit)




# LOCO for regression coefficients ----------------------------------------



## Get intervals df for coef, method, and ISO
fixef.full = fixef(brms.base)
coef.full.df = data.frame(
  variable = rownames(fixef.full),
  est = fixef.full[, "Estimate"],
  lower = fixef.full[, "Q2.5"],
  upper = fixef.full[, "Q97.5"]
)

method.full = ranef(brms.base)$method_std
method.full.df = data.frame(
  variable = rownames(method.full),
  est = method.full[, "Estimate", 1],
  lower = method.full[, "Q2.5", 1],
  upper = method.full[, "Q97.5", 1]
)


iso.full = ranef(brms.base)$ISO
iso.full.df = data.frame(
  variable = rownames(iso.full),
  est = iso.full[, "Estimate", 1],
  lower = iso.full[, "Q2.5", 1],
  upper = iso.full[, "Q97.5", 1]
)

## Now do LOO
n = sum(!is.na(pse.dat.fit$pse))
pse.dat.loco = subset(pse.dat.fit, !is.na(pse))
n.iso = length(unique(pse.dat.loco$ISO))
ISO.vec = unique(pse.dat.loco$ISO)
j = 1
fixef.loco = matrix(NA, nrow = n.iso, ncol = 8)
method.loco = matrix(NA, nrow = n.iso, ncol = 8)
iso.loco = matrix(NA, nrow = n.iso, ncol = n.iso)
for(i in 31:n.iso){
  pse.dat.loco = subset(pse.dat.fit, !is.na(pse))
  country.ind = which(pse.dat.loco$ISO == ISO.vec[i])
  pse.dat.loco$logit_prev[country.ind] = NA
  pse.dat.in = subset(pse.dat.loco, !is.na(logit_prev))
  pse.dat.miss = subset(pse.dat.loco, is.na(logit_prev))
  
  brms.fit = brm(base.form, data = pse.dat.in, cores = 3, chains = 3,
                 iter = 6000, warmup = 4000,
                 control = list(adapt_delta = 0.95))

  fixef.loco[i,] = fixef(brms.fit)[,"Estimate"]
  method.loco[i,] = ranef(brms.fit)$method_std[,"Estimate", 1]
  iso.loco[i,-i] = ranef(brms.fit)$ISO[,"Estimate", 1]
  
  ## Can comment above and uncomment below for frequentist fit
  # freq.fit = lmer(base.form,
  #                 data = pse.dat.in,
  #                 control = lmerControl(
  #                   optimizer ='optimx', optCtrl=list(method='nlminb')))
  # 
  # fixef.loco[i,] = fixef(freq.fit)
  # method.loco[i,] = unlist(ranef(freq.fit)$method_std)
  # iso.loco[i,-i] = unlist(ranef(freq.fit)$ISO)
  
  print(round(i/n.iso*100))
}

colnames(fixef.loco) = rownames(fixef(brms.fit))
colnames(method.loco) = rownames(ranef(brms.fit)$method_std)
colnames(iso.loco) = ISO.vec

fixef.loco.melt = reshape2::melt(fixef.loco)
method.loco.melt = reshape2::melt(method.loco)
iso.loco.melt = reshape2::melt(iso.loco)
iso.loco.melt = subset(iso.loco.melt, Var2 %in% ISO.vec)

fixef.loco.melt$lower = fixef.loco.melt$upper = fixef.loco.melt$est = NA
for(i in 1:nrow(fixef.loco.melt)){
  fixef.loco.melt$lower[i] = coef.full.df$lower[which(coef.full.df$variable == fixef.loco.melt$Var2[i])]
  fixef.loco.melt$upper[i] = coef.full.df$upper[which(coef.full.df$variable == fixef.loco.melt$Var2[i])]
  fixef.loco.melt$est[i] = coef.full.df$est[which(coef.full.df$variable == fixef.loco.melt$Var2[i])]
}

method.loco.melt$lower = method.loco.melt$upper = method.loco.melt$est = NA
for(i in 1:nrow(method.loco.melt)){
  method.loco.melt$lower[i] = method.full.df$lower[which(method.full.df$variable == method.loco.melt$Var2[i])]
  method.loco.melt$upper[i] = method.full.df$upper[which(method.full.df$variable == method.loco.melt$Var2[i])]
  method.loco.melt$est[i] = method.full.df$est[which(method.full.df$variable == method.loco.melt$Var2[i])]
}

iso.loco.melt$lower = iso.loco.melt$upper = iso.loco.melt$est = NA
for(i in 1:nrow(iso.loco.melt)){
  iso.loco.melt$lower[i] = iso.full.df$lower[which(iso.full.df$variable == iso.loco.melt$Var2[i])]
  iso.loco.melt$upper[i] = iso.full.df$upper[which(iso.full.df$variable == iso.loco.melt$Var2[i])]
  iso.loco.melt$est[i] = iso.full.df$est[which(iso.full.df$variable == iso.loco.melt$Var2[i])]
}
fixef.loco.melt$Var2 = as.character(fixef.loco.melt$Var2)

fixef.loco.melt$Var2[fixef.loco.melt$Var2 == "ref_pop_log"] = "Log Ref Pop"
fixef.loco.melt$Var2[fixef.loco.melt$Var2 == "urban"] = "% Urban"
fixef.loco.melt$Var2[fixef.loco.melt$Var2 == "cropland_rainfed"] = "% Rain Crop"
fixef.loco.melt$Var2[fixef.loco.melt$Var2 == "wc2.1_2.5m_elev_popweighted"] = "Elevation"
fixef.loco.melt$Var2[fixef.loco.melt$Var2 == "walking_healthcare_popweighted"] = "Healthcare"
fixef.loco.melt$Var2[fixef.loco.melt$Var2 == "City:ref_pop_log"] = "City:Log Ref Pop"

fixef.loco.melt$Var2 = factor(fixef.loco.melt$Var2, levels = c("Intercept", "City", "Log Ref Pop",
                                                               "City:Log Ref Pop", "% Urban", "% Rain Crop",
                                                               "Elevation", "Healthcare"))

gg.fixef = ggplot(fixef.loco.melt) +
  geom_errorbar(
    mapping = aes(
      x = Var2,
      ymin = lower,
      ymax = upper,
      group = Var2
    ),
    width = 0.2,
    size = 1,
    col = "blue",
  ) +
  geom_boxplot(aes(y = value, x = Var2, group = Var2), varwidth = TRUE, outlier.size = 4, outlier.shape = 18) +
  geom_point(aes(x = Var2, y = est), col = "blue", size = 5) +
  facet_wrap(~Var2, scales = "free") +
  theme_grey(base_size = 15) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 23),
        title = element_text(size = 22, face = "bold")) +
  ylab("Estimate") + xlab("")
gg.fixef

gg.method.eff = ggplot(method.loco.melt) +
  geom_errorbar(
    mapping = aes(
      x = Var2,
      ymin = lower,
      ymax = upper,
      group = Var2
    ),
    width = 0.2,
    size = 1,
    col = "blue",
  ) +
  geom_boxplot(aes(y = value, x = Var2, group = Var2), varwidth = TRUE, outlier.size = 4, outlier.shape = 18) +
  geom_point(aes(x = Var2, y = est), col = "blue", size = 5) +
  theme_grey(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 23),
        title = element_text(size = 22, face = "bold")) +
  ylab("Estimate") + xlab("Method")
gg.method.eff

gg.iso.eff = ggplot(iso.loco.melt) + 
  geom_errorbar(
    mapping = aes(
      x = Var2,
      ymin = lower,
      ymax = upper,
      group = Var2
    ),
    width = 0.4,
    size = 1,
    col = "blue",
  ) +
  geom_boxplot(aes(y = value, x = Var2, group = Var2), varwidth = TRUE, outlier.size = 3, outlier.shape = 18) +
  geom_point(aes(x = Var2, y = est), col = "blue", size = 3) +
  theme_grey(base_size = 15) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 23),
        title = element_text(size = 22, face = "bold")) +
  ylab("Estimate") + xlab("Method")
gg.iso.eff


ggsave("../Figures/LOCO_fixef.jpg", gg.fixef, width = 10, height = 10)
ggsave("../Figures/LOCO_method.jpg", gg.method.eff, width = 10, height = 10)
ggsave("../Figures/LOCO_ISO.jpg", gg.iso.eff, width = 15, height = 10)





# Compare predictive performance of full model to restricted --------------

full.form = as.formula(paste0("logit_prev ~ City * ref_pop_log + urban + cropland_rainfed",
                              " + wc2.1_2.5m_elev_popweighted + walking_healthcare_popweighted +",
                              "(1|method_std) + (1|ISO)"))

reduced.form = as.formula(paste0("logit_prev ~ City * ref_pop_log + urban +",
                                 "(1|method_std) + (1|ISO)"))

n = sum(!is.na(pse.dat.fit$pse))
full.rmse = reduced.rmse = rep(NA, length(uniq.iso))
j = 1
for(ind in uniq.iso){ ## LOCO
  pse.train = subset(pse.obs, ISO != ind)
  pse.test = subset(pse.obs, ISO == ind)
  
  
  full.fit = brm(base.form, data = pse.train, cores = 3, chains = 3,
                 iter = 6000, warmup = 4000,
                 control = list(adapt_delta = 0.95))

  reduced.fit = brm(reduced.form, data = pse.train, cores = 3, chains = 3,
                 iter = 6000, warmup = 4000,
                 control = list(adapt_delta = 0.95))
  
  full.pred.vec = fitted(full.fit, newdata = pse.test, allow_new_levels = TRUE,
                         sample_new_levels = "gaussian", summary = TRUE)[,"Estimate"]
  
  reduced.pred.vec = fitted(reduced.fit, newdata = pse.test, allow_new_levels = TRUE,
                         sample_new_levels = "gaussian", summary = TRUE)[,"Estimate"]
  
  full.rmse[j] = sqrt(mean((pse.test$logit_prev - full.pred.vec)^2))
  reduced.rmse[j] = sqrt(mean((pse.test$logit_prev - reduced.pred.vec)^2))
  
  
  ## Can comment above an uncomment below for frequentist fit
  # full.fit = lmer(base.form,
  #                 data = pse.train,
  #                 control = lmerControl(
  #                   optimizer ='optimx', optCtrl=list(method='nlminb')))
  # 
  # reduced.fit = lmer(reduced.form,
  #                    data = pse.train,
  #                    control = lmerControl(
  #                      optimizer ='optimx', optCtrl=list(method='nlminb')))
  # 
  # full.pred.vec = predict(full.fit, newdata = pse.test,
  #                         allow.new.levels = TRUE)
  # 
  # reduced.pred.vec = predict(reduced.fit, newdata = pse.test,
  #                            allow.new.levels = TRUE)
  # 
  # full.rmse[j] = sqrt(mean((pse.test$logit_prev - full.pred.vec)^2))
  # reduced.rmse[j] = sqrt(mean((pse.test$logit_prev - reduced.pred.vec)^2))

  
  j = j + 1
  
  print(j)
}

boxplot(data.frame(full = full.rmse, reduced = reduced.rmse))
mean(full.rmse)
mean(reduced.rmse)

## This is the value reported in the supplementary information
(mean(full.rmse) - mean(reduced.rmse)) / mean(full.rmse) * 100
