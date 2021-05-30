# Find highest correlation variables --------------------------------------
library(raster)
library(mice)
library(MASS)
library(INLA)
library(spdep)

## Redo modeling, but with DHS

library(raster)
library(spdep)
library(plyr)
library(tmap)
library(lme4)
library(ggplot2)
library(brms)
library(optimx)


rm(list=ls())
pse.dat.fit = readRDS("../Data/PSE_Prep.rds")
gadm.cov = readRDS("../Data/Shapefile_Prep.rds")
nb.mat = readRDS("../Data/nb_mat_Prep.rds")


## Combined pse and gadm.cov
pse.dat.fit$ref_pop = pse.dat.fit$ref_pop_joined
## Remove large Nairobi estimate
which.rm = which(pse.dat.fit$Prev_comb > 0.6 &
                   pse.dat.fit$fsw_sizeestimate_region == "Nairobi") 
pse.dat.fit = pse.dat.fit[-which.rm,]
n.obs = nrow(pse.dat.fit)

if(1){ ## Include rest of gadm.cov
  combined = rbind.fill(pse.dat.fit, gadm.cov@data)
  combined$ref_pop = round(combined$ref_pop)
}else{
  combined = pse.dat.fit
  combined$ref_pop = round(combined$ref_pop)
}

pse.dat.fit = combined[,c("pse", "ref_pop", "ID", "method_std",
                          "ISO", "NAME_1", "City",
                          names(combined)[c(52:389)])]
pse.dat.fit = pse.dat.fit[,-which(names(pse.dat.fit) == "ID.1")]
pse.dat.fit = subset(pse.dat.fit, !is.na(pse))

## Remove non-popweighted variables
names(pse.dat.fit)[234 + c(8,9,17,18,24,32,33:56,81,83,85,87,89,91,93,95,103,105,107,109)]
rm.vec = 234 + c(8,9,17,18,24,32,33:56,81,83,85,87,89,91,93,95,103,105,107,109)
pse.dat.fit = pse.dat.fit[,-rm.vec]

DHS.ind = 8:241
## Remove variable with fewer than 15 unique values
num.unique = apply(pse.dat.fit[,DHS.ind], 2, function(x){length(unique(x))})
which.rm = which(num.unique < 15) + 7
pse.dat.fit = pse.dat.fit[,-which.rm]
DHS.ind = 8:213
## Identify 30 covariates with the highest correlation
pse.dat.fit$prev = pse.dat.fit$pse / pse.dat.fit$ref_pop
pse.dat.fit$logit_prev = qlogis(pse.dat.fit$prev)
DHS.cor.vec = cor(pse.dat.fit$logit_prev, pse.dat.fit[,DHS.ind], use = "pairwise.complete.obs")
which.rm = 7 + which(rank(-abs(DHS.cor.vec)) > 30)
pse.dat.fit = pse.dat.fit[,-which.rm]
DHS.ind = 8:37

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


## Now scale
cov.names = names(pse.dat.fit)[c(8:48)]
## Remove correlated variables

cov.names = c(names(pse.dat.fit)[DHS.ind], "urban", "cropland_rainfed",
                              "wc2.1_2.5m_elev_popweighted", "walking_healthcare_popweighted")
pse.sub = subset(pse.dat.fit, !is.na(pse.dat.fit$pse))
pse.pred = subset(pse.dat.fit, is.na(pse.dat.fit$pse))













pse.obs = subset(pse.dat.fit, !is.na(pse))
pse.obs = pse.obs[,-6]
pse.obs = pse.obs[complete.cases(pse.obs),]
pse.obs[,c(7:100)] = scale(pse.obs[,c(7:100)])
pse.obs$row.id = 1:nrow(pse.obs)
spatial.ind = unique(pse.obs$ID)
cov.out = cov.names
## Decide what to keep (otherwise c())
cov.in = c("City", "ref_pop_log", "City * ref_pop_log")
stop = FALSE
dic.final.vec = rep(NA, length(cov.out))
best.cov = list()
k = 1

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



# Can also be doing using Frequentist
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
    
    dic.vec[i] = mean(loco.rmse)
    print(paste0("i/n:",i,"/",length(cov.out)))
  }
  
  
  if(min(dic.vec, na.rm = T) > min(dic.final.vec, na.rm = T)){
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

cov.in.dhs.and.aux = cov.in

## Also select just DHS
cov.out = cov.names[1:30]
## Decide what to keep (otherwise c())
cov.in = c("City", "ref_pop_log", "City * ref_pop_log")
stop = FALSE
dic.final.vec = rep(NA, length(cov.out))
best.cov = list()
k = 1



# INLA --------------------------------------------------------------------

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
  
  
  if(min(dic.vec, na.rm = T) > min(dic.final.vec, na.rm = T)){
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

# Frequentist -------------------------------------------------------------
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
    
    dic.vec[i] = mean(loco.rmse)
    print(paste0("i/n:",i,"/",length(cov.out)))
  }
  
  
  if(min(dic.vec, na.rm = T) > min(dic.final.vec, na.rm = T)){
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

cov.in.dhs.only = cov.in

save.image("../Data/DHS/Selected_DHS_with_aux.RData")


dhs.only.form = as.formula(paste0("logit_prev ~ ",
                             paste(cov.in.dhs.only, collapse = " + "),
                             "+ (1|method_std) +",
                             "(1|ISO)"))

dhs.form = as.formula(paste0("logit_prev ~ ",
                             paste(cov.in.dhs.and.aux, collapse = " + "),
                             "+ (1|method_std) +",
                             "(1|ISO)"))

nondhs.form = as.formula(paste0("logit_prev ~ City * ref_pop_log + urban + cropland_rainfed",
                                " + wc2.1_2.5m_elev_popweighted + walking_healthcare_popweighted +",
                                "(1|method_std) + (1|ISO)"))


dhsonly.full.fit = lmer(dhs.only.form, data = pse.obs)
dhs.full.fit = lmer(dhs.form, data = pse.obs)
nondhs.full.fit = lmer(nondhs.form, data = pse.obs)

library(MuMIn)
r.squaredGLMM(dhsonly.full.fit)
r.squaredGLMM(dhs.full.fit)
r.squaredGLMM(nondhs.full.fit)

## Residuals
plot(fitted(dhs.full.fit), resid(dhs.full.fit))
plot(fitted(nondhs.full.fit), resid(nondhs.full.fit))

plot(fitted(dhs.full.fit), fitted(dhsonly.full.fit))
plot(fitted(dhs.full.fit), fitted(nondhs.full.fit))
plot(fitted(nondhs.full.fit), fitted(dhsonly.full.fit))

plot(resid(dhs.full.fit), resid(nondhs.full.fit))

tmp.df = data.frame(dhs = fitted(dhs.full.fit), nondhs = fitted(nondhs.full.fit))
fitted.gg = ggplot(tmp.df) + geom_point(aes(x = dhs, y = nondhs)) +
  geom_abline(slope = 1, intercept = 0, size = 1.5) + 
  xlab("Combined Model") + ylab("Auxiliary Model") +
  theme_gray(base_size = 15) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 22, face = "bold"))
fitted.gg
ggsave("./Figures/Fitted_dhs.jpg", fitted.gg, width = 20, height = 10)


## Check LODO
n = sum(!is.na(pse.dat.fit$pse))
lodo.rmse.dhs = lodo.rmse.nondhs = lodo.rmse.dhsonly = rep(NA, length(unique(pse.obs$ID)))
j = 1
n = sum(!is.na(pse.dat.fit$pse))
for(ind in unique(pse.obs$ID)){ ## Leave one observation out
  pse.train = subset(pse.obs, ID != ind)
  pse.test = subset(pse.obs, ID == ind)
  
  dhsonly.fit = lmer(dhs.only.form, data = pse.train)
  dhs.fit = lmer(dhs.form, data = pse.train)
  nondhs.fit = lmer(nondhs.form, data = pse.train)
  
  lodo.pred.vec = predict(dhsonly.fit, newdata = pse.test,
                          allow.new.levels = TRUE)
  lodo.rmse.dhsonly[j] = sqrt(mean((pse.test$logit_prev - lodo.pred.vec)^2, na.rm = T))
  
  lodo.pred.vec = predict(dhs.fit, newdata = pse.test,
                          allow.new.levels = TRUE)
  lodo.rmse.dhs[j] = sqrt(mean((pse.test$logit_prev - lodo.pred.vec)^2, na.rm = T))
  
  lodo.pred.vec = predict(nondhs.fit, newdata = pse.test,
                          allow.new.levels = TRUE)
  lodo.rmse.nondhs[j] = sqrt(mean((pse.test$logit_prev - lodo.pred.vec)^2, na.rm = T))
  
    if(j %% 100 == 0){
      print(j/length(unique(pse.obs$ID)))
    }

    j = j + 1
}

mean(lodo.rmse.dhs) / mean(lodo.rmse.nondhs)
mean(lodo.rmse.dhsonly) / mean(lodo.rmse.nondhs)


## Check LOCO
uniq.iso = unique(pse.obs$ISO)
n = sum(!is.na(pse.dat.fit$pse))
loco.rmse.dhs = loco.rmse.nondhs = loco.rmse.dhsonly = rep(NA, length(uniq.iso))
j = 1
for(ind in uniq.iso){ ## LOCO
  pse.train = subset(pse.obs, ISO != ind)
  pse.test = subset(pse.obs, ISO == ind)
  
  dhsonly.fit = lmer(dhs.only.form, data = pse.train)
  dhs.fit = lmer(dhs.form, data = pse.train)
  nondhs.fit = lmer(nondhs.form, data = pse.train)

  
  lodo.pred.vec = predict(dhsonly.fit, newdata = pse.test,
                          allow.new.levels = TRUE)
  loco.rmse.dhsonly[j] = sqrt(mean((pse.test$logit_prev - lodo.pred.vec)^2, na.rm = T))
  
  lodo.pred.vec = predict(dhs.fit, newdata = pse.test,
                          allow.new.levels = TRUE)
  loco.rmse.dhs[j] = sqrt(mean((pse.test$logit_prev - lodo.pred.vec)^2, na.rm = T))
  
  lodo.pred.vec = predict(nondhs.fit, newdata = pse.test,
                          allow.new.levels = TRUE)
  loco.rmse.nondhs[j] = sqrt(mean((pse.test$logit_prev - lodo.pred.vec)^2, na.rm = T))
  j = j + 1
}

mean(loco.rmse.dhs) / mean(loco.rmse.nondhs)
mean(loco.rmse.dhsonly) / mean(loco.rmse.nondhs)




# Check ability to predict missing values ---------------------------------
## First using multiple imputation

pse.dat.impute = gadm.cov@data
pse.dat.impute = pse.dat.impute[,which(names(pse.dat.impute) %in%
                                         c(cov.in.dhs.and.aux[c(4, 6, 7, 11:15)],
                                           names(pse.dat.impute)[241:345]))]
est.mat = matrix(NA, nrow = nrow(pse.dat.impute), ncol = ncol(pse.dat.impute))
## Implement MICE
for(i in 1:nrow(pse.dat.impute)){
  pse.tmp = pse.dat.impute
  pse.tmp[i,1:9] = NA
  imp1 = mice(pse.tmp, m = 1, method = "cart", printFlag = FALSE)
  # imp1 = mice(pse.tmp, m = 1, method = "norm.predict", printFlag = FALSE)
  est.mat[i,1:9] = as.numeric(complete(imp1, action = 1)[i,1:9])

  print(i/nrow(pse.dat.impute) * 100)
}

plot(jitter(est.mat[,1]), jitter(pse.dat.impute[,1]))
plot(jitter(est.mat[,2]), jitter(pse.dat.impute[,2]))
plot(est.mat[,3], pse.dat.impute[,3])
plot(est.mat[,4], pse.dat.impute[,4])
plot(jitter(est.mat[,5]), jitter(pse.dat.impute[,5]))
plot(est.mat[,6], pse.dat.impute[,6])
plot(jitter(est.mat[,7]), jitter(pse.dat.impute[,7]))
plot(est.mat[,8], pse.dat.impute[,8])
plot(est.mat[,9], pse.dat.impute[,9])

par(mfrow = c(1,2))
plot(est.mat[,4], pse.dat.impute[,4], xlab = "Imputed Values", ylab = "True Values")
plot(est.mat[,6], pse.dat.impute[,6], xlab = "Imputed Values", ylab = "True Values")

tmp.dat = data.frame(Truth = pse.dat.impute$mv133_median_CR, Est = est.mat[,1])
gg.impute = ggplot(tmp.dat) + geom_point(aes(x = jitter(Est), y = jitter(Truth)), size = 2) +
  xlab("Imputed Value") + ylab("Observed Value") +
  geom_abline(slope = 1, intercept = 0, size = 1.5) +
  theme_grey(base_size = 15) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 22, face = "bold"))
gg.impute
ggsave("./Figures/MICE_impute_cart.jpg", gg.impute, width = 20, height = 10)


save.image("../Data/DHS/Imputation_res_norm_predict_2.RData")

# Try spatial imputation --------------------------------------------------

gadm.36 = readRDS("../Data/Shapefiles/gadm36_adm1_africa_dhs.rds")
gadm.36 = subset(gadm.36, !is.na(mv133_median_CR))
N = nrow(gadm.36@data)

nb.RK = poly2nb(gadm.36, queen=TRUE, row.names=gadm.36$ROW_ID,
                snap = 1e-4)
nb.RK.orig = nb.RK
nb.mat <- nb2mat(nb.RK, zero.policy = TRUE, style = "B")
which.zero = which(rowSums(nb.mat) == 0)
nb.RK = nb.RK[-which.zero]


## Do LOO prediction

gadm.new = gadm.36[-which.zero,]
N = nrow(gadm.new)
nb.RK = poly2nb(gadm.new, queen=TRUE, row.names=1:nrow(gadm.new),
                snap = 1e-4)
nb.RK.orig = nb.RK
nb.mat.orig = nb2mat(nb.RK.orig, style = "B")

which.ind = which(names(gadm.new) %in% cov.in.dhs.and.aux)

save.image("../Data/DHS/Spatial_impute_prep.RData")
car.stan = rstan::stan_model("./R/Test Code/Sparse_CAR.stan")

for(i in which.ind){
  all.phi = gadm.new@data[, i]
  N = length(all.phi)
  true.known.n = sum(!is.na(all.phi))
  which.known.true = which(!is.na(all.phi))
  phi.known.true = all.phi[which.known.true]
  which.unknown.true = which(is.na(all.phi))
  loo.est = rep(NA, true.known.n)
  for(iter in 1:true.known.n){
    which.unknown = c(which.known.true[iter], which.unknown.true)
    which.known = which.known.true[-iter]
    N.unknown = length(which.unknown)
    N.known = length(which.known)
    
    phi.known = phi.known.true[-iter]
    phi.known = scale(phi.known)
    stan.data <- list(n = N,    # number of observations
                      n_known = length(which.known),
                      n_unknown = length(which.unknown),
                      known_ind = which.known,
                      unknown_ind = as.array(which.unknown),
                      phi_known = phi.known[,1],
                      W_n = sum(nb.mat.orig) / 2,    # number of neighbor pairs
                      W = nb.mat.orig)     # adjacency matrix
    
    fit.loo = rstan::sampling(car.stan,
                              data=stan.data,
                              chains=2,
                              cores=2,
                              warmup=1000,
                              iter=1500,
                              save_warmup = FALSE)
    
    phi.unknown = rstan::extract(fit.loo, "phi_unknown")[[1]]
    phi.est = colMeans(phi.unknown)
    loo.est[iter] = mean(phi.unknown[,1]) * attr(phi.known, "scaled:scale") + 
      attr(phi.known, "scaled:center")
    print(100 * iter / true.known.n)
  }
}


library(ggplot2)
truth.in = scale(phi.known.true)
df = data.frame(loo = (loo.est - attr(truth.in, "scaled:center"))/
                  attr(truth.in, "scaled:scale"),
                truth = truth.in)
loo.plot = ggplot(df) + geom_point(aes(x = loo, y = truth)) +
  xlab("Imputed Value") + ylab("Observed Value") +
  geom_abline(slope = 1, intercept = 0, size = 1.5) +
  theme_grey(base_size = 15) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 22, face = "bold"))
loo.plot
ggsave("./Figures/Spatial_impute.jpg", loo.plot, width = 20, height = 10)
