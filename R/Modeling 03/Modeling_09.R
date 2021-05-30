library(raster)
library(spdep)
library(plyr)
library(tmap)
library(lme4)
library(ggplot2)
library(brms)
library(optimx)
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
                                     x = as.numeric(as.character(Prevalence_Calc)))) +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Calculated FSW Proportion") + ylab("Reported Literature FSW Proportion") + 
  theme_grey(base_size = 15) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 22, face = "bold"))
prev.vs.prev.gg
ggsave("./Figures/Prev_vs_prev.jpg", prev.vs.prev.gg, width = 20, height = 10)


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


pse.dat.fit[,cov.names] = scale(pse.dat.fit[,cov.names])
which.incomplete = which(!complete.cases(pse.dat.fit[,cov.names[-1]]))
pse.dat.fit$NAME_1[which.incomplete]

pse.dat.fit$prev = pse.dat.fit$pse / pse.dat.fit$ref_pop
cov.names = c(cov.names, "City")
pse.dat.fit$logit_prev = qlogis(pse.dat.fit$prev)
pse.sub = subset(pse.dat.fit, !is.na(pse.dat.fit$pse))
pse.pred = subset(pse.dat.fit, is.na(pse.dat.fit$pse))
saveRDS(pse.dat.fit, file = "../Data/Final_pse_dat_fit_3.rds")
saveRDS(pse.sub, file = "../Data/Final_brms_data_for_fitting.rds")
saveRDS(pse.pred, file = "../Data/Final_brms_data_for_prediction.rds")

# Stepwise variable selection ---------------------------------------------


pse.obs = subset(pse.dat.fit, !is.na(pse))
pse.obs$row.id = 1:nrow(pse.obs)
spatial.ind = unique(pse.obs$ID)
cov.out = cov.names
## Decide what to keep (otherwise c())
cov.in = c("City", "ref_pop_log", "City * ref_pop_log")
cov.out = cov.out[-which(cov.names %in% cov.in)]
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

## Look at diagnostics
gg.qq = ggplot(resid.df, aes(sample = resid)) + stat_qq() + geom_qq_line(size = 1) +
  xlab("Theoretical Quantiles") + ylab("Sample Quantiles") +
  theme_grey(base_size = 15) + ggtitle("Normal distribution") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 22, face = "bold"))
ggsave("./Figures/QQ_plot.jpg", gg.qq, width = 20, height = 10)

gg.resid = ggplot(resid.df) + geom_point(aes(x = fitted, y = resid)) +
  xlab("Fitted Values") + ylab("Residuals") +
  geom_abline(intercept = 0, slope = 0, size = 1) + 
  theme_grey(base_size = 15) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 22, face = "bold"))
ggsave("./Figures/Resid_plot.jpg", gg.resid, width = 20, height = 10)

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

## Remove some columns from gadm.cov
gadm.cov@data = gadm.cov@data[,c(1, 2, 3, 345:350)]
saveRDS(gadm.cov, "./Data/Results/Final_district_est.rds")
saveRDS(gadm.country, "./Data/Results/Final_country_est.rds")
saveRDS(brms.base, "./Data/Results/Final_model_fit.rds")
write.csv(gadm.cov@data, "./Data/Results/Final_district_est.csv")
write.csv(gadm.country@data, "./Data/Results/Final_country_est.csv")

save.image("../Data/Final_Results.RData")





###########################################################################
###########################################################################
## This ends the main results.
## The following code is additional diagnostics and secondary analysis
###########################################################################
###########################################################################



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
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 22, face = "bold"))
gg1
ggsave("./Figures/Method_boxplots.jpg", plot = gg1, height = 10, width = 20)






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
write.csv(gadm.country@data, "./Data/Results/Final_country_est.csv")



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




