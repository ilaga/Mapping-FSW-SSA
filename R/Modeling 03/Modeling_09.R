library(raster)
library(spdep)
library(plyr)
library(tmap)
library(lme4)
library(ggplot2)
library(brms)
library(optimx)
library(gridExtra)


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


combined = rbind.fill(pse.dat.fit, gadm.cov@data)
combined$ref_pop = round(combined$ref_pop)


pse.dat.fit = combined[,c("pse", "ref_pop", "ID", "method_std",
                          "ISO", "NAME_1", "City", "lower.uncertainty.measure",
                          "upper.uncertainty.measure", "type.of.uncertainty.measure",
                          names(combined)[c(286:389)])]
pse.dat.fit = pse.dat.fit[,-which(names(pse.dat.fit) == "ID.1")]

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
                                                            "tree_cover_necto",
                                                            "tree_cover_ndcto",
                                                            "mosaic_tree",
                                                            "water",
                                                            "shrub_cover",
                                                            "mosaic_natural_veg",
                                                            "mosaic_herbaceous",
                                                            "shrubland",
                                                            "grassland",
                                                            "snow_ice",
                                                            "lichens",
                                                            "wc2.1_2.5m_bio_14_popweighted",
                                                            "wc2.1_2.5m_bio_19_popweighted",
                                                            "wc2.1_2.5m_bio_17_popweighted",
                                                            "wc2.1_2.5m_bio_6_popweighted"))]


## Now scale
cov.names = names(pse.dat.fit)[c(11:89)]

single.subnational.ind = which(is.na(pse.dat.fit$pse))
scale.info = scale(pse.dat.fit[single.subnational.ind, cov.names])
for(k in 1:length(cov.names)){
  pse.dat.fit[,cov.names[k]] = (pse.dat.fit[,cov.names[k]] - attributes(scale.info)$`scaled:center`[k]) /
    attributes(scale.info)$`scaled:scale`[k]
}
which.incomplete = which(!complete.cases(pse.dat.fit[,cov.names[-1]]))
pse.dat.fit$NAME_1[which.incomplete]

## Remove variables with missing values
which.miss = names(which(colSums(is.na(pse.dat.fit[,cov.names])) > 0))
cov.names = cov.names[-which(cov.names %in% which.miss)]

cov.names = c(cov.names, "City", "City * ref_pop_log")
pse.dat.fit$prev = pse.dat.fit$pse / pse.dat.fit$ref_pop
pse.dat.fit$logit_prev = qlogis(pse.dat.fit$prev)
pse.obs = subset(pse.dat.fit, !is.na(pse))
pse.pred = subset(pse.dat.fit, is.na(pse))
pse.pred$City = 0
saveRDS(pse.dat.fit, file = "../Data/Final_pse_dat_fit.rds")
saveRDS(pse.obs, file = "../Data/Final_brms_data_for_fitting.rds")
saveRDS(pse.pred, file = "../Data/Final_brms_data_for_prediction.rds")
saveRDS(cov.names, file = "../Data/cov_names.rds")


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




# Horseshoe Penalized Regression ------------------------------------------

model.form = as.formula(paste0("logit_prev ~ ",
                               paste(cov.names, collapse = " + "),
                               "+ (1 | method_std) + (1 | ISO)"))

horseshoe.fit = brm(model.form, data = pse.obs, cores = 3, chains = 3,
                    iter = 6000, warmup = 4000,
                    control = list(adapt_delta = 0.95),
                    prior = prior(horseshoe(par_ratio = 0.1), class = "b"),
                    family = student())


est.eff = fixef(horseshoe.fit)[-1,]
rownames(est.eff) = c("Worldpop Total Pop", "Worldpop fcba Pop", "Worldpop fcba Ratio", "Rainfed Cropland", "Mosaic Cropland", "Percent Urban",
                   "Wind Speed", "Vapor Pressure", "Solar Radiation", "Precipitation", "Elevation", "Annual Mean Temperature", "Mean Diurnal Range",
                   "Isothermality", "Temp Seasonality", "Max Temp of Warm Month", "Min Temp of Cold Month", "Temp Annual Range",
                   "Mean Temp Wet Quarter", "Mean Temp Dry Quarter", "Mean Temp Warm Quarter", "Mean Temp Cold Quarter",
                   "Annual Prec", "Prec of Wet Month", "Prec of Dry Month", "Prec Seasonality", "Prec Wet Quarter", "Prec Dry Quarter",
                   "Prec Warm Quarter", "Prec Cold Quarter",
                   "Wind Speed PW", "Vapor Pressure PW", "Solar Radiation PW", "Elevation PW",
                   "Annual Mean Temperature PW", "Mean Diurnal Range PW",
                   "Isothermality PW", "Temp Seasonality PW", "Max Temp of Warm Month PW", "Temp Annual Range PW",
                   "Mean Temp Wet Quarter PW", "Mean Temp Dry Quarter PW", "Mean Temp Warm Quarter PW", "Mean Temp Cold Quarter PW",
                   "Annual Prec PW", "Prec of Wet Month PW", "Prec Seasonality PW", "Prec Wet Quarter PW",
                   "Prec Warm Quarter PW", "Growing Season Length", "Growing Season Length PW",
                   "Travel to 50K", "Walking Time to Healthcare", "Walking time to Healthcare PW", "Duffy Negative Freq", "Duffy Negative Freq PW",
                   "HBS Freq", "HBS Freq PW", "G6PD Freq", "G6PD Freq PW", "Nighttime Light Mean", "Nighttime Light 5 Quant", "Nighttime Light Median",
                   "Nighttime Light 95 Quant", "Population Density", "GDP", "Reference Population (log)", "City", "Reference Population (log) * City")
est.eff.df = data.frame(est.eff)
est.eff.df$Predictor = rownames(est.eff)
est.eff.df$Predictor = factor(est.eff.df$Predictor, levels = c(est.eff.df$Predictor)[order(abs(est.eff.df$Estimate))])

gg.eff = ggplot(est.eff.df, mapping = aes(y = Predictor)) + 
  geom_point(aes(x = Estimate), size = 3) +
  geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5), size = 1.2) +
  theme_gray(base_size = 15) +
  ylab("Predictor") + xlab("Estimated Coefficient") +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(size = 22, face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 22, face = "bold")) +
  theme(plot.margin = margin(unit(c(5.5, 15.5, 5.5, 5.5), "points")))

## Save Supplementary Figure XXX
ggsave("../Figures/Coef_plot.jpg", gg.eff, width = 10, height = 15)

###################
## Get values for Table 1
fixef.mat = fixef(horseshoe.fit)
rownames(fixef.mat) = c("Intercept", rownames(est.eff))
fixef.top.10 = round(fixef.mat[order(abs(fixef.mat[,1]), decreasing = T),][1:10,], digits = 3)
fixef.top.10 = data.frame(fixef.top.10)

table.1.mat = matrix(NA, nrow = 10, ncol = 2)
for(i in 1:10){
  table.1.mat[i,1] = rownames(fixef.top.10)[i]
  table.1.mat[i,2] = paste0(fixef.top.10$Estimate[i], " (", fixef.top.10$Q2.5[i],
                            ", ", fixef.top.10$Q97.5[i], ")")
}
colnames(table.1.mat) = c("Predictor", "Estimate (95% credible interval)")
write.csv(table.1.mat, "../Data/Results/Top_10_coef.csv", row.names = F)

###################
## Get values for Supplementary Table S2
fixef.mat = data.frame(Predictor = rownames(fixef(horseshoe.fit)), fixef(horseshoe.fit))
fixef.mat$Predictor = c("Intercept", rownames(est.eff))
fixef.mat$Estimate = format(round(fixef.mat$Estimate, digits = 3), nsmall = 3)
fixef.mat$Est.Error = format(round(fixef.mat$Est.Error, digits = 3), nsmall = 3)
fixef.mat$Q2.5 = format(round(fixef.mat$Q2.5, digits = 3), nsmall = 3)
fixef.mat$Q97.5 = format(round(fixef.mat$Q97.5, digits = 3), nsmall = 3)

table.S2.mat = matrix(NA, nrow = nrow(fixef.mat), ncol = 2)
for(i in 1:nrow(fixef.mat)){
  table.S2.mat[i,1] = fixef.mat$Predictor[i]
  table.S2.mat[i,2] = paste0(fixef.mat$Estimate[i], " (", fixef.mat$Q2.5[i],
                            ", ", fixef.mat$Q97.5[i], ")")
}

table.S2.mat = data.frame(table.S2.mat)
table.S2.mat$Explanation = NA

names(table.S2.mat) = c("Predictor", "Estimate (95% credible interval)", "Predictor Explanation")
table.S2.mat = table.S2.mat[order(abs(as.numeric(fixef.mat$Estimate)), decreasing = T),]

write.csv(table.S2.mat, "../Data/Results/All_est_coef.csv", row.names = F)


####################
## Get values for Table 2
method.mat = data.frame(ranef(horseshoe.fit)$method_std)
method.mat = round(method.mat, digits = 3)

## Find FSW estimate for methods
method.effect = data.frame(matrix(0, nrow = 8, ncol = length(cov.names) + 1))
names(method.effect) = c(cov.names, "method_std")
method.effect$method_std = c("CRC", "Enumeration", "Expert", "Mapping",
                             "Misc", "Multiple", "Multiplier", "NR")
cov.effect = fitted(horseshoe.fit, newdata = method.effect, allow_new_levels = F,
                    sample_new_levels = "gaussian", summary = FALSE,
                    re_formula = ~ (1 | method_std)) ## Ignore ISO

method.ave.fsw = data.frame(Method = method.effect$method_std,
           Estimate = round(100 * colMeans(plogis(cov.effect)), digits = 2))



table.2.mat = matrix(NA, nrow = nrow(method.mat), ncol = 3)
for(i in 1:nrow(method.mat)){
  table.2.mat[i,1] = rownames(method.mat)[i]
  table.2.mat[i,2] = paste0(method.mat$Estimate[i], " (", method.mat$Q2.5[i],
                            ", ", method.mat$Q97.5[i], ")")
  table.2.mat[i,3] = method.ave.fsw$Estimate[i]
}
colnames(table.2.mat) = c("Method", "Estimate (95% credible interval)", "FSW percent estimate at methods")
write.csv(table.2.mat, "../Data/Results/Method_effect.csv", row.names = F)




# LODO sequence -----------------------------------------------------------
## Normal vs t errors
n = nrow(pse.obs)
ID.vec = unique(pse.obs$ID)
n.ID = length(ID.vec)
ISO.vec = unique(pse.obs$ISO)
n.ISO = length(ISO.vec)

normal.mean.error = t.mean.error = c()

for(i in 1:n.ID){
  district.ind = which(pse.obs$ID == ID.vec[i])
  pse.sub = pse.obs[district.ind,]
  
  normal.pred = readRDS(file = paste0("../Data/Results/LOO_Results/Normal_pred_values_ID_",ID.vec[i],"_ind_",i,".rds"))
  t.pred = readRDS(file = paste0("../Data/Results/LOO_Results/t_pred_values_ID_",ID.vec[i],"_ind_",i,".rds"))
  
  normal.mean.error = c(normal.mean.error, colMeans(normal.pred) - pse.sub$logit_prev)
  t.mean.error = c(t.mean.error, colMeans(t.pred) - pse.sub$logit_prev)
  
  print(i)
}

mean(normal.mean.error^2)
mean(t.mean.error^2)

hist(normal.mean.error)
hist(t.mean.error)

plot(normal.mean.error, t.mean.error)
abline(0, 1, col = "red")


## Spatial vs no Spatial
spatial.mean.error = nospatial.mean.error = c()

for(i in 1:n.ID){
  district.ind = which(pse.obs$ID == ID.vec[i])
  pse.sub = pse.obs[district.ind,]
  
  normal.pred = readRDS(file = paste0("../Data/Results/LOO_Results/Spatial_pred_values_ID_",ID.vec[i],"_ind_",i,".rds"))
  spatial.pred = readRDS(file = paste0("../Data/Results/LOO_Results/Nospatial_pred_values_ID_",ID.vec[i],"_ind_",i,".rds"))
  
  spatial.mean.error = c(spatial.mean.error, colMeans(normal.pred) - pse.sub$logit_prev)
  nospatial.mean.error = c(nospatial.mean.error, colMeans(spatial.pred) - pse.sub$logit_prev)
  
  # print(i)
}

mean(spatial.mean.error^2)
mean(nospatial.mean.error^2)

hist(spatial.mean.error)
hist(nospatial.mean.error)

plot(spatial.mean.error, nospatial.mean.error)
abline(0, 1, col = "red")





# LOCO sequence -----------------------------------------------------------
## Normal vs t errors

normal.mean.error = t.mean.error = c()

for(i in 1:n.ISO){
  country.ind = which(pse.obs$ISO == ISO.vec[i])
  pse.sub = pse.obs[country.ind,]
  
  normal.pred = readRDS(file = paste0("../Data/Results/LOO_Results/Normal_pred_values_ISO_",ISO.vec[i],"_ind_",i,".rds"))
  t.pred = readRDS(file = paste0("../Data/Results/LOO_Results/t_pred_values_ISO_",ISO.vec[i],"_ind_",i,".rds"))
  
  normal.mean.error = c(normal.mean.error, colMeans(normal.pred) - pse.sub$logit_prev)
  t.mean.error = c(t.mean.error, colMeans(t.pred) - pse.sub$logit_prev)
  
  # print(i)
}

mean(normal.mean.error^2)
mean(t.mean.error^2)

hist(normal.mean.error)
hist(t.mean.error)

plot(normal.mean.error, t.mean.error)
abline(0, 1, col = "red")




## Spatial vs no Spatial
spatial.mean.error = nospatial.mean.error = c()

for(i in 1:n.ISO){
  country.ind = which(pse.obs$ISO == ISO.vec[i])
  pse.sub = pse.obs[country.ind,]
  
  normal.pred = readRDS(file = paste0("../Data/Results/LOO_Results/Spatial_pred_values_ISO_",ISO.vec[i],"_ind_",i,".rds"))
  spatial.pred = readRDS(file = paste0("../Data/Results/LOO_Results/Nospatial_pred_values_ISO_",ISO.vec[i],"_ind_",i,".rds"))
  
  spatial.mean.error = c(spatial.mean.error, colMeans(normal.pred) - pse.sub$logit_prev)
  nospatial.mean.error = c(nospatial.mean.error, colMeans(spatial.pred) - pse.sub$logit_prev)
  
  # print(i)
}

mean(spatial.mean.error^2)
mean(nospatial.mean.error^2)

hist(spatial.mean.error)
hist(nospatial.mean.error)

plot(spatial.mean.error, nospatial.mean.error)
abline(0, 1, col = "red")






# Compare fit of t and normal ---------------------------------------------


normal.fit = brm(model.form, data = pse.obs, cores = 3, chains = 3,
                    iter = 6000, warmup = 4000,
                    control = list(adapt_delta = 0.95),
                    prior = prior(horseshoe(par_ratio = 0.1), class = "b"))

gg.hs.dens = pp_check(horseshoe.fit, type = "dens_overlay") +
  theme_grey(base_size = 15) + ggtitle("Student's T") +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        title = element_text(size = 22, face = "bold"),
        aspect.ratio = 1)

gg.hs.ecdf = pp_check(horseshoe.fit, type = "ecdf_overlay") +
  theme_grey(base_size = 15) + ggtitle("Student's T") +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        title = element_text(size = 22, face = "bold"),
        aspect.ratio = 1)

gg.hs.stat = pp_check(horseshoe.fit, type = "stat_2d") +
  theme_grey(base_size = 15) + ggtitle("Student's T") +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        title = element_text(size = 22, face = "bold"),
        aspect.ratio = 1)

gg.hs.loo = pp_check(horseshoe.fit, type = "loo_pit") +
  theme_grey(base_size = 15) + ggtitle("Student's T") +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        title = element_text(size = 22, face = "bold"),
        aspect.ratio = 1)

### Normal plots
gg.normal.dens = pp_check(normal.fit, type = "dens_overlay") +
  theme_grey(base_size = 15) + ggtitle("Gaussian") +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        title = element_text(size = 22, face = "bold"),
        aspect.ratio = 1)

gg.normal.ecdf = pp_check(normal.fit, type = "ecdf_overlay") +
  theme_grey(base_size = 15) + ggtitle("Gaussian") +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        title = element_text(size = 22, face = "bold"),
        aspect.ratio = 1)

gg.normal.stat = pp_check(normal.fit, type = "stat_2d") +
  theme_grey(base_size = 15) + ggtitle("Gaussian") +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        title = element_text(size = 22, face = "bold"),
        aspect.ratio = 1)

gg.normal.loo = pp_check(normal.fit, type = "loo_pit") +
  theme_grey(base_size = 15) + ggtitle("Gaussian") +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        title = element_text(size = 22, face = "bold"),
        aspect.ratio = 1)


ggsave("../Figures/hs_dens.jpg", gg.hs.dens, width = 10, height = 10)
ggsave("../Figures/hs_ecdf.jpg", gg.hs.ecdf, width = 10, height = 10)
ggsave("../Figures/hs_stat.jpg", gg.hs.stat, width = 10, height = 10)
ggsave("../Figures/hs_loo.jpg", gg.hs.loo, width = 10, height = 10)

ggsave("../Figures/normal_dens.jpg", gg.normal.dens, width = 10, height = 10)
ggsave("../Figures/normal_ecdf.jpg", gg.normal.ecdf, width = 10, height = 10)
ggsave("../Figures/normal_stat.jpg", gg.normal.stat, width = 10, height = 10)
ggsave("../Figures/normal_loo.jpg", gg.normal.loo, width = 10, height = 10)
## qq plots are saved below







## Get width of intervals

normal.fitted = fitted(normal.fit, summary = F)
t.fitted = fitted(horseshoe.fit, summary = F)

normal.interval = t(apply(normal.fitted, 2, quantile, probs = c(0.025, 0.975)))
t.interval = t(apply(t.fitted, 2, quantile, probs = c(0.025, 0.975)))

normal.width.std = abs((normal.interval[,2] - normal.interval[,1])) / abs(colMeans(normal.fitted))
t.width.std = abs((t.interval[,2] - t.interval[,1])) / abs(colMeans(t.fitted))

plot(normal.width.std, t.width.std)
abline(0, 1, col = "red")

min.val = min(normal.width.std, t.width.std)
max.val = max(normal.width.std, t.width.std)

width.df = data.frame("normal" = normal.width.std,
                      "t" = t.width.std)

gg.normal.t.width = ggplot(width.df) + 
  geom_point(aes(x = normal, y = t)) +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  xlab("Relative Uncertainty of Gaussian Model") + ylab("Relative Uncertainty of Student's T Model") +
  theme_grey(base_size = 15) + ggtitle("") +
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
ggsave("../Figures/normal_t_width.jpg", gg.normal.t.width, width = 10, height = 10)

# Study Final Model ---------------------------------------------------------

## First compare prediction intervals to lower and upper bounds of observed estimates
pse.obs.lu = pse.obs
pse.obs.lu$lower.uncertainty.measure = as.numeric(pse.obs.lu$lower.uncertainty.measure)
pse.obs.lu$upper.uncertainty.measure = as.numeric(pse.obs.lu$upper.uncertainty.measure)
pse.obs.lu = subset(pse.obs.lu, !is.na(pse.obs.lu$lower.uncertainty.measure) | !is.na(pse.obs.lu$upper.uncertainty.measure))
pse.obs.lu$lower.prev = pse.obs.lu$lower.uncertainty.measure / pse.obs.lu$ref_pop
pse.obs.lu$upper.prev = pse.obs.lu$upper.uncertainty.measure / pse.obs.lu$ref_pop

# predicted.vals.sub = predict(horseshoe.fit, pse.obs.lu, summary = F)
predicted.vals.sub = fitted(horseshoe.fit, pse.obs.lu, summary = F)
predicted.vals.sub = plogis(predicted.vals.sub)

predicted.width = t(apply(predicted.vals.sub, 2, quantile, probs = c(0.025, 0.975)))

predicted.df = data.frame(pred.lower = predicted.width[,1],
                          pred.upper = predicted.width[,2],
                          pred.std = abs(predicted.width[,2] - predicted.width[,1]) / abs(colMeans(predicted.vals.sub)),
                          obs.lower = pse.obs.lu$lower.prev,
                          obs.upper = pse.obs.lu$upper.prev,
                          obs.std = abs(pse.obs.lu$upper.prev - pse.obs.lu$lower.prev) / abs(pse.obs.lu$prev))

ggplot(predicted.df) +
  geom_errorbar(aes(x = 1:nrow(pse.obs.lu), ymin = obs.lower, ymax = obs.upper)) +
  geom_errorbar(aes(x = 1:nrow(pse.obs.lu) + 0.2, ymin = pred.lower, ymax = pred.upper), col = "red")


predicted.melt = reshape2::melt(predicted.df, measure.vars = c("pred.std", "obs.std"))
predicted.melt$id = rep(1:nrow(predicted.df), 2)

gg.pred.width = ggplot(predicted.melt) +
  geom_point(aes(x = id, y = value, col = variable), size = 2) +
  scale_color_discrete(name = "Interval", labels = c("Model Prediction", "Literature")) +
  xlab("Observation Number") +
  ylab("Standardized Prediction Width") +
  theme_grey(base_size = 15) + ggtitle("") +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        title = element_text(size = 22, face = "bold"),
        aspect.ratio = 1)
ggsave("../Figures/Pred_vs_obs_width.jpg", gg.pred.width, width = 10, height = 10)

gg.pred.width.trunc = ggplot(predicted.melt) +
  geom_point(aes(x = id, y = value, col = variable), size = 2) +
  scale_color_discrete(name = "Interval", labels = c("Model Prediction", "Literature")) +
  xlab("Observation Number") +
  ylab("Standardized Prediction Width") +
  theme_grey(base_size = 15) + ggtitle("") +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        title = element_text(size = 22, face = "bold"),
        aspect.ratio = 1) +
  ylim(0, 2.2)
ggsave("../Figures/Pred_vs_obs_width_trunc.jpg", gg.pred.width.trunc, width = 10, height = 10)

mean(predicted.df$pred.std < predicted.df$obs.std)




ggplot(predicted.df) +
  geom_point(aes(x = pred.std, y = obs.std), size = 2) +
  xlab("Predicted Standardized Interval Width") +
  ylab("Literature Standardized Interval Width") +
  theme_grey(base_size = 15) + ggtitle("Normal distribution") +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        title = element_text(size = 22, face = "bold")) +
  geom_abline(slope = 1, intercept = 0, col = "red", size = 1.5) +
  xlim(0, 50)


## Now analyze the mean
fitted.vals = fitted(horseshoe.fit)
resid.vals = resid(horseshoe.fit)
resid.df = data.frame(resid = resid.vals[,1], fitted = fitted.vals[,1])

fitted.normal.vals = fitted(normal.fit)
resid.normal.vals = resid(normal.fit)
resid.normal.df = data.frame(resid = resid.normal.vals[,1], fitted = fitted.normal.vals[,1])

## Look at diagnostics
gg.qq = ggplot(resid.df, aes(sample = resid)) +
  stat_qq(distribution = qt, dparams = summary(horseshoe.fit)$spec_pars[2,1], size = 2.5) +
  geom_qq_line(distribution = qt, dparams = summary(horseshoe.fit)$spec_pars[2,1], size = 1.5, fullrange = T) +
  xlab("Theoretical Quantiles") + ylab("Sample Quantiles") +
  theme_grey(base_size = 15) + ggtitle("Student's T") +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        title = element_text(size = 22, face = "bold"),
        aspect.ratio = 1)
ggsave("../Figures/QQ_hs_plot.jpg", gg.qq, width = 10, height = 10)

gg.qq.normal = ggplot(resid.normal.df, aes(sample = resid)) +
  stat_qq(size = 2.5) +
  geom_qq_line(size = 1.5, fullrange = T) +
  xlab("Theoretical Quantiles") + ylab("Sample Quantiles") +
  theme_grey(base_size = 15) + ggtitle("Gaussian distribution") +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        title = element_text(size = 22, face = "bold"),
        aspect.ratio = 1)
ggsave("../Figures/QQ_normal_plot.jpg", gg.qq.normal, width = 10, height = 10)



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


## Extract fitted values for presentation
post.fitted = fitted(horseshoe.fit, newdata = pse.pred, allow_new_levels = TRUE,
                     sample_new_levels = "gaussian", summary = FALSE,
                     re_formula = ~ (1|ISO)) ## Ignore method
post.transformed = plogis(post.fitted)
post.mean = colMeans(post.transformed)
quant.interval = apply(post.transformed, 2, quantile, probs = c(0.025, 0.975))


## Add results to gadm.cov
gadm.cov = readRDS("../Data/Shapefile_Prep.rds")
gadm.cov$Prev = gadm.cov$Fitted_FSW = gadm.cov$Lower = gadm.cov$Upper =
  gadm.cov$Uncertainty = NA

gadm.cov$Prev = post.mean 
gadm.cov$Fitted_FSW = post.mean * pse.pred$ref_pop
gadm.cov$Lower = quant.interval[1,]
gadm.cov$Upper = quant.interval[2,]
gadm.cov$Uncertainty = (gadm.cov$Upper - gadm.cov$Lower) / gadm.cov$Prev


## Find FSW estimate for top 9 covariates, except for city and city interaction
## Create plots for this effect
cov.effect.df = data.frame(matrix(NA, nrow = 0, ncol = length(cov.names) + 3))
names(cov.effect) = c(cov.names, "value", "variable", "Estimate")

## Get base level without covariates
cov.tmp = data.frame(matrix(0, nrow = 1, ncol = length(cov.names) + 2))
names(cov.tmp) = c(cov.names, "value", "variable")
cov.effect = fitted(horseshoe.fit, newdata = cov.tmp, allow_new_levels = TRUE,
                    sample_new_levels = "gaussian", summary = FALSE,
                    re_formula = NA) ## Ignore method and ISO
cbind(cov.tmp, colMeans(plogis(cov.effect))) * 100

## Reference Population (log)
cov.tmp = data.frame(matrix(0, nrow = 1000, ncol = length(cov.names) + 2))
names(cov.tmp) = c(cov.names, "value", "variable")
cov.tmp$ref_pop_log = seq(-2, 2, length.out = 1000)
cov.tmp$value = seq(-2, 2, length.out = 1000)
cov.tmp$variable = "Log Reference Population"
cov.effect = fitted(horseshoe.fit, newdata = cov.tmp, allow_new_levels = TRUE,
                    sample_new_levels = "gaussian", summary = FALSE,
                    re_formula = NA) ## Ignore method and ISO
cov.effect.df = rbind(cov.effect.df, cbind(cov.tmp, colMeans(plogis(cov.effect))))

## Population Density (log)
cov.tmp = data.frame(matrix(0, nrow = 1000, ncol = length(cov.names) + 2))
names(cov.tmp) = c(cov.names, "value", "variable")
cov.tmp$PD = seq(-2, 2, length.out = 1000)
cov.tmp$value = seq(-2, 2, length.out = 1000)
cov.tmp$variable = "Log Population Density"
cov.effect = fitted(horseshoe.fit, newdata = cov.tmp, allow_new_levels = TRUE,
                    sample_new_levels = "gaussian", summary = FALSE,
                    re_formula = NA) ## Ignore method and ISO
cov.effect.df = rbind(cov.effect.df, cbind(cov.tmp, colMeans(plogis(cov.effect))))

## G6PD Frequency
cov.tmp = data.frame(matrix(0, nrow = 1000, ncol = length(cov.names) + 2))
names(cov.tmp) = c(cov.names, "value", "variable")
cov.tmp$g6pdd_frequency = seq(-2, 2, length.out = 1000)
cov.tmp$value = seq(-2, 2, length.out = 1000)
cov.tmp$variable = "G6PD Frequency"
cov.effect = fitted(horseshoe.fit, newdata = cov.tmp, allow_new_levels = TRUE,
                    sample_new_levels = "gaussian", summary = FALSE,
                    re_formula = NA) ## Ignore method and ISO
cov.effect.df = rbind(cov.effect.df, cbind(cov.tmp, colMeans(plogis(cov.effect))))

## Percent Urban (log)
cov.tmp = data.frame(matrix(0, nrow = 1000, ncol = length(cov.names) + 2))
names(cov.tmp) = c(cov.names, "value", "variable")
cov.tmp$urban = seq(-2, 2, length.out = 1000)
cov.tmp$value = seq(-2, 2, length.out = 1000)
cov.tmp$variable = "Log Percent Urban"
cov.effect = fitted(horseshoe.fit, newdata = cov.tmp, allow_new_levels = TRUE,
                    sample_new_levels = "gaussian", summary = FALSE,
                    re_formula = NA) ## Ignore method and ISO
cov.effect.df = rbind(cov.effect.df, cbind(cov.tmp, colMeans(plogis(cov.effect))))

## Precipitation Cold Quarter
cov.tmp = data.frame(matrix(0, nrow = 1000, ncol = length(cov.names) + 2))
names(cov.tmp) = c(cov.names, "value", "variable")
cov.tmp$wc2.1_2.5m_bio_19 = seq(-2, 2, length.out = 1000)
cov.tmp$value = seq(-2, 2, length.out = 1000)
cov.tmp$variable = "Prec Cold Quarter"
cov.effect = fitted(horseshoe.fit, newdata = cov.tmp, allow_new_levels = TRUE,
                    sample_new_levels = "gaussian", summary = FALSE,
                    re_formula = NA) ## Ignore method and ISO
cov.effect.df = rbind(cov.effect.df, cbind(cov.tmp, colMeans(plogis(cov.effect))))

## Solar Radiation
cov.tmp = data.frame(matrix(0, nrow = 1000, ncol = length(cov.names) + 2))
names(cov.tmp) = c(cov.names, "value", "variable")
cov.tmp$wc2.1_2.5m_srad_01 = seq(-2, 2, length.out = 1000)
cov.tmp$value = seq(-2, 2, length.out = 1000)
cov.tmp$variable = "Solar Radiation"
cov.effect = fitted(horseshoe.fit, newdata = cov.tmp, allow_new_levels = TRUE,
                    sample_new_levels = "gaussian", summary = FALSE,
                    re_formula = NA) ## Ignore method and ISO
cov.effect.df = rbind(cov.effect.df, cbind(cov.tmp, colMeans(plogis(cov.effect))))

## Walking time to nearest healthcare PW (log)
cov.tmp = data.frame(matrix(0, nrow = 1000, ncol = length(cov.names) + 2))
names(cov.tmp) = c(cov.names, "value", "variable")
cov.tmp$walking_healthcare_popweighted = seq(-2, 2, length.out = 1000)
cov.tmp$value = seq(-2, 2, length.out = 1000)
cov.tmp$variable = "Log Walking time to Healthcare PW"
cov.effect = fitted(horseshoe.fit, newdata = cov.tmp, allow_new_levels = TRUE,
                    sample_new_levels = "gaussian", summary = FALSE,
                    re_formula = NA) ## Ignore method and ISO
cov.effect.df = rbind(cov.effect.df, cbind(cov.tmp, colMeans(plogis(cov.effect))))




## Plot Estimates
names(cov.effect.df)[ncol(cov.effect.df)] = c("Estimate")
cov.effect.df$Estimate = cov.effect.df$Estimate * 100


cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
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





# Get sub-Saharan Africa FSW size estimates --------------------------------

post.fsw = post.transformed
for(i in 1:nrow(post.fsw)){
  post.fsw[i,] = post.fsw[i,] * gadm.cov$ref_pop
}

est.fsw.vec = rowSums(post.fsw)

## Estimated number of FSW in SSA
mean(est.fsw.vec)

## Lower and upper 95% credible interval of FSW in SSA
quantile(est.fsw.vec, probs = c(0.025, 0.975))


## And calculate in terms of percent of women of childbearing age
mean(est.fsw.vec) / sum(gadm.cov$ref_pop) * 100
quantile(est.fsw.vec, probs = c(0.025, 0.975))  / sum(gadm.cov$ref_pop) * 100





# Get country level data --------------------------------------------------

## Aggregate to country level
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


## Look at Nigeria
gadm.nga = subset(gadm.cov, ISO == "NGA")

head(gadm.nga@data[order(gadm.nga$Prev), c("NAME_1", "Prev")])
tail(gadm.nga@data[order(gadm.nga$Prev), c("NAME_1", "Prev")])


## Look at data availability for Malawi
pse.obs.mwi = subset(pse.obs, ISO == "MWI")
table(pse.obs.mwi$City, pse.obs.mwi$ID)


# Save Results ------------------------------------------------------------
save.image("../Data/Results/Final_Results.RData")

## Remove some columns from gadm.cov
gadm.cov@data = gadm.cov@data[,c("ID", "ISO", "NAME_0", "NAME_1", "ref_pop",
                                 "Uncertainty", "Upper", "Lower", "Fitted_FSW", "Prev")]
saveRDS(gadm.cov, "../Data/Results/Final_district_est.rds")
saveRDS(gadm.country, "../Data/Results/Final_country_est.rds")
saveRDS(horseshoe.fit, "../Data/Results/Final_model_fit.rds")
write.csv(gadm.cov@data, "../Data/Results/Final_district_est.csv")
write.csv(gadm.country@data, "../Data/Results/Final_country_est.csv")







###########################################################################
###########################################################################
## This ends the main results.
## The following code is additional diagnostics and secondary analysis
###########################################################################
###########################################################################

## Create Supplementary Table S3
## Rank country level random effects

country.ranef = data.frame(ranef(horseshoe.fit)$ISO)
country.ranef[order(country.ranef[,1,1]),]


## Rank estimated fixed effects
## Here we look at country FSW proportion from the predictors only
post.fixed = fitted(horseshoe.fit, newdata = pse.pred, allow_new_levels = TRUE,
                    sample_new_levels = "gaussian", summary = FALSE,
                    re_formula = NA) ## Ignore method and ISO
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
      rank(gadm.country$Percent), rank(gadm.country.fixed$Percent))[order(gadm.country$Percent),]

cbind(country.ranef[,1,1],rank(country.ranef[,1,1]))

gadm.country$Predictor_only = gadm.country.fixed$Percent
gadm.country$Predictor_only_rank = rank(gadm.country.fixed$Percent)

country.mat = data.frame(Country = gadm.country$NAME_0)
country.mat$`FSW proportion (percent)` = format(round(gadm.country$Percent, digits = 3), nsmall = 3)
country.mat$`FSW proportion from predictors only (percent)` = format(round(gadm.country$Predictor_only, digits = 3), nsmall = 3)
country.mat$`Predictor effect ordering` = gadm.country$Predictor_only_rank
country.mat$`Country effect` = NA
country.mat$`Country effect ordering` = NA

country.mat$`Country effect`[match(rownames(country.ranef), gadm.country$ISO)] = format(round(country.ranef$Estimate.Intercept, digits = 3), nsmall = 3)
country.mat$`Country effect ordering`[match(rownames(country.ranef), gadm.country$ISO)] = rank(country.ranef$Estimate.Intercept)

country.mat = country.mat[order(country.mat$`FSW proportion (percent)`),]
write.csv(country.mat, "../Data/Results/Final_country_est.csv", row.names = F)


# LOCO for regression coefficients ----------------------------------------



## Get intervals df for coef, method, and ISO
fixef.full = fixef(horseshoe.fit)
coef.full.df = data.frame(
  variable = c("Intercept", rownames(est.eff)),
  est = fixef.full[, "Estimate"],
  lower = fixef.full[, "Q2.5"],
  upper = fixef.full[, "Q97.5"]
)

method.full = ranef(horseshoe.fit)$method_std
method.full.df = data.frame(
  variable = rownames(method.full),
  est = method.full[, "Estimate", 1],
  lower = method.full[, "Q2.5", 1],
  upper = method.full[, "Q97.5", 1]
)


iso.full = ranef(horseshoe.fit)$ISO
iso.full.df = data.frame(
  variable = rownames(iso.full),
  est = iso.full[, "Estimate", 1],
  lower = iso.full[, "Q2.5", 1],
  upper = iso.full[, "Q97.5", 1]
)

## Now do LOO
## LOCO performed in Coefficient_sensitivity_LOCO.R to allow for parallel computing on a cluster
## Results read in here
n.country = length(unique(pse.obs$ISO))
ISO.vec = unique(pse.obs$ISO)

fixef.loco = matrix(NA, nrow = n.country, ncol = 70)
method.loco = matrix(NA, nrow = n.country, ncol = 8)
iso.loco = matrix(NA, nrow = n.country, ncol = n.country)

for(i in 1:n.country){
  fixef.tmp = readRDS(file = paste0("../Data/Results/LOO_Results/Coef/Fixef_sens_iso_",ISO.vec[i],"_ind_",i,".rds"))
  method.tmp = readRDS(file = paste0("../Data/Results/LOO_Results/Coef/Method_sens_iso_",ISO.vec[i],"_ind_",i,".rds"))
  iso.tmp = readRDS(file = paste0("../Data/Results/LOO_Results/Coef/ISO_sens_iso_",ISO.vec[i],"_ind_",i,".rds"))
  
  fixef.loco[i,] = fixef.tmp
  method.loco[i,] = method.tmp
  iso.loco[i,match(names(iso.tmp), ISO.vec)] = iso.tmp
}



colnames(fixef.loco) = c("Intercept", rownames(est.eff))
colnames(method.loco) = rownames(ranef(horseshoe.fit)$method_std)
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

# fixef.loco.melt$Var2[fixef.loco.melt$Var2 == "ref_pop_log"] = "Log Ref Pop"
# fixef.loco.melt$Var2[fixef.loco.melt$Var2 == "urban"] = "% Urban"
# fixef.loco.melt$Var2[fixef.loco.melt$Var2 == "cropland_rainfed"] = "% Rain Crop"
# fixef.loco.melt$Var2[fixef.loco.melt$Var2 == "wc2.1_2.5m_elev_popweighted"] = "Elevation"
# fixef.loco.melt$Var2[fixef.loco.melt$Var2 == "walking_healthcare_popweighted"] = "Healthcare"
# fixef.loco.melt$Var2[fixef.loco.melt$Var2 == "City:ref_pop_log"] = "City:Log Ref Pop"
# 
# fixef.loco.melt$Var2 = factor(fixef.loco.melt$Var2, levels = c("Intercept", "City", "Log Ref Pop",
#                                                                "City:Log Ref Pop", "% Urban", "% Rain Crop",
#                                                                "Elevation", "Healthcare"))





fixef.loco.melt = subset(fixef.loco.melt, Var2 != "Intercept")

fixef.loco.melt$Var2 = factor(fixef.loco.melt$Var2, levels = levels(est.eff.df$Predictor))
gg.fixef = ggplot(fixef.loco.melt) +
  geom_errorbar(
    mapping = aes(
      y = Var2,
      xmin = lower,
      xmax = upper,
      group = Var2
    ),
    width = 0.2,
    size = 1,
    col = "blue",
  ) +
  geom_boxplot(aes(x = value, y = Var2, group = Var2), varwidth = TRUE, outlier.size = 4, outlier.shape = 18) +
  geom_point(aes(x = est, y = Var2), col = "blue", size = 2) +
  theme_grey(base_size = 15) +
  theme(axis.text.y = element_text(hjust = 1, size = 20, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 23),
        title = element_text(size = 22, face = "bold")) +
  xlab("Estimate") + ylab("Predictor") +
  theme(plot.margin = margin(unit(c(5.5, 15.5, 5.5, 5.5), "points")))
gg.fixef

gg.method.eff = ggplot(method.loco.melt) +
  geom_errorbar(
    mapping = aes(
      y = Var2,
      xmin = lower,
      xmax = upper,
      group = Var2
    ),
    width = 0.2,
    size = 1,
    col = "blue",
  ) +
  geom_boxplot(aes(x = value, y = Var2, group = Var2), varwidth = TRUE, outlier.size = 4, outlier.shape = 18) +
  geom_point(aes(x = est, y = Var2), col = "blue", size = 5) +
  theme_grey(base_size = 15) +
  theme(axis.text.y = element_text(hjust = 1, size = 20, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"),
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
      y = Var2,
      xmin = lower,
      xmax = upper,
      group = Var2
    ),
    width = 0.4,
    size = 1,
    col = "blue",
  ) +
  geom_boxplot(aes(x = value, y = Var2, group = Var2), varwidth = TRUE, outlier.size = 3, outlier.shape = 18) +
  geom_point(aes(x = est, y = Var2), col = "blue", size = 3) +
  theme_grey(base_size = 15) +
  theme(axis.text.y = element_text(hjust = 1, size = 20, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        strip.text = element_text(size = 23),
        title = element_text(size = 22, face = "bold")) +
  ylab("Estimate") + xlab("Method")
gg.iso.eff


ggsave("../Figures/LOCO_fixef.jpg", gg.fixef, width = 10, height = 15, dpi = 600)
ggsave("../Figures/LOCO_method.jpg", gg.method.eff, width = 10, height = 10, dpi = 600)
ggsave("../Figures/LOCO_ISO.jpg", gg.iso.eff, width = 10, height = 15, dpi = 600)


