# Find highest correlation variables --------------------------------------
library(raster)
library(mice)
library(MASS)
library(spdep)

## Redo modeling, but with DHS

library(spdep)
library(plyr)
library(tmap)
library(lme4)
library(ggplot2)
library(brms)
library(optimx)


rm(list=ls())
pse.dhs = readRDS("../Data/PSE_Prep.rds")
pse.nondhs = readRDS("../Data/Final_brms_data_for_fitting.rds")
gadm.cov = readRDS("../Data/Shapefile_Prep.rds")
nb.mat = readRDS("../Data/nb_mat_Prep.rds")


## Remove large Nairobi estimate
which.rm = which(pse.dhs$Prev_comb > 0.6 &
                   pse.dhs$fsw_sizeestimate_region == "Nairobi") 
pse.dhs = pse.dhs[-which.rm,]

dhs.ind = 52:284
pse.dhs = pse.dhs[,dhs.ind]
## Remove variable with fewer than 15 unique values
num.unique = apply(pse.dhs, 2, function(x){length(unique(x))})
which.rm = which(num.unique < 15)
pse.dhs = pse.dhs[,-which.rm]

DHS.cor.vec = cor(pse.nondhs$logit_prev, pse.dhs, use = "pairwise.complete.obs")
which.keep = which(rank(-abs(DHS.cor.vec)) <= 30)
pse.dhs = pse.dhs[,which.keep]

## Combine DHS and nonDHS
pse.dat.fit = cbind(pse.nondhs, pse.dhs)
pse.dat.fit = pse.dat.fit[,-c(6, 8, 9, 10)]
pse.dat.fit = pse.dat.fit[complete.cases(pse.dat.fit),]
pse.dat.fit[,c(7:ncol(pse.dat.fit))] = scale(pse.dat.fit[,c(7:ncol(pse.dat.fit))])
spatial.ind = unique(pse.dat.fit$ID)
nondhs.cov.names = c(names(pse.dat.fit)[6:85], "City * ref_pop_log")
dhs.cov.names = names(pse.dat.fit)[88:117]

model.both.form = as.formula(paste0(
  "logit_prev ~ ",
  paste(c(nondhs.cov.names, dhs.cov.names), collapse = " + "),
  "+ (1 | method_std) + (1 | ISO)"
))

model.dhs.form = as.formula(paste0(
  "logit_prev ~ ",
  paste(c(dhs.cov.names), collapse = " + "),
  "+ (1 | method_std) + (1 | ISO)"
))

model.aux.form = as.formula(paste0(
  "logit_prev ~ ",
  paste(c(nondhs.cov.names), collapse = " + "),
  "+ (1 | method_std) + (1 | ISO)"
))

## LOAO Cross-validation performed in DHS_LOAO_07_1.R
## LOCO Cross-validation performed in DHS_LOCO_07_2.R


# LODO sequence -----------------------------------------------------------
## Normal vs t errors
n = nrow(pse.dat.fit)
ID.vec = unique(pse.dat.fit$ID)
n.ID = length(ID.vec)
ISO.vec = unique(pse.dat.fit$ISO)
n.ISO = length(ISO.vec)

both.mean.error = dhs.mean.error = aux.mean.error = c()

for(i in 1:n.ID){
  district.ind = which(pse.dat.fit$ID == ID.vec[i])
  pse.sub = pse.dat.fit[district.ind,]
  
  both.pred = readRDS(file = paste0("../Data/Results/LOO_Results/DHS/Both_pred_values_ID_",ID.vec[i],"_ind_",i,".rds"))
  dhs.pred = readRDS(file = paste0("../Data/Results/LOO_Results/DHS/DHS_pred_values_ID_",ID.vec[i],"_ind_",i,".rds"))
  aux.pred = readRDS(file = paste0("../Data/Results/LOO_Results/DHS/Aux_pred_values_ID_",ID.vec[i],"_ind_",i,".rds"))
  
  both.mean.error = c(both.mean.error, colMeans(both.pred) - pse.sub$logit_prev)
  dhs.mean.error = c(dhs.mean.error, colMeans(dhs.pred) - pse.sub$logit_prev)
  aux.mean.error = c(aux.mean.error, colMeans(aux.pred) - pse.sub$logit_prev)
  
  print(i)
}

mean(both.mean.error^2)
mean(dhs.mean.error^2)
mean(aux.mean.error^2)



# LOCO sequence -----------------------------------------------------------
## Normal vs t errors

both.mean.error = dhs.mean.error = aux.mean.error = c()

for(i in 1:n.ISO){
  country.ind = which(pse.dat.fit$ISO == ISO.vec[i])
  pse.sub = pse.dat.fit[country.ind,]
  
  both.pred = readRDS(file = paste0("../Data/Results/LOO_Results/DHS/Both_pred_values_iso_",ISO.vec[i],"_ind_",i,".rds"))
  dhs.pred = readRDS(file = paste0("../Data/Results/LOO_Results/DHS/DHS_pred_values_iso_",ISO.vec[i],"_ind_",i,".rds"))
  aux.pred = readRDS(file = paste0("../Data/Results/LOO_Results/DHS/Aux_pred_values_iso_",ISO.vec[i],"_ind_",i,".rds"))
  
  both.mean.error = c(both.mean.error, colMeans(both.pred) - pse.sub$logit_prev)
  dhs.mean.error = c(dhs.mean.error, colMeans(dhs.pred) - pse.sub$logit_prev)
  aux.mean.error = c(aux.mean.error, colMeans(aux.pred) - pse.sub$logit_prev)
  
  # print(i)
}

mean(both.mean.error^2)
mean(dhs.mean.error^2)
mean(aux.mean.error^2)







# Fit to full data --------------------------------------------------------

both.fit = brm(model.both.form, data = pse.dat.fit, cores = 3, chains = 3,
               iter = 6000, warmup = 4000,
               control = list(adapt_delta = 0.95),
               prior = prior(horseshoe(par_ratio = 0.1), class = "b"),
               family = student)

aux.fit = brm(model.aux.form, data = pse.dat.fit, cores = 3, chains = 3,
              iter = 6000, warmup = 4000,
              control = list(adapt_delta = 0.95),
              prior = prior(horseshoe(par_ratio = 0.1), class = "b"),
              family = student)


plot.df = data.frame(comb = fitted(both.fit), aux = fitted(aux.fit))


dhs.fitted.gg = ggplot(plot.df) + geom_point(aes(y = aux.Estimate,
                                                       x = comb.Estimate), size = 2) +
  geom_abline(intercept = 0, slope = 1, size = 1.2) +
  xlab("Combined Model") + ylab("Auxiliary Model") + 
  theme_grey(base_size = 15) +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        aspect.ratio = 1)
dhs.fitted.gg
ggsave("../Figures/Comb_vs_aux_fitted.jpg", dhs.fitted.gg, width = 10, height = 10)


# Check ability to predict missing values ---------------------------------
## First using multiple imputation



pse.dat.impute = gadm.cov@data
pse.dat.impute = pse.dat.impute[,which(names(pse.dat.impute) %in%
                                         c(dhs.cov.names,
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
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        title = element_text(size = 22, face = "bold"),
        aspect.ratio = 1) +
  xlim()
gg.impute
ggsave("../Figures/MICE_impute.jpg", gg.impute, width = 10, height = 10)


save.image("../Data/DHS/DHS_cart_imputation_results.RData")

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

## The save.image is useful to handle R crashes
save.image("../Data/DHS/Spatial_impute_prep.RData")
car.stan = rstan::stan_model("./R/Covariates 02/Sparse_CAR.stan")

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
plot(loo.est, 100*(phi.known.true - loo.est)/phi.known.true)
abline(h = 0, col = "red")
save.image(file = "../Data/DHS/DHS_spatial_imputation_results.RData")  

library(ggplot2)
truth.in = scale(phi.known.true)
df = data.frame(loo = (loo.est - attr(truth.in, "scaled:center"))/
                  attr(truth.in, "scaled:scale"),
                truth = truth.in)
loo.plot = ggplot(df) + geom_point(aes(x = loo, y = truth)) +
  xlab("Imputed Value") + ylab("Observed Value") +
  geom_abline(slope = 1, intercept = 0, size = 1.5) +
  theme_grey(base_size = 15) +
  theme(axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 18),
        title = element_text(size = 22, face = "bold"),
        aspect.ratio = 1) +
  xlim(min(df) - 0.01, max(df) + 0.01) + ylim(min(df) - 0.01, max(df) + 0.01)
loo.plot
ggsave("../Figures/Spatial_impute.jpg", loo.plot, width = 10, height = 10)