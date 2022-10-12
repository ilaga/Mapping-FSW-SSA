
library(brms)

args = commandArgs(trailingOnly=TRUE)
i = as.numeric(args[1])

pse.dhs = readRDS("Data/PSE_Prep.rds")
pse.nondhs = readRDS("Data/Final_brms_data_for_fitting.rds")


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


## Perform LOCO
n = nrow(pse.dat.fit)
n.country = length(unique(pse.dat.fit$ISO))
ISO.vec = unique(pse.dat.fit$ISO)

country.ind = which(pse.dat.fit$ISO == ISO.vec[i])
pse.dat.in = pse.dat.fit[-country.ind,]
pse.pred = pse.dat.fit[country.ind,]

both.fit = brm(model.both.form, data = pse.dat.in, cores = 3, chains = 3,
            iter = 6000, warmup = 4000,
            control = list(adapt_delta = 0.95),
            prior = prior(horseshoe(par_ratio = 0.1), class = "b"),
            family = student)

dhs.fit = brm(model.dhs.form, data = pse.dat.in, cores = 3, chains = 3,
               iter = 6000, warmup = 4000,
               control = list(adapt_delta = 0.95),
               prior = prior(horseshoe(par_ratio = 0.1), class = "b"),
               family = student)

aux.fit = brm(model.aux.form, data = pse.dat.in, cores = 3, chains = 3,
               iter = 6000, warmup = 4000,
               control = list(adapt_delta = 0.95),
               prior = prior(horseshoe(par_ratio = 0.1), class = "b"),
               family = student)


both.pred = predict(both.fit, pse.pred, allow_new_levels = TRUE, sample_new_levels = "gaussian", summary = FALSE)
dhs.pred = predict(dhs.fit, pse.pred, allow_new_levels = TRUE, sample_new_levels = "gaussian", summary = FALSE)
aux.pred = predict(aux.fit, pse.pred, allow_new_levels = TRUE, sample_new_levels = "gaussian", summary = FALSE)


saveRDS(both.pred, file = paste0("Results/LOO_Results/DHS/Both_pred_values_iso_",ISO.vec[i],"_ind_",i,".rds"))
saveRDS(dhs.pred, file = paste0("Results/LOO_Results/DHS/DHS_pred_values_iso_",ISO.vec[i],"_ind_",i,".rds"))
saveRDS(aux.pred, file = paste0("Results/LOO_Results/DHS/Aux_pred_values_iso_",ISO.vec[i],"_ind_",i,".rds"))