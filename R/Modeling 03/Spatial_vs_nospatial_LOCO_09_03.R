library(brms)

args = commandArgs(trailingOnly=TRUE)
i = as.numeric(args[1])

pse.dat.fit = readRDS(file = "Data/Final_brms_data_for_fitting.rds")
cov.names = readRDS(file = "Data/cov_names.rds")
nb.mat = readRDS("Data/nb_mat_Prep.rds")


model.form = as.formula(paste0("logit_prev | weights(wt) ~ ",
                               paste(cov.names, collapse = " + "),
                               "+ (1 | method_std) + (1 | ISO)"))

model.spatial.form = as.formula(paste0("logit_prev | weights(wt) ~ ",
                                       paste(cov.names, collapse = " + "),
                                       "+ (1 | method_std) + (1 | ISO)",
                                       "+ car(nb.mat, gr = ID, type = 'icar')"))

## Perform LOCO

n = nrow(pse.dat.fit)
n.country = length(unique(pse.dat.fit$ISO))
ISO.vec = unique(pse.dat.fit$ISO)


country.ind = which(pse.dat.fit$ISO == ISO.vec[i])
pse.dat.loco = pse.dat.fit
pse.dat.loco$wt = 1
pse.dat.loco$wt[country.ind] = 0


nospatial.fit = brm(model.form, data = pse.dat.loco, cores = 3, chains = 3,
                    iter = 6000, warmup = 4000,
                    control = list(adapt_delta = 0.95),
                    prior = prior(horseshoe(par_ratio = 0.1), class = "b"),
                    family = student)


spatial.fit = brm(model.spatial.form, data = pse.dat.loco, cores = 3, chains = 3,
                  iter = 12000, warmup = 5000,
                  control = list(adapt_delta = 0.99),
                  prior = prior(horseshoe(par_ratio = 0.1), class = "b"),
                  data2 = list(nb.mat = nb.mat),
                  family = student)


nospatial.pred = predict(nospatial.fit, pse.dat.loco[country.ind,], allow_new_levels = TRUE, sample_new_levels = "gaussian", summary = FALSE)
spatial.pred = predict(spatial.fit, pse.dat.loco[country.ind,], allow_new_levels = TRUE, sample_new_levels = "gaussian", summary = FALSE)


saveRDS(nospatial.pred, file = paste0("Results/LOO_Results/Nospatial_pred_values_ISO_",ISO.vec[i],"_ind_",i,".rds"))
saveRDS(spatial.pred, file = paste0("Results/LOO_Results/Spatial_pred_values_ISO_",ISO.vec[i],"_ind_",i,".rds"))


