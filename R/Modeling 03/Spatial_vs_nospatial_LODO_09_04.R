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

## Perform LODO

n = nrow(pse.dat.fit)
ID.vec = unique(pse.dat.fit$ID)


district.ind = which(pse.dat.fit$ID == ID.vec[i])
pse.dat.lodo = pse.dat.fit
pse.dat.lodo$wt = 1
pse.dat.lodo$wt[district.ind] = 0


nospatial.fit = brm(model.form, data = pse.dat.lodo, cores = 3, chains = 3,
                 iter = 6000, warmup = 4000,
                 control = list(adapt_delta = 0.95),
                 prior = prior(horseshoe(par_ratio = 0.1), class = "b"),
                 family = student)


spatial.fit = brm(model.spatial.form, data = pse.dat.lodo, cores = 3, chains = 3,
            iter = 12000, warmup = 5000,
            control = list(adapt_delta = 0.99),
            prior = prior(horseshoe(par_ratio = 0.1), class = "b"),
            data2 = list(nb.mat = nb.mat),
            family = student)


nospatial.pred = predict(nospatial.fit, pse.dat.lodo[district.ind,], allow_new_levels = TRUE, sample_new_levels = "gaussian", summary = FALSE)
spatial.pred = predict(spatial.fit, pse.dat.lodo[district.ind,], allow_new_levels = TRUE, sample_new_levels = "gaussian", summary = FALSE)


saveRDS(nospatial.pred, file = paste0("Results/LOO_Results/Nospatial_pred_values_ID_",ID.vec[i],"_ind_",i,".rds"))
saveRDS(spatial.pred, file = paste0("Results/LOO_Results/Spatial_pred_values_ID_",ID.vec[i],"_ind_",i,".rds"))


