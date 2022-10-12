library(brms)

args = commandArgs(trailingOnly=TRUE)
i = as.numeric(args[1])

pse.dat.fit = readRDS(file = "Data/Final_brms_data_for_fitting.rds")
cov.names = readRDS(file = "Data/cov_names.rds")


model.form = as.formula(paste0("logit_prev ~ ",
                               paste(cov.names, collapse = " + "),
                               "+ (1 | method_std) + (1 | ISO)"))

## Perform LOCO

n = nrow(pse.dat.fit)
n.country = length(unique(pse.dat.fit$ISO))
ISO.vec = unique(pse.dat.fit$ISO)


country.ind = which(pse.dat.fit$ISO == ISO.vec[i])
pse.dat.in = pse.dat.fit[-country.ind,]
pse.pred = pse.dat.fit[country.ind,]

normal.fit = brm(model.form, data = pse.dat.in, cores = 3, chains = 3,
                    iter = 6000, warmup = 4000,
                    control = list(adapt_delta = 0.95),
                    prior = prior(horseshoe(par_ratio = 0.1), class = "b"))


t.fit = brm(model.form, data = pse.dat.in, cores = 3, chains = 3,
                    iter = 6000, warmup = 4000,
                    control = list(adapt_delta = 0.95),
                    prior = prior(horseshoe(par_ratio = 0.1), class = "b"),
                    family = student)


normal.pred = predict(normal.fit, pse.pred, allow_new_levels = TRUE, sample_new_levels = "gaussian", summary = FALSE)
t.pred = predict(t.fit, pse.pred, allow_new_levels = TRUE, sample_new_levels = "gaussian", summary = FALSE)


saveRDS(normal.pred, file = paste0("Results/LOO_Results/Normal_pred_values_iso_",ISO.vec[i],"_ind_",i,".rds"))
saveRDS(t.pred, file = paste0("Results/LOO_Results/t_pred_values_iso_",ISO.vec[i],"_ind_",i,".rds"))


