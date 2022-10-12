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


loco.fit = brm(model.form, data = pse.dat.in, cores = 3, chains = 3,
               iter = 6000, warmup = 4000,
               control = list(adapt_delta = 0.95),
               prior = prior(horseshoe(par_ratio = 0.1), class = "b"),
               family = student)


fixef.loco = fixef(loco.fit)[,"Estimate"]
method.loco = ranef(loco.fit)$method_std[,"Estimate", 1]
iso.loco = ranef(loco.fit)$ISO[,"Estimate", 1]


saveRDS(fixef.loco, file = paste0("Results/LOO_Results/Coef/Fixef_sens_iso_",ISO.vec[i],"_ind_",i,".rds"))
saveRDS(method.loco, file = paste0("Results/LOO_Results/Coef/Method_sens_iso_",ISO.vec[i],"_ind_",i,".rds"))
saveRDS(iso.loco, file = paste0("Results/LOO_Results/Coef/ISO_sens_iso_",ISO.vec[i],"_ind_",i,".rds"))


