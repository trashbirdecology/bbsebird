# DCCO integrated model with eBird and BBS data for NY
# Setup -------------------------------------------------------------------
library(parallel)
library(coda)
library(mgcv)
library(jagsUI)
library(rjags)


# Data In -----------------------------------------------------------------
## Load pre-processed (by Paige Howell) data for the state of NY
jdat <- readRDS("examples/NY/data/jdat_dcco_ipm_10km_NY_2008-2020_2021-06-16.rds")


# Create initial N ----------------------------------------------
Ni <- apply(jdat$E, c(1,2), max, na.rm=TRUE)
Ni[Ni=="-Inf"]<-0
for(i in 1:nrow(Ni)){
  for(t in 1:ncol(Ni)){
    if(Ni[i,t]>0)
      Ni[i,t]<-round(Ni[i,t]+(Ni[i,t]*0.8))
  }
}
hist(Ni)
hist(log(Ni))


## 10 eBird checklists, No BBS stops (just routes), 10 x 10 km grid cells, state of NY
## Fit an IPM without the intercepts on the eBird
## Change BBS detection function to logit scale
## Overall intercept on BBS detection function
## Single year, Duchon Spline, 20 basis functions, log.uniform prior on penalty, iiD


## GAM portion of the model
# Generate the appropriate prior distributions for a poisson gam
jagam.data <- data.frame(N = rep(N = Ni[,1], length(jdat$XY[,1])), x=scale(jdat$XY[,"x"]), y=scale(jdat$XY[,"y"]))
jagam.mod <- mgcv::jagam(N ~ s(x, y,
                               k = 20,       # number of basis functions (controls upper level of 'smoothness')
                               bs = 'ds',    # duchon spline
                               m=c(1, 0.5)), # duchon spline w/ 1st deriv penalty (penalizes wiggliness)
                         sp.prior = "log.uniform", # use gamma (default) or log.uniform priors on the lambda penalty term
                         diagonalize = TRUE, # Should smooths be re-parameterized to have i.i.d. Gaussian priors (where possible)?
                         data = jagam.data, family = "poisson", file=system.file("./jags/", "jdpois_tp.jags"))

# GAM ---------------------------------------------------------------------


## Add GAM components to model
jdat$Z = jagam.mod$jags.data$X # Transformed XY coords
jdat$nknots = ncol(jdat$Z)     # Number of knots
jdat$nYears = 1   # Number of years
jdat$nYearsB = 1  # Number of years with BBS data


## Initial values
jagam_inits <- function(){list(N=matrix(Ni[,1], nrow=nrow(Ni), ncol=1),

                               # Detection process
                               # ebird
                               gam1 = rnorm(1),
                               gam2 = rnorm(1),
                               gam3 = rnorm(1),
                               gam4 = rnorm(1),
                               gam5 = rnorm(1),
                               gam6 = rnorm(1),
                               gam7 = rnorm(1),
                               gam8 = rnorm(1),
                               gam9 = rnorm(1),

                               # BBS
                               alpha0 = rnorm(1),
                               alpha1 = rnorm(1),
                               alpha2 = rnorm(1),

                               # Ecological process
                               beta1 = rnorm(1),

                               # GAM
                               #sigma.gam=runif(1),
                               b = matrix(rep(jagam.mod$jags.ini$b, each = jdat$nYears),
                                          ncol = jdat$nYears, byrow = TRUE),
                               rho = jagam.mod$jags.ini$rho)}


## Parameters to monitor
jagam_params <- c("lambda", #"sigma.gam",
                  "b",

                  "beta1",

                  "gam1", "gam2", "gam3", "gam4", "gam5", "gam6", "gam7", "gam8", "gam9",

                  "alpha0",
                  "alpha1", "alpha2",

                  "Ntot"#,

                  #"N",

                  #"Ebirdnew.y", "BBSnew.y", "Hnew.sy"
)

# Todays date for saving files
tdat <- Sys.Date()

nC=3; nI=40000; nB=20000; nT=2; n.adapt = 10000

system.time({jags.fit.ipm.dcco.bs20.DS.iiD.logU.BBSint.logit.10km.NY.1yr.1 <- jagsUI::jags(data = jdat, parameters.to.save = jagam_params,
                                                                                           inits = jagam_inits,
                                                                                           model.file = "./jags/nmix_jagam_ipm_test.JAG",
                                                                                           n.chains = nC, n.iter = nI, n.adapt = n.adapt, n.burnin = nB, n.thin = nT,
                                                                                           modules=c("glm"), # For Guassin iid
                                                                                           parallel = TRUE, verbose = FALSE)})

saveRDS(jags.fit.ipm.dcco.bs20.DS.iiD.logU.BBSint.logit.10km.NY.1yr.1, file=paste0("./output/", "jags.fit.ipm.dcco.bs20.DS.iiD.logU.BBSint.logit.10km.NY.1yr.1_", tdat, ".rds"))

# Read in the output
a <- readRDS(file="./output/jags.fit.ipm.dcco.bs20.DS.iiD.logU.BBSint.logit.10km.NY.1yr.1_2021-06-23.rds")
a
plot(a, ask=TRUE)

