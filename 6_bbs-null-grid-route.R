rm(list=ls())
source("4a_prep-jags-data.r")


# Pull only necessary data ------------------------------------------------

## specify desired data
M = jdat$n.GridCellswBBS # <site> number of unique grid cells sampled by BBS data
T = jdat$n.years.bbs # <time> number of years of data
C = jdat$N_i_bbs ## this isnt correct
# N_obs: observed max abundance on BBS data across grids
N_obs_bbs <- sum(apply(jdat$N_i_bbs, 1, max, na.rm=TRUE))


## make jags data list
jags.data <- list(M=M,
                  T=T,
                  C=C,
                  N = N_obs_bbs)


# Initial Values ----------------------------------------------------------
inits <- function() list(site = rnorm(nrow(C)),
                         year =  c(NA, rnorm(ncol(C)-1))
)


# Parameters monitored ----------------------------------------------------
params <- c("site", "year", "popindex")

# MCMC specs --------------------------------------------------------------
na <- 1000 # number of adaptation iterations
nb <- 5000 # number of iterations for burnin per chain
nc <- 3 # number of chains
ncores <- detectCores() - 1 # number of cores to use
ni <- 10000 # number of iterations per chain
nt <- 100 # thinning rate

# Model -------------------------------------------------------------------
## specify model
mod <- "model{

## Priors  ##
for(i in 1:M){
  site[i] ~ dnorm(0, 0.001) # prior for grid-level effects (parameter is precision and not variance in JAGS for dnorm; i.e. 0.001prec==1000variance)
}
  year[1] <- 0 # year 1

for(t in 2:T){ # year > 1
  year[t] ~ dnorm(0, 0.001)
}

## Likelihood ##
for(i in 1:M){
  for(t in 1:T){
    C[i,t] ~ dpois(lambda[i,t]) # grid cell (site) i year t
    log(lambda[i,t]) <- site[i] + year[t]
  }
}


## Derived ##
for(t in 1:T){
   popindex[t] <- sum(lambda[,t])
}
}"

## save model to file
fn.mod <- paste0(dir.jags,"/grid-level-pois-glm.txt")
cat(file=fn.mod, mod)

# Call JAGS ---------------------------------------------------------------
# library(jagsUI)
mod.out <- jagsUI::jags(data = jags.data, inits = inits,
                         parameters.to.save = params,
                         n.adapt = na,
                         n.burnin = nb,
                         n.chains = nc,
                         n.cores = ncores,
                         n.iter = ni,
                         n.thin = nt,
                         parallel = TRUE,
                         model.file = fn.mod
)


# Output ------------------------------------------------------------------
par(mfrow=c(3,3))
traceplot(mod.out)

jags.View(out.mod1)
print(out.mod1, 2)


# END RUN -----------------------------------------------------------------


