# MCMC specs --------------------------------------------------------------
na <- 1000 # number of adaptation iterations
nb <- 5000 # number of iterations for burnin per chain
nc <- 3 # number of chains (min for now)
ncores <- detectCores() - 1 # number of cores to use; keep one free just in case, or not
ni <- 10000 # number of iterations per chain
nt <- 10 # thinning rate

# Model -------------------------------------------------------------------
mod <- "
  model {
  ##################
  ### Priors
  ##################
  lambda ~ dunif(0, 100)         # Abundance rate
  theta ~ dunif(0, 100)          # Detection rate
  
  ##################
  ### Likelihoods
  ##################
  for (t in 1:T) {
    for (r in 1:R) {
    ## Total abundance
    N[r,t]    ~  dpois(mu[r,t])
    # mu[r,t] equals the weighted expected abundance
    mu[r,t]   <- sum(w.prop[r,t]/w.area[r,t]*lambda[r,t]) 
 
  ## Counts
  for(g in 1:G){

      C.route[r,t] ~ dpois(theta * N[t])         
      
      }#g
    }#r
  }#t
  
  ###########################
  ### Derived parameters
  ###########################
  
  ## Total abundance
  Ntot <- sum(N[1:T])
}#model
"

## save model to file
fn.mod <- paste0(dir.jags,"/nmix_pois.txt")
cat(file=fn.mod, mod)


# Initial Values ----------------------------------------------------------
inits <- function() list(year = c(NA, rnorm(ncol(C)-1)))


# Parameters monitored ----------------------------------------------------
params <- c("N", "lambda", "theta")

# Call JAGS ---------------------------------------------------------------
# library(jagsUI)
jags.data <- list(R=jdat$R, T=jdat$T, G=jdat$G, C=jdat$C.route)
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


plot(mod.out)




