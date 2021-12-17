# if(!exists("jdat")){
#   rm(list=ls())
#   source("4a_prep-jags-data.r")}

# MCMC specs --------------------------------------------------------------
na <- 1000 # number of adaptation iterations
nb <- 5000 # number of iterations for burnin per chain
nc <- 2 # number of chains (min for now)
ncores <- detectCores() - 1 # number of cores to use; keep one free just in case, or not
ni <- 10000 # number of iterations per chain
nt <- 100 # thinning rate


# Model -------------------------------------------------------------------
# mod <- "model{
## Likelihood

  ### BBS Data
  for (t in 1:T) {
    for (r in 1:R) {

        C[r,t] ~ dpois(mu[r,t])
        mu[r,t]      <- sum(w.prop[r,t]/w.area[r,t]*lambda) # weighted expected abundance (wt by proportion of route in grid cell)
        # mu[r,t] <-  sum(w[,r] * lambda) #

    for(g in 1:G){
          ## expected abundance on grid g equals the...
            ## ... sum of (counts within each grid cell ...
            ## ... weighted by the number of stops in each grid cell)
          log(lambda[g]) <-  beta_0 +
                                  beta_1*cov_1[g]  ## habitat dummy covar that doesn't change over time
                                  beta_2*cov_2[r,t]  ## observation covar for demonstration (this is a detect. covar.)

           } # end G loop (grid cells)

        } # end R loop (routes)

    } # end T loop (years)




## Priors ##
beta_0 ~ dnorm(0,0.001) # intercept
beta_1 ~ dnorm(0,0.001) # dummy covar 1
beta_2 ~ dnorm(0,0.001) # bbs covar: first-year observer effect


# }"

## save model to file
fn.mod <- paste0(dir.jags,"/mod.txt")
cat(file=fn.mod, mod)



# Initial Values ----------------------------------------------------------
inits <- function() list(year = c(NA, rnorm(ncol(C)-1)))


# Parameters monitored ----------------------------------------------------
params <- c("route", "year", "popindex", "grid")



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


