if(!exists("jdat")){
  rm(list=ls())
  source("4a_prep-jags-data.r")}

# MCMC specs --------------------------------------------------------------
na <- 1000 # number of adaptation iterations
nb <- 5000 # number of iterations for burnin per chain
nc <- 2 # number of chains (min for now)
ncores <- detectCores() - 1 # number of cores to use; keep one free just in case, or not
ni <- 10000 # number of iterations per chain
nt <- 100 # thinning rate


# Model -------------------------------------------------------------------
attach(jdat)
jags.data <- list(
  T=T, R=R, G=G,
  y=C.route,  # dims: [r,t]
  w=w.prop, # dims: [r,g]
  area=w.area, # dims: [g]
  cov1=rnorm(jdat$n.gridcellswbbs, 0,10)# dims: [g]
)
mod <- "
model{
### BBS Data
for (t in 1:T) {#years
  for (r in 1:R) {#routes
      y[r] ~ dpois(lambda.route[r])
      lambda.route[r] <- sum(w.prop[r,t,g]*lambda.grid[g])

    for(g in 1:G){#grids
        log(lambda.grid[g]) <-  log(w.area[g] + beta_0 + beta_1*cov1[g])
    }#g
  }#r
}#t

## Priors ##
beta_0 ~ dnorm(0,0.001) # intercept
beta_1 ~ dnorm(0,0.001) # dummy covar 1
# beta_2 ~ dnorm(0,0.001) # bbs covar: first-year observer effect

}#model

"

## save model to file
fn.mod <- paste0(dir.jags,"/mod.txt")
cat(file=fn.mod, mod)

# Initial Values ----------------------------------------------------------
inits <- function() list(year = c(NA, rnorm(ncol(jags.data$y)-1)))


# Parameters monitored ----------------------------------------------------
params <- c("lambda.route", "lambda.grid", "beta_1")



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


