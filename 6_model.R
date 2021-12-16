rm(list=ls())
source("4a_prep-jags-data.r")

# MCMC specs --------------------------------------------------------------
na <- 1000 # number of adaptation iterations
nb <- 5000 # number of iterations for burnin per chain
nc <- 2 # number of chains (min for now)
ncores <- detectCores() - 1 # number of cores to use; keep one free just in case, or not
ni <- 10000 # number of iterations per chain
nt <- 100 # thinning rate

# Pull only necessary data ------------------------------------------------

## specify or make data
G = dim(jdat$C.bbs.array)[1] # <site/grid cell> number of unique grid cells sampled by BBS data
R = dim(jdat$C.bbs.df)[1] # <route> number of unique BBS routes with data
T = dim(jdat$C.bbs.df)[2] # <time> number of years of data


## make jags data list
jags.data <- list(G=G, # number of grid cells with BBS data
                  R=R, # number of unique routes sampled across all BBS data
                  T=T, # number of years of BBS data
                  C.bbs.df = jdat$C.by.grid, # <matrix> indexing== matrix[R, T] (route, year)
                  C.bbs.array = jdat$C.bbs.array, # <array> # <array> bbs count data (grid cell by year sliced by RTENO); indexing== array[M, T, R] (site, year, route)
                  nRoutesPerYear=jdat$n.routesPerYear ,  # <vector> number of BBS routes sampled each year
                  w=jdat$w, # <array> dims grid by route
                  bbs.observer.experience = jdat$bbs.observer.experience, # <matrix> route by year; 0==not first year, 1==first year
                  cov_1 = rnorm(n = G, 0, 10) # random covariate for now
                  )



# Model -------------------------------------------------------------------
# mod <- "model{
## Likelihood
for (t in 1:T) {

  for (r in 1:R) {

      y[r,t] ~ dpois(mu[j,t])

      mu[r,t] <-  sum(w[,r] * lambda.grid) #


    } # end R loop (routes)


  for(g in 1:G){
    for(r in 1:R){

  # lambda.grid dim = grid id
          log(lambda.grid[g]) <-  beta_0 + beta_1*cov_1[g] + beta_2*cov.bbs.obs.fyer[r,t]

        } # end R loop #2
    } # end G loop

} # end T loop

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


