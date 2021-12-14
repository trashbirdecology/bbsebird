rm(list=ls())
source("4a_prep-jags-data.r")

### How to handle RTENOs where no data and no effort (i.e. all RTENO observations across time are NA???)

# Data Munge ----------------------------------------------------------
## BBS-------------
### count data
C <-  acast(bbs.df, id~year~rteno, value.var="C") # C: <array> id by year sliced by rteno, value = C (BBS count)

### detection covariates
coverage <- acast(bbs.df, id~year~rteno, value.var="proprouteincell") #coverage: <array> id by year sliced by rteno, value = % of BBS route in the grid cell; proportion of the route within a grid cell (id)
car <- acast(bbs.df, id~year~rteno, value.var="car.z") #wind: <array> id by year sliced by rteno, value = scaled average number of cars detected during the route run each year; covariate on detection for bbs (cars; scaled)
noise <- acast(bbs.df, id~year~rteno, value.var="noise.z") #noise: <array> id by year sliced by rteno, value = scaled average  noise level during the route run each year (noise; scaled)
wind <- acast(bbs.df, id~year~rteno, value.var="wind.z") #wind: <array> id by year sliced by rteno, value = scaled average wind level during the route run each year (wind; scaled)
## need to update upstream workflow to create scaled covars for: sky, temp....assistant (need to change NULL-->NA)

### trend effects
ydays <- acast(bbs.df, id~year~rteno, value.var="yday") #ydays: <array> id by year by rteno sliced    indicator for day of year BBS conducted

### habitat covariates


### indexes
G.ids <- sort(unique(grid$id)) # grid cell identities (study area grid cells)
M.ids <- sort(unique(bbs.df$rteno)) # unique BBS route identifiers (rteno; comprises unique ctry-state-route id)
T.ids <- sort(unique(bbs.df$year)) # years of BBS data
G     <- length(G.ids) # number of grid cells (with BBS data)
M     <- length(M.ids) # number of unique BBS routes (rteno)
T     <- length(T.ids) # number of years with BBS data




# Notes -------------------------------------------------------------------
## Important 1: access arrays C, coverage, car, noise, wind, ydays, by
## Important 1: calling array[nrow, ncol, nslice]
## Important 1: e.g., to access the first array element, call `array[,,1]`
## Important 2: remember that JAGS uses PRECISION and NOT VARIANCE paramters in distributions
## Important 2: e.g., dnorm(0,.001) means precision==0.001 and variance ==1,000!!!



# Make JAGS Data Object ---------------------------------------------------
jdat.bbs <- list(C=C, M=M, T=T, G=G)


# Initial Values ----------------------------------------------------------
inits <- function() list(route = rnorm(nrow(C)),
                         year =  c(NA, rnorm(ncol(C)-1))
)


# Parameters monitored ----------------------------------------------------
params <- c("route", "year", "popindex")

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
  route[i] ~ dnorm(0, 0.001) # prior for route-level effects (parameter is precision and not variance in JAGS for dnorm; i.e. 0.001prec==1000variance)
}
  year[1] <- 0 # year 1

for(t in 2:T){ # year > 1
  year[t] ~ dnorm(0, 0.001)
}

## Likelihood ##
for(i in 1:M){
  for(t in 1:T){
    C[i,t] ~ dpois(lambda[i,t]) # bbs count route(site) i year t
    log(lambda[i,t]) <- route[i] + year[t]
  }
}


## Derived ##
for(t in 1:T){
   popindex[t] <- sum(lambda[,t])
}
}"

## save model to file
fn.mod1 <- paste0(dir.jags,"/route-level-pois-glm.txt")
cat(file=fn.mod1, mod)

# Call JAGS ---------------------------------------------------------------
# library(jagsUI)
out.mod1 <- jagsUI::jags(data = jdat.bbs, inits = inits,
                         parameters.to.save = params,
                         n.adapt = na,
                         n.burnin = nb,
                         n.chains = nc,
                         n.cores = ncores,
                         n.iter = ni,
                         n.thin = nt,
                         parallel = TRUE,
                         model.file = fn.mod1
)


# Output ------------------------------------------------------------------
par(mfrow=c(3,3))
traceplot(out.mod1)

jags.View(out.mod1)
print(out.mod1, 2)


# END RUN -----------------------------------------------------------------


