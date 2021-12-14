# Simplest model ----------------------------------------------------------
## Non-spatially explicit,
## C at the grid-level with route-level effects

C <-  bbs.df %>%
  group_by(rteno, year. id) %>%
  mutate(C = sum(C)) %>%
  ungroup() %>%
  distinct(rteno, year, C) %>%
  pivot_wider(id_cols=rteno, names_from = year,
              values_from = "C",
              values_fill = NA) %>%
  tibble::column_to_rownames(var="rteno") %>%
  simplify2array() # this removes rownames but that aight


G.ids <- sort(unique(bbs.df$id)) # Grid cell identities
M.ids <- sort(unique(bbs.df$rteno)) # RTENO (ctry-state-route id)
T.ids <- sort(unique(bbs.df$year))
G     <- length(G.ids) # number of grid cells (with BBS data)
M     <-  length(M.ids)
T     <- length(T.ids)


### until I can figure out the issue with missing values just forcing NA to zero...
# C[is.na(C)] <- 0

jdat.bbs <- list(C=C, M=M, T=T)
str(jdat.bbs)

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
  route[i] ~ dnorm(0, 0.001) # prior for route-level effects
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


