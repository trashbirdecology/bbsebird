# Null Model for BBS Data at Site-Level ----------------------------------------------------------

C <-  bbs.df %>%
  group_by(rteno, year) %>%
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


jdat.bbs <- list(C=C, M=M, T=T)
str(jdat.bbs)

# Initial Values ----------------------------------------------------------
inits <- function() list(route = rnorm(nrow(C)),
                         year =  c(NA, rnorm(ncol(C)-1))
)


# Parameters monitored ----------------------------------------------------
params <- c("route", "year", "popindex", "C")

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
fn.mod <- paste0(dir.jags,"/mod-bbs-null.txt")
cat(file=fn.mod, mod)

# Call JAGS ---------------------------------------------------------------
# library(jagsUI)
mod.out <- jagsUI::jags(data = jdat.bbs, inits = inits,
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



# Missing Values ----------------------------------------------------------
n.missing <- rowSums(is.na(C))
(perc.missing <- round(n.missing/ncol(C), digits = 2))
(na.sites <- which(n.missing>0))



# # Output ------------------------------------------------------------------
# par(mfrow=c(3,3))
# traceplot(mod.null)
#
# jags.View(mod.null)
# print(mod.null, 2)
#

# END RUN -----------------------------------------------------------------


