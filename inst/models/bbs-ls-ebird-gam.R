mod.name <- paste0("bbs-ls-ebird-gam_", block.samp.type)


# WRITE NIMBLECODE MODEL --------------------------------------------------
code <- nimble::nimbleCode({
  ####################################################
  # BBS Data
  ####################################################
  for (i in 1:nobsb) {
    Cb[i]  ~  dpois(lambda.b[i] * pb[i])
    
    lambda.b[i]  <-    
      inprod(prop[siteb[i], 1:G], mu[1:G, yearb[i]]) 
    
    log(pb[i])   <-   eta * fyr[siteb[i], yearb[i]]       +  #1st YR OBS EFF
                            omega[rteobs[i]]                 #OBS-ROUTE EFF
  } 
  
  
  for(j in 1:nrteobs){
    omega[j] ~ dnorm(0, 1E-2)       # obs-route eff
  }
  eta   ~  dnorm(0, 1E-1) # first-year observer effect prior
 
  ####################################################
  # eBird Data
  ####################################################
  for (i in 1:nobse) {
    # Ce[i]  ~  dpois(lambda.e[i] * pe[i])
    # lambda.e[i]  <- mu[celle[i], yeare[i]]
    Ce[i]  ~  dpois(mu[celle[i], yeare[i]] * pe[i])
    
    log(pe[i])   <-   
      alpha.e                                   +
      alpha1.e * starttime[sitee[i], yeare[i]]  +  
      alpha2.e * mins[sitee[i], yeare[i]]       +
      alpha3.e * party[sitee[i], yeare[i]]
  }
  alpha.e    ~  dnorm(0, 1E-1)
  alpha1.e   ~  dnorm(0, 1E-1)
  alpha2.e   ~  dnorm(0, 1E-1)
  alpha3.e   ~  dnorm(0, 1E-1)
  
  
  ####################################################
  # Grid Expected
  ####################################################
  for (i in 1:ngy) {
    log(mu[gy[i, 1], gy[i, 2]])    <-  alpha[gy[i,2]] + 
                                       inprod( Z.mat[gy[i,1],1:K], b[1:K, gy[i,2]] )   
  }
  
  for(i in 1:K){
    mu.b[i] ~ dnorm(0, 0.01)
    b[i,1]  ~ dnorm(mu.b[i], tau.b)
    for(t in 2:T){
      b[i,t] ~ dnorm(b[i,t-1], tau.b)
    }
  }
  
  ####################################################
  ## Priors
  ####################################################
  tau.b       ~ dgamma(1E-2, 1)
  mu.alpha    ~ dnorm(0, 0.01)  
  for (i in 1:T) {
    alpha[i] ~ dnorm(mu.alpha, 0.01)
  }
  
  ####################################################
  ## Generated Qtys
  ####################################################
  sigma.b <- sqrt(1 / tau.b)
  # total N est by year
  for(t in 1:T){Ntot[t]    <- sum(mu[1:G,t])}
  
  })


# INITS -------------------------------------------------------------------
set.inits <- function(model.dat, keep){
  list.out <-  list(
    alpha     =  rnorm(model.dat$T), 
    alpha.bbs =  0, 
    alpha.e   =  0, 
    alpha1.e  =  0, 
    alpha2.e  =  0, 
    alpha3.e  =  0, 
    b         =  matrix(data=0, nrow=model.dat$K, ncol=model.dat$T),
    eta       =  rnorm(1), 
    # lambda.b  =  rnorm(model.dat$nobsb, 0, 10),
    # lambda.e  =  rnorm(model.dat$nobse, 0, 10),
    mu        =  matrix(data=0, nrow = model.dat$G, ncol = model.dat$T),
    mu.alpha  =  0, 
    mu.b      =  rnorm(model.dat$K, 0, 1E-2),
    omega     =  rnorm(model.dat$nrteobs, 0, 10),
    pb        =  rep(1, model.dat$nobsb), 
    pe        =  rep(1, model.dat$nobse),
    tau.b     =  rgamma(n=1, shape=2, rate=1), 
    tau.bbs   =  rgamma(n=1, shape=2, rate=1),
    tau.beta  =  rgamma(n=1, shape=2, rate=1) 
  )
  list.out <- pluck_multiple(list.out, keep=keep)
}

# INITS, DATA, CONSTANTS -----------------------------------------------
## Nimble performs better if initial values are provided for latent states
{params.inits <- c("alpha",
                   "alpha.e",
                   "alpha1.e",
                   "alpha2.e",
                   "alpha3.e",
                   "b", 
                   "eta", 
                   # "lambda.b",
                   # "lambda.e",
                   # "mu", 
                   # "mu.alpha", 
                   # "mu.b", 
                   "omega", 
                   # "pb", 
                   # "pe", 
                   "tau.b"
)

inits <- set.inits(model.dat, params.inits)
## check to ensure all desired inits available in inits..
stopifnot(params.inits %in% names(inits) || params.inits %in% names(inits[[1]]))

# Constants & Data 
rhs <-
  c("nobsb", "nobse", 
    "sitee","yeare", "celle", 
    "siteb","yearb","cellb",  "fyr",
    "nobsrb", "obsrb", 
    "rteobs", "nrteobs",
    'gy', 'ngy', 'G', 'T',
    'Z.mat', 'prop', "K", 
    "starttime", "mins", "party",
    "ref.year"
  )
lhs <- c("Cb", "Ce")  

# stopifnot(rhs %in% names(model.dat))
# stopifnot(lhs %in% names(model.dat))

data      <- pluck_multiple(model.dat, keep=lhs)  
constants <- pluck_multiple(model.dat, keep=rhs)  

monitors <- c("alpha", 
              "b",
              "eta",
              # "omega",
              "alpha1.e",
              "alpha2.e",
              "alpha3.e"
              )

dimensions <- NULL # not necessary unless using empty matrix/vec indexing
}

# # TEST MODEL BEFORE RUNNING ---------------------------------------------------
# FOR DEBUGGING
if(!exists("testmod")) testmod <- FALSE
if(testmod){
  b1 <- Sys.time()
  modtest     <- nimble::nimbleModel(code = code,
                                     constants = constants,
                                     inits = inits,
                                     data = data,
                                     dimensions = dimensions,
                                     debug=FALSE)
  b2 <- Sys.time()
  mcmctest    <- nimble::buildMCMC(modtest)
  b3 <- Sys.time()
  ## run uncompiled to have R errors and debugging available
  mcmctest$run(1) # will spit back issues w.r.t. logprobs, NAs, etc.
  b4 <- Sys.time()
  b3-b1
  b4-b1 # total runtime
  b2-b1 # compile model time
  b3-b2 # compile mcmc time
  b4-b3 # 1 iter run
  b5 <- Sys.time()
  mcmctest$run(100) # will spit back issues w.r.t. logprobs, NAs, etc.
  b6 <- Sys.time()
  b6-b5
  modtest$initializeInfo()
}
# 
# 
# # cell-year w NO DATA  ----------------------------------------------------------------
# ### need to add this into bbsebird::make_model_dat()

