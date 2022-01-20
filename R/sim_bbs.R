#' Simulate BBS Data for Use in JAGS
#'
#' Creates a list of BBS data for use in JAGS
#' @param propna proportion of all routes by year combinations to have no data (NA). this is a random process so will not always equal propna (uses rbinom to generate NAs)
#' @param propfyer proportion of routes by year combinations that comprise an observer's first year
#' @param nsite number of routes
#' @param nyear number of years
#' @param ngrid number of grid cells
#' @param zip TRUE will generate N using a zero-inflated poisson distribution, FALSE non-zip
#' @param maxN max number in the true state
#' @param probMinMultG probability of a route falling into multiple grid cells. probMinMultG should decrease as grid cell size increases (i.e., less chance of a rotue falling into multiple grid cells as spatial coverage of a cell increases)
sim_bbs <- function(nyear=20, nsite=10, ngrid=3, zip=TRUE, maxN=33, probMinMultG=0.75, probfyear=0.15, propna=0.25){

T    <- nyear
G    <- ngrid
M    <- nsite

# Simulate routes falling in multiple grids
## create a site by grid matrix for prop. route in a grid
prob <- prop <- matrix(NA, nrow=M, ncol=G)
for(j in 1:M){
  # for(i in 1:G){
  prob[j,] <- rbinom(n=G, size=1, prob = probMinMultG) #vector
}
## i think that because we supplied G as n in rbinom, each rowsum shuld be >0

# Simulate true state (N)
N <- matrix(nrow=M, ncol=T)
for(j in 1:M){
  if(zip)  N[j,] <- VGAM::rzipois(n=nyear, lambda=rpois(n=1, round(runif(1, 0, maxN))))
  if(!zip) N[j,] <- rpois(n=nyear, lambda=rpois(n=1, round(runif(1, 0, maxN))))
}

N[N=="NaN"] <- NA

# Simulate count data  (observations)
C <- matrix(nrow=M, ncol=T)
for(j in 1:M){
    if(zip)  C[j,] <- VGAM::rzipois(n=nyear, lambda=N[j,])
    if(!zip) C[j,] <- rpois(n=nyear, lambda=N[j,])
}
C[C=="NaN"] <- NA

## Impute missing values based on argument propyrsmissingperrte
C[matrix(rbinom(prod(dim(C)), size=1, prob=propna)==1, nrow=dim(C)[1])]<-NA

if(zip) sub="zero-inflated Poisson)" else{sub="Poisson)"}
plot(density(N, na.rm = TRUE), main= paste0("Simulated BBS Data \n(", sub))
lines(density(C, na.rm = TRUE), col="red")
legend("topright", c("N", "C"),
       col =c("black","red"), lty=1)


# Simulate detectability process
pc <- matrix(nrow=M, ncol=T) ##cars
pn <- matrix(nrow=M, ncol=T) ##noise
pw <- matrix(nrow=M, ncol=T) ##wind level
pf <- matrix(nrow=M, ncol=T) ##first year on bbs or route
for(j in 1:M){
  pc[j, ] <- VGAM::rzipois(n=T, lambda=rpois(n=1, round(runif(1, 0, 5))))
  pn[j, ] <- round(runif(n=T, min = 0, max=3))
  pw[j, ] <- round(runif(n=T, min = 0, max=3))
  pf[j, ] <- rbernoulli(n=T, p = probfyear)
  pf[pf==TRUE] <- 1
  pf[pf==FALSE] <- 0
}


# Simulate grid-level coviariates
hab1 <- rnorm(G, mean=0, sd=1)
hab2 <- scale(runif(G, 0, 100), center=TRUE)


list.out <- list(
  N=N,
  C=C,
  pc=pc,
  pn=pn,
  pw=pw,
  pf=pf,
  hab1=hab1,
  hab2=hab2,
  prop=prop
)
return(list.out)

} # END FUNCTION
