sim_bbs <- function(nyear=20, nsite=10, ngrid=3, zip=TRUE, maxN=33, probMinMultG=0.35, probfyear=0.15){

T    <- nyear
G    <- ngrid
M    <- nsite  
  
# Simulate routes falling in multiple grids
## create a site by grid matrix for prop. route in a grid
prop <- matrix(nrow=M, ncol=G)
for(i in 1:G){
  prop[,i] <- rbinom(n=M, size=1, prob = probMinMultG) #vector
  for(j in 1:M){
    prop1 = runif(1, min=0, max=1)
    prop2 = runif(1, min=0, max=1-prop1)
    prop3 = 1-prop2-prop1
    prop[j, ] <- c(prop1, prop2, prop3) 
  }
}

# Simulate true state (N)
N <- matrix(nrow=M, ncol=T)
for(j in 1:M){
  if(zip)  N[j,] <- VGAM::rzipois(n=nyear, lambda=rpois(n=1, round(runif(1, 0, maxN))))
  if(!zip) N[j,] <- rpois(n=nyear, lambda=rpois(n=1, round(runif(1, 0, maxN))))
}

N

# Simulate count data  (observations)  
C <- matrix(nrow=M, ncol=T)
for(j in 1:M){
    if(zip)  C[j,] <- VGAM::rzipois(n=nyear, lambda=N[j,])
    if(!zip) C[j,] <- rpois(n=nyear, lambda=N[j,])
}

if(zip) sub="zero-inflated Poisson)" else{sub="Poisson)"}
plot(density(N), main= paste0("Simulated BBS Data \n(", sub))
lines(density(C), col="red")
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
  hab2=hab2
  
)
return(list.out)

} # END FUNCTION