for(t in 1:T){ # loop over years with BBS data
  
  for(r in 1:R){  # loop over each route, each year
    
    # Count data: Number of DCCO observed on each route, each year
    C.bbs.df[r, t] ~ dpois(lambda.route[r, t]])
    
    # For each route, sum up the area weighted EN for all the grid cells it lands in
    lambda.route[r,t] <- sum(lambda.grid[t, r])
    
    lambda.route[t, Brsite[t, r]] <-
      sum(lambda.grid[t, 
                      Brsite[t, r], 
                      Brgrid[Brsite[t, r], 1:nGridswithRoutes[Brsite[t, r], t], t]
                      ])
    

    for(i in 1:nGridswithRoutes[Brsite[t,r],t]){ # loop over grid cells where routes were present
      
      # Expected abundance on each route = sum of counts within each grid cell, weighted by the number of stops in each grid cell (effort)
      # Below is correct, but won't work with rags syntax
      #lambda.route[t,r] <- sum(EN[i,t]*propStops[t,Brsite[t,r],Brgrid[r,i,t]])
      
      # Here we have to trick rags and index lambda.route by the grid cell
      
      # EN x propStops = Weighting the EN by the proportion of the grid cell area sampled by a route
      # lambda.grid = Stores EN x propStops for each grid cell that a route falls in, each year
      
      lambda.grid[t,Brsite[t,r],Brgrid[Brsite[t,r],i,t]] <- EN[Brsite[t,i],t] * propStops[t,Brsite[t,r],Brgrid[Brsite[t,r],i,t]]
    }
  }
  
  # Sum the perfect data set over routes to get annual counts for each survey protocol (BBS routes)
  # Hnew.sy[t] <- sum(C.bbs.dfnew[t,Brsite[t,1:R[t]]])
  
}