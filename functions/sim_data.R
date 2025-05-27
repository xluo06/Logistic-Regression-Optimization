sigmoid = function(z){
  
  #Logistic Function
  #applies logistic function to 
  
  
  #if W,X specified:
  # z = crossprod(xW)
  
  
  sig = 1/(1 + exp(-z))
  
  
  #W,X specified
  
  
  return(sig)
  
}





sim_data = function(p = 10 , n = 1000, s = 6){
  #Function that simulates logistic data
  
  # n; integer -- number of observations
  # p; integer -- number of covariates
  
  beta = seq(1,p)
  
  sigma_sq = s 
  
  X_sim = matrix(runif(n*p, -10,10), n, p)
  
  y_sim = as.vector(X_sim %*% beta + rnorm(n, mean = 0, sd = sqrt(sigma_sq)))
  
  y_sim = sapply(y_sim, sigmoid)
  
  y_sim = ifelse(y_sim  > 0.5, 1, 0)
  
  return(list(X_sim, y_sim))
}


