sigmoid = function(z){
  
  #Logistic Function
  #applies logistic function to 
  
  
  #if W,X specified:
  # z = crossprod(xW)
  
  
  sig = 1/(1 + exp(-z))
  
  
  #W,X specified
  
  
  return(sig)
  
}