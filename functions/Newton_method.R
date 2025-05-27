newtons_method_chol = function(X,y,W,m,iter_n =10){
  # sigmoid function
  
  # sigmoid function
  sigmoid  = function (X,W) {
    z <- X %*% W
    sig <- 1 / (1 + exp(-z) )
    return (sig)
  } 
  
  
  cost = function (X,y,W,m) {
    # cost function
    
    cost <- sum(-y * log(sigmoid(X,W)) - (1 - y) * log(1 - sigmoid(X,W)))
    return( cost/m)
  } 
  
  
  grad = function (X,y,W,m) {
    #gradient or first derivative of the cost function
    
    A <- sigmoid(X,W) - y
    return( crossprod(X,A)/m)
  }
  

  Hessian = function (X,W,m) {
    # Hessian
    
    #chol <- chol(t(X) %*% X )
    h_c <- diag(sigmoid(X,W)) *(1- diag((sigmoid(X,W))))
    hess <- (t(X)%*%X)* h_c
    #hess <- X%*%((diag(sigmoid(X,W)) - diag((sigmoid(X,W))^2))*t(X))
    return (hess/m)
  }
  
  cost_vector = c()
  i = 0
  while(i < iter_n) {

    chol <- chol(Hessian(X,W,m))
    inv_chol = chol2inv(chol)
    W = W - inv_chol%*% grad(X,y,W,m)
    cur_cost = cost(X,y,W,m)
    cost_vector = c(cost_vector, cur_cost)
    
    if(sqrt(crossprod(grad(X,y,W,m))) < 0.0001){break}
    i = i+1
    
  }
  return(list(W, cost_vector))
}