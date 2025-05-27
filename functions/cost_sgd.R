
log_regr_stoch_cost = function(X,y, tol = 0.00001, a = .1, max_iter = 30000, costplot = FALSE){
  # Function that takes in:
  #X, a data matrix (each row an observation)
  #y, a vector of labels (each row an observation)
  #m, a batch size (1, minibatch, n)
  #a, a specified learning rate
  #tol, a stopping criterion
  
  
  # Function performs gradient descent algorithm to obtain the 
  # vector of weights w* that minimize the log likelihood
  # function of the data (gives MLE) 
  
  # Note: MLE for logistic regression doesnt have a closed form solution.
  
  m = nrow(X)
  
  # number of observations
  
  d = ncol(X) # dimension of data; number of variables
  
  w_cur = rep(0,d) # initialize weight vector
  
  error = 10 # initialize error
  
  tol = tol # tolerance
  
  max_iter = max_iter # max iterations
  
  step = 0 # iteration counter
  
  b = 0 # bias term
  
  a = a # learning rate
  
  X = as.matrix(X)
  
  y = as.matrix(y)
  
  cost_vector= c()
  
  set.seed(0)
  
  i_rand = sample(1:m, m, replace = FALSE)
  
  while ((error > tol) && (step < max_iter)){
    
    
    
    w_prev = w_cur
    
    #cur_i = i_rand[step + 1%%m]
    
    cur_i = i_rand[(step%%m + 1)]
    
    yhat = 1/(1 + exp(-(X[cur_i,]%*%w_prev + b)))
    
    
    
    g =   t(yhat - y[cur_i,]) %*% X[cur_i,]
    
    #cost function

    cost = -((y[cur_i,] * log(yhat[1,1])) + ((1-y[cur_i,]) * log(1-yhat[1,1])))
    cost_vector = c(cost_vector, cost)

    
    w_cur = w_prev - a * t(g)
    
    error = as.double(sqrt(crossprod(t(g))))
    
    if(is.na(error)){
      break
    }
    
    step = step + 1
    
    

  }
  
  
  return(list(w_cur, error, g, yhat, cost_vector))
  
}




