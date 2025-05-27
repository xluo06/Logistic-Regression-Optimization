



log_regr = function(X,y, tol = 0.001, a = 0.1, plotcost = FALSE ){
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
  
  m = nrow(X) # number of observations
  
  d = ncol(X) # dimension of data; number of variables
  
  w_cur = rep(1/d,d) # initialize weight vector
  
  error = 10 # initialize error
  
  tol = tol # tolerance
  
  max_iter = 30000 # max iterations
  
  step = 0 # iteration counter
  
  b = 0 # bias term
  
  a = a # learning rate
  
  X = as.matrix(X)
  
  y = as.matrix(y)
  
  cost_vector = c()
  
  while ((error > tol) && (step < max_iter)){
    
    # prev = cur 
    # 
    # cur = prev - a * grad((Loss(X,w,y)))
    # 
    # 
    w_prev = w_cur
    
    
    yhat = 1/(1 + exp(-(X%*%w_prev + b)))
    
    
    
    g =  (1/m) * t(yhat - y) %*% X
    
    w_cur = w_prev - a * t(g)
    
    
    error = as.double(sqrt(crossprod(t(g))))
    
    step = step + 1
    
    # cost = -sum((y[cur_i,] * log(yhat[1,1])) + ((1-y[cur_i,]) * log(1-yhat[1,1])))/m
    # 
    # cost_vector = c(cost_vector, cost)
    
    cost = -sum((t(y) %*% log(yhat)) + (t(1-y) %*% log(1-yhat)))/m
    cost_vector = c(cost_vector, cost)
    
    
    
    
  }
  print(step)
  
  return(list(w_cur, error, g, yhat, cost_vector))
  
}

