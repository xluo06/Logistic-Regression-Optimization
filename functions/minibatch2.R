
log_regr_minibatch = function(X,y, tol = 0.001, a = .1, batch_size ){
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
  
  
  
  b = 0 # bias term
  
  a = a # learning rate
  
  X = as.matrix(X)
  
  y = as.matrix(y)
  
  set.seed(0)
  
  n_mini_batches = m %/% batch_size
  
  i_rand = sample(1:m, m, replace = FALSE)
  
  # batches=vector(mode="list", length=n_mini_batches)
  # for (i in 1:n_mini_batches) {
  #   batches[[i]]=i_rand[(i-1)*batch_size+1:(i)*batch_size]
  # }
  
  batches = split(i_rand, factor(1:n_mini_batches))
  max_iter = 30000 # max iterations
  step = 0 # iteration counter
  
  
  
  
  
  
  while ((error > tol) && (step < max_iter)){
    for (i in 1:n_mini_batches) {
      
      
      w_prev = w_cur
      
      cur_i = batches[[i]]
      
      yhat = 1/(1 + exp(-(X[cur_i,]%*%w_prev + b)))
      
      
      g =   (1/batch_size) * t(yhat - y[cur_i,]) %*% X[cur_i,]
      
      w_cur = w_prev - a * t(g)
      
      
      error = as.double(sqrt(crossprod(t(g))))
      
      step = step + 1
      
      
      #add a break statement
      # print(w_cur)
      
    }
  }
  
  
  
  
  print(step)
  
  return(list(w_cur, error, g, yhat))
  
}

