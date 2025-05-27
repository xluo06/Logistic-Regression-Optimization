


conf_matrix = function(w,A,y){
  
  # w; vector of weights 
  # A; a data matrix
  # y: vector of true labels
  
  temp = A %*% w # applies weights to data matrix
  
  temp_pred = 1/(1 + exp(-temp)) # apply logistic function to get probabilities P(y|X,w)
  
  temp_pred_labels = ifelse(temp_pred >  0.5, 1, 0) # decision rule P(y|x) > 0.5 --> 1
  
  accuracy = sum(temp_pred_labels == y)/length(y) # how many predicted labels match true labels?
  
  confusion_matrix = table(temp_pred_labels, y) # matrix comparing pred labels to true 
  # labels; sum of diagonal = accuracy 
  
  return(list(accuracy, confusion_matrix))
  
  
}

