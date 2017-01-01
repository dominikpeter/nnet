
nnet.query <- function(X, y, who, wih, af = "sigmoid") {

  hidden_input <- wih %*% X
  hidden_output <- do.call(af, list(x = hidden_input))
  
  final_input <- who %*% hidden_output
  final_output <- do.call(af, list(x = final_input))
  
  final_output
  
}


nnet <- function(formula, data, layer = 1, hidden = 10, af = "sigmoid", lr = 0.3) {
  m <- match.call()
  m[[1L]] <- quote(stats::model.frame)
  frame <- eval(m[1L:3L])
  y = as.matrix(frame[, 1])
  X = as.matrix(frame[, -1])
  
  input <- NROW(X)
  output <- NROW(y)

  wih <- matrix(rnorm(input*hidden, mean = 0, sd = 0.5),  ncol = input)
  who <- matrix(rnorm(hidden*output, mean = 0, sd = 0.5), ncol = hidden)

  hidden_input <- wih %*% X
  hidden_output <- do.call(af, list(x = hidden_input))
  
  final_input <- who %*% hidden_output
  final_output <- do.call(af, list(x = final_input))
  
  output_errors <- y - final_output
  hidden_errors <- t(who) %*% output_errors
  
  #update error 
  
  who <- who + lr * (output_errors * final_output * (1.0 - final_output)) %*% t(hidden_output)
  wih <- wih + lr * (hidden_errors * hidden_output * (1.0 - hidden_output)) %*% t(y)

  
  cbind(y, ifelse(nnet.query(X, y, who, wih, af = af)>0.5, 1, 0))
  
  
}


mtcars

formula <- am ~ hp

g <- nnet(formula, data = mtcars, hidden = 50, lr = 2)

mean(g[,1] == g[, 2])



