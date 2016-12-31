

nnet <- function(formula, data, layer = 1, hidden = 10, af = "sigmoid") {
  m <- match.call()
  m[[1L]] <- quote(stats::model.frame)
  frame <- eval(m[1L:3L])
  y = as.matrix(frame[, 1])
  X = as.matrix(frame[, -1])
  
  input <- NROW(X)
  output <- NROW(unique(y))

  wih <- matrix(rnorm(input*hidden, mean = 0, sd = 0.5),  ncol = input)
  who <- matrix(rnorm(hidden*output, mean = 0, sd = 0.5), ncol = hidden)

  hidden_input <- wih %*% X
  hidden_output <- do.call(af, list(x = hidden_input))
  
  final_input <- who %*% hidden_output
  final_output <- do.call(af, list(x = final_input))
  
  # error
  
  

}

formula <- am ~ hp

g <- nnet(formula, data = mtcars, hidden = 20)
g

w %*% t(mtcars$hp)

m


