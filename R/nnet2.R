
train <- function(model, ...) UseMethod("train")
query <- function(model, ...) UseMethod("query")


nnet <- function(inputnodes,
                      hiddennodes,
                      outputnodes,
                      activation_function = c("sigmoid", "tahn")
                      ) {
  inodes = inputnodes
  hnodes = hiddennodes
  onodes = outputnodes
  
  wih <- array(rnorm(hnodes*inodes, sd = inodes^-.5), dim = c(hnodes, inodes))
  who <- array(rnorm(onodes*hnodes, sd = hnodes^-.5), dim = c(onodes, hnodes))
  
  activation_function = match.arg(activation_function)
  
  structure(list(wih = wih,
                 who =  who,
                 activation_function = activation_function),
            class = "nnet")
}



query.nnet <- function(model, inputs_list) {
  
  inputs <- array(inputs_list)
  
  hidden_inputs <- model$wih %*% inputs
  hidden_outputs <- do.call(model[["activation_function"]],
                            list(x = hidden_inputs))
  
  final_inputs <- model$who %*% hidden_outputs
  final_outputs <- do.call(model[["activation_function"]],
                           list(x = final_inputs))
  
  list(hidden_inputs = hidden_inputs,
       hidden_outputs = hidden_outputs,
       final_inputs = final_inputs,
       final_outputs = final_outputs)
  
}

train.nnet <- function(model, inputs_list, target_list, learningrate) {
  qry_result <- query(model, inputs_list)
  targets <- array(target_list, dim = c(length(target_list), 1))
  
  who <- model$who
  wih <- model$wih
  hidden_output <- qry_result$hidden_output
  final_output <- qry_result$final_output
  lr <- learningrate
  
  output_errors <- targets - final_output
  hidden_errors <- t(who) %*% output_errors
  
  model$who <<- who + (lr * (output_errors * final_output * (1.0 - final_output)) %*%
                         t(hidden_output))
  
  model$wih <<- wih + (lr * (output_errors * final_output * (1.0 - final_output)) %*%
                         t(hidden_output))

}


predict.nnet <- function(model, newdata) {
  query(model, newdata)$final_outputs
}



model <- nnet(6,6,6, "sigmoid")
train(model, c(1,2,3,1,1,2), c(0,0,1,0,1,0), 0.1)

