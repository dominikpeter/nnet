
train <- function(model, ...) UseMethod("train")
query <- function(model, ...) UseMethod("query")

nnet <- function(inputnodes,
                      hiddennodes,
                      outputnodes,
                      activation_function = c("sigmoid", "tanh")
                      ) {
  inodes = inputnodes
  hnodes = hiddennodes
  onodes = outputnodes
  
  # init random weights
  wih <- array(rnorm(hnodes*inodes, sd = inodes^-.5), dim = c(hnodes, inodes))
  who <- array(rnorm(onodes*hnodes, sd = hnodes^-.5), dim = c(onodes, hnodes))
  
  activation_function = match.arg(activation_function)
  
  structure(list(layer = list(input = inodes,
                              hidden = hnodes,
                              output =  onodes),
                 weights = list(wih = wih, 
                                who =  who),
                 activation_function = activation_function),
            class = "nnet")
}



query.nnet <- function(model, inputs_list) {
  
  inputs <- matrix(inputs_list, nrow = model$layer$input)
  
  hidden_inputs <- model$weights$wih %*% inputs
  hidden_outputs <- do.call(model$activation_function,
                            list(x = hidden_inputs))
  
  final_inputs <- model$weights$who %*% hidden_outputs
  final_outputs <- do.call(model$activation_function,
                           list(x = final_inputs))
  
  list(inputs = inputs,
       hidden_inputs = hidden_inputs,
       hidden_outputs = hidden_outputs,
       final_inputs = final_inputs,
       final_outputs = final_outputs)
  
}

train.nnet <- function(model, inputs_list, target_list, learningrate) {
  qry_result <- query(model, inputs_list)
  
  inputs <- qry_result$inputs
  targets <- matrix(target_list, nrow = model$layer$output)
  
  who <- model$weights$who
  wih <- model$weights$wih
  hidden_output <- qry_result$hidden_output
  final_output <- qry_result$final_output
  lr <- learningrate
  
  output_errors <- targets - final_output
  hidden_errors <- t(who) %*% output_errors
  
  # list(output_errors = output_errors,
  #      hidden_errors = hidden_errors,
  #      hidden_output = hidden_output,
  #      inputs = inputs,
  #      who = model$weights$who,
  #      wih = model$weights$wih)
  
  model$weights$who <<- model$weights$who + (lr * (output_errors * final_output * (1.0 - final_output)) %*% t(hidden_output))
  model$weights$wih <<- model$weights$wih + (lr * (hidden_errors * hidden_output * (1.0 - hidden_output)) %*% t(inputs))
}



predict.nnet <- function(model, newdata) {
  query(model, newdata)$final_outputs
}



# Testing
# --------------------------------------------------------------------------------------------------


model <- nnet(4,3,2, "sigmoid")
model$weights


query(model, c(1,2,3,4))



train(model, 1:4, 1:2, 0.1)
model$weights

# Test Model
# --------------------------------------------------------------------------------------------------

library(magrittr)
library(scales)

input <- mtcars[, 1:7] %>% t %>% unname
output <- mtcars[, "am"]
output <- rbind(t(output), -1 %*% output + 1)

# number of input variables
niv <- nrow(input)

# number of output variables
nov <- nrow(output)

# rescale input 
input <- rescale(input)

# init nnet
model <- nnet(7, 3, 2, "sigmoid")

model


model$weights

output
input
model

train(model, input, output, learningrate = 0.1)



train(model, c(1:6), c(1:2), 0.1)
















