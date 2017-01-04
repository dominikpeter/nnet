
# get datadata

library(data.table)
library(magrittr)
library(ggplot2)
library(scales)
library(dummies)
library(foreach)
library(parallel)

parallel::makeCluster(4)


training <- fread("http://pjreddie.com/media/files/mnist_train.csv")
testing <- fread("http://pjreddie.com/media/files/mnist_test.csv")


# training <- fread("datasets/mnist_train_100.csv")
# testing <- fread("datasets/mnist_test_10.csv")  

setnames(training, c("label", 1:(ncol(training)-1)))
setnames(testing, c("label", 1:(ncol(testing)-1)))

# init
length(unique(training$label))

model <- nnet(784, 200, 10, activation_function = "sigmoid")



it <- 10L
errors <- vector(mode = "numeric", it)
i <- 1L

# while(i <= it) {
#   SSE <- train(model, input, output, learningrate = 0.3)
#   errors[i] <- SSE
#   i <- i + 1L
# }

errors <- array(0, dim = c(20, 784))

foreach(i = 1:784) %dopar% {
  input <- training[i,-1] %>% as.matrix %>% t
  input <- input / (255 * .99) + 0.01
  output <- training[i, 1] %>% as.matrix %>% dummy %>% t %>% unname
  output <- output + 0.01
  output[output == 1.01] <- 0.99
  foreach(t = 1:20) %dopar% {
    SSE <- train(model, input, output, learningrate = 0.1)
    errors[t:i] <- SSE
  }
}

errors

predict(model, input)
apply(predict(model, input), 2, which.max)

testing$label


