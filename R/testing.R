
set.seed(2323)



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

input <- mtcars[, 1:7] %>% t %>% unname %>% rescale
output <- mtcars[, "am"]
output <- rbind(t(output), -1 %*% output + 1)
row.names(output) <- c(0,1)


# number of input variables
niv <- nrow(input)

# number of output variables
nov <- nrow(output)

# init nnet
model <- nnet(7, 3, 2, "sigmoid")

model$weights


# train the model

it <- 10000L
errors <- vector(mode = "numeric", it)
i <- 1L

while(i <= it) {
  SSE <- train(model, input, output, learningrate = 0.5)
  errors[i] <- SSE
  i <- i + 1L
}


p <- predict(model, input)
p <- apply(p,2, which.max)
p <- ifelse(p == 2, 0, 1)

mtcars$am
p
# perfect trained model / probably not good ;-)



# new model
# ------------------

library(dummies)

input <- mtcars[, 1:7] %>% t %>% unname %>% rescale
output <- dummy(mtcars$gear) %>% t

unique(mtcars$gear)

model <- nnet(7, 4, 3, "sigmoid")
model$weights


# train the model

it <- 1000L
errors <- vector(mode = "numeric", it)
i <- 1L

while(i <= it) {
  SSE <- train(model, input, output, learningrate = 0.5)
  errors[i] <- SSE
  i <- i + 1L
}


# some errors
compare <- rbind(apply(predict(model, input),2, which.max), 
                 apply(dummy(mtcars$gear) %>% t, 2, which.max)) %>% t


library(ggplot2)

data.frame(errors = errors, i = 1:length(errors)) %>% 
  ggplot(aes(x = i, y = errors)) +
  geom_line()


## after ore than 500 iterations the erros start to wiggle





