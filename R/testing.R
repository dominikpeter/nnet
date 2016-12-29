
library(pixmap)
library(data.table)  
library(nnet)

pixmap::
img <- read.pnm("img/Number-5-handwritten.png")


train <- fread("http://www.pjreddie.com/media/files/mnist_train.csv") 
test <- fread("http://www.pjreddie.com/media/files/mnist_test.csv")

setnames(train, 1, "number")
setnames(test, 1, "number")


mod <- nnet(number ~ train_pca, data = train, size = 3)

hidden_layer <- matrix(c(.9, .2, .1,
              .3, .8, .5,
              .4, .2, .6), nrow = 3)

input <- c(.9, .1, .8)

sigmoid(m %*% input)

