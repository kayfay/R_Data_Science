#' A neural network prediction function
#'
#' This function allows you to make predictions based on a trained NN model.
#' @param model takes a list of learned parameters D=Nfeatures, K=Ncatagories, H=Nhidden layers, W1, b1, W2, b2
#' @param data A data set
#' @keywords deep neural network, DNN, neural network, NN
#' @examples
#' predict.dnn(model, testdata)
#'
#' example with DNN package
#'
#' library(DNN)
#' samp <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
#' iris.model <- train.dnn(x=1:4,y=5, traindata=iris[samp,],testdata=iris[-samp,],hidden=10,maxit=2000,display=50)
#' labels.dnn <- predict.dnn(iris.model, iris[-samp, -5])
predict.dnn <- function(model, data = X.test) {
    new.data <- data.matrix(data)

    # input layer
    hidden.layer <- sweep(new.data %*% model$W1, 2, model$b1, '+')
    # hidden layer
    hidden.layer <- pmax(hidden.layer, 0)
    # output layer
    score <- sweep(hidden.layer %*% model$W2, 2, model$b2, '+')

    # softmax loss function
    score.exp <- exp(score)
    probs <- sweep(score.exp, 1, rowSums(score.exp), '/')

    labels.predicted <- max.col(probs)
    return(labels.predicted)
}
