#' A neural network training function
#'
#' This function allows you to train a NN.
#' @param x columns from data to use as input
#' @param y column from data to use for classification
#' @param traindata data to train on
#' @param testdata data to test on Defaults to NULL
#' @param model a supplied model Defaults to NULL
#' @param hidden number of layers Defaults 6
#' @param maxit max iterations Defaults to 2000
#' @param abstol delta parameter for the loss/cost function Defaults to .01
#' @param lr learning rate Defaults to .01
#' @param reg rate of regularization Defaults to .001
#' @param display progress of learned parameters by steps Defaults to 100
#' @param random.seed set a seed value Defaults to 1
#' @keywords deep neural network, DNN, neural network, NN
#' @examples
#' train.dnn(x=1:2, y=1,trainingdata=d.train,testdata=d.test,hidden=6,maxit=2000,display=100)
#'
#' example with DNN package
#' samp <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
#' iris.model <- train.dnn(x=1:4,y=5, traindata=iris[samp,],
#'                      testdata=iris[-samp,],hidden=10,
#'                      maxit=2000,display=50)
#'
#' labels.dnn <- predict.dnn(iris.model, iris[-samp, -5])
#'
#' # Display a confusion matrix of the dataset
#' table(iris[-samp, 5], labels.dnn)

train.dnn <- function(x, y, traindata=data, testdata=NULL,
                      model=NULL,
                      hidden=c(6), # hidden layers
                      maxit=2000,  # iterations
                      abstol=1e-2, # delta loss
                      lr=1e-2,     # learning rate
                      reg=1e-3,    # regularization rate
                      display=100, # display learning rates
                      random.seed=1)
{
    set.seed(random.seed)
    # total number of rows in training set
    N <- nrow(traindata)

    # extract specified col, data and features
    X <- unname(data.matrix(traindata[,x]))
    # transform and index data
    Y <- traindata[,y]
    if(is.factor(Y)) {Y <- as.integer(Y)}
    Y.len   <- length(unique(Y))
    Y.set   <- sort(unique(Y))
    Y.index <- cbind(1:N, match(Y, Y.set))

    # create NN model
    if(is.null(model)) {
        # number of features
        D <- ncol(X)
        # number of categories
        K <- length(unique(Y))
        H <- hidden

        # initialize weights and bias units
        W1 <- 0.01 * matrix(rnorm(D*H), nrow=D, ncol=H)
        b1 <- matrix(0, nrow=1, ncol=H)

        W2 <- 0.01 * matrix(rnorm(H*K), nrow=H, ncol=K)
        b2 <- matrix(0, nrow=1, ncol=K)
    } else {
        D  <- model$D
        K  <- model$K
        H  <- model$H
        W1 <- model$W1
        b1 <- model$b1
        W2 <- model$W2
        b2 <- model$b2
    }

    # amount of training data to use
    batchsize <- N
    loss <- 100000

    # train network
    i <- 0
    while(i < maxit && loss > abstol){
        # counter
        i <- i + 1

        # foward propagation input layer
        hidden.layer <- sweep(X %*% W1, 2, b1, '+')
        # hidden layer
        hidden.layer <- pmax(hidden.layer, 0)
        # output layer
        score <- sweep(hidden.layer %*% W2, 2, b2, '+')

        # softmax normalization
        score.exp <- exp(score)
        probs <- score.exp/rowSums(score.exp)

        # impliment loss/cost function
        correct.logprobs <- -log(probs[Y.index])
        data.loss <- sum(correct.logprobs) / batchsize
        reg.loss <- 0.5 * reg * (sum(W1*W1) + sum(W2*W2))
        loss <- data.loss + reg.loss

        # display and batch update model
        if(i %% display == 0) {
            if(!is.null(testdata)) {
                model <- list(D  = D,
                              H  = H,
                              K  = K,
                              W1 = W1,
                              b1 = b1,
                              W2 = W2,
                              b2 = b2)
                labs <- predict.dnn(model, testdata[,-y])
                accuracy <- mean(as.integer(testdata[,y]) == Y.set[labs])
                cat(i, loss, accuracy, "\n")
                } else {
                    cat(i, loss, "\n")
                }
            }

        # back propagation
        dscores <- probs
        dscores[Y.index] <- dscores[Y.index] - 1
        dscores <- dscores / batchsize

        dW2 <- t(hidden.layer) %*% dscores
        db2 <- colSums(dscores)

        dhidden <- dscores %*% t(W2)
        dhidden[hidden.layer <= 0] <- 0

        dW1 <- t(X) %*% dhidden
        db1 <- colSums(dhidden)

        # simultaneous update
        dW2 <- dW2 + reg * W2
        dW1 <- dW1 + reg * W1

        W1 <- W1 - lr * dW1
        b1 <- b1 - lr * db1

        W2 <- W2 - lr * dW2
        b2 <- b2 - lr * db2

    }

    # build list of learned parameters
    model <- list(D  = D,
                  H  = H,
                  K  = K,
                  W1 = W1,
                  b1 = b1,
                  W2 = W2,
                  b2 = b2)

    return(model)

}
