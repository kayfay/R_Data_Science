# Simple Deep Neural Network


## Overview
I've written a neural network in Octave as a part of the stanford machine learnign course and decided to do this simple implementation of a deep neural network in R.

### Installation
To install this package directly into R issue the following commands
```
install.packages("devtools")
# library(devtools)
devtools::install_github("kayfay/R_programming/r_neural_network")

# import the package
library(DNN)
```
If you have trouble with the package Download a zip of the github and load it from directory
```
# or manuall load the package after downloading it from github
load_all('/directory/to/package/')
```
### Usage
Using example on train.dnn will provide example usage
using the iris dataset available in the R base package
```
example(train.dnn)
```
First create a random sample
```
samp <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))

```
Train the model on the iris dataset
```
iris.model <- train.dnn(x=1:4, y=5, traindata=iris[samp,],
        testdata=iris[-samp,], hidden=10, maxit=2000, display=50)
```
Perform predictions using
```
labels.dnn <- predict.dnn(iris.model, iris[-samp, -5])
```

Display a confusion matrix based on the predictions
 ```
table(iris[-samp, 5], labels.dnn)
#             labels.dnn
#               1  2  3
#   setosa     25  0  0
#   versicolor  0 22  3
#   virginica   0  0 25
```
