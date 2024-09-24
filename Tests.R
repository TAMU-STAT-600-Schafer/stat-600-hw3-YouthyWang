# This is a script to save your own tests for the function
source("FunctionsLR.R")
library(nnet)

## Simple test: a binary classification
# Breast Cancer Wisconsin (Diagnostic)
table <- read.table("wdbc.data", header = FALSE, sep = ",") 
label <- table[ , 2]
# Use (1 and 0) to replace ("Malignant" and "Benign")
num_label <- rep(0, length(label))
num_label[label=="M"] <- 1
# Random split to training and testing sets
splt <- sample(1:nrow(table), 0.8*nrow(table))
samp_mat <- as.matrix(table[ , 3:ncol(table)])
X <- cbind(rep(1, nrow(samp_mat[splt, ])), samp_mat[splt, ])
y <- num_label[splt]
Xt <- cbind(rep(1, nrow(samp_mat[-splt, ])), samp_mat[-splt, ])
yt <- num_label[-splt]

results <- LRMultiClass(X, y, Xt, yt, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL)

# Standard method from R function
data <- data.frame(y, X[ , 2:ncol(X)])
colnames(data)[-1] <- paste0("V", 2:ncol(X))
model <- multinom(y ~ ., data = data)
summary(model)

test_data <- data.frame(Xt[ , 2:ncol(Xt)])
colnames(test_data) <- paste0("V", 2:ncol(Xt))

# Predictions on the original data
predicted_classes <- predict(model)
error <- sum(predicted_classes != y) / length(y) * 100

predicted_classes_new <- predict(model, newdata = test_data)
error_tt <- sum(predicted_classes_new != yt) / length(yt) * 100
