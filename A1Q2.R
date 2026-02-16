library(MASS)
data(Animals)

# a.

Animals$ln_body <- log(Animals$body)
Animals$ln_brain <- log(Animals$brain)

plot(ln_body, ln_brain)

cols <- rainbow(6)

for (q in 1:6) {
  model <- lm(ln_brain ~ poly(ln_body, q), data = Animals)
  
  ord <- order(ln_body)
  
  lines(ln_body[ord], fitted(model)[ord], col = cols[q], lwd = 2)
}

legend("topleft", legend = paste("Degree", 1:6), col = cols, lwd = 2)


# b.

test_index <- c(2, 7, 11, 16, 26)
train <- Animals[-test_index, ]
test <- Animals[test_index, ]

APSE <- numeric(6)

for (q in 1:6) {
  model <- lm(ln_brain ~ poly(ln_body, q), data = train)
  y_pred <- predict(model, newdata = test)
  APSE[q] <- mean((test$ln_brain - y_pred)^2)
}

results <- data.frame(
  degree = 1:6,
  APSE = APSE
)

results

which.min(APSE) # Model 3 has the lowest APSE


# c.

ShuffledIndices = c(19, 8, 25, 5, 16, 3, 4, 17, 21, 10, 6, 27, 28, 20, 26, 23, 13, 14, 7, 15, 24, 2, 22, 9, 11, 18, 12, 1)
folds <- split(ShuffledIndices, rep(c(1, 2, 3, 4, 5), times = c(6, 6, 6, 5, 5)))
folds

cv_error <- numeric(6)

for (q in 1:6) {
  fold_APSE <- numeric(5)
  for (i in 1:5) {
    test_index <- folds[[i]]
    train_fold <- Animals[-test_index, ]
    test_fold <- Animals[test_index, ]
    
    model <- lm(ln_brain ~ poly(ln_body, q), data = train_fold)
    y_pred <- predict(model, newdata = test_fold)
    fold_APSE[i] <- mean((test_fold$ln_brain - y_pred)^2) 
  }
  cv_error[q] <- mean(fold_APSE)
}

results <- data.frame(
  degree = 1:6,
  cv_error = cv_error
)

results

which.min(cv_error) # Model 2 has the lowest APSE


# d.

# The best model in part b. was degree 3 with APSE 1.858240.
# The best model in part c. was degree 2 with APSE 1.653375.
# They do not agree on the best polynomial degree.
# The result in part c. is more trustworthy as it uses more test sets through k-fold CV.
# The model in part b. only uses 1 test set. This may result in overfitting if the
# training data is not a good representation of the dataset.
