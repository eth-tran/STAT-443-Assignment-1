# a.
l1_loss <- function(E) {
  return(abs(E))
}

l2_loss <- function(E) {
  return(E * E)
}

curve(l1_loss, from = -5, to = 5, col = "blue", 
      main = "Plot of L1 and L2 Loss Functions", xlab = "E", ylab = "L(E)")
curve(l2_loss, from = -5, to = 5, col = "red", add = TRUE)
legend("bottomleft", legend = c("L1", "L2"), col = c("blue", "red"), lty = 1)

# For smaller absolute prediction error (closer to true value), L1 loss penalizes more than L2.
# For larger absolute prediction error (further from true value), L2 loss penalizes more than L1.


# b.

library(MASS)
library(L1pack)
data(Animals)

Animals$ln_body <- log(Animals$body)
Animals$ln_brain <- log(Animals$brain)

plot(ln_body, ln_brain)

cols <- rainbow(6)

for (q in 1:6) {
  model <- lad(ln_brain ~ poly(ln_body, q), data = Animals)
  
  ord <- order(ln_body)
  
  lines(ln_body[ord], fitted(model)[ord], col = cols[q], lwd = 2)
}

legend("topleft", legend = paste("Degree", 1:6), col = cols, lwd = 2)

# Higher degree models are more sensitive to outliers. 
# Lower degree models are less sensitive to outliers.


# c.

test_index <- c(2, 7, 11, 16, 26)
train <- Animals[-test_index, ]
test <- Animals[test_index, ]

APAE <- numeric(6)

for (q in 1:6) {
  model <- lad(ln_brain ~ poly(ln_body, q), data = train)
  y_pred <- predict(model, newdata = test)
  APSE[q] <- mean(abs(test$ln_brain - y_pred))
}

results <- data.frame(
  degree = 1:6,
  APAE = APAE
)

results

which.min(APSE) # Model 3 has the lowest APSE, which is the same result as in 2b.


# d.

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
    
    model <- lad(ln_brain ~ poly(ln_body, q), data = train_fold)
    y_pred <- predict(model, newdata = test_fold)
    fold_APSE[i] <- mean(abs(test_fold$ln_brain - y_pred)) 
  }
  cv_error[q] <- mean(fold_APSE)
}

results <- data.frame(
  degree = 1:6,
  cv_error = cv_error
)

results

which.min(cv_error) # Model 3 has the lowest APSE, which disagrees with the result in 2c.

# This comparison is not very fair since both models use different objective functions.
# In Q2c, we used mean absolute error to evaluate the models, and in Q3d we used mean
# squared error. This may result in conflicting degree selection.

# e.

loss_function <- function(E) {
  return(log(cosh(E)))
}

cv_error_l1 <- numeric(6)
cv_error_l2 <- numeric(6)

# L1
for (q in 1:6) {
  fold_APSE <- numeric(5)
  for (i in 1:5) {
    test_index <- folds[[i]]
    train_fold <- Animals[-test_index, ]
    test_fold <- Animals[test_index, ]
    
    model <- lad(ln_brain ~ poly(ln_body, q), data = train_fold)
    y_pred <- predict(model, newdata = test_fold)
    E <- test_fold$ln_brain - y_pred
    fold_APSE[i] <- mean(loss_function(E))
  }
  cv_error_l1[q] <- mean(fold_APSE)
}

# L2
for (q in 1:6) {
  fold_APSE <- numeric(5)
  for (i in 1:5) {
    test_index <- folds[[i]]
    train_fold <- Animals[-test_index, ]
    test_fold <- Animals[test_index, ]
    
    model <- lm(ln_brain ~ poly(ln_body, q), data = train_fold)
    y_pred <- predict(model, newdata = test_fold)
    E <- test_fold$ln_brain - y_pred
    fold_APSE[i] <- mean(loss_function(E))
  }
  cv_error_l2[q] <- mean(fold_APSE)
}

results <- data.frame(
  degree = 1:6,
  cv_error_l1 = cv_error_l1,
  cv_error_l2 = cv_error_l2
)
results
# A model with degree 2 and L2 loss has the smallest log-cosh error, so we select it as the best model.









