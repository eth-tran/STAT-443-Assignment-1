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

APSE <- numeric(6)

for (q in 1:6) {
  model <- lad(ln_brain ~ poly(ln_body, q), data = train)
  y_pred <- predict(model, newdata = test)
  APSE[q] <- mean(abs(test$ln_brain - y_pred))
}

results <- data.frame(
  degree = 1:6,
  APSE = APSE
)

results

which.min(APSE) # Model 3 has the lowest APSE, which is the same result as in 2b.



