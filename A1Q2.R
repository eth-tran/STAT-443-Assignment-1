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
  print(y_pred)
  APSE[q] <- mean((test$ln_brain - y_pred)^2)
}

results <- data.frame(
  degree = 1:6,
  APSE = APSE
)

results

which.min(APSE) # Model 3 has the lowest APSE




