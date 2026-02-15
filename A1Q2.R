library(MASS)
data(Animals)

# a.

ln_body <- log(Animals$body)
ln_brain <- log(Animals$brain)

plot(ln_body, ln_brain)

cols <- rainbow(6)

for (q in 1:6) {
  model <- lm(ln_brain ~ poly(ln_body, q), data = Animals)
  
  ord <- order(ln_body)
  
  lines(ln_body[ord], fitted(model)[ord], col = cols[q], lwd = 2)
}

legend("topleft", legend = paste("Degree", 1:6), col = cols, lwd = 2)
