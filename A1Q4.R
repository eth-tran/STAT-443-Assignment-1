library(MASS)
data(Animals)

# a.


Animals$ln_body <- log(Animals$body)
Animals$ln_brain <- log(Animals$brain)

model <- lm(ln_brain ~ poly(ln_body, 2), data = Animals)

plot(ln_body, ln_brain)

ord <- order(ln_body)

lines(ln_body[ord], fitted(model)[ord], col = "magenta", lwd = 2)


# b.

body_new <- log(c(15, 150, 1000, 5000, 12000))
brain_prediction <- predict(model, newdata = data.frame(ln_body = body_new))
exp(brain_prediction)
plot(body_new, brain_prediction)


# c.

max(ln_body)
max(body_new)

min(ln_body)
min(body_new)

# We see that the body weights are within the range of the data, so this is interpolation.