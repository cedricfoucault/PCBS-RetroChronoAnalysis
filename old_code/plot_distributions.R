min <- -5
max <- 5

# basic gaussian distribution
x <- seq(min, max, by=.01)
norm.x <- dnorm(x)
plot(x, norm.x, type='l', ylab="density")

# mixture gaussian + uniform
unif.x <- dunif(x, min, max)
mixture.x <- 2/3 * norm.x + 1/3 * unif.x
plot(x, norm.x, type='l', ylab="density", col="gray")
lines(x, unif.x, col="gray")
lines(x, mixture.x, col="blue")

# mixture gaussian(mean1, sd1) + gaussian(mean2, sd2)
mean1 <- -1
mean2 <- 1.5
sd1 <- .6
sd2 <- 1.3
norm1 <- dnorm(x, mean=mean1, sd=sd1)
norm2 <- dnorm(x, mean=mean2, sd=sd2)
mixnorm <- 1/2 * norm1 + 1/2 * norm2
plot(x, norm1, type='l', col="gray")
lines(x, norm2, col="gray")
lines(x, mixnorm, col="blue")

# mixture gaussian(mean1, sd1) + gaussian(mean2, sd2) + uniform 
mixnormunif <- 2/3 * mixnorm + 1/3 * unif.x
plot(x, norm1, type='l', col="gray")
lines(x, norm2, col="gray")
lines(x, unif.x, col="gray")
lines(x, mixnormunif, col="blue")
label <- "1/3 * norm(-1, .6) + 1/3 * norm(1.5, 1.3) + 1/3 * unif(-5, 5)"
legend("top", inset=.1, c(label), lwd=2, col=c("blue"))
