soa <- c(-400, -200, -100, 100, 200, 400)
n.soa <- length(soa)
n.soa.precue <- length(soa[soa < 0])
n.soa.postcue <- n.soa - n.soa.precue
i.postcue <- (n.soa.precue + 1) : n.soa

jSOA.min = -600.0
jSOA.max = 600.0
jSOA.step = 5.0
jSOA <-  seq(jSOA.min, jSOA.max, by=jSOA.step)
n.jSOA <- length(jSOA)

# Proportion of each distrib component
p.base.seen <- 0.55
## valid cue
p.valid.not.seen <- c(0.35, 0.30, 0.24, 0.32, 0.35, 0.39)
p.valid.seen <- 1 - p.valid.not.seen
p.valid.seen.retro <- c(0, 0, 0, (p.valid.seen - p.base.seen)[4:6])
p.valid.seen <- p.valid.seen - p.valid.seen.retro
## invalid cue
p.invalid.seen <- rep(p.base.seen, times=n.soa)
p.invalid.not.seen <- 1 - p.invalid.seen

# "Not seen" uniform distrib component
djSOA.not.seen <- matrix(dunif(jSOA, jSOA.min, jSOA.max), nrow = n.soa, ncol = n.jSOA)

# "(Initially) Seen" gaussian distrib component
djSOA.mean.base.slope <- 0.5 # 0.2 ? <Marti et al 2010> Outside the interference regime, the iSOA increased by 135 ms (F(1, 9) = 6.53, p < 0.05) when the SOA increased from 700 to 1050 ms
djSOA.mean.base.intercept <- -20.0
djSOA.sd.base <- 50 # 300 ? from <Marti et al 2010>: SEM ~= 100, N=10, sd = SEM * sqrt(N-1)
djSOA.mean.invalid.seen <- soa * djSOA.mean.base.slope + djSOA.mean.base.intercept
djSOA.sd.invalid.seen <- rep(djSOA.sd.base, times=n.soa)
djSOA.mean.valid.seen <- djSOA.mean.invalid.seen # NO attention effect
djSOA.sd.valid.seen <- djSOA.sd.invalid.seen # NO attention effect
## build the distribution for each SOA
djSOA.invalid.seen <- matrix(nrow = n.soa, ncol = n.jSOA)
djSOA.valid.seen <- matrix(nrow = n.soa, ncol = n.jSOA)
for (i in 1:n.soa) {
  djSOA.invalid.seen[i,] <- dnorm(jSOA, mean = djSOA.mean.invalid.seen[i], sd = djSOA.sd.invalid.seen[i])
  djSOA.valid.seen[i,] <- dnorm(jSOA, mean = djSOA.mean.valid.seen[i], sd = djSOA.sd.valid.seen[i])
}

# "Retrospectively Seen" gaussian distrib component
djSOA.sd.retro <- djSOA.sd.base * 1.3 # a bit higher variance than for "initially seen"
## Model CA : Conscious Access
djSOA.mean.retro.CA <- rep(djSOA.mean.base.intercept, n.soa)
djSOA.sd.retro.CA <- rep(djSOA.sd.retro, n.soa)
## Model TM : Time Marker
djSOA.mean.retro.TM.slope <- djSOA.mean.base.slope
djSOA.mean.retro.TM.intercept <- djSOA.mean.base.intercept
djSOA.mean.retro.TM <- soa * djSOA.mean.retro.TM.slope + djSOA.mean.retro.TM.intercept
djSOA.sd.retro.TM <- djSOA.sd.retro.CA
## build the distribution for each SOA
djSOA.retro.CA <- matrix(nrow = n.soa, ncol = n.jSOA)
djSOA.retro.TM <- matrix(nrow = n.soa, ncol = n.jSOA)
for (i in 1:n.soa) {
  djSOA.retro.CA[i,] <- dnorm(jSOA, mean = djSOA.mean.retro.CA[i], sd = djSOA.sd.retro.CA[i])
  djSOA.retro.TM[i,] <- dnorm(jSOA, mean = djSOA.mean.retro.TM[i], sd = djSOA.sd.retro.TM[i])
}

# Mixture distribution
## CA
djSOA.valid.CA <- p.valid.seen.retro * djSOA.retro.CA + p.valid.seen * djSOA.valid.seen + p.valid.not.seen * djSOA.not.seen
## TM
djSOA.valid.TM <- p.valid.seen.retro * djSOA.retro.TM + p.valid.seen * djSOA.valid.seen + p.valid.not.seen * djSOA.not.seen
## invalid
djSOA.invalid <- p.invalid.seen * djSOA.invalid.seen + p.invalid.not.seen * djSOA.not.seen

# Mean of the mixture
mean.jSOA.valid.CA <- rep(0, n.soa)
mean.jSOA.valid.TM <- rep(0, n.soa)
mean.jSOA.invalid <- rep(0, n.soa)
for(i in 1:n.soa) {
  mean.jSOA.valid.CA[i] <- sum(djSOA.valid.CA[i,] * jSOA) * jSOA.step
  mean.jSOA.valid.TM[i] <- sum(djSOA.valid.TM[i,] * jSOA) * jSOA.step
  mean.jSOA.invalid[i] <- sum(djSOA.invalid * jSOA) * jSOA.step
}
# SD of the mixture
sd.jSOA.valid.CA <- rep(0, n.soa)
sd.jSOA.valid.TM <- rep(0, n.soa)
sd.jSOA.invalid <- rep(0, n.soa)
for(i in 1:n.soa) {
  sd.jSOA.valid.CA[i] <- sqrt(sum(djSOA.valid.CA[i,] * (jSOA ^ 2)) * jSOA.step - mean.jSOA.valid.CA[i] ^ 2)
  sd.jSOA.valid.TM[i] <- sqrt(sum(djSOA.valid.TM[i,] * (jSOA ^ 2)) * jSOA.step - mean.jSOA.valid.TM[i] ^ 2)
  sd.jSOA.invalid[i] <- sqrt(sum(djSOA.invalid[i,] * (jSOA ^ 2)) * jSOA.step - mean.jSOA.invalid[i] ^ 2)
}

# FIGURES

# Figure 1 : Mean
quartz()
plot(soa[i.postcue], mean.jSOA.valid.CA[i.postcue],
     xlab="SOA (ms)", ylab="Mean jSOA (ms)",
     ylim=c(min(mean.jSOA.valid.CA[i.postcue], mean.jSOA.valid.TM[i.postcue]), max(mean.jSOA.valid.CA[i.postcue], mean.jSOA.valid.TM[i.postcue])),
     main="Mean jSOA",
     type="b",
     col="blue",
     pch=3)
points(soa[i.postcue], mean.jSOA.valid.TM[i.postcue],
     type="b",
     col="green",
     pch=4)
legend("topleft", inset=.05, pch=c(3,4),
                c("Model CA (Conscious Access)", "Model TM (Time Marker)"),
                col=c("blue", "green"))

# Figure 2 : Standard Deviation
quartz()
plot(soa[i.postcue], sd.jSOA.valid.CA[i.postcue],
     xlab="SOA (ms)", ylab="SD (ms)",
     ylim=c(min(sd.jSOA.valid.CA[i.postcue], sd.jSOA.valid.TM[i.postcue]), max(sd.jSOA.valid.CA[i.postcue], sd.jSOA.valid.TM[i.postcue])),
     main="Standard Deviation of jSOA",
     type="b",
     col="blue",
     pch=3)
points(soa[i.postcue], sd.jSOA.valid.TM[i.postcue],
       type="b",
       col="green",
       pch=4)
legend("topleft", inset=.05, pch=c(3,4),
       c("Model CA (Conscious Access)", "Model TM (Time Marker)"),
       col=c("blue", "green"))

# Figure 3 : Distributions
quartz()
# plot.new()
# par(xlab="jSOA", ylab="density",
#     xlim = c(jSOA.min, jSOA.max), ylim = c(0.0, max(djSOA.valid.CA[i.postcue,])),
#     main="Distribution")
plot(c(), c(),
    xlab="jSOA", ylab="density",
    xlim = c(jSOA.min, jSOA.max), ylim = c(0.0, max(djSOA.valid.CA[i.postcue,], djSOA.valid.TM[i.postcue,])),
    main="Distribution")
for (i in i.postcue) {
  lines(jSOA, djSOA.valid.CA[i,], col="blue")
  lines(jSOA, djSOA.valid.TM[i,], col="green")
  legend("topleft", inset=.05, lwd=2,
         c("Model CA", "Model TM"),
         col=c("blue", "green"))
}

# Figure 4 : Distributions against invalid
quartz()
par(mfrow = c(2, n.soa.postcue))
for (i in 1:n.soa.postcue) {
  par(mfg = c(1, i))
  i.soa <- i + n.soa.precue
  plot(jSOA, djSOA.invalid[i.soa,], type="l", col="gray",
       xlab="jSOA", ylab="density",
       main=sprintf("SOA = %d", soa[i.soa]),
       xlim = c(jSOA.min, jSOA.max), ylim = c(0.0, max(djSOA.valid.seen)))
  lines(jSOA, djSOA.valid.CA[i.soa,], col="blue")
  legend("topright", inset=.05, lwd=2,
         c("invalid", "valid (CA)"),
         col=c("gray", "blue"))

  par(mfg = c(2, i))
  plot(jSOA, djSOA.invalid[i.soa,], type="l", col="gray",
       xlab="jSOA", ylab="density",
       main=sprintf("SOA = %d", soa[i.soa]),
       xlim = c(jSOA.min, jSOA.max), ylim = c(0.0, max(djSOA.valid.seen)))
  lines(jSOA, djSOA.valid.TM[i.soa,], col="green")
  legend("topright", inset=.05, lwd=2,
         c("invalid", "valid (TM)"),
         col=c("gray", "green"))
}

# Figure 5 : Distribution Components
quartz()
par(mfrow=c(2, n.soa / 2))
for (i in 1:n.soa) {
  plot(jSOA, djSOA.not.seen[i,], type="l", col="gray",
       xlab="jSOA", ylab="density",
       main=sprintf("SOA = %d", soa[i]),
       xlim = c(jSOA.min, jSOA.max), ylim = c(0.0, max(djSOA.valid.seen)))
  lines(jSOA, djSOA.valid.seen[i,], col="yellow")
  lines(jSOA, djSOA.retro.CA[i,], col="blue")
  lines(jSOA, djSOA.retro.TM[i,], col="green")
  legend("topright", inset=.05, lwd=2,
         c("not seen", "seen", "retro: CA", "retro: TM"),
         col=c("gray","yellow", "blue", "green"))
}
