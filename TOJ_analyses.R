source("TOJ_model.R")

should.plot.pss <- TRUE
should.plot.rau <- TRUE
should.plot.sensitivity <- TRUE
should.plot.test.rau <- FALSE

## Mixture PSS

pss.toBeep.mixture.nocue <- pss.toBeep.seen
pss.toBeep.mixture <- function(soa.toCue, model) {
  min <- pss.toBeep.mixture.nocue
  max <- pss.toBeep.retro(soa.toCue, model)
  soa.test <- seq(min, max, by=1)
  p.test <- p.beforeBeep.mixture(soa.test, soa.toCue, model)
  i <- which(abs(p.test - 0.5) == min(abs(p.test - 0.5)))
  soa.test[i]
}

## Rationalized Arcsine Transform (Studebaker)
rau.transform <- function(p) {
  asin(sqrt(p))
}

# Approximation of sensitivity from p(correct response), assuming H = 1 - F (no bias)
sensitivity.transform <- function(pc) {
  2 * qnorm(pc)
}

## Figure 1bis : Mixture psychometric function in RAU units
if (should.plot.rau) {
  quartz()
  par(mfrow=c(2, 1))
  par(oma=c(2, 0, 0, 0))
  ### 1A cue & no-cue, CA Model
  rau.nocue <- rau.transform(p.beforeBeep.mixture.nocue(data.soa.toBeep))
  rau.CA <- rau.transform(p.beforeBeep.mixture(data.soa.toBeep, soa.toCue = data.soa.toCue, model="CA"))
  rau.TM <- rau.transform(p.beforeBeep.mixture(data.soa.toBeep, soa.toCue = data.soa.toCue, model="TM"))
  plot(data.soa.toBeep, rau.nocue,
       type="l", col="black", lty="dotted",
       xlab="Target-to-Beep SOA (ms)", ylab="RAU P(\"T < B\")",
       ylim=c(0, rau.transform(1)),
       main="CA Model")
  lines(data.soa.toBeep, rau.CA,
        col="blue", lty="dashed")
  legend("topleft", inset=.05, lwd=2,
         c("no cue", sprintf("retro-cue : %dms", data.soa.toCue)),
         col=c("black","blue"),
         lty=c("dotted", "dashed"))
  ### 1B , TM Model
  plot(data.soa.toBeep, rau.nocue,
       type="l", col="black", lty="dotted",
       xlab="Target-to-Beep SOA (ms)", ylab="RAU P(\"T < B\")",
       ylim=c(0, rau.transform(1)),
       main="TM Model")
  lines(data.soa.toBeep, rau.TM,
        col="green", lty="dashed")
  legend("topleft", inset=.05, lwd=2,
         c("no cue", sprintf("retro-cue : %dms", data.soa.toCue)),
         col=c("black","green"),
         lty=c("dotted", "dashed"))
  mtext(sprintf("include guesses : %s", if (include.guesses) "yes" else "no"), side=1, outer=TRUE)
}

## Figure 1bisbis : Sensitivity as a function of SOA target-to-beep
if (should.plot.sensitivity) {
  quartz()
  par(mfrow=c(2, 1))
  par(oma=c(2, 0, 0, 0))
  ### 1A cue & no-cue, CA Model
  data.soa.toBeep.neg = data.soa.toBeep[data.soa.toBeep < 0]
  data.soa.toBeep.pos = data.soa.toBeep[data.soa.toBeep >= 0]
  pc.nocue <- c(1 - p.beforeBeep.mixture.nocue(data.soa.toBeep.neg),
                p.beforeBeep.mixture.nocue(data.soa.toBeep.pos))
  pc.CA <- c(1 - p.beforeBeep.mixture(data.soa.toBeep.neg, soa.toCue = data.soa.toCue, model="CA"),
             p.beforeBeep.mixture(data.soa.toBeep.pos, soa.toCue = data.soa.toCue, model="CA"))
  pc.TM <- c(1 - p.beforeBeep.mixture(data.soa.toBeep.neg, soa.toCue = data.soa.toCue, model="TM"),
             p.beforeBeep.mixture(data.soa.toBeep.pos, soa.toCue = data.soa.toCue, model="TM"))
  sensitivity.nocue <- sensitivity.transform(pc.nocue)
  sensitivity.CA <- sensitivity.transform(pc.CA)
  sensitivity.TM <- sensitivity.transform(pc.TM)
  plot(data.soa.toBeep, sensitivity.nocue,
       type="l", col="black", lty="dotted",
       xlab="Target-to-Beep SOA (ms)", ylab="d'",
       ylim=c(min(sensitivity.nocue, sensitivity.CA), max(sensitivity.nocue, sensitivity.CA)),
       main="CA Model")
  lines(data.soa.toBeep, sensitivity.CA,
        col="blue", lty="dashed")
  legend("topleft", inset=.05, lwd=2,
         c("no cue", sprintf("retro-cue : %dms", data.soa.toCue)),
         col=c("black","blue"),
         lty=c("dotted", "dashed"))
  ### 1B , TM Model
  plot(data.soa.toBeep, sensitivity.nocue,
       type="l", col="black", lty="dotted",
       xlab="Target-to-Beep SOA (ms)", ylab="d'",
       ylim=c(min(sensitivity.nocue, sensitivity.TM), max(sensitivity.nocue, sensitivity.TM)),
       main="TM Model")
  lines(data.soa.toBeep, sensitivity.TM,
        col="green", lty="dashed")
  legend("topleft", inset=.05, lwd=2,
         c("no cue", sprintf("retro-cue : %dms", data.soa.toCue)),
         col=c("black","green"),
         lty=c("dotted", "dashed"))
  mtext(sprintf("include guesses : %s", if (include.guesses) "yes" else "no"), side=1, outer=TRUE)
}

if (should.plot.pss) {
  ## Figure 2 : Mixture PSS
  data.soa.toCue <- c(100, 200, 400)
  data.pss.toBeep.mixture.CA <- sapply(data.soa.toCue, pss.toBeep.mixture.CA)
  data.pss.toBeep.mixture.TM <- sapply(data.soa.toCue, pss.toBeep.mixture.TM)
  quartz()
  par(mfrow=c(2, 1))
  ### 2A, CA Model
  plot(data.soa.toCue, data.pss.toBeep.mixture.CA,
       xlab="Target-to-Cue SOA (ms)", ylab="Target-to-Beep PSS (ms)",
       ylim=c(pss.toBeep.mixture.nocue, max(data.pss.toBeep.mixture.CA)),
       main="CA Model",
       type="b", col="blue", pch=3)
  points(data.soa.toCue, rep(pss.toBeep.mixture.nocue, length(data.soa.toCue)),
         type="b", col="black", pch=4)
  ### 2B, TM Model
  plot(data.soa.toCue, data.pss.toBeep.mixture.TM,
       xlab="Target-to-Cue SOA (ms)", ylab="Target-to-Beep PSS (ms)",
       main="CA Model",
       type="b", col="blue", pch=3)
  points(data.soa.toCue, rep(pss.toBeep.mixture.nocue, length(data.soa.toCue)),
         type="b", col="black", pch=4)
}

should.plot.fig3 <- FALSE
if (should.plot.fig3) {
  ## Figure 3 : Mixture psychometric function with >1 soa
  data.soa.toCue <- c(100, 200, 300, 400)
  colors <- c("#71DA96","#52A370", "#36704C", "#1D412B")
  ltys <-  rep("dashed", length(data.soa.toCue))
  labels <- sapply(data.soa.toCue, function(s) sprintf("retro-cue: %dms", s))
  quartz()
  par(mfrow=c(2, 1))
  par(oma=c(2, 0, 0, 0))
  ### 1A cue & no-cue, CA Model
  plot(data.soa.toBeep, p.beforeBeep.mixture.nocue(data.soa.toBeep),
       type="l", col="black", lty="dotted",
       xlab="Target-to-Beep SOA (ms)", ylab="P(\"T < B\")",
       main="CA Model")
  for (i in 1:length(data.soa.toCue)) {
    lines(data.soa.toBeep, p.beforeBeep.mixture(data.soa.toBeep, soa.toCue = data.soa.toCue[i], model="CA"),
          col=colors[i], lty=ltys[i])
  }
  legend("topleft", inset=.05, lwd=2,
         c("no cue", labels),
         col=c("black", colors),
         lty=c("dotted", ltys))
  ### 1B , TM Model
  plot(data.soa.toBeep, p.beforeBeep.mixture.nocue(data.soa.toBeep),
       type="l", col="black", lty="dotted",
       xlab="Target-to-Beep SOA (ms)", ylab="P(\"T < B\")",
       main="TM Model")
  for (i in 1:length(data.soa.toCue)) {
    lines(data.soa.toBeep, p.beforeBeep.mixture(data.soa.toBeep, soa.toCue = data.soa.toCue[i], model="TM"),
          col=colors[i], lty=ltys[i])
  }
  legend("topleft", inset=.05, lwd=2,
         c("no cue", labels),
         col=c("black", colors),
         lty=c("dotted", ltys))
  mtext(sprintf("include guesses : %s", if (include.guesses) "yes" else "no"), side=1, outer=TRUE)
}

if (should.plot.test.rau) {
  x <- seq(-5, 5, by=0.1)
  y <- logistic(x)
  plot(x, y, type='l', main="logistic")
  plot(x, rau.transform(y), type='l', main="RAU(logistic")
}
