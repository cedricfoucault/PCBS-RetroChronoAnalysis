#### Temporal Order Judgment (TOJ) Model ###

include.guesses <- FALSE
# include.guesses <- TRUE
should.plot.pss <- FALSE
# should.plot.pss <- TRUE
should.plot.rau <- TRUE
should.plot.sensitivity <- TRUE
should.plot.test.rau <- FALSE

# Proportions of trials

# base probability to see the target with no cue
p.seen.nocue <- 0.55

# probability to see the target with a cue as a function of its soa
p.seen <- function(soa.toCue) {
  data.soa.toCue <- c(-100, 100, 400) # from Sergent_Thibault_et_al 2016 PLOS
  data.p.guess <-  c(0.24, 0.32, 0.39) # from Sergent_Thibault_et_al 2016 PLOS
  p.guess.function <- approxfun(data.soa.toCue, data.p.guess, rule=2)
  1 - p.guess.function(soa.toCue)
}

# probability of retroperception among "seen" trials, as a function of the target-cue soa
p.retro <- function(soa.toCue) {
  data.soa.toCue <-  c(100, 400) # from Sergent_Thibault_et_al 2016 PLOS
  data.p.guess <-  c(0.32, 0.39) # from Sergent_Thibault_et_al 2016 PLOS
  data.p.retro <-  (1 - p.seen.nocue - data.p.guess) / p.seen.nocue
  approxfun(c(0, data.soa.toCue), c(0, data.p.retro), rule=2)(soa.toCue) # add (0,0) data point : no retro when soa.toCue <= 0
}

# Point of Subjective Simultaneity (PSS)

## Base Target-to-Beep PSS when target is initially seen, no cue
pss.toBeep.seen <- -10 # -10ms from <W Fujisaki, S Shimojo, M Kashino, S Nishida - Nature neuroscience, 2004>

## Target-Beep PSS with Time Marker Model
pss.toBeep.retro.TM <- function(soa.toCue = 0) {
  pss.toBeep.seen # constant
}

## Target-Beep with Conscious Access Model
pss.toBeep.retro.CA <- function(soa.toCue = 0) {
  pss.toBeep.seen + soa.toCue
}

# Psychometric Functions

jnd.toBeep.seen <- 30 # from <Hanson_et_al 2008 Exp_Brain_Res RecalibrationOfPerceivedTimeAc> estimate of sensitivity to temporal order in the form of just-noticeable difference (JND) (approximately half the offset between the 27 and 73% response levels on the psychometric function)

## logistic function to fit the psychometric functions to
logistic <- function(x, mu = 0, teta = 1) {
  1 / (1 + exp(-(x - mu) / teta))
}

## base psychometric function with no cue
p.beforeBeep.seen <- function(soa.toBeep) {
  logistic(soa.toBeep, pss.toBeep.seen, jnd.toBeep.seen)
}

## psychometric function for retro-seen trials assuming Time Marker model
p.beforeBeep.retro.TM <- function(soa.toBeep, soa.toCue = 0) {
  pss <- pss.toBeep.retro.TM(soa.toCue)
  jnd <- jnd.toBeep.seen + soa.toCue / 10 # lower sensitivity when retro-seen
  logistic(soa.toBeep, pss, jnd)
}

## psychometric function for retro-seen trials assuming Conscious Access model
p.beforeBeep.retro.CA <- function(soa.toBeep, soa.toCue = 0) {
  pss <- pss.toBeep.retro.CA(soa.toCue)
  jnd <- jnd.toBeep.seen + soa.toCue / 10 # lower sensitivity when retro-seen
  logistic(soa.toBeep, pss, jnd)
}

## Rationalized Arcsine Transform (Studebaker)

rau.transform <- function(p) {
  asin(sqrt(p))
}

# Approximation of sensitivity from p(correct response), assuming H = 1- F
sensitivity.transform <- function(pc) {
  2 * qnorm(pc)
}

# Mixture

## Mixture psychometric functions

p.beforeBeep.mixture <- function(soa.toBeep, soa.toCue = 0, model) {
  p.beforeBeep.retro <- if (model == "CA") p.beforeBeep.retro.CA else p.beforeBeep.retro.TM
  p.r <- p.retro(soa.toCue)
  p.beforeBeep <- p.r * p.beforeBeep.retro(soa.toBeep, soa.toCue) + (1 - p.r) * p.beforeBeep.seen(soa.toBeep)
  if (include.guesses) {
    p.s <- p.seen(soa.toCue)
    p.beforeBeep <- p.s * p.beforeBeep + (1 - p.s) * 0.5 # assume no bias in guesses
  } else {
    p.beforeBeep
  }
}
p.beforeBeep.mixture.CA <- function(soa.toBeep, soa.toCue = 0) {
  p.beforeBeep.mixture(soa.toBeep, soa.toCue, model = "CA")
}
p.beforeBeep.mixture.TM <- function(soa.toBeep, soa.toCue = 0) {
  p.beforeBeep.mixture(soa.toBeep, soa.toCue, model = "TM")
}
p.beforeBeep.mixture.nocue <- function(soa.toBeep) {
  if (include.guesses) {
    p.seen.nocue * p.beforeBeep.seen(soa.toBeep) + (1 - p.seen.nocue) * 0.5 # assume no bias in guesses
  } else {
    p.beforeBeep.seen(soa.toBeep)
  }
}

## Mixture PSS

pss.toBeep.mixture.nocue <- pss.toBeep.seen
pss.toBeep.mixture <- function(soa.toCue, model) {
  min <- pss.toBeep.mixture.nocue
  max <- if (model == "CA") pss.toBeep.retro.CA(soa.toCue) else pss.toBeep.retro.TM(soa.toCue)
  soa.test <- seq(min, max, by=1)
  p.test <- p.beforeBeep.mixture(soa.test, soa.toCue, model)
  i <- which(abs(p.test - 0.5) == min(abs(p.test - 0.5)))
  soa.test[i]
}
pss.toBeep.mixture.CA <- function(soa.toCue) {
  pss.toBeep.mixture(soa.toCue, model = "CA")
}
pss.toBeep.mixture.TM <- function(soa.toCue) {
  pss.toBeep.mixture(soa.toCue, model = "TM")
}

# Plots

## comparer no-cue et (retro-)cue

data.soa.toBeep <- seq(-400, 400, by=1)
#data.soa.toCue <- c(-100, 100, 200, 400)
#data.p.beforeBeep.mixture.CA <- sapply(data.soa.toBeep, function(x) p.beforeBeep.mixture.CA(x, soa.toCue = data.soa.toCue))

## Figure 1 : Mixture psychometric function
data.soa.toCue <- 300
quartz()
par(mfrow=c(2, 1))
par(oma=c(2, 0, 0, 0))
### 1A cue & no-cue, CA Model
plot(data.soa.toBeep, p.beforeBeep.mixture.nocue(data.soa.toBeep),
     type="l", col="black", lty="dotted",
     xlab="Target-to-Beep SOA (ms)", ylab="P(\"T < B\")",
     ylim=c(0,1),
     main="CA Model")
lines(data.soa.toBeep, p.beforeBeep.mixture.CA(data.soa.toBeep, soa.toCue = data.soa.toCue),
      col="blue", lty="dashed")
legend("topleft", inset=.05, lwd=2,
       c("no cue", sprintf("retro-cue : %dms", data.soa.toCue)),
       col=c("black","blue"),
       lty=c("dotted", "dashed"))
### 1B , TM Model
plot(data.soa.toBeep, p.beforeBeep.mixture.nocue(data.soa.toBeep),
     type="l", col="black", lty="dotted",
     xlab="Target-to-Beep SOA (ms)", ylab="P(\"T < B\")",
     ylim=c(0,1),
     main="TM Model")
lines(data.soa.toBeep, p.beforeBeep.mixture.TM(data.soa.toBeep, soa.toCue = data.soa.toCue),
      col="green", lty="dashed")
legend("topleft", inset=.05, lwd=2,
       c("no cue", sprintf("retro-cue : %dms", data.soa.toCue)),
       col=c("black","green"),
       lty=c("dotted", "dashed"))
mtext(sprintf("include guesses : %s", if (include.guesses) "yes" else "no"), side=1, outer=TRUE)

## Figure 1bis : Mixture psychometric function in RAU units
if (should.plot.rau) {
  quartz()
  par(mfrow=c(2, 1))
  par(oma=c(2, 0, 0, 0))
  ### 1A cue & no-cue, CA Model
  rau.nocue <- rau.transform(p.beforeBeep.mixture.nocue(data.soa.toBeep))
  rau.CA <- rau.transform(p.beforeBeep.mixture.CA(data.soa.toBeep, soa.toCue = data.soa.toCue))
  rau.TM <- rau.transform(p.beforeBeep.mixture.TM(data.soa.toBeep, soa.toCue = data.soa.toCue))
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
  pc.CA <- c(1 - p.beforeBeep.mixture.CA(data.soa.toBeep.neg, soa.toCue = data.soa.toCue),
             p.beforeBeep.mixture.CA(data.soa.toBeep.pos, soa.toCue = data.soa.toCue))
  pc.TM <- c(1 - p.beforeBeep.mixture.TM(data.soa.toBeep.neg, soa.toCue = data.soa.toCue),
             p.beforeBeep.mixture.TM(data.soa.toBeep.pos, soa.toCue = data.soa.toCue))
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
    lines(data.soa.toBeep, p.beforeBeep.mixture.CA(data.soa.toBeep, soa.toCue = data.soa.toCue[i]),
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
    lines(data.soa.toBeep, p.beforeBeep.mixture.TM(data.soa.toBeep, soa.toCue = data.soa.toCue[i]),
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
