#### Temporal Order Judgment (TOJ) Model ###

include.guesses <- FALSE
# include.guesses <- TRUE

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

# Target-to-Beep Point of Subjective Simultaneity (PSS)

## base PSS when target is initially seen, no cue
pss.toBeep.seen <- -10 # -10ms from <W Fujisaki, S Shimojo, M Kashino, S Nishida - Nature neuroscience, 2004>

## PSS when target is seen by retroperception 
pss.toBeep.retro <- function(soa.toCue = 0, model) {
  if (model == "CA") # Conscious Access Model: PSS close to cue
    pss.toBeep.seen + soa.toCue
  else # Time Marker Model: PSS close to beep
    pss.toBeep.seen # constant
}

# Slope

jnd.toBeep.seen <- 30 # from <Hanson_et_al 2008 Exp_Brain_Res RecalibrationOfPerceivedTimeAc> estimate of sensitivity to temporal order in the form of just-noticeable difference (JND) (approximately half the offset between the 27 and 73% response levels on the psychometric function)

jnd.toBeep.retro <- function(soa.toCue = 0, model) {
  jnd.toBeep.seen + soa.toCue / 10 # lower sensitivity when retro-seen
}

# Psychometric Functions

## logistic function to fit the psychometric functions to
logistic <- function(x, mu = 0, teta = 1) {
  1 / (1 + exp(-(x - mu) / teta))
}

## base psychometric function with no cue
p.beforeBeep.seen <- function(soa.toBeep) {
  logistic(soa.toBeep, pss.toBeep.seen, jnd.toBeep.seen)
}

## psychometric function for retro-seen trials
p.beforeBeep.retro <- function(soa.toBeep, soa.toCue = 0, model) {
  pss <- pss.toBeep.retro(soa.toCue, model)
  jnd <- jnd.toBeep.retro(soa.toCue, model)
  logistic(soa.toBeep, pss, jnd)
}

## Rationalized Arcsine Transform (Studebaker)

rau.transform <- function(p) {
  asin(sqrt(p))
}

# Approximation of sensitivity from p(correct response), assuming H = 1 - F (no bias)
sensitivity.transform <- function(pc) {
  2 * qnorm(pc)
}

# Mixture

## Mixture psychometric functions

p.beforeBeep.mixture <- function(soa.toBeep, soa.toCue = 0, model) {
  p.r <- p.retro(soa.toCue)
  p.beforeBeep <- p.r * p.beforeBeep.retro(soa.toBeep, soa.toCue, model) + (1 - p.r) * p.beforeBeep.seen(soa.toBeep)
  if (include.guesses) {
    p.s <- p.seen(soa.toCue)
    p.beforeBeep <- p.s * p.beforeBeep + (1 - p.s) * 0.5 # assume no bias in guesses
  } else {
    p.beforeBeep
  }
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
  max <- pss.toBeep.retro(soa.toCue, model)
  soa.test <- seq(min, max, by=1)
  p.test <- p.beforeBeep.mixture(soa.test, soa.toCue, model)
  i <- which(abs(p.test - 0.5) == min(abs(p.test - 0.5)))
  soa.test[i]
}

# Plots

## comparer no-cue et (retro-)cue

data.soa.toBeep <- seq(-400, 400, by=1)

## Figure 1 : Mixture psychometric function
data.soa.toCue <- 200
quartz()
par(mfrow=c(2, 1))
par(oma=c(2, 0, 0, 0))
plot.mixtures <- function(model, color) {
  plot(data.soa.toBeep, p.beforeBeep.mixture.nocue(data.soa.toBeep),
       type="l", col="black", lty="dotted",
       xlab="Target-to-Beep SOA (ms)", ylab="P(\"T < B\")",
       ylim=c(0,1),
       main=sprintf("%s Model", model))
  lines(data.soa.toBeep, p.beforeBeep.mixture(data.soa.toBeep, soa.toCue = data.soa.toCue, model),
        col=color, lty="dashed")
  legend("topleft", inset=.05, lwd=2,
         c("no cue", sprintf("retro-cue : %dms", data.soa.toCue)),
         col=c("black", color),
         lty=c("dotted", "dashed"))
}
plot.mixtures("CA", "blue")
plot.mixtures("TM", "green")
mtext(sprintf("include guesses : %s", if (include.guesses) "yes" else "no"), side=1, outer=TRUE)
