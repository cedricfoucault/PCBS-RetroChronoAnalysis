#### Temporal Order Judgment (TOJ) Model ###

# Chaque essai a 3 issues possibles :
# 1. soit la cible n’a pas été perçue consciemment ;
# 2. soit la cible a été perçue consciemment directement ;
# 3. soit la cible a été perçue consciemment rétrospectivement grâce à la cue.
# Dans la suite, on qualifiera ces 3 types d'essai par 1. "guess", 2. "seen", et 3. "retro"

# Proportions of trials

# baseline probability to see the target in no-cue condition,
# data from Sergent_Thibault_et_al 2016 PLOS
p.seen.nocue <- 0.55

## proportion of "guess" trials, with cue present
p.guess <- function(soa.toCue) {
  # data from Sergent_Thibault_et_al 2016 PLOS
  data.soa.toCue <- c(-100, 100, 400)
  data.p.guess <-  c(0.24, 0.32, 0.39)
  # linear interpolation of data points
  approxfun(data.soa.toCue, data.p.guess, rule=2)(soa.toCue)
}

## fraction of "retro" trials among both "seen" and "retro" trials
fraction.retro <- function(soa.toCue) {
  if (soa.toCue >= 0) {
    ## defined as the increase in probability to see the target from the baseline
    (1 - p.seen.nocue - p.guess(soa.toCue)) / p.seen.nocue
  } else {
    0 # retroperception cannot happen when the cue appears before the target
  }
}

### with cue present
p.trials <- function(soa.toCue, include.guesses = TRUE) {
  p.g <- if (include.guesses) p.guess(soa.toCue) else 0
  p.retro <- (1 - p.g) * fraction.retro(soa.toCue)
  p.seen <- 1 - p.g - p.retro
  list(guess = p.g, seen = p.seen, retro = p.retro)
}

### no cue
p.trials.nocue <- function(include.guesses = TRUE) {
  p.g <- if (include.guesses) 1 - p.seen.nocue else 0
  p.s <- 1 - p.g
  list(guess = p.g, seen = p.s)
}

# Psychometric Functions :
# probability of getting response "Target before Beep"
# as a function of the SOA between the Target and the Beep
# in the TOJ task

## PSS (Point of Subjective Simultaneity)

## for "seen" trials
pss.toBeep.seen <- -10 # -10ms from <W Fujisaki, S Shimojo, M Kashino, S Nishida - Nature neuroscience, 2004>

### for "retro" trials
pss.toBeep.retro <- function(soa.toCue = 0, model) {
  if (model == "CA") { # Conscious Access Model: PSS close to cue
    pss.toBeep.seen + soa.toCue
  } else { # Time Marker Model: PSS close to beep
    pss.toBeep.seen # constant
  } 
}

## Slope

### for "seen" trials
### data from <Hanson_et_al 2008 Exp_Brain_Res RecalibrationOfPerceivedTimeAc> estimate of sensitivity to temporal order in the form of just-noticeable difference (JND) (approximately half the offset between the 27 and 73% response levels on the psychometric function)
jnd.toBeep.seen <- 30

### for "retro" trials
jnd.toBeep.retro <- function(soa.toCue = 0, model) {
  jnd.toBeep.seen + soa.toCue / 10 # lower precision due to the degradation of the sensory memory trace over time
}

## Logistic function to fit the psychometric functions to
logistic <- function(x, mu = 0, teta = 1) {
  1 / (1 + exp(-(x - mu) / teta))
}


## Individual Component Functions

### for "guess" trials
p.beforeBeep.guess <- function(soa.toBeep) {
  0.5 # assuming no bias
}

### for "seen" trials
p.beforeBeep.seen <- function(soa.toBeep) {
  logistic(soa.toBeep, pss.toBeep.seen, jnd.toBeep.seen)
}

### for "retro" trials
p.beforeBeep.retro <- function(soa.toBeep, soa.toCue = 0, model) {
  pss <- pss.toBeep.retro(soa.toCue, model)
  jnd <- jnd.toBeep.retro(soa.toCue, model)
  logistic(soa.toBeep, pss, jnd)
}

## Mixture Functions

### with cue present
p.beforeBeep.mixture <- function(soa.toBeep, soa.toCue = 0, model, include.guesses = TRUE) {
  proportions <- p.trials(soa.toCue, include.guesses)
  proportions$retro * p.beforeBeep.retro(soa.toBeep, soa.toCue, model) +
  proportions$seen * p.beforeBeep.seen(soa.toBeep) +
  proportions$guess * p.beforeBeep.guess(soa.toBeep)
}

### no cue
p.beforeBeep.mixture.nocue <- function(soa.toBeep, include.guesses = TRUE) {
  proportions <- p.trials.nocue(include.guesses)
  proportions$guess * p.beforeBeep.guess(soa.toBeep) +
  proportions$seen * p.beforeBeep.seen(soa.toBeep)
}

# Plots

## comparer no-cue et (retro-)cue

data.soa.toBeep <- seq(-400, 400, by=1)
#include.guesses <- FALSE
include.guesses <- TRUE

## Figure 1 : Mixture psychometric function : with cue vs no cue
### 1A : Model CA
### 1B : Model TM
data.soa.toCue <- -100
quartz()
par(mfrow=c(2, 1))
par(oma=c(2, 0, 0, 0))
plot.mixtures <- function(model, color) {
  plot(data.soa.toBeep, p.beforeBeep.mixture.nocue(data.soa.toBeep, include.guesses),
       type="l", col="black", lty="dotted",
       xlab="Target-to-Beep SOA (ms)", ylab="P(\"T < B\")",
       ylim=c(0,1),
       main=sprintf("%s Model", model))
  lines(data.soa.toBeep, p.beforeBeep.mixture(data.soa.toBeep, soa.toCue = data.soa.toCue, model, include.guesses),
        col=color, lty="dashed")
  legend("topleft", inset=.05, lwd=2,
         c("no cue", sprintf("retro-cue : %dms", data.soa.toCue)),
         col=c("black", color),
         lty=c("dotted", "dashed"))
}
plot.mixtures("CA", "blue")
plot.mixtures("TM", "green")
mtext(sprintf("include guesses : %s", if (include.guesses) "yes" else "no"), side=1, outer=TRUE)
