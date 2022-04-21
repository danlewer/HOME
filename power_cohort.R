library(MASS) # for negative binomial model

years <- 3
alpha <- 0.05

supported <- 5000
readiness <- 5000

baseline <- 0.1 # episodes per month

# one-third visited A&E in past 12 months - https://bjgp.org/content/69/685/e515
# with negative binomial model and monthly rate of 0.1, size parameter is 0.25
mean(rnbinom(1e5, size = 0.25, mu = 0.1 * 12) > 0)

housing <- c(rep('readiness', readiness * years), rep('supported', supported * years))
housing <- factor(housing, c('readiness', 'supported'))

sim <- function (effect = 0.7, model = 'poisson') {
  fup_supported <- rpois(supported * years, 12)
  fup_readiness <- rpois(readiness * years, 12)
  fup <- c(fup_supported, fup_readiness) + 1
  rate <- rep(baseline, length(fup))
  rate[housing == 'supported'] <- baseline * effect
  events <- rnbinom(n = length(fup), size = 0.25, mu = rate * fup)
  # check mean events
  # tapply(events / fup, housing, FUN = mean)
  m <- if (model == 'poisson') {
    glm(events ~ housing + offset(log(fup)), family = 'poisson')
  } else {
    glm.nb(events ~ housing + offset(log(fup)))}
  summary(m)$coef[2,c(1, 4)]
}

# do simulation

run_sim <- function (effect = 0.7, nsim = 1000) {
  s <- NULL
  for (i in seq_len(nsim)) {
    print (paste0(effect, ': ', i))
    s <- rbind(s, sim(effect))
  }
  return(s)
}

effects <- seq(0.90, 0.99, 0.01)

set.seed(17)
t1 <- proc.time()
pwrs <- lapply(effects, run_sim)
proc.time() - t1

# estimate and plot power

pwrs2 <- cbind(effects, t(sapply(pwrs, function (x) c(exp(mean(x[,1])), mean(x[,2] < alpha)))))

logeffects <- log(effects)
m <- glm(pwrs2[,3] ~ logeffects, family = 'quasibinomial')
effects2 <- seq(0.9, 0.99, 0.001)
pred <- predict(m, newdata = data.frame(logeffects = log(effects2)), type = 'response')
lines(effects2, pred)

effects2[which.min(abs(pred - 0.8))] # power at 80% = 0.963
