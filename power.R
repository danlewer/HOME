# interrupted time series
# =======================

# inputs

pop <- 7500
baseline_rate <- 2
monthly_rate <- baseline_rate / 12
effect <- 0.9
mba <- 24 # months before and after policy

# model parameters

time <- 1:(mba * 2)
policy <- rep(0:1, each = mba)
pops <- rep(pop, mba * 2)
rates <- c(rep(monthly_rate, mba), rep(monthly_rate * effect, mba))

# example simulation

set.seed(50)
events <- sapply(rates, function(x) rpois(1, x * pop))
m <- glm(events ~ time * policy + offset(log(pops)), family = 'poisson')

dev.off()
plot(events, ylim = c(0, max(events)), xlab = 'Time')
pred <- data.frame(time = time, policy = policy, pops = pop)
pred$events <- predict(m, newdata = pred, type = 'response')
with(pred, lines(time, events))

# simulation multiple times

sf <- function () {
  events <- sapply(rates, function(x) rpois(1, x * pop))
  m <- glm(events ~ time * policy + offset(log(pops)), family = 'poisson')
  summary(m)$coef[3,c(1, 4)]
}

nsim <- 1000
sims <- sapply(seq_len(nsim), function (x) sf())
exp(mean(sims[1,])) # effect size

mean(sims[2,] < 0.05) # power

par(mfrow = c(1, 2))
hist(sims[1,], main = 'log IRR', xlab = NA)
hist(sims[2,], main = 'p-value', xlab = NA)

# find 80% power

sf2 <- function (effect = 0.8) {
  rates <- c(rep(monthly_rate, mba), rep(monthly_rate * effect, mba))
  events <- sapply(rates, function(x) rpois(1, x * pop))
  m <- glm(events ~ time * policy + offset(log(pops)), family = 'poisson')
  summary(m)$coef[3,c(1, 4)]
}

pw <- function (effect, nsim = 1000, alpha = 0.05) {
  print(effect)
  sims <- sapply(seq_len(nsim), function (x) sf2(effect))
  c(exp(mean(sims[1,])), mean(sims[2,] < alpha))
}

pws <- seq(0.8, 1, 0.01)
find80 <- sapply(pws, pw, nsim = 10000)
find80 <- cbind(pws, t(find80))

test_pws <- seq(0.8, 1, 0.001)
curve <- predict(glm(find80[,3] ~ pws, family = 'binomial'), newdata = data.frame(pws = test_pws), type = 'response')

power80 <- test_pws[which.min(abs(curve - 0.8))]
curve[which.min(abs(curve - 0.8))]

dev.off()
plot(find80[,1], find80[,3], xlab = 'Effect size', ylab = 'Power')
lines(test_pws, curve)
segments(0.8, 0.8, x1 = power80, lty = 2)
segments(power80, 0, y1 = 0.8, lty = 2)
