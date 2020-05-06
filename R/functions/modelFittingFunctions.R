# model fitting functions

# experiment 1

fitModel1.01 <- function(d1.01) {
  brm(data = d1.01, family = binomial,
      overall_num_shocks | trials(rounds_survived) ~ 0 + Intercept + Condition,
      prior = c(prior(normal(0, 1), class = b)),
      sample_prior = TRUE,
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 2113)
}

fitModel1.02 <- function(d1.02) {
  brm(data = d1.02, family = gaussian,
      total_cattle_lost ~ 0 + Intercept + Condition,
      prior = c(prior(normal(0, 100), class = b, coef = 'Intercept'),
                prior(normal(0, 5), class = b, coef = 'Condition')),
      sample_prior = TRUE,
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 2113)
}

fitModel1.03 <- function(d1.03) {
  brm(data = d1.03, family = bernoulli,
      request ~ 0 + Intercept + round_number + Condition 
      + (0 + Intercept + round_number | Group/ID),
      prior = c(prior(normal(0, 1), class = b)),
      sample_prior = TRUE,
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      control = list(adapt_delta = 0.9),
      seed = 2113)
}

fitModel1.04 <- function(d1.04) {
  brm(data = d1.04, family = bernoulli,
      request ~ 0 + Intercept + round_number + Condition
      + (0 + Intercept + round_number | Group/ID),
      prior = c(prior(normal(0, 1), class = b)),
      sample_prior = TRUE,
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      control = list(adapt_delta = 0.95),
      seed = 2113)
}

fitModel1.05 <- function(d1.05) {
  brm(data = d1.05, family = bernoulli,
      notResponded ~ 0 + Intercept + round_number + Condition
      + (0 + Intercept + round_number | Group/ID),
      prior = c(prior(normal(0, 1), class = b),
                prior(student_t(3, 0, 10), class = sd),
                prior(lkj(1), class = cor)),
      sample_prior = TRUE,
      iter = 2500, warmup = 1000, chains = 4, cores = 4,
      control = list(adapt_delta = 0.99),
      seed = 2113)
}

fitModel1.06 <- function(d1.06) {
  brm(data = d1.06, family = bernoulli,
      notFulfilled ~ 0 + Intercept + round_number + Condition
      + (0 + Intercept + round_number | Group/ID),
      prior = c(prior(normal(0, 1), class = b),
                prior(student_t(3, 0, 10), class = sd),
                prior(lkj(1), class = cor)),
      control = list(adapt_delta = 0.99),
      sample_prior = TRUE,
      iter = 2500, warmup = 1000, chains = 4, cores = 4,
      seed = 2113)
}

fitModel1.07 <- function(d1.07) {
  # one participant per row so do not need the ID random effect
  brm(rounds_survived | cens(censored) ~ 0 + Intercept + Condition + (1 | Group), 
      data = d1.07, family = weibull, inits = "0",
      prior = prior(normal(0, 2), class = b, coef = "Condition"),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = TRUE, seed = 2113)
}

fitModel1.08 <- function(d1.08) {
  brm(rounds_survived | cens(censored) ~ 0 + Intercept + prop_rule1 + (1 | Group), 
      data = d1.08, family = weibull, inits = "0",
      prior = prior(normal(0, 2), class = b, coef = "prop_rule1"),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = TRUE, seed = 2113)
}

fitModel1.09 <- function(d1.09) {
  brm(rounds_survived | cens(censored) ~ 0 + Intercept + prop_rule2 + (1 | Group), 
      data = d1.09, family = weibull, inits = "0",
      prior = prior(normal(0, 2), class = b, coef = "prop_rule2"),
      iter = 4000, warmup = 2000, chains = 4, cores = 4,
      control = list(adapt_delta = 0.99),
      sample_prior = TRUE, seed = 2113)
}

fitModel1.10 <- function(d1.10) {
  brm(herd_size_after_shock ~ 0 + Intercept + round_number + Condition*request + 
        (1 + round_number | Group/ID), 
      data = d1.10,
      prior = c(prior(normal(0, 100), class = b, coef = 'Intercept'),
                prior(normal(0, 5), class = b, coef = 'round_number'),
                prior(normal(0, 5), class = b, coef = 'Condition'),
                prior(normal(0, 5), class = b, coef = 'request'),
                prior(normal(0, 5), class = b, coef = 'Condition:request')),
      iter = 4000, warmup = 2000, chains = 4, cores = 4,
      control = list(adapt_delta = 0.95),
      sample_prior = TRUE, seed = 2113)
}

fitModel1.11 <- function(d1.11) {
  brm(request_amount.log ~ 0 + Intercept + round_number + Condition + 
        (1 + round_number | Group/ID), 
      data = d1.11,
      prior = c(prior(normal(0, 5), class = b, coef = 'Intercept'),
                prior(normal(0, 2), class = b, coef = 'round_number'),
                prior(normal(0, 2), class = b, coef = 'Condition')),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = TRUE, seed = 2113)
}

fitModel1.12 <- function(d1.12) {
  brm(diff ~ 0 + Intercept + round_number + Condition + 
        (1 + round_number | Group/ID), 
      data = d1.12,
      prior = c(prior(normal(0, 2), class = b, coef = 'Intercept'),
                prior(normal(0, 2), class = b, coef = 'round_number'),
                prior(normal(0, 2), class = b, coef = 'Condition')),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      control = list(adapt_delta = 0.9),
      sample_prior = TRUE, seed = 2113)
}

fitModel1.13 <- function(d1.13) {
  # deal with outliers by using student-t distribution
  # https://solomonkurz.netlify.app/post/robust-linear-regression-with-the-robust-student-s-t-distribution/
  brm(bf(diff ~ 0 + Intercept + round_number + Condition + 
        (1 + round_number | Group/ID), nu = 4), 
      data = d1.13, family = student,
      prior = c(prior(normal(0, 5), class = b, coef = 'Intercept'),
                prior(normal(0, 2), class = b, coef = 'round_number'),
                prior(normal(0, 2), class = b, coef = 'Condition')),
      iter = 4000, warmup = 2000, chains = 4, cores = 4,
      control = list(adapt_delta = 0.95),
      sample_prior = TRUE, seed = 2113)
}

# experiment 2

fitModel2.01 <- function(d2.01) {
  brm(data = d2.01, family = binomial,
      overall_num_shocks | trials(rounds_survived) ~ 0 + Intercept + Condition,
      prior = c(prior(normal(0, 1), class = b)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 2113)
}

fitModel2.02 <- function(d2.02) {
  brm(data = d2.02, family = gaussian,
      total_cattle_lost ~ 0 + Intercept + Condition,
      prior = c(prior(normal(0, 100), class = b, coef = 'Intercept'),
                prior(normal(0, 5), class = b, coef = 'Condition')),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 2113)
}

fitModel2.03 <- function(d2.03) {
  brm(data = d2.03, family = bernoulli,
      request ~ 0 + Intercept + round_number + Condition
      + (0 + Intercept + round_number + Condition | Group/ID),
      prior = c(prior(normal(0, 1), class = b),
                prior(student_t(3, 0, 10), class = sd),
                prior(lkj(1), class = cor)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = TRUE,
      control = list(adapt_delta = 0.99),
      seed = 2113)
}

fitModel2.04a <- function(d2.04) {
  brm(data = d2.04, family = bernoulli,
    request ~ 0 + Intercept + round_number + Condition
    + (0 + Intercept + round_number + Condition | Group/ID),
    prior = c(prior(normal(0, 1), class = b),
              prior(student_t(3, 0, 10), class = sd),
              prior(lkj(1), class = cor)),
    iter = 2000, warmup = 1000, chains = 4, cores = 4,
    sample_prior = TRUE,
    control = list(adapt_delta = 0.99),
    seed = 2113)
}

fitModel2.04b <- function(d2.04) {
  brm(data = d2.04, family = bernoulli,
      request ~ 0 + Intercept + round_number + Condition*Counterbalancing
      + (0 + Intercept + round_number + Condition | Group/ID),
      prior = c(prior(normal(0, 1), class = b),
                prior(student_t(3, 0, 10), class = sd),
                prior(lkj(1), class = cor)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = TRUE,
      control = list(adapt_delta = 0.99),
      seed = 2113)
}

fitModel2.05 <- function(d2.05) {
  brm(data = d2.05, family = bernoulli,
      notResponded ~ 0 + Intercept + round_number + Condition
      + (0 + Intercept + round_number + Condition | Group/ID),
      prior = c(prior(normal(0, 1), class = b),
                prior(student_t(3, 0, 10), class = sd),
                prior(lkj(1), class = cor)),
      iter = 2500, warmup = 1000, chains = 4, cores = 4,
      sample_prior = TRUE,
      control = list(adapt_delta = 0.99),
      seed = 2113)
}

fitModel2.06a <- function(d2.06) {
  brm(data = d2.06, family = bernoulli,
      notFulfilled ~ 0 + Intercept + round_number + Condition
      + (0 + Intercept + round_number + Condition | Group/ID),
      prior = c(prior(normal(0, 1), class = b),
                prior(student_t(3, 0, 10), class = sd),
                prior(lkj(1), class = cor)),
      control = list(adapt_delta = 0.99),
      sample_prior = TRUE,
      iter = 2500, warmup = 1000, chains = 4, cores = 4,
      seed = 2113)
}

fitModel2.06b <- function(d2.06) {
  brm(data = d2.06, family = bernoulli,
      notFulfilled ~ 0 + Intercept + round_number + Condition*Counterbalancing
      + (0 + Intercept + round_number + Condition | Group/ID),
      prior = c(prior(normal(0, 1), class = b),
                prior(student_t(3, 0, 10), class = sd),
                prior(lkj(1), class = cor)),
      control = list(adapt_delta = 0.99),
      sample_prior = TRUE,
      iter = 2500, warmup = 1000, chains = 4, cores = 4,
      seed = 2113)
}

fitModel2.07 <- function(d2.07) {
  brm(rounds_survived | cens(censored) ~ 0 + Intercept + Condition + (1 | Group/ID), 
      data = d2.07, family = weibull, inits = "0",
      prior = prior(normal(0, 2), class = b, coef = "Condition"),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      control = list(adapt_delta = 0.9, max_treedepth = 15),
      sample_prior = TRUE, seed = 2113)
}

fitModel2.08 <- function(d2.08) {
  brm(rounds_survived | cens(censored) ~ 0 + Intercept + prop_rule1 + (1 | Group/ID), 
      data = d2.08, family = weibull, inits = "0",
      prior = prior(normal(0, 2), class = b, coef = "prop_rule1"),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      control = list(adapt_delta = 0.99),
      sample_prior = TRUE, seed = 2113)
}

fitModel2.09 <- function(d2.09) {
  brm(rounds_survived | cens(censored) ~ 0 + Intercept + prop_rule2 + (1 | Group/ID), 
      data = d2.09, family = weibull, inits = "0",
      prior = prior(normal(0, 2), class = b, coef = "prop_rule2"),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      control = list(adapt_delta = 0.99),
      sample_prior = TRUE, seed = 2113)
}

# experiment 3

fitModel3.01 <- function(d3.01) {
  brm(data = d3.01, family = binomial,
      overall_num_shocks | trials(rounds_survived) ~ 0 + Intercept + Condition,
      prior = c(prior(normal(0, 1), class = b)),
      sample_prior = TRUE,
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 2113)
}

fitModel3.02 <- function(d3.02) {
  brm(data = d3.02, family = gaussian,
      total_cattle_lost ~ 0 + Intercept + Condition,
      prior = c(prior(normal(0, 100), class = b, coef = 'Intercept'),
                prior(normal(0, 5), class = b, coef = 'Condition')),
      sample_prior = TRUE,
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 2113)
}

fitModel3.03 <- function(d3.03) {
  brm(data = d3.03, family = bernoulli,
      request ~ 0 + Intercept + round_number + Condition
      + (0 + Intercept + round_number | ID),
      prior = c(prior(normal(0, 1), class = b)),
      sample_prior = TRUE,
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      control = list(adapt_delta = 0.95),
      seed = 2113)
}

fitModel3.04 <- function(d3.04) {
  brm(data = d3.04, family = bernoulli,
      request ~ 0 + Intercept + round_number + Condition
      + (0 + Intercept + round_number | ID),
      prior = c(prior(normal(0, 1), class = b)),
      sample_prior = TRUE,
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      control = list(adapt_delta = 0.95),
      seed = 2113)
}

fitModel3.05 <- function(d3.05) {
  brm(data = d3.05, family = bernoulli,
      notResponded ~ 0 + Intercept + round_number + Condition
      + (0 + Intercept + round_number | ID),
      prior = c(prior(normal(0, 1), class = b),
                prior(student_t(3, 0, 10), class = sd),
                prior(lkj(1), class = cor)),
      sample_prior = TRUE,
      iter = 2500, warmup = 1000, chains = 4, cores = 4,
      control = list(adapt_delta = 0.99),
      seed = 2113)
}

fitModel3.06 <- function(d3.06) {
  brm(data = d3.06, family = bernoulli,
      notFulfilled ~ 0 + Intercept + round_number + Condition
      + (0 + Intercept + round_number | ID),
      prior = c(prior(normal(0, 1), class = b),
                prior(student_t(3, 0, 10), class = sd),
                prior(lkj(1), class = cor)),
      control = list(adapt_delta = 0.99),
      sample_prior = TRUE,
      iter = 2500, warmup = 1000, chains = 4, cores = 4,
      seed = 2113)
}
