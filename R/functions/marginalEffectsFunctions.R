# marginal effects

# experiment 1

getMarginalEffects1.07 <- function(m1.07) {
  g <- marginal_effects(m1.07)
  plot(g, plot = FALSE)[[1]] + 
    scale_x_continuous(name = "Condition", breaks = 0:1, labels = c("Visible", "Hidden")) +
    scale_y_continuous(name = "Predicted number of rounds survived") +
    ggtitle("Survival rate in Experiment 1 (MTurk)")
}

getMarginalEffects1.08 <- function(m1.08) {
  g <- marginal_effects(m1.08)
  plot(g, plot = FALSE)[[1]] + 
    scale_x_continuous(name = "Proportion of Rule 1 cheating") +
    scale_y_continuous(name = "Predicted number of rounds survived") +
    ggtitle("Survival rate in Experiment 1 (MTurk)")
}

getMarginalEffects1.09 <- function(m1.09) {
  g <- marginal_effects(m1.09)
  plot(g, plot = FALSE)[[1]] + 
    scale_x_continuous(name = "Proportion of Rule 2 cheating") +
    scale_y_continuous(name = "Predicted number of rounds survived") +
    ggtitle("Survival rate in Experiment 1 (MTurk)")
}

# experiment 2

getMarginalEffects2.07 <- function(m2.07) {
  g <- marginal_effects(m2.07)
  plot(g, plot = FALSE)[[1]] + 
    scale_x_continuous(name = "Condition", breaks = 0:1, labels = c("Visible", "Hidden")) +
    scale_y_continuous(name = "Predicted number of rounds survived") +
    ggtitle("Survival rate in Experiment 2 (Lab)")
}

getMarginalEffects2.08 <- function(m2.08) {
  g <- marginal_effects(m2.08)
  plot(g, plot = FALSE)[[1]] + 
    scale_x_continuous(name = "Proportion of Rule 1 cheating") +
    scale_y_continuous(name = "Predicted number of rounds survived") +
    ggtitle("Survival rate in Experiment 2 (Lab)")
}

getMarginalEffects2.09 <- function(m2.09) {
  g <- marginal_effects(m2.09)
  plot(g, plot = FALSE)[[1]] + 
    scale_x_continuous(name = "Proportion of Rule 2 cheating") +
    scale_y_continuous(name = "Predicted number of rounds survived") +
    ggtitle("Survival rate in Experiment 2 (Lab)")
}