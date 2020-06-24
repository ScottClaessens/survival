bayesFactor1 <- function(model) {
  (1 / hypothesis(
    model,
    "inv_logit_scaled(Intercept) - inv_logit_scaled(Intercept + Condition) = 0",
    seed = 2113)$hypothesis$Evid.Ratio) %>% 
    round(2)
}

bayesFactor2 <- function(model) {
  (1 / hypothesis(
    model, 
    "inv_logit_scaled(Intercept + Counterbalancing) - inv_logit_scaled(Intercept + Counterbalancing + Condition + Condition:Counterbalancing) = 0", 
    seed = 2113)$hypothesis$Evid.Ratio) %>% 
    round(2)
}