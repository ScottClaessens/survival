# functions
loadData <- function(file) read.csv(file, header = TRUE) %>% as_tibble()
source("R/functions/dataWranglingFunctions.R")
source("R/functions/modelFittingFunctions.R")
source("R/functions/bayesFactorFunctions.R")
source("R/functions/fig1Function.R")
source("R/functions/fig2Function.R")
source("R/functions/fig3Function.R")
source("R/functions/condEffectsFunctions.R")