# drake plan
plan <- drake_plan(
  
  ################
  # Experiment 1 #
  ################
  
  # load data
  d1.00 = loadData('data/exp1data.csv'),
  # data wrangling
  d1.01 = wrangleData1.01(d1.00),
  d1.02 = wrangleData1.02(d1.00),
  d1.03 = wrangleData1.03(d1.00),
  d1.04 = wrangleData1.04(d1.00),
  d1.05 = wrangleData1.05(d1.00),
  d1.06 = wrangleData1.06(d1.00),
  d1.07 = wrangleData1.07(d1.00),
  d1.08 = wrangleData1.08(d1.00),
  d1.09 = wrangleData1.09(d1.00),
  d1.10 = wrangleData1.10(d1.00),
  d1.11 = wrangleData1.11(d1.00),
  d1.12 = wrangleData1.12(d1.00),
  d1.13 = wrangleData1.13(d1.00),
  # modelling
  m1.01 = fitModel1.01(d1.01), # game control check - number of shocks
  m1.02 = fitModel1.02(d1.02), # game control check - total amount lost due to shocks
  m1.03 = fitModel1.03(d1.03), # probability of requesting
  m1.04 = fitModel1.04(d1.04), # probability of requesting when beneath threshold
  m1.05 = fitModel1.05(d1.05), # probability of responding
  m1.06 = fitModel1.06(d1.06), # probability of fulfilling request when able
  m1.07 = fitModel1.07(d1.07), # survival analysis - condition
  m1.08 = fitModel1.08(d1.08), # survival analysis - proportion of rule 1 cheating
  m1.09 = fitModel1.09(d1.09), # survival analysis - proportion of rule 2 cheating
  m1.10 = fitModel1.10(d1.10), # herd size when requesting / not requesting
  m1.11 = fitModel1.11(d1.11), # log amount requested
  m1.12 = fitModel1.12(d1.12), # difference between amount requested and amount needed
  m1.13 = fitModel1.13(d1.13), # difference between amount requested by partner and amount given
  # prior summaries
  priors1.01 = prior_summary(m1.01),
  priors1.02 = prior_summary(m1.02),
  priors1.03 = prior_summary(m1.03),
  priors1.04 = prior_summary(m1.04),
  priors1.05 = prior_summary(m1.05),
  priors1.06 = prior_summary(m1.06),
  priors1.07 = prior_summary(m1.07),
  priors1.08 = prior_summary(m1.08),
  priors1.09 = prior_summary(m1.09),
  priors1.10 = prior_summary(m1.10),
  priors1.11 = prior_summary(m1.11),
  priors1.12 = prior_summary(m1.12),
  priors1.13 = prior_summary(m1.13),
  # parameter plots
  pars1.01 = mcmc_plot(m1.01),
  pars1.02 = mcmc_plot(m1.02),
  pars1.03 = mcmc_plot(m1.03),
  pars1.04 = mcmc_plot(m1.04),
  pars1.05 = mcmc_plot(m1.05),
  pars1.06 = mcmc_plot(m1.06),
  pars1.07 = mcmc_plot(m1.07),
  pars1.08 = mcmc_plot(m1.08),
  pars1.09 = mcmc_plot(m1.09),
  pars1.10 = mcmc_plot(m1.10),
  pars1.11 = mcmc_plot(m1.11),
  pars1.12 = mcmc_plot(m1.12),
  pars1.13 = mcmc_plot(m1.13),
  # posterior
  post1.01 = posterior_samples(m1.01),
  post1.02 = posterior_samples(m1.02),
  post1.03 = posterior_samples(m1.03),
  post1.04 = posterior_samples(m1.04),
  post1.05 = posterior_samples(m1.05),
  post1.06 = posterior_samples(m1.06),
  post1.07 = posterior_samples(m1.07),
  post1.08 = posterior_samples(m1.08),
  post1.09 = posterior_samples(m1.09),
  post1.10 = posterior_samples(m1.10),
  post1.11 = posterior_samples(m1.11),
  post1.12 = posterior_samples(m1.12),
  post1.13 = posterior_samples(m1.13),
  # bayes factors
  bf1.03 = bayesFactor1(m1.03),
  bf1.04 = bayesFactor1(m1.04),
  bf1.05 = bayesFactor1(m1.05),
  bf1.06 = bayesFactor1(m1.06),
  bf1.07 = bayesFactor3(m1.07, "Condition"),
  bf1.08 = bayesFactor3(m1.08, "prop_rule1"),
  bf1.09 = bayesFactor3(m1.09, "prop_rule2"),
  # conditional effects
  cond1.07 = getCondEffects1.07(m1.07),
  cond1.08 = getCondEffects1.08(m1.08),
  cond1.09 = getCondEffects1.09(m1.09),
  # figures
  fig1 = createFig1(),
  # report
  exp1rmd = rmarkdown::render(
    knitr_in("exp1.Rmd"),
    output_file = file_out("exp1.html"),
    quiet = TRUE
  ),
  
  ################
  # Experiment 2 #
  ################
  
  # load data
  d2.00 = loadData('data/exp2data.csv'),
  # data wrangling
  d2.01 = wrangleData2.01(d2.00),
  d2.02 = wrangleData2.02(d2.00),
  d2.03 = wrangleData2.03(d2.00),
  d2.04 = wrangleData2.04(d2.00),
  d2.05 = wrangleData2.05(d2.00),
  d2.06 = wrangleData2.06(d2.00),
  d2.07 = wrangleData2.07(d2.00),
  d2.08 = wrangleData2.08(d2.00),
  d2.09 = wrangleData2.09(d2.00),
  # modelling
  m2.01  = fitModel2.01(d2.01),  # game control check - number of shocks
  m2.02  = fitModel2.02(d2.02),  # game control check - total amount lost due to shocks
  m2.03  = fitModel2.03(d2.03),  # probability of requesting
  m2.04a = fitModel2.04a(d2.04), # probability of requesting when beneath threshold
  m2.04b = fitModel2.04b(d2.04), # probability of requesting when beneath threshold (counterbalancing)
  m2.05  = fitModel2.05(d2.05),  # probability of responding
  m2.06a = fitModel2.06a(d2.06), # probability of fulfilling request when able
  m2.06b = fitModel2.06b(d2.06), # probability of fulfilling request when able (counterbalancing)
  m2.07  = fitModel2.07(d2.07),  # survival analysis - condition
  m2.08  = fitModel2.08(d2.08),  # survival analysis - proportion of rule 1 cheating
  m2.09  = fitModel2.09(d2.09),  # survival analysis - proportion of rule 2 cheating
  # prior summaries
  priors2.01  = prior_summary(m2.01),
  priors2.02  = prior_summary(m2.02),
  priors2.03  = prior_summary(m2.03),
  priors2.04a = prior_summary(m2.04a),
  priors2.04b = prior_summary(m2.04b),
  priors2.05  = prior_summary(m2.05),
  priors2.06a = prior_summary(m2.06a),
  priors2.06b = prior_summary(m2.06b),
  priors2.07  = prior_summary(m2.07),
  priors2.08  = prior_summary(m2.08),
  priors2.09  = prior_summary(m2.09),
  # parameter plots
  pars2.01  = mcmc_plot(m2.01),
  pars2.02  = mcmc_plot(m2.02),
  pars2.03  = mcmc_plot(m2.03),
  pars2.04a = mcmc_plot(m2.04a),
  pars2.04b = mcmc_plot(m2.04b),
  pars2.05  = mcmc_plot(m2.05),
  pars2.06a = mcmc_plot(m2.06a),
  pars2.06b = mcmc_plot(m2.06b),
  pars2.07  = mcmc_plot(m2.07),
  pars2.08  = mcmc_plot(m2.08),
  pars2.09  = mcmc_plot(m2.09),
  # posterior
  post2.01  = posterior_samples(m2.01),
  post2.02  = posterior_samples(m2.02),
  post2.03  = posterior_samples(m2.03),
  post2.04a = posterior_samples(m2.04a),
  post2.04b = posterior_samples(m2.04b),
  post2.05  = posterior_samples(m2.05),
  post2.06a = posterior_samples(m2.06a),
  post2.06b = posterior_samples(m2.06b),
  post2.07  = posterior_samples(m2.07),
  post2.08  = posterior_samples(m2.08),
  post2.09  = posterior_samples(m2.09),
  # bayes factors
  bf2.03   = bayesFactor1(m2.03),
  bf2.04a  = bayesFactor1(m2.04a),
  bf2.04b1 = bayesFactor1(m2.04b),
  bf2.04b2 = bayesFactor2(m2.04b),
  bf2.05   = bayesFactor1(m2.05),
  bf2.06a  = bayesFactor1(m2.06a),
  bf2.06b1 = bayesFactor1(m2.06b),
  bf2.06b2 = bayesFactor2(m2.06b),
  bf2.07   = bayesFactor3(m2.07, "Condition"),
  bf2.08   = bayesFactor3(m2.08, "prop_rule1"),
  bf2.09   = bayesFactor3(m2.09, "prop_rule2"),
  # conditional effects
  cond2.07 = getCondEffects2.07(m2.07),
  cond2.08 = getCondEffects2.08(m2.08),
  cond2.09 = getCondEffects2.09(m2.09),
  # figures
  fig2 = createFig2(),
  # report
  exp2rmd = rmarkdown::render(
    knitr_in("exp2.Rmd"),
    output_file = file_out("exp2.html"),
    quiet = TRUE
  ),
  
  ################
  # Experiment 3 #
  ################
  
  # load data
  d3.00 = loadData('data/exp3data.csv'),
  # data wrangling
  d3.01 = wrangleData3.01(d3.00),
  d3.02 = wrangleData3.02(d3.00),
  d3.03 = wrangleData3.03(d3.00),
  d3.04 = wrangleData3.04(d3.00),
  d3.05 = wrangleData3.05(d3.00),
  d3.06 = wrangleData3.06(d3.00),
  # modelling
  m3.01 = fitModel3.01(d3.01), # game control check - number of shocks
  m3.02 = fitModel3.02(d3.02), # game control check - total amount lost due to shocks
  m3.03 = fitModel3.03(d3.03), # probability of requesting
  m3.04 = fitModel3.04(d3.04), # probability of requesting when beneath threshold
  m3.05 = fitModel3.05(d3.05), # probability of responding
  m3.06 = fitModel3.06(d3.06), # probability of fulfilling request when able
  # prior summaries
  priors3.01 = prior_summary(m3.01),
  priors3.02 = prior_summary(m3.02),
  priors3.03 = prior_summary(m3.03),
  priors3.04 = prior_summary(m3.04),
  priors3.05 = prior_summary(m3.05),
  priors3.06 = prior_summary(m3.06),
  # parameter plots
  pars3.01 = mcmc_plot(m3.01),
  pars3.02 = mcmc_plot(m3.02),
  pars3.03 = mcmc_plot(m3.03),
  pars3.04 = mcmc_plot(m3.04),
  pars3.05 = mcmc_plot(m3.05),
  pars3.06 = mcmc_plot(m3.06),
  # posterior
  post3.01 = posterior_samples(m3.01),
  post3.02 = posterior_samples(m3.02),
  post3.03 = posterior_samples(m3.03),
  post3.04 = posterior_samples(m3.04),
  post3.05 = posterior_samples(m3.05),
  post3.06 = posterior_samples(m3.06),
  # bayes factors
  bf3.03 = bayesFactor1(m3.03),
  bf3.04 = bayesFactor1(m3.04),
  bf3.05 = bayesFactor1(m3.05),
  bf3.06 = bayesFactor1(m3.06),
  # figures
  fig3 = createFig3(),
  # report
  exp3rmd = rmarkdown::render(
    knitr_in("exp3.Rmd"),
    output_file = file_out("exp3.html"),
    quiet = TRUE
  )
)