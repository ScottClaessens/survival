# fig 1 function

createFig1 <- function() {
  # prepare for gA
  post <- readd(post1.04)
  visible_prob <- inv_logit_scaled(post$b_Intercept)
  hidden_prob  <- inv_logit_scaled(post$b_Intercept + post$b_Condition)
  gA <-
    tibble(condition = factor(c('Visible','Hidden'), levels = c('Visible','Hidden')),
           median    = c(median(visible_prob), median(hidden_prob)),
           pi.025    = c(quantile(visible_prob, .025), quantile(hidden_prob, .025)),
           pi.250    = c(quantile(visible_prob, .250), quantile(hidden_prob, .250)),
           pi.750    = c(quantile(visible_prob, .750), quantile(hidden_prob, .750)),
           pi.975    = c(quantile(visible_prob, .975), quantile(hidden_prob, .975))) %>%
    ggplot(aes(x = condition, y = median, group = 1)) +
    geom_line() + 
    geom_point() + 
    geom_ribbon(aes(ymin = pi.025, ymax = pi.975), alpha = 0.2) +
    # geom_ribbon(aes(ymin = pi.250, ymax = pi.750), alpha = 0.2) +
    geom_text(aes(x = Inf, y = Inf, hjust = 1, vjust = 1.5, label = "BF = 11.10"), size = 3.5) +
    ylim(0,1) + 
    labs(y = 'Median predicted probability of\nrequesting when above threshold',
         x = 'Condition') +
    theme_classic() +
    theme(panel.grid.major.x = element_blank(),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 8))
  # prepare for gB and gC
  d.plot <-
    readd(d1.00) %>%
    select(ID, Group, Condition, ends_with('.player.herd_size_after_shock'), 
           ends_with('.player.request')) %>%
    gather(key, value, -ID, -Group, -Condition) %>%
    extract(key, c("round_number", "variable"), 
            "SurvivalGame.(.|..).player.(herd_size_after_shock|request)") %>%
    spread(variable, value) %>%
    mutate(round_number = as.integer(round_number)) %>%
    arrange(round_number, ID, Group) %>%
    mutate(above        = ifelse(herd_size_after_shock >= 64 & !is.na(request), 1, 0),
           requestAbove = ifelse(request == 1 & herd_size_after_shock >= 64, 1, 0),
           requestAbove = ifelse(is.na(request), NA, requestAbove)) %>%
    group_by(Condition, round_number) %>%
    summarise(alive        = sum(!is.na(herd_size_after_shock)),
              above        = sum(above, na.rm = T),
              request      = sum(request, na.rm = T),
              requestAbove = sum(requestAbove, na.rm = T))
  gB <-
    d.plot %>%
    filter(Condition == 0) %>%
    ggplot(aes(x = round_number)) +
    geom_line(aes(y = alive), colour = "steelblue") + 
    geom_point(aes(y = alive), colour = "steelblue") +
    geom_bar(aes(y = above), stat = "identity", fill = "steelblue", alpha = 0.075, width = 0.8) +
    geom_bar(aes(y = requestAbove), stat = "identity", fill = "steelblue", width = 0.8) +
    scale_x_continuous('Round number', c(5, 10, 15, 20, 25)) +
    scale_y_continuous(breaks = c(seq(0, 40, 10)), limits = c(0, 42)) +
    ylab('Number of players\nrequesting when above threshold') +
    theme_classic() +
    theme(panel.grid.major.x = element_blank(),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 8))
  gC <-
    d.plot %>%
    filter(Condition == 1) %>%
    ggplot(aes(x = round_number)) +
    geom_line(aes(y = alive), colour = "firebrick2") + 
    geom_point(aes(y = alive), colour = "firebrick2") +
    geom_bar(aes(y = above), stat = "identity", fill = "firebrick2", alpha = 0.075, width = 0.8) +
    geom_bar(aes(y = requestAbove), stat = "identity", fill = "firebrick2", width = 0.8) +
    scale_x_continuous('Round number', c(5, 10, 15, 20, 25)) +
    scale_y_continuous(breaks = c(seq(0, 40, 10)), limits = c(0, 42), position = "right") +
    ylab('Number of players alive') +
    theme_classic() +
    theme(panel.grid.major.x = element_blank(),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 8),
          axis.title.y.right = element_text(margin = margin(0, 0, 0, 6)))
  # prepare for gD
  post <- readd(post1.06)
  visible_prob <- inv_logit_scaled(post$b_Intercept)
  hidden_prob  <- inv_logit_scaled(post$b_Intercept + post$b_Condition)
  gD <-
    tibble(condition = factor(c('Visible','Hidden'), levels = c('Visible','Hidden')),
           median    = c(median(visible_prob), median(hidden_prob)),
           pi.025     = c(quantile(visible_prob, .025), quantile(hidden_prob, .025)),
           pi.250     = c(quantile(visible_prob, .250), quantile(hidden_prob, .250)),
           pi.750     = c(quantile(visible_prob, .750), quantile(hidden_prob, .750)),
           pi.975     = c(quantile(visible_prob, .975), quantile(hidden_prob, .975))) %>%
    ggplot(aes(x = condition, y = median, group = 1)) +
    geom_line() + 
    geom_point() + 
    geom_ribbon(aes(ymin = pi.025, ymax = pi.975), alpha = 0.2) +
    # geom_ribbon(aes(ymin = pi.250, ymax = pi.750), alpha = 0.2) +
    geom_text(aes(x = Inf, y = Inf, hjust = 1, vjust = 1.5, label = "BF = 3.03"), size = 3.5) +
    ylim(0,1) + 
    labs(y = 'Median predicted probability of\nnot fulfilling requests when able',
         x = 'Condition') +
    theme_classic() +
    theme(panel.grid.major.x = element_blank(),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 10))
  # prepare for gE and gF
  d.plot <-
    readd(d1.00) %>%
    select(ID, Group, Condition, ends_with('.player.herd_size_after_shock'),
           ends_with('.player.received'),
           ends_with('.player.request_amount')) %>%
    mutate_at(vars(ends_with(".player.received"), ends_with(".player.request_amount")), 
              function(x) x[1:nrow(readd(d1.00)) + c(1,-1)]) %>%
    gather(key, value, -ID, -Group, -Condition) %>%
    extract(key, c("round_number", "variable"), 
            "SurvivalGame.(.|..).player.(herd_size_after_shock|received|request_amount)") %>%
    spread(variable, value) %>%
    rename(gave              = received,
           partner_requested = request_amount) %>%
    mutate(round_number      = as.integer(round_number)) %>%
    arrange(round_number, ID, Group) %>%
    mutate(requested = ifelse(is.na(partner_requested), 0, 1),
           ableToFulfill = ifelse(herd_size_after_shock - partner_requested >= 64, 1, 0),
           noFulfillWhenAble = ifelse(herd_size_after_shock - partner_requested >= 64,
                                      ifelse(gave < partner_requested, 1, 0), 0)) %>%
    group_by(Condition, round_number) %>%
    summarise(alive          = sum(!is.na(herd_size_after_shock)),
              requested      = sum(requested, na.rm = T),
              ableToFulfill  = sum(ableToFulfill, na.rm = T),
              noFulfillWhenAble = sum(noFulfillWhenAble, na.rm = T))
  gE <-
    d.plot %>%
    filter(Condition == 0) %>%
    ggplot(aes(x = round_number)) +
    geom_line(aes(y = alive), colour = "steelblue") + 
    geom_point(aes(y = alive), colour = "steelblue") +
    geom_bar(aes(y = ableToFulfill), stat = "identity", 
             fill = "steelblue", alpha = 0.075, width = 0.8) +
    geom_bar(aes(y = noFulfillWhenAble), stat = "identity", 
             fill = "steelblue", width = 0.8) +
    scale_x_continuous('Round number', c(5, 10, 15, 20, 25)) +
    scale_y_continuous(breaks = c(seq(0, 40, 10)), limits = c(0, 42)) +
    ylab('Number of players\nnot fulfilling requests when able') +
    theme_classic() +
    theme(panel.grid.major.x = element_blank(),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 10))
  gF <-
    d.plot %>%
    filter(Condition == 1) %>%
    ggplot(aes(x = round_number)) +
    geom_line(aes(y = alive), colour = "firebrick2") + 
    geom_point(aes(y = alive), colour = "firebrick2") +
    geom_bar(aes(y = ableToFulfill), stat = "identity", 
             fill = "firebrick2", alpha = 0.075, width = 0.8) +
    geom_bar(aes(y = noFulfillWhenAble), stat = "identity", 
             fill = "firebrick2", width = 0.8) +
    scale_x_continuous('Round number', c(5, 10, 15, 20, 25)) +
    scale_y_continuous(breaks = c(seq(0, 40, 10)), limits = c(0, 42), position = "right") +
    ylab('Number of players alive') +
    theme_classic() +
    theme(panel.grid.major.x = element_blank(),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 8),
          axis.title.y.right = element_text(margin = margin(0, 0, 0, 6)))
  # create manual legend
  legend <- get_legend(
    tibble(Condition = factor(c("Visible","Hidden"),
                              levels = c(c("Visible","Hidden"))),
           Value = c(1, 1)) %>%
      ggplot(aes(y = Condition, x = Value, fill = Condition)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("steelblue", "firebrick2")) +
      theme(legend.box.margin = margin(0, 0, 25, 0),
            legend.title = element_blank(),
            legend.text = element_text(size = 10))
  )
  # finished plot!
  top <- plot_grid(gA, NULL, gB, gC, nrow = 1, 
                   labels = c("a","","b",""), 
                   rel_widths = c(0.7, 0.1, 1, 1))
  bottom <- plot_grid(gD, NULL, gE, gF, nrow = 1, 
                      labels = c("c","","d",""), 
                      rel_widths = c(0.7, 0.1, 1, 1))
  g <- plot_grid(top, NULL, bottom, ncol = 1, rel_heights = c(1, 0.075, 1))
  g <- plot_grid(g, legend, nrow = 1, rel_widths = c(1, 0.1))
  # save to working directory
  ggsave('figures/fig1.pdf', width = 9.5, height = 6)
  # return
  return(g)
}