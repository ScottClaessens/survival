# data wrangling functions

# experiment 1

wrangleData1.01 <- function(d1.00) d1.00 %>% select(Condition, rounds_survived, overall_num_shocks)

wrangleData1.02 <- function(d1.00) d1.00 %>% select(Condition, total_cattle_lost)

wrangleData1.03 <- function(d1.00) {
  d1.00 %>%
    select(ID, Group, Condition, ends_with('.player.request')) %>%
    gather(round_number, request, ends_with('.player.request')) %>%
    mutate(round_number = rep(1:25, each = 82)) %>%
    drop_na()
}

wrangleData1.04 <- function(d1.00) {
  d1.00 %>%
    select(ID, Group, Condition, ends_with('.player.herd_size_after_shock'), 
           ends_with('.player.request')) %>%
    gather(key, value, -ID, -Group, -Condition) %>%
    extract(key, c("round_number", "variable"), 
            "SurvivalGame.(.|..).player.(herd_size_after_shock|request)") %>%
    spread(variable, value) %>%
    mutate(round_number = as.integer(round_number)) %>%
    arrange(round_number, ID, Group) %>%
    drop_na() %>%                            # drop NAs, as player is no longer alive
    filter(herd_size_after_shock >= 64)      # only rounds above the minimum threshold
}

wrangleData1.05 <- function(d1.00) {
  d1.00 %>%
    select(ID, Group, Condition, ends_with('.player.received')) %>%
    # swap every other value to get what the player GAVE, rather than what they RECEIVED
    # swapping works because each player is next to their partner in the data frame
    mutate_at(vars(ends_with(".player.received")), function(x) x[1:nrow(d1.00) + c(1,-1)]) %>%
    gather(round_number, responded, ends_with('.player.received')) %>%
    mutate(round_number = rep(1:25, each = 82)) %>%
    # drop NAs, as no requesting happened
    drop_na() %>%
    # 0 = responded, 1 = did not respond
    mutate(notResponded = ifelse(responded > 0, 0, 1) %>% as.integer())
}

wrangleData1.06 <- function(d1.00) {
  d1.00 %>%
    # 1. select herd size, received, and requested
    select(ID, Group, Condition, ends_with('.player.herd_size_after_shock'),
           ends_with('.player.received'),
           ends_with('.player.request_amount')) %>%
    # 2a. flip the variables to get what the player gave and what their partner requested
    mutate_at(vars(ends_with(".player.received"), ends_with(".player.request_amount")), 
              function(x) x[1:nrow(d1.00) + c(1,-1)]) %>%
    gather(key, value, -ID, -Group, -Condition) %>%
    extract(key, c("round_number", "variable"), 
            "SurvivalGame.(.|..).player.(herd_size_after_shock|received|request_amount)") %>%
    spread(variable, value) %>%
    # 2b. rename the variables to match their new meaning
    rename(gave              = received,
           partner_requested = request_amount) %>%
    mutate(round_number      = as.integer(round_number)) %>%
    arrange(round_number, ID, Group) %>%
    # 3. drop NAs, as no requesting happened
    drop_na() %>%
    # 4. was the player able to give?
    filter(herd_size_after_shock - partner_requested >= 64) %>%
    # 5. code 0 = request fulfilled, 1 = request not fulfilled
    mutate(notFulfilled = ifelse(gave >= partner_requested, 0, 1) %>% as.integer())
}

wrangleData1.07 <- function(d1.00) {
  d1.00 %>% 
    select(ID, Group, Condition, rounds_survived) %>%
    # need to add column for censoring (people who survived all rounds may have died later)
    mutate(censored = ifelse(rounds_survived == 25, 1, 0))
}

wrangleData1.08 <- function(d1.00) {
  d_rule1 <-
    d1.00 %>%
    select(ID, Group, Condition, ends_with('.player.herd_size_after_shock'), 
           ends_with('.player.request')) %>%
    gather(key, value, -ID, -Group, -Condition) %>%
    extract(key, c("round_number", "variable"), 
            "SurvivalGame.(.|..).player.(herd_size_after_shock|request)") %>%
    spread(variable, value) %>%
    mutate(round_number = as.integer(round_number)) %>%
    arrange(round_number, ID, Group) %>%
    drop_na() %>%                            # drop NAs, as player is no longer alive
    filter(herd_size_after_shock >= 64) %>%  # only rounds above the minimum threshold
    # ADDED LINES: summarise
    group_by(ID, Group, Condition) %>%
    summarise(prop_rule1 = sum(request) / n())
  out <-
    d1.00 %>%
    select(ID, Group, Condition, rounds_survived) %>%
    # need to add column for censoring (people who survived all rounds may have died later)
    mutate(censored = ifelse(rounds_survived == 25, 1, 0)) %>%
    # add cheating variable
    right_join(d_rule1, by = c("ID", "Group", "Condition"))
  return(out)
}

wrangleData1.09 <- function(d1.00) {
  d_rule2 <-
    d1.00 %>%
    # 1. select herd size, received, and requested
    select(ID, Group, Condition, ends_with('.player.herd_size_after_shock'),
           ends_with('.player.received'),
           ends_with('.player.request_amount')) %>%
    # 2a. flip the variables to get what the player gave and what their partner requested
    mutate_at(vars(ends_with(".player.received"), ends_with(".player.request_amount")), 
              function(x) x[1:nrow(d1.00) + c(1,-1)]) %>%
    gather(key, value, -ID, -Group, -Condition) %>%
    extract(key, c("round_number", "variable"), 
            "SurvivalGame.(.|..).player.(herd_size_after_shock|received|request_amount)") %>%
    spread(variable, value) %>%
    # 2b. rename the variables to match their new meaning
    rename(gave              = received,
           partner_requested = request_amount) %>%
    mutate(round_number      = as.integer(round_number)) %>%
    arrange(round_number, ID, Group) %>%
    # 3. drop NAs, as no requesting happened
    drop_na() %>%
    # 4. was the player able to give?
    filter(herd_size_after_shock - partner_requested >= 64) %>%
    # 5. code 0 = request fulfilled, 1 = request not fulfilled
    mutate(notFulfilled = ifelse(gave >= partner_requested, 0, 1) %>% as.integer()) %>%
    # ADDED LINES: summarise
    group_by(ID, Group, Condition) %>%
    summarise(prop_rule2 = sum(notFulfilled) / n())
  out <-
    d1.00 %>%
    select(ID, Group, Condition, rounds_survived) %>%
    # need to add column for censoring (people who survived all rounds may have died later)
    mutate(censored = ifelse(rounds_survived == 25, 1, 0)) %>%
    # add cheating variable
    right_join(d_rule2, by = c("ID", "Group", "Condition"))
  return(out)
}

wrangleData1.10 <- function(d1.00) {
  d1.00 %>%
    select(ID, Group, Condition, ends_with('.player.herd_size_after_shock'), 
           ends_with('.player.request')) %>%
    gather(key, value, -ID, -Group, -Condition) %>%
    extract(key, c("round_number", "variable"), 
            "SurvivalGame.(.|..).player.(herd_size_after_shock|request)") %>%
    spread(variable, value) %>%
    mutate(round_number = as.integer(round_number)) %>%
    arrange(round_number, ID, Group) %>%
    drop_na() # drop NAs, as player is no longer alive
}

wrangleData1.11 <- function(d1.00) {
  d1.00 %>%
    select(ID, Group, Condition, ends_with('.player.request_amount'),
           ends_with('.player.herd_size_after_shock')) %>%
    gather(key, value, -ID, -Group, -Condition) %>%
    extract(key, c("round_number", "variable"), 
            "SurvivalGame.(.|..).player.(herd_size_after_shock|request_amount)") %>%
    spread(variable, value) %>%
    mutate(round_number = as.integer(round_number),
           request_amount.log = log(request_amount)) %>%
    arrange(round_number, ID, Group) %>% 
    drop_na() # drop NAs, player is not alive / did not request
}

wrangleData1.12 <- function(d1.00) {
  d1.00 %>%
    select(ID, Group, Condition, ends_with('.player.request_amount'),
           ends_with('.player.herd_size_after_shock')) %>%
    gather(key, value, -ID, -Group, -Condition) %>%
    extract(key, c("round_number", "variable"), 
            "SurvivalGame.(.|..).player.(herd_size_after_shock|request_amount)") %>%
    spread(variable, value) %>%
    filter(herd_size_after_shock < 64) %>%
    mutate(round_number = as.numeric(round_number),
           # get difference between amount requested and amount needed to reach threshold (64)
           diff = request_amount - (64 - herd_size_after_shock)) %>%
    arrange(round_number, ID, Group) %>%
    drop_na() # drop NAs player is not alive / did not request
}

wrangleData1.13 <- function(d1.00) {
  d1.00 %>%
    # 1. select received and requested
    select(ID, Group, Condition,
           ends_with('.player.received'),
           ends_with('.player.request_amount')) %>%
    # 2a. flip the variables to get what the player gave and what their partner requested
    mutate_at(vars(ends_with(".player.received"), ends_with(".player.request_amount")), 
              function(x) x[1:nrow(d1.00) + c(1,-1)]) %>%
    gather(key, value, -ID, -Group, -Condition) %>%
    extract(key, c("round_number", "variable"), 
            "SurvivalGame.(.|..).player.(received|request_amount)") %>%
    spread(variable, value) %>%
    # 2b. rename the variables to match their new meaning
    rename(gave              = received,
           partner_requested = request_amount) %>%
    mutate(round_number      = as.integer(round_number),
           diff              = gave - partner_requested) %>%
    arrange(round_number, ID, Group) %>%
    # 3. drop NA - no request from partner
    drop_na()
}

# experiment 2

wrangleData2.01 <- function(d2.00) {
  d2.00 %>%
    select(ID, ends_with('.rounds_survived'), ends_with('.overall_num_shocks')) %>%
    gather(key, value, -ID) %>%
    extract(key, c("Condition", "variable"), 
            "(hidden|visible).(rounds_survived|overall_num_shocks)") %>%
    spread(variable, value) %>%
    mutate(Condition = ifelse(Condition == 'hidden', 1, 0))
}

wrangleData2.02 <- function(d2.00) {
  d2.00 %>%
    select(hidden.total_cattle_lost, visible.total_cattle_lost) %>%
    gather(Condition, total_cattle_lost) %>%
    mutate(Condition = ifelse(Condition == 'hidden.total_cattle_lost', 1, 0))
}

wrangleData2.03 <- function(d2.00) {
  d2.00 %>%
    select(ID, Group, ends_with('.player.request')) %>%
    gather(key, request, -ID, -Group) %>%
    extract(key, c("Condition", "round_number"), 
            "SurvivalGame_(Hidden|Visible)[.](.|..)[.]player[.]request") %>%
    mutate(Condition    = ifelse(Condition == 'Hidden', 1, 0),
           round_number = as.integer(round_number)) %>%
    drop_na()
}

wrangleData2.04 <- function(d2.00) {
  d2.00 %>%
    select(ID, Group, Counterbalancing, ends_with('.player.herd_size_after_shock'), 
           ends_with('.player.request')) %>%
    gather(key, value, -ID, -Group, -Counterbalancing) %>%
    extract(key, c("Condition", "round_number", "variable"), 
            "SurvivalGame_(Hidden|Visible)[.](.|..)[.]player[.](request|herd_size_after_shock)") %>%
    spread(variable, value) %>%
    mutate(Condition    = ifelse(Condition == 'Hidden', 1, 0),
           round_number = as.integer(round_number)) %>%
    arrange(round_number, ID, Group) %>%
    drop_na() %>%
    filter(herd_size_after_shock >= 64)
}

wrangleData2.05 <- function(d2.00) {
  d2.00 %>%
    select(ID, Group, ends_with('.player.received')) %>%
    # swap every other value to get what the player GAVE, rather than what they RECEIVED
    # swapping works because each player is next to their partner in the data frame
    mutate_at(vars(ends_with(".player.received")), function(x) x[1:nrow(d2.00) + c(1,-1)]) %>%
    gather(key, received, -ID, -Group) %>%
    extract(key, c("Condition", "round_number"), 
            "SurvivalGame_(Hidden|Visible)[.](.|..)[.]player[.]received") %>%
    rename(responded    = received) %>%
    mutate(Condition    = ifelse(Condition == 'Hidden', 1, 0),
           round_number = as.integer(round_number),
           notResponded = ifelse(responded > 0, 0, 1) %>% as.integer()) %>%
    arrange(round_number, ID, Group) %>%
    drop_na()
}

wrangleData2.06 <- function(d2.00) {
  d2.00 %>%
    # 1. select herd size, received, and requested
    select(ID, Group, Counterbalancing, ends_with('.player.herd_size_after_shock'),
           ends_with('.player.received'),
           ends_with('.player.request_amount')) %>%
    # 2a. flip the variables to get what the player gave and what their partner requested
    mutate_at(vars(ends_with(".player.received"), ends_with(".player.request_amount")), 
              function(x) x[1:nrow(d2.00) + c(1,-1)]) %>%
    gather(key, value, -ID, -Group, -Counterbalancing) %>%
    extract(key, c("Condition", "round_number", "variable"), 
            "SurvivalGame_(Hidden|Visible)[.](.|..)[.]player[.](herd_size_after_shock|received|request_amount)") %>%
    spread(variable, value) %>%
    # 2b. rename the variables to match their new meaning
    rename(gave              = received,
           partner_requested = request_amount) %>%
    mutate(Condition    = ifelse(Condition == 'Hidden', 1, 0),
           round_number = as.integer(round_number)) %>%
    arrange(round_number, ID, Group) %>%
    # 3. drop NAs, as no requesting happened
    drop_na() %>%
    # 4. was the player able to give?
    filter(herd_size_after_shock - partner_requested >= 64) %>%
    # 5. code 0 = request fulfilled, 1 = request not fulfilled
    mutate(notFulfilled = ifelse(gave >= partner_requested, 0, 1) %>% as.integer())
}

wrangleData2.07 <- function(d2.00) {
  d2.00 %>% 
    select(ID, Group, Counterbalancing, hidden.rounds_survived, visible.rounds_survived) %>%
    # long format
    gather(Condition, rounds_survived, -ID, -Group, -Counterbalancing) %>%
    mutate(Condition = ifelse(Condition == "hidden.rounds_survived", 1, 0)) %>%
    # need to add column for censoring (people who survived all rounds may have died later)
    mutate(censored = ifelse(rounds_survived == 25, 1, 0))
}

wrangleData2.08 <- function(d2.00) {
  d_rule1 <-
    d2.00 %>%
    select(ID, Group, Counterbalancing, ends_with('.player.herd_size_after_shock'), 
           ends_with('.player.request')) %>%
    gather(key, value, -ID, -Group, -Counterbalancing) %>%
    extract(key, c("Condition", "round_number", "variable"), 
            "SurvivalGame_(Hidden|Visible)[.](.|..)[.]player[.](request|herd_size_after_shock)") %>%
    spread(variable, value) %>%
    mutate(Condition    = ifelse(Condition == 'Hidden', 1, 0),
           round_number = as.integer(round_number)) %>%
    arrange(round_number, ID, Group) %>%
    drop_na() %>%
    filter(herd_size_after_shock >= 64) %>%
    # ADDED LINES: summarise
    group_by(ID, Group, Condition) %>%
    summarise(prop_rule1 = sum(request) / n())
  out <-
    d2.00 %>%
    select(ID, Group, Counterbalancing, hidden.rounds_survived, visible.rounds_survived) %>%
    gather(Condition, rounds_survived, -ID, -Group, -Counterbalancing) %>%
    # need to add column for censoring (people who survived all rounds may have died later)
    mutate(censored = ifelse(rounds_survived == 25, 1, 0)) %>%
    # fix Condition column
    mutate(Condition = ifelse(Condition == "hidden.rounds_survived", 1, 0)) %>%
    # add cheating variable
    right_join(d_rule1, by = c("ID", "Group", "Condition"))
  return(out)
}

wrangleData2.09 <- function(d2.00) {
  d_rule2 <-
    d2.00 %>%
    select(ID, Group, Counterbalancing, ends_with('.player.herd_size_after_shock'),
           ends_with('.player.received'),
           ends_with('.player.request_amount')) %>%
    mutate_at(vars(ends_with(".player.received"), ends_with(".player.request_amount")), 
              function(x) x[1:nrow(d2.00) + c(1,-1)]) %>%
    gather(key, value, -ID, -Group, -Counterbalancing) %>%
    extract(key, c("Condition", "round_number", "variable"), 
            "SurvivalGame_(Hidden|Visible)[.](.|..)[.]player[.](herd_size_after_shock|received|request_amount)") %>%
    spread(variable, value) %>%
    rename(gave              = received,
           partner_requested = request_amount) %>%
    mutate(Condition    = ifelse(Condition == 'Hidden', 1, 0),
           round_number = as.integer(round_number)) %>%
    arrange(round_number, ID, Group) %>%
    drop_na() %>%
    filter(herd_size_after_shock - partner_requested >= 64) %>%
    mutate(notFulfilled = ifelse(gave >= partner_requested, 0, 1) %>% as.integer()) %>%
    # ADDED LINES: summarise
    group_by(ID, Group, Condition) %>%
    summarise(prop_rule2 = sum(notFulfilled) / n())
  out <-
    d2.00 %>%
    select(ID, Group, Counterbalancing, hidden.rounds_survived, visible.rounds_survived) %>%
    gather(Condition, rounds_survived, -ID, -Group, -Counterbalancing) %>%
    # need to add column for censoring (people who survived all rounds may have died later)
    mutate(censored = ifelse(rounds_survived == 25, 1, 0)) %>%
    # fix Condition column
    mutate(Condition = ifelse(Condition == "hidden.rounds_survived", 1, 0)) %>%
    # add cheating variable
    right_join(d_rule2, by = c("ID", "Group", "Condition"))
  return(out)
}

# experiment 3

wrangleData3.01 <- function(d3.00) {
  d3.00 %>%
    select(Condition, rounds_survived, overall_num_shocks)
}

wrangleData3.02 <- function(d3.00) {
  d3.00 %>%
    select(Condition, total_cattle_lost)
}

wrangleData3.03 <- function(d3.00) {
  d3.00 %>%
    select(ID, Group, IDinGroup, Condition, ends_with('.player.request')) %>%
    gather(round_number, request, ends_with('.player.request')) %>%
    mutate(round_number = rep(1:25, each = 84)) %>%
    drop_na() %>%
    filter(IDinGroup == 2)
}

wrangleData3.04 <- function(d3.00) {
  d3.00 %>%
    select(ID, IDinGroup, Condition, ends_with('.player.herd_size_after_shock'), 
           ends_with('.player.request')) %>%
    gather(key, value, -ID, -IDinGroup, -Condition) %>%
    extract(key, c("round_number", "variable"), 
            "SurvivalGame.(.|..).player.(herd_size_after_shock|request)") %>%
    spread(variable, value) %>%
    mutate(round_number = as.integer(round_number)) %>%
    arrange(round_number, ID) %>%
    drop_na() %>%                            # drop NAs, as player is no longer alive
    filter(herd_size_after_shock >= 64) %>%  # only rounds above the minimum threshold
    filter(IDinGroup == 2)
}

wrangleData3.05 <- function(d3.00) {
  d3.00 %>%
    select(ID, IDinGroup, Condition, ends_with('.player.received')) %>%
    # swap every other value to get what the player GAVE, rather than what they RECEIVED
    # swapping works because each player is next to their partner in the data frame
    mutate_at(vars(ends_with(".player.received")), function(x) x[1:nrow(d3.00) + c(1,-1)]) %>%
    gather(round_number, responded, ends_with('.player.received')) %>%
    mutate(round_number = rep(1:25, each = 84)) %>%
    # drop NAs, as no requesting happened
    drop_na() %>%
    # 0 = responded, 1 = did not respond
    mutate(notResponded = ifelse(responded > 0, 0, 1) %>% as.integer()) %>%
    filter(IDinGroup == 2)
}

wrangleData3.06 <- function(d3.00) {
  d3.00 %>%
    # 1. select herd size, received, and requested
    select(ID, IDinGroup, Condition, ends_with('.player.herd_size_after_shock'),
           ends_with('.player.received'),
           ends_with('.player.request_amount')) %>%
    # 2a. flip the variables to get what the player gave and what their partner requested
    mutate_at(vars(ends_with(".player.received"), ends_with(".player.request_amount")), 
              function(x) x[1:nrow(d3.00) + c(1,-1)]) %>%
    gather(key, value, -ID, -IDinGroup, -Condition) %>%
    extract(key, c("round_number", "variable"), 
            "SurvivalGame.(.|..).player.(herd_size_after_shock|received|request_amount)") %>%
    mutate(value = as.numeric(value)) %>%
    spread(variable, value) %>%
    # 2b. rename the variables to match their new meaning
    rename(gave              = received,
           partner_requested = request_amount) %>%
    mutate(round_number      = as.integer(round_number)) %>%
    arrange(round_number, ID, IDinGroup, Condition) %>%
    # 3. drop NAs, as no requesting happened
    drop_na() %>%
    # 4. was the player able to give?
    filter(herd_size_after_shock - partner_requested >= 64) %>%
    # 5. code 0 = request fulfilled, 1 = request not fulfilled
    mutate(notFulfilled = ifelse(gave >= partner_requested, 0, 1) %>% as.integer()) %>%
    # 6. keep only Player 2s
    filter(IDinGroup == 2)
}

