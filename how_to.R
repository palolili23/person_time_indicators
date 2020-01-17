df <- tibble(
  id = c(1:5),
  age_baseline = c(55,60,65,69, 58),
  t2event = c(5,8,3,2,10),
  outcome = c(0,1,2,3,2))

# Base R
# A <- df[rep(seq(nrow(df)), df$t2event),]

### Calculate the age 55 to t2event
B <- df %>% 
  mutate(age_dif = age_baseline - 55,
         t2event_plus_age = t2event + age_dif)

### Create person-years rows based on t2event_plus_age
B2 <- B %>% 
  group_by(id) %>% 
  slice(rep(1:n(), each = max(B$t2event_plus_age))) %>% 
  mutate(
    row_scale = row_number(),
    time_scale = row_number() + 54) %>% 
  filter(row_scale <= t2event_plus_age) %>% 
  ungroup()

### Create outcome indicators

C <- B2 %>% 
  group_by(id) %>% 
  mutate(
    outcome_plr = ifelse(row_scale == t2event_plus_age, outcome, 0),
    dementia_plr = ifelse(outcome_plr == 1, 1, 0),
    death_plr = ifelse(outcome_plr == 2, 1, 0),
    l2fu_plr = ifelse(outcome_plr == 3, 1, 0),
  ) %>% 
  ungroup()

