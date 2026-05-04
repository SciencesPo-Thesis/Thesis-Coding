#########################################################################
#########################################################################
############# Replication Code: "Coming of Age Under Trump" #############
######################### ANES RESTRICTED DATA ##########################
#########################################################################
#########################################################################


# Author: Nikolaos Vichos #


# Necessary packages #
library(styler)
library(psych) # factor analysis
library(tidyverse)
library(scales) # axis formattins
library(rdrobust) # RDD estimation
library(modelsummary) # regression tables
library(stargazer)
library(tibble) # tibble()
library(haven) # different file types
library(here) # used for managing location etc
library(kableExtra) # for formatting tables
library(gt) # for formatting tables
library(fastDummies) # for creating dummies
library(patchwork) # for combining ggplots with |


#------------------------------------------------------------------------------#


# File locations

## For running on local machine
loc <- "/Users/nikolaosvichos/Library/Mobile Documents/com~apple~CloudDocs/Sciences Po/Thesis/"

## For running on restricted virtual machine
# loc <- here()

## For accessing functions
source(paste0(loc, "Thesis-Github/Thesis-Coding/Functions/functions_restricted.R"))


#------------------------------------------------------------------------------#


# Data import and cleaning #


## Import file
df_unclean <- haven::read_dta(paste0(loc, "/Datasets/ANES/anes_data.dta"))


## Variable selection and cleaning
df_unstandardized <- df_unclean %>%
  dplyr::select(
    # survey type
    panel_data = V200003,
    pre_or_post = V200004,
    # demographics
    age = V201507x,
    party = V201231x,
    education_summary = V201511x,
    education = V201510,
    sex = V201600,
    income = V202468x,
    race = V201549x,
    lib_or_con = V201200,
    # election-related stuff (some stuff referring to 2020 is commented out for now)
    duty_or_choice = V201225x,
    voted2012 = V201104,
    voted2016 = V201101,
    # voted2020               = V202109x,
    voted_for_2012 = V201105,
    voted_for_2016 = V201103,
    # voted_for_2020          = V201029
    # vote_for_intention_2020 = V201033
    # preference_for_2020     = V201036
    expectations_2020 = V201217,
    # feeling thermometers
    feeling_dems = V201156,
    feeling_reps = V201157,
    # main index items
    free_press = V201366,
    checks_and_balances = V201367,
    rule_of_law = V201368,
    agree_on_facts = V201369,
    unitary_executive = V201372x,
    journalist_access = V201375x,
    media_undermined_concern = V201376,
    # other relevant variables, not used in index but kept
    media_trust = V201377,
    govt_trust = V201233,
    govt_capture = V201234,
    govt_corruption = V201236,
    foreign_help = V201378,
    govt_principled = V201379,
    urban_unrest = V201429,
  ) %>%
  filter(panel_data %in% c(3, 4, 5, 6)) %>%
  filter(pre_or_post %in% c(1, 3)) %>%
  mutate(
    # cleaning demographic variblaes
    age = case_when(
      age == -9 ~ NA_integer_,
      TRUE ~ age
    ),
    party = case_when(
      party == 3 ~ NA_integer_,
      TRUE ~ party
    ),
    party = case_when(
      party %in% c(-9, -8) ~ NA_integer_,
      TRUE ~ party
    ),
    party_summary = factor(
      case_when(
        party %in% c(1, 2) ~ 1,
        party %in% c(3, 4, 5) ~ 2,
        party %in% c(6, 7) ~ 3
      ),
      levels = c(1, 2, 3),
      labels = c(
        "Democrat",
        "Independent",
        "Republican"
      )
    ),
    education_summary = case_when(
      education_summary %in% c(-9, -8, -2) ~ NA_integer_,
      TRUE ~ education_summary
    ),
    education = case_when(
      education %in% c(-9, -8, 95) ~ NA_integer_,
      TRUE ~ education
    ),
    sex = case_when(
      sex == -9 ~ NA_integer_,
      sex == 2 ~ 0,
      TRUE ~ sex
    ),
    income = case_when(
      income %in% c(-9, -5) ~ NA_integer_,
      TRUE ~ income
    ),
    race = relevel(
      factor(
        case_when(
          race %in% c(-9, -8) ~ NA_integer_,
          TRUE ~ race
        ),
        levels = c(1, 2, 3, 4, 5, 6),
        labels = c("White", "Black", "Hispanic", "Asian", "Native", "Other")
      ),
      ref = "White"
    ),
    # cleaning vote-related variables (some stuff is commented out for now)
    duty_or_choice = case_when(
      duty_or_choice == -2 ~ NA_integer_,
      TRUE ~ duty_or_choice
    ),
    voted2012 = case_when(
      voted2012 %in% c(-9, -8) ~ NA_integer_,
      voted2012 == 2 ~ 0,
      TRUE ~ voted2012
    ),
    voted2016 = case_when(
      voted2016 %in% c(-9, -8, -1) ~ NA_integer_,
      voted2016 == 2 ~ 0,
      TRUE ~ voted2016
    ),
    # voted2020 = case_when(
    #   voted2020 == -2 ~ NA_integer_,
    #   TRUE             ~ voted2020
    # ),
    voted_for_2012 = case_when(
      voted_for_2012 %in% c(-9, -8, -1, 5) ~ NA_integer_,
      TRUE ~ voted_for_2012
    ),
    voted_for_2016 = case_when(
      voted_for_2016 %in% c(-9, -8, -1, 5) ~ NA_integer_,
      TRUE ~ voted_for_2016
    ),
    # voted_for_2020 = case_when(
    #   voted_for_2020 %in% c(-9, -8, -7, -6, -1, 5, 11, 12) ~ NA_integer_,
    #   TRUE ~ voted_for_2020
    # ),
    expectations_2020 = case_when(
      expectations_2020 %in% c(-9, -8, 5) ~ NA_integer_,
      expectations_2020 == 2 ~ 3, # recode to match the fact that republicans were previously coded as 3
      TRUE ~ expectations_2020
    ),
    expect_inparty_win = case_when(
      is.na(expectations_2020) | is.na(party_summary) ~ NA_integer_,
      party_summary == "Independent" ~ NA_integer_,
      expectations_2020 == 1 & party_summary == "Democrat" ~ 1L, # Biden to win, respondent is D
      expectations_2020 == 3 & party_summary == "Republican" ~ 1L, # Trump to win, respondent is R
      expectations_2020 == 1 & party_summary == "Republican" ~ 0L, # Biden to win, respondent is R
      expectations_2020 == 3 & party_summary == "Democrat" ~ 0L, # Trump to win, respondent is D
      TRUE ~ NA_integer_
    ),
    feeling_dems = case_when(
      feeling_dems %in% c(-9, 998) ~ NA_integer_,
      TRUE ~ feeling_dems
    ),
    feeling_reps = case_when(
      feeling_reps %in% c(-9, 998) ~ NA_integer_,
      TRUE ~ feeling_reps
    ),
    # cleaning main index items (note to reader and to self: higher values = higher support for liberalism)
    free_press = case_when( # V201366
      free_press %in% c(-9, -8) ~ NA_integer_,
      TRUE ~ free_press
    ),
    checks_and_balances = case_when( # V201367
      checks_and_balances %in% c(-9, -8) ~ NA_integer_,
      TRUE ~ checks_and_balances
    ),
    rule_of_law = case_when( # V201368
      rule_of_law %in% c(-9, -8) ~ NA_integer_,
      TRUE ~ rule_of_law
    ),
    agree_on_facts = case_when( # V201369
      agree_on_facts %in% c(-9, -8) ~ NA_integer_,
      TRUE ~ agree_on_facts
    ),
    unitary_executive = case_when( # V201372x
      unitary_executive == -2 ~ NA_integer_,
      TRUE ~ unitary_executive
    ),
    journalist_access = case_when( # V201375x
      journalist_access == -2 ~ NA_integer_,
      TRUE ~ journalist_access
    ),
    media_undermined_concern = case_when( # V201376
      media_undermined_concern %in% c(-9, -8) ~ NA_integer_,
      TRUE ~ media_undermined_concern
    ),
    # cleaning other items
    media_trust = case_when(
      media_trust %in% c(-9, -8) ~ NA_integer_,
      TRUE ~ media_trust
    ),
    govt_trust = case_when(
      govt_trust %in% c(-9, -8) ~ NA_integer_,
      TRUE ~ 6 - govt_trust
    ),
    govt_capture = case_when(
      govt_capture %in% c(-9, -8) ~ NA_integer_,
      TRUE ~ govt_capture
    ),
    govt_corruption = case_when(
      govt_corruption %in% c(-9, -8) ~ NA_integer_,
      TRUE ~ govt_corruption
    ),
    urban_unrest = case_when(
      urban_unrest %in% c(-9, -8, 99) ~ NA_integer_,
      TRUE ~ 8 - urban_unrest
    ),
    foreign_help = case_when(
      foreign_help %in% c(-9, -8) ~ NA_integer_,
      TRUE ~ foreign_help
    ),
    govt_principled = case_when(
      govt_principled %in% c(-9, -8) ~ NA_integer_,
      TRUE ~ govt_principled
    )
  )


## Create treatment variable based on date of birth
df_unstandardized <- df_unstandardized %>%
  # this small part is for practice purposes to work with YYYY-MM-DD variables (delete after accessing restricted data)
  mutate(
    year_of_birth = 2020 - age,
    month_of_birth = sample(1:12, n(), replace = TRUE),
    day_of_birth = sample(1:28, n(), replace = TRUE)
  ) %>%
  # create a date of birth variable, a cut-off and a treatment variable
  mutate(
    date_of_birth = as.Date(paste(year_of_birth, month_of_birth, day_of_birth, sep = "-")),
    date_cutoff = as.Date("1994-11-06"),
    date_2012_election = as.Date("2012-11-06"),
    days_from_cutoff = as.numeric(date_of_birth - date_cutoff),
    age_2012_election = as.numeric(interval(date_of_birth, date_2012_election), "years"),
    treatment = ifelse(date_of_birth > date_cutoff, 1, 0)
  )


#------------------------------------------------------------------------------#


# Create index

## Items used
liberal_items <- c(
  "free_press",
  "checks_and_balances",
  "rule_of_law",
  "agree_on_facts",
  "unitary_executive",
  "journalist_access",
  "media_undermined_concern"
)

## Standardize items
df <- df_unstandardized %>%
  mutate(
    across(all_of(liberal_items), ~ as.numeric(scale(.x)))
  )


## Check internal consistency of items

### First check
alpha_result <- psych::alpha(df_unstandardized[, liberal_items])
alpha_result

### Then save
alpha_summary <- data.frame(
  metric = c("Cronbach's Alpha (raw)", "Cronbach's Alpha (std)", "n_items"),
  value = c(
    round(alpha_result$total$raw_alpha, 3),
    round(alpha_result$total$std.alpha, 3),
    length(liberal_items)
  )
)
write.csv(alpha_summary, paste0(loc, "Thesis-Github/Thesis-Coding/Results/Appendix/Index/fa_alpha_summary.csv"), row.names = FALSE)


## Extract factors and view the values

### First check
fa_liberal <- fa(
  df[, liberal_items],
  nfactors = 1, rotate = "none", fm = "pa", max.iter = 100
)
fa_liberal$loadings

fa_diagnostics <- data.frame(
  item          = liberal_items,
  loading       = round(as.numeric(fa_liberal$loadings[, 1]), 3),
  communality   = round(fa_liberal$communality, 3),
  uniqueness    = round(fa_liberal$uniquenesses, 3),
  item_total_r  = round(alpha_result$item.stats$r.drop, 3)
)
write.csv(fa_diagnostics, paste0(loc, "Thesis-Github/Thesis-Coding/Results/Appendix/Index/fa_disagnostics.csv"), row.names = FALSE)


### Then save
fa_eigenvalues <- data.frame(
  factor = seq_along(fa_liberal$values),
  eigenvalue = round(fa_liberal$values, 3),
  variance_explained = round(fa_liberal$values / sum(fa_liberal$values), 3),
  cumulative_variance = round(cumsum(fa_liberal$values / sum(fa_liberal$values)), 3)
)
write.csv(fa_eigenvalues, paste0(loc, "Thesis-Github/Thesis-Coding/Results/Appendix/Index/fa_eigenvalues.csv"), row.names = FALSE)


## Screeplot
screeplot_liberal <- get_screeplot(fa_liberal, "Liberalism Index")
print(screeplot_liberal)

ggsave(
  paste0(loc, "Thesis-Github/Thesis-Coding/Results/Appendix/Index/screeplot_liberal.png"),
  plot = screeplot_liberal, width = 5, height = 3.5, units = "in", dpi = 300
)


## Create index (note to reader and to self: higher values = higher support for liberalism)
fa_liberal_df <- as.data.frame(fa_liberal$scores)

df <- df %>%
  mutate(
    liberal_index = scales::rescale(as.numeric(fa_liberal$scores, to = c(0, 1)))
  )

#------------------------------------------------------------------------------#

# Data analysis #

## Define partisan subsets

df_democrats <- df %>% filter(party_summary == "Democrat")
df_republicans <- df %>% filter(party_summary == "Republican")
df_partisans <- df %>% filter(party_summary %in% c("Democrat", "Republican"))
df_independents <- df %>% filter(party_summary == "Independent")
df_inparty_win <- df %>% filter(expect_inparty_win == 1)
df_inparty_loss <- df %>% filter(expect_inparty_win == 0)

## Get descriptive RD plots
discontinuityplot_parties <- get_discontinuityplot(df)
print(discontinuityplot_parties$plot_subgroups)
ggsave(
  paste0(loc, "Thesis-Github/Thesis-Coding/Results/Main/Plots/Discontinuity/discontinuityplot_full.png"),
  plot = discontinuityplot_parties$plot_all, width = 6.5, height = 5, units = "in", dpi = 300
)
ggsave(
  paste0(loc, "Thesis-Github/Thesis-Coding/Results/Main/Plots/Discontinuity/discontinuityplot_subgroups.png"),
  plot = discontinuityplot_parties$plot_subgroups, width = 6.5, height = 5.5, units = "in", dpi = 300
)

discontinuityplot_expectations <- get_discontinuityplot(df, party_id = "expect_inparty_win", colnumber = 1)
print(discontinuityplot_expectations$plot_subgroups)
ggsave(
  paste0(loc, "Thesis-Github/Thesis-Coding/Results/Main/Plots/Discontinuity/discontinuityplot_expectations.png"),
  plot = discontinuityplot_expectations$plot_subgroups, width = 6.5, height = 7.5, units = "in", dpi = 300
)

## Define control variables

### Group of control variables
potential_controls <- c(
  "education", "sex",
  "income", "race"
)

### Group of covariates that are imbalanced between the groups (all of them were imbalanced)
imbalanced_controls <- get_imbalanced_controls(df, potential_controls)

### Race is a factor variable: make a number of race dummies, with white as the baseline category
df <- fastDummies::dummy_cols(df, "race", remove_first_dummy = TRUE)

df_democrats <- fastDummies::dummy_cols(df_democrats, "race", remove_first_dummy = TRUE)

df_republicans <- fastDummies::dummy_cols(df_republicans, "race", remove_first_dummy = TRUE)

df_independents <- fastDummies::dummy_cols(df_independents, "race", remove_first_dummy = TRUE)

df_partisans <- fastDummies::dummy_cols(df_partisans, "race", remove_first_dummy = TRUE)

df_inparty_win <- fastDummies::dummy_cols(df_inparty_win, "race", remove_first_dummy = TRUE)

df_inparty_loss <- fastDummies::dummy_cols(df_inparty_loss, "race", remove_first_dummy = TRUE)


### Create final group of controls
race_dummies <- c("race_Black", "race_Hispanic", "race_Asian", "race_Native", "race_Other")
controls <- c(setdiff(imbalanced_controls, "race"), race_dummies)


## Run analyses on the samples


#### Full sample
rdd_liberal_full <- run_rdd_models(
  data         = df,
  index_var    = "liberal_index",
  controls     = controls,
  sample_label = "Full Sample"
)

####  Independents
rdd_liberal_independents <- run_rdd_models(
  data         = df_independents,
  index_var    = "liberal_index",
  controls     = controls,
  sample_label = "Independents"
)

####  Partisans
rdd_liberal_partisans <- run_rdd_models(
  data         = df_partisans,
  index_var    = "liberal_index",
  controls     = controls,
  sample_label = "Partisans"
)

####  Democrats
rdd_liberal_democrats <- run_rdd_models(
  data         = df_democrats,
  index_var    = "liberal_index",
  controls     = controls,
  sample_label = "Democrats"
)


####  Republicans
rdd_liberal_republicans <- run_rdd_models(
  data = df_republicans,
  index_var = "liberal_index",
  controls = controls,
  sample_label = "Republicans"
)

#### Expecting in party win
rdd_liberal_win <- run_rdd_models(
  data = df_inparty_win,
  index_var = "liberal_index",
  controls = controls,
  sample_label = "Expect In-Party Win"
)

#### Expecting in party loss
rdd_liberal_loss <- run_rdd_models(
  data = df_inparty_loss,
  index_var = "liberal_index",
  controls = controls,
  sample_label = "Expect In-Party Loss"
)


#### Combine results and export as csv
rdd_liberal <- bind_rows(
  rdd_liberal_full, rdd_liberal_independents, rdd_liberal_partisans,
  rdd_liberal_democrats, rdd_liberal_republicans, rdd_liberal_win, rdd_liberal_loss
) %>%
  mutate(Outcome = "Liberal Attitudes") %>%
  mutate(
    Sample = factor(
      Sample,
      levels = c("Republicans", "Democrats", "Partisans", "Independents", "Full Sample", "Expect In-Party Win", "Expect In-Party Loss")
    )
  )

write.csv(rdd_liberal, paste0(loc, paste0("Thesis-Github/Thesis-Coding/Results/Main/rdd_liberal.csv")))


#------------------------------------------------------------------------------#


# Descriptive statistics #

## Variables to describe — continuous/binary ones get mean + SD + t-test;
## race dummies are included as binary (0/1) so the mean = proportion
desc_vars <- c(
  "age_2012_election",
  "sex",
  "education",
  "income",
  "race_Black", "race_Hispanic", "race_Asian", "race_Native", "race_Other",
  "liberal_index"
)

## Function: computes mean, SD, and N by treatment group for each variable,
## plus a t-test p-value for the difference between groups
get_descriptives <- function(data, sample_label, vars = desc_vars) {
  map_dfr(vars, function(var) {
    x   <- data[[var]]
    trt <- data$treatment

    # Keep only rows where both var and treatment are non-missing
    keep    <- !is.na(x) & !is.na(trt)
    x_ctrl  <- x[keep & trt == 0]
    x_treat <- x[keep & trt == 1]

    pval <- tryCatch(
      t.test(x[keep] ~ trt[keep])$p.value,
      error = function(e) NA_real_
    )

    tibble(
      Sample             = sample_label,
      Variable           = var,
      `Mean (Control)`   = round(mean(x_ctrl),  3),
      `SD (Control)`     = round(sd(x_ctrl),    3),
      `N (Control)`      = length(x_ctrl),
      `Mean (Treatment)` = round(mean(x_treat), 3),
      `SD (Treatment)`   = round(sd(x_treat),   3),
      `N (Treatment)`    = length(x_treat),
      `P-Value`          = round(pval, 3)
    )
  })
}

## Run for all subgroups and combine
descriptives <- bind_rows(
  get_descriptives(df,               "Full Sample"),
  get_descriptives(df_independents,  "Independents"),
  get_descriptives(df_partisans,     "Partisans"),
  get_descriptives(df_democrats,     "Democrats"),
  get_descriptives(df_republicans,   "Republicans"),
  get_descriptives(df_inparty_win,   "Expect In-Party Win"),
  get_descriptives(df_inparty_loss,  "Expect In-Party Loss")
)

write.csv(
  descriptives,
  paste0(loc, "Thesis-Github/Thesis-Coding/Results/Appendix/descriptives.csv"),
  row.names = FALSE
)


#------------------------------------------------------------------------------#


# Robustness checks #

## Define subgroups for robustness checks
robustness_subgroups <- list(
  list(data = df, label = "Full Sample"),
  list(data = df_independents, label = "Independents"),
  list(data = df_partisans, label = "Partisans"),
  list(data = df_democrats, label = "Democrats"),
  list(data = df_republicans, label = "Republicans"),
  list(data = df_inparty_win, label = "Expect In-Party Win"),
  list(data = df_inparty_loss, label = "Expect In-Party Loss")
)

## Run the robustness checks and combine them into one data frame
all_robustness <- map_dfr(robustness_subgroups, function(sg) {
  message("Robustness checks: ", sg$label)
  run_robustness_checks(
    data         = sg$data,
    index_var    = "liberal_index",
    controls     = controls,
    sample_label = sg$label
  )
})

write.csv(all_robustness, paste0(loc, "Thesis-Github/Thesis-Coding/Results/Appendix/Robustness Checks/all_robustness_checks.csv"), row.names = FALSE)


## Combine and export ----------------------------------------------------------

# robust <- bind_rows(
#   map_dfr(all_robustness, "bandwidth")   %>% mutate(check_type = "bandwidth"),
#   map_dfr(all_robustness, "cutoffs")     %>% mutate(check_type = "cutoffs"),
#   map_dfr(all_robustness, "polynomial")  %>% mutate(check_type = "polynomial")
# )
#
#
#
# robustness_bw <- map_dfr(all_robustness, "bandwidth")
# robustness_co <- map_dfr(all_robustness, "cutoffs")
# robustness_poly <- map_dfr(all_robustness, "polynomial")
#
# write.csv(
#   robustness_bw,
#   paste0(loc, "Thesis-Github/Thesis-Coding/Results/robustness_bandwidth.csv"),
#   row.names = FALSE
# )
# write.csv(
#   robustness_co,
#   paste0(loc, "Thesis-Github/Thesis-Coding/Results/robustness_cutoffs.csv"),
#   row.names = FALSE
# )
# write.csv(
#   robustness_poly,
#   paste0(loc, "Thesis-Github/Thesis-Coding/Results/robustness_polynomial.csv"),
#   row.names = FALSE
# )


# ## Visualize robustness checks
# 
# ### Import the file
# all_robustness <- read.csv(paste0(loc, "Thesis-Github/Thesis-Coding/Results/Appendix/Robustness Checks/all_robustness_checks.csv"))
# 
# 
# ### Define a vector of the sub-group names
# subgroup_strings <- c(
#   "Full Sample", "Independents", "Partisans",
#   "Democrats", "Republicans",
#   "Expect In-Party Win", "Expect In-Party Loss"
# )
# 
# ### Set out the possible combinations of subgroups, estimate types, inclusion of covariates, etc.
# combinations_robustness <- expand.grid(
#   subgroup_name = subgroup_strings,
#   estimate_type = c(1, 2, 3),
#   include_covariates = c(FALSE, TRUE),
#   stringsAsFactors = FALSE
# )
# 
# ### Print all the plots
# walk(seq_len(nrow(combinations_robustness)), function(i) {
#   p <- get_robustnessplots(
#     data = all_robustness,
#     subgroup_name = combinations_robustness$subgroup_name[i],
#     estimate_type = combinations_robustness$estimate_type[i],
#     include_covariates = combinations_robustness$include_covariates[i]
#   )
#   print(p)
#   cat("\n\n")
# })


#------------------------------------------------------------------------------#


# Monte Carlo Simulation

## Define combinatios again

### Define a vector of the sub-group data frams
subgroup_dfs <- c(
  "df", "df_independents", "df_partisans",
  "df_democrats", "df_republicans",
  "df_inparty_win", "df_inparty_loss"
)


### Set out the possibel combinations
combinations_mc <- expand.grid(
  subset_df = subgroup_dfs,
  controls = list(controls, NULL),
  stringsAsFactors = FALSE
)

## Run the simulation for the combinations
mc_draws <- map_dfr(seq_len(nrow(combinations_mc)), function(i) {
  df_name <- combinations_mc$subset_df[i]
  controls <- combinations_mc$controls[[i]]

  run_mc_rdd(
    data = get(df_name),
    controls = combinations_mc$controls[[i]],
    sample_label = df_name
  )
})

## Save the file
write.csv(mc_draws, paste0(loc, paste0("Thesis-Github/Thesis-Coding/Results/Monte Carlo/mc_results.csv")), row.names = FALSE)


## Now format the results similar to the regression discontuinty results (e.g., rdd_liberal)
rdd_liberal_mc <- mc_draws %>% 
  group_by(sample, model) %>% 
  summarize(
    `Estimate Type` = "Monte Carlo", 
    Estimate = mean(estimate),
    SE = sd(estimate), 
    `P-Value`       = 2 * (1 - pnorm(abs(mean(estimate) / sd(estimate)))),
    `Bandwidth Type` = "MSE-optimal",
    `Bandwidth (h)` = mean(bw),
    N               = as.integer(round(mean(n_used))),
    Outcome         = "Liberal Attitudes",
  ) %>% 
  ungroup() %>% 
  rename(
    Sample = sample, 
    Model = model)%>%
  mutate(
    Sample = factor(Sample)
    ) %>%        
  select(Sample, Model, `Estimate Type`,     # match column order
         Estimate, SE, `P-Value`, 
         `Bandwidth Type`, `Bandwidth (h)`, 
         N, Outcome)




## Plot for Monte Carlo Simulations
mc_summary <- mc_draws %>%
  group_by(sample, model) %>%
  summarize(mean_est = mean(estimate), sd_est = sd(estimate), .groups = "drop")

mc_normal_curves <- mc_draws %>%
  group_by(sample, model) %>%
  reframe(
    x = seq(min(estimate), max(estimate), length.out = 300),
    y = dnorm(x, mean = mean(estimate), sd = sd(estimate))
  )

ggplot(mc_draws, aes(x = estimate)) +
  geom_histogram(aes(y = after_stat(density)),
    bins = 120,
    fill = "forestgreen", color = "white", alpha = 0.8
  ) +
  geom_line(
    data = mc_normal_curves, aes(x = x, y = y),
    color = "orchid4", linewidth = 1, inherit.aes = FALSE
  ) +
  geom_vline(data = mc_summary, aes(xintercept = mean_est), color = "red", linewidth = 0.5) +
  geom_vline(xintercept = 0, color = "gray50", linetype = "dashed") +
  facet_wrap(~ sample + model, scales = "free_y", ncol = 2) +
  labs(
    x = "RD Estimate (Bias-Corrected)",
    y = "Density",
    title = "Sampling Distribution of Monte Carlo Simulation Estimates",
    subtitle = "Liberalism Index"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold", hjust = 0.5),
    plot.subtitle   = element_text(hjust = 0.5)
  )


#------------------------------------------------------------------------------#


# Save dataset without birthdates but WITH treatment assignment, if allowed
df_approved <- df %>%
  select(-c(year_of_birth, month_of_birth, date_of_birth))

write_dta(df_approved, "/Users/nikolaosvichos/Library/Mobile Documents/com~apple~CloudDocs/Sciences Po/Thesis/Thesis-Github/Thesis-Coding/Results/Datasets/df_treatmentvar.dta")
write_csv(df_approved, "/Users/nikolaosvichos/Library/Mobile Documents/com~apple~CloudDocs/Sciences Po/Thesis/Thesis-Github/Thesis-Coding/Results/Datasets/df_treatmentvar.csv")


#------------------------------------------------------------------------------#


# Coefplots

rdd_liberal_for_simpleplot <- rdd_liberal %>%
  filter(`Estimate Type` == "Bias-Corrected") %>% 
  mutate(
    Model = factor(Model, 
    levels = c("Without Controls", "With Controls")
    ),
    Sample = factor(Sample,
      levels = c("Expect In-Party Loss", "Expect In-Party Win", "Republicans", "Democrats", "Partisans", "Independents", "Full Sample")
    )
  )

rdd_liberal_for_robustnessplot <- rdd_liberal %>%
  mutate(
    `Estimate Type` = factor(`Estimate Type`,
    levels = c("Conventional", "Bias-Corrected", "Bias-Corrected (Robust SE)")
  ),
  Sample = factor(Sample,
    levels = c("Expect In-Party Loss", "Expect In-Party Win", "Republicans", "Democrats", "Partisans", "Independents", "Full Sample")
  )
  )

get_coefplot_robustness(rdd_liberal_for_robustnessplot)

get_coefplot(rdd_liberal_for_simpleplot)


#------------------------------------------------------------------------------#


rdd_liberal_master <- bind_rows(rdd_liberal, rdd_liberal_mc)


lambda_table <- rdd_liberal_master %>%
  # Standardise model labels so they match across estimate types
  filter(`Estimate Type` %in% c("Bias-Corrected", "Monte Carlo")) %>%
  select(Sample, Model, `Estimate Type`, Estimate, Outcome) %>%
  pivot_wider(
    names_from  = `Estimate Type`,
    values_from = Estimate
  ) %>% 
  mutate(
    Lambda = `Bias-Corrected` / `Monte Carlo`
  )
