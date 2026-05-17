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

### N of respondents with complete data on all 7 index items (used in FA and alpha)
n_fa <- sum(complete.cases(df_unstandardized[, liberal_items]))

### Then save (N_respondents added for ICPSR compliance)
alpha_summary <- data.frame(
  metric = c("Cronbach's Alpha (raw)", "Cronbach's Alpha (std)", "n_items", "N_respondents_complete_cases"),
  value = c(
    round(alpha_result$total$raw_alpha, 3),
    round(alpha_result$total$std.alpha, 3),
    length(liberal_items),
    n_fa
  ),
  note = c(
    "Raw (unstandardized) Cronbach's alpha across 7 ANES items",
    "Standardized Cronbach's alpha across 7 ANES items",
    "Number of items in the index",
    "Respondents with non-missing data on all 7 items (listwise complete cases used in factor analysis)"
  )
)
write.csv(alpha_summary, paste0(loc, "Thesis-Github/Thesis-Coding/Results_mirror/Appendix/Index/fa_alpha_summary.csv"), row.names = FALSE)


## Extract factors and view the values

### First check
fa_liberal <- fa(
  df[, liberal_items],
  nfactors = 1, rotate = "none", fm = "pa", max.iter = 100
)
fa_liberal$loadings

fa_diagnostics <- data.frame(
  item          = liberal_items,
  item_label    = c(
    "Free press protection (V201366): support for protecting freedom of the press",
    "Checks and balances (V201367): importance of checks and balances on government",
    "Rule of law (V201368): importance that government follows laws even when inconvenient",
    "Agree on facts (V201369): importance of leaders agreeing on basic facts",
    "Limits on executive power (V201372x): opposition to president acting without Congress/courts",
    "Journalist access (V201375x): support for journalist access to government",
    "Media undermined concern (V201376): concern about politicians undermining the media"
  ),
  anes_variable = c("V201366", "V201367", "V201368", "V201369", "V201372x", "V201375x", "V201376"),
  loading       = round(as.numeric(fa_liberal$loadings[, 1]), 3),
  communality   = round(fa_liberal$communality, 3),
  uniqueness    = round(fa_liberal$uniquenesses, 3),
  item_total_r  = round(alpha_result$item.stats$r.drop, 3),
  N_nonmissing  = sapply(liberal_items, function(v) sum(!is.na(df_unstandardized[[v]])))
)
write.csv(fa_diagnostics, paste0(loc, "Thesis-Github/Thesis-Coding/Results_mirror/Appendix/Index/fa_disagnostics.csv"), row.names = FALSE)


### Then save (N_respondents added; linked to screeplot for ICPSR chart-table requirement)
fa_eigenvalues <- data.frame(
  factor              = seq_along(fa_liberal$values),
  eigenvalue          = round(fa_liberal$values, 3),
  variance_explained  = round(fa_liberal$values / sum(fa_liberal$values), 3),
  cumulative_variance = round(cumsum(fa_liberal$values / sum(fa_liberal$values)), 3),
  N_respondents       = n_fa
)
write.csv(fa_eigenvalues, paste0(loc, "Thesis-Github/Thesis-Coding/Results_mirror/Appendix/Index/fa_eigenvalues.csv"), row.names = FALSE)


## Screeplot
screeplot_liberal <- get_screeplot(fa_liberal, "Liberalism Index")
print(screeplot_liberal)

ggsave(
  paste0(loc, "Thesis-Github/Thesis-Coding/Results_mirror/Appendix/Index/screeplot_liberal.png"),
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
  paste0(loc, "Thesis-Github/Thesis-Coding/Results_mirror/Main/Plots/Discontinuity/discontinuityplot_full.png"),
  plot = discontinuityplot_parties$plot_all, width = 6.5, height = 5, units = "in", dpi = 300
)
ggsave(
  paste0(loc, "Thesis-Github/Thesis-Coding/Results_mirror/Main/Plots/Discontinuity/discontinuityplot_subgroups.png"),
  plot = discontinuityplot_parties$plot_subgroups, width = 6.5, height = 5.5, units = "in", dpi = 300
)

## Backing data table for discontinuityplot_full.png and discontinuityplot_subgroups.png
## (ICPSR requirement: charts must have an associated table)
## Observations binned in 60-day intervals of days_from_cutoff; summary statistics per bin.
disc_data_parties <- df %>%
  filter(!is.na(liberal_index), !is.na(party_summary), !is.na(days_from_cutoff),
         days_from_cutoff >= -4000) %>%
  mutate(
    Subgroup = case_when(
      party_summary == "Democrat"    ~ "Democrats",
      party_summary == "Republican"  ~ "Republicans",
      party_summary == "Independent" ~ "Independents"
    ),
    Bin_60days = cut(
      days_from_cutoff,
      breaks = seq(
        floor(min(days_from_cutoff, na.rm = TRUE) / 60) * 60,
        ceiling(max(days_from_cutoff, na.rm = TRUE) / 60) * 60,
        by = 60
      ),
      include.lowest = TRUE
    )
  ) %>%
  group_by(Subgroup, treatment, Bin_60days) %>%
  summarize(
    N                  = n(),
    Mean_Liberal_Index = round(mean(liberal_index, na.rm = TRUE), 4),
    SD_Liberal_Index   = round(sd(liberal_index,   na.rm = TRUE), 4),
    .groups = "drop"
  ) %>%
  mutate(
    Treatment_Label    = ifelse(treatment == 1,
                                "Born after Nov 6, 1994 (Treatment, treatment=1)",
                                "Born on/before Nov 6, 1994 (Control, treatment=0)"),
    Outcome            = "Liberal Democratic Norms Index (0-1)",
    Bin_Width_Days     = 60
  )

write.csv(
  disc_data_parties,
  paste0(loc, "Thesis-Github/Thesis-Coding/Results_mirror/Main/Plots/Discontinuity/discontinuityplot_data_parties.csv"),
  row.names = FALSE
)

discontinuityplot_expectations <- get_discontinuityplot(df, party_id = "expect_inparty_win", colnumber = 1)
print(discontinuityplot_expectations$plot_subgroups)
ggsave(
  paste0(loc, "Thesis-Github/Thesis-Coding/Results_mirror/Main/Plots/Discontinuity/discontinuityplot_expectations.png"),
  plot = discontinuityplot_expectations$plot_subgroups, width = 6.5, height = 7.5, units = "in", dpi = 300
)

## Backing data table for discontinuityplot_expectations.png
disc_data_expectations <- df %>%
  filter(!is.na(liberal_index), !is.na(expect_inparty_win), !is.na(days_from_cutoff),
         days_from_cutoff >= -4000) %>%
  mutate(
    Subgroup = case_when(
      expect_inparty_win == 1 ~ "Expect In-Party Win",
      expect_inparty_win == 0 ~ "Expect In-Party Loss"
    ),
    Bin_60days = cut(
      days_from_cutoff,
      breaks = seq(
        floor(min(days_from_cutoff, na.rm = TRUE) / 60) * 60,
        ceiling(max(days_from_cutoff, na.rm = TRUE) / 60) * 60,
        by = 60
      ),
      include.lowest = TRUE
    )
  ) %>%
  group_by(Subgroup, treatment, Bin_60days) %>%
  summarize(
    N                  = n(),
    Mean_Liberal_Index = round(mean(liberal_index, na.rm = TRUE), 4),
    SD_Liberal_Index   = round(sd(liberal_index,   na.rm = TRUE), 4),
    .groups = "drop"
  ) %>%
  mutate(
    Treatment_Label = ifelse(treatment == 1,
                             "Born after Nov 6, 1994 (Treatment, treatment=1)",
                             "Born on/before Nov 6, 1994 (Control, treatment=0)"),
    Outcome         = "Liberal Democratic Norms Index (0-1)",
    Bin_Width_Days  = 60
  )

write.csv(
  disc_data_expectations,
  paste0(loc, "Thesis-Github/Thesis-Coding/Results_mirror/Main/Plots/Discontinuity/discontinuityplot_data_expectations.csv"),
  row.names = FALSE
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
## Note: Outcome, Running_Variable, Cutoff, Weighted, Controls_Used columns
## are already added inside run_rdd_models(); no need to add them here.
rdd_liberal <- bind_rows(
  rdd_liberal_full, rdd_liberal_independents, rdd_liberal_partisans,
  rdd_liberal_democrats, rdd_liberal_republicans, rdd_liberal_win, rdd_liberal_loss
) %>%
  mutate(
    Sample = factor(
      Sample,
      levels = c("Republicans", "Democrats", "Partisans", "Independents", "Full Sample", "Expect In-Party Win", "Expect In-Party Loss")
    )
  )

write.csv(rdd_liberal, paste0(loc, paste0("Thesis-Github/Thesis-Coding/Results_mirror/Main/rdd_liberal.csv")))


#------------------------------------------------------------------------------#


# Descriptive statistics #

## Variable labels (for ICPSR codebook compliance)
var_labels <- c(
  age_2012_election = "Age in years at the November 2012 presidential election",
  sex               = "Sex: binary indicator (1 = Male, 0 = Female; recoded from ANES V201600)",
  education         = "Education level (ANES V201510; ordinal 1–16; 1 = Less than 1st grade, 16 = Doctorate)",
  income            = "Household income (ANES V202468x; ordinal 1–22; 1 = Under $9,999, 22 = $250,000 or more)",
  race_Black        = "Race dummy: Black / African-American (1 = Yes, 0 = No; reference category = White; derived from ANES V201549x)",
  race_Hispanic     = "Race dummy: Hispanic / Latino (1 = Yes, 0 = No; reference = White)",
  race_Asian        = "Race dummy: Asian / Asian-American (1 = Yes, 0 = No; reference = White)",
  race_Native       = "Race dummy: Native American / Alaskan Native (1 = Yes, 0 = No; reference = White)",
  race_Other        = "Race dummy: Other race (1 = Yes, 0 = No; reference = White)",
  liberal_index     = "Liberal Democratic Norms Index: 1-factor principal-axis score of 7 ANES items (V201366, V201367, V201368, V201369, V201372x, V201375x, V201376), rescaled to [0, 1]; higher = stronger support for liberal democratic norms; unweighted"
)

## Plain-English descriptions of each sample / subgroup
sample_descriptions <- c(
  "Full Sample"         = "All ANES 2020 panel respondents in waves 3–6 (V200003 in {3,4,5,6}), pre- and post-election interviews (V200004 in {1,3}), with non-missing outcome and running variable",
  "Independents"        = "Respondents self-identifying as Independent on the 7-point party ID scale (ANES V201231x codes 3–5, including leaners); excludes pure Democrats and Republicans",
  "Partisans"           = "Respondents self-identifying as Democrat or Republican (ANES V201231x codes 1–2 or 6–7); Independents (codes 3–5) excluded",
  "Democrats"           = "Respondents self-identifying as Democrat or leaning Democrat (ANES V201231x codes 1–2)",
  "Republicans"         = "Respondents self-identifying as Republican or leaning Republican (ANES V201231x codes 6–7)",
  "Expect In-Party Win" = "Partisan respondents (non-Independents) who expected their own party's candidate to win the 2020 presidential election (derived from ANES V201217 crossed with party_summary)",
  "Expect In-Party Loss"= "Partisan respondents (non-Independents) who expected the opposing party's candidate to win the 2020 presidential election (derived from ANES V201217 crossed with party_summary)"
)

## Variables to describe — split into two tables for readability.
## Table 1 (descriptives_main.csv): continuous and single binary variables + outcome
desc_vars_main <- c(
  "age_2012_election",
  "sex",
  "education",
  "income",
  "liberal_index"
)

## Table 2 (descriptives_race.csv): race dummies (binary 0/1; mean = proportion)
## Reference category (White) is omitted from regressions but described in metadata.
desc_vars_race <- c(
  "race_Black", "race_Hispanic", "race_Asian", "race_Native", "race_Other"
)

## Function: computes descriptive statistics by treatment group for each variable.
## Additions for ICPSR compliance:
##   - Sample_Description: plain-English definition of the subgroup
##   - Variable_Label: human-readable description of the variable and its scale
##   - Min / Max with observation counts (required by ICPSR for continuous variables)
##   - Count_0 / Count_1 for binary/dummy variables (required by ICPSR)
##   - Control_Group_Label / Treatment_Group_Label: explicit definition of the two groups
##   - Weighted flag
get_descriptives <- function(data, sample_label, vars = desc_vars_main) {
  map_dfr(vars, function(var) {
    x   <- data[[var]]
    trt <- data$treatment

    # Keep only rows where both var and treatment are non-missing
    keep    <- !is.na(x) & !is.na(trt)
    x_ctrl  <- x[keep & trt == 0]
    x_treat <- x[keep & trt == 1]

    is_binary <- all(na.omit(x) %in% c(0, 1))

    pval <- tryCatch(
      t.test(x[keep] ~ trt[keep])$p.value,
      error = function(e) NA_real_
    )

    tibble(
      Sample                  = sample_label,
      Sample_Description      = unname(sample_descriptions[sample_label]),
      Variable                = var,
      Variable_Label          = unname(var_labels[var]),
      Weighted                = "No",
      # --- Control group (treatment = 0: born on/before Nov 6 1994) ---
      # Min/Max suppressed (shown as NA) when fewer than 10 observations take that value,
      # per ICPSR disclosure-avoidance rules.
      `Mean (Control)`        = round(mean(x_ctrl),  4),
      `SD (Control)`          = round(sd(x_ctrl),    4),
      `N (Control)`           = length(x_ctrl),
      `Min (Control)`         = if (sum(x_ctrl == min(x_ctrl)) < 10) NA_real_ else min(x_ctrl),
      `N at Min (Control)`    = if (sum(x_ctrl == min(x_ctrl)) < 10) NA_integer_ else sum(x_ctrl == min(x_ctrl)),
      `Max (Control)`         = if (sum(x_ctrl == max(x_ctrl)) < 10) NA_real_ else max(x_ctrl),
      `N at Max (Control)`    = if (sum(x_ctrl == max(x_ctrl)) < 10) NA_integer_ else sum(x_ctrl == max(x_ctrl)),
      `Count_0 (Control)`     = if (is_binary) sum(x_ctrl == 0) else NA_integer_,
      `Count_1 (Control)`     = if (is_binary) sum(x_ctrl == 1) else NA_integer_,
      # --- Treatment group (treatment = 1: born after Nov 6 1994) ---
      `Mean (Treatment)`      = round(mean(x_treat), 4),
      `SD (Treatment)`        = round(sd(x_treat),   4),
      `N (Treatment)`         = length(x_treat),
      `Min (Treatment)`       = if (sum(x_treat == min(x_treat)) < 10) NA_real_ else min(x_treat),
      `N at Min (Treatment)`  = if (sum(x_treat == min(x_treat)) < 10) NA_integer_ else sum(x_treat == min(x_treat)),
      `Max (Treatment)`       = if (sum(x_treat == max(x_treat)) < 10) NA_real_ else max(x_treat),
      `N at Max (Treatment)`  = if (sum(x_treat == max(x_treat)) < 10) NA_integer_ else sum(x_treat == max(x_treat)),
      `Count_0 (Treatment)`   = if (is_binary) sum(x_treat == 0) else NA_integer_,
      `Count_1 (Treatment)`   = if (is_binary) sum(x_treat == 1) else NA_integer_,
      # --- Test and group labels ---
      `P-Value (t-test)`      = round(pval, 4),
      Control_Group_Label     = "Born on or before November 6, 1994 (treatment = 0): eligible to vote in the 2012 presidential election",
      Treatment_Group_Label   = "Born after November 6, 1994 (treatment = 1): first eligible presidential election was 2016"
    )
  })
}

## Table 1: main demographic variables + outcome (age, sex, education, income, liberal_index)
descriptives_main <- bind_rows(
  get_descriptives(df,               "Full Sample",          vars = desc_vars_main),
  get_descriptives(df_independents,  "Independents",         vars = desc_vars_main),
  get_descriptives(df_partisans,     "Partisans",            vars = desc_vars_main),
  get_descriptives(df_democrats,     "Democrats",            vars = desc_vars_main),
  get_descriptives(df_republicans,   "Republicans",          vars = desc_vars_main),
  get_descriptives(df_inparty_win,   "Expect In-Party Win",  vars = desc_vars_main),
  get_descriptives(df_inparty_loss,  "Expect In-Party Loss", vars = desc_vars_main)
)

write.csv(
  descriptives_main,
  paste0(loc, "Thesis-Github/Thesis-Coding/Results_mirror/Appendix/descriptives_main.csv"),
  row.names = FALSE
)

## Table 2: race dummies (mean = proportion; Count_0/Count_1 give raw cell counts)
## Reference category White is omitted from regressions; noted in Reference_Category_Race
## column of rdd_liberal.csv.
descriptives_race <- bind_rows(
  get_descriptives(df,               "Full Sample",          vars = desc_vars_race),
  get_descriptives(df_independents,  "Independents",         vars = desc_vars_race),
  get_descriptives(df_partisans,     "Partisans",            vars = desc_vars_race),
  get_descriptives(df_democrats,     "Democrats",            vars = desc_vars_race),
  get_descriptives(df_republicans,   "Republicans",          vars = desc_vars_race),
  get_descriptives(df_inparty_win,   "Expect In-Party Win",  vars = desc_vars_race),
  get_descriptives(df_inparty_loss,  "Expect In-Party Loss", vars = desc_vars_race)
)

write.csv(
  descriptives_race,
  paste0(loc, "Thesis-Github/Thesis-Coding/Results_mirror/Appendix/descriptives_race.csv"),
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

write.csv(all_robustness, paste0(loc, "Thesis-Github/Thesis-Coding/Results_mirror/Appendix/Robustness Checks/all_robustness_checks.csv"), row.names = FALSE)


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
#   paste0(loc, "Thesis-Github/Thesis-Coding/Results_mirror/robustness_bandwidth.csv"),
#   row.names = FALSE
# )
# write.csv(
#   robustness_co,
#   paste0(loc, "Thesis-Github/Thesis-Coding/Results_mirror/robustness_cutoffs.csv"),
#   row.names = FALSE
# )
# write.csv(
#   robustness_poly,
#   paste0(loc, "Thesis-Github/Thesis-Coding/Results_mirror/robustness_polynomial.csv"),
#   row.names = FALSE
# )


# ## Visualize robustness checks
# 
### Import the file
### Compatibility patch: CSVs generated before the column-rename refactor used lowercase
### names (check_type, bandwidth, cutoff, polynomial, controls as TRUE/FALSE, sample, h_opt).
### The rename_with block below silently maps old → new names so get_robustnessplots()
### works regardless of which version of run_robustness_checks() produced the file.
all_robustness <- read.csv(paste0(loc, "Thesis-Github/Thesis-Coding/Results_mirror/Appendix/Robustness Checks/all_robustness_checks.csv")) %>%
  rename(any_of(c(
    Check_Type          = "check_type",
    Bandwidth           = "bandwidth",
    Placebo_Cutoff      = "cutoff",
    Polynomial_Degree   = "polynomial",
    Controls            = "controls",
    Sample              = "sample",
    Optimal_Bandwidth_h = "h_opt"
  ))) %>%
  mutate(
    # Translate old check_type values → new descriptive labels (no-op if already new)
    Check_Type = case_when(
      Check_Type == "bandwidth"  ~ "Bandwidth Sensitivity",
      Check_Type == "cutoffs"    ~ "Placebo Cutoff Test",
      Check_Type == "polynomial" ~ "Polynomial Degree Sensitivity",
      TRUE                       ~ Check_Type
    ),
    # Translate old TRUE/FALSE controls column → character labels (no-op if already new)
    Controls = case_when(
      Controls %in% c("TRUE",  "true",  "1") ~ "With Controls",
      Controls %in% c("FALSE", "false", "0") ~ "Without Controls",
      TRUE                                   ~ as.character(Controls)
    )
  )


### Define a vector of the sub-group names
subgroup_strings <- c(
  "Full Sample", "Independents", "Partisans",
  "Democrats", "Republicans",
  "Expect In-Party Win", "Expect In-Party Loss"
)

### Set out the possible combinations of subgroups, estimate types, inclusion of covariates, etc.
combinations_robustness <- expand.grid(
  subgroup_name = subgroup_strings,
  estimate_type = c(1, 2, 3),
  include_covariates = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)

### Print all plots (rendered via Quarto for online appendix)
walk(seq_len(nrow(combinations_robustness)), function(i) {
  print(get_robustnessplots(
    data               = all_robustness,
    subgroup_name      = combinations_robustness$subgroup_name[i],
    estimate_type      = combinations_robustness$estimate_type[i],
    include_covariates = combinations_robustness$include_covariates[i]
  ))
})


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
write.csv(mc_draws, paste0(loc, paste0("Thesis-Github/Thesis-Coding/Results_mirror/Monte Carlo/mc_results.csv")), row.names = FALSE)


## Now format the results similar to the regression discontinuity results (e.g., rdd_liberal)
## Estimate = mean of bias-corrected LATE across 10,000 MC draws
## SE       = SD of the sampling distribution (Monte Carlo standard error)
## N_total         = total respondents in the subgroup before any bandwidth restriction (constant across draws)
## N_within_bw     = mean number of observations within the MSE-optimal bandwidth across draws
rdd_liberal_mc <- mc_draws %>%
  group_by(sample, model) %>%
  summarize(
    `Estimate Type`  = "Monte Carlo",
    Estimate         = mean(estimate),
    SE               = sd(estimate),
    `P-Value`        = 2 * (1 - pnorm(abs(mean(estimate) / sd(estimate)))),
    `Bandwidth Type` = "MSE-optimal",
    `Bandwidth (h)`  = round(mean(bw), 2),
    N_total          = first(n_total),
    N_within_bw      = as.integer(round(mean(n_used))),
    Outcome          = "Liberal Democratic Norms Index: 1-factor principal-axis score of 7 ANES items (V201366, V201367, V201368, V201369, V201372x, V201375x, V201376), rescaled to [0, 1]; higher = stronger support for liberal democratic norms",
    Weighted         = "No",
    .groups          = "drop"
  ) %>%
  rename(Sample = sample, Model = model) %>%
  mutate(Sample = factor(Sample)) %>%
  select(Sample, Model, `Estimate Type`,
         Estimate, SE, `P-Value`,
         `Bandwidth Type`, `Bandwidth (h)`,
         N_total, N_within_bw, Outcome, Weighted)




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

## Save the MC histogram (ICPSR requirement: charts must have associated table)
ggsave(
  paste0(loc, "Thesis-Github/Thesis-Coding/Results_mirror/Monte Carlo/mc_histogram.png"),
  width = 12, height = 8, units = "in", dpi = 300
)

## Backing data table for mc_histogram.png: binned density of simulation estimates
## 30 equal-width bins per sample × model combination, matching the histogram bins above
mc_histogram_data <- mc_draws %>%
  group_by(sample, model) %>%
  mutate(
    Bin = cut(estimate, breaks = 30, include.lowest = TRUE)
  ) %>%
  group_by(sample, model, Bin) %>%
  summarize(
    N_draws         = n(),
    Bin_Midpoint    = round(mean(estimate), 6),
    Density         = round(n() / (nrow(cur_group()) * diff(range(estimate))), 6),
    .groups = "drop"
  ) %>%
  rename(Sample = sample, Model = model) %>%
  mutate(
    Outcome  = "Liberal Democratic Norms Index (0-1)",
    Weighted = "No",
    Note     = "Each row is one histogram bin. Bin_Midpoint = mean estimate within the bin. N_draws = count of simulation iterations falling in that bin."
  )

write.csv(
  mc_histogram_data,
  paste0(loc, "Thesis-Github/Thesis-Coding/Results_mirror/Monte Carlo/mc_histogram_data.csv"),
  row.names = FALSE
)


#------------------------------------------------------------------------------#


# Save dataset without birthdates but WITH treatment assignment, if allowed
df_approved <- df %>%
  select(-c(year_of_birth, month_of_birth, date_of_birth))

write_dta(df_approved, "/Users/nikolaosvichos/Library/Mobile Documents/com~apple~CloudDocs/Sciences Po/Thesis/Thesis-Github/Thesis-Coding/Results_mirror/Datasets/df_treatmentvar.dta")
write_csv(df_approved, "/Users/nikolaosvichos/Library/Mobile Documents/com~apple~CloudDocs/Sciences Po/Thesis/Thesis-Github/Thesis-Coding/Results_mirror/Datasets/df_treatmentvar.csv")


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
    ), 
    Outcome = case_when()
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


#------------------------------------------------------------------------------#


# Study-level metadata (ICPSR requirement: study name and PI within output files)

metadata <- data.frame(
  Field = c(
    "Study_Name",
    "PI_Name",
    "PI_Institution",
    "Data_Source",
    "Data_Version",
    "Analysis_Date",
    "Outcome_Variable",
    "Outcome_Description",
    "Running_Variable",
    "RDD_Cutoff_Date",
    "Control_Group",
    "Treatment_Group",
    "Weighted",
    "Survey_Weights_Available",
    "Subgroups",
    "Subgroup_Variable_Source",
    "RDD_Package",
    "FA_Method",
    "Index_ANES_Items",
    "Index_Scale",
    "N_MC_Simulations",
    "Output_Files"
  ),
  Value = c(
    "Coming of Age Under Trump: First Electoral Exposure and Support for Liberal Democratic Norms",
    "Nikolaos Vichos",
    "Sciences Po Paris",
    "American National Election Study (ANES) 2020 Time Series Study — Restricted version (contains exact respondent birthdates)",
    "Panel waves 3–6 (V200003 in {3, 4, 5, 6}), pre- and post-election interviews (V200004 in {1, 3})",
    as.character(Sys.Date()),
    "liberal_index",
    "Liberal Democratic Norms Index: 1-factor principal-axis score of 7 ANES items (V201366, V201367, V201368, V201369, V201372x, V201375x, V201376), rescaled to [0, 1]; higher values = stronger support for liberal democratic norms",
    "days_from_cutoff: integer days between respondent's exact date of birth and November 6, 1994; negative values = born before cutoff (Control group)",
    "November 6, 1994 — respondents born on or before this date were aged 18 or older at the November 2012 presidential election and therefore eligible to vote (Control, treatment=0); those born after this date had the 2016 presidential election as their first eligible election (Treatment, treatment=1)",
    "Born on or before November 6, 1994 (treatment=0): eligible to vote in the 2012 presidential election",
    "Born after November 6, 1994 (treatment=1): first eligible presidential election was 2016 (Trump's first election as candidate)",
    "No — all analyses are unweighted; ANES post-stratification weight variable V200010b is available in the restricted dataset but was not applied",
    "Yes — ANES post-stratification weight V200010b is present in restricted data",
    "Full Sample; Democrats; Republicans; Independents; Partisans; Expect In-Party Win; Expect In-Party Loss",
    "Party subgroups from ANES V201231x (7-point party identification); Expectation subgroups from V201217 (2020 election outcome expectations) crossed with party_summary",
    "rdrobust (Calonico, Cattaneo & Titiunik 2014/2020); MSE-optimal bandwidth (bwselect = 'mserd'); local linear polynomial (p=1); bias-corrected estimate preferred for main results",
    "psych::fa(), nfactors=1, rotate='none', fm='pa' (principal axis); scores rescaled to [0,1] via scales::rescale()",
    "free_press (V201366), checks_and_balances (V201367), rule_of_law (V201368), agree_on_facts (V201369), unitary_executive (V201372x), journalist_access (V201375x), media_undermined_concern (V201376)",
    "Factor scores from psych::fa() rescaled to [0, 1] using scales::rescale(); higher = stronger liberal democratic norm support",
    "10,000 draws per sample × model combination; birth month and day simulated uniformly for 1994-born respondents only; rdrobust() run on each draw; mean of sampling distribution = point estimate, SD = standard error",
    paste(
      "Results/Main/rdd_liberal.csv (main RDD results);",
      "Results/Appendix/descriptives.csv (balance table);",
      "Results/Appendix/Robustness Checks/all_robustness_checks.csv;",
      "Results/Monte Carlo/mc_results.csv (raw MC draws);",
      "Results/Monte Carlo/mc_histogram.png + mc_histogram_data.csv;",
      "Results/Appendix/Index/fa_alpha_summary.csv;",
      "Results/Appendix/Index/fa_disagnostics.csv;",
      "Results/Appendix/Index/fa_eigenvalues.csv + screeplot_liberal.png;",
      "Results/Main/Plots/Discontinuity/discontinuityplot_*.png + *_data_*.csv"
    )
  ),
  stringsAsFactors = FALSE
)

write.csv(
  metadata,
  paste0(loc, "Thesis-Github/Thesis-Coding/Results_mirror/metadata.csv"),
  row.names = FALSE
)




