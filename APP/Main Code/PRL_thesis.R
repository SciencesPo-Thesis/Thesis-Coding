#########################################################################
#########################################################################
############# Replication Code: "Coming of Age Under Trump" #############
########### The Effect of First Electoral Exposure in a Trump ###########
################# Election on Long-Term Liberal Attitudes ###############
###################### POLARIZATION RESEARCH LAB DATA ###################
#########################################################################
#########################################################################

# Author: Nikolaos VICHOS


# Necessary Packages
library(styler) # code reformatting
library(psych) # factor analysis
library(tidyverse) # loads ggplot2, tidyr, purrr, readr, forcats, dplyr, tibble
library(scales) # rescale() for index 0–1 scaling
library(rdrobust) # RDD estimation
library(modelsummary) # regression tables
library(ggthemes) # additional ggplot2 themes
library(patchwork) # combine multiple ggplot2 plots side-by-side
library(knitr) # kable() for markdown/LaTeX tables
library(ggtext) # element_markdown() for HTML/Markdown in ggplot2 labels and captions


#------------------------------------------------------------------------------#


# File paths

## Results
loc_results <- "/Users/nikolaosvichos/Library/Mobile Documents/com~apple~CloudDocs/Sciences Po/Thesis/Thesis-Github/Thesis-Coding/APP/Results"

## Data files
loc_data <- "/Users/nikolaosvichos/Library/Mobile Documents/com~apple~CloudDocs/Sciences Po/Thesis/Datasets/PRL/prl_data.csv"
loc_ANES <- "/Users/nikolaosvichos/Library/Mobile Documents/com~apple~CloudDocs/Sciences Po/Thesis/Thesis-Github/Thesis-Coding/ANES Restricted/Results"

## Functions
loc_func <- "/Users/nikolaosvichos/Library/Mobile Documents/com~apple~CloudDocs/Sciences Po/Thesis/Thesis-Github/Thesis-Coding/APP/Main Code/functions.R"

## Load functions
source(loc_func)


#------------------------------------------------------------------------------#


# Data import and cleaning

## Data import
df_unclean <- read.csv(loc_data)

## Variable selection sand cleianing
df_unstandardized <- df_unclean %>%
  dplyr::select(
    id, # responder id
    weight, # weights
    feeling_dems = democrat_therm_1, # feeling thermometer toward democrats
    feeling_reps = republican_therm_1, # feeling thermometer toward republicans
    democracy_importance, # how important is it to live in a democracy
    norm_judges, # democratic norm: respect judicial decisions (agree = undemocratic)
    norm_polling, # democratic norm: voter access (agree = undemocratic)
    norm_executive, # democratic norm: executive orders (agree = undemocratic)
    norm_censorship, # democratic norm: media freedom (agree = undemocratic)
    norm_loyalty, # democratic norm: respecting election results (agree = undemocratic)
    year_of_birth = birthyr, # year of birth
    sex = gender, # sex
    race, # race
    education = educ, # level of education
    income = faminc_new, # family income
    party = pid7, # party, 7-point category
    voted_for_2016 = presvote16post, # candidate choice in 2016
    voted_for_2020 = presvote20post, # candidate choice in 2020
    voted_for_2024 = presvote24post, # candidate choice in 2024
    urban_rural = urbanicity2, # urban typology -- where does respondent live
    ideology = ideo5, # 5-point scale of ideology (liberal-conservative)
    survey_year = year, # year during which the survey was conducted
    survey_week = week, # week during which the survey was conducted
    survey_wave = survey, # survey wave
    survey_starttime = starttime, # survey start time
    survey_endtime = endtime, # survey end time
  ) %>%
  mutate(
    # convert timestamps and calculate duration
    survey_starttime = as.POSIXct(survey_starttime, format = "%Y-%m-%d %H:%M:%S"),
    survey_endtime = as.POSIXct(survey_endtime, format = "%Y-%m-%d %H:%M:%S"),
    survey_duration = as.numeric(difftime(survey_endtime, survey_starttime, units = "mins")),
  ) %>%
  select(-survey_starttime, -survey_endtime) %>%
  filter(!is.na(survey_duration), survey_duration > 0) %>%
  filter(
    survey_duration >= quantile(survey_duration, 0.25, na.rm = TRUE) - 1.5 * IQR(survey_duration, na.rm = TRUE),
    survey_duration <= quantile(survey_duration, 0.75, na.rm = TRUE) + 1.5 * IQR(survey_duration, na.rm = TRUE)
  ) %>%
  mutate(
    # convert democracy_importance to numeric; anything unrecognised (incl. "") → NA
    democracy_importance = case_when(
      democracy_importance == "Very unimportant" ~ 1,
      democracy_importance == "Unimportant" ~ 2,
      democracy_importance == "Neither important nor unimportant" ~ 3,
      democracy_importance == "Important" ~ 4,
      democracy_importance == "Very important" ~ 5,
      TRUE ~ NA
    ),
    # convert norm_* variables to numeric; anything unrecognised (incl. "") → NA
    norm_judges = case_when(
      norm_judges == "Strongly disagree" ~ 1,
      norm_judges == "Disagree" ~ 2,
      norm_judges == "Neither agree nor disagree" ~ 3,
      norm_judges == "Agree" ~ 4,
      norm_judges == "Strongly agree" ~ 5,
      TRUE ~ NA
    ),
    norm_polling = case_when(
      norm_polling == "Strongly disagree" ~ 1,
      norm_polling == "Disagree" ~ 2,
      norm_polling == "Neither agree nor disagree" ~ 3,
      norm_polling == "Agree" ~ 4,
      norm_polling == "Strongly agree" ~ 5,
      TRUE ~ NA
    ),
    norm_executive = case_when(
      norm_executive == "Strongly disagree" ~ 1,
      norm_executive == "Disagree" ~ 2,
      norm_executive == "Neither agree nor disagree" ~ 3,
      norm_executive == "Agree" ~ 4,
      norm_executive == "Strongly agree" ~ 5,
      TRUE ~ NA
    ),
    norm_censorship = case_when(
      norm_censorship == "Strongly disagree" ~ 1,
      norm_censorship == "Disagree" ~ 2,
      norm_censorship == "Neither agree nor disagree" ~ 3,
      norm_censorship == "Agree" ~ 4,
      norm_censorship == "Strongly agree" ~ 5,
      TRUE ~ NA
    ),
    norm_loyalty = case_when(
      norm_loyalty == "Strongly disagree" ~ 1,
      norm_loyalty == "Disagree" ~ 2,
      norm_loyalty == "Neither agree nor disagree" ~ 3,
      norm_loyalty == "Agree" ~ 4,
      norm_loyalty == "Strongly agree" ~ 5,
      TRUE ~ NA
    ),
    # ── clean remaining variables ─────────────────────────────────────────────
    # sex: 1 = Male, 0 = Female; numeric-coded rows (unverifiable mapping) → NA
    sex = case_when(
      sex == "Male" ~ 1L,
      sex == "Female" ~ 0L,
      TRUE ~ NA_integer_
    ),
    # race: factor with White as reference; unrecognised values (incl. numeric-coded rows) → NA
    race = relevel(
      factor(
        case_when(
          race == "White" ~ 1L,
          race == "Black" ~ 2L,
          race == "Hispanic" ~ 3L,
          race == "Asian" ~ 4L,
          race == "Native American" ~ 5L,
          race == "Middle Eastern" ~ 6L,
          race == "Two or more races" ~ 7L,
          race == "Other" ~ 8L,
          TRUE ~ NA_integer_
        ),
        levels = c(1, 2, 3, 4, 5, 6, 7, 8),
        labels = c(
          "White", "Black", "Hispanic", "Asian",
          "Native American", "Middle Eastern", "Two or more races", "Other"
        )
      ),
      ref = "White"
    ),
    # education: numeric-coded rows from earlier waves have unverifiable mapping → NA
    education = case_when(
      education == "No HS" ~ 1L,
      education == "High school graduate" ~ 2L,
      education == "Some college" ~ 3L,
      education == "2-year" ~ 4L,
      education == "4-year" ~ 5L,
      education == "Post-grad" ~ 6L,
      TRUE ~ NA_integer_
    ),
    # income: ordinal 1–16; numeric-coded rows, "Prefer not to say", and stray codes → NA
    # 1=<$10k, 2=$10–19k, 3=$20–29k, ..., 16=$500k+
    income = case_when(
      income == "Less than $10,000" ~ 1L,
      income == "$10,000 - $19,999" ~ 2L,
      income == "$20,000 - $29,999" ~ 3L,
      income == "$30,000 - $39,999" ~ 4L,
      income == "$40,000 - $49,999" ~ 5L,
      income == "$50,000 - $59,999" ~ 6L,
      income == "$60,000 - $69,999" ~ 7L,
      income == "$70,000 - $79,999" ~ 8L,
      income == "$80,000 - $99,999" ~ 9L,
      income == "$100,000 - $119,999" ~ 10L,
      income == "$120,000 - $149,999" ~ 11L,
      income == "$150,000 - $199,999" ~ 12L,
      income == "$200,000 - $249,999" ~ 13L,
      income == "$250,000 - $349,999" ~ 14L,
      income == "$350,000 - $499,999" ~ 15L,
      income == "$500,000 or more" ~ 16L,
      TRUE ~ NA_integer_
    ),
    # party: pid7 as cleaned 7-point integer (1=Strong Dem … 4=Independent … 7=Strong Rep)
    # numeric-coded rows from earlier waves have unverifiable mapping → NA
    party = case_when(
      party == "Strong Democrat" ~ 1L,
      party == "Not very strong Democrat" ~ 2L,
      party == "Lean Democrat" ~ 3L,
      party == "Independent" ~ 4L,
      party == "Lean Republican" ~ 5L,
      party == "Not very strong Republican" ~ 6L,
      party == "Strong Republican" ~ 7L,
      TRUE ~ NA_integer_
    ),
    # party_summary: pid7 collapsed to 3-category factor, following ANES coding
    # 1–2 = Democrat, 3–5 = Independent (including leaners), 6–7 = Republican
    party_summary = factor(
      case_when(
        party %in% c(1, 2) ~ 1,
        party %in% c(3, 4, 5) ~ 2,
        party %in% c(6, 7) ~ 3,
        TRUE ~ NA
      ),
      levels = c(1, 2, 3),
      labels = c("Democrat", "Independent", "Republican")
    ),
    # ideology (ideo5): 1=Very liberal … 3=Moderate … 5=Very conservative
    # numeric-coded rows and "Not sure" → NA
    ideology = case_when(
      ideology == "Very liberal" ~ 1L,
      ideology == "Liberal" ~ 2L,
      ideology == "Moderate" ~ 3L,
      ideology == "Conservative" ~ 4L,
      ideology == "Very conservative" ~ 5L,
      TRUE ~ NA_integer_
    ),
    # urban_rural: 1=Big city, 2=Suburban area, 3=Smaller city, 4=Small town, 5=Rural area
    # numeric-coded rows from earlier waves have unverifiable mapping → NA
    urban_rural = case_when(
      urban_rural == "Big city" ~ 1L,
      urban_rural == "Suburban area" ~ 2L,
      urban_rural == "Smaller city" ~ 3L,
      urban_rural == "Small town" ~ 4L,
      urban_rural == "Rural area" ~ 5L,
      TRUE ~ NA_integer_
    ),
    # voted_for_2016: numeric-coded rows from earlier waves have unverifiable mapping → NA
    voted_for_2016 = case_when(
      voted_for_2016 == "Hillary Clinton" ~ 1L,
      voted_for_2016 == "Donald Trump" ~ 2L,
      voted_for_2016 == "Gary Johnson" ~ 3L,
      voted_for_2016 == "Jill Stein" ~ 4L,
      voted_for_2016 == "Evan McMullin" ~ 5L,
      voted_for_2016 == "Other" ~ 6L,
      voted_for_2016 == "Did not vote for President" ~ 7L,
      TRUE ~ NA_integer_
    ),
    # voted_for_2020: numeric-coded rows from earlier waves have unverifiable mapping → NA
    voted_for_2020 = case_when(
      voted_for_2020 == "Joe Biden" ~ 1L,
      voted_for_2020 == "Donald Trump" ~ 2L,
      voted_for_2020 == "Jo Jorgensen" ~ 3L,
      voted_for_2020 == "Howie Hawkins" ~ 4L,
      voted_for_2020 == "Other" ~ 5L,
      voted_for_2020 == "Did not vote for President" ~ 6L,
      TRUE ~ NA_integer_
    ),
    # voted_for_2024: numeric-coded rows from earlier waves have unverifiable mapping → NA
    voted_for_2024 = case_when(
      voted_for_2024 == "Kamala Harris" ~ 1L,
      voted_for_2024 == "Donald Trump" ~ 2L,
      voted_for_2024 == "Chase Oliver" ~ 3L,
      voted_for_2024 == "Cornel West" ~ 4L,
      voted_for_2024 == "Jill Stein" ~ 5L,
      voted_for_2024 == "Robert F. Kennedy, Jr." ~ 6L,
      voted_for_2024 == "Other" ~ 7L,
      voted_for_2024 == "Did not vote for President" ~ 8L,
      TRUE ~ NA_integer_
    ), 
    in_power = case_when(
      party_summary == "Democrat" & survey_wave < 123 ~ 1L, # Democrats in power before Jan 2025,
      party_summary == "Republican" & survey_wave > 123 ~ 1L, # Republicans in power after Jan 2025,
      party_summary == "Democrat" & survey_wave > 123 ~ 0L, # Democrats out of power after Jan 2025,
      party_summary == "Republican" & survey_wave < 123 ~ 0L, # Republicans out of power before Jan 2025,
      TRUE ~ NA_integer_
    )
  )


# Create a coarse treatment variable
df_unstandardized <- df_unstandardized %>% 
  mutate(
    treatment = ifelse(year_of_birth > 1994, 1, 0),
    age_2012_election = 2012 - year_of_birth, 
    years_from_cutoff = year_of_birth - 1994
  )


#------------------------------------------------------------------------------#


# Create index

## Items used
liberal_items <- c(
  "norm_judges",
  "norm_polling",
  "norm_executive",
  "norm_censorship",
  "norm_loyalty"
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

### N of respondents with complete data on all 5 index items (used in FA and alpha)
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
    "Raw (unstandardized) Cronbach's alpha across 5 PRL items",
    "Standardized Cronbach's alpha across 5 PRL items",
    "Number of items in the index",
    "Respondents with non-missing data on all 5 items (listwise complete cases used in factor analysis)"
  )
)
write.csv(alpha_summary, paste0(loc_results, "/Appendix/Index/fa_alpha_summary.csv"), row.names = FALSE)


## Extract factors and view the values

### First check
fa_liberal <- fa(
  df[, liberal_items],
  nfactors = 1, rotate = "none", fm = "pa", max.iter = 100
)
fa_liberal$loadings

fa_diagnostics <- data.frame(
  item         = liberal_items,
  item_label   = c(
    "Respect judicial decisions (norm_judges): agree = undemocratic",
    "Voter access / polling restrictions (norm_polling): agree = undemocratic",
    "Executive orders bypassing Congress (norm_executive): agree = undemocratic",
    "Government censorship of media (norm_censorship): agree = undemocratic",
    "Respecting election results (norm_loyalty): agree = undemocratic"
  ),
  prl_variable = liberal_items,
  loading      = round(as.numeric(fa_liberal$loadings[, 1]), 3),
  communality  = round(fa_liberal$communality, 3),
  uniqueness   = round(fa_liberal$uniquenesses, 3),
  item_total_r = round(alpha_result$item.stats$r.drop, 3),
  N_nonmissing = sapply(liberal_items, function(v) sum(!is.na(df_unstandardized[[v]])))
)
write.csv(fa_diagnostics, paste0(loc_results, "/Appendix/Index/fa_disagnostics.csv"), row.names = FALSE)


### Then save (N_respondents added; linked to screeplot for ICPSR chart-table requirement)
fa_eigenvalues <- data.frame(
  factor              = seq_along(fa_liberal$values),
  eigenvalue          = round(fa_liberal$values, 3),
  variance_explained  = round(fa_liberal$values / sum(fa_liberal$values), 3),
  cumulative_variance = round(cumsum(fa_liberal$values / sum(fa_liberal$values)), 3),
  N_respondents       = n_fa
)
write.csv(fa_eigenvalues, paste0(loc_results, "/Appendix/Index/fa_eigenvalues.csv"), row.names = FALSE)


## Screeplot
screeplot_liberal <- get_screeplot(fa_liberal, "Liberalism Index")
print(screeplot_liberal)

ggsave(
  paste0(loc_results, "/Appendix/Index/screeplot_liberal.png"),
  plot = screeplot_liberal, width = 5, height = 3.5, units = "in", dpi = 300
)


## Create index (note to reader and to self: higher values = higher support for liberalism)
fa_liberal_df <- as.data.frame(fa_liberal$scores)

df <- df %>%
  mutate(
    liberal_index = scales::rescale(-as.numeric(fa_liberal$scores), to = c(0, 1))
  )

####################################
########### Data Analysis ###########
####################################

## Define partisan subsets

df_democrats    <- df %>% filter(party_summary == "Democrat")
df_republicans  <- df %>% filter(party_summary == "Republican")
df_partisans    <- df %>% filter(party_summary %in% c("Democrat", "Republican"))
df_independents <- df %>% filter(party_summary == "Independent")
df_inpower <- df %>% filter(in_power == 1)
df_outofpower <- df %>% filter(in_power == 0)




## Get descriptive RD plots for H1:H3

### With geom_point()
discontinuityplot_parties <- get_discontinuityplot(df)

print(discontinuityplot_parties$plot_all)
print(discontinuityplot_parties$plot_subgroups)

ggsave(
  paste0(loc_results, "/Main/Plots/Discontinuity/discontinuityplot_full.png"),
  plot = discontinuityplot_parties$plot_all, width = 6.5, height = 5, units = "in", dpi = 300
)
ggsave(
  paste0(loc_results, "/Main/Plots/Discontinuity/discontinuityplot_subgroups.png"),
  plot = discontinuityplot_parties$plot_subgroups, width = 6.5, height = 5.5, units = "in", dpi = 300
)


### Without geom_point()
discontinuityplot_parties_noscatter <- get_discontinuityplot_noscatter(df)

print(discontinuityplot_parties_noscatter$plot_all)
print(discontinuityplot_parties_noscatter$plot_subgroups)

ggsave(
  paste0(loc_results, "/Main/Plots/Discontinuity/discontinuityplot_full.png"),
  plot = discontinuityplot_parties_noscatter$plot_all, width = 6.5, height = 5, units = "in", dpi = 300
)
ggsave(
  paste0(loc_results, "/Main/Plots/Discontinuity/discontinuityplot_subgroups.png"),
  plot = discontinuityplot_parties_noscatter$plot_subgroups, width = 6.5, height = 5.5, units = "in", dpi = 300
)



## Get descriptive RD plots for H4

### With geom_point()
discontinuityplot_power <- get_discontinuityplot(df, party_id = "in_power", colnumber = 1)

print(discontinuityplot_power$plot_subgroups)

ggsave(
  paste0(loc_results, "/Main/Plots/Discontinuity/discontinuityplot_power.png"),
  plot = discontinuityplot_power$plot_subgroups, width = 6.5, height = 7.5, units = "in", dpi = 300
)

### With geom_point()
discontinuityplot_power_noscatter <- get_discontinuityplot_noscatter(df, party_id = "in_power", colnumber = 1)

print(discontinuityplot_power_noscatter$plot_subgroups)

ggsave(
  paste0(loc_results, "/Main/Plots/Discontinuity/discontinuityplot_power_noscatter.png"),
  plot = discontinuityplot_power_noscatter$plot_subgroups, width = 6.5, height = 7.5, units = "in", dpi = 300
)


## Define control variables

### Group of control variables
potential_controls <- c("education", "sex", "income", "race")

### Group of covariates that are imbalanced between the groups
imbalanced_controls <- get_imbalanced_controls(df, potential_controls)

### Race is a factor variable: make race dummies, with White as the baseline category
df              <- fastDummies::dummy_cols(df,              "race", remove_first_dummy = TRUE, ignore_na = TRUE)
df_democrats    <- fastDummies::dummy_cols(df_democrats,    "race", remove_first_dummy = TRUE, ignore_na = TRUE)
df_republicans  <- fastDummies::dummy_cols(df_republicans,  "race", remove_first_dummy = TRUE, ignore_na = TRUE)
df_independents <- fastDummies::dummy_cols(df_independents, "race", remove_first_dummy = TRUE, ignore_na = TRUE)
df_partisans    <- fastDummies::dummy_cols(df_partisans,    "race", remove_first_dummy = TRUE, ignore_na = TRUE)
df_inpower  <- fastDummies::dummy_cols(df_inpower,  "race", remove_first_dummy = TRUE, ignore_na = TRUE)
df_outofpower <- fastDummies::dummy_cols(df_outofpower, "race", remove_first_dummy = TRUE, ignore_na = TRUE)

### Create final group of controls
### (PRL race factor levels: Black, Hispanic, Asian, Native American, Middle Eastern, Two or more races, Other)
race_dummies <- c(
  "race_Black", "race_Hispanic", "race_Asian",
  "race_Native American", "race_Middle Eastern",
  "race_Two or more races", "race_Other"
)
controls <- c(setdiff(imbalanced_controls, "race"), race_dummies)



#-----------------------------------------------------------------------------

# Run the models

## H1
rdd_liberal_full <- run_rdd_models_coarse (
  data         = df,
  index_var    = "liberal_index",
  controls     = controls,
  sample_label = "Full Sample"
)

## H2
rdd_liberal_independents <- run_rdd_models_coarse(
  data         = df_independents,
  index_var    = "liberal_index",
  controls     = controls,
  sample_label = "Independents"
)

## H2 
rdd_liberal_partisans <- run_rdd_models_coarse(
  data         = df_partisans,
  index_var    = "liberal_index",
  controls     = controls,
  sample_label = "Partisans"
)

## H3 
rdd_liberal_democrats <- run_rdd_models_coarse(
  data         = df_democrats,
  index_var    = "liberal_index",
  controls     = c("sex", "education", "income"),
  sample_label = "Democrats"
)

## H3 
rdd_liberal_republicans <- run_rdd_models_coarse(
  data         = df_republicans,
  index_var    = "liberal_index",
  controls     = c("sex", "education", "income"),
  sample_label = "Republicans"
)

## H4 
rdd_liberal_inpower <- run_rdd_models_coarse(
  data         = df_inpower,
  index_var    = "liberal_index",
  controls     = c("sex", "education", "income"),
  sample_label = "In-Power"
)

## H4 
rdd_liberal_outofpower <- run_rdd_models_coarse(
  data         = df_outofpower,
  index_var    = "liberal_index",
  controls     = c("sex", "education", "income"),
  sample_label = "Out-of-Power"
)



## Combine all together into the same frame
rdd_liberal <- bind_rows(
  rdd_liberal_full, rdd_liberal_independents, rdd_liberal_partisans,
  rdd_liberal_democrats, rdd_liberal_republicans, rdd_liberal_inpower, rdd_liberal_outofpower
) %>%
  mutate(
    Sample = factor(
      Sample,
      levels = c("Republicans", "Democrats", "Partisans", "Independents", "Full Sample", "In-Power", "Out-of-Power")
    )
  )

write.csv(rdd_liberal, paste0(loc_results, "/Main/rdd_liberal_coarserv.csv"))



#-----------------------------------------------------------------------------

# Create coefficient plots for the coarse models

## First, for the main stuff, not the robustness checks

### Keep the relevant estimate types
rdd_liberal_for_simpleplot <- rdd_liberal %>%
  filter(`Estimate Type` == "Bias-Corrected") %>% 
  mutate(
    Model = factor(Model, 
                   levels = c("Without Controls", "With Controls")
    ),
    Sample = factor(Sample,
                    levels = c("Out-of-Power", "In-Power", "Republicans", "Democrats", "Partisans", "Independents", "Full Sample")
    ), 
    Outcome = case_when(
      Outcome == "Liberal Democratic Norms Index: 1-factor principal-axis score of 5 PRL items (norm_judges, norm_polling, norm_executive, norm_censorship, norm_loyalty), rescaled to [0, 1]; higher = stronger support for liberal democratic norms" ~ "Liberal Norm Support Index (0 – 1)",
      TRUE ~ Outcome
    )
  )

### Create and save plot
coefplot_liberal <- get_coefplot(rdd_liberal_for_simpleplot, 1)
print(coefplot_liberal)
ggsave(
  paste0(loc_results, "/Main/Plots/Coefplot/coefplot_coarse_.png"),
  plot = screeplot_liberal, width = 5, height = 3.5, units = "in", dpi = 300
)



## Now the robustness checks

### Prepare dataset
rdd_liberal_for_robustnessplot <- rdd_liberal %>%
  mutate(
    `Estimate Type` = factor(`Estimate Type`,
                             levels = c("Conventional", "Bias-Corrected", "Bias-Corrected (Robust SE)")
    ),
    Sample = factor(Sample,
                    levels = c("Out-of-Power", "In-Power", "Republicans", "Democrats", "Partisans", "Independents", "Full Sample")
    )
  )

get_coefplot_robustness(rdd_liberal_for_robustnessplot)



### Create and save plot
coefplot_liberal <- get_coefplot(rdd_liberal_for_simpleplot, 1)
print(coefplot_liberal)
ggsave(
  paste0(loc_results, "/Main/Plots/Coefplot/coefplot_coarse_.png"),
  plot = screeplot_liberal, width = 5, height = 3.5, units = "in", dpi = 300
)


#------------------------------------------------------------------------------#


# Descriptive statistics

## Variable labels 
var_labels <- c(
  age_2012_election        = "Age in years at the November 2012 presidential election (approximation: 2012 - year_of_birth; PRL provides birth year only)",
  sex                   = "Sex: binary indicator (1 = Male, 0 = Female; recoded from PRL gender variable)",
  education                = "Education level (PRL educ; ordinal 1-6; 1 = No high school, 6 = Post-graduate)",
  income                   = "Household income (PRL faminc_new; ordinal 1-16; 1 = Less than $10,000, 16 = $500,000 or more)",
  `race_Black`             = "Race dummy: Black / African-American (1 = Yes, 0 = No; reference category = White; derived from PRL race variable)",
  `race_Hispanic`          = "Race dummy: Hispanic / Latino (1 = Yes, 0 = No; reference = White)",
  `race_Asian`             = "Race dummy: Asian / Asian-American (1 = Yes, 0 = No; reference = White)",
  `race_Native American`   = "Race dummy: Native American / Alaskan Native (1 = Yes, 0 = No; reference = White)",
  `race_Middle Eastern`    = "Race dummy: Middle Eastern (1 = Yes, 0 = No; reference = White)",
  `race_Two or more races` = "Race dummy: Two or more races (1 = Yes, 0 = No; reference = White)",
  `race_Other`             = "Race dummy: Other race (1 = Yes, 0 = No; reference = White)",
  liberal_index            = "Liberal Democratic Norms Index: 1-factor principal-axis score of 5 PRL items (norm_judges, norm_polling, norm_executive, norm_censorship, norm_loyalty), rescaled to [0, 1]; higher = stronger support for liberal democratic norms; unweighted"
)

## Plain-English descriptions of each sample / subgroup
sample_descriptions <- c(
  "Full Sample"   = "All PRL respondents with non-missing outcome and running variable; pooled across 160+ weekly survey waves (2022-2026)",
  "Independents"  = "Respondents self-identifying as Independent on the 7-point party ID scale (pid7 codes 3-5, including leaners); excludes pure Democrats and Republicans",
  "Partisans"     = "Respondents self-identifying as Democrat or Republican (pid7 codes 1-2 or 6-7); Independents (codes 3-5) excluded",
  "Democrats"     = "Respondents self-identifying as Democrat or leaning Democrat (pid7 codes 1-2)",
  "Republicans"   = "Respondents self-identifying as Republican or leaning Republican (pid7 codes 6-7)",
  "In Power"  = "Partisan respondents whose party held the presidency at the time of the survey (Democrat: waves before 123; Republican: waves after 123; wave 123 excluded as presidential transition period)",
  "Out of Power" = "Partisan respondents whose party did not hold the presidency at the time of the survey (Democrat: waves after 123; Republican: waves before 123; wave 123 excluded as presidential transition period)"
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
  "race_Black", "race_Hispanic", "race_Asian",
  "race_Native American", "race_Middle Eastern",
  "race_Two or more races", "race_Other"
)

## Function: computes descriptive statistics by treatment group for each variable.
get_descriptives <- function(data, sample_label, vars = desc_vars_main) {
  map_dfr(vars, function(var) {
    x   <- data[[var]]
    trt <- data$treatment

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
      `Mean (Control)`        = round(mean(x_ctrl),  4),
      `SD (Control)`          = round(sd(x_ctrl),    4),
      `N (Control)`           = length(x_ctrl),
      `Min (Control)`         = if (sum(x_ctrl == min(x_ctrl)) < 10) NA_real_ else min(x_ctrl),
      `N at Min (Control)`    = if (sum(x_ctrl == min(x_ctrl)) < 10) NA_integer_ else sum(x_ctrl == min(x_ctrl)),
      `Max (Control)`         = if (sum(x_ctrl == max(x_ctrl)) < 10) NA_real_ else max(x_ctrl),
      `N at Max (Control)`    = if (sum(x_ctrl == max(x_ctrl)) < 10) NA_integer_ else sum(x_ctrl == max(x_ctrl)),
      `Count_0 (Control)`     = if (is_binary) sum(x_ctrl == 0) else NA_integer_,
      `Count_1 (Control)`     = if (is_binary) sum(x_ctrl == 1) else NA_integer_,
      `Mean (Treatment)`      = round(mean(x_treat), 4),
      `SD (Treatment)`        = round(sd(x_treat),   4),
      `N (Treatment)`         = length(x_treat),
      `Min (Treatment)`       = if (sum(x_treat == min(x_treat)) < 10) NA_real_ else min(x_treat),
      `N at Min (Treatment)`  = if (sum(x_treat == min(x_treat)) < 10) NA_integer_ else sum(x_treat == min(x_treat)),
      `Max (Treatment)`       = if (sum(x_treat == max(x_treat)) < 10) NA_real_ else max(x_treat),
      `N at Max (Treatment)`  = if (sum(x_treat == max(x_treat)) < 10) NA_integer_ else sum(x_treat == max(x_treat)),
      `Count_0 (Treatment)`   = if (is_binary) sum(x_treat == 0) else NA_integer_,
      `Count_1 (Treatment)`   = if (is_binary) sum(x_treat == 1) else NA_integer_,
      `P-Value (t-test)`      = round(pval, 4),
      Control_Group_Label     = "Born on or before November 6, 1994 (treatment = 0): eligible to vote in the 2012 presidential election",
      Treatment_Group_Label   = "Born after November 6, 1994 (treatment = 1): first eligible presidential election was 2016"
    )
  })
}

## Table 1: main demographic variables + outcome
descriptives_main <- bind_rows(
  get_descriptives(df,               "Full Sample",   vars = desc_vars_main),
  get_descriptives(df_independents,  "Independents",  vars = desc_vars_main),
  get_descriptives(df_partisans,     "Partisans",     vars = desc_vars_main),
  get_descriptives(df_democrats,     "Democrats",     vars = desc_vars_main),
  get_descriptives(df_republicans,   "Republicans",   vars = desc_vars_main),
  get_descriptives(df_inpower,   "In-Power",  vars = desc_vars_main),
  get_descriptives(df_outofpower,  "Out-of_Power", vars = desc_vars_main)
)

write.csv(
  descriptives_main,
  paste0(loc_results, "/Appendix/descriptives_main.csv"),
  row.names = FALSE
)

## Add the following note manually: 
# In the following columns (N at Min (Control), Min(Control), N at Max (Control), Max(Control)), NA means that fewer than 10 observations were at that specific value

## Table 2: race dummies
descriptives_race <- bind_rows(
  get_descriptives(df,               "Full Sample",   vars = desc_vars_race),
  get_descriptives(df_independents,  "Independents",  vars = desc_vars_race),
  get_descriptives(df_partisans,     "Partisans",     vars = desc_vars_race),
  get_descriptives(df_democrats,     "Democrats",     vars = desc_vars_race),
  get_descriptives(df_republicans,   "Republicans",   vars = desc_vars_race),
  get_descriptives(df_inpower,   "In-Power",  vars = desc_vars_race),
  get_descriptives(df_outofpower,  "Out-of_Power", vars = desc_vars_race)
)

write.csv(
  descriptives_race,
  paste0(loc_results, "/Appendix/descriptives_race.csv"),
  row.names = FALSE
)



#------------------------------------------------------------------------------#

# 
# # Robustness checks #
# 
# ## Define subgroups for robustness checks
# robustness_subgroups <- list(
#   list(data = df, label = "Full Sample"),
#   list(data = df_independents, label = "Independents"),
#   list(data = df_partisans, label = "Partisans"),
#   list(data = df_democrats, label = "Democrats"),
#   list(data = df_republicans, label = "Republicans"),
#   list(data = df_inpower, label = "In-Power"),
#   list(data = df_outofpower, label = "Out-of-Power")
# )
# 
# ## Run the robustness checks and combine them into one data frame
# all_robustness <- map_dfr(robustness_subgroups, function(sg) {
#   message("Robustness checks: ", sg$label)
#   run_robustness_checks(
#     data         = sg$data,
#     index_var    = "liberal_index",
#     controls     = controls,
#     sample_label = sg$label
#   )
# })
# 
# write.csv(all_robustness, paste0(loc_results, "/Appendix/Robustness Checks/all_robustness_checks.csv"), row.names = FALSE)



#------------------------------------------------------------------------------#

# Monte Carlo Simulation

## Define combinations again

### Define a vector of the sub-group data frams
subgroup_dfs <- c(
  "df", "df_independents", "df_partisans",
  "df_democrats", "df_republicans",
  "df_inpower", "df_outofpower"
)


### Set out the possibel combinations
combinations_mc <- expand.grid(
  subset_df = subgroup_dfs,
  controls = list(controls, NULL),
  stringsAsFactors = FALSE
)

## Run the simulation for the combinations
bootstrap_results <- map(seq_len(nrow(combinations_mc)), function(i) {
  df_name  <- combinations_mc$subset_df[i]
  controls <- combinations_mc$controls[[i]]
  
  run_mc_rdd_bootstrap(
    data         = get(df_name),
    controls     = controls,
    sample_label = df_name,
    n_sim        = 1000,
    n_boot       = 500
  )
})

# Extract the three components separately
boot_summary    <- map_dfr(bootstrap_results, "summary")
boot_draws      <- map_dfr(bootstrap_results, "outer_draws")   # for MC histogram
boot_null       <- map_dfr(bootstrap_results, "null_estimates") # for null plot


# MC histogram
plot_mc_histogram_boot(boot_draws)
ggsave(paste0(loc_results, "/Monte Carlo/mc_histogram_bootstrap.png"),
       width = 12, height = 8, dpi = 300)

# Null distribution
plot_null_distribution(boot_null, boot_summary)
ggsave(paste0(loc_results, "/Monte Carlo/null_distribution.png"),
       width = 12, height = 8, dpi = 300)




# Single-combo test — full sample with controls (your slowest one)
test_run <- run_mc_rdd_bootstrap(
  data         = df,
  controls     = controls,
  sample_label = "df",
  n_sim        = 1000,
  n_boot       = 500,
  n_cores      = parallel::detectCores() - 1,   # use all but one core
  run_diagnostic = TRUE
)

# Inspect
test_run$summary
test_run$diagnostic    # ← this is what tells you if the operator is OK

