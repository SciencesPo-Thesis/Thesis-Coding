#########################################################################
#########################################################################
############# Replication Code: "Coming of Age Under Trump" #############
########### The Effect of First Electoral Exposure in a Trump ###########
################# Election on Long-Term Liberal Attitudes #################
#########################################################################
#########################################################################

## Author: Nikolaos VICHOS


### Set-Up
library(styler)
library(psych)
library(dplyr)
library(tidyverse)
library(scales)
library(rdrobust)
library(modelsummary)
library(ggthemes)
library(patchwork)
library(tibble)
library(knitr)
library(ggtext)

#Location
location <- "/Users/nikolaosvichos/Library/Mobile Documents/com~apple~CloudDocs/Sciences Po/Thesis/"

# Get functions
source(paste0(location, "/Thesis-Github/Thesis-Coding/Functions/functions.R"))

# Import data
df_unclean <- haven::read_dta(paste0(location, "/Datasets/USA/usa_data.dta"))


##############################################################################################################
############################################## Data Management ###############################################
##############################################################################################################


df_unstandardized <- df_unclean %>% # outcome variables to remember: pre(V201366, V201376, V201377, V201375x); post(V202312, V202175)
  dplyr::select(
    V201507x, V201231x, V200003,
    V201156, V201157, V201511x,
    V201600, V202468x, V201510,
    V201104, V201101, V201429,
    V202109x, V201228, V201225x,
    V201366, V201367, V201368,
    V201369, V201376, V201375x,
    V201377, V201378, V201372x,
    V201379, V201233, V201234,
    V201236, V202431, V202212,
    V202213, V202304, V202305,
    V202308x, V202311, V202312,
    V202432, V202409, V202410,
    V202411, V202412, V202413,
    V202414, V202415, V202416,
    V202417, V202431, V202432,
    V202440, V202439, V202073,
    V201103, V201105, V202265,
    V202266, V202267, V202268,
    V202269, V202290x, V202270,
    V202273x, V202416, V202419,
    V202175, V200004
  ) %>%
  filter(V200003 != 2) %>%
  filter(V200004 == 3) %>%
  rename(
    "age" = "V201507x",
    "party" = "V201228",
    "party_strength" = "V201231x",
    "panel_data" = "V200003",
    "pre_or_post" = "V200004",
    "feeling_dems" = "V201156",
    "feeling_reps" = "V201157",
    "feeling_journalists" = "V202175",
    "education" = "V201510",
    "education_summary" = "V201511x",
    "gender" = "V201600",
    "income" = "V202468x",
    "duty_or_choice" = "V201225x",
    "voted2012" = "V201104",
    "voted2016" = "V201101",
    "voted2020" = "V202109x",
    "free_press" = "V201366",
    "checks_and_balances" = "V201367",
    "rule_of_law" = "V201368",
    "agree_on_facts" = "V201369",
    "media_undermined_concern" = "V201376",
    "journalist_access" = "V201375x",
    "media_trust" = "V201377",
    "foreign_help" = "V201378",
    "unitary_executive" = "V201372x",
    "govt_principled" = "V201379",
    "govt_trust" = "V201233",
    "govt_capture" = "V201234",
    "govt_corruption" = "V201236",
    "difference_inpower" = "V202431",
    "difference_vote" = "V202432",
    "compromise_politics" = "V202409",
    "politicians_notcare" = "V202410",
    "politicians_trustworthy" = "V202411",
    "politicians_mainproblem" = "V202412",
    "strong_leader" = "V202413",
    "people_power" = "V202414",
    "politicians_carerich" = "V202415",
    "democracy_satisfaction" = "V202440",
    "urban_unrest" = "V201429",
    "officials_dont_care" = "V202212",
    "no_say_in_govt" = "V202213",
    "polsystem_for_insiders" = "V202304",
    "rich_and_powerful" = "V202305",
    "people_or_experts" = "V202308x",
    "few_powerful_control" = "V202311",
    "powerful_indoctrination" = "V202312",
    "left_right" = "V202439",
    "minorities_should_adapt" = "V202416",
    "will_of_majority" = "V202417",
    "traditional_family_values" = "V202265",
    "child_independent_or_respect" = "V202266",
    "child_curious_or_wellmannered" = "V202267",
    "child_obedient_or_selfreliant" = "V202268",
    "child_considerate_or_wellbehaved" = "V202269",
    "stay_at_home_wife" = "V202290x",
    "world_like_america" = "V202270",
    "america_better" = "V202273x",
    "immigrants_harm_culture" = "V202419",
    "voted_for_2020" = "V202073",
    "voted_for_2016" = "V201103",
    "voted_for_2012" = "V201105"
  ) %>%
  mutate(
    age = case_when(
      age == -9 ~ NA_integer_,
      TRUE ~ age
    ),
    party = case_when(
      party %in% c(-9, -8, -4, 0, 5) ~ NA_integer_,
      TRUE ~ party
    ),
    party_binary = case_when(
      party == 3 ~ NA_integer_,
      TRUE ~ party
    ),
    party_strength = case_when(
      party_strength %in% c(-9, -8) ~ NA_integer_,
      TRUE ~ party_strength
    ),
    feeling_dems = case_when(
      feeling_dems %in% c(-9, 998) ~ NA_integer_,
      TRUE ~ feeling_dems
    ),
    feeling_reps = case_when(
      feeling_reps %in% c(-9, 998) ~ NA_integer_,
      TRUE ~ feeling_reps
    ),
    feeling_journalists = case_when(
      feeling_reps %in% c(-9, -7, -6, -5, -4, 998, 999) ~ NA_integer_,
      TRUE ~ feeling_reps
    ),
    education = case_when(
      education %in% c(-9, -8, 95) ~ NA_integer_,
      TRUE ~ education
    ),
    education_summary = case_when(
      education_summary %in% c(-9, -8, -2) ~ NA_integer_,
      TRUE ~ education_summary
    ),
    gender = case_when(
      gender == -9 ~ NA_integer_,
      gender == 2 ~ 0, # change female (previously 2) to 0
      TRUE ~ gender
    ),
    income = case_when(
      income %in% c(-9, -5) ~ NA_integer_,
      TRUE ~ income
    ),
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
    voted2020 = case_when(
      voted2020 == -2 ~ NA_integer_,
      TRUE ~ voted2020
    ),
    eligible_2012 = case_when(
      age >= 26 ~ 1,
      TRUE ~ 0
    ),
    free_press = case_when(
      free_press %in% c(-9, -8) ~ NA_integer_,
      TRUE ~ free_press
    ),
    checks_and_balances = case_when(
      checks_and_balances %in% c(-9, -8) ~ NA_integer_,
      TRUE ~ checks_and_balances
    ),
    rule_of_law = case_when(
      rule_of_law %in% c(-9, -8) ~ NA_integer_,
      TRUE ~ rule_of_law
    ),
    agree_on_facts = case_when(
      agree_on_facts %in% c(-9, -8) ~ NA_integer_,
      TRUE ~ agree_on_facts
    ),
    unitary_executive = case_when(
      unitary_executive == -2 ~ NA_integer_,
      TRUE ~ unitary_executive
    ),
    journalist_access = case_when(
      journalist_access == -2 ~ NA_integer_,
      TRUE ~ journalist_access
    ),
    media_undermined_concern = case_when(
      media_undermined_concern %in% c(-9, -8) ~ NA_integer_,
      TRUE ~ media_undermined_concern
    ),
    media_trust = case_when(
      media_trust %in% c(-9, -8) ~ NA_integer_,
      TRUE ~ media_trust
    ),
    foreign_help = case_when(
      foreign_help %in% c(-9, -8) ~ NA_integer_,
      TRUE ~ foreign_help
    ),
    govt_principled = case_when(
      govt_principled %in% c(-9, -8) ~ NA_integer_,
      TRUE ~ govt_principled
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
    difference_inpower = case_when(
      difference_inpower %in% c(-9, -7, -6, -5) ~ NA_integer_,
      TRUE ~ difference_inpower
    ),
    difference_vote = case_when(
      difference_vote %in% c(-9, -7, -6, -5) ~ NA_integer_,
      TRUE ~ difference_vote
    ),
    compromise_politics = case_when(
      compromise_politics %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ compromise_politics
    ),
    politicians_notcare = case_when(
      politicians_notcare %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ politicians_notcare
    ),
    politicians_trustworthy = case_when(
      politicians_trustworthy %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ 6 - politicians_trustworthy
    ),
    politicians_mainproblem = case_when(
      politicians_mainproblem %in% c(-9, -7, -6, -5) ~ NA_integer_,
      TRUE ~ politicians_mainproblem
    ),
    strong_leader = case_when(
      strong_leader %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ strong_leader
    ),
    people_power = case_when(
      people_power %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ people_power
    ),
    politicians_carerich = case_when(
      politicians_carerich %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ politicians_carerich
    ),
    democracy_satisfaction = case_when(
      democracy_satisfaction %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ 6 - democracy_satisfaction
    ),
    left_right = case_when(
      left_right %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ left_right
    ),
    voted_for_2020 = case_when(
      voted_for_2020 %in% c(-9, -8, -7, -6, -1, 5, 11, 12) ~ NA_integer_,
      TRUE ~ voted_for_2020
    ),
    voted_for_2016 = case_when(
      voted_for_2016 %in% c(-9, -8, -1, 5) ~ NA_integer_,
      TRUE ~ voted_for_2016
    ),
    voted_for_2012 = case_when(
      voted_for_2012 %in% c(-9, -8, -1, 5) ~ NA_integer_,
      TRUE ~ voted_for_2016
    ),
    urban_unrest = case_when(
      urban_unrest %in% c(-9, -8, 99) ~ NA_integer_,
      TRUE ~ 8 - urban_unrest
    ),
    officials_dont_care = case_when(
      officials_dont_care %in% c(-9, -7, -6, -5, -4) ~ NA_integer_,
      TRUE ~ officials_dont_care
    ),
    no_say_in_govt = case_when(
      no_say_in_govt %in% c(-9, -7, -6, -5) ~ NA_integer_,
      TRUE ~ no_say_in_govt
    ),
    polsystem_for_insiders = case_when(
      polsystem_for_insiders %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ 6 - polsystem_for_insiders
    ),
    rich_and_powerful = case_when(
      rich_and_powerful %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ 6 - rich_and_powerful
    ),
    people_or_experts = case_when(
      people_or_experts %in% c(-7, -6, -5, -2) ~ NA_integer_,
      TRUE ~ people_or_experts
    ),
    few_powerful_control = case_when(
      few_powerful_control %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ 6 - few_powerful_control
    ),
    powerful_indoctrination = case_when(
      powerful_indoctrination %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ 6 - powerful_indoctrination
    ),
    minorities_should_adapt = case_when(
      minorities_should_adapt %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ minorities_should_adapt
    ),
    will_of_majority = case_when(
      will_of_majority %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ will_of_majority
    ),
    traditional_family_values = case_when(
      traditional_family_values %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ traditional_family_values
    ),
    child_independent_or_respect = case_when(
      child_independent_or_respect %in% c(-9, -7, -6, -5, 3) ~ NA_integer_,
      child_independent_or_respect == 2 ~ 0,
      TRUE ~ child_independent_or_respect
    ),
    child_curious_or_wellmannered = case_when(
      child_curious_or_wellmannered %in% c(-9, -7, -6, -5, 3) ~ NA_integer_,
      child_curious_or_wellmannered == 2 ~ 0,
      TRUE ~ child_curious_or_wellmannered
    ),
    child_curious_or_wellmannered = case_when(
      child_curious_or_wellmannered %in% c(-9, -7, -6, -5, 3) ~ NA_integer_,
      child_curious_or_wellmannered == 2 ~ 0,
      TRUE ~ child_curious_or_wellmannered
    ),
    child_obedient_or_selfreliant = case_when(
      child_obedient_or_selfreliant %in% c(-9, -7, -6, -5, 3, 4) ~ NA_integer_,
      child_obedient_or_selfreliant == 1 ~ 0,
      child_obedient_or_selfreliant == 2 ~ 1
    ),
    child_considerate_or_wellbehaved = case_when(
      child_considerate_or_wellbehaved %in% c(-9, -7, -6, -5, 3) ~ NA_integer_,
      child_considerate_or_wellbehaved == 2 ~ 0,
      TRUE ~ child_considerate_or_wellbehaved
    ),
    stay_at_home_wife = case_when(
      stay_at_home_wife %in% c(-7, -6, -5, -2) ~ NA_integer_,
      TRUE ~ stay_at_home_wife
    ),
    world_like_america = case_when(
      world_like_america %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ world_like_america
    ),
    america_better = case_when(
      america_better %in% c(-7, -6, -5, -2) ~ NA_integer_,
      TRUE ~ america_better
    ),
    immigrants_harm_culture = case_when(
      immigrants_harm_culture %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ immigrants_harm_culture
    )
  )

# create treatment variable
df_unstandardized$treatment <- ifelse(df_unstandardized$age >= 26, 0, 1)

# create a copy, that will be the standardized version of the dataset for the DVs
df <- df_unstandardized

# standardize the relevant items used to construct the index and the potential demographic variables

liberal_items <- c( # select the relevant items
  "free_press", "checks_and_balances", "rule_of_law", 
  "agree_on_facts", "journalist_access", "media_undermined_concern"
)


df[, liberal_items] <- scale(df[, liberal_items]) # standardize them


###################################################################################################################
############################################### Constructing Indices################################################
###################################################################################################################


##### Constructing Liberal Index #####

# check internal consistency
psych::alpha(df[, liberal_items])

# factor construction
fa_liberal <- fa(
  df[, liberal_items],
  nfactors = 1, rotate = "none", fm = "pa", max.iter = 100
)

# eigevalues
fa_liberal$values

# scree plot
screeplot_liberal <- get_screeplot(fa_liberal, "Liberal Attitudes")

# get factor loadings
print(fa_liberal$loadings, cutoff = 0.3)


# add as an index
fa_liberal_df <- as.data.frame(fa_liberal$scores)
df$liberal_index <- scales::rescale(fa_liberal_df[, 1], to = c(0, 1)) # 1st factor → 0–1 scale


###########################################################################################################
###############################################  Analysis #################################################
###########################################################################################################




####################################
########## Running Models ##########
####################################

# Define data subsets
df_independents <- df %>% # independents subset
  filter(party == 3)

df_partisans <- df %>% # partisans subset
  filter(party %in% c(1, 2))

df_democrats <- df %>% # democrats subset
  filter(party == 1)

df_republicans <- df %>% # republicans subset
  filter(party == 2)

# Define potential controls
potential_control_items <- c(
  "education", "gender", 
  "income", "left_right")


# Run the t-test for imbalances between treatment and control groups

# Initialize empty vector for controls to include
control_items <- c()

# Test each potential control
for (var in potential_control_items) {
  # Run t-test
  t_result <- t.test(df[[var]] ~ df$treatment)
  print(t_result)
  
  # Keep if p-value > 0.3 (not significantly different)
  # OR if you want imbalanced ones: p-value < 0.3
  if (t_result$p.value < 0.05) {  # imbalanced variables
    control_items <- c(control_items, var)
  }
}


# Liberal Attitude models
rdd_liberal_full <- run_rdd_models( # entire sample
  data = df,
  index_var = "liberal_index",
  controls = c("gender", "education", "income"),
  sample_label = "Full Sample"
)

rdd_liberal_independents <- run_rdd_models( # independents
  data = df_independents,
  index_var = "liberal_index",
  controls = c("gender", "education", "income"),
  sample_label = "Independents"
)

rdd_liberal_partisans <- run_rdd_models( # partisans
  data = df_partisans,
  index_var = "liberal_index",
  controls = c("gender", "education", "income"),
  sample_label = "Partisans"
)

rdd_liberal_democrats <- run_rdd_models( # democrats
  data = df_democrats,
  index_var = "liberal_index",
  controls = c("gender", "education", "income"),
  sample_label = "Democrats"
)

rdd_liberal_republicans <- run_rdd_models( # republicans
  data = df_republicans,
  index_var = "liberal_index",
  controls = c("gender", "education", "income"),
  sample_label = "Republicans"
)


# Aggregate estimates in dataframes
rdd_liberal <- bind_rows(rdd_liberal_full, rdd_liberal_independents, rdd_liberal_partisans, rdd_liberal_democrats, rdd_liberal_republicans) %>%
  mutate(Outcome = "Liberal Attitudes") %>%
  mutate(
    Sample = factor(
      Sample,
      levels = c("Republicans", "Democrats", "Partisans", "Independents", "Full Sample")
    )
  )


######################################################################################################################
############################################### Data Visualizations  #################################################
######################################################################################################################



#### Get coefficient plots for the different outcome variables ####
coefplot_liberal <- get_coefplot(rdd_liberal, 1)


#### Get discontinuity plots plots for the different subgroups and full group ####
# full sample
discontinuityplot <- get_discontinuityplot(df)


#### Display plots #### 
coefplot_liberal
discontinuityplot$plot_all
discontinuityplot$plot_subgroups





