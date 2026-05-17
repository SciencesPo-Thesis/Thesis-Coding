
# Note : The purpose of this documents is to create two types of tables: 
# - one that summarizes the Monte Carlo results in a format similar to rdd_liberal.csv
# - one that calculates the attenuation factor (lambda)


# Load librarires
library(here)
library(tidyverse)


# Load files
mc_results <- read.csv(here("ANES Restricted/Results/Monte Carlo/mc_results.csv"))

rdd_liberal <- read.csv(here("ANES Restricted/Results/Main/rdd_liberal.csv")) %>% 
  select(-c(X, N_left_cutoff, N_right_cutoff, N_left_bw, N_right_bw)) 

# Create and save Monte Carlo summary table
rdd_liberal_mc <- mc_results %>%
  group_by(sample, model) %>%
  summarize(
    Outcome = "Liberal Democratic Norms Index: 1-factor principal-axis score of 7 ANES items (V201366, V201367, V201368, V201369, V201372x, V201375x, V201376), rescaled to [0, 1]; higher = stronger support for liberal democratic norms",
    Running_Variable = "days_from_cutoff: days between respondent date of birth and November 6, 1994; negative = born before cutoff (Control group)",
    Cutoff = "November 6, 1994: born on/before this date = eligible to vote in 2012 presidential election (Control, treatment=0); born after = first eligible presidential election was 2016 (Treatment, treatment=1)",
    Weighted = "No — analyses are unweighted; ANES post-stratification weight V200010b available but not applied",
    Estimate.Type = "Monte Carlo",
    Estimate = mean(estimate),
    SE = sd(estimate),
    P.Value = 2 * (1 - pnorm(abs(mean(estimate) / sd(estimate)))),
    Bandwidth.Type = "MSE-optimal",
    Bandwidth..h. = round(mean(bw), 2),
    N_total = first(n_total),
    N_within_bw = as.integer(round(mean(n_used))),
    .groups = "drop"
  ) %>%
  rename(Sample = sample, Model = model) %>%
  mutate(
    Sample = factor(Sample),
    Controls_Used = case_when(
      Model == "With Controls" ~ "education; sex; income; race_Black; race_Hispanic; race_Asian; race_Native; race_Other",
      Model == "Without Controls" ~ "None"
    ),
    Reference_Category_Race = case_when(
      Model == "With Controls" ~ "White (omitted baseline; dummies included: race_Black, race_Hispanic, race_Asian, race_Native, race_Other)",
      Model == "Without Controls" ~ "None"
    )
  ) %>%
  select(
    Sample, Outcome, Running_Variable, Cutoff, Weighted, Model,
    Estimate.Type, Estimate, SE, P.Value,
    Bandwidth.Type, Bandwidth..h.,
    N_total, N_within_bw,
    Controls_Used, Reference_Category_Race
  )

write.csv(rdd_liberal_mc, here("ANES Restricted/Results/Monte Carlo/rdd_liberal_mc.csv"), row.names = FALSE)


# Combine the main results with those of the Monte Carlo simulations into a single master file
rdd_liberal_master <- bind_rows(rdd_liberal, rdd_liberal_mc)

# Create and save table that includes the lambda coefficient
lambda_table <- rdd_liberal_master %>%
  filter(`Estimate.Type` %in% c("Bias-Corrected", "Monte Carlo")) %>%
  select(Sample, Model, `Estimate.Type`, Estimate, SE, Outcome) %>%
  pivot_wider(
    names_from  = `Estimate.Type`,
    values_from = c(Estimate, SE)
  ) %>% 
  mutate(
    Lambda = `Estimate_Bias-Corrected` / 
      (`Estimate_Monte Carlo` + `SE_Monte Carlo`)
  )
