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


####################################
########## Data Functions ##########
####################################

# Create & use %notin%
`%notin%` <- Negate(`%in%`)

# Function for recoding NAs
na_recode <- function(x, invalid) {
  if_else(x %in% invalid, NA_integer_, x)
}

# Function for simplifying the voted/did not question ()
voted_recode <- function(x) {
  if_else(x == 1, x, 0)
}

# Function for fixing binary dummys (making 2s into 0s)
order_recode <- function(x, range_plus1){
  range_plus1 - x
}


binary_recode <- function(x) {
  if_else(x == 2, 0, x)
}

binary_tranpose <- function(x) {
  if_else(x == 1, 0, 1)
}







####################################
########## Plot Functions ##########
####################################


##### Scree-plot function #####

get_screeplot <- function(outcome, outcomename) {
  plot(1:length(outcome$values), outcome$values,
    type = "b",
    ylab = "Eigenvalue", xlab = "Factor", main = paste("Scree plot —", outcomename)
  )
}


##### Define function for coefplots #####

get_coefplot <- function(dataframe, colnumber = 3) {
  dataframe <- dataframe %>%
    mutate(
      Estimate_Inverse = -Estimate
    )

  ggplot(dataframe, aes(y = Sample, x = Estimate_Inverse, color = Model)) +
    geom_point(position = position_dodge(width = 0.6), size = 2.5) +
    geom_errorbarh(
      aes(xmin = Estimate_Inverse - 1.96 * SE, xmax = Estimate_Inverse + 1.96 * SE),
      height = 0.25,
      position = position_dodge(width = 0.6)
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    scale_color_manual(values = c("orange2", "orchid4")) +
    labs(
      title = "Estimated RD Effects Across Outcomes",
      subtitle = "Bias-Adjusted Estimates",
      x = "Estimated RD Effect",
      y = NULL,
      color = "Model"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom",
      plot.caption = ggtext::element_markdown()
    ) +
    facet_wrap(~Outcome, ncol = colnumber)
}


##### Define function for discontinuity plots for single outcome #####
get_discontinuityplot <- function(dataframe, outcome = "media_index", outcome_name = "Media Attitudes", colnumber = 2) { # arguments:
  # dataframe
  # the outcome of interest (specific index)
  # a string referring to the outcome name to be printed
  # number of columns

  dataframe <- dataframe %>%
    filter(age <= 60, !is.na(party))

  df_all <- dataframe %>% # republicans, democrats, independents
    mutate(
      subgroup = case_when(
        party == 1 ~ "Democrats",
        party == 2 ~ "Republicans",
        party == 3 ~ "Independents"
      )
    )

  df_partisans <- dataframe %>% # partisans subset
    filter(party %in% c(1, 2)) %>%
    mutate(
      subgroup = "Partisans"
    )


  df_plot <- bind_rows(
    df_all,
    df_partisans
  ) %>%
    filter(age <= 60, !is.na(.data[[outcome]]), !is.na(subgroup)) %>%
    mutate(subgroup = factor(
      subgroup,
      levels = c(
        "Independents",
        "Partisans",
        "Democrats",
        "Republicans"
      )
    ))


  # Now plot the grid #
  plot_subgroups <- ggplot(df_plot, aes(x = age, y = .data[[outcome]], color = factor(treatment))) +
    geom_point(alpha = 0.5) +
    geom_smooth(
      aes(group = factor(treatment)),
      method = "lm",
      formula = y ~ poly(x, 1),
      se = TRUE,
      color = "black",
      alpha = 0.75
    ) +
    geom_vline(xintercept = 26, linetype = "dashed", color = "black") +
    scale_color_brewer(
      palette = "Dark2",
      name = "2012 Voting Eligibility",
      labels = c("Not Eligible (<26)", "Eligible (≥26)")
    ) +
    labs(
      x = "Age in 2020",
      y = NULL,
      title = paste("Discontinuities in", outcome_name),
      subtitle = paste("Among Different Partisanship Categories")
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    facet_wrap(~subgroup, scales = "free_y", ncol = colnumber)


  plot_all <- ggplot(dataframe, aes(x = age, y = .data[[outcome]], color = factor(treatment))) +
    geom_point(alpha = 0.5) +
    geom_smooth(
      aes(group = factor(treatment)),
      method = "lm",
      formula = y ~ poly(x, 1),
      se = TRUE,
      color = "black",
      alpha = 0.75
    ) +
    geom_vline(xintercept = 26, linetype = "dashed", color = "black") +
    scale_color_brewer(
      palette = "Dark2",
      name = "2012 Voting Eligibility",
      labels = c("Not Eligible (<26)", "Eligible (≥26)")
    ) +
    labs(
      x = "Age in 2020",
      y = NULL,
      title = paste("Discontinuities in", outcome_name),
      subtitle = paste("Independents, Democrats, and Republicans (Full Sample)")
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )


  plots <- list(
    plot_all = plot_all,
    plot_subgroups = plot_subgroups
  )
}


##### Define function for discontinuity plots for multiple outcomes but one subgroup #####

get_discontinuityplot_multipleoutcomes <- function(dataframe, subset, primary_only = TRUE, colnumber = 3) { # arguments:
  # dataframe referring to the specific subset,
  # a string identifying the subset, and column numbers
  # primary outcomes or include traditionalism too
  # number of columns
  df_long <- dataframe %>%
    filter(age <= 60) %>%
    pivot_longer(
      cols = c(populism_index, authoritarianism_index, nativism_index, traditionalism_index),
      names_to = "Outcome",
      values_to = "Value"
    ) %>%
    filter(!is.na(Value)) %>%
    mutate(
      Outcome = recode(
        Outcome,
        populism_index = "Populism",
        authoritarianism_index = "Authoritarianism",
        nativism_index = "Nativism",
        traditionalism_index = "Traditionalism"
      ),
      Outcome = factor(
        Outcome,
        levels = c(
          "Nativism", "Authoritarianism",
          "Populism", "Traditionalism"
        )
      ),
    )

  # Check whether only primary variables should be kept
  if (primary_only) {
    df_long <- df_long %>%
      filter(Outcome == "Populism" | Outcome == "Authoritarianism" | Outcome == "Nativism")
  }

  # Now do the plotting
  ggplot(df_long, aes(x = age, y = Value, color = factor(treatment))) +
    geom_point(alpha = 0.5) +
    geom_smooth(
      aes(group = factor(treatment)),
      method = "lm",
      formula = y ~ poly(x, 1),
      se = TRUE,
      color = "black",
      alpha = 0.75
    ) +
    geom_vline(xintercept = 26, linetype = "dashed", color = "black") +
    scale_color_brewer(
      palette = "Dark2",
      name = "2012 Voting Eligibility",
      labels = c("Not Eligible (<26)", "Eligible (≥26)")
    ) +
    labs(
      x = "Age in 2020",
      y = NULL,
      title = paste("Discontinuities in Radical Right Attitudes Among", subset),
      subtitle = "Across Multiple Outcomes"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    facet_wrap(~Outcome, scales = "free_y", ncol = colnumber)
}


####################################
########### RDD Functions ##########
####################################


#####  Function that extracts the core values as a table to easily look at them #####
extract_rdd_summary <- function(rd_object, model_label = "Model") {
  # Extract core values
  coef <- rd_object$coef[2]
  se <- rd_object$se[2]
  pval <- rd_object$pv[2]
  p <- rd_object$p
  bw_type <- rd_object$bwselect
  bw_value <- rd_object$bws[1]
  n_below <- rd_object$N_h[1]
  n_above <- rd_object$N_h[2]

  # # Significance stars
  # stars <- case_when(
  #   pval < 0.001 ~ "***",
  #   pval < 0.01 ~ "**",
  #   pval < 0.05 ~ "*",
  #   TRUE ~ ""
  # )

  # Output as a table
  tibble(
    `Model` = model_label,
    # `Estimate` = paste0(round(coef, 3), stars),
    `Estimate` = round(coef, 3),
    `SE` = round(se, 3),
    `Bandwidth Type` = ifelse(bw_type == "mserd", "MSE-optimal", bw_type),
    `Bandwidth (h)` = round(bw_value, 2),
    `N` = n_below + n_above
  )
}


#####  Function that utilizes the function above and the existing rd_robust function to run the models #####
run_rdd_models <- function(data, index_var, controls, sample_label) {
  # Extract outcome + running variable
  y <- data[[index_var]]
  x <- data$age

  # --- Simple RDD model ---
  rdd_simple <- rdrobust(
    y = y,
    x = x,
    c = 26,
    all = TRUE
  )
  summary_simple <- extract_rdd_summary(
    rdd_simple,
    model_label = "Without Controls"
  )

  # --- Controls RDD model ---
  covs <- data[, controls, drop = FALSE]

  rdd_controls <- rdrobust(
    y = y,
    x = x,
    c = 26,
    covs = covs,
    all = TRUE
  )
  summary_controls <- extract_rdd_summary(
    rdd_controls,
    model_label = "With Controls"
  )

  # Add sample label and return combined results
  bind_rows(summary_simple, summary_controls) %>%
    dplyr::mutate(Sample = sample_label, .before = 1)
}
