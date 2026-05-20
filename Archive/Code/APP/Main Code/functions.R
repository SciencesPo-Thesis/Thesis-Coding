#------------------------------------------------------------------------------#
########################### Functions Used In Script ###########################    
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
# Screeplot for index construction #
get_screeplot <- function(outcome, outcomename) {
  data.frame(factor = seq_along(outcome$values), eigenvalue = outcome$values) %>%
    ggplot(aes(x = factor, y = eigenvalue)) +
    geom_line() +
    geom_point(size = 2) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    scale_x_continuous(breaks = seq_along(outcome$values)) +
    labs(x = "Factor", y = "Eigenvalue", title = paste("Scree Plot: ", outcomename)) +
    theme_bw() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
}
#------------------------------------------------------------------------------#



#------------------------------------------------------------------------------#
# Function for visualizing discontinuity #
get_discontinuityplot <- function(dataframe, outcome = "liberal_index", outcome_name = "Liberal Norm Support", party_id = "party_summary", colnumber = 2) {
  dataframe <- dataframe %>%
    filter(!is.na(.data[[party_id]]), !is.na(age_2012_election), age_2012_election < 30)
  
  is_expectations <- all(na.omit(dataframe[[party_id]]) %in% c(0, 1))
  
  df_all <- dataframe %>%
    mutate(
      subgroup = if (is_expectations) {
        case_when(
          .data[[party_id]] == 1 ~ "In-Power",
          .data[[party_id]] == 0 ~ "Out-of-Power"
        )
      } else {
        case_when(
          .data[[party_id]] == "Democrat" ~ "Democrats",
          .data[[party_id]] == "Republican" ~ "Republicans",
          .data[[party_id]] == "Independent" ~ "Independents"
        )
      }
    )
  
  
  subgroup_levels <- if (is_expectations) {
    c("In-Power", "Out-of-Power")
  } else {
    c("Independents", "Partisans", "Democrats", "Republicans")
  }

  df_plot <- if (is_expectations) {
    df_all
  } else {
    df_partisans <- dataframe %>%
      filter(.data[[party_id]] %in% c("Democrat", "Republican")) %>%
      mutate(subgroup = "Partisans")
    bind_rows(df_all, df_partisans)
  } %>%
    filter(!is.na(.data[[outcome]]), !is.na(subgroup)) %>%
    mutate(subgroup = factor(subgroup, levels = subgroup_levels))
  
  # Subgroup plot
  plot_subgroups <- ggplot(df_plot, aes(x = age_2012_election, y = .data[[outcome]], color = factor(treatment))) +
    geom_point(alpha = 0.15, size = 0.6) +
    geom_smooth(
      aes(group = factor(treatment)),
      method = "lm",
      formula = y ~ poly(x, 1),
      se = TRUE,
      color = "black",
      fill = "black",
      alpha = 0.2
    ) +
    geom_vline(xintercept = 18, linetype = "dashed", color = "black") +
    scale_color_brewer(
      palette = "Dark2",
      name    = "Group",
      labels  = c("Control (born ≤ Nov 6 1994; eligible 2012)", "Treatment (born > Nov 6 1994; eligible 2016)")
    ) +
    labs(
      x        = "Age at 2012 Election",
      y        = paste0(outcome_name, " Index (0 – 1)"),
      title    = paste("Discontinuity in", outcome_name),
      subtitle = if (is_expectations) "By Power Status" else "Among Different Partisanship Categories"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title      = element_text(face = "bold", hjust = 0.5),
      plot.subtitle   = element_text(hjust = 0.5)
    ) +
    facet_wrap(~subgroup, scales = "free_y", ncol = colnumber) +
    scale_x_continuous(breaks = seq(10, 30, by = 4))
  
  # Full-sample plot (skipped for expectations variable)
  if (is_expectations) {
    list(plot_subgroups = plot_subgroups)
  } else {
    plot_all <- ggplot(dataframe, aes(x = age_2012_election, y = .data[[outcome]], color = factor(treatment))) +
      geom_point(alpha = 0.15, size = 0.6) +
      geom_smooth(
        aes(group = factor(treatment)),
        method = "lm",
        formula = y ~ poly(x, 1),
        se = TRUE,
        color = "black",
        fill = "black",
        alpha = 0.2
      ) +
      geom_vline(xintercept = 18, linetype = "dashed", color = "black") +
      scale_color_brewer(
        palette = "Dark2",
        name    = "Group",
        labels  = c("Control (born ≤ Nov 6 1994; eligible 2012)", "Treatment (born > Nov 6 1994; eligible 2016)")
      ) +
      labs(
        x        = "Age at 2012 Election",
        y        = paste0(outcome_name, " Index (0 – 1)"),
        title    = paste("Discontinuity in", outcome_name),
        subtitle = "Independents, Democrats, and Republicans (Full Sample)"
      ) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        plot.title      = element_text(face = "bold", hjust = 0.5),
        plot.subtitle   = element_text(hjust = 0.5)
      ) +
      scale_x_continuous(breaks = seq(10, 30, by = 4))
    
    list(
      plot_all       = plot_all,
      plot_subgroups = plot_subgroups
    )
  }
}
#------------------------------------------------------------------------------#


# Function for visualizing discontinuity #
get_discontinuityplot_noscatter <- function(dataframe, outcome = "liberal_index", outcome_name = "Liberal Norm Support", party_id = "party_summary", colnumber = 2) {
  dataframe <- dataframe %>%
    filter(!is.na(.data[[party_id]]), !is.na(age_2012_election), age_2012_election < 30)
  
  is_expectations <- all(na.omit(dataframe[[party_id]]) %in% c(0, 1))
  
  df_all <- dataframe %>%
    mutate(
      subgroup = if (is_expectations) {
        case_when(
          .data[[party_id]] == 1 ~ "In-Power",
          .data[[party_id]] == 0 ~ "Out-of-Power"
        )
      } else {
        case_when(
          .data[[party_id]] == "Democrat" ~ "Democrats",
          .data[[party_id]] == "Republican" ~ "Republicans",
          .data[[party_id]] == "Independent" ~ "Independents"
        )
      }
    )
  
  
  subgroup_levels <- if (is_expectations) {
    c("In-Power", "Out-of-Power")
  } else {
    c("Independents", "Partisans", "Democrats", "Republicans")
  }
  
  df_plot <- if (is_expectations) {
    df_all
  } else {
    df_partisans <- dataframe %>%
      filter(.data[[party_id]] %in% c("Democrat", "Republican")) %>%
      mutate(subgroup = "Partisans")
    bind_rows(df_all, df_partisans)
  } %>%
    filter(!is.na(.data[[outcome]]), !is.na(subgroup)) %>%
    mutate(subgroup = factor(subgroup, levels = subgroup_levels))
  
  # Subgroup plot
  plot_subgroups <- ggplot(df_plot, aes(x = age_2012_election, y = .data[[outcome]], color = factor(treatment))) +
    geom_smooth(
      aes(group = factor(treatment)),
      method = "lm",
      formula = y ~ poly(x, 1),
      se = TRUE,
      alpha = 0.2
    ) +
    geom_vline(xintercept = 18, linetype = "dashed", color = "black") +
    scale_color_brewer(
      palette = "Dark2",
      name    = "Group",
      labels  = c("Control (born ≤ Nov 6 1994; eligible 2012)", "Treatment (born > Nov 6 1994; eligible 2016)")
    ) +
    labs(
      x        = "Age at 2012 Election",
      y        = paste0(outcome_name, " Index (0 – 1)"),
      title    = paste("Discontinuity in", outcome_name),
      subtitle = if (is_expectations) "By Power Status" else "Among Different Partisanship Categories"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title      = element_text(face = "bold", hjust = 0.5),
      plot.subtitle   = element_text(hjust = 0.5)
    ) +
    facet_wrap(~subgroup, scales = "free_y", ncol = colnumber) +
    scale_x_continuous(breaks = seq(10, 30, by = 4))
  
  # Full-sample plot (skipped for expectations variable)
  if (is_expectations) {
    list(plot_subgroups = plot_subgroups)
  } else {
    plot_all <- ggplot(dataframe, aes(x = age_2012_election, y = .data[[outcome]], color = factor(treatment))) +
      geom_smooth(
        aes(group = factor(treatment)),
        method = "lm",
        formula = y ~ poly(x, 1),
        se = TRUE,
        alpha = 0.2
      ) +
      geom_vline(xintercept = 18, linetype = "dashed", color = "black") +
      scale_color_brewer(
        palette = "Dark2",
        name    = "Group",
        labels  = c("Control (born ≤ Nov 6 1994; eligible 2012)", "Treatment (born > Nov 6 1994; eligible 2016)")
      ) +
      labs(
        x        = "Age at 2012 Election",
        y        = paste0(outcome_name, " Index (0 – 1)"),
        title    = paste("Discontinuity in", outcome_name),
        subtitle = "Independents, Democrats, and Republicans (Full Sample)"
      ) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        plot.title      = element_text(face = "bold", hjust = 0.5),
        plot.subtitle   = element_text(hjust = 0.5)
      ) +
      scale_x_continuous(breaks = seq(10, 30, by = 4))
    
    list(
      plot_all       = plot_all,
      plot_subgroups = plot_subgroups
    )
  }
}
#------------------------------------------------------------------------------#



#------------------------------------------------------------------------------#
# Coarse RDD models using years_from_cutoff as the running variable (PRL) #
run_rdd_models_coarse <- function(data, index_var, controls, sample_label) {
  y             <- data[[index_var]]
  x             <- data$years_from_cutoff
  n_total       <- sum(!is.na(y) & !is.na(x))
  controls_str  <- paste(controls, collapse = "; ")

  # --- Model 1: no covariates ---
  rdd_simple <- rdrobust(y = y, x = x, c = 0, all = TRUE)
  summary_simple <- extract_rdd_summary(
    rdd_simple,
    model_label = "Without Controls",
    n_total     = n_total
  ) %>%
    mutate(
      Controls_Used           = "None",
      Reference_Category_Race = NA_character_
    )

  # --- Model 2: with covariates ---
  covs <- data[, controls, drop = FALSE]
  rdd_controls <- rdrobust(y = y, x = x, c = 0, covs = covs, all = TRUE)
  summary_controls <- extract_rdd_summary(
    rdd_controls,
    model_label = "With Controls",
    n_total     = n_total
  ) %>%
    mutate(
      Controls_Used           = controls_str,
      Reference_Category_Race = "White (omitted baseline; dummies included: race_Black, race_Hispanic, race_Asian, race_Native American, race_Middle Eastern, race_Two or more races, race_Other)"
    )

  bind_rows(summary_simple, summary_controls) %>%
    mutate(
      Sample           = sample_label,
      Outcome          = "Liberal Democratic Norms Index: 1-factor principal-axis score of 5 PRL items (norm_judges, norm_polling, norm_executive, norm_censorship, norm_loyalty), rescaled to [0, 1]; higher = stronger support for liberal democratic norms",
      Running_Variable = "years_from_cutoff: birth year minus 1994; negative = born before cutoff (Control group); positive = born after cutoff (Treatment group)",
      Cutoff           = "1994: born in 1994 or earlier = eligible to vote in 2012 presidential election (Control, treatment=0); born in 1995 or later = first eligible presidential election was 2016 (Treatment, treatment=1). Note: coarse approximation — exact cutoff is November 6, 1994; respondents born in 1994 are misclassified by birth month/day.",
      Weighted         = "No — analyses are unweighted; PRL survey weights available but not applied",
      .before          = 1
    )
}
#------------------------------------------------------------------------------#





#------------------------------------------------------------------------------#
# Function for getting controls that are imbalanced between the treatment and control groups
get_imbalanced_controls <- function(data, potential_controls) {
  imbalanced_controls <- c()
  
  for (var in potential_controls) {
    if (is.numeric(data[[var]])) {
      result <- t.test(data[[var]] ~ data$treatment)
      threshold <- 0.3
    } else {
      result <- chisq.test(table(data[[var]], data$treatment))
      threshold <- 0.05
    }
    
    if (result$p.value < threshold) {
      imbalanced_controls <- c(imbalanced_controls, var)
      message(var, " is imbalanced")
    } else {
      message(var, " is not imbalanced")
    }
  }
  
  imbalanced_controls
}
#------------------------------------------------------------------------------#



#------------------------------------------------------------------------------#
# Helper function that pulls estimates
# Column naming convention:
#   Conventional        = standard RD estimator (no bias correction)
#   BiasCorrected       = bias-corrected point estimate (preferred for main results)
#   BC_RobustSE         = bias-corrected estimate with robust (robust-bias-corrected) SE
# N columns:
#   N_within_bw         = total observations inside the MSE-optimal bandwidth
#   N_left/right_bw     = observations left / right of cutoff within bandwidth
#   N_total_left/right  = total observations left / right of cutoff in full dataset
pull_estimates <- function(r) {
  tibble(
    Estimate_Conventional  = r$coef[1], SE_Conventional  = r$se[1], PValue_Conventional  = r$pv[1],
    Estimate_BiasCorrected = r$coef[2], SE_BiasCorrected = r$se[2], PValue_BiasCorrected = r$pv[2],
    Estimate_BC_RobustSE   = r$coef[3], SE_BC_RobustSE   = r$se[3], PValue_BC_RobustSE   = r$pv[3],
    N_within_bw            = r$N_h[1] + r$N_h[2],
    N_left_bw              = r$N_h[1],
    N_right_bw             = r$N_h[2],
    N_total_left           = r$N[1],
    N_total_right          = r$N[2]
  )
}
#------------------------------------------------------------------------------#



#------------------------------------------------------------------------------#
# Function that extracts the core values of the RD as a table to easily look at them
## Returns one row per estimate type (Conventional, Bias-Corrected, Bias-Corrected + Robust SE)
## Preferred specification for main text is row 2: Bias-Corrected
## n_total: total non-missing observations in the subgroup BEFORE bandwidth restriction
## N_left/right_cutoff: full split of the dataset at the cutoff (not bandwidth-restricted)
## N_within_bw / N_left_bw / N_right_bw: observations actually used in estimation
extract_rdd_summary <- function(rd_object, model_label = "Model", n_total = NA_integer_) {
  est <- pull_estimates(rd_object)
  tibble(
    Model           = model_label,
    `Estimate Type` = c("Conventional", "Bias-Corrected", "Bias-Corrected (Robust SE)"),
    Estimate        = round(c(est$Estimate_Conventional, est$Estimate_BiasCorrected, est$Estimate_BC_RobustSE), 4),
    SE              = round(c(est$SE_Conventional,       est$SE_BiasCorrected,       est$SE_BC_RobustSE),       4),
    `P-Value`       = round(c(est$PValue_Conventional,   est$PValue_BiasCorrected,   est$PValue_BC_RobustSE),   4),
    `Bandwidth Type`= ifelse(rd_object$bwselect == "mserd", "MSE-optimal", rd_object$bwselect),
    `Bandwidth (h)` = round(rd_object$bws[1], 2),
    N_total         = n_total,
    N_left_cutoff   = rd_object$N[1],
    N_right_cutoff  = rd_object$N[2],
    N_within_bw     = rd_object$N_h[1] + rd_object$N_h[2],
    N_left_bw       = rd_object$N_h[1],
    N_right_bw      = rd_object$N_h[2]
  )
}
#------------------------------------------------------------------------------#



#------------------------------------------------------------------------------#
# Function adapting the use of the rdrobust::rdrobust() for the purposes of this project
run_rdd_models <- function(data, index_var, controls, sample_label) {
  y             <- data[[index_var]]
  x             <- data$days_from_cutoff
  # N_total: observations with non-missing outcome AND running variable in this subgroup
  n_total       <- sum(!is.na(y) & !is.na(x))
  controls_str  <- paste(controls, collapse = "; ")

  # --- Model 1: no covariates ---
  rdd_simple <- rdrobust(y = y, x = x, c = 0, all = TRUE)
  summary_simple <- extract_rdd_summary(
    rdd_simple,
    model_label = "Without Controls",
    n_total     = n_total
  ) %>%
    mutate(
      Controls_Used           = "None",
      Reference_Category_Race = NA_character_
    )

  # --- Model 2: with covariates ---
  covs <- data[, controls, drop = FALSE]
  rdd_controls <- rdrobust(y = y, x = x, c = 0, covs = covs, all = TRUE)
  summary_controls <- extract_rdd_summary(
    rdd_controls,
    model_label = "With Controls",
    n_total     = n_total
  ) %>%
    mutate(
      Controls_Used           = controls_str,
      Reference_Category_Race = "White (omitted baseline; dummies included: race_Black, race_Hispanic, race_Asian, race_Native, race_Other)"
    )

  bind_rows(summary_simple, summary_controls) %>%
    mutate(
      Sample           = sample_label,
      Outcome          = "Liberal Democratic Norms Index: 1-factor principal-axis score of 7 ANES items (V201366, V201367, V201368, V201369, V201372x, V201375x, V201376), rescaled to [0, 1]; higher = stronger support for liberal democratic norms",
      Running_Variable = "days_from_cutoff: days between respondent date of birth and November 6, 1994; negative = born before cutoff (Control group)",
      Cutoff           = "November 6, 1994: born on/before this date = eligible to vote in 2012 presidential election (Control, treatment=0); born after = first eligible presidential election was 2016 (Treatment, treatment=1)",
      Weighted         = "No — analyses are unweighted; ANES post-stratification weight V200010b available but not applied",
      .before          = 1
    )
}
#------------------------------------------------------------------------------#



#------------------------------------------------------------------------------#
# Runs robustness checks (bandwidth sensitivity, placebo cutoffs, and polynomial degree checks)
run_robustness_checks <- function(data, index_var, controls, sample_label) {
  y <- data[[index_var]]
  x <- data$days_from_cutoff
  covs <- data[, controls, drop = FALSE]
  
  # MSE-optimal bandwidth (with controls) used to set the sensitivity range
  h_opt <- rdrobust(y = y, x = x, c = 0, covs = covs)$bws[1]
  
  # Bandwidth sensitivity
  bws <- unique(round(seq(h_opt * 0.3, h_opt * 2.0, length.out = 20)))
  bw_results <- map_dfr(c(FALSE, TRUE), function(use_covs) {
    map_dfr(bws, function(h) {
      tryCatch(
        {
          r <- rdrobust(
            y    = y,
            x    = x,
            c    = 0,
            h    = h,
            covs = if (use_covs) covs else NULL,
            all  = TRUE
          )
          pull_estimates(r) %>%
            mutate(
              Bandwidth           = h,
              Optimal_Bandwidth_h = h_opt,
              Controls            = if (use_covs) "With Controls" else "Without Controls"
            )
        },
        error = function(e) {
          message("Bandwidth = ", h, " (controls = ", use_covs, "): ", e$message)
          NULL
        }
      )
    })
  }) %>% mutate(Sample = sample_label)

  # Placebo cutoffs
  placebo_cutoffs <- c(-730, -365, 0, 365, 730)
  cutoff_results <- map_dfr(c(FALSE, TRUE), function(use_covs) {
    map_dfr(placebo_cutoffs, function(co) {
      tryCatch(
        {
          r <- rdrobust(
            y    = y,
            x    = x,
            c    = co,
            covs = if (use_covs) covs else NULL,
            all  = TRUE
          )
          pull_estimates(r) %>%
            mutate(
              Placebo_Cutoff = co,
              Controls       = if (use_covs) "With Controls" else "Without Controls"
            )
        },
        error = function(e) {
          message("Placebo cutoff = ", co, " (controls = ", use_covs, "): ", e$message)
          NULL
        }
      )
    })
  }) %>% mutate(Sample = sample_label)

  # Polynomial degree
  poly_results <- map_dfr(c(FALSE, TRUE), function(use_covs) {
    map_dfr(1:3, function(p) {
      tryCatch(
        {
          r <- rdrobust(
            y    = y,
            x    = x,
            c    = 0,
            p    = p,
            covs = if (use_covs) covs else NULL,
            all  = TRUE
          )
          pull_estimates(r) %>%
            mutate(
              Polynomial_Degree = p,
              Controls          = if (use_covs) "With Controls" else "Without Controls"
            )
        },
        error = function(e) {
          message("Polynomial = ", p, " (controls = ", use_covs, "): ", e$message)
          NULL
        }
      )
    })
  }) %>% mutate(Sample = sample_label)

  bind_rows(
    bw_results     %>% mutate(Check_Type = "Bandwidth Sensitivity"),
    cutoff_results %>% mutate(Check_Type = "Placebo Cutoff Test"),
    poly_results   %>% mutate(Check_Type = "Polynomial Degree Sensitivity")
  ) %>%
    mutate(
      Outcome  = "Liberal Democratic Norms Index (0-1 factor score of 7 ANES items; higher = stronger liberal norm support)",
      Weighted = "No"
    )
}
#------------------------------------------------------------------------------#



#------------------------------------------------------------------------------#
# Function for plotting robustness checks
## estimate_type: 1 = Conventional, 2 = Bias-Corrected, 3 = BC + Robust SE (preferred)
get_robustnessplots <- function(data,
                                subgroup_name,
                                estimate_type = 3,
                                include_covariates = TRUE) {
  
  bw_data   <- data %>% filter(Check_Type == "Bandwidth Sensitivity")
  co_data   <- data %>% filter(Check_Type == "Placebo Cutoff Test")
  poly_data <- data %>% filter(Check_Type == "Polynomial Degree Sensitivity")
  # Select coefficient and SE columns for the chosen estimate type —
  # used consistently across all three panels
  cols <- switch(as.character(estimate_type),
                 "1" = c("Estimate_Conventional",  "SE_Conventional"),
                 "2" = c("Estimate_BiasCorrected",  "SE_BiasCorrected"),
                 "3" = c("Estimate_BC_RobustSE",    "SE_BC_RobustSE"),
                 stop("estimate_type must be 1, 2, or 3")
  )
  
  estimate_name <- switch(as.character(estimate_type),
                          "1" = "Conventional",
                          "2" = "Bias-Corrected",
                          "3" = "Bias-Corrected (Robust SE)"
  )
  
  control_label <- if (include_covariates) "(With Controls)" else "(Without Controls)"
  
  # Filter to the requested subgroup and attach estimate/se columns
  controls_val <- if (include_covariates) "With Controls" else "Without Controls"
  prep <- function(df, filter_controls = FALSE) {
    df %>%
      filter(Sample == subgroup_name, Controls == controls_val) %>%
      mutate(
        estimate = .data[[cols[1]]],
        se       = .data[[cols[2]]]
      )
  }

  bw_plot_data   <- prep(bw_data)
  co_plot_data   <- prep(co_data)
  poly_plot_data <- prep(poly_data)
  
  # Shared panel builder — no end caps on error bars (width = 0) for consistency
  make_panel <- function(data, x_var, x_lab, title_str, subtitle_str) {
    ggplot(data, aes(x = .data[[x_var]], y = estimate)) +
      geom_point(size = 1.5, colour = "black") +
      geom_errorbar(
        aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
        width = 0, colour = "black", linewidth = 0.35
      ) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(
        x        = x_lab,
        y        = "LATE",
        title    = title_str,
        subtitle = subtitle_str
      ) +
      theme_classic(base_size = 8) +
      theme(
        plot.title    = element_text(face = "bold", hjust = 0.5, size = 8),
        plot.subtitle = element_text(hjust = 0.5)
      )
  }
  
  h_opt_val <- bw_plot_data$Optimal_Bandwidth_h[1]
  
  panel_bw <- make_panel(
    bw_plot_data, "Bandwidth",
    "Bandwidth (days)",
    "Bandwidth Sensitivity",
    "(1st-order polynomial, cutoff = 0)"
  ) +
    geom_vline(xintercept = h_opt_val, linetype = "dashed", colour = "grey50", linewidth = 0.5)

  panel_co <- make_panel(
    co_plot_data, "Placebo_Cutoff",
    "Placebo cutoff (days from true cutoff)",
    "Placebo Cutoffs",
    "(MSE-optimal bandwidth, 1st-order polynomial)"
  )

  panel_poly <- make_panel(
    poly_plot_data, "Polynomial_Degree",
    "Polynomial degree",
    "Polynomial Degree",
    "(MSE-optimal bandwidth, cutoff = 0)"
  )
  
  (panel_bw | panel_co | panel_poly) +
    plot_annotation(
      title = paste(estimate_name, "—", subgroup_name, control_label),
      theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10))
    )
}


#------------------------------------------------------------------------------#


## Run Montecarlo function for RDD
run_mc_rdd <- function(
    data,
    outcome_var = "liberal_index",
    birth_year_var = "year_of_birth",
    controls = NULL,
    cutoff_date = as.Date("1994-11-06"),
    sample_label = NULL,
    n_sim = 100,
    seed = 42
) {
  set.seed(seed)
  
  # Get the sample name from type of sample
 sample_label <- case_when(
   sample_label == "df" ~ "Full Sample",
   sample_label == "df_democrats" ~ "Democrats",
   sample_label == "df_republicans" ~ "Republicans",
   sample_label == "df_independents" ~ "Independents",
   sample_label == "df_partisans" ~ "Partisans",
   sample_label == "df_inpower" ~ "In-Power",
   sample_label == "df_outofpower" ~ "Out-of-Power",
  TRUE ~ paste0(sample_label, " is an unknown dataset name")
)
   
   # Get the model name from the beginning
   model_label <- case_when(
     is.null(controls) ~ "Without Controls", 
     TRUE ~ "With Controls"
   )
  
  # Drop rows with missing birth year
  ok <- !is.na(data[[birth_year_var]])
  data <- data[ok, ]
  
  by <- data[[birth_year_var]]
  y <- data[[outcome_var]]
  covs <- if (!is.null(controls)) data[, controls, drop = FALSE] else NULL
  n <- nrow(data)
  
  # Results: one row per simulation
  # n_total: total observations in this subgroup (after dropping NA birth years) before any bandwidth restriction
  # n_used:  observations within the MSE-optimal bandwidth for that simulation draw
  out <- data.frame(
    sim      = seq_len(n_sim),
    estimate = NA_real_,
    bw       = NA_real_,
    n_used   = NA_integer_,
    n_total  = n,
    sample   = sample_label,
    model    = model_label,
    outcome  = "Liberal Democratic Norms Index (0-1 factor score of 7 ANES items; higher = stronger liberal norm support)",
    weighted = "No"
  )
  
  month_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  start_time <- Sys.time() #
  message(paste0("-------------- Running simulation for ", sample_label, " (", model_label, ") --------------"))
  for (i in seq_len(n_sim)) {
    #message(paste0("Running simulation for ", sample_label))
    if (i %% 100 == 0) { # ← add this at the very top of the loop
      elapsed <- difftime(Sys.time(), start_time, units = "mins")
      expected <- elapsed * n_sim / i
      message(
        "Iteration ", i, " of ", n_sim,
        "\n| Elapsed time for this sample: ", round(elapsed, 1), " min",
        "\n| Expected total for this sample: ", round(expected, 1), " min",
        "\n| (Very approximate) Expected total runtime: ", round(expected*14, 1), "min"
      )
    }
    # Simulate a random birth month and day for every respondent,
    # then convert to days_from_cutoff. For non-1994 respondents this
    # never changes their side of the cutoff (a 1993-born person is
    # always negative regardless of month/day), but it removes the
    # arbitrary July 2 assumption and is more consistent.
    sim_month <- sample(1L:12L, n, replace = TRUE)
    sim_day <- ceiling(runif(n) * month_days[sim_month])
    sim_dates <- as.Date(paste(by, sim_month, sim_day, sep = "-"))
    x <- as.integer(sim_dates - cutoff_date)
    
    # tryCatch runs rdrobust() and returns NULL if it crashes
    # (e.g. too few observations in the bandwidth), so one bad
    # iteration doesn't stop the whole simulation
    fit <- tryCatch(
      suppressWarnings(
        if (is.null(covs)) {
          rdrobust(y = y, x = x, c = 0, all = TRUE)
        } else {
          rdrobust(y = y, x = x, c = 0, covs = covs, all = TRUE)
        }
      ),
      error = function(e) NULL
    )
    
    if (!is.null(fit)) {
      out$estimate[i] <- fit$coef[2]
      out$bw[i] <- fit$bws[1, 1]
      out$n_used[i] <- fit$N_h[1] + fit$N_h[2]
    }
  }
  
  out
}


#------------------------------------------------------------------------------#


# Function for main coefplots
get_coefplot <- function(
    dataframe,
    colnumber = 3,
    facet_var = "Outcome",
    title     = "Estimated Effects Across Subgroups (APP)",   # override for cohort OLS vs RDD
    subtitle  = "Bias-Adjusted / OLS Estimates",
    caption   = NULL
) {
  ggplot(dataframe, aes(y = Sample, x = Estimate, color = Model, linetype = Model)) +
    # Dot for the point estimate; position_dodge() separates the two Model specs vertically
    geom_point(position = position_dodge(width = 0.6), size = 2.5) +
    # 95% CI as a horizontal error bar; ±1.96 × SE assumes approximate normality of the
    # estimator (valid for both OLS and rdrobust bias-corrected estimates in large samples)
    geom_errorbarh(
      aes(xmin = Estimate - 1.96 * SE, xmax = Estimate + 1.96 * SE),
      height = 0.25,
      position = position_dodge(width = 0.6)
    ) +
    # Dashed vertical line at x = 0: the null hypothesis of no cohort effect
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    # Orange = Without Controls; orchid = With Controls (consistent across all plots)
    scale_color_manual(values = c("lightseagreen", "orchid4")) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    labs(
      title    = title,
      subtitle = subtitle,
      x        = "Estimated LATE (τ)",
      y        = NULL,
      color    = "Model",
      caption  = caption
    ) +
    theme_bw() +
    theme(
      plot.title    = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption  = element_text(hjust = 0, size = 8, color = "gray40"),
      legend.position = "bottom",
      plot.caption.position = "plot"
    ) +
    # as.formula(paste("~", facet_var)) converts e.g. "Outcome" → ~Outcome dynamically,
    # allowing the caller to switch faceting between outcome variables and methods.
    facet_wrap(as.formula(paste("~", facet_var)), ncol = colnumber)
}


#------------------------------------------------------------------------------#


# Function for robustness-type coefplots

get_coefplot_robustness <- function(
    dataframe,
    colnumber = 2,
    facet_var = "Model",
    title     = "Estimated Effects Across Subgroups (APP)",   # override for cohort OLS vs RDD
    subtitle  = "Across Different Model Specifications",
    caption   = NULL
) {
  ggplot(dataframe, aes(y = Sample, x = Estimate, color = `Estimate Type`, linetype = `Estimate Type`)) +
    # Dot for the point estimate; position_dodge() separates the two Model specs vertically
    geom_point(position = position_dodge(width = 0.6), size = 2.5) +
    # 95% CI as a horizontal error bar; ±1.96 × SE assumes approximate normality of the
    # estimator (valid for both OLS and rdrobust bias-corrected estimates in large samples)
    geom_errorbarh(
      aes(xmin = Estimate - 1.96 * SE, xmax = Estimate + 1.96 * SE),
      height = 0.25,
      position = position_dodge(width = 0.6)
    ) +
    # Dashed vertical line at x = 0: the null hypothesis of no cohort effect
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    # Orange = Without Controls; orchid = With Controls (consistent across all plots)
    scale_color_manual(values = c("gray", "olivedrab4", "tan3")) +
    scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
    labs(
      title    = title,
      subtitle = subtitle,
      x        = "Estimated LATE (τ)",
      y        = NULL,
      color    = "Model",
      linetype = "Model",
      caption  = caption
    ) +
    theme_bw() +
    theme(
      plot.title    = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption  = element_text(hjust = 0, size = 8, color = "gray40"),
      legend.position = "bottom",
      plot.caption.position = "plot"
    ) +
    # as.formula(paste("~", facet_var)) converts e.g. "Outcome" → ~Outcome dynamically,
    # allowing the caller to switch faceting between outcome variables and methods.
    facet_wrap(as.formula(paste("~", facet_var)), ncol = colnumber)
}


#------------------------------------------------------------------------------#


# Function for bootstrapping

run_mc_rdd_bootstrap <- function(
    data,
    outcome_var    = "liberal_index",
    birth_year_var = "year_of_birth",
    controls       = NULL,
    cutoff_date    = as.Date("1994-11-06"),
    sample_label   = NULL,
    n_sim          = 100,
    n_boot         = 500,
    seed           = 42
) {
  set.seed(seed)
  
  # ── Labels ────────────────────────────────────────────────────────────────
  sample_label <- case_when(
    sample_label == "df"              ~ "Full Sample",
    sample_label == "df_democrats"    ~ "Democrats",
    sample_label == "df_republicans"  ~ "Republicans",
    sample_label == "df_independents" ~ "Independents",
    sample_label == "df_partisans"    ~ "Partisans",
    sample_label == "df_inpower"  ~ "In-Power",
    sample_label == "df_outofpower" ~ "Out-of-Power",
    TRUE ~ paste0(sample_label, " is an unknown dataset name")
  )
  model_label <- if (is.null(controls)) "Without Controls" else "With Controls"
  
  # ── Prepare data ──────────────────────────────────────────────────────────
  ok   <- !is.na(data[[birth_year_var]])
  data <- data[ok, ]
  
  by   <- data[[birth_year_var]]
  y    <- data[[outcome_var]]
  covs <- if (!is.null(controls)) data[, controls, drop = FALSE] else NULL
  n    <- nrow(data)
  
  month_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  # ── Storage ───────────────────────────────────────────────────────────────
  # Outer MC draws — one row per simulation (mirrors run_mc_rdd output)
  outer_draws <- data.frame(
    sim      = seq_len(n_sim),
    estimate = NA_real_,
    bw       = NA_real_,
    n_used   = NA_integer_,
    n_total  = n,
    sample   = sample_label,
    model    = model_label,
    outcome  = "Liberal Democratic Norms Index (0-1 factor score; higher = stronger liberal norm support)",
    weighted = "No"
  )
  
  # Null estimates — one value per bootstrap draw, pre-allocated for speed
  null_raw <- numeric(n_sim * n_boot)
  
  # ── Outer loop ────────────────────────────────────────────────────────────
  start_time <- Sys.time()
  message(paste0("------ Bootstrap: ", sample_label, " (", model_label, ") ------"))
  
  for (i in seq_len(n_sim)) {
    
    if (i %% 50 == 0) {
      elapsed  <- difftime(Sys.time(), start_time, units = "mins")
      expected <- elapsed * n_sim / i
      message(
        "Outer iteration ", i, " of ", n_sim,
        "\n| Elapsed: ",        round(elapsed,  1), " min",
        "\n| Expected total: ", round(expected, 1), " min"
      )
    }
    
    # Step 1: simulate birth dates → running variable
    sim_month <- sample(1L:12L, n, replace = TRUE)
    sim_day   <- ceiling(runif(n) * month_days[sim_month])
    sim_dates <- as.Date(paste(by, sim_month, sim_day, sep = "-"))
    x         <- as.integer(sim_dates - cutoff_date)
    
    # Step 2: rdrobust on real Y → point estimate
    fit <- tryCatch(
      suppressWarnings(
        if (is.null(covs))
          rdrobust(y = y, x = x, c = 0, all = TRUE)
        else
          rdrobust(y = y, x = x, c = 0, covs = covs, all = TRUE)
      ),
      error = function(e) NULL
    )
    
    if (is.null(fit)) next
    
    outer_draws$estimate[i] <- fit$coef[2]
    outer_draws$bw[i]       <- fit$bws[1, 1]
    outer_draws$n_used[i]   <- fit$N_h[1] + fit$N_h[2]
    
    # Step 3: null model within bandwidth
    h     <- fit$bws[1, 1]
    in_bw <- abs(x) <= h
    kw    <- 1 - abs(x[in_bw]) / h
    x_bw  <- x[in_bw]
    y_bw  <- y[in_bw]
    
    if (is.null(covs)) {
      null_df    <- data.frame(y = y_bw, x = x_bw)
      null_model <- lm(y ~ x, data = null_df, weights = kw)
    } else {
      covs_bw    <- covs[in_bw, , drop = FALSE]
      null_df    <- data.frame(y = y_bw, x = x_bw, covs_bw)
      null_model <- lm(y ~ ., data = null_df, weights = kw)
    }
    
    y_hat  <- fitted(null_model)
    resids <- residuals(null_model)
    n_bw   <- sum(in_bw)
    
    # Step 4: inner bootstrap loop
    idx_start <- (i - 1) * n_boot + 1
    idx_end   <- i       * n_boot
    
    for (b in seq_len(n_boot)) {
      y_fake <- y_hat + sample(resids, size = n_bw, replace = TRUE)
      
      fake_fit <- tryCatch(
        suppressWarnings(
          if (is.null(covs))
            rdrobust(y = y_fake, x = x_bw, c = 0, h = h, all = TRUE)
          else
            rdrobust(y = y_fake, x = x_bw, c = 0, h = h,
                     covs = covs_bw, all = TRUE)
        ),
        error = function(e) NULL
      )
      
      if (!is.null(fake_fit))
        null_raw[idx_start + b - 1] <- fake_fit$coef[2]
    }
  }
  
  # ── Collapse ──────────────────────────────────────────────────────────────
  # Remove placeholder zeros from failed iterations
  null_estimates <- null_raw[null_raw != 0]
  
  tau_bar  <- mean(outer_draws$estimate, na.rm = TRUE)
  sigma_mc <- sd(outer_draws$estimate,   na.rm = TRUE)
  
  # Bootstrap p-value from null distribution
  p_boot <- mean(abs(null_estimates) >= abs(tau_bar))
  
  # Percentile CI from MC draws (no normality assumption)
  ci <- quantile(outer_draws$estimate, c(0.025, 0.975), na.rm = TRUE)
  
  summary_out <- data.frame(
    Sample        = sample_label,
    Model         = model_label,
    Estimate.Type = "Monte Carlo (Bootstrap)",
    Estimate      = tau_bar,
    SE            = sigma_mc,
    CI_lower      = ci[[1]],
    CI_upper      = ci[[2]],
    SE_lower      = (tau_bar - ci[[1]]) / 1.96,
    SE_upper      = (ci[[2]] - tau_bar) / 1.96,
    P.Value       = p_boot,
    Bandwidth.h   = round(mean(outer_draws$bw,    na.rm = TRUE), 2),
    N_total       = n,
    N_within_bw   = as.integer(round(mean(outer_draws$n_used, na.rm = TRUE))),
    Controls_Used = if (is.null(controls)) "None" else paste(controls, collapse = "; ")
  )
  
  # Store null estimates with labels for plotting
  null_df <- data.frame(
    null_estimate = null_estimates,
    sample        = sample_label,
    model         = model_label
  )
  
  list(
    summary        = summary_out,
    outer_draws    = outer_draws,   # same structure as run_mc_rdd() output
    null_estimates = null_df        # for null distribution plots
  )
}



#------------------------------------------------------------------------------#



plot_mc_histogram_boot <- function(draws, title = "Sampling Distribution of Bootstrap MC Estimates") {
  
  mc_summary <- draws %>%
    group_by(sample, model) %>%
    summarize(mean_est = mean(estimate, na.rm = TRUE),
              sd_est   = sd(estimate,   na.rm = TRUE),
              .groups  = "drop")
  
  mc_curves <- draws %>%
    group_by(sample, model) %>%
    reframe(
      x = seq(min(estimate, na.rm = TRUE),
              max(estimate, na.rm = TRUE),
              length.out = 300),
      y = dnorm(x,
                mean = mean(estimate, na.rm = TRUE),
                sd   = sd(estimate,   na.rm = TRUE))
    )
  
  ggplot(draws, aes(x = estimate)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 120, fill = "forestgreen",
                   color = "white", alpha = 0.8) +
    geom_line(data = mc_curves, aes(x = x, y = y),
              color = "orchid4", linewidth = 1, inherit.aes = FALSE) +
    geom_vline(data = mc_summary, aes(xintercept = mean_est),
               color = "red", linewidth = 0.5) +
    geom_vline(xintercept = 0, color = "gray50", linetype = "dashed") +
    facet_wrap(~ sample + model, scales = "free_y", ncol = 2) +
    labs(x        = "RD Estimate (Bias-Corrected)",
         y        = "Density",
         title    = title,
         subtitle = "Liberal Index — Bootstrap MC Draws") +
    theme_bw() +
    theme(plot.title    = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
}


#------------------------------------------------------------------------------#



plot_null_distribution <- function(null_df, summary_df,
                                   title = "Empirical Null Distribution (Bootstrap)") {
  # Join real estimate into null_df for the vline
  null_plot <- null_df %>%
    left_join(summary_df %>% select(Sample, Model, Estimate),
              by = c("sample" = "Sample", "model" = "Model"))
  
  ggplot(null_plot, aes(x = null_estimate)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 100, fill = "steelblue",
                   color = "white", alpha = 0.7) +
    # Highlight the real estimate
    geom_vline(aes(xintercept = Estimate),
               color = "red", linewidth = 0.8, linetype = "solid") +
    # Null reference line
    geom_vline(xintercept = 0,
               color = "gray40", linewidth = 0.5, linetype = "dashed") +
    # Mirror of real estimate (other tail)
    geom_vline(aes(xintercept = -Estimate),
               color = "red", linewidth = 0.5, linetype = "dotted") +
    facet_wrap(~ sample + model, scales = "free_y", ncol = 2) +
    labs(x        = "Null RD Estimate",
         y        = "Density",
         title    = title,
         subtitle = "Red line = real estimate. Shaded area beyond red = bootstrap p-value.",
         caption  = "Null distribution constructed by resampling residuals from a no-effect model.") +
    theme_bw() +
    theme(plot.title    = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption  = element_text(hjust = 0, size = 8, color = "gray40"))
}













# =============================================================================
# run_mc_rdd_bootstrap_fast.R
# Fast Monte Carlo + Bootstrap RDD inference
# =============================================================================
#
# Drop-in replacement for the original run_mc_rdd_bootstrap(). Returns the
# SAME list shape (summary / outer_draws / null_estimates) so your existing
# wrapper code and plotting functions work unchanged.
#
# WHAT CHANGED, AND WHY
# ---------------------
# 1) INNER LOOP — replaces 500 rdrobust() calls per outer iter with a
#    closed-form local-polynomial operator.
#
#    The original code calls rdrobust() in the inner bootstrap loop. At a
#    fixed bandwidth h, rdrobust does:
#       (a) local-linear (p=1) fits each side of the cutoff, kernel-weighted
#       (b) local-quadratic (q=2) pilot fits each side for the bias term
#       (c) bias-corrected estimate = (alpha_R - alpha_L) - h^2 * (b2_R - b2_L)
#    All variance / SE / bandwidth-selector machinery is irrelevant for a
#    null distribution — we only need the point estimate. So we precompute
#    the linear maps (X'WX)^{-1}X'W once per outer iter and apply them as
#    matrix multiplications. ALL B null draws are computed in a single set
#    of BLAS calls.
#
#    NOTE: this fixes the bias-correction bandwidth at b = h (rho = 1)
#    within each outer iter. rdrobust by default reselects b on every call.
#    The diagnostic below quantifies the difference.
#
# 2) DIAGNOSTIC — on outer iter 1 only, also compute all B null estimates
#    using full rdrobust(h=h, b=h) and report the correlation + max abs diff
#    between the operator-based and rdrobust-based null estimates. Costs
#    a few seconds total. If correlation > 0.99, the fast version is
#    interchangeable with the slow one for inferential purposes.
#
# 3) NULL MODEL — replaces lm() with .lm.fit() (skips formula parsing,
#    model.frame, full QR with metadata).
#
# 4) PARALLEL OUTER LOOP — outer iters are independent. n_cores > 1 runs
#    them on multiple cores via parallel::mclapply (mac/linux) or
#    parallel::parLapply (windows).
#
# 5) INTEGER RUNNING VARIABLE — precompute integer day-offsets per
#    (year, month) cell, skip as.Date() in the hot loop.
#
# 6) NA SENTINELS instead of zero, so a genuine null draw equal to 0 isn't
#    silently dropped.
#
# Required packages: rdrobust, dplyr (for case_when only). parallel is base.
# =============================================================================


# ---- helper: %||% ----------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a


# ---- helper: closed-form bias-corrected RDD operator -----------------------
#
# Given a running variable x and bandwidth h, returns a function that maps
# any outcome vector y (defined on x) to the bias-corrected RD estimate.
#
# This is mathematically identical to rdrobust(y, x, c=0, h=h, rho=1)$coef[2].

build_rdd_operator <- function(x_bw, h) {
  # x_bw: running variable values WITHIN bandwidth, both sides of 0
  # h:    bandwidth (scalar)
  
  w     <- 1 - abs(x_bw) / h            # triangular kernel
  right <- x_bw >= 0
  idx_L <- which(!right)
  idx_R <- which( right)
  
  # left side: main p=1 and pilot q=2
  xl <- x_bw[idx_L]; wl <- w[idx_L]
  Xl1 <- cbind(1, xl);          WXl1 <- Xl1 * wl
  Xl2 <- cbind(1, xl, xl * xl); WXl2 <- Xl2 * wl
  Ml1 <- solve(crossprod(Xl1, WXl1)) %*% t(WXl1)
  Ml2 <- solve(crossprod(Xl2, WXl2)) %*% t(WXl2)
  
  # right side
  xr <- x_bw[idx_R]; wr <- w[idx_R]
  Xr1 <- cbind(1, xr);          WXr1 <- Xr1 * wr
  Xr2 <- cbind(1, xr, xr * xr); WXr2 <- Xr2 * wr
  Mr1 <- solve(crossprod(Xr1, WXr1)) %*% t(WXr1)
  Mr2 <- solve(crossprod(Xr2, WXr2)) %*% t(WXr2)
  
  # Row vectors that produce, by inner product with y on the relevant side,
  # the intercept (alpha) and the x^2 coefficient (b2).
  aL <- Ml1[1, ];  bL <- Ml2[3, ]
  aR <- Mr1[1, ];  bR <- Mr2[3, ]
  
  list(
    estimate = function(y_bw) {
      yL <- y_bw[idx_L]; yR <- y_bw[idx_R]
      (sum(aR * yR) - sum(aL * yL)) - h^2 * (sum(bR * yR) - sum(bL * yL))
    },
    # Vectorized: takes a matrix Y where each column is one y_bw, returns
    # a numeric vector of length ncol(Y), one estimate per column.
    estimate_matrix = function(Y_bw) {
      YL <- Y_bw[idx_L, , drop = FALSE]
      YR <- Y_bw[idx_R, , drop = FALSE]
      as.numeric(
        (aR %*% YR) - (aL %*% YL) - h^2 * ((bR %*% YR) - (bL %*% YL))
      )
    }
  )
}


# ---- helper: same operator, with covariates partialled out -----------------
#
# rdrobust(covs = Z) is equivalent (for the point estimate) to the local
# polynomial estimator on the covariate-residualized outcome, where the
# residualization uses the same kernel weights.

build_rdd_operator_with_covs <- function(x_bw, h, covs_bw) {
  op    <- build_rdd_operator(x_bw, h)
  Z     <- as.matrix(covs_bw)
  w     <- 1 - abs(x_bw) / h
  WZ    <- Z * w
  Hcov  <- solve(crossprod(Z, WZ), t(WZ))   # equals (Z'WZ)^{-1} Z'W
  
  list(
    estimate = function(y_bw) {
      coefs   <- as.numeric(Hcov %*% y_bw)
      y_resid <- y_bw - as.numeric(Z %*% coefs)
      op$estimate(y_resid)
    },
    estimate_matrix = function(Y_bw) {
      Coefs   <- Hcov %*% Y_bw
      Y_resid <- Y_bw - Z %*% Coefs
      op$estimate_matrix(Y_resid)
    }
  )
}


# ---- main function ---------------------------------------------------------

run_mc_rdd_bootstrap <- function(
    data,
    outcome_var    = "liberal_index",
    birth_year_var = "year_of_birth",
    controls       = NULL,
    cutoff_date    = as.Date("1994-11-06"),
    sample_label   = NULL,
    n_sim          = 1000,
    n_boot         = 500,
    seed           = 42,
    n_cores        = 1L,
    run_diagnostic = TRUE,    # compare operator vs rdrobust on iter 1
    verbose        = TRUE
) {
  # ── Labels ────────────────────────────────────────────────────────────────
  sample_label <- dplyr::case_when(
    sample_label == "df"              ~ "Full Sample",
    sample_label == "df_democrats"    ~ "Democrats",
    sample_label == "df_republicans"  ~ "Republicans",
    sample_label == "df_independents" ~ "Independents",
    sample_label == "df_partisans"    ~ "Partisans",
    sample_label == "df_inpower"      ~ "In-Power",
    sample_label == "df_outofpower"   ~ "Out-of-Power",
    TRUE ~ paste0(sample_label, " is an unknown dataset name")
  )
  model_label <- if (is.null(controls)) "Without Controls" else "With Controls"
  
  # ── Prepare data once ─────────────────────────────────────────────────────
  # Complete-case filter on EVERYTHING the analysis touches: birth year,
  # outcome, and (if applicable) controls. The original lm()-based code
  # silently dropped rows with missing controls via na.action = na.omit;
  # .lm.fit doesn't, so we have to be explicit. rdrobust also internally
  # drops NA cases — by filtering up front we keep all components on the
  # same sample.
  needed_vars <- c(birth_year_var, outcome_var, controls)
  ok          <- stats::complete.cases(data[, needed_vars, drop = FALSE])
  n_dropped   <- sum(!ok)
  data <- data[ok, , drop = FALSE]
  by   <- as.integer(data[[birth_year_var]])
  y    <- as.numeric(data[[outcome_var]])
  covs <- if (!is.null(controls)) as.matrix(data[, controls, drop = FALSE]) else NULL
  n    <- length(y)
  if (verbose && n_dropped > 0) {
    message(sprintf("Dropped %d rows with NA in birth year, outcome, or controls (%d remaining).",
                    n_dropped, n))
  }
  # Catch column-typed problems early so we fail loudly, not deep in the loop.
  if (!is.null(covs)) {
    if (!is.numeric(covs)) {
      stop("Controls must be numeric. Convert factors to dummies before calling. Offending columns: ",
           paste(controls[!vapply(data[, controls, drop = FALSE], is.numeric, logical(1))], collapse = ", "))
    }
    if (any(!is.finite(covs))) {
      stop("Non-finite values (NaN or Inf) found in controls after NA-filter. Check for divide-by-zero or log(0) in your control construction.")
    }
  }
  if (any(!is.finite(y))) {
    stop("Non-finite values (NaN or Inf) found in outcome variable.")
  }
  
  # Precompute integer day-offsets from cutoff to first-of-month for each
  # birth year present in the data. Saves us calling as.Date() in the loop.
  unique_years <- sort(unique(by))
  first_of_month_offsets <- vapply(unique_years, function(yy) {
    as.integer(
      as.Date(paste0(yy, "-", sprintf("%02d", 1:12), "-01")) - cutoff_date
    )
  }, integer(12))   # rows = month, cols = year
  year_idx <- match(by, unique_years)
  month_days <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
  
  # ── One outer iteration ───────────────────────────────────────────────────
  do_outer <- function(i, sub_seed, do_diag = FALSE) {
    set.seed(sub_seed)
    
    # Step 1: simulate birth dates → integer running variable
    sim_month <- sample.int(12L, n, replace = TRUE)
    sim_day   <- ceiling(runif(n) * month_days[sim_month])
    x <- first_of_month_offsets[cbind(sim_month, year_idx)] + sim_day - 1L
    
    # Step 2: real rdrobust call → MSE-optimal bandwidth, point estimate
    fit <- tryCatch(
      suppressWarnings(
        if (is.null(covs))
          rdrobust::rdrobust(y = y, x = x, c = 0, all = TRUE)
        else
          rdrobust::rdrobust(y = y, x = x, c = 0, covs = covs, all = TRUE)
      ),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      return(list(
        estimate = NA_real_, bw = NA_real_, n_used = NA_integer_,
        null_estimates = rep(NA_real_, n_boot),
        diag = NULL
      ))
    }
    
    h     <- fit$bws[1, 1]
    in_bw <- abs(x) <= h
    x_bw  <- x[in_bw]
    y_bw  <- y[in_bw]
    kw    <- 1 - abs(x_bw) / h
    n_bw  <- length(x_bw)
    
    # Step 3: null model via .lm.fit (smooth trend, no jump)
    if (is.null(covs)) {
      X_null  <- cbind(1, x_bw)
      covs_bw <- NULL
    } else {
      covs_bw <- covs[in_bw, , drop = FALSE]
      X_null  <- cbind(1, x_bw, covs_bw)
    }
    sw       <- sqrt(kw)
    null_fit <- .lm.fit(X_null * sw, y_bw * sw)
    coefs    <- null_fit$coefficients
    y_hat    <- as.numeric(X_null %*% coefs)
    resids   <- y_bw - y_hat
    
    # Step 4: build operator once for this outer iter
    op <- if (is.null(covs))
      build_rdd_operator(x_bw, h)
    else
      build_rdd_operator_with_covs(x_bw, h, covs_bw)
    
    # Step 5: vectorized inner bootstrap — build all n_boot fake outcomes
    # as columns of a matrix, then apply the operator in one BLAS call.
    resid_idx <- matrix(
      sample.int(n_bw, n_bw * n_boot, replace = TRUE),
      nrow = n_bw, ncol = n_boot
    )
    Y_fake   <- matrix(resids[resid_idx], nrow = n_bw, ncol = n_boot) + y_hat
    null_est <- tryCatch(op$estimate_matrix(Y_fake),
                         error = function(e) rep(NA_real_, n_boot))
    
    # ── Diagnostic (only on outer iter 1) ─────────────────────────────────
    diag <- NULL
    if (do_diag) {
      # Recompute the same n_boot null estimates with full rdrobust at fixed h.
      # We pass b = h to match the operator's rho=1 convention; this is a
      # check of arithmetic equivalence, not of the rho choice.
      slow_est <- numeric(n_boot)
      for (b in seq_len(n_boot)) {
        slow_est[b] <- tryCatch(
          suppressWarnings(
            if (is.null(covs))
              rdrobust::rdrobust(y = Y_fake[, b], x = x_bw, c = 0,
                                 h = h, b = h, all = TRUE)$coef[2]
            else
              rdrobust::rdrobust(y = Y_fake[, b], x = x_bw, c = 0,
                                 h = h, b = h, covs = covs_bw, all = TRUE)$coef[2]
          ),
          error = function(e) NA_real_
        )
      }
      ok_pair <- !is.na(slow_est) & !is.na(null_est)
      diag <- list(
        n_compared    = sum(ok_pair),
        correlation   = if (sum(ok_pair) > 1) cor(null_est[ok_pair], slow_est[ok_pair]) else NA_real_,
        max_abs_diff  = if (sum(ok_pair) > 0) max(abs(null_est[ok_pair] - slow_est[ok_pair])) else NA_real_,
        mean_abs_diff = if (sum(ok_pair) > 0) mean(abs(null_est[ok_pair] - slow_est[ok_pair])) else NA_real_,
        operator_mean = mean(null_est[ok_pair]),
        rdrobust_mean = mean(slow_est[ok_pair])
      )
    }
    
    list(
      estimate       = fit$coef[2],
      bw             = h,
      n_used         = fit$N_h[1] + fit$N_h[2],
      null_estimates = null_est,
      diag           = diag
    )
  }
  
  # ── Run outer loop ────────────────────────────────────────────────────────
  if (verbose) message(sprintf("------ Bootstrap: %s (%s) | S=%d B=%d cores=%d ------",
                               sample_label, model_label, n_sim, n_boot, n_cores))
  start_time <- Sys.time()
  
  set.seed(seed)
  sub_seeds <- sample.int(.Machine$integer.max, n_sim)
  
  # Iter 1 separately, with diagnostic
  if (verbose && run_diagnostic) message("Running iter 1 with diagnostic check vs. rdrobust...")
  result_1 <- do_outer(1, sub_seeds[1], do_diag = run_diagnostic)
  
  if (run_diagnostic && !is.null(result_1$diag)) {
    d <- result_1$diag
    if (verbose) {
      message(sprintf(
        "Diagnostic: n=%d compared | cor=%.6f | max|diff|=%.2e | mean|diff|=%.2e",
        d$n_compared, d$correlation, d$max_abs_diff, d$mean_abs_diff
      ))
      if (!is.na(d$correlation) && d$correlation < 0.99) {
        warning("Operator and rdrobust null estimates have correlation < 0.99 — investigate before trusting bootstrap p-values.")
      }
    }
  }
  
  # Iters 2..n_sim, optionally in parallel
  do_one <- function(i) do_outer(i, sub_seeds[i], do_diag = FALSE)
  rest_idx <- if (n_sim >= 2) 2:n_sim else integer(0)
  
  if (length(rest_idx) > 0 && n_cores > 1L) {
    if (.Platform$OS.type != "windows") {
      results_rest <- parallel::mclapply(
        rest_idx, do_one,
        mc.cores = n_cores, mc.preschedule = TRUE
      )
    } else {
      cl <- parallel::makeCluster(n_cores)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      parallel::clusterExport(
        cl,
        c("do_outer", "build_rdd_operator", "build_rdd_operator_with_covs",
          "y", "covs", "n", "month_days", "first_of_month_offsets",
          "year_idx", "sub_seeds", "n_boot", "%||%"),
        envir = environment()
      )
      parallel::clusterEvalQ(cl, library(rdrobust))
      results_rest <- parallel::parLapply(cl, rest_idx, do_one)
    }
  } else if (length(rest_idx) > 0) {
    results_rest <- vector("list", length(rest_idx))
    for (k in seq_along(rest_idx)) {
      i <- rest_idx[k]
      if (verbose && i %% 50 == 0) {
        elapsed  <- difftime(Sys.time(), start_time, units = "mins")
        expected <- elapsed * n_sim / i
        message(sprintf("Outer iter %d/%d | elapsed %.2f min | expected total %.2f min",
                        i, n_sim, as.numeric(elapsed), as.numeric(expected)))
      }
      results_rest[[k]] <- do_one(i)
    }
  } else {
    results_rest <- list()
  }
  
  results <- c(list(result_1), results_rest)
  
  # ── Collapse ──────────────────────────────────────────────────────────────
  outer_draws <- data.frame(
    sim      = seq_len(n_sim),
    estimate = vapply(results, `[[`, numeric(1), "estimate"),
    bw       = vapply(results, `[[`, numeric(1), "bw"),
    n_used   = vapply(results, function(r) as.integer(r$n_used %||% NA_integer_),
                      integer(1)),
    n_total  = n,
    sample   = sample_label,
    model    = model_label,
    outcome  = "Liberal Democratic Norms Index (0-1 factor score; higher = stronger liberal norm support)",
    weighted = "No"
  )
  
  null_estimates <- unlist(lapply(results, `[[`, "null_estimates"), use.names = FALSE)
  null_estimates <- null_estimates[!is.na(null_estimates)]
  
  tau_bar  <- mean(outer_draws$estimate, na.rm = TRUE)
  sigma_mc <- sd(outer_draws$estimate,   na.rm = TRUE)
  p_boot   <- mean(abs(null_estimates) >= abs(tau_bar))
  ci       <- quantile(outer_draws$estimate, c(0.025, 0.975), na.rm = TRUE)
  
  summary_out <- data.frame(
    Sample        = sample_label,
    Model         = model_label,
    Estimate.Type = "Monte Carlo (Bootstrap)",
    Estimate      = tau_bar,
    SE            = sigma_mc,
    CI_lower      = ci[[1]],
    CI_upper      = ci[[2]],
    SE_lower      = (tau_bar - ci[[1]]) / 1.96,
    SE_upper      = (ci[[2]] - tau_bar) / 1.96,
    P.Value       = p_boot,
    Bandwidth.h   = round(mean(outer_draws$bw, na.rm = TRUE), 2),
    N_total       = n,
    N_within_bw   = as.integer(round(mean(outer_draws$n_used, na.rm = TRUE))),
    Controls_Used = if (is.null(controls)) "None" else paste(controls, collapse = "; ")
  )
  
  null_df <- data.frame(
    null_estimate = null_estimates,
    sample        = sample_label,
    model         = model_label
  )
  
  if (verbose) {
    total_time <- difftime(Sys.time(), start_time, units = "mins")
    message(sprintf("Done in %.2f minutes. tau_bar = %.4f | p_boot = %.4f",
                    as.numeric(total_time), tau_bar, p_boot))
  }
  
  list(
    summary        = summary_out,
    outer_draws    = outer_draws,
    null_estimates = null_df,
    diagnostic     = result_1$diag    # NULL if run_diagnostic = FALSE
  )
}