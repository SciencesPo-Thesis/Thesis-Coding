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
    filter(!is.na(.data[[party_id]])) %>%
    filter(days_from_cutoff >= -4000)
  
  is_expectations <- all(na.omit(dataframe[[party_id]]) %in% c(0, 1))
  
  df_all <- dataframe %>%
    mutate(
      subgroup = if (is_expectations) {
        case_when(
          .data[[party_id]] == 1 ~ "Expect In-Party Win",
          .data[[party_id]] == 0 ~ "Expect In-Party Loss"
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
    c("Expect In-Party Win", "Expect In-Party Loss")
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
    geom_point(alpha = 0.5) +
    geom_smooth(
      aes(group = factor(treatment)),
      method = "lm",
      formula = y ~ poly(x, 1),
      se = TRUE,
      color = "black",
      alpha = 0.75
    ) +
    geom_vline(xintercept = 18, linetype = "dashed", color = "black") +
    scale_color_brewer(
      palette = "Dark2",
      name    = "2012 Voting Eligibility",
      labels  = c("Eligible", "Not Eligible")
    ) +
    labs(
      x        = "Age at 2012 Election",
      y        = paste0(outcome_name, " Index (0 – 1)"),
      title    = paste("Discontinuity in", outcome_name),
      subtitle = if (is_expectations) "By Expected Election Outcome" else "Among Different Partisanship Categories"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title      = element_text(face = "bold", hjust = 0.5),
      plot.subtitle   = element_text(hjust = 0.5)
    ) +
    facet_wrap(~subgroup, scales = "free_y", ncol = colnumber) +
    scale_x_continuous(breaks = seq(10, 30, by = 2))
  
  # Full-sample plot (skipped for expectations variable)
  if (is_expectations) {
    list(plot_subgroups = plot_subgroups)
  } else {
    plot_all <- ggplot(dataframe, aes(x = age_2012_election, y = .data[[outcome]], color = factor(treatment))) +
      geom_point(alpha = 0.5) +
      geom_smooth(
        aes(group = factor(treatment)),
        method = "lm",
        formula = y ~ poly(x, 1),
        se = TRUE,
        color = "black",
        alpha = 0.75
      ) +
      geom_vline(xintercept = 18, linetype = "dashed", color = "black") +
      scale_color_brewer(
        palette = "Dark2",
        name    = "2012 Voting Eligibility",
        labels  = c("Eligible", "Not Eligible")
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
      scale_x_continuous(breaks = seq(10, 30, by = 2))
    
    list(
      plot_all       = plot_all,
      plot_subgroups = plot_subgroups
    )
  }
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
pull_estimates <- function(r) {
  tibble(
    coef_conv = r$coef[1], se_conv = r$se[1], pv_conv = r$pv[1],
    coef_bc   = r$coef[2], se_bc   = r$se[2], pv_bc   = r$pv[2],
    coef_rob  = r$coef[3], se_rob  = r$se[3], pv_rob  = r$pv[3]
  )
}
#------------------------------------------------------------------------------#



#------------------------------------------------------------------------------#
# Function that extracts the core values of the RD as a table to easily look at them
## Returns one row per estimate type (Conventional, Bias-Corrected, Bias-Corrected + Robust SE)
## Preferred specification for main text is row 2: Bias-Corrected 
extract_rdd_summary <- function(rd_object, model_label = "Model") {
  est <- pull_estimates(rd_object)
  tibble(
    Model           = model_label,
    `Estimate Type` = c("Conventional", "Bias-Corrected", "Bias-Corrected (Robust SE)"),
    Estimate        = round(c(est$coef_conv, est$coef_bc, est$coef_rob), 2),
    SE              = round(c(est$se_conv,   est$se_bc,   est$se_rob),   2),
    `P-Value`       = round(c(est$pv_conv,   est$pv_bc,   est$pv_rob),   2),
    `Bandwidth Type`= ifelse(rd_object$bwselect == "mserd", "MSE-optimal", rd_object$bwselect),
    `Bandwidth (h)` = round(rd_object$bws[1], 2),
    N               = rd_object$N_h[1] + rd_object$N_h[2]
  )
}
#------------------------------------------------------------------------------#



#------------------------------------------------------------------------------#
# Function adapting the use of the rdrobust::rdrobust() for the purposes of this project
run_rdd_models <- function(data, index_var, controls, sample_label) {
  # Extract outcome + running variable
  y <- data[[index_var]]
  x <- data$days_from_cutoff
  
  # Simple
  rdd_simple <- rdrobust(
    y = y,
    x = x,
    c = 0,
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
    c = 0,
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
          pull_estimates(r) %>% mutate(bandwidth = h, controls = use_covs, h_opt = h_opt)
        },
        error = function(e) {
          message("Bandwidth = ", h, " (controls = ", use_covs, "): ", e$message)
          NULL
        }
      )
    })
  }) %>% mutate(sample = sample_label)
  
  #  Placebo cutoffs 
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
          pull_estimates(r) %>% mutate(cutoff = co, controls = use_covs)
        },
        error = function(e) {
          message("Placebo cutoff = ", co, " (controls = ", use_covs, "): ", e$message)
          NULL
        }
      )
    })
  }) %>% mutate(sample = sample_label)
  
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
          pull_estimates(r) %>% mutate(polynomial = p, controls = use_covs)
        },
        error = function(e) {
          message("Polynomial = ", p, " (controls = ", use_covs, "): ", e$message)
          NULL
        }
      )
    })
  }) %>% mutate(sample = sample_label)
  
  bind_rows(
    bw_results   %>% mutate(check_type = "bandwidth"),
    cutoff_results  %>% mutate(check_type = "cutoffs"),
    poly_results %>% mutate(check_type = "polynomial")
  )}
#------------------------------------------------------------------------------#



#------------------------------------------------------------------------------#
# Function for plotting robustness checks
## estimate_type: 1 = Conventional, 2 = Bias-Corrected, 3 = BC + Robust SE (preferred)
get_robustnessplots <- function(data,
                                subgroup_name,
                                estimate_type = 3,
                                include_covariates = TRUE) {
  
  bw_data   <- data %>% filter(check_type == "bandwidth")
  co_data   <- data %>% filter(check_type == "cutoffs")
  poly_data <- data %>% filter(check_type == "polynomial")
  # Select coefficient and SE columns for the chosen estimate type —
  # used consistently across all three panels
  cols <- switch(as.character(estimate_type),
                 "1" = c("coef_conv", "se_conv"),
                 "2" = c("coef_bc", "se_bc"),
                 "3" = c("coef_rob", "se_rob"),
                 stop("estimate_type must be 1, 2, or 3")
  )
  
  estimate_name <- switch(as.character(estimate_type),
                          "1" = "Conventional",
                          "2" = "Bias-Corrected",
                          "3" = "Bias-Corrected (Robust SE)"
  )
  
  control_label <- if (include_covariates) "(With Controls)" else "(Without Controls)"
  
  # Filter to the requested subgroup and attach estimate/se columns
  prep <- function(df, filter_controls = NA) {
    out <- df %>% filter(sample == subgroup_name)
    if (!is.na(filter_controls)) out <- out %>% filter(controls == filter_controls)
    out %>% mutate(
      estimate = .data[[cols[1]]],
      se       = .data[[cols[2]]]
    )
  }
  
  bw_plot_data   <- prep(bw_data,   filter_controls = include_covariates)
  co_plot_data   <- prep(co_data,   filter_controls = include_covariates)
  poly_plot_data <- prep(poly_data, filter_controls = include_covariates)
  
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
  
  h_opt_val <- bw_plot_data$h_opt[1]
  
  panel_bw <- make_panel(
    bw_plot_data, "bandwidth",
    "Bandwidth (days)",
    "Bandwidth Sensitivity",
    "(1st-order polynomial, cutoff = 0)"
  ) +
    geom_vline(xintercept = h_opt_val, linetype = "dashed", colour = "grey50", linewidth = 0.5)
  
  panel_co <- make_panel(
    co_plot_data, "cutoff",
    "Placebo cutoff (days from true cutoff)",
    "Placebo Cutoffs",
    "(MSE-optimal bandwidth, 1st-order polynomial)"
  )
  
  panel_poly <- make_panel(
    poly_plot_data, "polynomial",
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
    n_sim = 10000,
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
   sample_label == "df_inparty_win" ~ "Expect In-Party Win",
   sample_label == "df_inparty_loss" ~ "Expect In-Party Loss",
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
  out <- data.frame(
    sim      = seq_len(n_sim),
    estimate = NA_real_,
    bw       = NA_real_,
    n_used   = NA_integer_,
    sample   = sample_label,
    model    = model_label
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
    title     = "Estimated Effects Across Subgroups",   # override for cohort OLS vs RDD
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
    title     = "Estimated Effects Across Subgroups",   # override for cohort OLS vs RDD
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


# Function for robustness-type coefplots


