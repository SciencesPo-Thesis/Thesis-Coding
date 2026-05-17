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

# Infix negation of %in% — convenient for excluding codes in case_when() and filter().
# Example:  filter(party %notin% c(-9, -8, 0, 5))  reads more naturally than
#           filter(!(party %in% c(-9, -8, 0, 5))).
`%notin%` <- Negate(`%in%`)


# ─── na_recode() ─────────────────────────────────────────────────────────────
# ANES uses negative integer codes to signal item non-response. The most common are:
#   -9 = refused to answer
#   -8 = don't know / not sure
#   -7 = not applicable (routing skipped this question for this respondent)
#   -6 = not asked in this wave (pre-election vs. post-election split questionnaire)
#   -5, -4, -3, -2, -1 = various other out-of-range / not-applicable codes
# We replace every value listed in `invalid` with NA so that downstream functions
# (mean, sd, lm, rdrobust, psych::fa) treat them as missing — not as real
# negative-valued data points that would corrupt any numerical calculation.
na_recode <- function(x, invalid) {
  if_else(x %in% invalid, NA_integer_, x)
}


# ─── voted_recode() ──────────────────────────────────────────────────────────
# The ANES "did you vote?" questions code confirmed voters as 1; all other
# responses (2 = "no", and various skip codes) are NOT confirmed votes.
# This collapses everything except confirmed-voter (1) to 0, creating a clean
# 0/1 turnout dummy suitable for regression or subsetting.
voted_recode <- function(x) {
  if_else(x == 1, x, 0)
}


# ─── order_recode() ──────────────────────────────────────────────────────────
# Reverses the direction of a Likert scale so that higher values consistently
# mean "more" of the theoretical construct (e.g. more liberal, more democratic).
# ANES sometimes codes 1 = "strongly agree" and 5 = "strongly disagree", so
# a raw correlation would be negatively signed relative to what we want.
# Passing range_plus1 = 6 (for a 1–5 scale) flips the direction:
#   original 1 → 6 - 1 = 5   (maximum after reversal)
#   original 5 → 6 - 5 = 1   (minimum after reversal)
# We apply this before standardizing items so factor loadings are all positive.
order_recode <- function(x, range_plus1){
  range_plus1 - x
}


# ─── binary_recode() ─────────────────────────────────────────────────────────
# ANES binary variables sometimes use 1 = Yes / 2 = No rather than the
# conventional 1/0 dummy encoding. Without this recode, a "No" response would
# appear as 2 in regression, giving it twice the coefficient weight of "Yes" —
# nonsensical for a qualitative distinction. This converts 2 → 0 so the variable
# is a proper 0/1 indicator and OLS coefficients represent a Yes-vs-No contrast.
binary_recode <- function(x) {
  if_else(x == 2, 0, x)
}


# ─── binary_tranpose() ───────────────────────────────────────────────────────
# Flips a 0/1 variable: 0 → 1 and 1 → 0. Used when ANES codes the "pro-democratic"
# response as 0 but our index should load positively (higher score = more
# liberal/democratic). Example: an item where 1 = "don't trust government" and
# 0 = "trust government" would load negatively on a liberal-attitudes factor;
# transposing it (so 1 = trust) makes all items point in the same direction before
# factor analysis, producing positive loadings throughout.
binary_tranpose <- function(x) {
  if_else(x == 1, 0, 1)
}







####################################
########## Plot Functions ##########
####################################


##### Scree-plot function #####
# A scree plot shows each factor's eigenvalue in descending order. Eigenvalues
# represent the proportion of total variance explained by each factor.
# The canonical decision rule (Kaiser criterion) retains all factors with
# eigenvalue > 1 — i.e. factors that explain more variance than a single item.
# We also look for the "elbow": the point where the line bends and eigenvalues
# drop off sharply. Everything above the elbow is worth retaining.
# Because we always extract nfactors = 1 in this project (a single liberal-
# attitudes dimension), the scree plot is used mainly to CONFIRM that one factor
# dominates and a second factor does not also exceed 1 — which would suggest the
# construct is multidimensional and our items need to be revised.

get_screeplot <- function(outcome, outcomename) {
  # outcome$values: vector of eigenvalues returned by psych::fa()
  # 1:length(...) creates the x-axis index (Factor 1, Factor 2, ...)
  # type = "b" draws both points and connecting lines (b = "both")
  plot(1:length(outcome$values), outcome$values,
    type = "b",
    ylab = "Eigenvalue", xlab = "Factor", main = paste("Scree plot —", outcomename)
  )
}


##### Define function for coefplots #####

get_coefplot <- function(
    dataframe,
    colnumber = 3,
    facet_var = "Outcome",
    title     = "Estimated Effects Across Subgroups",   # override for cohort OLS vs RDD
    subtitle  = "Bias-Adjusted / OLS Estimates",
    caption   = NULL
) {
  # facet_var: "Outcome" (default) = one panel per outcome variable;
  #            "Method"           = one panel per estimation method (used in MC validation plot).
  #            passed as a string and converted to a formula inside facet_wrap().

  # ── Sign inversion ──────────────────────────────────────────────────────────
  # The estimates stored in `dataframe` follow the convention:
  #   RDD (run_rdd_models):   Estimate = Pre-Trump − Post-Trump
  #     (because rdrobust returns E[Y|above cutoff] − E[Y|below cutoff], and "above"
  #     the age cutoff = older = pre-Trump when age is the running variable)
  #   Cohort OLS (after negation in usa_thesis.R):  also stored as Pre-Trump − Post-Trump
  # We negate here so the x-axis reads Post-Trump − Pre-Trump, where:
  #   positive x = Post-Trump cohort scores HIGHER (e.g. more democratic attitudes)
  #   negative x = Post-Trump cohort scores LOWER (H1 prediction)
  # This is the natural reader-facing direction: the thing we expect to be negative
  # (Post-Trump scoring lower on democratic norms) sits to the left of the null line.
  dataframe <- dataframe %>%
    mutate(Estimate_Inverse = -Estimate)

  ggplot(dataframe, aes(y = Sample, x = Estimate_Inverse, color = Model)) +
    # Dot for the point estimate; position_dodge() separates the two Model specs vertically
    geom_point(position = position_dodge(width = 0.6), size = 2.5) +
    # 95% CI as a horizontal error bar; ±1.96 × SE assumes approximate normality of the
    # estimator (valid for both OLS and rdrobust bias-corrected estimates in large samples)
    geom_errorbarh(
      aes(xmin = Estimate_Inverse - 1.96 * SE, xmax = Estimate_Inverse + 1.96 * SE),
      height = 0.25,
      position = position_dodge(width = 0.6)
    ) +
    # Dashed vertical line at x = 0: the null hypothesis of no cohort effect
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    # Orange = Without Controls; orchid = With Controls (consistent across all plots)
    scale_color_manual(values = c("orange2", "orchid4")) +
    labs(
      title    = title,
      subtitle = subtitle,
      x        = "Estimated Effect (Post-Trump \u2212 Pre-Trump)",
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


##### Define function for cross-method coefficient plot: RDD vs. Cohort OLS #####
# This plot puts the RDD (causal) and cohort OLS (observational) estimates on the
# same axes so you can immediately see if both methods agree.
#
#   WHY THIS MATTERS:
#   • If RDD ≈ Cohort OLS, it means the cohort comparison is not badly confounded
#     even though OLS does not restrict itself to observations near the cutoff.
#   • If they diverge, the OLS estimate is picking up something the RDD is not
#     (e.g. a broader age trend that the demographic controls didn't fully absorb).
#
#   INPUT CONVENTION:
#   Both input dataframes must be sign-aligned BEFORE passing them here.
#   In usa_thesis.R both rdd_liberal and cohort_liberal are stored with
#   Estimate = Pre-Trump − Post-Trump (the sign is negated inside for display).
#   get_coefplot_methods() applies −Estimate for display → positive = Post-Trump higher.
#
#   AESTHETICS:
#   • shape = method:  circle (16) = RDD  |  triangle (17) = Cohort OLS
#   • color = model:   orange = Without Controls  |  orchid = With Controls

get_coefplot_methods <- function(rdd_df, cohort_df, colnumber = 1) {
  # Stack both method dataframes and tag each with a Method label
  combined <- bind_rows(
    rdd_df    %>% select(Sample, Model, Estimate, SE, Outcome) %>% mutate(Method = "RDD (Causal)"),
    cohort_df %>% select(Sample, Model, Estimate, SE, Outcome) %>% mutate(Method = "Cohort OLS")
  ) %>%
    # Invert for display: stored values are Pre − Post; we show Post − Pre
    mutate(Estimate_Inverse = -Estimate)

  ggplot(combined, aes(y = Sample, x = Estimate_Inverse, color = Model, shape = Method)) +
    geom_point(position = position_dodge(width = 0.6), size = 2.5) +
    geom_errorbarh(
      aes(xmin = Estimate_Inverse - 1.96 * SE, xmax = Estimate_Inverse + 1.96 * SE),
      height = 0.25,
      position = position_dodge(width = 0.6)
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    scale_color_manual(values = c("orange2", "orchid4"), name = "Specification") +
    scale_shape_manual(
      values = c("RDD (Causal)" = 16, "Cohort OLS" = 17),
      name   = "Method"
    ) +
    labs(
      title    = "Cross-Method Comparison: RDD vs. Cohort OLS",
      subtitle = "Circles = RDD (causal) \u2502 Triangles = Cohort OLS (observational)\nPositive x = Post-Trump cohort scores higher on the outcome",
      x        = "Estimated Effect (Post-Trump \u2212 Pre-Trump)",
      y        = NULL
    ) +
    theme_bw() +
    theme(
      plot.title      = element_text(face = "bold", hjust = 0.5),
      plot.subtitle   = element_text(hjust = 0.5),
      legend.position = "bottom",
      plot.caption    = ggtext::element_markdown()
    ) +
    facet_wrap(~Outcome, ncol = colnumber)
}


##### Define function for comparing Standard RDD vs Monte Carlo RDD #####
# Used for the validation table plot. All estimates appear in a single panel
# (or one panel per outcome) so that the two methods can be read off the same
# axis. Shape encodes the estimation method — distinguishable in black & white —
# while colour encodes the model specification (with / without controls).
#   • circle  (shape 16) = Standard RDD
#   • triangle (shape 17) = Monte Carlo RDD

get_coefplot_comparison <- function(dataframe, colnumber = 1) {
  # Apply the same sign-inversion as get_coefplot():
  # both Standard RDD and MC RDD store Estimate = Pre − Post; we display Post − Pre.
  # See the detailed comment in get_coefplot() for the rationale.
  dataframe <- dataframe %>%
    mutate(Estimate_Inverse = -Estimate)

  ggplot(dataframe, aes(y = Sample, x = Estimate_Inverse,
                        color = Model, shape = Method)) +
    # Two aesthetics distinguish the methods:
    #   color = specification (orange = without controls, orchid = with controls)
    #   shape = method (circle = Standard RDD, triangle = Monte Carlo RDD)
    # This dual encoding means the plot is readable in black-and-white (shape alone
    # distinguishes methods) AND in color (color alone distinguishes specifications).
    geom_point(position = position_dodge(width = 0.5), size = 2.5) +
    geom_errorbarh(
      aes(xmin = Estimate_Inverse - 1.96 * SE, xmax = Estimate_Inverse + 1.96 * SE),
      height = 0.2,
      position = position_dodge(width = 0.5)
    ) +
    # Null reference line at x = 0 (no cohort effect)
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    scale_color_manual(values = c("orange2", "orchid4")) +
    # Circles (16) = Standard RDD (age-based running variable, exact cutoff at 26)
    # Triangles (17) = Monte Carlo RDD (simulated birth-year running variable)
    # If both shapes overlap tightly, the MC simulation replicates the standard RDD.
    scale_shape_manual(values = c("Standard RDD" = 16, "Monte Carlo RDD" = 17)) +
    labs(
      title    = "Standard RDD vs Monte Carlo RDD",
      subtitle = "Bias-Adjusted Estimates",
      x        = "Estimated RD Effect",
      y        = NULL,
      color    = "Specification",
      shape    = "Method"
    ) +
    theme_bw() +
    theme(
      plot.title    = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom",
      plot.caption  = ggtext::element_markdown()
    ) +
    facet_wrap(~Outcome, ncol = colnumber)
}


# ─────────────────────────────────────────────────────────────────────────────
# get_mc_histogram()
#
# Diagnostic plot: histogram of all n_sim MC estimates with a Normal density
# overlay and the mean marked as a vertical line. The main purpose is to
# verify that the sampling distribution of the Monte Carlo estimates is
# approximately normal — a prerequisite for the mean ± 1.96 × SD confidence
# interval formula to be valid. If the distribution is skewed or bimodal,
# percentile-based CIs should be used instead.
#
# SIGN NOTE:
#   The draws returned by run_mc_rdd(return_draws = TRUE) use the BIRTH-YEAR
#   running variable, so the raw estimates are already τ = post − pre
#   (positive = post-Trump cohort scores higher). This is the natural display
#   direction and no negation is applied here. The displayed mean will match
#   -Estimate in the mc_liberal tibble (which was negated to flip to pre − post
#   before being fed into get_coefplot()).
#
# ARGUMENTS:
#   draws        — numeric vector from run_mc_rdd(return_draws=TRUE)$draws.
#   outcome_name — string label for the x-axis.
#   model_label  — "Without Controls" or "With Controls" (for the title).
#   sample_label — subgroup label, e.g. "Full Sample".
#   title        — plot title; NULL = auto-generated.
# ─────────────────────────────────────────────────────────────────────────────

get_mc_histogram <- function(
    draws,
    outcome_name = "Liberal Attitudes",
    model_label  = "Without Controls",
    sample_label = "Full Sample",
    title        = NULL
) {
  mean_val <- mean(draws)
  sd_val   <- sd(draws)
  n_sim    <- length(draws)

  if (is.null(title))
    title <- paste0("MC Sampling Distribution \u2014 ", sample_label, " (", model_label, ")")

  draws_df <- data.frame(estimate = draws)

  ggplot(draws_df, aes(x = estimate)) +
    # Histogram normalized to density so the Normal curve overlays correctly
    geom_histogram(
      aes(y = after_stat(density)),
      fill  = "steelblue3",
      color = "white",
      bins  = 60,
      alpha = 0.8
    ) +
    # Normal(μ̂, σ̂²) density overlay — should match the histogram if distribution
    # is approximately normal, validating the mean ± 1.96 × SD CI formula
    stat_function(
      fun       = dnorm,
      args      = list(mean = mean_val, sd = sd_val),
      color     = "orchid4",
      linewidth = 1.0
    ) +
    # Mean of the sampling distribution = the MC point estimate
    geom_vline(
      xintercept = mean_val,
      color      = "orange2",
      linewidth  = 1.2
    ) +
    # Null hypothesis reference line
    geom_vline(
      xintercept = 0,
      color      = "gray50",
      linewidth  = 0.8,
      linetype   = "dashed"
    ) +
    # Annotate the mean value on the plot
    annotate(
      "text",
      x     = mean_val,
      y     = Inf,
      label = paste0("\u03bc = ", round(mean_val, 3)),
      hjust = -0.12,
      vjust = 1.8,
      color = "orange2",
      size  = 3.5,
      fontface = "bold"
    ) +
    scale_x_continuous(labels = scales::number_format(accuracy = 0.001)) +
    labs(
      x        = paste0(
        "\u03c4\u0302  (MC estimate on ", outcome_name,
        ")\nPositive = Post-Trump cohort scores higher"
      ),
      y        = "Density",
      title    = title,
      subtitle = paste0(
        "n_sim = ", n_sim, "  |  ",
        "\u03bc = ", round(mean_val, 3), "  |  ",
        "SD = ", round(sd_val, 3), "  |  ",
        "95% CI: [", round(mean_val - 1.96 * sd_val, 3), ", ",
                     round(mean_val + 1.96 * sd_val, 3), "]"
      ),
      caption  = paste0(
        "Orange = mean (point estimate). Orchid = Normal(\u03bc, SD\u00b2) overlay. Dashed = \u03c4 = 0 (null).\n",
        "If the histogram closely follows the Normal curve, the mean \u00b1 1.96 \u00d7 SD CI is valid."
      )
    ) +
    theme_bw() +
    theme(
      plot.title    = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 9),
      plot.caption  = element_text(hjust = 0, size = 8, color = "gray40")
    )
}


##### Define function for discontinuity plots for single outcome #####
#
# PURPOSE:
#   Produces two scatter-plus-fit plots that let you VISUALLY inspect the RDD
#   discontinuity at age 26 (the cohort cutoff). The x-axis is age in 2020;
#   the y-axis is the outcome index. If there is a genuine discontinuity at
#   the cutoff, the regression lines fitted separately for the two cohorts
#   (above/below age 26) will not join smoothly — there will be a visible
#   jump in level at age 26. That jump is what rdrobust() estimates formally.
#
# WHAT THIS PLOT IS AND ISN'T:
#   ● It IS a descriptive visual check — useful for seeing whether the
#     discontinuity "looks real" before running the formal RDD model.
#   ● It IS useful for spotting sorting / manipulation near the cutoff
#     (if one cohort is much larger or has very different variance just above/
#     below 26, that is a red flag for non-random assignment at the boundary).
#   ● It is NOT the RDD estimate itself — the linear fit spans the full age
#     range on each side (not just the local bandwidth), so it is not the
#     same as the local-linear rdrobust() estimate.
#
# RETURNS: a list with $plot_all (full sample) and $plot_subgroups (faceted).
#
# ARGUMENTS:
#   dataframe    — the full ANES dataframe
#   outcome      — string name of the index column (e.g. "liberal_index")
#   outcome_name — human-readable label for plot titles
#   colnumber    — number of columns in the subgroup facet grid

get_discontinuityplot <- function(dataframe, outcome = "liberal_index", outcome_name = "Liberal Attitudes", colnumber = 2) {

  # Drop respondents older than 60 (extreme ages have very small cells and can
  # distort the fitted lines without adding meaningful information near the cutoff).
  # Also drop those with missing party ID, since the subgroup plot requires it.
  dataframe <- dataframe %>%
    filter(age <= 60, !is.na(party))

  # Assign partisan subgroup labels to each respondent.
  # We use separate objects rather than modifying the filtered dataframe in place
  # because we need to STACK partisans into a separate "Partisans" facet below.
  df_all <- dataframe %>%
    mutate(
      subgroup = case_when(
        party == "Democrat"    ~ "Democrats",
        party == "Republican"  ~ "Republicans",
        party == "Independent" ~ "Independents"
      )
    )

  # Duplicate partisan rows with a "Partisans" label for its own facet.
  # This lets us show both the individual-party panels AND a combined Partisans panel
  # without double-counting in the full-sample plot (plot_all uses `dataframe` directly).
  df_partisans <- dataframe %>%
    filter(party %in% c("Democrat", "Republican")) %>%
    mutate(subgroup = "Partisans")

  # Stack the three-party breakdown with the duplicated partisans group.
  # After stacking, some respondents appear TWICE: once as Democrat/Republican
  # and once as Partisans. This is intentional — the facet panels are independent.
  df_plot <- bind_rows(df_all, df_partisans) %>%
    filter(age <= 60, !is.na(.data[[outcome]]), !is.na(subgroup)) %>%
    # Ordered factor controls the left-to-right panel order in facet_wrap()
    mutate(subgroup = factor(
      subgroup,
      levels = c("Independents", "Partisans", "Democrats", "Republicans")
    ))


  # ── Subgroup plot (faceted: one panel per partisan group) ──────────────────
  # Each panel shows:
  #   ● Colored dots = individual respondents (Dark2: green = pre-Trump, orange = post-Trump)
  #   ● Black regression lines fit separately on each side of the cutoff
  #     (poly(x,1) = linear fit; geom_smooth fits one per `group = treatment`)
  #   ● Dashed vertical line at age 26 = the RDD cutoff
  #
  # The gap between the two black lines AT age 26 is the visible discontinuity.
  # If the CI bands for the two lines overlap at the cutoff, the effect may not
  # be statistically significant for that subgroup.
  #
  # Note: the color = treatment here is applied to `geom_point` via `aes()` but
  # overridden with color = "black" inside `geom_smooth` — so the scatter is
  # colored by cohort but the fit lines are always black (to avoid confusion with
  # color-coded subgroup lines in other plots).
  plot_subgroups <- ggplot(df_plot, aes(x = age, y = .data[[outcome]], color = factor(treatment))) +
    geom_point(alpha = 0.5) +
    geom_smooth(
      aes(group = factor(treatment)),  # fit separately for treatment = 0 (pre) and 1 (post)
      method  = "lm",
      formula = y ~ poly(x, 1),        # linear fit on each side
      se      = TRUE,                  # show 95% CI band (gray ribbon)
      color   = "black",               # override the fill color so lines are always black
      alpha   = 0.75
    ) +
    # Vertical reference at the cutoff (age 26 = first year of post-Trump eligibility cohort)
    geom_vline(xintercept = 26, linetype = "dashed", color = "black") +
    scale_color_brewer(
      palette = "Dark2",
      name    = "2012 Voting Eligibility",
      labels  = c("Not Eligible (<26)", "Eligible (≥26)")
    ) +
    labs(
      x        = "Age in 2020",
      y        = NULL,
      title    = paste("Discontinuities in", outcome_name),
      subtitle = "Among Different Partisanship Categories"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title      = element_text(face = "bold", hjust = 0.5),
      plot.subtitle   = element_text(hjust = 0.5)
    ) +
    # scales = "free_y" allows each facet to rescale its y-axis independently:
    # useful because the index mean can vary substantially across partisan groups,
    # and a shared y-axis would compress the within-group variation to near-zero.
    facet_wrap(~subgroup, scales = "free_y", ncol = colnumber)


  # ── Full-sample plot (all respondents on a single panel) ───────────────────
  # Same visual logic as the subgroup plot but without faceting.
  # Provides the most statistical power (largest sample) and is the clearest
  # single-image representation of the overall discontinuity.
  plot_all <- ggplot(dataframe, aes(x = age, y = .data[[outcome]], color = factor(treatment))) +
    geom_point(alpha = 0.5) +
    geom_smooth(
      aes(group = factor(treatment)),
      method  = "lm",
      formula = y ~ poly(x, 1),
      se      = TRUE,
      color   = "black",
      alpha   = 0.75
    ) +
    geom_vline(xintercept = 26, linetype = "dashed", color = "black") +
    scale_color_brewer(
      palette = "Dark2",
      name    = "2012 Voting Eligibility",
      labels  = c("Not Eligible (<26)", "Eligible (≥26)")
    ) +
    labs(
      x        = "Age in 2020",
      y        = NULL,
      title    = paste("Discontinuities in", outcome_name),
      subtitle = "Independents, Democrats, and Republicans (Full Sample)"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title      = element_text(face = "bold", hjust = 0.5),
      plot.subtitle   = element_text(hjust = 0.5)
    )

  # Return both plots as a named list so the caller can display them separately:
  #   discontinuityplot$plot_all        → full sample
  #   discontinuityplot$plot_subgroups  → faceted by partisan group
  plots <- list(
    plot_all       = plot_all,
    plot_subgroups = plot_subgroups
  )
}


##### Define function for discontinuity plots for multiple outcomes but one subgroup #####
#
# PURPOSE:
#   Faceted version of get_discontinuityplot() that puts ALL outcome indices
#   on a single multi-panel figure. Each panel = one outcome. This is useful
#   for a bird's-eye robustness check: are discontinuities consistent across
#   all the anti-liberal-democracy dimensions, or does the effect concentrate
#   in just one or two outcomes?
#
# ARGUMENTS:
#   dataframe    — a pre-filtered subset (e.g. df_republicans), NOT the full df.
#                  The caller is responsible for subsetting before calling.
#   subset       — string label for the title (e.g. "Republicans").
#   primary_only — if TRUE (default), show only Populism, Authoritarianism, Nativism.
#                  Set FALSE to also include Traditionalism.
#   colnumber    — number of columns in the facet grid.

get_discontinuityplot_multipleoutcomes <- function(dataframe, subset, primary_only = TRUE, colnumber = 3) {

  # pivot_longer converts from wide format (one column per index) to long format
  # (one row per person × outcome combination). This is the standard ggplot2 idiom
  # for producing faceted plots across multiple outcome variables without writing
  # separate ggplot calls for each one.
  df_long <- dataframe %>%
    filter(age <= 60) %>%      # same age cap as get_discontinuityplot()
    pivot_longer(
      cols      = c(populism_index, authoritarianism_index, nativism_index, traditionalism_index),
      names_to  = "Outcome",   # new column holding the outcome name (as a string)
      values_to = "Value"      # new column holding the index score for that outcome
    ) %>%
    filter(!is.na(Value)) %>%  # drop missing values AFTER pivoting (avoids NAs in ribbon)
    mutate(
      # Rename from R column names (with underscores) to display labels
      Outcome = recode(
        Outcome,
        populism_index         = "Populism",
        authoritarianism_index = "Authoritarianism",
        nativism_index         = "Nativism",
        traditionalism_index   = "Traditionalism"
      ),
      # Ordered factor controls the panel order in the facet grid:
      # Nativism | Authoritarianism | Populism | (Traditionalism, if included)
      Outcome = factor(
        Outcome,
        levels = c("Nativism", "Authoritarianism", "Populism", "Traditionalism")
      )
    )

  # Traditionalism is a secondary index (more cultural/social than strictly
  # anti-democratic) and may be excluded from the main paper's figure.
  # primary_only = TRUE keeps only the three core radical-right dimensions.
  if (primary_only) {
    df_long <- df_long %>%
      filter(Outcome %in% c("Populism", "Authoritarianism", "Nativism"))
  }

  # Faceted discontinuity plot — same visual logic as get_discontinuityplot()
  # but the facet variable is Outcome instead of partisan subgroup.
  # scales = "free_y" allows each outcome panel to use its own y-axis scale,
  # since different indices may have different means and ranges after rescaling
  # (even though all are rescaled to 0–1, the density of values within that range varies).
  ggplot(df_long, aes(x = age, y = Value, color = factor(treatment))) +
    geom_point(alpha = 0.5) +
    geom_smooth(
      aes(group = factor(treatment)),
      method  = "lm",
      formula = y ~ poly(x, 1),
      se      = TRUE,
      color   = "black",
      alpha   = 0.75
    ) +
    geom_vline(xintercept = 26, linetype = "dashed", color = "black") +
    scale_color_brewer(
      palette = "Dark2",
      name    = "2012 Voting Eligibility",
      labels  = c("Not Eligible (<26)", "Eligible (≥26)")
    ) +
    labs(
      x        = "Age in 2020",
      y        = NULL,
      title    = paste("Discontinuities in Radical Right Attitudes Among", subset),
      subtitle = "Across Multiple Outcomes"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title      = element_text(face = "bold", hjust = 0.5),
      plot.subtitle   = element_text(hjust = 0.5)
    ) +
    facet_wrap(~Outcome, scales = "free_y", ncol = colnumber)
}


####################################
########## Cohort Functions ########
####################################


##### Extract cohort model summary #####
# Helper function: pulls the treatment coefficient and its standard error from a
# fitted lm object and returns them as a tidy one-row tibble.
# `predictor` defaults to "treatment" because that is our cohort dummy (0/1).
# Keeping this as a separate function (rather than inline in run_cohort_models)
# means we can reuse it for any predictor variable in any lm object.
extract_cohort_summary <- function(lm_object, model_label = "Model", predictor = "treatment") {
  # summary()$coefficients is a matrix with rows = predictors and
  # columns = Estimate, Std. Error, t value, Pr(>|t|).
  # We index by row name (predictor) and column name to avoid hardcoding positions.
  coef_summary <- summary(lm_object)$coefficients
  coef_val <- coef_summary[predictor, "Estimate"]
  se_val   <- coef_summary[predictor, "Std. Error"]
  n        <- nobs(lm_object)  # number of observations used (after NA removal)

  tibble(
    Model    = model_label,          # "Without Controls" or "With Controls"
    Estimate = round(coef_val, 3),   # β for the `treatment` dummy
    SE       = round(se_val, 3),     # standard error of β
    N        = n                     # effective sample size (listwise deletion)
  )
}


##### Run cohort OLS models — treatment + age polynomial, with and without demographic controls #####
#
# Fits two OLS regressions on `data`:
#   Simple model:  index ~ treatment + age + I(age²)
#   Full model:    index ~ treatment + age + I(age²) + controls
# Returns a 2-row tibble (one row per specification) tagged with `sample_label`.
#
# WHY age + I(age²)?
#   The post-Trump cohort is by construction younger than the pre-Trump cohort.
#   Simply including a linear age term would partial out a straight-line age
#   trend, but life-cycle political attitudes often follow a U-shape or other
#   non-monotonic path — young people start liberal, shift right in mid-life,
#   then move back. The quadratic term I(age²) captures this curvature, so the
#   treatment coefficient picks up only what the smooth age polynomial cannot
#   explain: the discrete jump at the cohort boundary.
#
# WHY as.formula(paste0(...))?
#   R formulas must be Formula objects, not strings. paste0() builds the formula
#   string dynamically (allowing the caller to pass any outcome variable or controls
#   vector), and as.formula() converts it to the correct type for lm().

run_cohort_models <- function(data, index_var, controls, sample_label) {

  # ── Simple model: only age controls, no demographics ─────────────────────────
  # This is the minimal specification. The treatment coefficient here conflates
  # the cohort effect with any demographic differences between cohorts (e.g. the
  # post-Trump cohort is more educated on average, which could independently
  # predict higher support for democratic norms).
  formula_simple <- as.formula(paste0(index_var, " ~ treatment + age + I(age^2)"))
  lm_simple      <- lm(formula_simple, data = data)
  summary_simple <- extract_cohort_summary(lm_simple, "Without Controls")

  # ── Full model: adds demographic controls ─────────────────────────────────────
  # Controls (gender, education, income) are confounders — variables that differ
  # between cohorts AND independently predict the outcome. Controlling for them
  # isolates the cohort effect from pre-existing demographic differences.
  # See the detailed discussion in usa_thesis.R on mediators vs. confounders.
  formula_controls <- as.formula(paste0(
    index_var, " ~ treatment + age + I(age^2) + ",
    paste(controls, collapse = " + ")  # e.g. "gender + education + income"
  ))
  lm_controls      <- lm(formula_controls, data = data)
  summary_controls <- extract_cohort_summary(lm_controls, "With Controls")

  # Stack both rows and prepend the sample label as the first column.
  # .before = 1 places Sample before Model in column order for easier reading.
  bind_rows(summary_simple, summary_controls) %>%
    dplyr::mutate(Sample = sample_label, .before = 1)
}


# ─── SHARED HELPER ───────────────────────────────────────────────────────────
# All three cohort plot functions need the same data-prep steps:
#   1. Remove rows with missing party, outcome, or treatment.
#   2. Label treatment as "Pre-Trump" / "Post-Trump".
#   3. Assign each person to a partisan subgroup.
#   4. Duplicate rows so each person also appears in the "Partisans" and "Full Sample" panels.
# We do this once in a helper and call it from each plot function.

.prep_cohort_plotdata <- function(dataframe, outcome) {

  # ── Step 1: Clean and label ──────────────────────────────────────────────────
  # Drop respondents missing party, the outcome index, or the treatment variable.
  # We need all three to plot cohort differences by subgroup.
  # Relabel `treatment` from 0/1 to "Pre-Trump"/"Post-Trump" so ggplot2 legends
  # are self-explanatory without needing a separate legend guide.
  base <- dataframe %>%
    filter(!is.na(party), !is.na(.data[[outcome]]), !is.na(treatment)) %>%
    mutate(
      cohort   = factor(treatment, levels = c(0, 1), labels = c("Pre-Trump", "Post-Trump")),
      subgroup = case_when(
        party == "Democrat"    ~ "Democrats",
        party == "Republican"  ~ "Republicans",
        party == "Independent" ~ "Independents"
        # Respondents with party == NA are already dropped by the filter above,
        # so no explicit NA case is needed here.
      )
    )

  # ── Step 2: Duplicate partisan rows under a "Partisans" label ───────────────
  # H2 predicts that the cohort effect is larger among partisans (Dems + Reps)
  # than among independents. To test this visually, we need a panel that shows
  # all partisans pooled together. We achieve this by duplicating the partisan
  # rows with a new subgroup label — they still appear in their own party panels
  # AND in the combined Partisans panel.
  df_partisans <- base %>%
    filter(party %in% c("Democrat", "Republican")) %>%
    mutate(subgroup = "Partisans")

  # ── Step 3: Duplicate all rows under a "Full Sample" label ──────────────────
  # Analogously, we need a panel for the full sample (all parties combined)
  # so the overall cohort effect is visible alongside the subgroup comparisons.
  df_full <- base %>%
    mutate(subgroup = "Full Sample")

  # ── Step 4: Stack all copies into one long dataframe ─────────────────────────
  # Each respondent now appears in ONE of {Democrats, Republicans, Independents}
  # PLUS "Partisans" (if partisan) PLUS "Full Sample".
  # The ordered factor ensures a consistent left-to-right panel order in all
  # downstream facet plots: Full Sample → Independents → Partisans → Dems → Reps.
  bind_rows(base, df_partisans, df_full) %>%
    filter(!is.na(subgroup)) %>%
    mutate(subgroup = factor(
      subgroup,
      levels = c("Full Sample", "Independents", "Partisans", "Democrats", "Republicans")
    ))
}
# ─────────────────────────────────────────────────────────────────────────────


##### Plot 1 — Mean ± 95% CI: simple dot-and-error-bar comparison #####
# Best for: quickly reading off whether the two cohorts differ and by how much.
# Each dot = group mean; bars = 95% confidence interval around that mean.
get_cohortplot <- function(dataframe, outcome = "liberal_index", outcome_name = "Liberal Attitudes", colnumber = 2) {
  df_plot <- .prep_cohort_plotdata(dataframe, outcome)

  # Collapse the individual rows down to one summary row per subgroup × cohort cell
  df_summary <- df_plot %>%
    group_by(subgroup, cohort) %>%
    summarise(
      mean_val = mean(.data[[outcome]], na.rm = TRUE),
      se_val   = sd(.data[[outcome]], na.rm = TRUE) / sqrt(n()),
      .groups  = "drop"
    )

  ggplot(df_summary, aes(x = cohort, y = mean_val, color = cohort)) +
    geom_point(size = 3) +
    geom_errorbar(
      aes(ymin = mean_val - 1.96 * se_val, ymax = mean_val + 1.96 * se_val),
      width = 0.2
    ) +
    # Dark2[1] = dark green → Pre-Trump; Dark2[2] = orange → Post-Trump
    # Matches the color convention used in get_discontinuityplot()
    scale_color_brewer(palette = "Dark2") +
    labs(
      x        = NULL,
      y        = outcome_name,
      title    = paste("Cohort Differences in", outcome_name),
      subtitle = "Mean \u00b1 95% CI by Cohort and Subgroup",
      color    = "Cohort"
    ) +
    theme_bw() +
    theme(
      plot.title      = element_text(face = "bold", hjust = 0.5),
      plot.subtitle   = element_text(hjust = 0.5),
      legend.position = "bottom"
    ) +
    facet_wrap(~subgroup, ncol = colnumber)
}


##### Plot 2 — Model-fit plot: OLS predicted curves with visible cohort gap #####
# This is the most informative cohort plot because:
#   ● It shows the RAW data (scatter) so you can see individual variation.
#   ● It shows the MODEL-PREDICTED curves (lines + confidence bands), which incorporate
#     all controls (age², gender, education, income held at sample means).
#   ● The GAP between the two curves at the dashed cutoff line (age 26) is exactly β,
#     the cohort effect estimate from the regression.
#   ● Because the model has NO treatment × age interaction, the two curves are PARALLEL
#     (same curvature, different height). This is a feature, not a bug: it means the
#     model assumes the age trend is the same for both cohorts, and only the intercept
#     differs. If the curves were allowed to cross, that would suggest the cohort effect
#     is age-dependent, which would require a treatment × age interaction term.
#   ● Controls are INCORPORATED: predictions are at the sample mean of each control
#     variable, showing the cohort effect for a "typical" respondent in each subgroup.
get_cohortplot_fit <- function(dataframe, outcome = "liberal_index", outcome_name = "Liberal Attitudes", controls = c("gender", "education", "income"), colnumber = 2) {
  df_prepped <- .prep_cohort_plotdata(dataframe, outcome)

  subgroup_levels <- c("Full Sample", "Independents", "Partisans", "Democrats", "Republicans")

  # Fit one model per subgroup and generate age-range predictions
  pred_frames <- lapply(subgroup_levels, function(sg) {
    sub <- df_prepped %>% filter(subgroup == sg)
    if (nrow(sub) < 20) return(NULL)

    # Fit the controlled cohort model (same spec as run_cohort_models "With Controls")
    fm  <- as.formula(paste0(outcome, " ~ treatment + age + I(age^2) + ", paste(controls, collapse = " + ")))
    fit <- lm(fm, data = sub, na.action = na.exclude)

    # Prediction grid: age 18–70, treatment follows the real-world rule (< 26 = Post-Trump),
    # all demographic controls held at their sample means within this subgroup
    age_seq    <- seq(18, min(max(sub$age, na.rm = TRUE), 70), by = 0.5)
    ctrl_means <- sub %>% summarise(across(all_of(controls), ~ mean(.x, na.rm = TRUE)))
    grid <- data.frame(age = age_seq, treatment = as.integer(age_seq < 26)) %>%
      bind_cols(ctrl_means[rep(1, length(age_seq)), , drop = FALSE])

    preds      <- predict(fit, newdata = grid, interval = "confidence")
    grid$fit   <- preds[, "fit"]
    grid$lower <- preds[, "lwr"]
    grid$upper <- preds[, "upr"]
    grid$cohort   <- factor(grid$treatment, levels = c(0, 1), labels = c("Pre-Trump", "Post-Trump"))
    grid$subgroup <- sg
    grid
  })

  pred_all <- bind_rows(pred_frames) %>%
    mutate(subgroup = factor(subgroup, levels = subgroup_levels))

  # Scatter of the actual observed data (lightly plotted — the model lines are the focus)
  scatter <- df_prepped %>%
    mutate(cohort = factor(treatment, levels = c(0, 1), labels = c("Pre-Trump", "Post-Trump")))

  ggplot() +
    # Raw data (transparent scatter — shows real variation behind the model)
    geom_point(
      data  = scatter,
      aes(x = age, y = .data[[outcome]], color = cohort),
      alpha = 0.15, size = 0.6
    ) +
    # 95% confidence band around each predicted curve
    geom_ribbon(
      data  = pred_all,
      aes(x = age, ymin = lower, ymax = upper, fill = cohort),
      alpha = 0.25
    ) +
    # Predicted curve — the jump between the two curves at age 26 IS the cohort effect β
    geom_line(
      data      = pred_all,
      aes(x = age, y = fit, color = cohort),
      linewidth = 1.1
    ) +
    # Dashed vertical line marks the cohort cutoff
    geom_vline(xintercept = 26, linetype = "dashed", color = "gray30", linewidth = 0.7) +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette  = "Dark2") +
    labs(
      x        = "Age in 2020",
      y        = outcome_name,
      title    = paste("Cohort Effect on", outcome_name),
      subtitle = "OLS-predicted values (age\u00b2 + controls at sample means) | gap at dashed line = \u03b2",
      color    = "Cohort",
      fill     = "Cohort"
    ) +
    theme_bw() +
    theme(
      plot.title      = element_text(face = "bold", hjust = 0.5),
      plot.subtitle   = element_text(hjust = 0.5),
      legend.position = "bottom"
    ) +
    facet_wrap(~subgroup, scales = "free_y", ncol = colnumber)
}


##### Plot 3 — Density curves: full distribution of each cohort #####
# Best for: seeing the SHAPE of each cohort's responses, not just the average.
# Overlapping filled curves show where most responses pile up and how much the
# two cohorts overlap. Dashed vertical lines mark each group's mean.
get_cohortplot_density <- function(dataframe, outcome = "liberal_index", outcome_name = "Liberal Attitudes", colnumber = 2) {
  df_plot <- .prep_cohort_plotdata(dataframe, outcome)

  # Compute per-subgroup × cohort means for the vertical reference lines
  df_means <- df_plot %>%
    group_by(subgroup, cohort) %>%
    summarise(mean_val = mean(.data[[outcome]], na.rm = TRUE), .groups = "drop")

  ggplot(df_plot, aes(x = .data[[outcome]], color = cohort, fill = cohort)) +
    # Filled, semi-transparent density curve for each cohort
    geom_density(alpha = 0.2, linewidth = 0.8) +
    # Dashed vertical line at each cohort's mean (uses the pre-computed df_means)
    geom_vline(
      data     = df_means,
      aes(xintercept = mean_val, color = cohort),
      linetype = "dashed", linewidth = 0.8
    ) +
    # Dark2[1] = dark green → Pre-Trump; Dark2[2] = orange → Post-Trump
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette  = "Dark2") +
    labs(
      x        = outcome_name,
      y        = "Density",
      title    = paste("Distribution of", outcome_name, "by Cohort"),
      subtitle = "Dashed lines mark group means",
      color    = "Cohort",
      fill     = "Cohort"
    ) +
    theme_bw() +
    theme(
      plot.title      = element_text(face = "bold", hjust = 0.5),
      plot.subtitle   = element_text(hjust = 0.5),
      legend.position = "bottom"
    ) +
    facet_wrap(~subgroup, ncol = colnumber)
}


##### Plot 3 — Violin + box: distribution shape AND summary statistics #####
# Best for: combining the density information (the width of the violin at each
# value) with classic box-plot statistics (median, IQR, outliers).
# The narrow white box inside each violin shows the median and interquartile range.
get_cohortplot_violin <- function(dataframe, outcome = "liberal_index", outcome_name = "Liberal Attitudes", colnumber = 2) {
  df_plot <- .prep_cohort_plotdata(dataframe, outcome)

  ggplot(df_plot, aes(x = cohort, y = .data[[outcome]], fill = cohort)) +
    # Violin: the width at each y-value shows how many people scored there
    geom_violin(alpha = 0.6, trim = FALSE) +
    # Thin white box inside the violin: shows median (middle line) and IQR (box edges)
    geom_boxplot(width = 0.1, fill = "white", outlier.size = 0.5, outlier.alpha = 0.4) +
    # Dark2[1] = dark green → Pre-Trump; Dark2[2] = orange → Post-Trump
    scale_fill_brewer(palette = "Dark2") +
    labs(
      x        = NULL,
      y        = outcome_name,
      title    = paste("Distribution of", outcome_name, "by Cohort"),
      subtitle = "Violin width = density of responses; inner box = median and IQR",
      fill     = "Cohort"
    ) +
    theme_bw() +
    theme(
      plot.title      = element_text(face = "bold", hjust = 0.5),
      plot.subtitle   = element_text(hjust = 0.5),
      legend.position = "bottom"
    ) +
    facet_wrap(~subgroup, ncol = colnumber)
}


##### Plot 4 — Slopegraph: Pre→Post-Trump change per subgroup on a single panel #####
#
# WHY THIS IS BETTER THAN THE OTHER COHORT PLOTS:
#
#   The discontinuity plot, the model-fit plot, and even the mean ± CI plot all
#   use a faceted layout: one panel per subgroup. That means you have to mentally
#   compare slope angles or error bars across different panels — which is cognitively
#   hard. This slopegraph puts a CHOSEN SET of subgroups on ONE panel as colored slope
#   lines, so you can instantly compare:
#
#   ● Does the Full Sample slope point DOWN? (H1: Post-Trump scores lower)
#   ● Is the Partisans slope steeper than the Independents slope? (H2)
#   ● Do Democrats and Republicans slope in OPPOSITE directions? (H3)
#
#   IMPORTANT: These slopes show RAW UNADJUSTED means (no age or demographic controls).
#   They are descriptive, not causal. For the causal/controlled estimate, see the
#   coefficient plots (get_coefplot / get_coefplot_methods). The raw slopes may differ
#   in direction from the controlled estimates because younger respondents (Post-Trump)
#   differ from older respondents (Pre-Trump) for reasons beyond Trump exposure —
#   this is why we need the OLS age controls and the RDD.
#
# ARGUMENTS:
#   dataframe    — the full dataframe (subsampling by party is done internally)
#   outcome      — string name of the index column (e.g. "liberal_index")
#   outcome_name — human-readable label for the y-axis
#   subgroups    — character vector of subgroups to display; must be a subset of
#                  c("Full Sample", "Independents", "Partisans", "Democrats", "Republicans")
#                  Default = all five. Pass a smaller set to focus on specific hypotheses.
#   title        — optional custom plot title (auto-generated if NULL)
#   subtitle     — optional custom subtitle (auto-generated if NULL)

get_cohortplot_slopes <- function(
    dataframe,
    outcome      = "liberal_index",
    outcome_name = "Liberal Attitudes",
    subgroups    = c("Full Sample", "Independents", "Partisans", "Democrats", "Republicans"),
    controls     = NULL,   # character vector, e.g. c("gender","education","income")
    #   NULL  → show raw unadjusted group means
    #   provided → show OLS-predicted values at ref_age with controls at sample means;
    #              the gap between the two points equals β (the cohort OLS coefficient)
    ref_age      = 26,     # reference age for controlled predictions; default = the cutoff
    title        = NULL,
    subtitle     = NULL
) {
  df_plot <- .prep_cohort_plotdata(dataframe, outcome) %>%
    filter(subgroup %in% subgroups) %>%
    mutate(subgroup = factor(subgroup, levels = subgroups))

  # ── Two ways to compute the endpoints ───────────────────────────────────────
  if (is.null(controls)) {

    # RAW MEANS — no age or demographic adjustment.
    # Shows the descriptive difference between cohorts, confounded by the fact
    # that Post-Trump respondents are simply younger.
    df_summary <- df_plot %>%
      group_by(subgroup, cohort) %>%
      summarise(
        mean_val = mean(.data[[outcome]], na.rm = TRUE),
        se_val   = sd(.data[[outcome]], na.rm = TRUE) / sqrt(n()),
        .groups  = "drop"
      ) %>%
      mutate(
        x     = if_else(cohort == "Pre-Trump", 0, 1),
        lower = mean_val - 1.96 * se_val,
        upper = mean_val + 1.96 * se_val
      )

    if (is.null(subtitle))
      subtitle <- "Raw unadjusted means \u00b1 95% CI | downward = Post-Trump scores lower"

  } else {

    # CONTROLLED MEANS — OLS-predicted values.
    #
    # WHAT THE GAP MEANS: for each subgroup we fit the same model as run_cohort_models():
    #   outcome ~ treatment + age + I(age²) + controls
    # Then we predict at ref_age (default = 26, the cutoff) for both treatment = 0
    # (Pre-Trump) and treatment = 1 (Post-Trump), holding all controls at the subgroup's
    # sample mean. The vertical gap between the two predicted points IS β — the
    # treatment coefficient from the regression. So this slopegraph is a visual
    # representation of what the coefficient plot already shows numerically.
    #
    # WHY THIS DIFFERS FROM THE RAW MEANS: the OLS polynomial partial-out the age
    # trend. Because Post-Trump respondents are younger, and younger people may
    # differ for non-Trump reasons, the controlled estimate can have a different sign
    # or magnitude than the raw descriptive difference. Both answers are correct —
    # they just answer different questions.

    df_summary <- lapply(levels(df_plot$subgroup), function(sg) {
      sub <- df_plot %>% filter(subgroup == sg)
      if (nrow(sub) < 20) return(NULL)

      # Fit the same model specification as run_cohort_models()
      fm  <- as.formula(paste0(
        outcome, " ~ treatment + age + I(age^2) + ", paste(controls, collapse = " + ")
      ))
      fit <- lm(fm, data = sub, na.action = na.exclude)

      # Prediction grid: Pre-Trump (treatment=0) and Post-Trump (treatment=1),
      # both at ref_age, with all controls fixed at the subgroup's sample mean.
      ctrl_means <- sub %>%
        summarise(across(all_of(controls), ~mean(.x, na.rm = TRUE)))

      pred_df <- data.frame(treatment = c(0L, 1L), age = rep(ref_age, 2)) %>%
        bind_cols(ctrl_means[rep(1L, 2L), , drop = FALSE])

      preds <- predict(fit, newdata = pred_df, interval = "confidence")

      tibble(
        subgroup = sg,
        cohort   = factor(c(0, 1), levels = c(0, 1),
                          labels = c("Pre-Trump", "Post-Trump")),
        mean_val = preds[, "fit"],
        lower    = preds[, "lwr"],
        upper    = preds[, "upr"]
      )
    }) %>%
      bind_rows() %>%
      mutate(
        subgroup = factor(subgroup, levels = levels(df_plot$subgroup)),
        x = if_else(cohort == "Pre-Trump", 0, 1)
      )

    if (is.null(subtitle))
      subtitle <- paste0(
        "OLS-predicted at age ", ref_age, " (controls at sample means)",
        " | gap between points = \u03b2"
      )
  }

  # Default title
  if (is.null(title)) title <- paste("Pre- vs. Post-Trump Cohort Shift in", outcome_name)

  # Wide format for geom_segment: one row per subgroup with y_pre and y_post
  df_segs <- df_summary %>%
    select(subgroup, cohort, mean_val) %>%
    pivot_wider(names_from = cohort, values_from = mean_val) %>%
    rename(y_pre = `Pre-Trump`, y_post = `Post-Trump`)

  ggplot() +
    # Slope lines: direction and steepness encode the cohort effect
    geom_segment(
      data      = df_segs,
      aes(x = 0, xend = 1, y = y_pre, yend = y_post, color = subgroup),
      linewidth = 1.2, alpha = 0.85
    ) +
    # 95% CI at each endpoint
    geom_errorbar(
      data = df_summary,
      aes(x = x, ymin = lower, ymax = upper, color = subgroup),
      width = 0.02, linewidth = 0.7
    ) +
    geom_point(
      data = df_summary,
      aes(x = x, y = mean_val, color = subgroup),
      size = 3.5
    ) +
    scale_x_continuous(
      breaks = c(0, 1),
      labels = c("Pre-Trump\n(first eligible: 2012)",
                 "Post-Trump\n(first eligible: 2016+)"),
      limits = c(-0.08, 1.08)
    ) +
    scale_color_brewer(palette = "Dark2", name = "Subgroup") +
    labs(
      x        = NULL,
      y        = outcome_name,
      title    = title,
      subtitle = subtitle
    ) +
    theme_bw() +
    theme(
      plot.title         = element_text(face = "bold", hjust = 0.5),
      plot.subtitle      = element_text(hjust = 0.5),
      legend.position    = "right",
      panel.grid.minor.x = element_blank()
    )
}


##### Plot 5 — Coefficient slopegraph: β as a visual slope ######################
#
# WHAT THIS PLOT SHOWS:
#   Every slope line directly represents a regression coefficient β (treatment effect).
#   • Left endpoint: fixed at y = 0 — the Pre-Trump baseline (β = 0 = no effect).
#   • Right endpoint: at y = β — the OLS estimate of the Post-Trump cohort effect.
#   • Slope direction: downward → β < 0 → Post-Trump scores LOWER (H1 confirmed).
#                      upward   → β > 0 → Post-Trump scores HIGHER (backlash).
#                      flat     → β ≈ 0 → no cohort gap.
#
# FACETING AND COLORS:
#   Facets = model specification (two panels: "Without Controls" | "With Controls").
#   Color  = subgroup (Dark2 palette — matches discontinuity plots).
#   This layout lets you directly compare subgroups within each model spec:
#     • Do Republicans and Democrats slope in opposite directions? (H3)
#     • Is the Partisans slope steeper than Independents? (H2)
#     • Does adding controls change the direction of the effect? (robustness)
#
# WHAT GOES INTO EACH MODEL:
#   Simple model:  treatment + age + age²
#   Full model:    treatment + age + age² + controls (listed in caption)
#
# RELATIONSHIP TO get_coefplot():
#   Both show the same β values. get_cohortplot_slopes_coef() adds a "slope"
#   metaphor that makes direction immediately intuitive; get_coefplot() adds
#   precise numerical readout. Use them together.

get_cohortplot_slopes_coef <- function(
    dataframe,
    outcome      = "liberal_index",
    outcome_name = "Liberal Attitudes",
    subgroups    = c("Full Sample", "Independents", "Partisans", "Democrats", "Republicans"),
    controls     = c("gender", "education", "income"),
    colnumber    = 2,     # number of facet columns (2 = Without Controls | With Controls side by side)
    title        = NULL,
    subtitle     = NULL
) {
  df_plot <- .prep_cohort_plotdata(dataframe, outcome) %>%
    filter(subgroup %in% subgroups) %>%
    mutate(subgroup = factor(subgroup, levels = subgroups))

  # Fit both models for every requested subgroup and extract the treatment β and SE.
  coef_data <- lapply(levels(df_plot$subgroup), function(sg) {
    sub <- df_plot %>% filter(subgroup == sg)
    if (nrow(sub) < 20) return(NULL)

    # Simple model: treatment + age + age² (no demographic controls)
    cs <- summary(
      lm(as.formula(paste0(outcome, " ~ treatment + age + I(age^2)")),
         data = sub, na.action = na.exclude)
    )$coefficients["treatment", ]

    # Full model: adds demographic controls
    cf <- summary(
      lm(as.formula(paste0(outcome, " ~ treatment + age + I(age^2) + ",
                           paste(controls, collapse = " + "))),
         data = sub, na.action = na.exclude)
    )$coefficients["treatment", ]

    bind_rows(
      tibble(subgroup = sg, Model = "Without Controls",
             beta = cs[["Estimate"]], se = cs[["Std. Error"]]),
      tibble(subgroup = sg, Model = "With Controls",
             beta = cf[["Estimate"]], se = cf[["Std. Error"]])
    )
  }) %>%
    bind_rows() %>%
    mutate(
      subgroup = factor(subgroup, levels = levels(df_plot$subgroup)),
      Model    = factor(Model, levels = c("Without Controls", "With Controls")),
      lower    = beta - 1.96 * se,
      upper    = beta + 1.96 * se
    )

  if (is.null(title))
    title <- paste("OLS Treatment Coefficients:", outcome_name)
  if (is.null(subtitle))
    subtitle <- paste0(
      "Slope = \u03b2 (OLS treatment effect) | color = subgroup | ",
      "facets = model specification | downward = Post-Trump scores lower"
    )

  ggplot(coef_data) +
    # Slope from (0, 0) to (1, β): direction and steepness = the cohort effect
    geom_segment(
      aes(x = 0, xend = 1, y = 0, yend = beta, color = subgroup),
      linewidth = 1.2, alpha = 0.85
    ) +
    # Dashed horizontal at β = 0 (null hypothesis: no cohort effect)
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    # Shared left anchor at (0, 0) — all subgroups start at the same baseline
    geom_point(aes(x = 0, y = 0, color = subgroup), size = 3, shape = 16) +
    # Right endpoint = estimated β, with 95% CI
    geom_errorbar(
      aes(x = 1, ymin = lower, ymax = upper, color = subgroup),
      width = 0.03, linewidth = 0.7
    ) +
    geom_point(aes(x = 1, y = beta, color = subgroup), size = 3.5) +
    scale_x_continuous(
      breaks = c(0, 1),
      labels = c("Pre-Trump\n(baseline: \u03b2 = 0)", "Post-Trump\n(estimated \u03b2)"),
      limits = c(-0.1, 1.15)
    ) +
    scale_color_brewer(palette = "Dark2", name = "Subgroup") +
    labs(
      x        = NULL,
      y        = paste0("\u03b2 — treatment effect on ", outcome_name),
      title    = title,
      subtitle = subtitle,
      caption  = paste0(
        "Simple model: treatment + age + age\u00b2\n",
        "Full model:   + ", paste(controls, collapse = " + ")
      )
    ) +
    theme_bw() +
    theme(
      plot.title         = element_text(face = "bold", hjust = 0.5),
      plot.subtitle      = element_text(hjust = 0.5),
      plot.caption       = element_text(hjust = 0, size = 8, color = "gray40"),
      legend.position    = "bottom",
      panel.grid.minor.x = element_blank()
    ) +
    # Facet by model spec: enables direct subgroup comparison within each specification
    facet_wrap(~Model, ncol = colnumber)
}


# ─────────────────────────────────────────────────────────────────────────────
# get_rddplot_slopes()
#
# Visualises RDD (rdrobust) estimates as a slope graph: each subgroup is drawn
# as a line from (0, 0) — the null of no cohort effect — to (1, τ_display),
# where τ_display = −Estimate so that a positive value means the Post-Trump
# cohort scores HIGHER on the outcome.
#
# SIGN CONVENTION (mirrors get_coefplot / get_coefplot_methods):
#   rdd_df stores Estimate = Pre-Trump − Post-Trump (raw rdrobust with age cutoff,
#   where "above cutoff" = older = pre-Trump). The function negates internally:
#     beta  = −Estimate  → positive on screen = Post-Trump higher
#     lower = −Estimate − 1.96 × SE
#     upper = −Estimate + 1.96 × SE
#
# FACETING:
#   Panels = model specification ("Without Controls" | "With Controls"), so all
#   subgroups are directly comparable within each specification — mirrors the
#   design of get_cohortplot_slopes_coef().
#   Color = subgroup (Dark2 palette).
#
# ARGUMENTS:
#   rdd_df       — tibble from bind_rows(run_rdd_models(...)); must have columns
#                  Sample, Model, Estimate, SE.
#   outcome_name — string label used in the y-axis title.
#   subgroups    — character vector; which Sample levels to include.
#   colnumber    — ncol passed to facet_wrap (2 = two panels side by side).
#   title        — plot title; NULL = auto-generated.
#   subtitle     — plot subtitle; NULL = auto-generated.
# ─────────────────────────────────────────────────────────────────────────────

get_rddplot_slopes <- function(
    rdd_df,
    outcome_name = "Liberal Attitudes",
    subgroups    = c("Full Sample", "Independents", "Partisans", "Democrats", "Republicans"),
    colnumber    = 2,
    title        = NULL,
    subtitle     = NULL
) {
  # --- Prepare data -----------------------------------------------------------
  # Filter to requested subgroups; negate Estimate for Post − Pre display.
  coef_data <- rdd_df %>%
    filter(Sample %in% subgroups) %>%
    mutate(
      Sample = factor(Sample, levels = subgroups),
      Model  = factor(Model,  levels = c("Without Controls", "With Controls")),
      # Negate: stored as Pre − Post → display as Post − Pre
      beta   = -Estimate,
      lower  = -Estimate - 1.96 * SE,   # 95 % CI lower bound (display scale)
      upper  = -Estimate + 1.96 * SE    # 95 % CI upper bound (display scale)
    )

  # --- Default labels ---------------------------------------------------------
  if (is.null(title))
    title <- paste("RDD Estimates:", outcome_name)
  if (is.null(subtitle))
    subtitle <- paste0(
      "\u03c4 = RDD treatment effect (bias-adjusted) | color = subgroup | ",
      "facets = model specification | downward = Post-Trump scores lower"
    )

  # --- Plot -------------------------------------------------------------------
  ggplot(coef_data) +
    # Slope from (0, 0) to (1, τ): direction and steepness = the cohort effect
    geom_segment(
      aes(x = 0, xend = 1, y = 0, yend = beta, color = Sample),
      linewidth = 1.2, alpha = 0.85
    ) +
    # Dashed horizontal at τ = 0 (null: no discontinuity at the cutoff)
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    # Shared left anchor: every subgroup starts from (0, 0)
    geom_point(aes(x = 0, y = 0, color = Sample), size = 3, shape = 16) +
    # Right endpoint = estimated τ, with 95 % CI whiskers
    geom_errorbar(
      aes(x = 1, ymin = lower, ymax = upper, color = Sample),
      width = 0.03, linewidth = 0.7
    ) +
    geom_point(aes(x = 1, y = beta, color = Sample), size = 3.5) +
    # x-axis: two ticks only — the anchor (baseline) and the RD estimate
    scale_x_continuous(
      breaks = c(0, 1),
      labels = c("Pre-Trump\n(baseline: \u03c4 = 0)", "Post-Trump\n(estimated \u03c4)"),
      limits = c(-0.1, 1.15)
    ) +
    scale_color_brewer(palette = "Dark2", name = "Subgroup") +
    labs(
      x        = NULL,
      y        = paste0("\u03c4 \u2014 RDD effect on ", outcome_name),
      title    = title,
      subtitle = subtitle,
      caption  = paste0(
        "Bias-corrected rdrobust estimates (Calonico, Cattaneo & Titiunik 2014); ",
        "cutoff = age 26 (\u2248 birth year 1994).\n",
        "Positive \u03c4 = Post-Trump cohort scores higher on the outcome."
      )
    ) +
    theme_bw() +
    theme(
      plot.title         = element_text(face = "bold", hjust = 0.5),
      plot.subtitle      = element_text(hjust = 0.5),
      plot.caption       = element_text(hjust = 0, size = 8, color = "gray40"),
      legend.position    = "bottom",
      panel.grid.minor.x = element_blank()
    ) +
    # Facet by model spec: subgroups compared within each specification
    facet_wrap(~Model, ncol = colnumber)
}


####################################
########### RDD Functions ##########
####################################


#####  Function that extracts the core values as a table to easily look at them #####
#
# rdrobust() returns three types of estimates for the RD treatment effect,
# accessible via the `$coef` vector:
#   index 1 = Conventional:      uses standard OLS bandwidth on each side.
#   index 2 = Bias-Corrected:    corrects for the bias introduced by using a
#             non-optimal bandwidth. This is the recommended estimate per
#             Calonico, Cattaneo & Titiunik (2014); we always use index 2.
#   index 3 = Robust:            uses a larger "robust" CI (wider to account
#             for the bias correction); preferred when reporting CIs.
# Similarly, $se[2] = bias-corrected SE, $pv[2] = bias-corrected p-value.
#
# $bws is a matrix; bws[1,1] = the left-side bandwidth (h); by default,
# rdrobust uses a symmetric bandwidth so left = right.
#
# $N_h[1] and $N_h[2] are the numbers of observations within the bandwidth
# on the left (pre-Trump, above the age cutoff) and right (post-Trump, below).
# Their sum is the effective sample size used for the local-linear fit.
#
# We intentionally retain the sign convention from rdrobust (pre−post in this
# context) and negate later (in get_coefplot) for display purposes, to preserve
# the original estimate values for the validation comparison with MC RDD.

extract_rdd_summary <- function(rd_object, model_label = "Model") {
  coef    <- rd_object$coef[2]         # bias-corrected point estimate (τ)
  se      <- rd_object$se[2]           # SE of the bias-corrected estimate
  bw_type <- rd_object$bwselect        # bandwidth selection method (e.g. "mserd")
  bw_value <- rd_object$bws[1, 1]     # optimal bandwidth h (left side = right side)
  n_below <- rd_object$N_h[1]         # obs within bandwidth on the left of cutoff
  n_above <- rd_object$N_h[2]         # obs within bandwidth on the right of cutoff

  # Significance stars are commented out because we report numeric p-values
  # in the tables; the plot CIs communicate statistical uncertainty visually.
  # stars <- case_when(pval < 0.001 ~ "***", pval < 0.01 ~ "**", ...)

  tibble(
    `Model`          = model_label,
    `Estimate`       = round(coef, 3),
    `SE`             = round(se, 3),
    # "mserd" = MSE-optimal bandwidth for sharp RDD (Calonico et al. 2014 default)
    `Bandwidth Type` = ifelse(bw_type == "mserd", "MSE-optimal", bw_type),
    `Bandwidth (h)`  = round(bw_value, 2),
    `N`              = n_below + n_above  # total observations inside bandwidth
  )
}


####################################
######## Monte Carlo Functions #####
####################################


# ─────────────────────────────────────────────────────────────────────────────
# run_mc_rdd()
#
# THE PROBLEM THIS SOLVES
# ───────────────────────
# The PRL (Polarization Research Lab) survey records birth YEAR but not birth
# month or day. The RD cutoff is November 1, 1994: respondents born before that
# date had their first eligible presidential election in 2012 (pre-Trump); those
# born on or after it had their first eligible election in 2016 (Trump's first).
#
# For anyone born in 1993 or earlier, their entire birth year falls before the
# cutoff — they are unambiguously pre-Trump. For anyone born in 1995 or later,
# their entire birth year falls after the cutoff — unambiguously post-Trump.
# But for respondents born in 1994 specifically, the cutoff falls mid-year:
# born Jan–Oct → pre-Trump; born Nov–Dec → post-Trump. Without birth months,
# we do not know which side of the cutoff they belong to.
#
# THE MONTE CARLO SOLUTION
# ────────────────────────
# On each of n_sim iterations we randomly draw a birth month (1–12) and birth
# day (1–28) for every 1994-born respondent, compute their implied decimal
# birth year (e.g. March 15 → 1994.203), and run rdrobust() on the resulting
# dataset. Repeating this n_sim times produces a SAMPLING DISTRIBUTION of RD
# estimates. The mean of that distribution is our point estimate; its standard
# deviation is our standard error. This correctly propagates the extra
# uncertainty that comes from not knowing birth months.
#
# THE RUNNING VARIABLE
# ────────────────────
# Birth dates are expressed as decimal years:
#   • January 1,  1994  → 1994.000
#   • November 1, 1994  → 1994.833  ← the RD cutoff
#   • December 31, 1994 → 1994.997
# For non-1994 respondents we use year + 0.5 (the mid-year point) as a fixed
# placeholder. This is unambiguous — all of 1993 is below 1994.833, all of
# 1995 is above it — and leaves the discontinuity estimation unaffected.
#
# SIGN CONVENTION
# ───────────────
# rdrobust() computes τ = E[Y | x just above cutoff] − E[Y | x just below].
# With birth_year as running variable, "above cutoff" = born after Nov 1994
# = post-Trump cohort.  So the raw estimate = post-Trump − pre-Trump.
# The wrapper run_mc_rdd_models() negates the estimate so that the output
# follows the same convention as run_rdd_models() (which uses age, where
# "above cutoff" = older = pre-Trump).  Final convention: positive estimate =
# post-Trump cohort scores HIGHER on the outcome.
#
# ARGUMENTS
# ─────────
#   data           — dataframe containing all needed variables
#   outcome_var    — string: name of the outcome column (e.g. "liberal_index")
#   birth_year_var — string: name of the integer birth-year column
#   controls       — character vector of covariate names, or NULL for no covs
#   cutoff_decimal — RD cutoff as a decimal year (default: Nov 1 1994 ≈ 1994.833)
#   boundary_year  — birth year whose respondents are simulated (default: 1994)
#   n_sim          — Monte Carlo iterations; 1000 for validation, 5000 for final
#   seed           — RNG seed for reproducibility
#
# RETURNS
# ───────
# A 1-row tibble: Model | Estimate | SE | Bandwidth Type | Bandwidth (h) | N |
#                 N_sim (successful iterations) | N_failed
# ─────────────────────────────────────────────────────────────────────────────

run_mc_rdd <- function(
    data,
    outcome_var,
    birth_year_var,
    controls       = NULL,
    cutoff_decimal = 1994 + 304 / 365,  # November 1, 1994 ≈ 1994.833
    boundary_year  = 1994,
    n_sim          = 1000,
    seed           = 42,
    return_draws   = FALSE  # if TRUE, return list(summary = tibble, draws = vector)
                            # of successful estimates; used for diagnostic histograms
) {
  set.seed(seed)  # ensures identical results when re-run

  # --- Calendar lookup table ------------------------------------------------
  # We need to convert a simulated (month, day) pair into a day-of-year number
  # so we can express birth dates as decimal years.  month_start_day[m] gives
  # the number of days elapsed before month m starts (January = day 0).
  # We use 1–28 for days to avoid leap-year logic (Feb 29 never occurs).
  days_per_month  <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  month_start_day <- c(0, cumsum(days_per_month[-12]))  # length 12, 0-indexed

  # --- Extract vectors from the dataframe -----------------------------------
  # Working with plain vectors (not dataframe columns) is much faster inside
  # a loop with thousands of iterations.
  birth_year <- data[[birth_year_var]]
  y          <- data[[outcome_var]]
  covs       <- if (!is.null(controls)) data[, controls, drop = FALSE] else NULL

  # --- Drop rows with NA birth_year -----------------------------------------
  # rdrobust() requires no NA values in the running variable (x).
  # Respondents who refused to give their age have NA birth_year and must be
  # excluded before we build the running variable.
  keep <- !is.na(birth_year)
  birth_year <- birth_year[keep]
  y          <- y[keep]
  if (!is.null(covs)) covs <- covs[keep, , drop = FALSE]

  # --- Identify the boundary cohort -----------------------------------------
  # is_boundary flags every respondent born in boundary_year (default: 1994).
  # n_boundary is how many such respondents exist — this is the sample size
  # drawn in each Monte Carlo iteration.
  is_boundary <- birth_year == boundary_year
  n_boundary  <- sum(is_boundary)

  # --- Build the base running variable --------------------------------------
  # For non-boundary respondents, use year + 0.5 (mid-year) as the running
  # variable value. This is fixed across all iterations — only the 1994-born
  # respondents' values change. Boundary slots are set to NA here and will be
  # overwritten inside the loop each iteration.
  running_base              <- birth_year + 0.5
  running_base[is_boundary] <- NA_real_

  # --- Storage for Monte Carlo results --------------------------------------
  # Pre-allocate with NA so that failed iterations (where rdrobust errors out)
  # are distinguishable from successful ones with an estimate of exactly 0.
  estimates  <- rep(NA_real_, n_sim)
  bandwidths <- rep(NA_real_, n_sim)
  ns         <- rep(NA_real_, n_sim)

  # --- Monte Carlo loop -----------------------------------------------------
  for (i in seq_len(n_sim)) {

    # Step 1: Draw a random birth month (1–12) and day (1–28) for every
    # boundary-year respondent.  Using 1–28 keeps things valid for February
    # without any special leap-year handling.
    sim_month <- sample(1L:12L, n_boundary, replace = TRUE)
    sim_day   <- sample(1L:28L, n_boundary, replace = TRUE)

    # Step 2: Convert (month, day) → decimal year.
    # day_of_year counts how many days into the year the birth date falls
    # (January 1 = day 1).  Dividing by 365 converts to a fraction of the year.
    # Example: March 15 → day_of_year = 0 + 15 = 74 (using 0-indexed offset for
    # Jan) → 1994 + (74-1)/365 = 1994 + 0.200 = 1994.200.
    day_of_year <- month_start_day[sim_month] + sim_day
    sim_decimal <- boundary_year + (day_of_year - 1L) / 365

    # Step 3: Assemble the full running variable for this iteration.
    # Non-boundary respondents keep their fixed mid-year value; boundary
    # respondents get the newly simulated decimal birth date.
    running              <- running_base
    running[is_boundary] <- sim_decimal

    # Step 4: Run rdrobust().  suppressWarnings() silences the mass-points
    # warning that arises because non-1994 respondents all share the same
    # running variable value (year + 0.5), creating ties.  The warning is
    # cosmetic — estimates are identical to the default behaviour.
    # tryCatch returns NULL if rdrobust errors (e.g. too few obs in bandwidth),
    # causing the iteration to be recorded as failed and skipped.
    result <- tryCatch(
      suppressWarnings(
        if (is.null(covs))
          rdrobust(y = y, x = running, c = cutoff_decimal, all = TRUE)
        else
          rdrobust(y = y, x = running, c = cutoff_decimal, covs = covs, all = TRUE)
      ),
      error = function(e) NULL
    )

    # Step 5: Store results from this iteration.
    # coef[2] = bias-corrected estimate (preferred over conventional coef[1]).
    # bws[1,1] = main bandwidth h (left/right bandwidths are equal by default).
    # N_h[1] + N_h[2] = total observations within the bandwidth.
    if (!is.null(result)) {
      estimates[i]  <- result$coef[2]
      bandwidths[i] <- result$bws[1, 1]
      ns[i]         <- result$N_h[1] + result$N_h[2]
    }
  }

  # --- Aggregate over successful iterations ---------------------------------
  # valid flags iterations where rdrobust returned an estimate (not NA).
  # The point estimate is the mean of the sampling distribution; the SE is
  # its standard deviation — this is the MC standard error, capturing both
  # the usual RD sampling uncertainty AND the uncertainty from unknown birth
  # months in the boundary year.
  valid    <- !is.na(estimates)
  n_valid  <- sum(valid)
  n_failed <- n_sim - n_valid

  summary_tbl <- tibble(
    Model            = if (is.null(controls)) "Without Controls" else "With Controls",
    Estimate         = round(mean(estimates[valid]), 3),
    SE               = round(sd(estimates[valid]),   3),
    `Bandwidth Type` = "MC (MSE-optimal)",
    `Bandwidth (h)`  = round(mean(bandwidths[valid]), 2),
    N                = round(mean(ns[valid])),
    N_sim            = n_valid,    # how many iterations produced a valid estimate
    N_failed         = n_failed    # how many rdrobust calls errored out
  )

  # return_draws = TRUE: also hand back the raw vector of per-iteration estimates
  # (only the valid ones). Used by get_mc_histogram() to plot the full sampling
  # distribution. Identical results to the normal path when using the same seed.
  if (return_draws) {
    list(summary = summary_tbl, draws = estimates[valid])
  } else {
    summary_tbl
  }
}


# ─────────────────────────────────────────────────────────────────────────────
# run_mc_rdd_models()
#
# Convenience wrapper around run_mc_rdd() that mirrors the two-specification
# pattern of run_rdd_models() (simple model + model with controls).
# Runs n_sim Monte Carlo iterations twice and returns a 2-row tibble
# with a Sample column prepended — the format expected by bind_rows()
# and get_coefplot() / get_rddplot_slopes().
#
# SEED LOGIC:
#   The simple model uses `seed` (default 42); the controlled model uses
#   `seed + 1L` to ensure the two models do NOT share the same random birth-
#   date draws. Using the same seed would mean the boundary-year respondents
#   receive IDENTICAL simulated birth dates in both specifications, which would
#   artificially correlate the two estimates. The +1L offset breaks this
#   correlation while keeping results fully reproducible.
#
# NOTE: If you call run_mc_rdd() directly with return_draws = TRUE for
# diagnostic histograms, use seed = 42 for the simple model and seed = 43
# for the controlled model to reproduce exactly the draws used here.
# ─────────────────────────────────────────────────────────────────────────────

run_mc_rdd_models <- function(
    data,
    outcome_var,
    birth_year_var,
    controls,
    cutoff_decimal = 1994 + 304 / 365,  # November 1, 1994 in decimal years
    boundary_year  = 1994,              # birth year whose residents are simulated
    n_sim          = 1000,              # MC iterations (use 5000 for final analysis)
    sample_label   = "Full Sample",
    seed           = 42
) {
  # ── Simple model (no covariates) ─────────────────────────────────────────────
  simple <- run_mc_rdd(
    data           = data,
    outcome_var    = outcome_var,
    birth_year_var = birth_year_var,
    controls       = NULL,              # no covariates in the simple specification
    cutoff_decimal = cutoff_decimal,
    boundary_year  = boundary_year,
    n_sim          = n_sim,
    seed           = seed               # e.g. 42
  )

  # ── Controlled model (gender + education + income) ───────────────────────────
  with_controls <- run_mc_rdd(
    data           = data,
    outcome_var    = outcome_var,
    birth_year_var = birth_year_var,
    controls       = controls,          # covariate vector passed by caller
    cutoff_decimal = cutoff_decimal,
    boundary_year  = boundary_year,
    n_sim          = n_sim,
    seed           = seed + 1L          # +1L: different seed → independent draws
  )

  # Stack into a 2-row tibble and add the sample identifier as first column
  bind_rows(simple, with_controls) %>%
    dplyr::mutate(Sample = sample_label, .before = 1)
}


#####  Run the two RDD specifications (with and without controls) for one subgroup  #####
#
# WHAT IS THE RDD CUTOFF HERE?
#   Cutoff = 26 on the `age` running variable. This corresponds to people who,
#   in 2020, are exactly 26 years old — meaning they turned 18 in 2012, the last
#   pre-Trump election. Those aged < 26 turned 18 in 2016 or later (post-Trump).
#
#   NOTE: This cutoff is approximate. The true cutoff is November 1, 1994 as a
#   birth DATE. In the final analysis this should be converted to an exact decimal
#   age at the time of the 2020 survey. For now, age = 26 is used as a clean integer
#   approximation and the results are validated against the Monte Carlo birth-year RDD.
#
# rdrobust() KEY ARGUMENTS:
#   y     — the outcome vector (the factor-analysis index, 0–1 scaled)
#   x     — the running variable (age in years, continuous decimal from ANES)
#   c     — the cutoff value (26)
#   covs  — optional matrix of covariates for covariate-adjusted RDD:
#           including covariates increases precision but does NOT change the
#           identification assumption (it still relies on continuity at the cutoff)
#   all   — if TRUE, rdrobust returns all three estimator types (Conventional,
#           Bias-Corrected, Robust); we extract index 2 (Bias-Corrected) via
#           extract_rdd_summary()
#
# SIGN OF THE ESTIMATE:
#   rdrobust returns τ = E[Y | x just ABOVE cutoff] − E[Y | x just BELOW cutoff].
#   With age as running variable, "above 26" = older = pre-Trump cohort.
#   So τ = pre-Trump mean − post-Trump mean.
#   A POSITIVE τ means the post-Trump cohort scores LOWER (H1 direction).
#   We negate in get_coefplot() so the displayed axis shows post − pre (intuitive).

run_rdd_models <- function(data, index_var, controls, sample_label) {
  # Extract outcome and running variable as plain vectors for speed
  y <- data[[index_var]]   # the 0–1 scaled index (e.g. liberal_index)
  x <- data$age            # continuous age in years (the RDD running variable)

  # ── Simple RDD: no covariates ─────────────────────────────────────────────────
  # This is the baseline RDD relying only on the discontinuity in age.
  # Bandwidth is chosen automatically by rdrobust using the MSE-optimal rule.
  rdd_simple <- rdrobust(
    y   = y,
    x   = x,
    c   = 26,      # cutoff: age 26 → first cohort that could NOT vote in 2012
    all = TRUE     # return all three estimator types; we use coef[2] = bias-corrected
  )
  summary_simple <- extract_rdd_summary(rdd_simple, model_label = "Without Controls")

  # ── Covariate-adjusted RDD: adds demographic controls ─────────────────────────
  # `covs` is a dataframe (NOT a formula vector) passed directly to rdrobust.
  # rdrobust residualizes y and x on covs before the local-linear fit,
  # reducing residual variance and tightening confidence intervals.
  # The causal identification is unchanged — it still relies on continuity at 26.
  # drop = FALSE preserves the dataframe class even for a single control column.
  covs <- data[, controls, drop = FALSE]

  rdd_controls <- rdrobust(
    y    = y,
    x    = x,
    c    = 26,
    covs = covs,   # covariate matrix (gender, education, income)
    all  = TRUE
  )
  summary_controls <- extract_rdd_summary(rdd_controls, model_label = "With Controls")

  # Stack the two model summaries into one 2-row tibble and prepend Sample label
  bind_rows(summary_simple, summary_controls) %>%
    dplyr::mutate(Sample = sample_label, .before = 1)
}
