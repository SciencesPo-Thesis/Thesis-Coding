# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Master thesis by Nikolaos Vichos (Sciences Po): *"Coming of Age Under Trump"* — studying whether first electoral exposure in a Trump election has a lasting effect on support for liberal democratic norms.

The thesis design is documented in `Structure.qmd` / `Structure.html`.

## Running the Analysis

Scripts are run interactively in RStudio. The main entry point is:

```r
source("usa_thesis.R")
```

The `location` variable at the top of `usa_thesis.R` must point to the parent directory containing both `Thesis-Github/` and `Datasets/`:

```r
location <- "/Users/nikolaosvichos/Library/Mobile Documents/com~apple~CloudDocs/Sciences Po/Thesis/"
```

Data files (not stored in this repo):
- ANES 2020: `{location}/Datasets/USA/usa_data.dta` — Stata format, read via `haven::read_dta()`
- PRL (Prolific Research Lab): 160+ weekly waves (2022–2026), not yet integrated in code

## Research Design (Two-Step)

### 1. Cohort Analysis (Observational)

Compares respondents whose first eligible presidential election was 2016 or later ("PostTrump" cohort) vs. those who could first vote in 2012 or earlier. Estimable from a single ANES cross-section; PRL's 160+ waves enable tests of long-run stability ("stickiness"):

$$Y_{it} = \alpha + \beta_1 \text{PostTrump}_i + \beta_2(\text{PostTrump}_i \times t) + \lambda_t + \varepsilon_{it}$$

A stable $\beta_2 \approx 0$ (gap neither widens nor narrows over time) is the primary evidence for long-run stickiness.

### 2. RDD (Causal)

Cutoff: **birth year November 1994** — those born before November 1994 turned 18 before the 2016 election; those born after had 2016 (Trump's first election) as their first eligible election.

- **ANES**: has exact birthdates → `rdrobust()` applied directly
- **PRL**: has birth year only → for respondents born in 1994, birth month/day is simulated via **Monte Carlo** (uniform distribution, repeated draws); `rdrobust()` run on each simulated dataset; mean of sampling distribution = causal estimate, SD = standard error

The RDD and cohort analyses are run on both datasets; converging findings constitute cross-dataset replication.

### Hypotheses

- **H1**: Post-Trump cohorts show lower support for liberal democratic norms overall
- **H2**: The cohort gap is larger among partisans than independents
- **H3**: Asymmetric — the gap is larger among Republicans than Democrats
- **H4a**: The effect interacts with party-in-power: post-Trump cohorts show a larger "in-power discount" on democratic norms (requires PRL panel; ANES proxy = electoral expectations)
- **H4b**: Assessment of government performance (TBD)

## Code Architecture

### Entry Point

`usa_thesis.R` (root): loads libraries, sources `Functions/functions.R`, imports and cleans ANES data, constructs factor-analysis-based indices, runs RDD models, produces plots.

### Functions (`Functions/functions.R`)

**Data helpers**: `na_recode()`, `voted_recode()`, `binary_recode()`, `binary_tranpose()`, `order_recode()` — recode raw ANES negative values (−9, −8, etc.) to NA and fix variable scales.

**RDD pipeline**:
- `run_rdd_models(data, index_var, controls, sample_label)` — runs two `rdrobust()` models (with/without controls, cutoff = 26 in current code; to be updated to November 1994 birth date cutoff) and returns a combined summary tibble
- `extract_rdd_summary(rd_object, model_label)` — extracts estimate, SE, bandwidth, and N from an `rdrobust` object

**Visualization**:
- `get_coefplot(dataframe, colnumber)` — coefficient plot across subgroups/outcomes (inverts estimates for display)
- `get_discontinuityplot(dataframe, outcome, outcome_name, colnumber)` — scatter + linear fit by subgroup (Dem/Rep/Ind/Partisans) and full sample; returns list with `$plot_all` and `$plot_subgroups`
- `get_discontinuityplot_multipleoutcomes(dataframe, subset, primary_only, colnumber)` — faceted plot across multiple outcome indices
- `get_screeplot(outcome, outcomename)` — base R scree plot for factor analysis

### Index Construction Pattern

For each outcome index (e.g., `liberal_index`):
1. Select relevant ANES items and recode negatives to NA
2. `scale()` to standardize items
3. `psych::fa()` with 1 factor (`fm = "pa"`)
4. `scales::rescale(fa$scores, to = c(0, 1))` → rescale to 0–1
5. Attach to dataframe as `df$<name>_index`

### Archive

`Archive/Countries/` — older per-country scripts (BRA, DEU, USA) from an earlier version of the project; superseded by `usa_thesis.R`.

`Functions/ReplicationCode_NoFunctions.R` — older monolithic script before functions were extracted; kept for reference.

## Key R Packages

`rdrobust` (RDD estimation), `psych` (factor analysis), `haven` (Stata .dta), `tidyverse`, `ggtext`, `patchwork`, `modelsummary`, `scales`
