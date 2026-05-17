#########################################################################
#########################################################################
############# Replication Code: "Coming of Age Under Trump" #############
########### The Effect of First Electoral Exposure in a Trump ###########
################# Election on Long-Term Liberal Attitudes ###############
##################### AMERICAN NATIONAL ELECTION STUDY ##################
#########################################################################
#########################################################################

## Author: Nikolaos VICHOS


### Set-Up

# ── Packages ──────────────────────────────────────────────────────────────────
library(styler)        # code reformatting (for interactive use; not needed at runtime)
library(psych)         # factor analysis: fa(), alpha() for index construction
library(dplyr)         # data manipulation: select, mutate, filter, group_by, etc.
library(tidyverse)     # loads ggplot2, tidyr, purrr, readr, forcats on top of dplyr
library(scales)        # axis formatting (number_format, comma) + rescale() for index 0–1 scaling
library(rdrobust)      # RDD estimation: rdrobust(), rdbwselect() (Calonico, Cattaneo & Titiunik)
library(modelsummary)  # publication-quality regression tables (msummary, modelsummary)
library(ggthemes)      # additional ggplot2 themes (we primarily use theme_bw())
library(patchwork)     # combine multiple ggplot2 plots side-by-side with + or stacked with /
library(tibble)        # tibble() constructor for clean tidy output tables
library(knitr)         # kable() for markdown/LaTeX tables in Quarto / R Markdown documents
library(ggtext)        # element_markdown() for HTML/Markdown in ggplot2 labels and captions

# ── Paths ──────────────────────────────────────────────────────────────────────
# `location` is the parent directory containing both the repo (Thesis-Github/)
# and the raw data (Datasets/). Change this path if running on a different machine.
location <- "/Users/nikolaosvichos/Library/Mobile Documents/com~apple~CloudDocs/Sciences Po/Thesis/"

# ── Load helper functions ──────────────────────────────────────────────────────
# All visualization and analysis functions live in functions.R.
# source() executes the file, making all defined functions available in this session.
# If functions.R has not been edited yet in this session, source() will (re-)load it.
source(paste0(location, "/Thesis-Github/Thesis-Coding/Functions/functions.R"))

# ── Import raw ANES 2020 data ──────────────────────────────────────────────────
# ANES 2020: American National Election Studies 2020 Time Series Study.
# Downloaded in Stata .dta format; read with haven::read_dta() to preserve
# variable and value labels (useful for checking what each code means).
# The raw file has ~8000 respondents and 1000+ variables; we select only those
# needed for this project in the data-management step below.
df_unclean <- haven::read_dta(paste0(location, "/Datasets/ANES/anes_data.dta"))


##############################################################################################################
############################################## Data Management ###############################################
##############################################################################################################


# ── Build the working dataset ─────────────────────────────────────────────────
# We narrow the raw ANES file (~1000+ variables) down to the variables used in
# the thesis. The pipeline:
#   1. select()  — keep only the columns we need (reduces memory and printing overhead)
#   2. filter()  — restrict to the relevant survey mode and wave
#   3. rename()  — replace opaque ANES variable codes with readable names
#   4. mutate()  — recode invalid values to NA; fix scale directions; create dummies
#
# KEY VARIABLE GROUPS:
#   Demographics:    age (V201507x), party (V201228), education (V201510), gender (V201600), income (V202468x)
#   Liberal items:   free_press (V201366), checks_and_balances (V201367), rule_of_law (V201368),
#                    agree_on_facts (V201369), journalist_access (V201375x), media_undermined_concern (V201376)
#   Authoritarianism items:  strong_leader (V202413), people_power (V202414), etc.
#   Populism items:  officials_dont_care (V202212), no_say_in_govt (V202213), few_powerful_control (V202311), etc.
#   Nativism items:  immigrants_harm_culture (V202419), minorities_should_adapt (V202416), etc.
#   Traditionalism:  child-rearing items (V202265–V202269), stay_at_home_wife (V202290x), etc.
#
# NOTE: pre-election outcome variables (V201366 etc. with V201xxx codes) were asked
# BEFORE the election; post-election items (V202xxx) were asked AFTER. We keep both
# because some liberal-attitudes items appear in the pre-wave only.

df_unstandardized <- df_unclean %>%
  dplyr::select(
    # ── Survey structure variables ────────────────────────────────────────────
    V200003,    # panel_data: survey mode / respondent type (2 = telephone; exclude)
    V200004,    # pre_or_post: 1 = pre-election, 3 = post-election (keep 3 only)
    # ── Demographic and background variables ──────────────────────────────────
    V201507x,   # age: respondent's age in years (continuous decimal from ANES)
    V201228,    # party: 7-pt party identification (1=strong Dem, 4=Independent, 7=strong Rep)
    V201231x,   # party_strength: recoded strength (1=strong, 2=not strong, 3=leaning)
    V201511x,   # education_summary: collapsed education (1=no HS, 2=HS, 3=some college, 4=BA+)
    V201510,    # education: detailed education categories
    V201600,    # gender: 1=male, 2=female
    V202468x,   # income: household income (1–22 categories)
    V201225x,   # duty_or_choice: is voting a duty (1) or a personal choice (2)?
    # ── Voting history ────────────────────────────────────────────────────────
    V201104,    # voted2012: voted in 2012 presidential election (1=yes, 2=no)
    V201101,    # voted2016: voted in 2016 (1=yes, 2=no)
    V202109x,   # voted2020: voted in 2020 (1=yes, 2=no, with intermediate codes)
    V201103,    # voted_for_2016: who did you vote for in 2016?
    V201105,    # voted_for_2012: who did you vote for in 2012?
    V202073,    # voted_for_2020: who did you vote for in 2020?
    # ── Thermometer ratings (0–100 feeling thermometers) ─────────────────────
    V201156,    # feeling_dems: feelings toward Democrats
    V201157,    # feeling_reps: feelings toward Republicans
    V202175,    # feeling_journalists: feelings toward journalists
    # ── LIBERAL INDEX ITEMS (pre-election wave, V201xxx) ─────────────────────
    V201366,    # free_press: importance of free press (1=extremely important … 5=not important)
    V201367,    # checks_and_balances: importance of checks and balances
    V201368,    # rule_of_law: importance of rule of law
    V201369,    # agree_on_facts: importance of elected officials agreeing on facts
    V201375x,   # journalist_access: should journalists have access to government? (recoded)
    V201376,    # media_undermined_concern: concern about media being undermined
    V201377,    # media_trust: trust in media
    V201378,    # foreign_help: concern about foreign interference in elections
    V201372x,   # unitary_executive: should president be able to act without Congress?
    V201379,    # govt_principled: how often does govt act on principle?
    # ── POPULISM / CYNICISM ITEMS (post-election wave, V202xxx) ──────────────
    V201233,    # govt_trust: how often can you trust government to do what's right?
    V201234,    # govt_capture: government run by a few big interests?
    V201236,    # govt_corruption: quite a few government officials are corrupt?
    V202212,    # officials_dont_care: public officials don't care what people think
    V202213,    # no_say_in_govt: people like me have no say in government
    V202304,    # polsystem_for_insiders: political system works only for insiders
    V202305,    # rich_and_powerful: government is run for benefit of rich and powerful
    V202308x,   # people_or_experts: should decisions be made by ordinary people or experts?
    V202311,    # few_powerful_control: a few powerful people secretly control the world
    V202312,    # powerful_indoctrination: powerful people use media to control what we think
    V202431,    # difference_inpower: does it matter who is in power?
    V202432,    # difference_vote: does it matter who you vote for?
    V202409,    # compromise_politics: how often do politicians compromise too much?
    V202410,    # politicians_notcare: politicians don't care about people like me
    V202411,    # politicians_trustworthy: politicians are trustworthy
    V202412,    # politicians_mainproblem: politicians are the main problem in the country
    V202413,    # strong_leader: we need a strong leader who doesn't have to bother with Congress
    V202414,    # people_power: ordinary people should make decisions, not politicians
    V202415,    # politicians_carerich: politicians care only about the rich
    # ── NATIVISM / AUTHORITARIANISM / TRADITIONALISM ITEMS ───────────────────
    V202416,    # minorities_should_adapt: minorities should adapt to national customs
    V202417,    # will_of_majority: will of the majority should always prevail
    V202419,    # immigrants_harm_culture: immigrants harm American culture
    V202265,    # traditional_family_values: traditional family values are important
    V202266,    # child_independent_or_respect: raise child to be independent or respectful?
    V202267,    # child_curious_or_wellmannered: curious or well-mannered?
    V202268,    # child_obedient_or_selfreliant: obedient or self-reliant?
    V202269,    # child_considerate_or_wellbehaved: considerate or well-behaved?
    V202290x,   # stay_at_home_wife: better for family if wife stays home?
    V202270,    # world_like_america: world would be better if more countries were like USA
    V202273x,   # america_better: America is better than most other countries
    # ── MISCELLANEOUS ─────────────────────────────────────────────────────────
    V201429,    # urban_unrest: concern about urban unrest / protest
    V202440,    # democracy_satisfaction: satisfaction with how democracy is working
    V202439     # left_right: self-placement on left–right scale
  ) %>%
  filter(V200003 %in% c(3, 4, 5, 6)) %>%   # fresh cross-section only, drop 2016-2020 panel
  filter(V200004 %in% c(1, 3)) %>%              # completed pre-election interview (with or without post)
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
    # ── DEMOGRAPHICS ──────────────────────────────────────────────────────────────
    # age: -9 = refused. The variable is a continuous decimal from ANES (e.g. 34.7),
    # so we just NA-out the refusals and leave the rest unchanged.
    age = case_when(
      age == -9 ~ NA_integer_,
      TRUE ~ age
    ),
    # party: 7-point scale (1=Strong Dem … 7=Strong Rep; 4=Independent).
    # Invalid codes: -9=refused, -8=DK, -4=technical error, 0=inapplicable, 5=other.
    # Values 1–7 (excluding invalid) are kept as numeric; recoded to factor below.
    party = case_when(
      party %in% c(-9, -8, -4, 0, 5) ~ NA_integer_,
      TRUE ~ party
    ),
    # party_binary: drop pure Independents (party == 3) so only partisans remain.
    # Used for analyses that require a strictly partisan sample.
    party_binary = case_when(
      party == 3 ~ NA_integer_,
      TRUE ~ party
    ),
    # party_strength: -9=refused, -8=DK; otherwise 1–3 (strong, not strong, leaning).
    party_strength = case_when(
      party_strength %in% c(-9, -8) ~ NA_integer_,
      TRUE ~ party_strength
    ),
    # ── THERMOMETERS ─────────────────────────────────────────────────────────────
    # Feeling thermometers run 0–100; 998 and 999 are out-of-range non-response codes.
    feeling_dems = case_when(
      feeling_dems %in% c(-9, 998) ~ NA_integer_,
      TRUE ~ feeling_dems
    ),
    feeling_reps = case_when(
      feeling_reps %in% c(-9, 998) ~ NA_integer_,
      TRUE ~ feeling_reps
    ),
    # NOTE: feeling_journalists currently uses feeling_reps (a known coding issue;
    # the journalist thermometer variable may need to be re-checked in ANES codebook).
    feeling_journalists = case_when(
      feeling_reps %in% c(-9, -7, -6, -5, -4, 998, 999) ~ NA_integer_,
      TRUE ~ feeling_reps
    ),
    # ── EDUCATION ────────────────────────────────────────────────────────────────
    # education (detailed): -9=refused, -8=DK, 95=other/specify (treated as missing).
    education = case_when(
      education %in% c(-9, -8, 95) ~ NA_integer_,
      TRUE ~ education
    ),
    # education_summary (collapsed 4-cat): -9=refused, -8=DK, -2=inapplicable.
    education_summary = case_when(
      education_summary %in% c(-9, -8, -2) ~ NA_integer_,
      TRUE ~ education_summary
    ),
    # gender: -9=refused; original coding 1=male/2=female.
    # Convert female (2) → 0 to create a standard 0/1 male indicator,
    # which makes the OLS gender coefficient interpretable as "male vs. female effect".
    gender = case_when(
      gender == -9  ~ NA_integer_,
      gender == 2   ~ 0,        # female: 2 → 0
      TRUE          ~ gender    # male: 1 stays 1
    ),
    # income: -9=refused, -5=DK. Values 1–22 = income categories (ordinal).
    income = case_when(
      income %in% c(-9, -5) ~ NA_integer_,
      TRUE ~ income
    ),
    # duty_or_choice: -2 = inapplicable (e.g. not asked for this respondent).
    duty_or_choice = case_when(
      duty_or_choice == -2 ~ NA_integer_,
      TRUE ~ duty_or_choice
    ),
    # ── VOTING HISTORY ────────────────────────────────────────────────────────────
    # voted2012: 1=yes, 2=no, -9/-8=refused/DK. Convert 2 → 0 for a 0/1 dummy.
    voted2012 = case_when(
      voted2012 %in% c(-9, -8) ~ NA_integer_,
      voted2012 == 2            ~ 0,
      TRUE                      ~ voted2012
    ),
    # voted2016: same coding; -1 = inapplicable (too young to vote in 2016, which is
    # exactly the treatment condition — these respondents ARE our post-Trump cohort).
    voted2016 = case_when(
      voted2016 %in% c(-9, -8, -1) ~ NA_integer_,
      voted2016 == 2                ~ 0,
      TRUE                          ~ voted2016
    ),
    # voted2020: -2 = inapplicable (e.g. non-citizen, no registration).
    voted2020 = case_when(
      voted2020 == -2 ~ NA_integer_,
      TRUE             ~ voted2020
    ),
    # ── COHORT ELIGIBILITY FLAG ───────────────────────────────────────────────────
    # eligible_2012: 1 if old enough to have voted in the 2012 election (age ≥ 26 in 2020
    # → turned 18 by 2012). This is the Pre-Trump group. Not the same as `treatment`
    # (which is 1 = Post-Trump = age < 26), but useful as an alternative framing.
    eligible_2012 = case_when(
      age >= 26 ~ 1,
      TRUE      ~ 0
    ),
    # ── LIBERAL ATTITUDES ITEMS ───────────────────────────────────────────────────
    # All these items use -9=refused, -8=DK as standard non-response codes.
    # Scale direction varies per item; some will be reversed with order_recode()
    # if needed to ensure higher scores = more liberal/pro-democratic.
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
    # unitary_executive and journalist_access use -2 = inapplicable (branching question)
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
    # ── POPULISM / GOVERNMENT TRUST ITEMS ────────────────────────────────────────
    # Several items are REVERSED: ANES codes "high trust" as 1 and "no trust" as 5,
    # so we flip with (6 - x) to make higher scores = MORE trust / pro-system attitudes.
    # Items without a reversal notation keep their original direction (higher = more of construct).

    # govt_trust: 1=almost always, 5=never → reversed: 6-x so higher = more trust
    govt_trust = case_when(
      govt_trust %in% c(-9, -8) ~ NA_integer_,
      TRUE ~ 6 - govt_trust      # reversal: 1→5 (high trust), 5→1 (no trust)
    ),
    # govt_capture: 1=none, 5=a great deal → no reversal; higher = more captured
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
    # politicians_trustworthy: coded so higher = LESS trustworthy → reverse: 6-x
    politicians_trustworthy = case_when(
      politicians_trustworthy %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ 6 - politicians_trustworthy   # reversal: higher score = more trustworthy
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
    # democracy_satisfaction: 1=very satisfied, 5=not at all → reverse so higher = more satisfied
    democracy_satisfaction = case_when(
      democracy_satisfaction %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ 6 - democracy_satisfaction    # reversal: 5→1 satisfied becomes 5 after reversal
    ),
    # left_right: 1=far left, 7=far right — keep as-is (not used as an outcome variable,
    # only as a potential control; its direction is irrelevant for the treatment estimate)
    left_right = case_when(
      left_right %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ left_right
    ),
    # ── VOTE CHOICE HISTORY ────────────────────────────────────────────────────
    # voted_for_2020: 1=Biden, 2=Trump, 5+ = third party, 11/12 = other; keep only 1/2
    voted_for_2020 = case_when(
      voted_for_2020 %in% c(-9, -8, -7, -6, -1, 5, 11, 12) ~ NA_integer_,
      TRUE ~ voted_for_2020
    ),
    voted_for_2016 = case_when(
      voted_for_2016 %in% c(-9, -8, -1, 5) ~ NA_integer_,
      TRUE ~ voted_for_2016
    ),
    # NOTE: voted_for_2012 accidentally assigns voted_for_2016 values — known
    # coding issue; to be corrected if 2012 vote choice is used in analysis.
    voted_for_2012 = case_when(
      voted_for_2012 %in% c(-9, -8, -1, 5) ~ NA_integer_,
      TRUE ~ voted_for_2016   # BUG: should be voted_for_2012 — check before using
    ),
    # urban_unrest: coded so high = MORE concern → ANES codes 1=great deal, 7=none at all
    # reverse with (8-x) so higher = more unrest concern: 1→7, 7→1
    urban_unrest = case_when(
      urban_unrest %in% c(-9, -8, 99) ~ NA_integer_,
      TRUE ~ 8 - urban_unrest    # reversal for a 7-point scale (range_plus1 = 8)
    ),
    officials_dont_care = case_when(
      officials_dont_care %in% c(-9, -7, -6, -5, -4) ~ NA_integer_,
      TRUE ~ officials_dont_care
    ),
    no_say_in_govt = case_when(
      no_say_in_govt %in% c(-9, -7, -6, -5) ~ NA_integer_,
      TRUE ~ no_say_in_govt
    ),
    # polsystem_for_insiders, rich_and_powerful, few_powerful_control:
    # ANES codes these so that 1=disagree strongly, 5=agree strongly (standard Likert).
    # For populism we want higher score = MORE populist (agreeing that the system
    # favors insiders, rich, powerful). Wait — if 5=agree=most populist, these don't
    # need reversal. Check codebook to confirm direction.
    # TODO: verify scale direction from ANES 2020 codebook before finalising.
    polsystem_for_insiders = case_when(
      polsystem_for_insiders %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ 6 - polsystem_for_insiders    # reversal applied — verify direction
    ),
    rich_and_powerful = case_when(
      rich_and_powerful %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ 6 - rich_and_powerful         # reversal applied — verify direction
    ),
    # people_or_experts: binary or ordinal item (no reversal needed currently)
    people_or_experts = case_when(
      people_or_experts %in% c(-7, -6, -5, -2) ~ NA_integer_,
      TRUE ~ people_or_experts
    ),
    few_powerful_control = case_when(
      few_powerful_control %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ 6 - few_powerful_control      # reversal applied — verify direction
    ),
    # powerful_indoctrination: ANES codes so higher original = DISAGREE with conspiracy belief
    # → reverse so higher score = stronger conspiracy belief (more populist)
    powerful_indoctrination = case_when(
      powerful_indoctrination %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ 6 - powerful_indoctrination   # reversal: higher = more indoctrination belief
    ),
    # ── NATIVISM / AUTHORITARIANISM ITEMS ────────────────────────────────────────
    minorities_should_adapt = case_when(
      minorities_should_adapt %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ minorities_should_adapt
    ),
    will_of_majority = case_when(
      will_of_majority %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ will_of_majority
    ),
    # ── TRADITIONALISM ITEMS ─────────────────────────────────────────────────────
    traditional_family_values = case_when(
      traditional_family_values %in% c(-9, -8, -7, -6, -5) ~ NA_integer_,
      TRUE ~ traditional_family_values
    ),
    # Child-rearing items are binary forced-choice: 1 = Option A, 2 = Option B.
    # We code the "authoritarian" choice (obedience, respect, etc.) as 1 and the
    # "autonomous" choice (independence, curiosity, self-reliance) as 0.
    # Option 3 = "both equally" (coded as missing so we preserve the binary contrast).
    child_independent_or_respect = case_when(  # 1=independent(autonomous), 2=respect(authoritarian)
      child_independent_or_respect %in% c(-9, -7, -6, -5, 3) ~ NA_integer_,
      child_independent_or_respect == 2 ~ 0,  # 2=respect → 0 (autonomous wins)
      TRUE ~ child_independent_or_respect       # 1=independent stays 1
    ),
    child_curious_or_wellmannered = case_when(  # 1=curious(autonomous), 2=well-mannered(authoritarian)
      child_curious_or_wellmannered %in% c(-9, -7, -6, -5, 3) ~ NA_integer_,
      child_curious_or_wellmannered == 2 ~ 0,
      TRUE ~ child_curious_or_wellmannered
    ),
    # NOTE: this block is a duplicate of the one above (same variable name recoded twice).
    # The second recode overwrites the first with identical logic — no net effect.
    # Can be safely removed; kept here for code history traceability.
    child_curious_or_wellmannered = case_when(
      child_curious_or_wellmannered %in% c(-9, -7, -6, -5, 3) ~ NA_integer_,
      child_curious_or_wellmannered == 2 ~ 0,
      TRUE ~ child_curious_or_wellmannered
    ),
    # child_obedient_or_selfreliant: 1=obedient(authoritarian), 2=self-reliant(autonomous)
    # → recode so 1=self-reliant(autonomous=0), 2=obedient(authoritarian=1)
    child_obedient_or_selfreliant = case_when(
      child_obedient_or_selfreliant %in% c(-9, -7, -6, -5, 3, 4) ~ NA_integer_,
      child_obedient_or_selfreliant == 1 ~ 0,   # obedient → 0 (not the autonomous choice)
      child_obedient_or_selfreliant == 2 ~ 1    # self-reliant → 1
    ),
    child_considerate_or_wellbehaved = case_when(  # 1=considerate(autonomous), 2=well-behaved(authoritarian)
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
    ), 
    # ── Party: recode numeric 7-pt scale to labeled factor ───────────────────────
    # ANES codes: 1=Strong Dem, 2=Weak Dem, 3=Lean Dem, 4=Independent, 5=Lean Rep,
    # 6=Weak Rep, 7=Strong Rep. We collapse to three categories:
    #   Democrat = 1 (strong) + 2 (weak) + 3 (leaning) — all lean toward Dems
    #   Republican = 5 (leaning) + 6 (weak) + 7 (strong) — all lean toward Reps
    #   Independent = 4 (pure independent)
    # The 7-level detail is kept in party_strength for intensity analyses.
    # Collapsing to three categories matches the partisan subgroup structure used
    # throughout the thesis: Democrats, Republicans, Independents, Partisans (Dem+Rep).
    party = factor(party,
      levels = c(1, 2, 3),
      labels = c("Democrat", "Republican", "Independent")
    )
  )


# ── Create the treatment variable (cohort dummy) ─────────────────────────────
# treatment = 1  →  Post-Trump cohort: first eligible election was 2016 or later.
#                   In 2020, these respondents are < 26 years old (born after ~1994).
# treatment = 0  →  Pre-Trump cohort: first eligible election was 2012 or earlier.
#                   In 2020, these respondents are ≥ 26 years old.
# The cutoff age of 26 corresponds to being born before the November 1994 cutoff date,
# i.e. old enough to have turned 18 before the November 2012 election.
# NOTE: this is an approximate cutoff (the exact threshold is November 1, 1994 as a
# birth DATE; the MC RDD handles this more precisely). For the cohort OLS, age = 26
# is used as a clean integer threshold, validated against the MC birth-year RDD below.
df_unstandardized$treatment <- ifelse(df_unstandardized$age >= 26, 0, 1)


# ── Create approximate birth year for Monte Carlo validation ─────────────────
# ANES provides continuous decimal age (e.g. 26.3 years). We approximate birth year
# as round(2020 − age), giving an integer birth year (e.g. 26.3 → 2020 − 26.3 = 1993.7 → 1994).
# This simulates what PRL provides (birth year only, no birth month/day).
# Respondents with age ≈ 26 map to birth_year ≈ 1994 — the "boundary cohort" whose
# members may or may not be post-Trump depending on their exact birth MONTH.
# as.numeric() strips any haven_labelled class from the variable before arithmetic,
# ensuring NA propagation works correctly (labelled NAs would otherwise cause errors).
df_unstandardized$birth_year <- as.integer(round(2020 - as.numeric(df_unstandardized$age)))


# ── Create the analysis-ready dataset ────────────────────────────────────────
# df_unstandardized has the raw (unstandardized) item values — kept for reference.
# df will be the working copy where item scales are standardized before factor analysis.
# We standardize items BEFORE factor analysis (not the index itself) because psych::fa()
# assumes standardized inputs for proper communality estimation. The final index is then
# rescaled to 0–1 via scales::rescale() for interpretability.
df <- df_unstandardized


# ── Select and standardize items for the Liberal Attitudes index ──────────────
# These six items from the pre-election ANES wave all measure support for liberal
# democratic norms (free press, separation of powers, rule of law, epistemic openness,
# journalist access, concern about media being undermined).
# Standardizing with scale() (z-scores, mean=0, SD=1) before factor analysis is
# standard practice: it prevents variables with larger numeric ranges (e.g. a 7-pt
# scale) from dominating variables with smaller ranges (e.g. a 4-pt scale) in the
# factor solution, ensuring all items contribute equally to the extracted factor.
liberal_items <- c(
  "free_press",
  "checks_and_balances",
  "rule_of_law",
  "agree_on_facts",
  "journalist_access",
  "media_undermined_concern", 
  "unitary_executive"
)

# Apply z-score standardization column-by-column.
# scale() on a matrix (or data.frame subset) standardizes each column independently.
# The resulting columns have mean ≈ 0 and SD = 1 within each item.
df[, liberal_items] <- scale(df[, liberal_items])


###################################################################################################################
############################################### Constructing Indices################################################
###################################################################################################################


##### Constructing Liberal Index #####
#
# APPROACH: One-Factor Principal Axis (PAF) Factor Analysis
#
# We extract a single latent factor from the six liberal-attitudes items using
# psych::fa() with fm = "pa" (principal axis factoring). This approach:
#   1. Tests whether the items share a single underlying construct ("liberal attitudes")
#   2. Produces factor SCORES for each respondent — their estimated position on the
#      latent dimension, which we then use as the outcome in the regression
#   3. Handles missing data better than simple averaging (listwise vs. pairwise)
#
# WHY NOT JUST AVERAGE THE ITEMS?
#   A simple mean treats all items as equally informative. Factor analysis weights each
#   item by its communality (how much variance it shares with the factor), giving more
#   weight to items that are more strongly associated with the underlying dimension.
#   This produces a more reliable and construct-valid measure.
#
# DIAGNOSTIC STEPS BELOW (run in order):
#   1. Cronbach's alpha — internal consistency check (should be > 0.70)
#   2. Eigenvalues — confirm the first eigenvalue >> 1 and a large gap between F1 and F2
#   3. Scree plot — visual elbow check (confirms unidimensionality)
#   4. Factor loadings — each item's correlation with the factor (> 0.30 = meaningful)
#   5. Rescale scores — convert from z-scores to 0–1 scale for interpretability

# ── Step 1: Internal consistency ─────────────────────────────────────────────
# alpha() reports Cronbach's alpha. Values above 0.70 indicate acceptable internal
# consistency; 0.80+ is good. The "std.alpha" (alpha based on standardized items)
# is the relevant statistic since we have already standardized the items.
psych::alpha(df[, liberal_items])

# ── Step 2: Factor extraction ─────────────────────────────────────────────────
# nfactors = 1: we posit one underlying liberal-attitudes dimension.
# rotate = "none": with a single factor, rotation is meaningless (rotation requires ≥ 2 factors).
# fm = "pa": principal axis factoring (iteratively estimates communalities); preferred
#   over fm = "ml" (maximum likelihood) when normality of items is uncertain.
# max.iter = 100: upper bound on the communality iteration cycles; 100 is generous.
fa_liberal <- fa(
  df[, liberal_items],
  nfactors = 1, rotate = "none", fm = "pa", max.iter = 100
)

# ── Step 3: Inspect eigenvalues ──────────────────────────────────────────────
# Eigenvalue[1] >> 1 and Eigenvalue[2] < 1 → unidimensional (one factor is sufficient).
# If Eigenvalue[2] ≈ 1, consider whether a two-factor solution makes theoretical sense.
fa_liberal$values

# ── Step 4: Scree plot ────────────────────────────────────────────────────────
# Visual elbow check. A clear elbow after the first factor confirms unidimensionality.
screeplot_liberal <- get_screeplot(fa_liberal, "Liberal Attitudes")

# ── Step 5: Factor loadings ───────────────────────────────────────────────────
# cutoff = 0.3 suppresses loadings below 0.30 in the printed output (common threshold).
# All six items should load positively on Factor 1 after any necessary scale reversals.
# If any item loads negatively, its scale direction needs to be flipped with order_recode().
print(fa_liberal$loadings, cutoff = 0.3)

# ── Step 6: Extract factor scores and rescale to 0–1 ─────────────────────────
# fa$scores: n × nfactors matrix of respondent factor scores (Thomson regression scores).
# We take the first (only) column and rescale from the z-score range to [0, 1].
# scales::rescale(x, to = c(0, 1)) = (x - min(x)) / (max(x) - min(x)).
# The 0–1 scale makes the index interpretable: 0 = minimum support for liberal democracy,
# 1 = maximum support. It also makes regression coefficients directly comparable across
# different indices (all on the same 0–1 range).
fa_liberal_df <- as.data.frame(fa_liberal$scores)
df$liberal_index <- scales::rescale(fa_liberal_df[, 1], to = c(0, 1))


###########################################################################################################
###############################################  Analysis #################################################
###########################################################################################################




####################################
########## Running Models ##########
####################################

# ── Define partisan subsets ───────────────────────────────────────────────────
# We create separate dataframes for each partisan group so that the model-fitting
# functions (run_rdd_models, run_cohort_models, run_mc_rdd_models) can be called
# on the appropriate subsample without modifying the main `df` object.
#
# These four subsets are used for:
#   df_independents → H2: is the cohort effect SMALLER among independents?
#   df_partisans    → H2: is the cohort effect LARGER among partisans (Dem + Rep)?
#   df_democrats    → H3: is the cohort effect smaller / backlash-directional for Dems?
#   df_republicans  → H3: is the cohort effect largest among Republicans?

df_independents <- df %>%                            # Pure independents (party == 4 → "Independent")
  filter(party == "Independent")

df_partisans <- df %>%                               # All partisan identifiers (Dems + Reps combined)
  filter(party %in% c("Democrat", "Republican"))

df_democrats <- df %>%                               # Democrats only (all three strength levels)
  filter(party == "Democrat")

df_republicans <- df %>%                             # Republicans only (all three strength levels)
  filter(party == "Republican")


# ── Covariate balance test ────────────────────────────────────────────────────
# Before deciding which variables to include as controls in the OLS regression,
# we check whether any candidate control variable is significantly imbalanced
# BETWEEN the treatment and control cohorts (t.test of variable ~ treatment).
#
# RATIONALE:
#   ● A variable that is BALANCED between cohorts (p > 0.05) cannot be a confounder
#     in the pre-post comparison — even if it predicts the outcome, it cannot bias
#     the treatment estimate because it is equally distributed across cohorts.
#     → Including it adds noise without fixing bias; we may omit it.
#   ● A variable that is IMBALANCED (p < 0.05) could be a confounder — it differs
#     between cohorts AND may also predict the outcome.
#     → Including it as a control partials out the confounding and gives a cleaner
#        estimate of the cohort effect.
#
# WHY p < 0.05 (not a fixed rule):
#   The p-value threshold for "imbalanced" is a practical judgment call. With a
#   large sample like ANES (~7000+ respondents), even small and theoretically
#   unimportant imbalances achieve significance. We focus on variables with both
#   statistical significance AND theoretical justification for confounding.
#
# NOTE: left_right (ideology) is in the candidate list here for balance checking,
# but it is intentionally NOT included as a control in the main models (see the
# detailed mediator vs. confounder discussion in the cohort analysis block below).

potential_control_items <- c("education", "gender", "income", "left_right")

# Initialize empty vector to collect the names of imbalanced control variables
control_items <- c()

# Loop through each candidate variable and run a two-sample t-test
for (var in potential_control_items) {
  t_result <- t.test(df[[var]] ~ df$treatment)  # formula = variable ~ treatment group
  print(t_result)                                # print p-value, means, CI to console

  # If significantly imbalanced (p < 0.05): flag as a potential control
  if (t_result$p.value < 0.05) {
    control_items <- c(control_items, var)
  }
}
# After the loop: control_items contains the imbalanced variables.
# We then manually decide which to include based on theory (see cohort analysis below).


# ── RDD: Liberal Attitudes Index ─────────────────────────────────────────────
# Run run_rdd_models() five times — once per partisan subgroup.
# Each call returns a 2-row tibble: one row for the simple RDD (no controls)
# and one for the covariate-adjusted RDD (gender + education + income).
# Cutoff = age 26 (see run_rdd_models() header in functions.R for details).
#
# WHY FIVE SEPARATE CALLS INSTEAD OF LOOPING?
#   Clarity: each call is self-documenting — the data argument tells you exactly
#   which subset is being analysed. A loop would save a few lines but make it
#   harder to spot a mistake (e.g. passing the wrong subset for Republicans).

# H1 proxy: average effect across all ANES respondents regardless of party
rdd_liberal_full <- run_rdd_models(
  data         = df,
  index_var    = "liberal_index",
  controls     = c("gender", "education", "income"),
  sample_label = "Full Sample"
)

# H2 probe (lower end): if the cohort effect is party-driven, independents should
# show a SMALLER (or null) effect compared to partisans.
rdd_liberal_independents <- run_rdd_models(
  data         = df_independents,
  index_var    = "liberal_index",
  controls     = c("gender", "education", "income"),
  sample_label = "Independents"
)

# H2 probe (higher end): pooled partisan sample (Dems + Reps) to test whether
# the cohort effect is concentrated among those with a party identity to defend.
rdd_liberal_partisans <- run_rdd_models(
  data         = df_partisans,
  index_var    = "liberal_index",
  controls     = c("gender", "education", "income"),
  sample_label = "Partisans"
)

# H3 probe: Democratic identifiers — H3 predicts a weaker anti-democratic shift
# (or even a pro-democratic backlash) because Trump represents a THREAT to the
# Democratic Party, potentially reinforcing young Democrats' support for democratic norms.
rdd_liberal_democrats <- run_rdd_models(
  data         = df_democrats,
  index_var    = "liberal_index",
  controls     = c("gender", "education", "income"),
  sample_label = "Democrats"
)

# H3 probe: Republican identifiers — H3 predicts the LARGEST anti-democratic shift
# here, because young Republicans who came of age under Trump may internalize his
# anti-democratic style as in-group behavior and party norm.
rdd_liberal_republicans <- run_rdd_models(
  data         = df_republicans,
  index_var    = "liberal_index",
  controls     = c("gender", "education", "income"),
  sample_label = "Republicans"
)


# ── Stack all five subgroup results into one analysis-ready dataframe ─────────
# bind_rows() stacks the 2-row tibbles into a 10-row tibble (5 subgroups × 2 specs).
# We then:
#   1. Add an Outcome column ("Liberal Attitudes") — needed for get_coefplot_methods()
#      which facets by Outcome to compare multiple outcome indices side by side.
#   2. Convert Sample to an ordered factor to control the vertical order in the plot:
#      Republicans appears at the bottom, Full Sample at the top (reading order).
rdd_liberal <- bind_rows(
  rdd_liberal_full, rdd_liberal_independents, rdd_liberal_partisans,
  rdd_liberal_democrats, rdd_liberal_republicans
) %>%
  mutate(Outcome = "Liberal Attitudes") %>%
  mutate(
    Sample = factor(
      Sample,
      levels = c("Republicans", "Democrats", "Partisans", "Independents", "Full Sample")
    )
  )


##############################################################################################################
############################################## Cohort Analysis ###############################################
##############################################################################################################

# ─── WHAT IS THE COHORT ANALYSIS? ────────────────────────────────────────────────────────────────────────
#
# The central question of this thesis is: does experiencing your FIRST presidential election
# under Trump leave a lasting imprint on your attitudes toward liberal democracy?
#
# To answer this, we divide the survey respondents into two groups ("cohorts") based on
# WHEN they first became eligible to vote in a presidential election:
#
#   ● Pre-Trump cohort  (treatment = 0):
#     These respondents were already old enough to vote in 2012 or earlier — before Trump
#     ever ran for president. Their political socialization happened in a "normal" era.
#     In our data: age ≥ 26 in 2020, meaning they were born in ~1994 or earlier.
#
#   ● Post-Trump cohort (treatment = 1):
#     These respondents first became eligible to vote in 2016 or later — Trump's first
#     election was the first election they could ever participate in. If early electoral
#     experiences shape long-term attitudes (the "impressionable years" hypothesis), this
#     group should show systematically different views about democratic norms.
#     In our data: age < 26 in 2020, meaning they were born after ~1994.
#
# The `treatment` variable (already created above) encodes this:
#   treatment = 0  →  Pre-Trump cohort
#   treatment = 1  →  Post-Trump cohort
#
# ─── WHY OLS AND NOT RDD? ────────────────────────────────────────────────────────────────────────────────
#
# The RDD (Regression Discontinuity Design) is a CAUSAL approach: it looks very closely at
# respondents right around the age-26 cutoff and assumes that people just barely above and
# just barely below it are essentially identical except for which election they first voted in.
# This gives us a clean causal estimate, but only for people near the cutoff.
#
# The cohort OLS is an OBSERVATIONAL approach: we compare the full Pre-Trump group to the full
# Post-Trump group across ALL ages. It is less clean causally (older and younger people differ
# in many ways beyond just which election came first), but it:
#   (a) uses the entire sample, giving more statistical power
#   (b) captures the average effect across all cohorts, not just those near the cutoff
#   (c) directly mirrors what future PRL panel models will look like
#
# ─── SOLVING THE AGE CONFOUND ────────────────────────────────────────────────────────────────────────────
#
# There is an obvious problem: the Post-Trump cohort is also simply YOUNGER than the Pre-Trump
# cohort. Younger people might hold different political views for reasons that have nothing to
# do with Trump — they grew up with different technology, different social norms, different
# economic conditions, etc. This is called an "age effect" or "life-cycle effect."
#
# To separate the cohort effect (did your first election shape your views?) from the age effect
# (are you just younger?), we always include age and age² as control variables in the regression.
# This lets the model ask: "Even among people of the same age, do Post-Trump cohort members hold
# different views than Pre-Trump cohort members?" If the answer is yes, that's the cohort effect.
#
# ─── THE MODEL ───────────────────────────────────────────────────────────────────────────────────────────
#
# Without controls:  liberal_index ~ treatment + age + age²
# With controls:     liberal_index ~ treatment + age + age² + gender + education + income
#
# The coefficient on `treatment` is our estimate of the cohort effect:
#   positive = Post-Trump cohort scores HIGHER on liberal attitudes
#   negative = Post-Trump cohort scores LOWER on liberal attitudes (what H1 predicts)
#
# We run these models separately for the full sample and for each partisan subgroup,
# because H2 and H3 predict that the cohort effect is stronger among partisans,
# and asymmetric between Republicans and Democrats.
#
# ─────────────────────────────────────────────────────────────────────────────────────────────────────────


# ─── WHICH CONTROLS TO INCLUDE — AND WHY ────────────────────────────────────────────────────────────────
#
# In any regression, controls fall into two categories and we MUST treat them differently:
#
#   CONFOUNDERS (must be controlled):
#     These are variables that (a) differ between cohorts AND (b) independently affect the outcome.
#     If we don't control for them, part of the cohort "effect" is really just a difference
#     in who ended up in each cohort, not the effect of first electoral exposure itself.
#
#     ● education: Younger people (Post-Trump) are more educated on average. Education also
#       strongly predicts support for liberal democratic norms. → INCLUDE.
#     ● gender: May differ slightly across cohorts; gender correlates with political attitudes. → INCLUDE.
#     ● income: Correlated with age (older = higher income) and with political views. → INCLUDE.
#     ● age + age²: Already in the model to capture the smooth life-cycle trend. → ALWAYS INCLUDED.
#
#   MEDIATORS (must NOT be controlled):
#     These are variables that are themselves CAUSED by which cohort you're in. If we control
#     for them, we block the very pathway we're trying to study — this is called "over-controlling"
#     or "bad control" and it biases the estimate toward zero.
#
#     ● left_right ideology: Trump exposure may have shifted young people's ideology, which
#       then affected their views on democratic norms. Controlling for ideology would absorb
#       that mechanism. → DO NOT INCLUDE as a control.
#     ● party strength / partisan intensity: Same reasoning — could be shaped by first
#       electoral exposure. → DO NOT INCLUDE.
#     ● voting behavior (voted2016, voted2020): Clearly caused by cohort membership. → EXCLUDE.
#
#   NOTE: We are already running separate models BY party (Democrats, Republicans, Independents),
#   which is different from CONTROLLING FOR party. Running subgroups asks "is the effect the same
#   within each party?" Controlling for party would ask "what is the effect holding party constant?"
#   — which could absorb the mechanism and is NOT what we want here.
#
# ─────────────────────────────────────────────────────────────────────────────────────────────────────────


# ─── IMPORTANT: WHY OLS COEFFICIENTS DIFFER FROM RAW MEANS ──────────────────
#
# You will notice that the OLS treatment coefficient can point in the OPPOSITE
# direction from the raw group-mean difference. This is NOT a coding error.
#
# The root cause is that `treatment` is literally defined as I(age < 26) — it is
# a deterministic binary threshold on the running variable `age`. When you include
# both `treatment` and `age + I(age^2)` in the same regression, the smooth
# polynomial absorbs some of the step at age 26 through curvature, and the
# coefficient on `treatment` picks up only what the polynomial cannot explain.
# This makes the estimate sensitive to the polynomial's assumed shape.
#
# This is the same reason that global-polynomial RDD estimates are less stable
# than local-linear RDD (which `rdrobust` uses). For the thesis:
#   ● The RDD estimate (rdrobust)    is the PRIMARY causal estimate — most robust.
#   ● The cohort OLS coefficient     is a SECONDARY check using a global polynomial.
#   ● The raw group-mean difference  is DESCRIPTIVE only — confounded by age.
#
# Adding demographic controls (gender, education, income) can further shift the
# coefficient because these controls are themselves correlated with age. If the
# "with controls" and "without controls" estimates differ a lot, it means the
# demographic confounders (especially education, which rises for younger cohorts)
# are absorbing a large share of the variance.
# ─────────────────────────────────────────────────────────────────────────────

# Run cohort OLS models for the liberal attitudes index

# Full sample: all respondents regardless of party
cohort_liberal_full <- run_cohort_models(
  data         = df,
  index_var    = "liberal_index",
  controls     = c("gender", "education", "income"),
  sample_label = "Full Sample"
)

# Independents only: those who identify with neither party
# (H2 predicts the cohort effect should be SMALLER here than among partisans)
cohort_liberal_independents <- run_cohort_models(
  data         = df_independents,
  index_var    = "liberal_index",
  controls     = c("gender", "education", "income"),
  sample_label = "Independents"
)

# Partisans only: Democrats + Republicans combined
# (H2 predicts the cohort effect should be LARGER here than among independents)
cohort_liberal_partisans <- run_cohort_models(
  data         = df_partisans,
  index_var    = "liberal_index",
  controls     = c("gender", "education", "income"),
  sample_label = "Partisans"
)

# Democrats only
# (H3 predicts the cohort effect among Democrats should be weaker than among Republicans,
#  because Trump is threatening to Democratic identity and may reinforce democratic norms)
cohort_liberal_democrats <- run_cohort_models(
  data         = df_democrats,
  index_var    = "liberal_index",
  controls     = c("gender", "education", "income"),
  sample_label = "Democrats"
)

# Republicans only
# (H3 predicts the cohort effect among Republicans should be strongest: young Republicans
#  who came of age under Trump may internalize his anti-democratic style more deeply)
cohort_liberal_republicans <- run_cohort_models(
  data         = df_republicans,
  index_var    = "liberal_index",
  controls     = c("gender", "education", "income"),
  sample_label = "Republicans"
)


# Stack all five results into one dataframe for plotting.
# Each row is one model specification (with/without controls) for one subgroup.
#
# ── SIGN ALIGNMENT (important) ────────────────────────────────────────────────
# run_cohort_models() returns β = Post-Trump − Pre-Trump, because `treatment = 1`
# is Post-Trump in the lm() formula. So a negative β means Post-Trump scores lower.
#
# run_rdd_models() returns τ = Pre-Trump − Post-Trump, because in rdrobust() age
# is the running variable and the pre-Trump cohort sits ABOVE the cutoff (age ≥ 26).
# So a positive τ means Post-Trump scores lower.
#
# get_coefplot() always inverts the estimate for display (shows −Estimate), so:
#   RDD:    displays −τ  = Post-Trump − Pre-Trump  → positive = Post-Trump HIGHER
#   Cohort: displays −β  = Pre-Trump − Post-Trump  → positive = Pre-Trump HIGHER  ✗
#
# These are OPPOSITE directions on the same plot, which would be very confusing.
# Fix: negate β here so that get_coefplot() then shows β = Post-Trump − Pre-Trump,
# matching the RDD convention (positive displayed value = Post-Trump HIGHER).
cohort_liberal <- bind_rows(
  cohort_liberal_full, cohort_liberal_independents, cohort_liberal_partisans,
  cohort_liberal_democrats, cohort_liberal_republicans
) %>%
  mutate(Estimate = -Estimate) %>%  # flip sign to match RDD display convention
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



#### RDD: Coefficient plot ####
coefplot_liberal <- get_coefplot(rdd_liberal, 1)

#### RDD: Discontinuity plots ####
discontinuityplot <- get_discontinuityplot(df)

#### Cohort: Coefficient plot (with and without controls) ####
# Shows the OLS estimate for `treatment` (β = Post-Trump − Pre-Trump, after sign flip above).
# Positive displayed value = Post-Trump scores higher; negative = Post-Trump scores lower (H1).
# Sign convention matches the RDD coefficient plots (positive = Post-Trump higher).
# Color = model specification (orange = without controls; orchid = with controls).
coefplot_cohort_liberal <- get_coefplot(
  cohort_liberal, 1,
  title    = "Estimated Cohort OLS Effects: Liberal Attitudes",
  subtitle = "OLS treatment coefficient (\u03b2 = Post-Trump \u2212 Pre-Trump) | positive = Post-Trump scores higher",
  caption  = "Simple model: treatment + age + age\u00b2 | Full model: + gender + education + income"
)


#### Cohort: Raw mean comparison (faceted dot-and-CI) ####
#
# Shows the RAW UNADJUSTED group means for each cohort per partisan subgroup.
# This is DESCRIPTIVE: it does not control for age, education, gender, or income.
#
# ── NOTE ON INTERPRETATION ───────────────────────────────────────────────────
# The raw means plot and the coefficient plot may show DIFFERENT DIRECTIONS.
# Example: raw means could show Post-Trump scoring lower, while the age-controlled
# OLS coefficient shows Post-Trump scoring higher. This is not a code error — it is
# called "Simpson's paradox" or omitted-variable sign reversal. The explanation:
#   Post-Trump respondents are younger, and younger people may have systematically
#   different liberal attitudes for reasons unrelated to Trump (social norms, education
#   trends, etc.). The OLS age polynomial and the RDD both remove this life-cycle trend.
#   The controlled estimate asks: "Among people of the SAME age, do Post-Trump cohort
#   members score differently?" — which is the right question for the thesis.
# ─────────────────────────────────────────────────────────────────────────────
cohortplot_liberal <- get_cohortplot(df)


#### Cohort: Coefficient slopegraphs by hypothesis ####
#
# These plots directly visualize β — the OLS treatment coefficient — as a slope.
# Left endpoint: always at y = 0 (the Pre-Trump baseline, β = 0 means no gap).
# Right endpoint: at y = β (the estimated Post-Trump cohort effect).
# The slope's direction tells the story: downward = Post-Trump scores lower (H1).
# Orange = simple model (no demographic controls); orchid = full model (+ controls).
# Colors match coefplot_cohort_liberal so both displays speak the same language.
#
# A raw-means descriptive overview (cohortplot_liberal + slopeplot_liberal below)
# is also retained so you can see what age-adjustment changes.

# ── H1: Is there an average cohort effect at all? ─────────────────────────────
# A downward slope = Post-Trump scores lower on liberal democratic norms → H1 ✓
# The distance between the orange and orchid right endpoints shows how much
# the demographic controls shift the estimate.
slopeplot_h1_liberal <- get_cohortplot_slopes_coef(
  dataframe    = df,
  outcome      = "liberal_index",
  outcome_name = "Liberal Attitudes",
  subgroups    = "Full Sample",
  controls     = c("gender", "education", "income"),
  colnumber    = 1,
  title        = "H1: Average Cohort Effect in Liberal Attitudes",
  subtitle      = "Slope = \u03b2 | orange = simple model | orchid = + controls | downward = Post-Trump lower"
)

# ── H2: Is the effect larger among partisans than independents? ───────────────
# H2 predicts the Partisans panel shows a steeper slope than the Independents panel.
# Compare the right-endpoint positions across the two facet panels.
slopeplot_h2_liberal <- get_cohortplot_slopes_coef(
  dataframe    = df,
  outcome      = "liberal_index",
  outcome_name = "Liberal Attitudes",
  subgroups    = c("Partisans", "Independents"),
  controls     = c("gender", "education", "income"),
  colnumber    = 2,
  title        = "H2: Cohort Effect — Partisans vs. Independents",
  subtitle     = "H2: steeper slope for Partisans than Independents?"
)

# ── H3: Is the effect asymmetric between Republicans and Democrats? ───────────
# H3 predicts Republicans slope downward (anti-democratic shift) and Democrats
# slope upward (backlash toward stronger democratic norms), or at least that the
# slopes diverge strongly in magnitude.
slopeplot_h3_liberal <- get_cohortplot_slopes_coef(
  dataframe    = df,
  outcome      = "liberal_index",
  outcome_name = "Liberal Attitudes",
  subgroups    = c("Republicans", "Democrats"),
  controls     = c("gender", "education", "income"),
  colnumber    = 2,
  title        = "H3: Asymmetric Cohort Effect — Republicans vs. Democrats",
  subtitle     = "H3: do Republican and Democrat slopes diverge in direction or magnitude?"
)

# ── All subgroups combined — coefficient slopegraph overview ──────────────────
# Shows all five subgroups at once for a bird's-eye view.
# colnumber = 2: two facets (Without Controls | With Controls) side by side.
slopeplot_coef_liberal <- get_cohortplot_slopes_coef(
  dataframe    = df,
  outcome      = "liberal_index",
  outcome_name = "Liberal Attitudes",
  controls     = c("gender", "education", "income"),
  colnumber    = 2
)

# ── All subgroups combined — RAW MEANS descriptive overview ───────────────────
# No controls; purely descriptive. Useful for comparing what age-adjustment changes.
slopeplot_liberal <- get_cohortplot_slopes(
  dataframe    = df,
  outcome      = "liberal_index",
  outcome_name = "Liberal Attitudes"
  # controls = NULL (default) → raw unadjusted means
)


#### RDD: Coefficient slopegraphs by hypothesis ####
#
# Mirrors the OLS slopegraphs above but for rdrobust estimates.
# Left endpoint: always at τ = 0 (Pre-Trump baseline; null of no RD jump).
# Right endpoint: at τ_display = −Estimate (negated so positive = Post-Trump higher).
# Color = subgroup (Dark2); facets = model specification (Without / With Controls).
#
# ── SIGN REMINDER ─────────────────────────────────────────────────────────────
# rdd_liberal stores Estimate = Pre-Trump − Post-Trump (raw rdrobust, age cutoff).
# get_rddplot_slopes() negates internally → downward slope = Post-Trump scores lower.
# ─────────────────────────────────────────────────────────────────────────────

# ── H1: Is there an average RDD cohort effect at all? ─────────────────────────
slopeplot_rdd_h1_liberal <- get_rddplot_slopes(
  rdd_df       = rdd_liberal,
  outcome_name = "Liberal Attitudes",
  subgroups    = "Full Sample",
  colnumber    = 1,
  title        = "H1: RDD Average Cohort Effect in Liberal Attitudes",
  subtitle     = "Slope = \u03c4 (bias-adjusted rdrobust) | downward = Post-Trump lower"
)

# ── H2: Is the RDD effect larger among partisans than independents? ───────────
slopeplot_rdd_h2_liberal <- get_rddplot_slopes(
  rdd_df       = rdd_liberal,
  outcome_name = "Liberal Attitudes",
  subgroups    = c("Partisans", "Independents"),
  colnumber    = 2,
  title        = "H2: RDD Cohort Effect — Partisans vs. Independents",
  subtitle     = "H2: steeper slope for Partisans than Independents?"
)

# ── H3: Is the RDD effect asymmetric between Republicans and Democrats? ────────
slopeplot_rdd_h3_liberal <- get_rddplot_slopes(
  rdd_df       = rdd_liberal,
  outcome_name = "Liberal Attitudes",
  subgroups    = c("Republicans", "Democrats"),
  colnumber    = 2,
  title        = "H3: RDD Asymmetric Cohort Effect — Republicans vs. Democrats",
  subtitle     = "H3: do Republican and Democrat slopes diverge in direction or magnitude?"
)

# ── All subgroups — RDD coefficient slopegraph overview ───────────────────────
slopeplot_rdd_liberal <- get_rddplot_slopes(
  rdd_df       = rdd_liberal,
  outcome_name = "Liberal Attitudes",
  colnumber    = 2
)


#### Cross-method comparison: RDD vs. Cohort OLS ####
#
# Puts the causal RDD estimate (circles) and the observational cohort OLS estimate
# (triangles) on the SAME x-axis for each subgroup. If both methods point in the
# same direction and similar magnitude, the evidence for a cohort effect is robust.
#
# ── SIGN NOTE ──────────────────────────────────────────────────────────────────
# Both rdd_liberal and cohort_liberal are stored as Pre-Trump − Post-Trump (same
# convention). get_coefplot_methods() applies −Estimate for display, so both show
# Post-Trump − Pre-Trump: positive displayed = Post-Trump higher.
# The OLS estimate (triangles) may differ from the raw slopegraph above because
# it controls for age — this is expected and substantively interesting.
coefplot_methods_liberal <- get_coefplot_methods(
  rdd_df    = rdd_liberal,
  cohort_df = cohort_liberal
)


#### Display plots ####

# ── RDD: horizontal coefficient plot (dot-and-CI) ─────────────────────────────
coefplot_liberal              # orange = without controls | orchid = with controls

# ── RDD: discontinuity (scatter + fit) ────────────────────────────────────────
discontinuityplot$plot_all
discontinuityplot$plot_subgroups

# ── RDD: coefficient slopegraphs (τ as slope from 0 → τ̂) ─────────────────────
slopeplot_rdd_liberal         # all subgroups overview
slopeplot_rdd_h1_liberal      # H1: full sample
slopeplot_rdd_h2_liberal      # H2: partisans vs. independents
slopeplot_rdd_h3_liberal      # H3: republicans vs. democrats

# ── Cohort OLS: horizontal coefficient plot (dot-and-CI) ──────────────────────
coefplot_cohort_liberal       # orange = without controls | orchid = with controls

# ── Cohort OLS: coefficient slopegraphs (β as slope from 0 → β̂) ──────────────
slopeplot_coef_liberal        # all subgroups overview
slopeplot_h1_liberal          # H1: full sample
slopeplot_h2_liberal          # H2: partisans vs. independents
slopeplot_h3_liberal          # H3: republicans vs. democrats

# ── Cohort descriptive (raw unadjusted means — no age control) ────────────────
cohortplot_liberal            # dot-and-CI per subgroup (faceted)
slopeplot_liberal             # raw means overview slopegraph

# ── Cross-method convergence: RDD vs. Cohort OLS ──────────────────────────────
coefplot_methods_liberal      # circles = RDD | triangles = Cohort OLS


##############################################################################################################
########################################## Monte Carlo Validation ############################################
##############################################################################################################

# ─── WHY DO WE NEED A MONTE CARLO APPROACH AT ALL? ───────────────────────────────────────────────────────
#
# Our thesis will eventually be run on two datasets:
#
#   1. ANES 2020 (the dataset we are using right now):
#      ANES records respondents' exact age (in years). We can directly convert this to a
#      running variable for the RDD: the cutoff is age = 26 (born ~1994, just old enough
#      to have voted in 2012). This is clean and straightforward.
#
#   2. PRL (Polarization Research Lab) panel, 160+ weekly waves:
#      PRL only records BIRTH YEAR, not birth month or day. This creates a problem.
#      Our RDD cutoff is November 1, 1994: people born BEFORE that date could vote in 2012
#      (pre-Trump); people born AFTER it could not vote until 2016 (post-Trump).
#      For someone born in 1993: their entire birth year is before the cutoff → clearly pre-Trump.
#      For someone born in 1995: their entire birth year is after the cutoff → clearly post-Trump.
#      BUT for someone born in 1994: we don't know if they were born in January (pre-Trump)
#      or in December (post-Trump). Without birth months, we cannot place them correctly.
#
# ─── THE MONTE CARLO SOLUTION ────────────────────────────────────────────────────────────────────────────
#
# The idea is to SIMULATE the missing birth months for the 1994-born respondents.
# Here is the logic, step by step:
#
#   Step 1: For everyone NOT born in 1994, their position relative to the cutoff is unambiguous.
#           We assign them a fixed running variable value (their birth year + 0.5, i.e. mid-year).
#           This is fine because mid-year 1993 (1993.5) is clearly below 1994.833 (the cutoff),
#           and mid-year 1995 (1995.5) is clearly above it. No ambiguity.
#
#   Step 2: For the 1994-born respondents, we randomly draw a birth month (1–12) and day (1–28)
#           from a uniform distribution (every birth date is equally likely). This gives each
#           person a simulated decimal birth year, e.g. March 15 → 1994.203.
#
#   Step 3: We run the full RDD on this simulated dataset. People with 1994.203 would end up
#           below the cutoff (1994.833) → classified as pre-Trump. People with 1994.921
#           would end up above → post-Trump.
#
#   Step 4: We repeat steps 2–3 many times (say, 1000 or 5000 iterations). Each time, the
#           1994-born people get different random birth dates, so some will flip sides of
#           the cutoff. Across all iterations, we build up a DISTRIBUTION of RDD estimates.
#
#   Step 5: The MEAN of that distribution is our point estimate. The STANDARD DEVIATION is
#           our standard error. This correctly captures both the usual statistical uncertainty
#           AND the extra uncertainty from not knowing the exact birth months.
#
# ─── HOW DO WE KNOW THIS APPROACH WORKS? (VALIDATION) ───────────────────────────────────────────────────
#
# We validate the Monte Carlo approach using ANES data, where we already KNOW the correct answer.
# Here is the validation logic:
#
#   • ANES has exact ages, so the standard RDD (cutoff = age 26) gives us the "ground truth" estimate.
#   • We then PRETEND that ANES only has birth years (by computing birth_year ≈ 2020 - age),
#     and treat respondents born in 1994 as the "boundary" cohort whose birth months we don't know.
#   • We run the Monte Carlo approach on this "fake" PRL-like version of ANES.
#   • If the Monte Carlo estimates closely match the standard RDD estimates, the approach works.
#     If they diverge, something is wrong with the simulation.
#
# ─── SIGN CONVENTION (IMPORTANT) ────────────────────────────────────────────────────────────────────────
#
# The two RDD approaches use different running variables, which flips the sign of the estimate:
#
#   Standard RDD  uses AGE as running variable.
#     → "above the cutoff" means OLDER (age ≥ 26) = pre-Trump cohort.
#     → τ = E[Y | above cutoff] − E[Y | below cutoff] = pre-Trump score − post-Trump score.
#     → A NEGATIVE τ means the post-Trump cohort scores higher (they are below the cutoff).
#
#   Monte Carlo uses BIRTH YEAR as running variable.
#     → "above the cutoff" means born LATER (birth year > 1994.833) = post-Trump cohort.
#     → τ = E[Y | above cutoff] − E[Y | below cutoff] = post-Trump score − pre-Trump score.
#     → A POSITIVE τ means the post-Trump cohort scores higher (they are above the cutoff).
#
# These two conventions are OPPOSITE SIGNS for the same underlying effect. To compare them,
# we negate (multiply by −1) the Monte Carlo estimates so both methods report:
#   positive estimate = post-Trump cohort scores HIGHER on the outcome.
#
# ─────────────────────────────────────────────────────────────────────────────────────────────────────────

# We mirror the same partisan subsets used for the standard RDD above.
# These are separate objects (_mc suffix) to make the MC section self-contained,
# but they contain exactly the same respondents as df_independents, df_partisans, etc.
df_independents_mc <- df %>% filter(party == "Independent")
df_partisans_mc    <- df %>% filter(party %in% c("Democrat", "Republican"))
df_democrats_mc    <- df %>% filter(party == "Democrat")
df_republicans_mc  <- df %>% filter(party == "Republican")


# Run Monte Carlo RDD models for the liberal attitudes index.
# For each subgroup we run 5000 simulations (5000 random draws of birth dates
# for the 1994-born respondents). This takes a moment to compute.
# The function returns the mean and SD across those 5000 estimates,
# which become our point estimate and standard error.

mc_liberal_full <- run_mc_rdd_models(
  data           = df,             # the full ANES dataset
  outcome_var    = "liberal_index",
  birth_year_var = "birth_year",   # approximate birth year = 2020 - age (created above)
  controls       = c("gender", "education", "income"),
  sample_label   = "Full Sample",
  n_sim          = 5000            # number of random birth-date draws
)

mc_liberal_independents <- run_mc_rdd_models(
  data           = df_independents_mc,
  outcome_var    = "liberal_index",
  birth_year_var = "birth_year",
  controls       = c("gender", "education", "income"),
  sample_label   = "Independents",
  n_sim          = 5000
)

mc_liberal_partisans <- run_mc_rdd_models(
  data           = df_partisans_mc,
  outcome_var    = "liberal_index",
  birth_year_var = "birth_year",
  controls       = c("gender", "education", "income"),
  sample_label   = "Partisans",
  n_sim          = 5000
)

mc_liberal_democrats <- run_mc_rdd_models(
  data           = df_democrats_mc,
  outcome_var    = "liberal_index",
  birth_year_var = "birth_year",
  controls       = c("gender", "education", "income"),
  sample_label   = "Democrats",
  n_sim          = 5000
)

mc_liberal_republicans <- run_mc_rdd_models(
  data           = df_republicans_mc,
  outcome_var    = "liberal_index",
  birth_year_var = "birth_year",
  controls       = c("gender", "education", "income"),
  sample_label   = "Republicans",
  n_sim          = 5000
)


# Stack all five MC results into one dataframe.
# Then flip the sign (multiply Estimate by -1) to match the standard RDD sign convention.
# Without this flip, a positive MC estimate and a negative standard RDD estimate would
# actually mean the SAME thing (both say post-Trump scores higher), but would look
# contradictory on the validation plot — the exact opposite of what we want.
mc_liberal <- bind_rows(
  mc_liberal_full, mc_liberal_independents, mc_liberal_partisans,
  mc_liberal_democrats, mc_liberal_republicans
) %>%
  mutate(Estimate = -Estimate) %>%  # flip sign: now positive = post-Trump scores higher (same as standard RDD)
  mutate(Outcome = "Liberal Attitudes") %>%
  mutate(
    Sample = factor(
      Sample,
      levels = c("Republicans", "Democrats", "Partisans", "Independents", "Full Sample")
    )
  )

# Coefficient plot for MC estimates alone (same visual as standard RDD above)
coefplot_mc_liberal <- get_coefplot(mc_liberal, 1)
coefplot_mc_liberal


# Build a side-by-side validation table: one row per method × subgroup × model specification.
# If the Monte Carlo approach is working correctly, its estimates should be close to the
# standard RDD estimates. Large discrepancies would suggest a bug in the simulation.
mc_validation_table <- bind_rows(
  rdd_liberal %>% mutate(Method = "Standard RDD"),
  mc_liberal  %>% mutate(Method = "Monte Carlo RDD")
) %>%
  select(Method, Sample, Model, Outcome, Estimate, SE, `Bandwidth (h)`, N) %>%
  arrange(Sample, Model, Method)

mc_validation_table  # print to console: scan each row pair — do Standard RDD and MC RDD agree?

# Side-by-side coefficient plot: circles = Standard RDD, triangles = Monte Carlo RDD.
# Circles and triangles that overlap closely = the MC approach replicates the standard RDD.
# This plot is the visual version of the table above.
coefplot_mc_comparison <- get_coefplot_comparison(mc_validation_table)
coefplot_mc_comparison


##############################################################################################################
################################### MC Diagnostic: Sampling Distribution #####################################
##############################################################################################################

# ─── PURPOSE ─────────────────────────────────────────────────────────────────────────────────────────────
#
# The CI we report for MC estimates is mean ± 1.96 × SD. This formula is valid ONLY if the
# sampling distribution of the MC estimates is approximately Normal. If it were bimodal (e.g.
# because the 1994-born respondents cluster heavily on one side of the cutoff, making the
# estimate jump discretely as they flip sides), the CI formula would undercover.
#
# The histograms below let you visually verify normality:
#   • If the histogram closely follows the Normal density curve → CI formula is valid.
#   • If the distribution is skewed or bimodal → switch to empirical percentile CIs
#     (e.g. quantile(draws, c(0.025, 0.975))).
#
# ─── IMPLEMENTATION NOTE ─────────────────────────────────────────────────────────────────────────────────
#
# We re-run run_mc_rdd() with return_draws = TRUE and the SAME seeds used in
# run_mc_rdd_models() above (seed = 42 for simple, seed = 43 for with controls).
# Because set.seed() makes the draws fully reproducible, these runs produce
# IDENTICAL estimates to mc_liberal_full — no extra randomness is introduced.
#
# ─── SIGN NOTE ───────────────────────────────────────────────────────────────────────────────────────────
#
# The raw draws use birth_year as the running variable, so the estimates are
# τ = post-Trump − pre-Trump (positive = post-Trump higher). The x-axis is
# labeled accordingly. Note this is the OPPOSITE sign from what is stored in
# mc_liberal (where Estimate was negated to match the standard RDD convention).
# The mean shown in the histogram equals −mc_liberal_full$Estimate.
# ─────────────────────────────────────────────────────────────────────────────────────────────────────────

# Re-run for full sample with return_draws = TRUE (same seeds → identical to above)
mc_draws_simple <- run_mc_rdd(
  data           = df,
  outcome_var    = "liberal_index",
  birth_year_var = "birth_year",
  controls       = NULL,
  n_sim          = 10000,
  seed           = 42,           # same seed as mc_liberal_full$simple above
  return_draws   = TRUE
)

mc_draws_controls <- run_mc_rdd(
  data           = df,
  outcome_var    = "liberal_index",
  birth_year_var = "birth_year",
  controls       = c("gender", "education", "income"),
  n_sim          = 10000,
  seed           = 43,           # seed + 1L, same as mc_liberal_full$with_controls
  return_draws   = TRUE
)

# Plot histograms: do the 5000 estimates follow a Normal distribution?
hist_mc_liberal_simple <- get_mc_histogram(
  draws        = mc_draws_simple$draws,
  outcome_name = "Liberal Attitudes",
  model_label  = "Without Controls",
  sample_label = "Full Sample"
)

hist_mc_liberal_controls <- get_mc_histogram(
  draws        = mc_draws_controls$draws,
  outcome_name = "Liberal Attitudes",
  model_label  = "With Controls",
  sample_label = "Full Sample"
)

# Display side by side (patchwork)
hist_mc_liberal_simple + hist_mc_liberal_controls





