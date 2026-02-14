#########################################################################
#########################################################################
############# Replication Code: "Coming of Age Under Trump" #############
########### The Effect of First Electoral Exposure in a Trump ###########
################# Election on Long-Term Media Attitudes #################
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
library(lubridate)

# Get functions
source("/Users/nikolaosvichos/Library/Mobile Documents/com~apple~CloudDocs/Sciences Po/Thesis/Thesis Coding/Visualizations and Functions/functions.R")





# Import data
df_unclean <- haven::read_sav("/Users/nikolaosvichos/Library/Mobile Documents/com~apple~CloudDocs/Sciences Po/Thesis/Datasets/BRA/deu_data/deu_data.sav")
#Create a better guide
sjPlot::view_df(df_unclean, file = "/Users/nikolaosvichos/Library/Mobile Documents/com~apple~CloudDocs/Sciences Po/Thesis/Datasets/BRA/bra_data/bra_surveyguide.html")


##############################################################################################################
############################################## Data Management ###############################################
##############################################################################################################


df_semiclean <- df_unclean %>% 
  #SELECT VARIABLES
  dplyr::select(
    # demographics
    UF, REG, T_ZONA,              # state, region, urban/rural (urban = 1/rural = 2)
    T_D01A_1, T_D01A_2, T_D01A_3, # birthday, birthmonth, birthyear
    D01A_IDADE, D02, D03,         # age (in years), gender(male = 1, female = 2), education (rm 99)
    D06, D06a, D08,               # work situation (rm 98, 99), work time(rm 98, 99), work field (rm 997-999)
    D04, D05,                     # married/single/etc (rm 97 98), in_union (yes = 1)
    D09_RENDAF, D09a_FX_RENDAF,   # exact income (rm 999997 999998), income_brackets (rm 97 99)
    D11, D12a,                    # religiosity (# times/week, 1-6, 1 = very religious, rm 97 98), race (rm 97 98)
    # political interest (rm 97 98, 1 = HIGH interest)
    Q01,                         
    # LIKERT (1 = HIGHLY AGREE, rm 97 98) 
    Q03, Q04a, Q04b,              # understands most important issues, democracy preferable, judicial check
    Q04c, Q05a,                   # support strong leader (REVERSE), 
    Q05a, Q05b, Q05c,             # business leaders in power (REVERSE), independent experts in power (REVERSE), citizens/referendums in power (REVERSE but populism)
    # brazil how democratic (1-10, rm 97 98)   
    Q06,                         
    # LIKERT (1 = A LOT OF CONFIDENCE, rm 97 98)
    Q07a, Q07b, Q07c,             # congress, federal govt, judiciary
    Q07e, Q07f, Q07j,             # parties, media, big business
    Q07l, Q07m, Q07n,             # armed forces, police, federal police
    Q07o,                         # public ministry
    # voting information
    Q10P1a, Q10P2a,               # voted in first round, voted in second round (rm 97 98, voted = 1)
    Q10P1b, Q10P2b,               # who voted for in first round, who voted for in second round (rm 97 98, 50 = null vote, 60 = blank)
    # satisfaction with election (1 = VERY Satisfied, rm 97 98)
    Q11a, Q11b, Q11c,             # satisfaction with vote choice, satisfaction with blank/null vote, satisfaction with not voting
    Q12,                          # satisfaction with candidate variety
    # voting information for 2018
    Q14A, Q14A2,                  # voted in first round, voted in second round (rm 97 98, voted = 1)
    Q14B, Q14B2,                  # who voted for in first round, who voted for in second round (rm 97 98, 50 = null vote, 60 = blank)
    # power of vote (5 = influences a lot of what happens, rm 97 98)
    Q15,
    # party affection (0-10, rm 96 97 98)
    Q16_1, Q16_2, Q16_3,          # PDT, PL, PODEMOS
    Q16_4, Q16_5, Q16_6,          # PP, PT, PSB
    Q16_7, Q16_8, Q16_9,          # PSD, PSDB, PSOL
    Q16_10, Q16_11, Q16_12,       # REDE, REPUBLICANOS, UNIAO BRASIL
    Q16_13,                       # MDB
    # politician affection (0-10, rm 96 97 98)
    Q17_1, Q17_2, Q17_3,          # PDT, PL, PODEMOS
    Q17_4, Q17_5, Q17_6,          # PP, PSB, PSD
    Q17_7, Q17_8, Q17_9,          # PSDB, PSOL, REDE
    Q17_10, Q17_11, Q17_12,       # REPUBLICANOS, UNIAO BRASIL, MDB
    Q17_13,
    # left-right (0 = left, rm 95 96 98)
    Q19,
    # satisfaction with democracy (LIKERT, 1 = VERY satisfied, rm 97 98)
    Q22,
    # party preference (rm 97 98)
    Q23a, Q23b, Q23c,             # close to a party (1 = yes), close to a party (yes = 1), which party close to (rm 88 too)
    Q23d,                         # how close to party (1 = very close, 3 = not very close)
    # what is democracy, aggregated (Q29 is more aggregated, rm 97 98)
    Q28_codigos_agregados1,       # more aggregate 
    # Q28_codigos_desagregados,   # less aggregate
    # liberal democratic questions (rm 97 98, generally 1 = authoritarian)
    Q29,                          # democracy_without_parties (1 = anti-populist)
    Q30a, Q30b, Q30c            # military rule under a) crime, b) corruption, c) political instability
  ) %>%
  #RENAME VARIABLES
  rename(
    state = UF, 
    region = REG,
    is_urban = T_ZONA, #recode rural = 0 instead of 2
    birthday = T_D01A_1, 
    birthmonth = T_D01A_2, 
    birthyear = T_D01A_3, 
    age_in_years = D01A_IDADE,
    gender = D02,
    education = D03, 
    work_situation = D06, 
    work_time = D06a, 
    work_field = D08, 
    civil_situation = D04, 
    in_union = D05, 
    income_exact = D09_RENDAF, 
    income_brackets = D09a_FX_RENDAF, 
    religiosity = D11,
    race = D12a, 
    political_interest = Q01, 
    understands_political_issues = Q03, 
    democracy_preferable = Q04a, 
    judicial_checks = Q04b,
    oppose_strongmanrule = Q04c, 
    oppose_businessrule = Q05a, 
    oppose_expertrule = Q05b, 
    oppose_referendumrule = Q05c, 
    brazil_howdemocratic = Q06,
    trust_congress = Q07a, 
    trust_federalgovt = Q07b, 
    trust_judiciary = Q07c, 
    trust_parties = Q07e, 
    trust_media = Q07f, 
    trust_bigbusiness = Q07j, 
    trust_armedforces = Q07l, 
    trust_police = Q07m, 
    trust_federalpolice = Q07n, 
    trust_publicministry = Q07o, 
    voted2022_round1 = Q10P1a, 
    voted2022_round2 = Q10P2a, 
    candidate2022_round1 = Q10P1b, 
    candidate2022_round2 = Q10P2b, 
    voted2018_round1 = Q14A, 
    voted2018_round2 = Q14A2, 
    candidate2018_round1 = Q14B, 
    candidate2018_round2 = Q14B2, 
    satisfiedwith_votechoice = Q11a, 
    satisfiedwith_blankvote = Q11b, 
    satisfiedwith_notvoting = Q11c, 
    satisfiedwith_candidatevariety = Q12,
    voting_influences = Q15,
    thermometer_PDT = Q16_1, 
    thermometer_PL = Q16_2, 
    thermometer_PODEMOS = Q16_3, 
    thermometer_PP = Q16_4,
    thermometer_PT = Q16_5, 
    thermometer_PSB = Q16_6, 
    thermometer_PSD = Q16_7, 
    thermometer_PSDB = Q16_8, 
    thermometer_PSOL = Q16_9,
    thermometer_REDE = Q16_10, 
    thermometer_REPUBLICANOS = Q16_11, 
    thermometer_UNIAOBRASIL = Q16_12, 
    thermometer_MDB = Q16_13,
    thermometer_CIROGOMES = Q17_1,
    thermometer_BOLSONARO = Q17_2, 
    thermometer_ALVARODIAS = Q17_3, 
    thermometer_ARTHURLIRA = Q17_4,
    thermometer_LULA = Q17_5, 
    thermometer_GERALDOALCKMIN = Q17_6, 
    thermometer_GILBERTOKASSAB = Q17_7, 
    thermometer_EDUARTOLEITE = Q17_8, 
    thermometer_BOULOS = Q17_9,
    thermometer_MARINASILVA = Q17_10, 
    thermometer_TARCISIODEFREITAS = Q17_11, 
    thermometer_LUCIANOBIVAR = Q17_12, 
    thermometer_SIMONETIBET = Q17_13,
    left_right = Q19, 
    satisfiedwith_democracy = Q22, 
    is_close_party = Q23a,
    is_close_party_b = Q23b, 
    preferred_party = Q23c, 
    preferred_party_intensity = Q23d,
    whatis_democracy_simple = Q28_codigos_agregados1, 
    # whatis_democracy_detailed = Q28_codigos_desagregados, 
    democracy_without_parties = Q29, 
    militaryrule_againstcrime = Q30a, 
    militaryrule_againstcorruption = Q30b, 
    militaryrule_againstinstability = Q30c
    ) 


# commmon missing values to remove all at once
missing_global <- c(
  95,96,97,98,99,
  997,998,999,
  999997,999998,999999
)

# slightly less common missing values to remove all at once
missing_blanknull <- c(
  50, 60
)

# varlist that has the above missing values
varlist_candidateselected <- c(
  "candidate2022_round1", "candidate2022_round2",
  "candidate2018_round1", "candidate2018_round2"
)

# voted items to fix (recode 50s, 60s)
varlist_voted <- c(
  "voted2022_round1", "voted2022_round2", 
  "voted2018_round1", "voted2018_round2"
)

# binary variables to recode (1/2 --> 1/0)
varlist_binaries <- c(
  "is_urban", "gender", "in_union", 
  "voted2022_round1", "voted2022_round2", 
  "voted2018_round1", "voted2018_round2",
  "is_close_party", "is_close_party_b"
)

# binary variables to transpose (1/2 --> 0/1): same direction but 0-1
varlist_binaries_transpose <- c(
  "democracy_without_parties", "militaryrule_againstcrime", 
  "militaryrule_againstcorruption", "militaryrule_againstinstability"
)



#-------------------------------------#
#-- other variables to reverse code --#
#-------------------------------------#

# remember to put high values for 
# highly agree/highly interested/highly trust,
# or for democratic norms

# from 1 to 6 --> from 6 to 1
varlist_recode6 <- c(             
  "religiosity", "preferred_party_intensity"
  )                      

# from 1 to 5 --> from 5 to 1
varlist_recode5 <- c(            
  "understands_political_issues", "democracy_preferable", 
  "democracy_preferable", "judicial_checks", 
  "satisfiedwith_democracy"
                     )  
# from 1 to 4 --> from 4 to 1
varlist_recode4 <- c(
  "political_interest", "trust_congress", 
  "trust_federalgovt", "trust_judiciary", 
  "trust_parties", "trust_media", 
  "trust_bigbusiness", "trust_armedforces", 
  "trust_police", "trust_federalpolice", 
  "trust_publicministry", "satisfiedwith_votechoice", 
  "satisfiedwith_votechoice", "satisfiedwith_blankvote", 
  "satisfiedwith_notvoting", "satisfiedwith_candidatevariety"
)

# from 1 to 3 --> from 3 to 1
varlist_recode3 <- c("work_time")




## Create an unstandardized 
df_unstandardized <- df_semiclean %>% 
  # drop NAs
  mutate(
    # first, the standard NAs
    across(where(is.numeric), ~ na_recode(.x, missing_global)), 
    # then, less standard NAs
    across(all_of(varlist_candidateselected), ~ na_recode(.x, missing_blanknull)), 
    # then the final ones
    preferred_party = case_when(
      preferred_party == 88 ~ NA, 
      TRUE ~ preferred_party
    ), 
    # now simplify the voted/did not questions
    across(all_of(varlist_voted), ~voted_recode(.x)),
    ###############
    # NOW FIX ORDER OR BINARIES + OTHERS
    ###############
    #fix order of binaries
    across(all_of(varlist_binaries), ~order_recode(.x, 2)), #want this to remain binary, so keep like that for now
    #fix order of 3s
    across(all_of(varlist_recode3), ~order_recode(.x, 4)),
    #fix order of 4s
    across(all_of(varlist_recode4), ~order_recode(.x, 5)),
    #fix order of 5s
    across(all_of(varlist_recode5), ~order_recode(.x, 6)),
    #fix order of 6s
    across(all_of(varlist_recode6), ~order_recode(.x, 7)),
    #transpose some binaries so that higher values correspond to more democratic support
    across(all_of(varlist_binaries_transpose), ~binary_tranpose(.x))
  )
    

# sapply(df_unstandardized[varlist_binaries_transpose], function(x) binary_tranpose(x))
# sapply(df_unstandardized[varlist_binaries_transpose], function(x) table(x))
sjPlot::view_df(df_unstandardized, file = "/Users/nikolaosvichos/Library/Mobile Documents/com~apple~CloudDocs/Sciences Po/Thesis/Datasets/BRA/bra_data/bra_surveyguide_cleaned.html")


# Now standardize the relevant DVs for the index
index_items_prelim <- c("democracy_preferable", "judicial_checks", "oppose_strongmanrule", 
                 "oppose_businessrule", "oppose_expertrule", "oppose_referendumrule",
                 "trust_congress", "trust_federalgovt", "trust_judiciary",
                 "trust_parties", "trust_media", "trust_bigbusiness", 
                 "trust_armedforces", "trust_police", "trust_federalpolice", 
                 "trust_publicministry", "voting_influences", "satisfiedwith_democracy", 
                 "democracy_without_parties", "militaryrule_againstcrime", "militaryrule_againstcorruption", 
                 "militaryrule_againstinstability"
                 )

# create a copy
df <- df_unstandardized %>% 
  mutate(
    across(all_of(index_items_prelim), scale)
  )

election_date <- ymd("2018-10-07")


# Finaly create two age variables and two treatment variables
df <- df %>% 
  mutate(
    age_approx = 2018 - birthyear, 
    birthdate = make_datetime(
      year = birthyear,
      month = birthmonth,
      day = birthday), 
    age_exact = interval(start = birthdate, end = election_date) / years(1), 
    treatment16_exact = ifelse(age_exact > 16, TRUE, FALSE), 
    treatment18_exact = ifelse(age_exact > 18, TRUE, FALSE)
    )
  


###################################################################################################################
############################################### Constructing Indices################################################
###################################################################################################################


##### Constructing Media Index #####



# check internal consistency
psych::alpha(df[, index_items_prelim])

## Create sub-indices: democracy
democracy_items <- c("democracy_preferable", "judicial_checks", "oppose_strongmanrule", 
                   "oppose_businessrule", "oppose_expertrule", "militaryrule_againstcrime", 
                   "militaryrule_againstcorruption", "militaryrule_againstinstability"
)

psych::alpha(df[, democracy_items])

# factor construction
fa_democracy <- fa(
  df[, democracy_items],
  nfactors = 1, rotate = "none", fm = "pa", max.iter = 100
)

# eigevalues
fa_democracy$values

# scree plot
screeplot_democracy <- get_screeplot(fa_democracy, "Democratic Attitudes")

# get factor loadings
print(fa_democracy$loadings, cutoff = 0.3)



## Create sub-indices: trust
trust_items <- c("trust_congress", "trust_federalgovt", "trust_judiciary",
                 "trust_parties", "trust_media", "trust_bigbusiness", 
                 "trust_armedforces", "trust_police", "trust_federalpolice", 
                 "trust_publicministry")

psych::alpha(df[, trust_items])


# factor construction
fa_trust <- fa(
  df[, trust_items],
  nfactors = 1, rotate = "none", fm = "pa", max.iter = 100
)

# eigevalues
fa_trust$values

# scree plot
screeplot_trust <- get_screeplot(fa_democracy, "Institutional Trust")

# get factor loadings
print(fa_trust$loadings, cutoff = 0.3)








# add as an index
fa_media_df <- as.data.frame(fa_media$scores)
df$media_index <- scales::rescale(fa_media_df[, 1], to = c(0, 1)) # 1st factor → 0–1 scale


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


# Media Attitude models
rdd_media_full <- run_rdd_models( # entire sample
  data = df,
  index_var = "media_index",
  controls = c("gender", "education", "income"),
  sample_label = "Full Sample"
)

rdd_media_independents <- run_rdd_models( # independents
  data = df_independents,
  index_var = "media_index",
  controls = c("gender", "education", "income"),
  sample_label = "Independents"
)

rdd_media_partisans <- run_rdd_models( # partisans
  data = df_partisans,
  index_var = "media_index",
  controls = c("gender", "education", "income"),
  sample_label = "Partisans"
)

rdd_media_democrats <- run_rdd_models( # democrats
  data = df_democrats,
  index_var = "media_index",
  controls = c("gender", "education", "income"),
  sample_label = "Democrats"
)

rdd_media_republicans <- run_rdd_models( # republicans
  data = df_republicans,
  index_var = "media_index",
  controls = c("gender", "education", "income"),
  sample_label = "Republicans"
)


# Aggregate estimates in dataframes
rdd_media <- bind_rows(rdd_media_full, rdd_media_independents, rdd_media_partisans, rdd_media_democrats, rdd_media_republicans) %>%
  mutate(Outcome = "Media Attitudes") %>%
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
coefplot_media <- get_coefplot(rdd_media, 1)


#### Get discontinuity plots plots for the different subgroups and full group ####
# full sample
discontinuityplot <- get_discontinuityplot(df)



#### Display plots #### 
coefplot_media
discontinuityplot$plot_all
discontinuityplot$plot_subgroups





