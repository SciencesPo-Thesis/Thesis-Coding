UF, REG, T_ZONA,              # state, region, urban/rural (urban = 1/rural = 2)
T_D01A_1, T_D01A_2, T_D01A_3, # birthday, birthmonth, birthyear
D01A_IDADE, D02, D03,         # age (in years), gender(male = 1, female = 2), education (rm 99)
D06, D06a, D08,               # work situation (rm 98, 99), work time(rm 98, 99), work field (rm 997-999)
D04, D05,                     # married/single/etc (rm 97 98), in_union (yes = 1)
D09_RENDAF, D09a_FX_RENDAF,   # exact income (rm 999997 999998), income_brackets (rm 97 99)
D11, D12a,                    # religiosity (# times/week, 1-6, 1 = very religious, rm 97 98), race (rm 97 98)
# political interest (rm 97 98, 1 = HIGH interest, 4 = low interest)
Q01,                         
# LIKERT (1-5, 1 = HIGHLY AGREE, rm 97 98)  #REVERSE
Q03, Q04a, Q04b,              # understands most important problems, democracy preferable, judicial check
Q04c, Q04d, Q05a,             # support strong leader (REVERSE), feminism gone too far (REVERSE), business leaders in power (REVERSE)
Q05b, Q05c,                   # independent experts in power (REVERSE), citizens/referendums in power (REVERSE but populism)
# brazil how democratic (1-10, rm 97 98)   
Q06,                         
# LIKERT (1 = A LOT OF CONFIDENCE, 4 = NO CONFIDENCE, rm 97 98) #REVERSE
Q07a, Q07b, Q07c,             # congress, federal govt, judiciary
Q07e, Q07f, Q07j,             # parties, media, big business
Q07l, Q07m, Q07n,             # armed forces, police, federal police
Q07o,                         # public ministry
# voting information
Q10P1a, Q10P2a,               # voted in first round, voted in second round (rm 97 98, voted = 1)
Q10P1b, Q10P2b,               # who voted for in first round, who voted for in second round (rm 97 98, 50 = null vote, 60 = blank)
# satisfaction with election (1 = VERY Satisfied, 4 = NOT Satisfied rm 97 98)
Q11a, Q11b, Q11c,             # satisfaction with vote choice, satisfaction with blank/null vote, satisfaction with not voting
Q12,                          # satisfaction with candidate variety
# voting information for 2018
Q14A, Q14A2,                  # voted in first round, voted in second round (rm 97 98, voted = 1)
Q14B, Q14B2,                  # who voted for in first round, who voted for in second round (rm 97 98, 50 = null vote, 60 = blank)
# power of vote (5 = influences a lot of what happens, rm 95 97 98)
Q15,
# party affection (0-10, rm 96 97 98)
Q16_1, Q16_2, Q16_3,          # PDT, PL, PODEMOS
Q16_4, Q16_5, Q16_6,          # PP, PSB, PSD
Q16_7, Q16_8, Q16_9,          # PSDB, PSOL, REDE
Q16_10, Q16_11, Q16_12,       # REPUBLICANOS, UNIAO BRASIL, MDB
Q16_13,                       # CIRO GOMES
# politician affection
Q17_1, Q17_2, Q17_3,          # CIRO GOMES, BOLSONARO, ALVARO DIAS
Q17_4, Q17_5, Q17_6,          # ARTHUR LIRA, LULA, GERALDO ALCKMIN
Q17_7, Q17_8, Q17_9,          # GLIBERTO KASSAB, EDUARTO LEITE, BOULOS
Q17_10, Q17_11, Q17_12,       # MARINA SILVA, TARCISIO DE FREITAS, LUCIANO BIVAR
Q17_13,                       # SIMONE TEBET
# left-right (0 = left, rm 9 96 98)
Q19,
# satisfaction with democracy (LIKERT, 1 = VERY satisfied, 5 = not satisfied, rm 97 98)
Q22,
# party preference (rm 88 97 98)
Q23a, Q23b, Q23c,             # close to a party (1 = yes), close to a party (yes = 1), which party close to
Q23d,                         # how close to party (1 = very close, 3 = not very close)
# what is democracy, aggregated (Q29 is more aggregated, rm 97 98)
Q28_codigos_agregados1,       # more aggregate 
Q28_codigos_desagregados,     # less aggregate
# liberal democratic questions (rm 97 98, generally 1 = authoritarian)
Q29,                          # democracy_without_parties (1 = anti-populist)
Q30a, Q30b, Q30c            # military rule under a) crime, b) corruption, c) political instability


## FLAGS
## DIFFERENTT NAs = preferred_party (88), candidate2022_round1, candidate2022_round2, candidate2018_round1, candidate2018_round2 (50,  60)

## Reverse binary: ubrna/rural, gender, in_union, 

## Reverse other: religiosity, political interest, 
#support strong leader, business leader in power, independent experts in power, referendums in pwoer
# LIKERT_democracy_questions, trust questions
# party prefernece questions


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
# LIKERT (1 = HIGHLY AGREE, 5 = Disagree rm 97 98)  FUX
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
)

