# preprocess.r
# 20190319

message('Preprocessing: initial cleanup.')
# Remove columns that are unnecessary and/or artifacts of the merge process
abcd_frame <- abcd_frame %>% 
  select(
    -contains('eventname'), 
    -contains('collection_id'), 
    -contains('collection_title'), 
    -contains('study_cohort_name'), 
    -contains('dataset_id'), 
    -contains('visit'))

message('Preprocessing: transformations.')
# Recode family history of psychosis."
abcd_frame <- abcd_frame %>% 
  mutate(fh_psychosis = case_when(
    fam_history_8_yes_no == 1 ~ 1,
    fam_history_8_yes_no == 0 ~ 0,
    fam_history_8_yes_no == 999 ~ NA_real_)) 

# Recode sex; 3 is for the one intersex participant (who we'll later remove)
abcd_frame <- abcd_frame %>% 
  mutate(sex_index = recode(sex, M = 1, F = 0)) %>%
  mutate(sex = recode(demo_sex_v2, `1` = 1L, `2` = 0L, .default = 3L)) 

# Recode ethnicity; 3 is people without ethnicity information (to remove)
abcd_frame <- abcd_frame %>%  
  mutate(ethnicity_latinx = recode(
    demo_ethn_v2, `1` = 1L, `2` = 0L, .default = 3L)) 

# Derive a "number of races" indicator to figure out whether someone's 
# multiracial. Derive three mutually-exclusive categories: white, black, other.
abcd_frame <- abcd_frame %>% 
  rename(race_white = demo_race_a_p___10) %>%
  rename(race_black = demo_race_a_p___11) %>%
  rename(race_native_american = demo_race_a_p___12) %>%
  rename(race_native_alaskan = demo_race_a_p___13) %>%
  rename(race_native_hawaiian = demo_race_a_p___14) %>%
  rename(race_guamanian = demo_race_a_p___15) %>%
  rename(race_samoan = demo_race_a_p___16) %>%
  rename(race_other_pacific_islander = demo_race_a_p___17) %>%
  rename(race_asian_indian = demo_race_a_p___18) %>%
  rename(race_chinese = demo_race_a_p___19) %>%
  rename(race_filipinx = demo_race_a_p___20) %>%
  rename(race_japanese = demo_race_a_p___21) %>%
  rename(race_korean = demo_race_a_p___22) %>%
  rename(race_vietnamese = demo_race_a_p___23) %>%
  rename(race_other_asian = demo_race_a_p___24) %>%
  rename(race_other_race = demo_race_a_p___25) %>%
  rename(race_refused_to_answer = demo_race_a_p___77) %>%
  rename(race_unknown = demo_race_a_p___99) %>%
  mutate(n_races_raw = 
    race_white +
    race_black + 
    race_native_american +
    race_native_alaskan +
    race_native_hawaiian +
    race_guamanian +
    race_samoan +
    race_other_pacific_islander +
    race_asian_indian +
    race_chinese +
    race_filipinx +
    race_japanese +
    race_korean +
    race_vietnamese +
    race_other_asian + 
    race_other_race +
    race_unknown) %>%
  mutate(n_races = n_races_raw) %>%
# if the person has zero races - we'll assume they have 1
#  mutate_at("n_races", function(x) if_else(x == 0, 1, x)) %>%
  mutate(multiracial = 
    if_else(n_races > 1, 1, 0)) %>%
  mutate(race_white_only = if_else(
    race_white == 1 & multiracial == 0, 1, 0)) %>%
  mutate(race_black_only = if_else(
    race_black == 1 & multiracial == 0, 1, 0)) %>%
  mutate(race_asian_only = if_else(
    (race_asian_indian == 1 |
    race_chinese == 1 |
    race_filipinx == 1 |
    race_japanese == 1 |
    race_korean == 1 |
    race_vietnamese == 1 |
    race_other_asian == 1) & multiracial == 0, 1, 0)) %>%
  mutate(race_other = if_else(
    race_native_american == 1 |
    race_native_alaskan == 1 |
    race_native_hawaiian == 1 |
    race_guamanian == 1 |
    race_samoan == 1 |
    race_other_pacific_islander == 1 |
    race_other_race == 1 |
    race_unknown == 1 |
    multiracial == 1, 1, 0)) %>%
  mutate(race_other_expanded = if_else(
    race_asian_only == 1 |
    race_native_american == 1 |
    race_native_alaskan == 1 |
    race_native_hawaiian == 1 |
    race_guamanian == 1 |
    race_samoan == 1 |
    race_other_pacific_islander == 1 |
    race_other_race == 1 |
    race_unknown == 1 |
    multiracial == 1, 1, 0))

# Lay the groundwork for deriving income-to-needs later
abcd_frame <- abcd_frame %>% 
  rename(n_in_household = demo_roster_v2) %>%
  mutate(n_in_household_factor = n_in_household) %>%
  mutate(n_in_household_with_zero = n_in_household) %>%
  mutate(household_income = case_when(
    demo_comb_income_v2 == 1 ~ 4999,
    demo_comb_income_v2 == 2 ~ 11999,
    demo_comb_income_v2 == 3 ~ 15999,
    demo_comb_income_v2 == 4 ~ 24999,
    demo_comb_income_v2 == 5 ~ 34999,
    demo_comb_income_v2 == 6 ~ 49999,
    demo_comb_income_v2 == 7 ~ 74999,
    demo_comb_income_v2 == 8 ~ 99999,
    demo_comb_income_v2 == 9 ~ 199999,
    demo_comb_income_v2 == 10 ~ 200000,
    demo_comb_income_v2 == 999 ~ NA_real_,
    demo_comb_income_v2 == 777 ~ NA_real_)) %>%
  rename(household_income_category = demo_comb_income_v2)

# NIH toolbox measures
abcd_frame <- abcd_frame %>%
  rename(tb_cardsort = nihtbx_cardsort_agecorrected) %>%
  rename(tb_pattern = nihtbx_pattern_agecorrected) %>%
  rename(tb_list = nihtbx_list_agecorrected) %>%
  rename(tb_picture = nihtbx_picture_agecorrected) %>%
  rename(tb_flanker = nihtbx_flanker_agecorrected) %>% 
  rename(tb_fluid = nihtbx_fluidcomp_agecorrected)

# Country of origin
abcd_frame <- abcd_frame %>%
  rename(country_of_origin_child = demo_origin_v2) %>%
  rename(country_of_origin_parent = demo_prnt_origin_v2) %>%
  mutate(binary_origin_child = if_else(
    country_of_origin_child == 189, 1, 0)) %>%
  mutate(binary_origin_parent = if_else(
    country_of_origin_parent == 189, 1, 0)) 

# Low birth weight/preterm status
abcd_frame <- abcd_frame %>%
  mutate(birth_weight_oz_decimal = birth_weight_oz/16) %>%
  mutate(birth_weight_grams = 
    (birth_weight_lbs + birth_weight_oz_decimal)*0.45359237*1000) %>%
  mutate(lbw = if_else(birth_weight_grams < 2500, 1, 0)) %>%
  mutate(preterm = if_else(devhx_12a_p == 1, 1, 0))

# Other transformations
abcd_frame <- abcd_frame %>%
  rename(maternal_age_at_birth = devhx_3_p) %>%
  rename(paternal_age_at_birth = devhx_4_p) %>%
  mutate(age_years = interview_age/12) %>%
  mutate(trauma_score = 
    ksads_ptsd_raw_754_p +
    ksads_ptsd_raw_755_p +
    ksads_ptsd_raw_756_p +
    ksads_ptsd_raw_757_p +
    ksads_ptsd_raw_758_p +
    ksads_ptsd_raw_759_p +
    ksads_ptsd_raw_760_p +
    ksads_ptsd_raw_761_p +
    ksads_ptsd_raw_762_p +
    ksads_ptsd_raw_763_p +
    ksads_ptsd_raw_764_p +
    ksads_ptsd_raw_765_p +
    ksads_ptsd_raw_766_p +
    ksads_ptsd_raw_767_p +
    ksads_ptsd_raw_768_p +
    ksads_ptsd_raw_769_p +
    ksads_ptsd_raw_770_p) %>%
  # treating no information about delayed speech as "no delayed speech"
  mutate(delayed_speech = if_else(devhx_21_p >= 4, 1, 0))

# If n_in_household is zero, replace with the modal value of 4
#abcd_frame <- abcd_frame %>% 
#  mutate_at("n_in_household", function(x) if_else(x == 0, 4, x)) %>%
#  mutate_at("n_in_household_factor", function(x) if_else(x == 0, 4, x))

abcd_frame$interview_date <- mdy(abcd_frame$interview_date)

# Clean up and merge poverty threshold data
thresholds <- thresholds %>% 
  map(select, -threshold) %>%
  map(mutate, household_size = case_when(
    grepl('One', household_size) ~ 1,
    grepl('Two', household_size) ~ 2,
    grepl('Three', household_size) ~ 3,
    grepl('Four', household_size) ~ 4,
    grepl('Five', household_size) ~ 5,
    grepl('Six', household_size) ~ 6,
    grepl('Seven', household_size) ~ 7,
    grepl('Eight', household_size) ~ 8,
    grepl('Nine', household_size) ~ 9)) %>%
  map(drop_na) %>%
  reduce(inner_join, by='household_size')

message('Preprocessing: ASD.')

# ASD as defined by CBCL profile.
abcd_frame <- abcd_frame %>% 
  mutate(cbcl_asd = if_else(
    cbcl_scr_syn_withdep_t + 
    cbcl_scr_syn_thought_t + 
    cbcl_scr_syn_social_t > 195, 1, 0)) 
    
# ASD as defined by short SRS score from 1-year follow-up.
abcd_frame <- abcd_frame %>%
  mutate(short_srs_total = 
    ssrs_6_p +
    ssrs_15r_p + 
    ssrs_16_p + 
    ssrs_18_p + 
    ssrs_24_p +
    ssrs_29_p +
    ssrs_35_p + 
    ssrs_37_p + 
    ssrs_39_p + 
    ssrs_42_p +
    ssrs_58_p)

message('Preprocessing: psychosis.')

# This works but there's a lot of clumsy/inefficient/repetitious code here,
# particularly in the way 1 year follow-up scores are calculated.

construct_item <- function(number, type) {
# `type` parameter:
# 1 = normal item
# 2 = distress item
# 3 = normal item (1 year follow-up)
# 4 = distress item (1 year follow-up)
  if(type==1) { return(paste0('prodromal_', number, '_y')) } 
  else if(type==2) { return(paste0('prodromal_', number, 'b_y')) }
  else if(type==3) { return(paste0('prodromal_', number, '_y_1_yr')) }
  else if(type==4) { return(paste0('prodromal_', number, 'b_y_1_yr')) }
}

# Prodromal summary score: just the number of items endorsed
abcd_frame <- abcd_frame %>%
  mutate(prodromal_summary_score = rowSums(
    # Regex below will exclude variables ending in "r" so it doesn't pull in the
    # 1-year follow-up data
    select(., matches('prodromal_\\d{1,2}_y*[^r]$'))))

abcd_frame <- abcd_frame %>%
  mutate(prodromal_summary_score_1_yr = rowSums(
    select(., matches('prodromal_\\d{1,2}_y_1_yr'))))

# Prodromal distress score: severity-weighted version of summary score.

# Since prodromal distress items that were 0 are coded as NA, we need to fix 
# this, but in a way that doesn't affect other NAs.
distress_list <-
  1:20 %>% 
  map(construct_item, 2) %>%
  paste0(' = 0, ') %>%
  paste0(collapse='') %>%
  paste0(construct_item(21, 2), ' = 0') %>% 
  paste0('list(', ., ')')

distress_list_1_yr <-
  1:20 %>% 
  map(construct_item, 4) %>%
  paste0(' = 0, ') %>%
  paste0(collapse='') %>%
  paste0(construct_item(21, 4), ' = 0') %>% 
  paste0('list(', ., ')')

abcd_frame <- abcd_frame %>% replace_na(eval(parse(text=distress_list)))
abcd_frame <- abcd_frame %>% replace_na(eval(parse(text=distress_list_1_yr)))

construct_distress_score <- function(x) {
  var1 <- construct_item(x, 1)
  var2 <- construct_item(x, 2)
  return(paste0(var1, '*', var2))
}

construct_distress_score_1_yr <- function(x) {
  var1 <- construct_item(x, 3)
  var2 <- construct_item(x, 4)
  return(paste0(var1, '*', var2))
}

distress_scores <- 
  1:20 %>%
  map(construct_distress_score) %>%
  paste0(' + ') %>%
  paste0(collapse='') %>%
  paste0(construct_distress_score(21))

distress_scores_1_yr <- 
  1:20 %>%
  map(construct_distress_score_1_yr) %>%
  paste0(' + ') %>%
  paste0(collapse='') %>%
  paste0(construct_distress_score_1_yr(21))

abcd_frame <- abcd_frame %>% 
  mutate(prodromal_distress_score = eval(parse(text=distress_scores)))

abcd_frame <- abcd_frame %>% 
  mutate(prodromal_distress_score_1_yr = eval(parse(text=distress_scores_1_yr)))

abcd_frame <- abcd_frame %>% 
# These binary variables use a 2 SD cutoff
  mutate(binary_psychosis = if_else(
    rescale(prodromal_summary_score) >= 1, 1, 0)) %>%
  mutate(binary_distress_psychosis = if_else(
    rescale(prodromal_distress_score)>=1,1,0)) %>%
# Progression from low to high symptom severity
  mutate(progression_12_months = if_else(
    rescale(prodromal_distress_score_1_yr) >= 1 &
    rescale(prodromal_distress_score) < 1, 1, 0))

message('Deriving population density.')

# Get density for location of most recent residence
abcd_frame <- abcd_frame %>% mutate(pop_density = 
  ifelse(!is.na(reshist_addr6_popdensity), reshist_addr6_popdensity,
    ifelse(!is.na(reshist_addr5_popdensity), reshist_addr5_popdensity,
      ifelse(!is.na(reshist_addr4_popdensity), reshist_addr4_popdensity,
        ifelse(!is.na(reshist_addr3_popdensity), reshist_addr3_popdensity,
          ifelse(!is.na(reshist_addr2_popdensity), reshist_addr2_popdensity,
            ifelse(!is.na(reshist_addr1_popdensity), reshist_addr1_popdensity,
             NA)))))))

# Factorize factors
factors <- c(
  'site_id_l', 
  'rel_family_id', 
  'scrn_asd',
  'cbcl_asd',
  'fh_psychosis',
  'sex',
  'binary_psychosis',
  'binary_distress_psychosis',
  'delayed_speech',
  'country_of_origin_child',
  'country_of_origin_parent',
  'binary_origin_child',
  'binary_origin_parent',
  'household_income_category',
  'n_in_household_factor',
  'race_white_only',
  'race_black_only',
  'race_other_expanded',
  'ethnicity_latinx')

for (factor in factors) {
    abcd_frame[[factor]] <- as.factor(abcd_frame[[factor]])
}

message('Deriving income-to-needs.')
derive_income_to_needs <- function(size, income, year) {
  # table doesn't have values for household size greater than 9,
  # so in this case treat the household size as if it's 9
  if (!is.na(size) && size > 9) { size <- 9 }
  row <- thresholds %>% filter(household_size==size)
  column <- paste0('threshold_', year)
  threshold = as.integer(row[column])
  return(income/threshold)
}

abcd_frame$income_to_needs <- NA

for (row in 1:nrow(abcd_frame)) {
  abcd_frame[row, 'income_to_needs'] <- 
    derive_income_to_needs(
      as.integer(abcd_frame[row, 'n_in_household']),
      as.integer(abcd_frame[row, 'household_income']),
      year(as.data.frame(abcd_frame)[row, 'interview_date']))
}

message('Identifying complete cases.')

# create a group for observations with complete data
abcd_frame <- abcd_frame %>% 
  mutate(complete_data = ifelse(
    !is.na(scrn_asd) &
    sex != 3 & # excluding the single intersex participant
    ethnicity_latinx !=3 &
    !is.na(prodromal_summary_score) &
    !is.na(n_in_household) &
    n_in_household > 0 &
    !is.na(household_income) &
    !is.na(fh_psychosis) &
    !is.na(maternal_age_at_birth) &
    !is.na(paternal_age_at_birth) &
    paternal_age_at_birth != 332 & # implausible ages
    paternal_age_at_birth != 389 & 
    devhx_21_p != 999 &
    !is.na(devhx_21_p) &
    devhx_12a_p != 999 &
    !is.na(devhx_12a_p) &
    !is.na(tb_cardsort) &
    !is.na(tb_pattern) &
    !is.na(tb_list) &
#    !is.na(binary_origin_child) &
#    !is.na(pop_density) &
    !is.na(trauma_score), 1, 0))

message('Subsetting data.')

abcd_frame_full <- abcd_frame

# Only complete observations
abcd_frame <- abcd_frame_full %>% filter(complete_data==1)

abcd_frame <- abcd_frame %>% 
    mutate(
        asd_pls_group = case_when(
            scrn_asd == 1 & binary_distress_psychosis == 1 ~ "ASD+/PLS+",
            scrn_asd == 1 & binary_distress_psychosis == 0 ~ "ASD+/PLS-",
            scrn_asd == 0 & binary_distress_psychosis == 1 ~ "ASD-/PLS+"))

abcd_continuous <- abcd_frame %>%
    select(
        scrn_asd, #not continuous but this is the grouping factor
        age_years,
        prodromal_summary_score,
        prodromal_distress_score,
        income_to_needs,
        trauma_score,
        maternal_age_at_birth,
        paternal_age_at_birth,
        tb_cardsort,
        tb_pattern,
        tb_list,
        trauma_score)

abcd_categorical <- abcd_frame %>%
    select(
        scrn_asd,
        sex,
        race_white_only,
        race_black_only,
        race_other_expanded,
        ethnicity_latinx,
        fh_psychosis,
        delayed_speech)

message('Preprocess step complete.')
