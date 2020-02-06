# 1_import.r
# 20190319
#
# Create and save a list object comprising data extracted from raw tsv
# files.
#
DATA_RAW <- here("data","raw","abcd2.0.1")
THRESHOLD_PATH <- here('data','raw','poverty_thresholds')
years <- c(2016, 2017, 2018)

measures <- tibble(
  filename = c(
    "abcd_screen01",
    "abcd_cbcl01",
    "pps01",
    "dhx01",
    "fhxp102",
    "fhxp201",
    "abcd_cbcls01",
    "pdem02",
    "abcd_tbss01",
    "abcd_sscey01",
    "abcd_lt01",
    "acspsw03",
    "abcd_betnet02",
    "abcd_mri01",
    "abcd_pssrs01",
    "abcd_rhds01",
    "abcd_ptsd01"
  ),
  shortname = c(
    "screen",
    "cbcl_raw",
    "prodromal",
    "dev_hx",
    "family_hx1",
    "family_hx2",
    "cbcl_scored",
    "parent_demographics",
    "toolbox_scores",
    "culture_environment",
    "longitudinal_tracking",
    "post_stratification_weights",
    "connectivity",
    "mri_info",
    "short_srs",
    "res_hx",
    "trauma"
  )
)

message('Importing ABCD data.')

measures$path <- measures$filename %>% 
  map(function(x) str_c(DATA_RAW, "/", x, ".txt"))

# Get the first row (which contains variable names) from each tsv file.
column_names <- measures$path %>% map(read_tsv, n_max=0) 

# Extract data from each tsv file into separate dataframes,
# skipping the first 2 rows:
# - Row 1 - var names (already have)
# - Row 2 - var labels (redundant (in codebook) and confuse col type
#           guessing)
abcd <- measures$path %>% map(read_tsv, col_names = FALSE, skip = 2)

# So measures can be referred to by name rather than index position
names(abcd) <- measures$shortname

for (i in seq_along(abcd)){
  # Re-attach variable names for each dataframe
  colnames(abcd[[i]]) <- colnames(column_names[[i[[1]]]])

  # For dataframes where "gender" is named "sex," rename it so that 
  # they can be merged
  if("gender" %in% colnames(abcd[[i]])){
    abcd[[i]] <- abcd[[i]] %>% rename(sex = gender)
  }
}

# prepare follow-up srs data for merge
short_srs <- abcd$short_srs %>%
  rename(age_at_1yr_followup = interview_age) %>%
  rename(date_of_1yr_followup = interview_date) %>%
  select(-c(
    abcd_pssrs01_id,
    eventname,
    sex, 
    collection_id, 
    collection_title, 
    study_cohort_name))

abcd$short_srs <- NULL

prodromal <- abcd$prodromal %>%
  filter(visit=="1_year_follow_up_y_arm_1") %>%
  rename(age_at_1yr_followup = interview_age) %>%
  rename(date_of_1yr_followup = interview_date)

colnames(prodromal) <- paste(
  colnames(prodromal), "1_yr", sep = "_")

message('Importing poverty threshold data.')

# import poverty threshold data
thresholds <- dir(THRESHOLD_PATH, full.names = TRUE) %>%
    map2(
      c('threshold_2016', 'threshold_2017', 'threshold_2018'), 
      ~read_xls(
        .x, 
        range='A10:B24', 
        col_names=c('household_size', 'threshold')) %>% 
      mutate(!!.y := threshold))

message('Merging dataframes.')

abcd_frame <- abcd %>% 
  reduce(left_join, by = c(
    "sex",
    "subjectkey",
    "src_subject_id",
    "interview_age",
    "interview_date"))

# Merge in data from one-year follow-up
abcd_frame <- abcd_frame %>% 
  left_join(prodromal, by = c(
    "subjectkey" = "subjectkey_1_yr",
    "src_subject_id" = "src_subject_id_1_yr"))

abcd_frame <- abcd_frame %>% 
  left_join(short_srs, by = c("subjectkey", "src_subject_id"))

message('Import step complete.')
