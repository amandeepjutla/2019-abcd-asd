# ---
# jupyter:
#   jupytext:
#     formats: ipynb,R:percent
#     text_representation:
#       extension: .R
#       format_name: percent
#       format_version: '1.2'
#       jupytext_version: 1.2.4
#   kernelspec:
#     display_name: 'R: ABCD'
#     language: R
#     name: ir-abcd
# ---

# %% [markdown]
# # model.r - generates tables.
#

# %% [markdown]
# ## Setup

# %%


# %% [markdown]
# ## 1: Characteristics of study participants across ASD and non-ASD groups

# %%
# Characteristics of participants in full sample
table_1_all <- abcd_frame %>%
    select(
        age_years,
        scrn_asd,
        prodromal_summary_score,
        prodromal_distress_score,
        sex,
        race_white_only,
        race_black_only,
        race_other_expanded,
        ethnicity_latinx,
        income_to_needs,
        fh_psychosis,
        trauma_score,
        delayed_speech,
        maternal_age_at_birth,
        paternal_age_at_birth,
        tb_cardsort,
        tb_picture,
        tb_flanker,
        tb_pattern,
        tb_list) %>% 
    skim()  %>%
    filter(stat == "top_counts" | stat == "mean" | stat == "sd") %>%
    select(-c(type, level, formatted)) %>% 
    mutate(value = round(value,2)) %>%
    write_excel_csv(here("r","output","table_1","table_1_full_sample.csv"))

# %%
# Characteristics of participants grouped by ASD vs non-ASD
table_1_grouped <- abcd_frame %>%
    select(
        age_years,
        scrn_asd,
        prodromal_summary_score,
        prodromal_distress_score,
        sex,
        race_white_only,
        race_black_only,
        race_other_expanded,
        ethnicity_latinx,
        income_to_needs,
        fh_psychosis,
        trauma_score,
        delayed_speech,
        maternal_age_at_birth,
        paternal_age_at_birth,
        tb_cardsort,
        tb_picture,
        tb_flanker,
        tb_pattern,
        tb_list
    ) %>% 
    group_by(scrn_asd) %>% 
    skim() %>% 
    filter(stat == "top_counts" | stat == "mean" | stat == "sd") %>%
    select(-c(type, level, formatted)) %>% 
    mutate(value = round(value,2)) %>% 
    write_excel_csv(here("r","output","table_1","table_1_grouped_sample.csv"))

# %%
# Between-group comparisons of continuous variables 
table_1_t_tests <- tibble()

for (variable_name in colnames(select(abcd_continuous, -c(scrn_asd)))) {
    variable_t <- 
        t.test(abcd_continuous[[variable_name]] ~ abcd_continuous[['scrn_asd']]) %>% 
        tidy() %>%
        mutate(variable_name = variable_name) 
    table_1_t_tests <- table_1_t_tests %>% bind_rows(variable_t)
}

adjusted_p_from_t <- p.adjust(table_1_t_tests[['p.value']], method = "BH") 

table_1_t_tests <- table_1_t_tests %>% 
    cbind(adjusted_p_from_t) %>%
    write_excel_csv(here("r","output","table_1","table_1_continuous_t_tests.csv"))

# %%
# Between-group comparisons of categorical variables
table_1_chisq_tests <- tibble()

for (variable_name in colnames(select(abcd_categorical, -c(scrn_asd)))) {
    variable_chisq <- 
        chisq.test(
            abcd_categorical[[variable_name]], 
            abcd_categorical[['scrn_asd']],
            correct = FALSE) %>%
        tidy() %>%
        mutate(variable_name = variable_name)
    table_1_chisq_tests <- table_1_chisq_tests %>% bind_rows(variable_chisq)
}

adjusted_p_from_chisq <- p.adjust(table_1_chisq_tests[['p.value']], method = "BH")

table_1_chisq_tests <- table_1_chisq_tests %>% 
    cbind(adjusted_p_from_chisq) %>%
    write_excel_csv(here("r","output","table_1","table_1_categorical_chisq_tests.csv"))

# %% [markdown]
# ## 2: Predictors of continuous PQ-BC distress score

# %%
predictors_of_continuous_prodromal_distress_score <- lmerTest::lmer(
    prodromal_distress_score ~ 
        scrn_asd + 
        sex +
        rescale(interview_age) + 
        race_white_only +
        race_black_only +
        ethnicity_latinx + 
        fh_psychosis +
        rescale(trauma_score) +
        rescale(income_to_needs) + 
        rescale(tb_cardsort) + 
        rescale(tb_list) + 
        rescale(tb_pattern) + 
        (1 | site_id_l / rel_family_id),
    control=lmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5)),
    data = abcd_frame)

# %%
predictors_of_continuous_prodromal_distress_score %>% 
    tidy(conf.int=TRUE, conf.method = "Wald") %>% 
    arrange(desc(estimate)) %>%
    filter(effect=='fixed') %>%
    write_excel_csv(here("r","output","table_2","table_2_model_parameters.csv")) %>%
    mutate_at(vars(-c(p.value, group, effect, term)), round, 2) %>%
    write_excel_csv(here("r","output","table_2","table_2_model_parameters_rounded.csv"))

# %% [markdown]
# ## 3: Predictors of PQ-BC distress score above 2 SD cutoff

# %%
predictors_of_prodromal_distress_above_cutoff <- glmer(
    binary_distress_psychosis ~ 
        scrn_asd + 
        sex +
        race_white_only +
        race_black_only +
        ethnicity_latinx + 
        rescale(trauma_score) +
        rescale(interview_age) + 
        rescale(income_to_needs) + 
        rescale(tb_cardsort) + 
        rescale(tb_list) + 
        rescale(tb_pattern) + 
        fh_psychosis +
        (1 | site_id_l / rel_family_id),
    control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5)),
    family=binomial(link='logit'),
    data = abcd_frame)

# %%
summary(predictors_of_prodromal_distress_above_cutoff)

# %%
predictors_of_prodromal_distress_above_cutoff %>% 
    tidy(
        exponentiate = TRUE, 
        conf.int = TRUE,
        conf.method = "Wald") %>%
    filter(effect=='fixed') %>%
    arrange(desc(estimate)) %>%
    write_excel_csv(here("r","output","table_3","table_3_model_parameters.csv")) %>%
    mutate_at(vars(-c(p.value, group, effect, term)), round, 2) %>%
    write_excel_csv(here("r","output","table_3","table_3_model_parameters_rounded.csv"))

# %% [markdown]
# ## S1: Missingness by variable

# %%
n_all_participants <- abcd_frame_full %>% nrow()
n_all_participants

# %%
n_retained_participants <- abcd_frame %>% nrow()
n_retained_participants

# %%
n_excluded_participants <- n_all_participants - n_retained_participants
n_excluded_participants

# %%
percentage_retained <- n_retained_participants/n_all_participants*100
round(percentage_retained,2)

# %%
# Characteristics of excluded vs included participants
table_s1 <- abcd_frame_full %>%
    select(
        age_years,
        scrn_asd,
        prodromal_summary_score,
        prodromal_distress_score,
        sex,
        race_white_only,
        race_black_only,
        race_other_expanded,
        ethnicity_latinx,
        n_in_household,
        household_income,
        income_to_needs,
        fh_psychosis,
        trauma_score,
        delayed_speech,
        maternal_age_at_birth,
        paternal_age_at_birth,
        tb_cardsort,
        tb_picture,
        tb_flanker,
        tb_pattern,
        tb_list) %>% 
#    group_by(complete_data) %>% 
    skim() %>%
    filter(stat == "complete" | stat == "missing") %>%
    select(-c(type, level, formatted)) %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    mutate(percent_complete = round((complete / (missing + complete) * 100), 2)) %>%
    rename(n_missing = missing, n_complete = complete) %>%
    arrange(desc(percent_complete))

# %%
table_s1 %>% write_excel_csv(here("r","output","table_s1","table_s1.csv"))

# %% [markdown]
# ## S2: Item-level PQ-BC distress score comparisons

# %%
# Characteristics of participants grouped by ASD vs non-ASD
table_s2_frame <- abcd_frame %>%
    select(
        scrn_asd,
        prodromal_1b_y,
        prodromal_2b_y,
        prodromal_3b_y,
        prodromal_4b_y,
        prodromal_5b_y,
        prodromal_6b_y,
        prodromal_7b_y,
        prodromal_8b_y,
        prodromal_9b_y,
        prodromal_10b_y,
        prodromal_11b_y,
        prodromal_12b_y,
        prodromal_13b_y,
        prodromal_14b_y,
        prodromal_15b_y,
        prodromal_16b_y,
        prodromal_17b_y,
        prodromal_18b_y,
        prodromal_19b_y,
        prodromal_20b_y,
        prodromal_21b_y)

# get item-level summary stats before running between-group comparisons

table_s2_frame %>%
    group_by(scrn_asd) %>%
    skim() %>%
    filter(stat == "mean" | stat == "sd") %>%
    select(-c(type, level, formatted)) %>%
    mutate(value = round(value,2)) %>%
    write_excel_csv(here("r","output","table_s2","table_s2_summary_stats.csv"))

table_s2_anovas <- tibble()

for (variable_name in colnames(select(table_s2_frame, -c(scrn_asd)))) {
    variable_anova <- 
        aov(table_s2_frame[[variable_name]] ~ table_s2_frame[['scrn_asd']]) %>% 
        tidy() %>%
        mutate(variable_name = variable_name)
    table_s2_anovas <- table_s2_anovas %>% bind_rows(variable_anova)
}


adjusted_p_from_anova <- p.adjust(table_s2_anovas[['p.value']], method = "BH")

table_s2_anovas <- table_s2_anovas %>% 
    cbind(adjusted_p_from_anova) %>%
    write_excel_csv(here("r", "output", "table_s2", "table_s2_anovas.csv"))

# %% [markdown]
# ## S3: Predictors of parent-reported ASD

# %%
# Validity check: are the predictors of ASD what one would expect?
# MLM does not converge - so using GEE here.

predictors_of_asd <- geeglm(
    unfactor(scrn_asd) ~ 
        unfactor(sex) +
        race_white_only +
        race_black_only +
        droplevels(ethnicity_latinx) + 
        delayed_speech + 
        rescale(interview_age) +
        rescale(income_to_needs) + 
        rescale(maternal_age_at_birth) +
        rescale(paternal_age_at_birth),
    family=binomial(link='logit'),
    id = rel_family_id,
    data = abcd_frame)

# %%
summary(predictors_of_asd)

# %%
predictors_of_asd %>% 
    tidy(
        exponentiate = TRUE, 
        conf.int = TRUE,
        conf.method = "Wald") %>%
    mutate(statistic = sqrt(statistic)) %>% # wald to Z
    arrange(desc(estimate)) %>%
    write_excel_csv(here("r","output","table_s3","table_s3_model_parameters.csv")) %>%
    mutate_at(vars(-c(p.value, term)), round, 2) %>%
    write_excel_csv(here("r","output","table_s3","table_s3_model_parameters_rounded.csv"))

# %%
predict_asd_frame <- predict_asd %>% 
    tidy(exponentiate=TRUE, conf.int=TRUE) %>%
    mutate(statistic = sqrt(statistic)) # wald to z

# %%
# Total scores
range(abcd_frame$prodromal_summary_score)
round(mean(abcd_frame$prodromal_summary_score),2) 
round(sd(abcd_frame$prodromal_summary_score),2)

# %%
# Distress scores
range(abcd_frame$prodromal_distress_score)
round(mean(abcd_frame$prodromal_distress_score),2) 
round(sd(abcd_frame$prodromal_distress_score),2)

# %%
# Number of participants who meet the 2 SD / top 5% cutoff
abcd_frame %>% filter(binary_distress_psychosis==1) %>% nrow()

# %%
# Number of ASD participants who meet the cutoff
abcd_frame %>% filter(binary_distress_psychosis==1 & scrn_asd==1) %>% nrow()

# %%
# Extent to which summary and distress scores correlate
round(cor(abcd_frame$prodromal_summary_score, abcd_frame$prodromal_distress_score),2)
