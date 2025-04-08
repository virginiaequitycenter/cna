# Community Needs Assessment
# Survey Data from Network2Work, United Way, MACAA

# Libraries ----
library(here)
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
require(openxlsx)

# .........................................
# Read in Survey Data ----
# Network2Work
n2w_fy_23_24 <- read_csv("Network2Work/data/survey_23_24_pdc.csv")

# United Way
uw_fy_24 <- read_csv("UnitedWay/data/CE_FY24_CNA.csv") %>% 
  clean_names()
# Filter for planning district only: Albemarle, Charlottesville, Fluvanna, Greene,  Louisa, Nelson
uw_fy_24 <- uw_fy_24 %>% 
  filter(locality %in% c("Albemarle", "Charlottesville", "Fluvanna", "Greene", "Louisa", "Nelson"))

# Compare survey data
# n2w_cols <- colnames(n2w_fy_23_24)
# n2w_qs <- data.frame(survey = "N2W", questions = n2w_cols)
# uw_cols <- colnames(uw_fy_24)
# uw_qs <- data.frame(survey = "UWGC", questions = uw_cols)
# 
# survey_compare <- rbind(n2w_qs, uw_qs)
# write_csv(survey_compare, "survey_compare_blank.csv")

# .........................................
# Make Tables ----
# Locality ----
locality_n2w <- n2w_fy_23_24 %>%
  count(dem_locality) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Locality",
         survey = "Network2Work",
         dem_locality = case_when(dem_locality == "Charlottesville (city)" ~ "Charlottesville",
                                 .default = dem_locality)) %>% 
  rename(data_point = dem_locality,
         survey_count = n)

locality_uw <- uw_fy_24 %>%
  count(locality) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Locality",
         survey = "United Way")%>% 
  rename(data_point = locality,
         survey_count = n)

locality <- rbind(locality_n2w, locality_uw) %>%
  select(survey, survey_question, data_point, survey_count, survey_total, survey_percent)

local_all <- locality %>% 
  group_by(survey_question, data_point) %>% 
  summarise(survey_count = sum(survey_count)) %>%
  arrange(desc(survey_count)) %>%
  mutate(survey_total = sum(survey_count),
         survey_percent = round((survey_count/survey_total) * 100, 2),
         survey = "All") %>% 
  ungroup()

locality <- rbind(locality, local_all)

# Age ----
age_n2w <- n2w_fy_23_24 %>%
  mutate(dem_age = case_when(dem_age == "18" ~ "18 and under",
                             .default = dem_age)) %>% 
  count(dem_age) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Age",
         survey = "Network2Work") %>% 
  rename(data_point = dem_age,
         survey_count = n)

age_uw <- uw_fy_24 %>%
  mutate(parent_age = case_when(parent_age == "under 19" ~ "18 and under",
                                .default = parent_age)) %>% 
  count(parent_age) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Age",
         survey = "United Way",
         parent_age = str_replace(parent_age, " - ", "-")) %>% 
  rename(data_point = parent_age,
         survey_count = n)

age <- rbind(age_n2w, age_uw) %>%
  select(survey, survey_question, data_point, survey_count, survey_total, survey_percent)

age_all <- age %>% 
  mutate(data_point = case_when(data_point %in% c("25-39", "40-54", "25-54") ~ "25-54",
                                 .default = data_point)) %>% 
  group_by(survey_question, data_point) %>% 
  summarise(survey_count = sum(survey_count)) %>%
  arrange(desc(survey_count)) %>%
  mutate(survey_total = sum(survey_count),
         survey_percent = round((survey_count/survey_total) * 100, 2),
         survey = "All") %>% 
  ungroup()

age <- rbind(age, age_all)

# Race ----
race_n2w <- n2w_fy_23_24 %>%
  count(dem_raceethnicity) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Race",
         survey = "Network2Work",
         dem_raceethnicity = case_when(dem_raceethnicity == "Black or African American" ~ "Black",
                                       dem_raceethnicity == "American Indian/Alaskan native" ~ "American Indian/Alaska Native", 
                                       dem_raceethnicity == "Native Hawaiian or Other Pacific Islander" ~ "Hawaiian/Pacific Islander",
                                       .default = dem_raceethnicity)) %>% 
  rename(data_point = dem_raceethnicity,
         survey_count = n)

# Rename multiple selections as "Multiracial"
race_uw <- uw_fy_24 %>%
  mutate(group_race_check_all_that_apply = case_when(str_detect(group_race_check_all_that_apply, ",") ~ "Multiracial",
                                                     .default = group_race_check_all_that_apply)) %>% 
  count(group_race_check_all_that_apply) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Race",
         survey = "United Way") %>% 
  rename(data_point = group_race_check_all_that_apply,
         survey_count = n)

race <- rbind(race_n2w, race_uw) %>%
  select(survey, survey_question, data_point, survey_count, survey_total, survey_percent)

race_all <- race %>% 
  group_by(survey_question, data_point) %>% 
  summarise(survey_count = sum(survey_count)) %>%
  arrange(desc(survey_count)) %>%
  mutate(survey_total = sum(survey_count),
         survey_percent = round((survey_count/survey_total) * 100, 2),
         survey = "All") %>% 
  ungroup()

race <- rbind(race, race_all)

# Ethnicity ----
ethn_n2w <- n2w_fy_23_24 %>%
  count(dem_hispanic) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Ethnicity",
         survey = "Network2Work",
         dem_hispanic = case_when(dem_hispanic == "Yes" ~ "Hispanic",
                                  dem_hispanic == "No" ~ "Not Hispanic")) %>% 
  rename(data_point = dem_hispanic,
         survey_count = n)

ethn_uw <- uw_fy_24 %>%
  count(group_hispanic_latino) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Ethnicity",
         survey = "United Way",
         group_hispanic_latino = case_when(group_hispanic_latino == TRUE ~ "Hispanic",
                                           group_hispanic_latino == FALSE ~ "Not Hispanic")) %>% 
  rename(data_point = group_hispanic_latino,
         survey_count = n)

ethnicity <- rbind(ethn_n2w, ethn_uw) %>%
  select(survey, survey_question, data_point, survey_count, survey_total, survey_percent)

ethn_all <- ethnicity %>% 
  group_by(survey_question, data_point) %>% 
  summarise(survey_count = sum(survey_count)) %>%
  arrange(desc(survey_count)) %>%
  mutate(survey_total = sum(survey_count),
         survey_percent = round((survey_count/survey_total) * 100, 2),
         survey = "All") %>% 
  ungroup()

ethnicity <- rbind(ethnicity, ethn_all)

# Gender ----
gender_n2w <- n2w_fy_23_24 %>%
  count(dem_genderidentity) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Gender",
         survey = "Network2Work",
         dem_genderidentity = case_when(dem_genderidentity == "Prefer not to answer" ~ "Prefer not to disclose",
                                        .default = dem_genderidentity)) %>% 
  rename(data_point = dem_genderidentity,
         survey_count = n)

gender_uw <- uw_fy_24 %>%
  count(group_gender) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Gender",
         survey = "United Way") %>% 
  rename(data_point = group_gender,
         survey_count = n)

gender <- rbind(gender_n2w, gender_uw) %>%
  select(survey, survey_question, data_point, survey_count, survey_total, survey_percent)

gender_all <- gender %>% 
  group_by(survey_question, data_point) %>% 
  summarise(survey_count = sum(survey_count)) %>%
  arrange(desc(survey_count)) %>%
  mutate(survey_total = sum(survey_count),
         survey_percent = round((survey_count/survey_total) * 100, 2),
         survey = "All") %>% 
  ungroup()

gender <- rbind(gender, gender_all)

# Education Level ----
educ_n2w <- n2w_fy_23_24 %>%
  filter(educ_highestgrade != "Unsure--I was educated outside the U.S." | is.na(educ_highestgrade)) %>%
  count(educ_highestgrade) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Education Level",
         survey = "Network2Work",
         educ_highestgrade = case_when(educ_highestgrade == "No high school diploma or GED" ~ "Did Not Complete High School",
                                       educ_highestgrade == "High School Diploma" ~ "High School Graduate",
                                       educ_highestgrade == "Advanced Degree" ~ "Graduate Degree",
                           .default = educ_highestgrade)) %>% 
  rename(data_point = educ_highestgrade,
         survey_count = n)

educ_uw <- uw_fy_24 %>%
  mutate(group_highest_education_completed = case_when(group_highest_education_completed %in% c("Master's Degree", "Doctorate Degree") ~ "Graduate Degree",
                                                       .default = group_highest_education_completed)) %>% 
  count(group_highest_education_completed) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Education Level",
         survey = "United Way") %>% 
  rename(data_point = group_highest_education_completed,
         survey_count = n)

educ_level <- rbind(educ_n2w, educ_uw) %>%
  select(survey, survey_question, data_point, survey_count, survey_total, survey_percent)

educ_level_all <- educ_level %>% 
  group_by(survey_question, data_point) %>% 
  summarise(survey_count = sum(survey_count)) %>%
  arrange(desc(survey_count)) %>%
  mutate(survey_total = sum(survey_count),
         survey_percent = round((survey_count/survey_total) * 100, 2),
         survey = "All") %>% 
  ungroup()

educ_level <- rbind(educ_level, educ_level_all)


# Military Service ----
# N2W does have answer option for "Yes, I am active duty military" but 0 responses in this subset 
military_n2w <- n2w_fy_23_24 %>%
  count(military_veteran) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Military Service",
         survey = "Network2Work",
         military_veteran = case_when(military_veteran == "Yes, I am a U.S. military veteran" ~ "U.S. military service",
                                      military_veteran == "Yes, I am active duty military" ~ "U.S. military service",
                                      military_veteran == "No" ~ "None"
                                      )) %>% 
  rename(data_point = military_veteran,
         survey_count = n)

military_uw <- uw_fy_24 %>%
  mutate(group_military_family_member = case_when(str_detect(group_military_family_member, "At least one parent/guardian") ~ "U.S. military service",
                                                  str_detect(group_military_family_member, "None of the above") ~ "None")) %>% 
  count(group_military_family_member) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Military Service",
         survey = "United Way") %>% 
  rename(data_point = group_military_family_member,
         survey_count = n) 

military <- rbind(military_n2w, military_uw) %>%
  select(survey, survey_question, data_point, survey_count, survey_total, survey_percent)

military_all <- military %>% 
  group_by(survey_question, data_point) %>% 
  summarise(survey_count = sum(survey_count)) %>%
  arrange(desc(survey_count)) %>%
  mutate(survey_total = sum(survey_count),
         survey_percent = round((survey_count/survey_total) * 100, 2),
         survey = "All") %>% 
  ungroup()

military <- rbind(military, military_all)


# Number of Children in the home ----
# Reduce categories to 4 or more
# Includes N2W respondents with no children
child_count_n2w <- n2w_fy_23_24 %>%
  select(fam_parent, fam_numchildren) %>% 
  mutate(fam_numchildren = case_when(fam_parent == "No" | fam_numchildren == "0" | fam_numchildren == "None, my children aren't living with me" ~ "0",
                                     .default = fam_numchildren)) %>% 
  count(fam_numchildren) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Count of children living in home",
         survey = "Network2Work") %>% 
  rename(data_point = fam_numchildren,
         survey_count = n)

child_count_uw <- uw_fy_24 %>%
  mutate(group_how_many_children_live_in_the_home = case_when(group_how_many_children_live_in_the_home %in% c("4","5","6","7","8","9","10") ~ "4 or more",
                                                              group_how_many_children_live_in_the_home == "Son is temporarily staying with grandma for coverage" ~ "0",
                                                              group_how_many_children_live_in_the_home == "One" ~ "1",
                                                              group_how_many_children_live_in_the_home %in% c("Two", "two") ~ "2",
                                                              group_how_many_children_live_in_the_home == "Three" ~ "3",
                                                              .default = group_how_many_children_live_in_the_home)) %>% 
  count(group_how_many_children_live_in_the_home) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Count of children living in home",
         survey = "United Way") %>% 
  rename(data_point = group_how_many_children_live_in_the_home,
         survey_count = n)

child_count <- rbind(child_count_n2w, child_count_uw) %>%
  select(survey, survey_question, data_point, survey_count, survey_total, survey_percent)

child_count_all <- child_count %>% 
  group_by(survey_question, data_point) %>% 
  summarise(survey_count = sum(survey_count)) %>%
  arrange(desc(survey_count)) %>%
  mutate(survey_total = sum(survey_count),
         survey_percent = round((survey_count/survey_total) * 100, 2),
         survey = "All") %>% 
  ungroup()

child_count <- rbind(child_count, child_count_all)


# Single Parent ----
# Only includes respondents with children (N2W)
# Reduce/Combine responses to only single parent or not
singleparent_n2w <- n2w_fy_23_24 %>%
  mutate(fam_singleparent = case_when(fam_singleparent %in% c("Yes-I'm a single mom", "Yes-I'm a single dad") ~ "Single parent/guardian",
                                      .default = fam_singleparent)) %>% 
  count(fam_singleparent) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Single Parent",
         survey = "Network2Work") %>% 
  rename(data_point = fam_singleparent,
         survey_count = n)

singleparent_uw <- uw_fy_24 %>%
  mutate(group_select_all_that_apply_7 = case_when(str_detect(group_select_all_that_apply_7, "Single parent/guardian") ~ "Single parent/guardian",
                                                   !str_detect(group_select_all_that_apply_7, "Single parent/guardian") ~ "No",
                                                   .default = group_select_all_that_apply_7)) %>% 
  count(group_select_all_that_apply_7) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Single Parent",
         survey = "United Way") %>% 
  rename(data_point = group_select_all_that_apply_7,
         survey_count = n)

singleparent <- rbind(singleparent_n2w, singleparent_uw) %>%
  select(survey, survey_question, data_point, survey_count, survey_total, survey_percent)

singleparent_all <- singleparent %>% 
  group_by(survey_question, data_point) %>% 
  summarise(survey_count = sum(survey_count)) %>%
  arrange(desc(survey_count)) %>%
  mutate(survey_total = sum(survey_count),
         survey_percent = round((survey_count/survey_total) * 100, 2),
         survey = "All") %>% 
  ungroup()

singleparent <- rbind(singleparent, singleparent_all)

# Language ----
# Just use united way data

# language_n2w <- n2w_fy_23_24 %>%
#   count(ell_language) %>%
#   arrange(desc(n)) %>% 
#   mutate(survey_total = sum(n),
#          survey_percent = round((n/survey_total) * 100, 2),
#          survey_question = "Primary Language",
#          survey = "Network2Work") %>% 
#   rename(data_point = ell_language,
#          survey_count = n)

language <- uw_fy_24 %>%
  count(group_primary_language_at_home) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Primary Language",
         survey = "United Way") %>% 
  rename(data_point = group_primary_language_at_home,
         survey_count = n) %>%
  select(survey, survey_question, data_point, survey_count, survey_total, survey_percent)

# Employment Status ----
# For UW - any employed (full time and part time) changed to "Yes" (currently working)
employed_n2w <- n2w_fy_23_24 %>%
  count(work_current) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Employment Status (Currently Working)",
         survey = "Network2Work") %>% 
  rename(data_point = work_current,
         survey_count = n)

employed_uw <- uw_fy_24 %>%
  mutate(group_what_is_the_primary_parent_guardians_employment_status_select_all_that_apply = case_when(str_detect(group_what_is_the_primary_parent_guardians_employment_status_select_all_that_apply, "Employed -") ~ "Yes",
                                                   !str_detect(group_select_all_that_apply_7, "Employed -") ~ "No",
                                                   .default = group_select_all_that_apply_7)) %>% 
  count(group_what_is_the_primary_parent_guardians_employment_status_select_all_that_apply) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Employment Status (Currently Working)",
         survey = "United Way") %>% 
  rename(data_point = group_what_is_the_primary_parent_guardians_employment_status_select_all_that_apply,
         survey_count = n)

employed <- rbind(employed_n2w, employed_uw) %>%
  select(survey, survey_question, data_point, survey_count, survey_total, survey_percent)

employed_all <- employed %>% 
  group_by(survey_question, data_point) %>% 
  summarise(survey_count = sum(survey_count)) %>%
  arrange(desc(survey_count)) %>%
  mutate(survey_total = sum(survey_count),
         survey_percent = round((survey_count/survey_total) * 100, 2),
         survey = "All") %>% 
  ungroup()

employed <- rbind(employed, employed_all)

# Public Benefits ----
# Reduce to yes/no public benefits
# Update - N2W benefits_ssdi must be a nested question - No answer in benefits_receive always has NA answer for ssdi
benefits_n2w <- n2w_fy_23_24 %>%
  # select(benefits_receive, benefits_ssdi) %>% 
  # mutate(benefits_all = case_when(benefits_receive == "Yes" & benefits_ssdi %in% c("Yes, SSI", "Yes, SSDI") ~ "Yes",
  #                                 benefits_receive == "Yes" & benefits_ssdi == "No" ~ "Yes",
  #                                 benefits_receive == "Yes" & is.na(benefits_ssdi) ~ "Yes",
  #                                 benefits_receive == "No" & benefits_ssdi %in% c("Yes, SSI", "Yes, SSDI") ~ "Yes",
  #                                 benefits_receive == "No" & benefits_ssdi == "No" ~ "No",
  #                                 .default = paste0(benefits_receive, ", ", benefits_ssdi))) %>% 
  count(benefits_receive) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Public Benefits",
         survey = "Network2Work") %>% 
  rename(data_point = benefits_receive,
         survey_count = n)

# Only rows with "None" to No, all others, "yes"
benefits_uw <- uw_fy_24 %>%
  mutate(group_family_supports = case_when(group_family_supports == "None" ~ "No",
                                           group_family_supports != "None" ~ "Yes",
                                           .default = group_family_supports)) %>% 
  count(group_family_supports) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Public Benefits",
         survey = "United Way") %>% 
  rename(data_point = group_family_supports,
         survey_count = n)

benefits <- rbind(benefits_n2w, benefits_uw) %>%
  select(survey, survey_question, data_point, survey_count, survey_total, survey_percent)

benefits_all <- benefits %>% 
  group_by(survey_question, data_point) %>% 
  summarise(survey_count = sum(survey_count)) %>%
  arrange(desc(survey_count)) %>%
  mutate(survey_total = sum(survey_count),
         survey_percent = round((survey_count/survey_total) * 100, 2),
         survey = "All") %>% 
  ungroup()

benefits <- rbind(benefits, benefits_all)

# Housing instability ----
# UW unstable including movement / unstable not including
# N2W - combine answers from housing_rentown, housing_stable
housing_stable_n2w <- n2w_fy_23_24 %>%
  select(housing_rentown, housing_stable) %>%
  mutate(housing_combine = case_when(is.na(housing_rentown) & is.na(housing_stable) ~ NA,
                                 .default = paste0(housing_rentown, "; ", housing_stable))) %>%
  mutate(housing_all = case_when(str_detect(housing_combine, "Unstable") ~ "Unstable",
                                 str_detect(housing_combine, "Neither-I am staying in an emergency shelter") ~ "Unstable",
                                 str_detect(housing_combine, "Neither-I am living in a hotel") ~ "Unstable",
                                 str_detect(housing_combine, "Neither-I am living in a place not meant for housing") ~ "Unstable",
                                 str_detect(housing_combine, "Neither-I jump from couch to couch") ~ "Unstable",
                                housing_combine %in% c("Rent-I am on the lease; NA", 
                                                    "Neither-I live with family or friends and I AM NOT on the lease; Stable-I have no concerns about having to leave", 
                                                    "Own-I pay a mortgage; NA",
                                                    "Rent-I am on the lease; Stable-I have no concerns about having to leave") ~ "Stable",
                                 .default = housing_combine)) %>% 
  count(housing_all) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Stable Housing",
         survey = "Network2Work") %>%
  ungroup() %>% 
  rename(data_point = housing_all,
         survey_count = n)

housing_stable_uw <- uw_fy_24 %>%
  mutate(group_unstable_housing = case_when(str_detect(group_unstable_housing, "We are currently living in temporary/transitional housing|We are currently experiencing homelessness|We live/have lived in a campground/motel/shelter") ~ "Unstable",
                                            group_unstable_housing %in% c("None of the above", "None of these", "Other", "None of these, None of the above",
                                                                          "We share our living space with other person(s)", "None of these, We share our living space with other person(s)",
                                                                          "None of these, Other", "None of these, We share our living space with other person(s), Other") ~ "Stable",
                                            str_detect(group_unstable_housing, "We have moved|have lived") ~ "Prior instability and/or frequent movement",
                                            .default = group_unstable_housing)) %>% 
  count(group_unstable_housing) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Stable Housing",
         survey = "United Way") %>% 
  rename(data_point = group_unstable_housing,
         survey_count = n)

housing_stable <- rbind(housing_stable_n2w, housing_stable_uw) %>%
  select(survey, survey_question, data_point, survey_count, survey_total, survey_percent)

housing_stable_all <- housing_stable %>% 
  group_by(survey_question, data_point) %>% 
  summarise(survey_count = sum(survey_count)) %>%
  arrange(desc(survey_count)) %>%
  mutate(survey_total = sum(survey_count),
         survey_percent = round((survey_count/survey_total) * 100, 2),
         survey = "All") %>% 
  ungroup()

housing_stable <- rbind(housing_stable, housing_stable_all)


# Transportation ----
# N2W and UW separate - can combine some 
# N2W transportation
# "None" for N2W transportation needs derived from respondents with all of the following conditions:
# transportation_need == "No-I plan to take my own vehicle" & 
# transportation_validlicense == "Yes" & transportation_caraccess == "Yes" & 
# str_detect(transportation_carinspection, "Yes") & transportation_carregistration == "Yes, registration is up to date"
# For "Does not have reliable transportation" any of the following answers:                                 
# transportation_need == "Yes" 
# transportation_need == "No- I plan to walk" (All answers with this also stated they would not walk in the rain/bad weather, and would need transportation)
# transportation_reliableride == "Somewhat reliable-I may need help with a backup transportation plan"

transportation_n2w <- n2w_fy_23_24 %>%
  select(transportation_need, transportation_validlicense, transportation_caraccess, transportation_carinspection, transportation_carregistration, 	
         transportation_reliableride, transportation_walkweather, 	
         transportation_busline) %>% 
  mutate(tranport_needs = case_when(is.na(transportation_need) ~ NA,
                                    str_detect(transportation_validlicense, "No") ~ "Does not have driver’s license",
                                    transportation_need == "Yes" | transportation_need == "No- I plan to walk" | transportation_reliableride == "Somewhat reliable-I may need help with a backup transportation plan" ~ "Does not have reliable transportation",
                                    transportation_need == "No-I plan to take my own vehicle" & transportation_validlicense == "Yes" & transportation_caraccess == "Yes" & str_detect(transportation_carinspection, "Yes") & transportation_carregistration == "Yes, registration is up to date" ~ "None")) %>% 
  
  count(tranport_needs) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Transportation needs",
         survey = "Network2Work") %>% 
  rename(data_point = tranport_needs,
         survey_count = n)

# United Way - transportation needs
transport_select_uw <- uw_fy_24 %>%
  select(group_family_transportation_needs) %>% 
  mutate(work_hours = case_when(str_detect(group_family_transportation_needs, "work longer hours than the typical school day") ~ "Works longer hours than school day"),
         drivers_license = case_when(str_detect(group_family_transportation_needs, "do not have a driver’s license") ~ "Does not have driver’s license"),
         reliable_transport = case_when(str_detect(group_family_transportation_needs, "do not have reliable transportation") ~ "Does not have reliable transportation"),
         transport_barrier = case_when(str_detect(group_family_transportation_needs, "Transportation is a barrier") ~ "Transportation is a barrier to getting child to and from school"),
         none = case_when(group_family_transportation_needs == "None of the above" ~ "None"),
         na = case_when(is.na(group_family_transportation_needs) ~ "NA"))

work_hours_uw <- transport_select_uw %>% 
  count(work_hours, sort = TRUE) %>%
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Transportation needs",
         survey = "United Way") %>% 
  rename(data_point = work_hours,
         survey_count = n) %>% 
  na.omit()

drivers_license_uw <- transport_select_uw %>% 
  count(drivers_license, sort = TRUE) %>%
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Transportation needs",
         survey = "United Way") %>% 
  rename(data_point = drivers_license,
         survey_count = n) %>% 
  na.omit()

reliable_transport_uw <- transport_select_uw %>% 
  count(reliable_transport, sort = TRUE) %>%
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Transportation needs",
         survey = "United Way") %>% 
  rename(data_point = reliable_transport,
         survey_count = n) %>% 
  na.omit()

transport_barrier_uw <- transport_select_uw %>% 
  count(transport_barrier, sort = TRUE) %>%
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Transportation needs",
         survey = "United Way") %>% 
  rename(data_point = transport_barrier,
         survey_count = n) %>% 
  na.omit()

none_transp_uw <- transport_select_uw %>% 
  count(none, sort = TRUE) %>%
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Transportation needs",
         survey = "United Way") %>% 
  rename(data_point = none,
         survey_count = n) %>% 
  na.omit()

na_transp_uw <-  transport_select_uw %>% 
  count(na, sort = TRUE) %>%
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Transportation needs",
         survey = "United Way") %>% 
  rename(data_point = na,
         survey_count = n) %>% 
  na.omit()

transportation_needs_uw <- rbind(work_hours_uw, drivers_license_uw, reliable_transport_uw, transport_barrier_uw, none_transp_uw, na_transp_uw) %>% 
  mutate(data_point = case_when(data_point == "NA" ~ NA,
                                .default = data_point))

transportation <- rbind(transportation_n2w, transportation_needs_uw) %>%
  select(survey, survey_question, data_point, survey_count, survey_total, survey_percent)


# Incarceration ----
# N2W crim_prison nested - including no answers to crim_everconvicted for "No" answers
incarceration_n2w <- n2w_fy_23_24 %>%
  select(crim_everconvicted, crim_prison) %>% 
  mutate(crim_incarcer = case_when(crim_everconvicted == "No" & is.na(crim_prison) ~ "No",
                                   crim_everconvicted == "Yes" & crim_prison == "No" ~ "No",
                                   crim_everconvicted == "Yes" & crim_prison == "Yes" ~ "Yes",
                                   is.na(crim_everconvicted) | is.na(crim_prison) ~ NA,
                                   .default = paste0(crim_everconvicted, ", ", crim_prison))) %>% 
  count(crim_incarcer) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Incarceration history",
         survey = "Network2Work") %>% 
  rename(data_point = crim_incarcer,
         survey_count = n)

incarceration_uw <- uw_fy_24 %>%
  mutate(group_is_either_parent_guardian_incarcerated = case_when(str_detect(group_is_either_parent_guardian_incarcerated,"Parent") ~ "Yes",
                                                                  !str_detect(group_is_either_parent_guardian_incarcerated,"Parent") ~ "No",
                                                                  .default = group_is_either_parent_guardian_incarcerated)) %>% 
  count(group_is_either_parent_guardian_incarcerated) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Incarceration history",
         survey = "United Way") %>% 
  rename(data_point = group_is_either_parent_guardian_incarcerated,
         survey_count = n)

incarceration <- rbind(incarceration_n2w, incarceration_uw) %>%
  select(survey, survey_question, data_point, survey_count, survey_total, survey_percent)

incarceration_all <- incarceration %>% 
  group_by(survey_question, data_point) %>% 
  summarise(survey_count = sum(survey_count)) %>%
  arrange(desc(survey_count)) %>%
  mutate(survey_total = sum(survey_count),
         survey_percent = round((survey_count/survey_total) * 100, 2),
         survey = "All") %>% 
  ungroup()

incarceration <- rbind(incarceration, incarceration_all)

# Disability, Learning challenge, Mental & Chronic Health ----

# UW How often does each show up
disability_mental_health_uw <- uw_fy_24 %>%
  select(group_parent_guardian_mental_health) %>% 
  mutate(chronic_health_condition = case_when(str_detect(group_parent_guardian_mental_health, "At least one parent/guardian has a chronic health condition") ~ "Chronic health condition"),
         disability = case_when(str_detect(group_parent_guardian_mental_health, "At least one parent/guardian has a disability") ~ "Disability"),
         learning_challenge = case_when(str_detect(group_parent_guardian_mental_health, "At least one parent/guardian has a learning challenge") ~ "Learning challenge"),
         mental_health_concern = case_when(str_detect(group_parent_guardian_mental_health, "At least one parent/guardian has a mental health concern") ~ "Mental health concern"),
         substance_abuse = case_when(str_detect(group_parent_guardian_mental_health, "At least one parent/guardian is experiencing substance abuse") ~ "Experiencing substance abuse"),
         none = case_when(group_parent_guardian_mental_health == "None of the above" ~ "None"),
         na = case_when(is.na(group_parent_guardian_mental_health) ~ "NA"))

chronic_health_uw <- disability_mental_health_uw %>% 
  count(chronic_health_condition, sort = TRUE) %>%
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Disability, Learning challenge, and Health Concerns",
         survey = "United Way") %>% 
  rename(data_point = chronic_health_condition,
         survey_count = n) %>% 
  na.omit()

disability_uw <- disability_mental_health_uw %>% 
  count(disability, sort = TRUE) %>%
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Disability, Learning challenge, and Health Concerns",
         survey = "United Way") %>% 
  rename(data_point = disability,
         survey_count = n) %>% 
  na.omit()

learning_challenge_uw <- disability_mental_health_uw %>% 
  count(learning_challenge, sort = TRUE) %>%
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Disability, Learning challenge, and Health Concerns",
         survey = "United Way") %>% 
  rename(data_point = learning_challenge,
         survey_count = n) %>% 
  na.omit()

mental_health_concern_uw <- disability_mental_health_uw %>% 
  count(mental_health_concern, sort = TRUE) %>%
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Disability, Learning challenge, and Health Concerns",
         survey = "United Way") %>% 
  rename(data_point = mental_health_concern,
         survey_count = n) %>% 
  na.omit()

substance_abuse_uw <- disability_mental_health_uw %>% 
  count(substance_abuse, sort = TRUE) %>%
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Disability, Learning challenge, and Health Concerns",
         survey = "United Way") %>% 
  rename(data_point = substance_abuse,
         survey_count = n) %>% 
  na.omit()

none_concern_uw <- disability_mental_health_uw %>% 
  count(none, sort = TRUE) %>%
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Disability, Learning challenge, and Health Concerns",
         survey = "United Way") %>% 
  rename(data_point = none,
         survey_count = n) %>% 
  na.omit()

na_concern_uw <-  disability_mental_health_uw %>% 
  count(na, sort = TRUE) %>%
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Disability, Learning challenge, and Health Concerns",
         survey = "United Way") %>% 
  rename(data_point = na,
         survey_count = n) %>% 
  na.omit()

health_mental_concerns_uw <- rbind(chronic_health_uw, disability_uw, mental_health_concern_uw, learning_challenge_uw, substance_abuse_uw, none_concern_uw, na_concern_uw)

# Behind on rent/utils ----
housing_rentbehind_n2w <- n2w_fy_23_24 %>%
  select(housing_rentbehind, housing_billselecgas, housing_billswatersewer) %>%
  mutate(housing_rent_utils_combine = paste0(housing_rentbehind, "; ", housing_billselecgas, "; ", housing_billswatersewer),
         behind_rent_utils = case_when(str_detect(housing_rent_utils_combine, "Yes") ~ "Yes",
                                       housing_rent_utils_combine == "NA; NA; NA" ~ NA,
                                       .default = "No")) %>% 
  count(behind_rent_utils) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Trouble paying rent and/or utils",
         survey = "Network2Work") %>% 
  rename(data_point = behind_rent_utils,
         survey_count = n)

group_housing_situation_uw <- uw_fy_24 %>%
  mutate(group_housing_situation = case_when(str_detect(group_housing_situation,"We are having trouble paying utilities and/or rent") ~ "Yes",
                                             !str_detect(group_housing_situation,"We are having trouble paying utilities and/or rent") ~ "No",
                                             .default = group_housing_situation)) %>% 
  count(group_housing_situation) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Trouble paying rent and/or utils",
         survey = "United Way") %>% 
  rename(data_point = group_housing_situation,
         survey_count = n)

housing_rent_utils <- rbind(housing_rentbehind_n2w, group_housing_situation_uw) %>%
  select(survey, survey_question, data_point, survey_count, survey_total, survey_percent)

housing_rent_utils_all <- housing_rent_utils %>% 
  group_by(survey_question, data_point) %>% 
  summarise(survey_count = sum(survey_count)) %>%
  arrange(desc(survey_count)) %>%
  mutate(survey_total = sum(survey_count),
         survey_percent = round((survey_count/survey_total) * 100, 2),
         survey = "All") %>% 
  ungroup()

housing_rent_utils <- rbind(housing_rent_utils, housing_rent_utils_all)

# Combine tables ----
data_all <- rbind(locality,
                  age,
                  race,
                  ethnicity,
                  gender,
                  educ_level,
                  military,
                  child_count,
                  singleparent,
                  language,
                  employed,
                  benefits,
                  housing_stable,
                  transportation,
                  housing_rent_utils,
                  incarceration,
                  health_mental_concerns_uw)

# Create CSV ----
write_csv(data_all, "cna_data_all.csv")

# Create Excel workbook ----
demographics_sheet <- rbind(locality, age, race, ethnicity, gender)
housing_sheet <- rbind(housing_stable, housing_rent_utils)

wb_sheets <- list("Demographics" = demographics_sheet,
                  "Education Level" = educ_level,
                  "Military Service" = military,
                  "Children living in home" = child_count,
                  "Single Parent" = singleparent,
                  "Primary Language" = language,
                  "Employment Status" = employed,
                  "Public Benefits" = benefits,
                  "Housing" = housing_sheet,
                  "Transportation needs" = transportation,
                  "Incarceration" = incarceration,
                  "Health" = health_mental_concerns_uw)

write.xlsx(wb_sheets, file = "CNA_Survey_Data.xlsx", keepNA = TRUE)


