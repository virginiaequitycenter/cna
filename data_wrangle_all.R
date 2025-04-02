# Community Needs Assessment
# Survey Data from Network2Work, United Way, MACAA

# Libraries ----
library(here)
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

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
  count(dem_age) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Age",
         survey = "Network2Work") %>% 
  rename(data_point = dem_age,
         survey_count = n)

age_uw <- uw_fy_24 %>%
  count(parent_age) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Age",
         survey = "United Way",
         parent_age = str_to_title(str_replace(parent_age, " - ", "-"))) %>% 
  rename(data_point = parent_age,
         survey_count = n)

combine_age_group <- age_uw %>% 
  filter(data_point %in% c("25-39", "25-39")) %>% 
  mutate(data_point = case_when(data_point == "25-39" ~ "25-54",
                                data_point == "40-54" ~ "25-54",
                                .default = data_point)) %>% 
  group_by(data_point, survey_total, survey_question, survey) %>% 
  summarise(survey_count = sum(survey_count),
            survey_percent = round((survey_count/survey_total) * 100, 2))

age_uw <- rbind(age_uw, combine_age_group)

age <- rbind(age_n2w, age_uw) %>%
  select(survey, survey_question, data_point, survey_count, survey_total, survey_percent)

age_all <- age %>% 
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
  count(educ_highestgrade) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Education Level",
         survey = "Network2Work",
         educ_highestgrade = case_when(educ_highestgrade == "No high school diploma or GED" ~ "Did Not Complete High School",
                                       educ_highestgrade == "High School Diploma" ~ "High School Graduate",
                           .default = educ_highestgrade)) %>% 
  rename(data_point = educ_highestgrade,
         survey_count = n)

educ_uw <- uw_fy_24 %>%
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
         military_veteran = case_when(military_veteran == "Yes, I am a U.S. military veteran" ~ "Veteran of the U.S. military",
                                      military_veteran == "Yes, I am active duty military" ~ "Active duty member of the U.S. military",
                                      military_veteran == "No" ~ "None"
                                      )) %>% 
  rename(data_point = military_veteran,
         survey_count = n)

military_uw <- uw_fy_24 %>%
  count(group_military_family_member) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Military Service",
         survey = "United Way") %>% 
  rename(data_point = group_military_family_member,
         survey_count = n) 

military_uw_split <- military_uw %>% 
  separate(data_point, into = c("data_point", "data_point_2"), sep = ", ")

military_uw_1 <- military_uw_split %>% 
  select(survey, survey_question, data_point, survey_count, survey_total, survey_percent)

military_uw_2 <- military_uw_split %>% 
  select(survey, survey_question, data_point_2, survey_count, survey_total, survey_percent) %>% 
  filter(! is.na(data_point_2)) %>% 
  rename(data_point = data_point_2)

military_uw <- rbind(military_uw_1, military_uw_2) %>% 
  mutate(data_point = case_when(data_point == "At least one parent/guardian is a veteran of the United States military" ~ "Veteran of the U.S. military",
                                data_point %in% c("At least one parent/guardian is an active duty member of the United States military", "At least one parent/guardian is actively deployed to a combat zone") ~ "Active duty member of the U.S. military",
                                data_point == "None of the above" ~ "None")) %>% 
  group_by(survey, survey_question, data_point) %>% 
  summarise(survey_count = sum(survey_count)) %>% 
  ungroup() %>% 
  mutate(survey_total = sum(survey_count),
         survey_percent = round((survey_count/survey_total) * 100, 2))

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
child_count_n2w <- n2w_fy_23_24 %>%
  count(fam_numchildren) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Count of children living in home",
         survey = "Network2Work") %>% 
  rename(data_point = fam_numchildren,
         survey_count = n)

child_count_uw <- uw_fy_24 %>%
  count(group_how_many_children_live_in_the_home) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Count of children living in home",
         survey = "United Way",
         group_how_many_children_live_in_the_home = case_when(group_how_many_children_live_in_the_home == "One" ~ "1",
                                                              group_how_many_children_live_in_the_home %in% c("Two", "two") ~ "2",
                                                              group_how_many_children_live_in_the_home == "Three" ~ "3",
                                                              .default = group_how_many_children_live_in_the_home)) %>% 
  rename(data_point = group_how_many_children_live_in_the_home,
         survey_count = n)

child_count <- rbind(child_count_n2w, child_count_uw) %>%
  select(survey, survey_question, data_point, survey_count, survey_total, survey_percent)


# Single Parent ----
singleparent_n2w <- n2w_fy_23_24 %>%
  count(fam_singleparent) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Single Parent",
         survey = "Network2Work") %>% 
  rename(data_point = fam_singleparent,
         survey_count = n)

singleparent_uw <- uw_fy_24 %>%
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


# Language ----
language_n2w <- n2w_fy_23_24 %>%
  count(ell_language) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Primary Language",
         survey = "Network2Work") %>% 
  rename(data_point = ell_language,
         survey_count = n)

language_uw <- uw_fy_24 %>%
  count(group_primary_language_at_home) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Primary Language",
         survey = "United Way") %>% 
  rename(data_point = group_primary_language_at_home,
         survey_count = n)

language <- rbind(language_n2w, language_uw) %>%
  select(survey, survey_question, data_point, survey_count, survey_total, survey_percent)

# Employment Status ----
employed_n2w <- n2w_fy_23_24 %>%
  count(work_current) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Employment Status",
         survey = "Network2Work") %>% 
  rename(data_point = work_current,
         survey_count = n)

employed_uw <- uw_fy_24 %>%
  count(group_what_is_the_primary_parent_guardians_employment_status_select_all_that_apply) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Employment Status",
         survey = "United Way") %>% 
  rename(data_point = group_what_is_the_primary_parent_guardians_employment_status_select_all_that_apply,
         survey_count = n)

employed <- rbind(employed_n2w, employed_uw) %>%
  select(survey, survey_question, data_point, survey_count, survey_total, survey_percent)

# Public Benefits ----
benefits_n2w <- n2w_fy_23_24 %>%
  count(benefits_receive) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Public Benefits",
         survey = "Network2Work") %>% 
  rename(data_point = benefits_receive,
         survey_count = n)

benefits_uw <- uw_fy_24 %>%
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

# Housing instability ----
housing_stable_n2w <- n2w_fy_23_24 %>%
  count(housing_stable) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Stable Housing",
         survey = "Network2Work") %>% 
  rename(data_point = housing_stable,
         survey_count = n)

housing_type_n2w <- n2w_fy_23_24 %>%
  count(housing_rentown) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Stable Housing - Type",
         survey = "Network2Work") %>% 
  rename(data_point = housing_rentown,
         survey_count = n)

housing_stable_uw <- uw_fy_24 %>%
  count(group_unstable_housing) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Stable Housing",
         survey = "United Way") %>% 
  rename(data_point = group_unstable_housing,
         survey_count = n)

housing_stable <- rbind(housing_stable_n2w, housing_type_n2w, housing_stable_uw) %>%
  select(survey, survey_question, data_point, survey_count, survey_total, survey_percent)

# Transportation ----
transportation_n2w <- n2w_fy_23_24 %>%
  count(transportation_need) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Transportation needs",
         survey = "Network2Work") %>% 
  rename(data_point = transportation_need,
         survey_count = n)

transport_lic_n2w <- n2w_fy_23_24 %>%
  count(transportation_validlicense) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Transportation - Driver's license",
         survey = "Network2Work") %>% 
  rename(data_point = transportation_validlicense,
         survey_count = n)

transportation_uw <- uw_fy_24 %>%
  count(group_family_transportation_needs) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Transportation needs",
         survey = "United Way") %>% 
  rename(data_point = group_family_transportation_needs,
         survey_count = n)

transportation <- rbind(transportation_n2w, transport_lic_n2w, transportation_uw) %>%
  select(survey, survey_question, data_point, survey_count, survey_total, survey_percent)


# Incarceration ----
incarceration_n2w <- n2w_fy_23_24 %>%
  count(crim_prison) %>%
  arrange(desc(n)) %>% 
  mutate(survey_total = sum(n),
         survey_percent = round((n/survey_total) * 100, 2),
         survey_question = "Incarceration history",
         survey = "Network2Work") %>% 
  rename(data_point = crim_prison,
         survey_count = n)

incarceration_uw <- uw_fy_24 %>%
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
                  incarceration)

write_csv(data_all, "cna_data_all.csv")

