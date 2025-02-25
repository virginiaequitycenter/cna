# Network2Work Survey Data
# Wrangle for Needs Assessment

library(here)
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

# Set WD
setwd(here("Network2Work"))

survey_responses <- read_csv("data/survey_responses_wide.csv")
survey_reference <- read_csv("data/survey_reference.csv")
wage_sheet <- read_excel("data/survey_data_copied.xlsx", sheet = "Wage Data-October 2024") %>%
  clean_names() 

# write_csv(wage_sheet, "data/wage_sheet.csv")

# Filter wage date by enrollment date
wage_fy_23_24 <- wage_sheet %>% 
  filter(between(enrollment_date, as.Date('2022-07-01'), as.Date('2024-06-30'))) 

# fy_23_24_unique_ids <- unique(wage_fy_23_24$seeker_id)

# Convert periodyear and period to date
wage_fy_23_24_filtered <- wage_fy_23_24 %>% 
  mutate(wage_yq = paste0(periodyear, ".", period),
         wage_date = lubridate::yq(wage_yq))

# Filter wage date to pre-enrollment value closest to enrollment date
wage_fy_23_24_filtered <- wage_fy_23_24_filtered %>% 
  filter(wage_date <= enrollment_date) %>% 
  mutate(diff = abs(as.Date(enrollment_date) - wage_date)) %>% 
  group_by(seeker_id, enrollment_date) %>% 
  slice_min(diff)

# Sum wage date from same quarter
wage_fy_23_24_filtered <- wage_fy_23_24_filtered %>% 
  group_by(seeker_id, enrollment_date, wage_date) %>%
  summarise(wages = sum(wages))
  
# fy_23_24_filtered_unique_ids <- unique(wage_fy_23_24_filtered$seeker_id)

# Join wage data with survey results
survey_fy_23_24 <- survey_responses %>% 
  inner_join(wage_fy_23_24_filtered)

# survey_fy_23_24_filtered_unique_ids <- unique(survey_fy_23_24$seeker_id)

# .....................................
# Wage sheet with enrollment date only
wage_fy_23_24_enroll_date <- wage_fy_23_24 %>% 
  select(seeker_id, enrollment_date) %>% 
  unique()

# Join with survey data
survey_fy_23_24_enroll_date <- survey_responses %>% 
  inner_join(wage_fy_23_24_enroll_date)

# .........................................
# Surveys by questions categories
# Demographics
survey_demographics <- survey_fy_23_24 %>% 
  select(seeker_id, starts_with("dem_"))

# Family
survey_family <- survey_fy_23_24 %>% 
  select(seeker_id, starts_with("fam_"))

# Housing
survey_housing <- survey_fy_23_24 %>% 
  select(seeker_id, starts_with("housing_"))

# Health
survey_health <- survey_fy_23_24 %>% 
  select(seeker_id, starts_with("health_"))

