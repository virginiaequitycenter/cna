# Network2Work Clean Survey Response Data

library(here)
library(tidyverse)
library(readxl)
library(janitor)

# Set WD
setwd(here("Network2Work"))

survey_sheet <- read_excel("data/survey_data_copied.xlsx", sheet = "Survey Responses") %>% 
  mutate(Question = str_remove_all(Question, "<p>|</p>")) %>% 
  clean_names() 

# unique_ids <- survey_sheet$seeker_id %>% unique()
# unique_questions <- survey_sheet$question %>% unique()

survey_wide <- survey_sheet %>% 
  select(-section_name) %>% 
  pivot_wider(names_from = question, values_from = answer_choice) %>% 
  unnest()

# Rename columns
name_vec <- c("seeker_id", "fam_afterschoolfund", "fam_afterschoolfind", "services_bankaccount", 
              "training_mathreadwrite", "educ_hsreadaloud", "educ_hsextrahelp", "crim_bonding", 
              "transportation_busline", "tranpsportation_carinsurance", "transportation_carloan", 
              "transportation_carinspection", "fam_childcarefund", "fam_childcarefind", "fam_youngchildren", 
              "noncitizen_path", "training_computer", "crim_probation", "crim_prison", "crim_felmisd",
              "crim_type", "health_dental", "health_dentalneed", "disability_services", 
              "disability_accommodate", "transportation_whysuspended", "transportation_validlicense",
              "educ_highestgrade", "housing_billselecgas", "ell_assistance", "ell_class",
              "ell_language", "services_financecoach", "foodsecure_month", "foster_program",
              "educ_gedprogram", "citizen_workpermit", "military_veteran", "military_spouse",
              "work_current", "fam_parent", "benefits_receive", "crim_everconvicted",
              "wellbeing_social", "wellbeing_ladder", "foster_care", "dem_age", "dem_locality", 
              "work_whyjobtrack", "dem_hispanic", "consent", "dem_genderidentity", "dem_raceethnicity",
              "health_eyecare", "foster_programinterest", "health_careaccess", "health_insurance",
              "fam_parentsupport", "crim_released", "housing_ownerinterest", "housing_utilities",
              "housing_rentown", "foodsecure_day", "housing_stable", "noncitizen_arrival",
              "veteran_disabilityfile", "ell_individualsupport", "services_interviewattire",
              "legal_pending", "legal_issue", "work_unemploybenefits", "work_unemploylength",
              "health_mentalhlth", "military_pscspouse", "housing_mortgagebehind", "fam_numchildren",
              "fam_otherdependents", "services_ownbusiness", "services_personalcare", 
              "services_phoneinternet", "nolicense_id", "health_substanceuse", "health_doctor",
              "health_absent", "pvcc", "health_rx", "benefits_view", "benefits_housing",
              "benefits_snap", "benefits_ssdi", "benefits_tanf", "work_fulltime", 
              "housing_rentbehind", "housing_rent", "services_resume", "dem_selectiveservice",
              "veteran_disability", "fam_singleparent", "foodsecure_snap", "training_required",
              "training_type", "training_enrolled", "training_wioa", "transportation_need",
              "transportation_reliableride", "transportation_walkweather", "transportation_caraccess",
              "transportation_owncar", "transportation_carregistration", "veteran_last48",
              "veteran_dischargestatus", "veteran_benfits", "housing_billswatersewer", 
              "services_worksupplies", "training_workplacereadiness")

names(survey_wide) <- name_vec

# Save wide survey data
write_csv(survey_wide, "data/survey_responses_wide.csv")

# Create reference sheet for survey questions and column names
questions <- list(column_name = name_vec[-1],
                  survey_question = survey_sheet$question %>% unique())

questions_df <- as.data.frame(questions)
questions_df <- questions_df %>% 
  mutate(survey_question = str_remove_all(survey_question, "<p>|</p>"))

write_csv(questions_df, "data/survey_reference.csv")
