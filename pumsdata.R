# Community needs assessment 2025
# PUMS data for baseline comparisons


# Setup ----
library(tidyverse)
library(tidycensus)
library(janitor)
library(readxl)
require(openxlsx)

# ACS PUMS, 2023 5-year release ----
# FTP access: https://www2.census.gov/programs-surveys/acs/data/pums/2023/5-Year/
# Virginia person-level records: https://www2.census.gov/programs-surveys/acs/data/pums/2023/5-Year/csv_pva.zip
# Virginia housing-level records: https://www2.census.gov/programs-surveys/acs/data/pums/2023/5-Year/csv_hva.zip
# Documentation: https://www.census.gov/programs-surveys/acs/microdata/documentation.html
## download ----
# pva_link <- "https://www2.census.gov/programs-surveys/acs/data/pums/2023/5-Year/csv_pva.zip"
# hva_link <- "https://www2.census.gov/programs-surveys/acs/data/pums/2023/5-Year/csv_hva.zip"
# 
# download.file(pva_link, "downloads/csv_pva5.zip")
# download.file(hva_link, "downloads/csv_hva5.zip")
# 
# unzip("downloads/csv_pva5.zip", exdir = "downloads/")
# unzip("downloads/csv_hva5.zip", exdir = "downloads/")
# 
# ## read ----
# pva <- read_csv("downloads/psam_p51.csv") 
# hva <- read_csv("downloads/psam_h51.csv")
# 
# ## filter to cville region pumas ----
# # these contain the six localities of the
# # thomas jefferson planning district:
# # Charlottesville, Albemarle, Louisa, Nelson, Fluvanna, Greene
# # 2010 pumas used for 2019-2022 surveys; 2020 pumas used for 2021-2023 surveys
# pcvl <- pva %>% 
#   filter(PUMA %in% c("54001", "10901"))
# 
# hcvl <- hva %>% 
#   filter(PUMA %in% c("54001", "10901"))
# 
# ## save ----
# write_csv(pcvl, "data/cvl_acspums_person_5year.csv")
# write_csv(hcvl, "data/cvl_acspums_household_5year.csv")


## read ----
pcvl <- read_csv("data/cvl_acspums_person_5year.csv") %>% 
  clean_names()
hcvl <- read_csv("data/cvl_acspums_household_5year.csv") %>% 
  clean_names()


# N2W equivalent pop ---- 
# 18 to 65, in labor force, wages below 75K
## locality populations ----
# person estimates
# can't get wages for this one; pop 20-64 in labor force
# acs_subject <- load_variables(year = 2023, dataset = "acs5/subject")
region <-  c("540", "003", "065", "079", "109", "125")
var <- c(pop2064 = "S2301_C01_021",
         lfpr = "S2301_C02_021")

locality_n2w <- get_acs(geography = "county",
                        state = "51",
                        county = region,
                        variables = var,
                        survey = "acs5",
                        year = 2023,
                        output = "wide")

locality_n2w <- locality_n2w %>% 
  mutate(lfnum = pop2064E*(lfprE/100),
         total = sum(lfnum),
         percent = (lfnum / total)*100,
         pop_equiv = "Network2Work",
         question_equiv = "Locality",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = NAME,
         estimate = lfnum, total, percent, estimate_units)

## person data
pn2w <- pcvl %>% 
  filter(wagp < 75000,
         agep < 65,
         esr != 6)

## household data 
hn2w <- hcvl %>% 
  filter(serialno %in% pn2w$serialno)

## age, race, ethnicity, sex ----
# person level
age_n2w <- pn2w %>% 
  mutate(age_bins = case_when(
    agep < 19 ~ "18 and under",
    agep >= 19 & agep < 25 ~ "19-24",
    agep >= 25 & agep < 55 ~ "25-54",
    agep >= 55 & agep < 60 ~ "55-59",
    agep >= 60 ~ "60+"),
    total = sum(pwgtp)) %>% 
  group_by(age_bins) %>% 
  summarize(estimate = sum(pwgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "Network2Work",
         question_equiv = "Age",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = age_bins,
         estimate, total, percent, estimate_units)
    
race_n2w <- pn2w %>% 
  mutate(race = case_when(
    rac1p == 1 ~ "White",
    rac1p == 2 ~ "Black",
    rac1p == 6 ~ "Asian",
    rac1p == 7 ~ "Hawaiian/Pacific Islander",
    rac1p == 8 ~ "Some other race",
    rac1p == 9 ~ "Multiracial",
    rac1p %in% c(3,4,5) ~ "American Indian/Alaskan Native"),
    total = sum(pwgtp)) %>% 
  group_by(race) %>% 
  summarize(estimate = sum(pwgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "Network2Work",
         question_equiv = "Race",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = race,
         estimate, total, percent, estimate_units)

hisp_n2w <- pn2w %>% 
  mutate(hispanic = if_else(hisp == "01", "Not Hispanic", "Hispanic"),
         total = sum(pwgtp)) %>% 
  group_by(hispanic) %>% 
  summarize(estimate = sum(pwgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "Network2Work",
         question_equiv = "Ethnicity",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = hispanic,
         estimate, total, percent, estimate_units)

sex_n2w <- pn2w %>% 
  mutate(sex = if_else(sex == 1, "Male", "Female"),
         total = sum(pwgtp)) %>% 
  group_by(sex) %>% 
  summarize(estimate = sum(pwgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "Network2Work",
         question_equiv = "Sex",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = sex,
         estimate, total, percent, estimate_units)

## education, employment, veteran/military, disability, language ---- 
# person level
educ_n2w <- pn2w %>% 
  mutate(educ = case_when(
    schl < 16 ~ "Did Not Complete High School",
    schl == 16 ~ "High School Graduate",
    schl == 17 ~ "GED",
    schl == 18 | schl == 19 ~ "Some College or Advanced Training",
    schl == 20 ~ "Associate's Degree",
    schl == 21 ~ "Bachelor's Degree",
    schl > 21 ~ "Graduate Degree"),
    total = sum(pwgtp)) %>% 
  group_by(educ) %>% 
  summarize(estimate = sum(pwgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "Network2Work",
         question_equiv = "Education Level",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = educ,
         estimate, total, percent, estimate_units)

work_n2w <- pn2w %>% 
  mutate(working = case_when(
    esr == 3 ~ "No",
    esr %in% c(1,2,4,5) ~ "Yes"),
    total = sum(pwgtp)) %>% 
  group_by(working) %>% 
  summarize(estimate = sum(pwgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "Network2Work",
         question_equiv = "Employment Status",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = working,
         estimate, total, percent, estimate_units)

mil_n2w <- pn2w %>% 
  mutate(military = case_when(
    mil == 4 ~ "None",
    mil %in% c(1,2,3) ~ "U.S. Military Service",
    TRUE ~ "None"),
    total = sum(pwgtp)) %>% 
  group_by(military) %>% 
  summarize(estimate = sum(pwgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "Network2Work",
         question_equiv = "Military Service",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = military,
         estimate, total, percent, estimate_units)

dis_n2w <- pn2w %>% 
  mutate(disability = case_when(
    dis == 1 ~ "Disability",
    dratx == 1 ~ "Disability",
    TRUE ~ "None"),
    total = sum(pwgtp)) %>% 
  group_by(disability) %>% 
  summarize(estimate = sum(pwgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "Network2Work",
         question_equiv = "Disability",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = disability,
         estimate, total, percent, estimate_units)

lang_n2w <- pn2w %>% 
  mutate(language = case_when(
    lanx == 1 ~ "Other",
    lanx == 2 ~ "English"),
    total = sum(pwgtp)) %>% 
  group_by(language) %>% 
  summarize(estimate = sum(pwgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "Network2Work",
         question_equiv = "Primary Language",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = language,
         estimate, total, percent, estimate_units)

## children at home, single parent, snap ---- 
# from household, link to person level
child_n2w <- hn2w %>%
  mutate(numchild = case_when(
    nrc %in% c(0,1,2,3) ~ as.character(nrc),
    nrc > 3 ~ "4 or more",
    TRUE ~ as.character(0))) %>% 
  group_by(serialno) %>% 
  summarize(serialno = first(serialno),
            numchild = first(numchild)) %>% 
  ungroup() %>% 
  left_join(pn2w) %>% 
  mutate(total = sum(pwgtp)) %>% 
  group_by(numchild) %>%
  summarize(estimate = sum(pwgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "Network2Work",
         question_equiv = "Children at Home",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = numchild,
         estimate, total, percent, estimate_units)

# (hht2 == 3 is cohabitating and with children)
singleparent_n2w <- hn2w %>%
  mutate(singleparent = case_when(
    hht2 %in% c("06","10", "03") ~ "Single parent/guardian",
    TRUE ~ "No")) %>% 
  group_by(serialno) %>% 
  summarize(serialno = first(serialno),
            singleparent = first(singleparent)) %>% 
  ungroup() %>% 
  left_join(pn2w) %>% 
  mutate(total = sum(pwgtp)) %>% 
  group_by(singleparent) %>%
  summarize(estimate = sum(pwgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "Network2Work",
         question_equiv = "Single Parent",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = singleparent,
         estimate, total, percent, estimate_units)

# FS (food stamps or snap) household; 
# from household, link to person level
snap_n2w <- hn2w %>%
  mutate(snap = if_else(fs == 1, "SNAP", "No SNAP")) %>% 
  group_by(serialno) %>% 
  summarize(serialno = first(serialno),
            snap = first(snap)) %>% 
  ungroup() %>% 
  left_join(pn2w) %>% 
  mutate(total = sum(pwgtp)) %>% 
  group_by(snap) %>%
  summarize(estimate = sum(pwgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "Network2Work",
         question_equiv = "SNAP Recipients",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = snap,
         estimate, total, percent, estimate_units)

## Housing stability (move, crowd, burden) ----
# from household, link to person level
moved_n2w <- hn2w %>%
  mutate(move = if_else(mv == 1, "Moved within Year", "No Move within Year", "No Move within Year")) %>% 
  group_by(serialno) %>% 
  summarize(serialno = first(serialno),
            move = first(move)) %>% 
  ungroup() %>% 
  left_join(pn2w) %>% 
  mutate(total = sum(pwgtp)) %>% 
  group_by(move) %>%
  summarize(estimate = sum(pwgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "Network2Work",
         question_equiv = "Stable Housing",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = move,
         estimate, total, percent, estimate_units)

crowd_n2w <- hn2w %>% 
  mutate(per_bedroom = np / bdsp,
         per_room = np /rmsp,
         overcrowd = if_else(per_bedroom > 2 | per_room > 1, "Overcrowded", "Not Overcrowded", "Not Overcrowded")) %>%
  group_by(serialno) %>% 
  summarize(serialno = first(serialno),
            overcrowd = first(overcrowd)) %>% 
  ungroup() %>% 
  left_join(pn2w) %>% 
  mutate(total = sum(pwgtp)) %>% 
  group_by(overcrowd) %>%
  summarize(estimate = sum(pwgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "Network2Work",
         question_equiv = "Stable Housing",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = overcrowd,
         estimate, total, percent, estimate_units)

homeburden_n2w <- hn2w %>% 
  mutate(burden = case_when(
    grpip >= 50 ~ "Severely Burdened",
    grpip >= 30 ~ "Burdened",
    grpip < 30 ~ "Not Burdened",
    ocpip >= 50 ~ "Severely Burdened",
    ocpip >= 30 ~ "Burdened",
    ocpip < 30 ~ "Not Burdened",
    typehugq == 3 ~ "Not Burdened",
    TRUE ~ "Not Burdened")) %>%
  group_by(serialno) %>% 
  summarize(serialno = first(serialno),
            burden = first(burden)) %>% 
  ungroup() %>% 
  left_join(pn2w) %>% 
  mutate(total = sum(pwgtp)) %>% 
  group_by(burden) %>%
  summarize(estimate = sum(pwgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "Network2Work",
         question_equiv = "Stable Housing",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = burden,
         estimate, total, percent, estimate_units)

## Transportation (commute, commute_car, hh vehicle) ----
# person level
# means of commute (jwtrns, 01; 02-08; 09-12)
commute_n2w <- pn2w %>% 
  mutate(commute = case_when(
    jwtrns %in% c("01") ~ "Commute by Car",
    jwtrns %in% c("02", "03", "04", "05", "06", "07") ~ "Commute by Public Transportation",
    jwtrns %in% c("08", "09", "10", "11", "12") ~ "Commute by Motorcycle, Bike, Walk, Other")) %>% 
  filter(!is.na(commute)) %>% 
  mutate(total = sum(pwgtp)) %>% 
  group_by(commute) %>% 
  summarize(estimate = sum(pwgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "Network2Work",
         question_equiv = "Transportation",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = commute,
         estimate, total, percent, estimate_units)

# vehicle occupancy (jwrip, 1; 2-10)
commute_car_n2w <- pn2w %>% 
  mutate(commute = case_when(
    jwrip == 1 ~ "Commute by Car: Own Car",
    jwrip > 1 & jwrip <= 10 ~ "Commute by Car: Carpool")) %>% 
  filter(!is.na(commute)) %>% 
  mutate(total = sum(pwgtp)) %>% 
  group_by(commute) %>% 
  summarize(estimate = sum(pwgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "Network2Work",
         question_equiv = "Transportation",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = commute,
         estimate, total, percent, estimate_units)

# vehicle available (veh (HH), 0; 1-6)
# from household, link to person level
veh_n2w <- hn2w %>%
  mutate(veh = if_else(veh == 0, "Household has no Car", "Household has Car")) %>% 
  group_by(serialno) %>% 
  summarize(serialno = first(serialno),
            veh = first(veh)) %>% 
  ungroup() %>% 
  filter(!is.na(veh)) %>% 
  left_join(pn2w) %>% 
  mutate(total = sum(pwgtp)) %>% 
  group_by(veh) %>%
  summarize(estimate = sum(pwgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "Network2Work",
         question_equiv = "Transportation",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = veh,
         estimate, total, percent, estimate_units)

## Wages ----
wage_n2w <- pn2w %>% 
  mutate(wages = case_when(
    wagp < 10000 ~ "Less than $10,000",
    wagp < 15000 ~ "$10,000 to $14,999",
    wagp < 25000 ~ "$15,000 to $24,999",
    wagp < 35000 ~ "$25,000 to $34,999",
    wagp < 50000 ~ "$35,000 to $49,999",
    wagp < 75000 ~ "$50,000 to $74,999",
    wagp >= 75000 ~ "$75,000 or more"),
    total = sum(pwgtp)) %>% 
  group_by(wages) %>% 
  summarize(estimate = sum(pwgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "Network2Work",
         question_equiv = "Annual Personal Wages",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = wages,
         estimate, total, percent, estimate_units)


## Combine ----
n2w <- rbind(locality_n2w,
             age_n2w, race_n2w, hisp_n2w, sex_n2w,
             educ_n2w, work_n2w, mil_n2w, dis_n2w, lang_n2w,
             child_n2w, singleparent_n2w, snap_n2w,
             moved_n2w, crowd_n2w, homeburden_n2w,
             commute_n2w, commute_car_n2w, veh_n2w, wage_n2w)

# UW equivalent pop ---- 
# early age children at home, 
# family incomes of 160000 or below
## locality populations ----
# household estimates
# can't get incomes for this one; families with children < 6
# acs_subject <- load_variables(year = 2023, dataset = "acs5/subject")
region <-  c("540", "003", "065", "079", "109", "125")
var <- c(hhchild = "S1101_C01_005",
         u6only = "S1101_C01_006",
         u6and = "S1101_C01_007")

locality_uw <- get_acs(geography = "county",
                        state = "51",
                        county = region,
                        variables = var,
                        survey = "acs5",
                        year = 2023,
                        output = "wide")

locality_uw <- locality_uw %>% 
  mutate(hhchildu6 = hhchildE*((u6onlyE + u6andE)/100),
         total = sum(hhchildu6),
         percent = (hhchildu6 / total)*100,
         pop_equiv = "United Way",
         question_equiv = "Locality",
         estimate_units = "households") %>% 
  select(pop_equiv, question_equiv, group = NAME,
         estimate = hhchildu6, total, percent, estimate_units)

## household data
huw <- hcvl %>% 
  filter(fincp < 160000,
         fparc %in% c(1,3))

## person data 
puw <- pcvl %>% 
  filter(serialno %in% huw$serialno,
         agep >= 18)

## age, race, ethnicity, sex ----
# household level
age_uw <- huw %>% 
  mutate(age_bins = case_when(
    hhldragep < 19 ~ "18 and under",
    hhldragep >= 19 & hhldragep < 25 ~ "19-24",
    hhldragep >= 25 & hhldragep < 39 ~ "25-39",
    hhldragep >= 40 & hhldragep < 55 ~ "40-54",
    hhldragep >= 55 & hhldragep < 60 ~ "55-59",
    hhldragep >= 60 ~ "60+"),
    total = sum(wgtp)) %>% 
  group_by(age_bins) %>% 
  summarize(estimate = sum(wgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "United Way",
         question_equiv = "Age",
         estimate_units = "households") %>% 
  select(pop_equiv, question_equiv, group = age_bins,
         estimate, total, percent, estimate_units)

race_uw <- huw %>% 
  mutate(race = case_when(
    hhldrrac1p == 1 ~ "White",
    hhldrrac1p == 2 ~ "Black",
    hhldrrac1p == 6 ~ "Asian",
    hhldrrac1p == 7 ~ "Hawaiian/Pacific Islander",
    hhldrrac1p == 8 ~ "Some other race",
    hhldrrac1p == 9 ~ "Multiracial",
    hhldrrac1p %in% c(3,4,5) ~ "American Indian/Alaskan Native"),
    total = sum(wgtp)) %>% 
  group_by(race) %>% 
  summarize(estimate = sum(wgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "United Way",
         question_equiv = "Race",
         estimate_units = "households") %>% 
  select(pop_equiv, question_equiv, group = race,
         estimate, total, percent, estimate_units)

hisp_uw <- huw %>% 
  mutate(hispanic = if_else(hhldrhisp == "01", "Not Hispanic", "Hispanic"),
         total = sum(wgtp)) %>% 
  group_by(hispanic) %>% 
  summarize(estimate = sum(wgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "United Way",
         question_equiv = "Ethnicity",
         estimate_units = "households") %>% 
  select(pop_equiv, question_equiv, group = hispanic,
         estimate, total, percent, estimate_units)

# person level
sex_uw <- puw %>% 
  filter(agep >= 18, sch == 1) %>% 
  mutate(sex = if_else(sex == 1, "Male", "Female"),
         total = sum(pwgtp)) %>% 
  group_by(sex) %>% 
  summarize(estimate = sum(pwgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "United Way",
         question_equiv = "Sex",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = sex,
         estimate, total, percent, estimate_units)


## education, employment, veteran/military, disability, language ---- 
# person level
educ_uw <- puw %>% 
  filter(agep >= 18, sch == 1) %>% 
  mutate(educ = case_when(
    schl < 16 ~ "Did Not Complete High School",
    schl == 16 ~ "High School Graduate",
    schl == 17 ~ "GED",
    schl == 18 | schl == 19 ~ "Some College or Advanced Training",
    schl == 20 ~ "Associate's Degree",
    schl == 21 ~ "Bachelor's Degree",
    schl > 21 ~ "Graduate Degree"),
    total = sum(pwgtp)) %>% 
  group_by(educ) %>% 
  summarize(estimate = sum(pwgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "United Way",
         question_equiv = "Education Level",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = educ,
         estimate, total, percent, estimate_units)

work_uw <- puw %>% 
  filter(agep >= 18, sch == 1) %>% 
  mutate(working = case_when(
    esr %in% c(3,6) ~ "No",
    esr %in% c(1,2,4,5) ~ "Yes"),
    total = sum(pwgtp)) %>% 
  group_by(working) %>% 
  summarize(estimate = sum(pwgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "United Way",
         question_equiv = "Employment Status",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = working,
         estimate, total, percent, estimate_units)

mil_uw <- puw %>% 
  mutate(military = case_when(
    mil == 4 ~ "None",
    mil %in% c(1,2,3) ~ "U.S. Military Service",
    TRUE ~ "None")) %>% 
  group_by(serialno) %>% 
  summarize(military = if_else(
    any(military == "U.S. Military Service", na.rm = TRUE), 
    "U.S. Military Service", "None"
  )) %>% 
  left_join(huw, by = "serialno") %>% 
  mutate(total = sum(wgtp)) %>% 
  group_by(military) %>% 
  summarize(estimate = sum(wgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "United Way",
         question_equiv = "Military Service",
         estimate_units = "household") %>% 
  select(pop_equiv, question_equiv, group = military,
         estimate, total, percent, estimate_units)

dis_uw <- puw %>% 
  mutate(disability = case_when(
    dis == 1 ~ "Disability",
    dratx == 1 ~ "Disability",
    TRUE ~ "None")) %>% 
  group_by(serialno) %>% 
  summarize(disability = if_else(
    any(disability == "Disability", na.rm = TRUE), 
    "Disability", "None"
  )) %>% 
  left_join(huw, by = "serialno") %>% 
  mutate(total = sum(wgtp)) %>% 
  group_by(disability) %>% 
  summarize(estimate = sum(wgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "United Way",
         question_equiv = "Disability",
         estimate_units = "household") %>% 
  select(pop_equiv, question_equiv, group = disability,
         estimate, total, percent, estimate_units)

lang_uw <- puw %>% 
  mutate(language = case_when(
    lanx == 1 ~ "Other",
    lanx == 2 ~ "English")) %>% 
  group_by(serialno) %>% 
  summarize(language = if_else(
    any(language == "Other", na.rm = TRUE), 
    "Other", "English"
  )) %>% 
  left_join(huw, by = "serialno") %>% 
  mutate(total = sum(wgtp)) %>% 
  group_by(language) %>% 
  summarize(estimate = sum(wgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "United Way",
         question_equiv = "Primary Language",
         estimate_units = "households") %>% 
  select(pop_equiv, question_equiv, group = language,
         estimate, total, percent, estimate_units)

## children at home, single parent, snap ---- 
# household level
child_uw <- huw %>%
  mutate(numchild = case_when(
    nrc %in% c(0,1,2,3) ~ as.character(nrc),
    nrc > 3 ~ "4 or more",
    TRUE ~ as.character(0)),
    total = sum(wgtp)) %>% 
  group_by(numchild) %>%
  summarize(estimate = sum(wgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "United Way",
         question_equiv = "Children at Home",
         estimate_units = "households") %>% 
  select(pop_equiv, question_equiv, group = numchild,
         estimate, total, percent, estimate_units)

# (hht2 == 3 is cohabitating and with children)
singleparent_uw <- huw %>%
  mutate(singleparent = case_when(
    hht2 %in% c("06","10", "03") ~ "Single parent/guardian",
    TRUE ~ "No"),
    total = sum(wgtp)) %>% 
  group_by(singleparent) %>%
  summarize(estimate = sum(wgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "United Way",
         question_equiv = "Single Parent",
         estimate_units = "households") %>% 
  select(pop_equiv, question_equiv, group = singleparent,
         estimate, total, percent, estimate_units)

# FS (food stamps or snap) household 
snap_uw <- huw %>%
  mutate(snap = if_else(fs == 1, "SNAP", "No SNAP"),
         total = sum(wgtp)) %>% 
  group_by(snap) %>%
  summarize(estimate = sum(wgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "United Way",
         question_equiv = "SNAP Recipients",
         estimate_units = "households") %>% 
  select(pop_equiv, question_equiv, group = snap,
         estimate, total, percent, estimate_units)

## Housing stability ----
# household level
moved_uw <- huw %>%
  mutate(move = if_else(mv == 1, "Moved within Year", "No Move within Year", "No Move within Year"),
         total = sum(wgtp)) %>% 
  group_by(move) %>%
  summarize(estimate = sum(wgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "United Way",
         question_equiv = "Stable Housing",
         estimate_units = "households") %>% 
  select(pop_equiv, question_equiv, group = move,
         estimate, total, percent, estimate_units)

crowd_uw <- huw %>% 
  mutate(per_bedroom = np / bdsp,
         per_room = np /rmsp,
         overcrowd = if_else(per_bedroom > 2 | per_room > 1, "Overcrowded", "Not Overcrowded", "Not Overcrowded"),
         total = sum(wgtp)) %>%
  group_by(overcrowd) %>%
  summarize(estimate = sum(wgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "United Way",
         question_equiv = "Stable Housing",
         estimate_units = "households") %>% 
  select(pop_equiv, question_equiv, group = overcrowd,
         estimate, total, percent, estimate_units)

homeburden_uw <- huw %>% 
  mutate(burden = case_when(
    grpip >= 50 ~ "Severely Burdened",
    grpip >= 30 ~ "Burdened",
    grpip < 30 ~ "Not Burdened",
    ocpip >= 50 ~ "Severely Burdened",
    ocpip >= 30 ~ "Burdened",
    ocpip < 30 ~ "Not Burdened",
    typehugq == 3 ~ "Not Burdened",
    TRUE ~ "Not Burdened"),
    total = sum(wgtp)) %>%
  group_by(burden) %>%
  summarize(estimate = sum(wgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "United Way",
         question_equiv = "Stable Housing",
         estimate_units = "households") %>% 
  select(pop_equiv, question_equiv, group = burden,
         estimate, total, percent, estimate_units) 

## Transportation ----
# person level
# means of commute (jwtrns, 01; 02-08; 09-12)
commute_uw <- puw %>% 
  mutate(commute = case_when(
    jwtrns %in% c("01") ~ "Commute by Car",
    jwtrns %in% c("02", "03", "04", "05", "06", "07") ~ "Commute by Public Transportation",
    jwtrns %in% c("08", "09", "10", "11", "12") ~ "Commute by Motorcycle, Bike, Walk, Other")) %>% 
  filter(!is.na(commute)) %>% 
  mutate(total = sum(pwgtp)) %>% 
  group_by(commute) %>% 
  summarize(estimate = sum(pwgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "United Way",
         question_equiv = "Transportation",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = commute,
         estimate, total, percent, estimate_units) 

# vehicle occupancy (jwrip, 1; 2-10)
commute_car_uw <- puw %>% 
  mutate(commute = case_when(
    jwrip == 1 ~ "Commute by Car: Own Car",
    jwrip > 1 & jwrip <= 10 ~ "Commute by Car: Carpool")) %>% 
  filter(!is.na(commute)) %>% 
  mutate(total = sum(pwgtp)) %>% 
  group_by(commute) %>% 
  summarize(estimate = sum(pwgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "United Way",
         question_equiv = "Transportation",
         estimate_units = "persons") %>% 
  select(pop_equiv, question_equiv, group = commute,
         estimate, total, percent, estimate_units) 

# vehicle available (veh (HH), 0; 1-6)
# household level
veh_uw <- huw %>%
  mutate(veh = if_else(veh == 0, "Household has no Car", "Household has Car"),
         total = sum(wgtp)) %>% 
  group_by(veh) %>%
  summarize(estimate = sum(wgtp),
            total = first(total),
            percent = estimate / total * 100) %>% 
  mutate(pop_equiv = "United Way",
         question_equiv = "Transportation",
         estimate_units = "households") %>% 
  select(pop_equiv, question_equiv, group = veh,
         estimate, total, percent, estimate_units) 

## Combine ----
uw <- rbind(locality_uw,
             age_uw, race_uw, hisp_uw, sex_uw, 
             educ_uw, work_uw, mil_uw, dis_uw, lang_uw,
             child_uw, singleparent_uw, snap_uw,
             moved_uw, crowd_uw, homeburden_uw,
             commute_uw, commute_car_uw, veh_uw)


# Combine and save ----
pop_benchmark <- rbind(n2w, uw)
write_csv(pop_benchmark, "population_benchmarks.csv")

# Create Excel workbook ----
education_sheet <- rbind(educ_n2w, educ_uw)
disability_sheet <- rbind(dis_n2w, dis_uw)
military_sheet <- rbind(mil_n2w, mil_uw)
language_sheet <- rbind(lang_n2w, lang_uw)
children_sheet <- rbind(child_n2w, child_uw)
parent_sheet <- rbind(singleparent_n2w, singleparent_uw)
work_sheet <- rbind(work_n2w, work_uw)
benefits_sheet <- rbind(snap_n2w, snap_uw)
housing_sheet <- rbind(moved_n2w, crowd_n2w, homeburden_n2w, 
                       moved_uw, crowd_uw, homeburden_uw)
transportation_sheet <- rbind(commute_n2w, commute_car_n2w, veh_n2w, 
                              commute_uw, commute_car_uw, veh_uw)
demographics_sheet <- rbind(locality_n2w, locality_uw,
                            age_n2w, age_uw,
                            race_n2w, race_uw,
                            hisp_n2w, hisp_uw,
                            sex_n2w, sex_uw)

wb_sheets <- list("Education Level" = education_sheet,
                  "Disability" = disability_sheet,
                  "Military Service" = military_sheet,
                  "Primary Language" = language_sheet,
                  "Children living in home" = children_sheet,
                  "Single Parent" = parent_sheet,
                  "Employment Status" = work_sheet,
                  "Public Benefits & SNAP" = benefits_sheet,
                  "Annual Wages" = wage_n2w,
                  "Housing" = housing_sheet,
                  "Transportation needs" = transportation_sheet,
                  "Demographics" = demographics_sheet)

write.xlsx(wb_sheets, file = "CNA_PopulationBenchmark_Data.xlsx", keepNA = TRUE)


