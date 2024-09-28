# Income by county
# 2024-09-28 mpc

# Setup ----
library(tidyverse)
library(tidycensus)
library(viridis)

# Get data ---- 
region <- c("003", "540", "065", "079", "109", "125")

## ACS data ---- 
income_county <- get_acs(geography = "county",
                      state = "51",
                      county = region,
                      table = "B19001",
                      summary_var = "B19001_001",
                      year = 2022,
                      #geometry = TRUE,
                      survey = "acs5")

fam_income_county <- get_acs(geography = "county",
                         state = "51",
                         county = region,
                         table = "B19101",
                         summary_var = "B19101_001",
                         year = 2022,
                         #geometry = TRUE,
                         survey = "acs5")

# Prep data ---- 
income_county <- income_county %>% 
  filter(variable != "B19001_001") %>% 
  mutate(percent = (estimate/summary_est)*100,
    income_bin = case_when(
    variable == "B19001_002" ~ "Less than $10,000",
    variable == "B19001_003" ~ "$10,000 to $14,999",
    variable == "B19001_004" ~ "$15,000 to $19,999",
    variable == "B19001_005" ~ "$20,000 to $24,999",
    variable == "B19001_006" ~ "$25,000 to $29,999",
    variable == "B19001_007" ~ "$30,000 to $34,999",
    variable == "B19001_008" ~ "$35,000 to $39,999",
    variable == "B19001_009" ~ "$40,000 to $44,999",
    variable == "B19001_010" ~ "$45,000 to $49,999",
    variable == "B19001_011" ~ "$50,000 to $59,999",
    variable == "B19001_012" ~ "$60,000 to $74,999",
    variable == "B19001_013" ~ "$75,000 to $99,999",
    variable == "B19001_014" ~ "$100,000 to $124,999",
    variable == "B19001_015" ~ "$125,000 to $149,999",
    variable == "B19001_016" ~ "$150,000 to $199,999",
    variable == "B19001_017" ~ "$200,000 or more"
  ),
  income_bin = factor(income_bin,
                      levels = c("Less than $10,000", "$10,000 to $14,999",
                                 "$15,000 to $19,999", "$20,000 to $24,999",
                                 "$25,000 to $29,999", "$30,000 to $34,999",
                                 "$35,000 to $39,999", "$40,000 to $44,999",
                                 "$45,000 to $49,999", "$50,000 to $59,999",
                                 "$60,000 to $74,999", "$75,000 to $99,999",
                                 "$100,000 to $124,999","$125,000 to $149,999",
                                 "$150,000 to $199,999", "$200,000 or more")))

fam_income_county <- fam_income_county %>% 
  filter(variable != "B19101_001") %>% 
  mutate(percent = (estimate/summary_est)*100,
    income_bin = case_when(
    variable == "B19101_002" ~ "Less than $10,000",
    variable == "B19101_003" ~ "$10,000 to $14,999",
    variable == "B19101_004" ~ "$15,000 to $19,999",
    variable == "B19101_005" ~ "$20,000 to $24,999",
    variable == "B19101_006" ~ "$25,000 to $29,999",
    variable == "B19101_007" ~ "$30,000 to $34,999",
    variable == "B19101_008" ~ "$35,000 to $39,999",
    variable == "B19101_009" ~ "$40,000 to $44,999",
    variable == "B19101_010" ~ "$45,000 to $49,999",
    variable == "B19101_011" ~ "$50,000 to $59,999",
    variable == "B19101_012" ~ "$60,000 to $74,999",
    variable == "B19101_013" ~ "$75,000 to $99,999",
    variable == "B19101_014" ~ "$100,000 to $124,999",
    variable == "B19101_015" ~ "$125,000 to $149,999",
    variable == "B19101_016" ~ "$150,000 to $199,999",
    variable == "B19101_017" ~ "$200,000 or more"
  ),
  income_bin = factor(income_bin,
                      levels = c("Less than $10,000", "$10,000 to $14,999",
                                 "$15,000 to $19,999", "$20,000 to $24,999",
                                 "$25,000 to $29,999", "$30,000 to $34,999",
                                 "$35,000 to $39,999", "$40,000 to $44,999",
                                 "$45,000 to $49,999", "$50,000 to $59,999",
                                 "$60,000 to $74,999", "$75,000 to $99,999",
                                 "$100,000 to $124,999","$125,000 to $149,999",
                                 "$150,000 to $199,999", "$200,000 or more")))

# Visualize ---- 
ggplot(income_county) +
  geom_col(aes(x = fct_rev(income_bin), y = percent, fill = income_bin)) +
  scale_fill_viridis_d(direction = -1) +
  guides(fill = FALSE) + 
  coord_flip() +
  facet_wrap(~NAME) +
  labs(x = "", y = "Percent of Population", 
       title = "Population (%) by Income Range")

ggplot(fam_income_county) +
  geom_col(aes(x = fct_rev(income_bin), y = percent, fill = income_bin)) +
  scale_fill_viridis_d(direction = -1) +
  guides(fill = FALSE) + 
  coord_flip() +
  facet_wrap(~NAME) +
  labs(x = "", y = "Percent of Families", 
       title = "Families (%) by Income Range")

# Save ---- 
save(income_county, fam_income_county, file = "income.RData")
