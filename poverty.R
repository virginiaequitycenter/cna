# Poverty by block group, tract, county
# 2024-09-28 mpc

# Setup ----
library(tidyverse)
library(tidycensus)
library(sf)
library(leaflet)
library(tigris)
library(viridis)

# Get data ---- 
region <- c("003", "540", "065", "079", "109", "125")

## ACS data ---- 
pov_blkgp <- get_acs(geography = "cbg",
                     state = "51",
                     county = region,
                     variable = c("C17002_002", "C17002_003"),
                     summary_var = "C17002_001",
                     year = 2022,
                     #geometry = TRUE,
                     survey = "acs5")

pov_tract <- get_acs(geography = "tract",
                     state = "51",
                     county = region,
                     variable = "B17001_002",
                     summary_var = "B17001_001",
                     year = 2022,
                     #geometry = TRUE,
                     survey = "acs5")

pov_county <- get_acs(geography = "county",
                     state = "51",
                     county = region,
                     variable = "B17001_002",
                     summary_var = "B17001_001",
                     year = 2022,
                     #geometry = TRUE,
                     survey = "acs5")

## Geometry ----
blkgp <- block_groups(state = "51", county = region, cb = TRUE, year = 2022)
tract <- tracts(state = "51", county = region, cb = TRUE, year = 2022)
county <- counties(state = "51", cb = TRUE, year = 2022)
county <- county %>% 
  filter(COUNTYFP %in% region)

## Names ----
# Option to pull from google sheet
# gs_auth(new_user = TRUE)
googlesheets4::gs4_deauth()
tractname_sheet <- "https://docs.google.com/spreadsheets/d/19wl75rrOBjEqiQKMB38RSz3G9bGHxXbNUUK3x1iWfKk/edit#gid=0"
tractnames <- googlesheets4::read_sheet(tractname_sheet, sheet = "Sheet1") 


# Prep data ---- 
# Clean up tractnames
tractnames <- tractnames %>%
  rename(count = locality,
         locality = locality_num)
tractnames$GEOID <- as.character(tractnames$GEOID)

pov_blkgp <- pov_blkgp %>% 
  group_by(GEOID) %>% 
  summarize(estimate = sum(estimate),
            moe =  moe_sum(moe, estimate),
            denom = first(summary_est),
            denom_moe = first(summary_moe)) %>% 
  mutate(percent = (estimate/denom)*100,
         percent = round(percent, 1),
         percent_moe = moe_prop(estimate, denom, moe, denom_moe),
         percent_moe = percent_moe*100,
         percent = ifelse(percent_moe > 100, NA_real_, percent),
         estimate = ifelse(percent_moe > 100, NA_integer_, estimate))

pov_tract <- pov_tract %>% 
  mutate(percent = (estimate/summary_est)*100,
         percent = round(percent,1),
         percent_moe = moe_prop(estimate, summary_est, moe, summary_moe),
         percent_moe = percent_moe*100) %>% 
  select(GEOID, estimate, moe, denom = summary_est, denom_moe = summary_moe, 
         percent, percent_moe)
# add tract names 
pov_tract <- pov_tract %>% 
  left_join(tractnames, by = "GEOID")

pov_county <- pov_county %>% 
  mutate(percent = (estimate/summary_est)*100,
         percent = round(percent,1),
         percent_moe = moe_prop(estimate, summary_est, moe, summary_moe),
         percent_moe = percent_moe*100) %>% 
  select(GEOID, estimate, moe, denom = summary_est, denom_moe = summary_moe, 
         percent, percent_moe, name = NAME)




# Map data ----
pov_blkgp_sf <- left_join(blkgp, pov_blkgp)
pov_tract_sf <- left_join(tract, pov_tract)
pov_county_sf <- left_join(county, pov_county)

summary(pov_blkgp$percent)
summary(pov_tract$percent)
summary(pov_county$percent)

pov_blkgp_sf <- st_transform(pov_blkgp_sf, crs = 4326)
pov_tract_sf <- st_transform(pov_tract_sf, crs = 4326)
pov_county_sf <- st_transform(pov_county_sf, crs = 4326)

# pal_vector <- c("#EE6400","#F37903","#F78C05","#FCA108","#E7AD1B","#ABB046","#6FB272","#33B59D","#2DB0AC","#4CA7A9")
# pal <- colorBin(pal_vector, domain = pov_blkgp_sf$percent)
pal <- colorBin(viridis_pal(option = "C")(10), domain = pov_blkgp_sf$percent)

## Block group ----
leaflet() %>% 
  leaflet(width = "100%") %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = pov_blkgp_sf,
              fillColor = ~pal(percent),
              weight = 1,
              opacity = 1,
              color = "white", 
              fillOpacity = 0.6,
              smoothFactor = 0.3,
              highlight = highlightOptions(
                weight = 1, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("Percent below Poverty: ", pov_blkgp_sf$percent, "%", "<br>",
                             "Census Block Group: ", pov_blkgp_sf$GEOID)) %>% 
  addLegend("bottomright", pal = pal, values = pov_blkgp_sf$percent, 
            title = "Percent below Poverty", opacity = 0.7)

## Tract ----
leaflet() %>% 
  leaflet(width = "100%") %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = pov_tract_sf,
              fillColor = ~pal(percent),
              weight = 1,
              opacity = 1,
              color = "white", 
              fillOpacity = 0.6,
              smoothFactor = 0.3,
              highlight = highlightOptions(
                weight = 1, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("Percent below Poverty: ", pov_tract_sf$percent, "%", "<br>",
                             "Name of Area: ", pov_tract$tractnames, "<br>",
                             "Census Tract: ", pov_tract_sf$GEOID)) %>% 
  addLegend("bottomright", pal = pal, values = pov_tract_sf$percent, 
            title = "Percent below Poverty", opacity = 0.7)

## County ----
leaflet() %>% 
  leaflet(width = "100%") %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = pov_county_sf,
              fillColor = ~pal(percent),
              weight = 1,
              opacity = 1,
              color = "white", 
              fillOpacity = 0.6,
              smoothFactor = 0.3,
              highlight = highlightOptions(
                weight = 1, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("Percent below Poverty: ", pov_county_sf$percent, "%", "<br>",
                             "County: ", pov_county_sf$name)) %>% 
  addLegend("bottomright", pal = pal, values = pov_county_sf$percent, 
            title = "Percent below Poverty", opacity = 0.7)

# Save ---- 
save(pov_blkgp_sf, pov_tract_sf, pov_county_sf, file = "poverty.RData")
