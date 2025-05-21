# Community Needs Assessment
# Figures for final report

# Libraries ----
library(tidyverse)
library(scales)
library(patchwork)
library(ggthemes)

# Data ----
surv <- read_csv("cna_data_all.csv") # combined surveys
pops <- read_csv("population_benchmarks.csv") # population benchmarks

# Figures ----
# 1. Network2Work Income distribution-percent
wage <- surv %>% filter(str_detect(survey_question, "Wages")) %>% 
  mutate(data_point = factor(data_point),
         data_point = fct_shift(data_point, -1))
title <- wage$survey_question[1]

p1 <- wage %>% 
  ggplot(aes(x = data_point, y = round(survey_percent, 0), fill = survey, label = paste0(round(survey_percent, 0), "%") )) +  
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single"), width = 0.8,
           fill = few_pal(palette = "Medium")(3)[[2]]) +
  geom_text(position = position_dodge2(width = 0.9, preserve = "single"), 
            vjust = -0.5,
            size = 3) +
  scale_x_discrete(labels = scales::label_wrap(10),
                   name = "") +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.08)),
                     name = "% of Responses") +
  scale_fill_few() +
  labs(title = title) +
  coord_cartesian(expand=FALSE, clip="off") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.justification.top = "left")

p1

# 2. N2W % employed compared to pop benchmark; UW % employed compared to pop benchmark]
surv_emp <- surv %>% 
  filter(str_detect(survey_question, "Employment"),
         data_point == "Yes",
         survey != "All") %>% 
  mutate(survey_question = "Employment Status",
         source = "Intake Survey") %>% 
  rename(group = data_point, count = survey_count, 
         total = survey_total, percent = survey_percent)
pop_emp <- pops %>% 
  filter(str_detect(question_equiv, "Employment"),
         group == "Yes") %>% 
  mutate(source = "Population Benchmark") %>% 
  rename(survey = pop_equiv, survey_question = question_equiv,
         count = estimate)

emp <- bind_rows(surv_emp, pop_emp)

p2 <- emp %>% 
  ggplot(aes(x = survey, y = round(percent, 0), fill = source, label = paste0(round(percent, 0), "%") )) +  
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single"), width = 0.8) +
  scale_fill_manual(values = c("#5DA5DA", "grey50"), name = "") +
  geom_text(position = position_dodge2(width = 0.9, preserve = "single"), 
            vjust = -0.5,
            size = 3) +
  scale_x_discrete(labels = scales::label_wrap(10),
                   name = "") +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.08)),
                     name = "% of Responses") +
  labs(title = title) +
  coord_cartesian(expand=FALSE, clip="off") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.justification.top = "left")

p2

# 3. panel with N2W % housing unstable compared to benchmarks; panel with UW % housing unstable/moved combined compared to benchmarks
surv_house <- surv %>% 
  filter(str_detect(survey_question, "Housing"),
         survey != "All") %>% 
  mutate(data_point = if_else(
    data_point == "Prior instability and/or frequent movement", 
    "Unstable", data_point),
    source = "Intake Survey") %>% 
  group_by(survey, data_point) %>% 
  summarize(survey_question = first(survey_question),
            survey_count = sum(survey_count),
            survey_total = first(survey_total),
            survey_percent = sum(survey_percent)) %>% 
  rename(group = data_point, count = survey_count, 
         total = survey_total, percent = survey_percent) %>% 
  filter(group == "Unstable") %>% 
  mutate(group = "Unstable Housing", source = "Intake Survey")

pop_house <- pops %>% 
  filter(str_detect(question_equiv, "Housing")) %>% 
  mutate(source = "Population Benchmark") %>% 
  rename(survey = pop_equiv, survey_question = question_equiv,
         count = estimate) %>% 
  filter(group %in% c("Moved within Year", "Overcrowded", "Burdened", "Severely Burdened")) %>% 
  mutate(group = if_else(group == "Burdened", "Rent Burdened", group),
    group = if_else(group == "Severely Burdened", "Rent Burdened", group)) %>% 
  group_by(survey, survey_question, group, source, estimate_units) %>% 
  summarize(count = sum(count), total = first(total), percent = sum(percent))

house <- bind_rows(surv_house, pop_house) %>% 
  mutate(group = factor(group, 
                        levels = c("Unstable Housing", "Overcrowded",
                                   "Moved within Year", "Rent Burdened")))

house %>% 
  ggplot(aes(x = group, y = round(percent, 0), fill = source, label = paste0(round(percent, 0), "%") )) +  
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single"), width = 0.8) +
  scale_fill_manual(values = c("#5DA5DA", "grey50"), name = "") +
  geom_text(position = position_dodge2(width = 0.9, preserve = "single"), 
            vjust = -0.5,
            size = 3) +
  scale_x_discrete(labels = scales::label_wrap(10),
                   name = "") +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.08)),
                     name = "% of Responses") +
  labs(title = title) +
  coord_cartesian(expand=FALSE, clip="off") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.justification.top = "left") +
  facet_wrap(~survey)


# 4. panel of % N2W transportation needs; panel of % UW transportation needs
tran1 <- surv %>% filter(survey == "Network2Work" & str_detect(survey_question, "Transportation")) %>% 
  mutate(data_point = factor(data_point, 
                             levels = c("None", "Does not have reliable transportation", "Have car access, not reliable or registered", "Does not have driver’s license")))
title <- tran1$survey_question[1]

p3 <- tran1 %>% 
  ggplot(aes(x = data_point, y = round(survey_percent, 0), label = paste0(round(survey_percent, 0), "%") )) +  
  geom_col(fill = few_pal(palette = "Medium")(3)[[2]]) +
  geom_text(vjust = -0.5, size = 4.5) +
  scale_x_discrete(labels = scales::label_wrap(10),
                   name = "") +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.08)),
                     name = "% of Job-Seekers") +
  labs(title = title) +
  coord_cartesian(expand=FALSE, clip="off") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.justification.top = "left")

tran2 <- surv %>% filter(survey == "United Way" & str_detect(survey_question, "Transportation")) %>% 
  mutate(data_point = factor(data_point, 
                             levels = c("None", "Does not have reliable transportation", "Does not have driver’s license", "Transportation is a barrier to getting child to and from school", "Works longer hours than school day")))
title <- tran2$survey_question[1]

p4 <- tran2 %>% 
  ggplot(aes(x = data_point, y = round(survey_percent, 0), label = paste0(round(survey_percent, 0), "%") )) +  
  geom_col(fill = few_pal(palette = "Medium")(3)[[3]]) +
  geom_text(vjust = -0.5, size = 4.5) +
  scale_x_discrete(labels = scales::label_wrap(10),
                   name = "") +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.08)),
                     name = "% of Child-Care-Seekers") +
  labs(title = title) +
  coord_cartesian(expand=FALSE, clip="off") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.justification.top = "left")

p3 + p4

# A1. panel of UW % locality compared to benchmark; panel of N2W % locality compared to benchmark


# A2. panel of UW % age compared to benchmark; panel of N2W % age compared to benchmark


# A3/A4. panel of UW % race and ethnicity compared to benchmark; panel of N2W % race and ethnicity compared to benchmark


# A5. panel of UW % education compared to benchmark; panel of N2W % education compared to benchmark
