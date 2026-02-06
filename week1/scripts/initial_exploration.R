# Week 1: Initial Data Exploration ====
# Author: [Teagan Pearse]
# Date: [30th Jan 2026]

# Load packages ====
library(tidyverse)
library(here)
library(naniar)
library(janitor)
library(skimr)
# Load data ====
mosquito_egg_raw <- read_csv(here("week1/data", "mosquito_egg_data.csv"),
                             name_repair = janitor::make_clean_names)

# Basic overview ====
glimpse(mosquito_egg_raw)
summary(mosquito_egg_raw)
skim(mosquito_egg_raw)

# React table====
# view interactive table of data
view(mosquito_egg_raw)


# Counts by site and treatment====

mosquito_egg_raw |> 
  group_by(site, treatment) |> 
  summarise(n = n())


# W E E K   2  - fixes
# FIX 1: [capitalisation inconsistencies and typos in treatment and site variables] ====

# Show the problem:
mosquito_egg_data|>  
  distinct(treatment)

mosquito_egg_data|>  
  distinct(site)

# Fix it:
mosquito_egg_data_step1 <- mosquito_egg_data |> 
  mutate(treatment = case_when(
    treatment == "Medium_dose" ~ "medium_dose",
    treatment == "High_dose" ~ "high_dose",
    treatment == "Low_dose" ~ "low_dose",
    treatment == "Control" ~ "control",
    treatment == "MEDIUM_DOSE" ~ "medium_dose",
    treatment == "HIGH_DOSE" ~ "high_dose",
    treatment == "LOW_DOSE" ~ "low_dose",
    treatment == "CONTROL" ~ "control",
    .default = as.character(treatment)
  )
  )
  
mosquito_egg_data_step2 <- mosquito_egg_data_step1 |> 
  mutate(site = case_when(
    site == "Site_A" ~ "site_a",
    site == "Site-A" ~ "site_a",
    site == "Site A" ~ "site_a",
    site == "Site B" ~ "site_b",
    site == "Site_B" ~ "site_b",
    site == "Site-B" ~ "site_b",
    site == "Site_C" ~ "site_c",
    site == "Site-C" ~ "site_c",
    site == "Site C" ~ "site_c",
    .default = as.character(site)
  )
  )
  
  # Verify it worked:
  view(mosquito_egg_data_step1)
  
  view(mosquito_egg_data_step2)
  
  # What changed and why it matters:
  # [2-3 sentences explaining consequences]
  #
  
  
  # FIX 2: [Biological inconsistencies in body mass variable]  ====

# Show the problem:
# [Code]

# Fix it:
mosquito_egg_data_step3 <- mosquito_egg_data_step2 |>
  # YOUR CODE
  
  
  # Verify it worked:
  # [Code]
  
  # What changed and why it matters:
  # [2-3 sentences]
  #



  