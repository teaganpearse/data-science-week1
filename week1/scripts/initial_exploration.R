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
mosquito_egg_data <- read_csv(here("week1/data", "mosquito_egg_data.csv"),
                             name_repair = janitor::make_clean_names)

# Basic overview ====
glimpse(mosquito_egg_data)
summary(mosquito_egg_data)
skim(mosquito_egg_data)

# React table====
# view interactive table of data
view(mosquito_egg_data)


# Counts by site and treatment====

mosquito_egg_data |> 
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
  
  # What changed and why it matters: [all text in treatment and site variables are in lowercase. fixing inconsistencies ensures comparisons are accurate and keeps data reliable]
  
  
  # FIX 2: [Biological inconsistencies in body mass variable]  ====

# Show the problem:
  # Check body mass range
  mosquito_egg_data_step2 |> 
    summarise(
      min_mass = min(body_mass_mg, na.rm = TRUE),
      max_mass = max(body_mass_mg, na.rm = TRUE)
    )

  # Check for zero or negative values where zero doesn't make biological sense
  mosquito_egg_data_step2 |> 
    filter(body_mass_mg <= 0)

  # Fix it:
mosquito_flagged <- mosquito_egg_data_step2|>
  mutate(
    flag_impossible = case_when(
      body_mass_mg <= 0 ~ "negative_or_zero_mass",
      TRUE ~ NA_character_
    ),
    flag_implausible = case_when(
      body_mass_mg < -93.0 ~ "impossible_weight",
      body_mass_mg < -87.2 ~ "impossible_weight",
      body_mass_mg < -56.8 ~ "impossible_weight",
      TRUE ~ NA_character_
    ),flag_id_weight = case_when(
      female_id == "184" & body_mass_mg < -93.0 ~ "184_impossible_weight",
      female_id == "109" & body_mass_mg < -87.2 ~ "109_impossible_weight",
      female_id == "98" & body_mass_mg < -56.8 ~ "98_impossible_weight",
      TRUE ~ NA_character_
    ),
    any_flag = !is.na(flag_impossible) | !is.na(flag_implausible) | !is.na(flag_id_weight)
  )


  

  # Verify it worked:
    # summarize flagged observations
mosquito_flagged |> 
  summarise(
    n_impossible = sum(!is.na(flag_impossible)),
    n_implausible = sum(!is.na(flag_implausible)),
    n_id_weight = sum(!is.na(flag_id_weight)),
    total_flagged = sum(any_flag)
  )
  
view(mosquito_flagged)

  # What changed and why it matters: [impossible weight variables are now flagged. flagging impossible values is important for identifying errors that could distort results and lead to inconsistencies]