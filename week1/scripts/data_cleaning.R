# 3.  S T R I N G S

penguins_clean_names <- readRDS(url("https://github.com/UEABIO/5023B/raw/refs/heads/2026/files/penguins.RDS"))

library(tidyverse)

# 3.3 Basic string manipulation
  
  # 3.3.1 Trim
    # trim white space on either side of a string
str_trim(" Adelie Penguin (Pygoscelis adeliae) ")

    # trim white space on one side of a string
str_trim("  Adelie Penguin (Pygoscelis adeliae)  ", side = "left")

  # 3.3.2 Squish
    # remove leading, trailing, and extra internal white space, leaving only single spaces between words
str_squish("  Adelie    Penguin   (Pygoscelis   adeliae)  ")

  # 3.3.3 Truncate
    # shorten long strings to a specific width
str_trunc("Adelie Penguin (Pygoscelis adeliae)", width = 18, side = "right")

  # 3.3.4 Split
    # split a string into smaller pieces based on a separator
str_split("Adelie Penguin (Pygoscelis adeliae)", " ")
  
  # 3.3.5 Concatenate
    # join pieces of text into one string
str_c("Adelie", "Penguin", sep = "_")

# 3.4 Cleaning strings with dplyr
  # print only unique character strings in this variable
penguins_clean_names |>  
  distinct(sex)

# 3.5 Conditional changes with case_when() and if_else()

  # 3.5.1 case_when()
    # changing text or category labels in data based on certain conditions
    # use mutate and case_when for multiple conditions with different outcomes for each / a tidy way to handle many possible categories
penguins_clean_names |> 
  mutate(species = case_when(
    species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
    species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo",
    species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
    .default = as.character(species)
  )
  )

  # 3.5.2 if_when()
    # use mutate and if_else for two-way decisions (true or false)
penguins_clean_names |> 
  mutate(sex = if_else(
    sex == "MALE", "Male", "Female"
  )
  )

# 3.6 Rename text values with stringr
  # use mutate and case_when for a statement that conditionally changes the names of the values in a variable
penguins_clean_names |> 
  mutate(species = stringr::word(species, 1)
  ) |> 
  mutate(sex = stringr::str_to_title(sex))

  # convert all species names to uppercase
str_to_upper("Adelie")
str_to_upper("Gentoo")
str_to_upper("Chinstrap")

# 3.7 Split columns (!!!!)
penguins_clean_names |> 
  separate(
    species,
    into = c("species", "full_latin_name"),
    sep = "(?=\\()"
  ) 
# 3.8 Matching

  # 3.8.1 detect a pattern
str_detect("Genus specificus", "Genus")

  # 3 possible names in species column
penguins_clean_names |> distinct(species)

  #filter species names to only those containing the pattern "papua" (!!!!)
penguins_clean_names |>
  filter(str_detect(species, "papua")) |>
  select(species)

  # 3.8.2 Remove a pattern (remove match for Genus followed by a white space)
str_remove("Genus specificus", pattern = "Genus ")

  # remove brackets (!!!!)
penguins_clean_names |> 
  separate(
    species,
    into = c("species", "full_latin_name"),
    sep = "(?=\\()" # regex pattern: split before the '('
  ) |> 
  mutate(full_latin_name = str_remove_all(full_latin_name, "[\\(\\)]"))

# 4.  D u p l i c a t e s

  # 4.1 Duplicated rows
    # check for whole duplicate rows in the data
    # dplyr
penguins_clean_names |> 
  filter(duplicated(across(everything())))
sum() 

    # janitor
penguins_clean_names |> 
  get_dupes()

  # 4.1.1 Working with duplications
    # adding duplications
penguins_demo <- penguins_clean_names |> 
  slice(1:50) |> 
  bind_rows(slice(penguins_clean_names, c(1,5,10,15,30)))

penguins_demo |> 
  filter(duplicated(across(everything())))
sum()

      # removing duplications (keep only unduplicated data with !)
penguins_demo |> 
  filter(!duplicated(across(everything())))

penguins_demo |> 
  distinct()
  
  # 4.1.2 Counting unique entries using n_distinct()
penguins_clean_names |> 
  summarise(
    n = n(),
    n_distinct(individual_id)
  )

# 5.  M I S S I N G   D A T A

  # 5.2 Finding missing values
    # summary()
summary(penguins_clean_names)

    # skim()
skimr::skim(penguins_clean_names)

    # vis_miss()
naniar::vis_miss(penguins_clean_names)

    # upset_plot()
naniar::gg_miss_upset(penguins_clean_names)

  # return all rows with a missing variable
penguins_clean_names |> 
  filter(if_any(everything(), is.na)) |>
  select(culmen_length_mm, culmen_depth_mm, flipper_length_mm, 
         sex, delta_15n, delta_13c,comments,
         everything()) # reorder columns

  # renaming variables
penguins_clean_names <- penguins_clean_names %>%
  rename(
    delta_15n = delta_15_n_o_oo
  )
penguins_clean_names <- penguins_clean_names %>%
  rename(
    delta_13c = delta_13n
  )

  # look at specific columns (NA)
penguins_clean_names |> 
  filter(if_any(culmen_length_mm, is.na))  # reorder columns

  # 5.3 Remove missing values

  # 5.3.1 drop_na() on everything
penguins_clean_names |> 
  drop_na()

  # 5.3.2 drop_na() on a particular variable

  # 5.3.3 Use arguments inside functions
penguins_clean_names |> 
  group_by(species) |> 
  summarise(
    mean_body_mass = mean(body_mass_g, na.rm = T)
  )

# 6.  D A T E S

  # 6.1 Reformat
    # use functions in the lubridate package to reformat dates to the YYYY-MM-DD format
date("2017-10-11T14:02:00")
dmy("11 October 2020")
mdy("10/11/2020")

df <- tibble(
  date = c("X2020.01.22",
           "X2020.01.22",
           "X2020.01.22",
           "X2020.01.22")
)

df |> 
  mutate(
    date = as_date(date)
  )

  # use % to specifcy where each part of the date is in the date column
df |> 
  mutate(
    date = as_date(date, format = "X%Y.%m.%d")
  )

  # 6.2 Extract
year("2017-11-28T14:02:00")
month("2017-11-28T14:02:00")
week("2017-11-28T14:02:00")
day("2017-11-28T14:02:00")

  # Excel date formats
excel_numeric_to_date(42370)

    # 6.3.1 Calculations with dates
penguins_clean_names |> 
  summarise(min_date=min(date_egg),
            max_date=max(date_egg))

penguins_clean_names <- penguins_clean_names |> 
  mutate(year = lubridate::year(date_egg))

    # 6.3.2 Filter dates
# return records after 2008
penguins_clean_names |>
  filter(date_egg >= ymd("2008-01-01"))

# 7.  C H E C K   D A T A   C O N S I S T E N C Y

  # 7.0.2 Basic range checks
    # Check ranges of all numeric variables at once
penguins_clean_names |> 
  summarise(across(where(is.numeric), 
                   list(min = ~min(., na.rm = TRUE),
                        max = ~max(., na.rm = TRUE))))
    # Check body mass range
penguins_clean_names |> 
  summarise(
    min_mass = min(body_mass_g, na.rm = TRUE),
    max_mass = max(body_mass_g, na.rm = TRUE)
  )

  # 7.0.3
    # Check for negative values (impossible for mass, length measurements)
penguins_clean_names |> 
  filter(if_any(c(body_mass_g, flipper_length_mm, 
                  culmen_length_mm, culmen_depth_mm), 
                ~ . < 0))
  # Check for zero or negative values where zero doesn't make biological sense
penguins_clean_names |> 
  filter(body_mass_g <= 0)

  # 7.0.4 Species-specific checks
    # Body mass ranges by species
penguins_clean_names |> 
  group_by(species) |> 
  summarise(
    min_mass = min(body_mass_g, na.rm = TRUE),
    max_mass = max(body_mass_g, na.rm = TRUE),
    mean_mass = mean(body_mass_g, na.rm = TRUE)
  )

  # Find Adelie penguins with Gentoo-sized body mass
penguins_clean_names |> 
  filter(species == "Adelie Penguin (Pygoscelis adeliae)", body_mass_g > 4750)

  # 7.0.5 Visual inspection
penguins_clean_names |> 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  labs(
    x = "",
    y = "Body mass (g)"
  ) +
  theme_minimal()+
  coord_flip()

  # 7.0.6 Cross-variable checks: Expected correlations
#| label: fig-mass-flipper
#| fig-cap: "Body mass should generally increase with flipper length within species. Points far from the trend may indicate measurement errors."

penguins_clean_names |> 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(alpha = 0.6) +
  labs(
    x = "Flipper length (mm)",
    y = "Body mass (g)",
  ) +
  theme_minimal()

  # Find penguins with large flippers but low body mass
penguins_clean_names |> 
  filter(flipper_length_mm > 210, body_mass_g < 3500) |> 
  select(species, sex, flipper_length_mm, body_mass_g, island)

  # Find penguins with small flippers but high body mass
penguins_clean_names |> 
  filter(flipper_length_mm < 185, body_mass_g > 4500) |> 
  select(species, sex, flipper_length_mm, body_mass_g, island)

  # 7.0.7 Cross-variable checks: Biological impossibilities
    # Males cannot lay eggs
penguins_clean_names |> 
  filter(sex == "MALE", !is.na(date_egg)) |> 
  select(species, sex, date_egg, body_mass_g, island)

  # 7.0.8 Cross-variable checks: Spatial consistency
    # Check which species appear on which islands
penguins_clean_names |> 
  count(species, island) |> 
  pivot_wider(names_from = island, values_from = n, values_fill = 0)

  # 7.0.9 Flagging suspicious values
    # Create flags for different types of potential issues
penguins_flagged <- penguins_clean_names |> 
  mutate(
    # Single-variable flags
    flag_impossible = case_when(
      body_mass_g <= 0 ~ "negative_or_zero_mass",
      flipper_length_mm <= 0 ~ "negative_or_zero_flipper",
      TRUE ~ NA_character_
    ),
    flag_implausible = case_when(
      body_mass_g < 2000 ~ "suspiciously_light",
      body_mass_g > 7000 ~ "suspiciously_heavy",
      TRUE ~ NA_character_
    ),
    
    # Cross-variable flags
    flag_species_size = case_when(
      species == "Adelie" & body_mass_g > 5000 ~ "Adelie_too_heavy",
      species == "Gentoo" & body_mass_g < 4000 ~ "Gentoo_too_light",
      TRUE ~ NA_character_
    ),
    # Any flag present?
    any_flag = !is.na(flag_impossible) | !is.na(flag_implausible) | 
      !is.na(flag_species_size) 
  )

  # Summarize flagged observations
penguins_flagged |> 
  summarise(
    n_impossible = sum(!is.na(flag_impossible)),
    n_implausible = sum(!is.na(flag_implausible)),
    n_species_size = sum(!is.na(flag_species_size)),
    total_flagged = sum(any_flag)
  )
