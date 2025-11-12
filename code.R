# Load tidyverse for data manipulation
library(tidyverse)

# Read in the CSV files
combine <- read_csv("combine.csv")
draft <- read_csv("draft.csv")

# Merge on playerId (keeping all combine players)
merged <- combine %>%
  left_join(draft %>% select(playerId, round, draftTeam, pick), by = "playerId")

# Create binary drafted variable (1 = drafted, 0 = not drafted)
merged <- merged %>%
  mutate(drafted = if_else(!is.na(round), 1, 0))

# Keep only data from year 2000 and beyond
nfl <- merged %>%
  filter(combineYear >= 2000)
range(nfl$combineYear, na.rm = TRUE)
nrow(nfl)

summary(nfl)
table(nfl$homeState)

table(nfl$draftTeam)

# cleaning the draftTeam column
nfl <- nfl %>%
  mutate(
    draftTeam = case_when(
      draftTeam == "ARI" ~ "ARZ",
      draftTeam == "BAL" ~ "BLT",
      draftTeam == "CLE" ~ "CLV",
      draftTeam == "HOU" ~ "HST",
      draftTeam == "LA" ~ "LAR",
      draftTeam == "SD" ~ "LAC",
      draftTeam == "SL" ~ "LAR",
      TRUE ~ draftTeam))

table(nfl$draftTeam)

# creating a regions column
merged <- merged %>%
  mutate(
    region = case_when(
      homeState %in% c() ~ "Midwest",
      homeState %in% c() ~ "Northeast",
      homeState %in% c() ~ "South",
      homeState %in% c() ~ "West",
      TRUE ~ "Other"  # fallback for any states not matched
    )
  )