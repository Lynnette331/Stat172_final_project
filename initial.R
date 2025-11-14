

rm(list = ls())

# Load tidyverse for data manipulation

library(tidyverse)
library(dplyr)

# Read in the CSV files
combine <- read_csv("combine.csv")
draft <- read_csv("draft.csv")

# Merge on playerId (keeping all combine players)
merged <- combine %>%
  left_join(draft %>% select(playerId, round, draftTeam, pick), by = "playerId")

# Create binary drafted variable (1 = drafted, 0 = not drafted)
merged <- merged %>%
  mutate(drafted = if_else(!is.na(round), 1, 0))

# Summarize results
table(merged$drafted)

# Optional: quick summary
cat("Total players:", nrow(merged), "\n")
cat("Drafted proportion:", mean(merged$drafted), "\n")

summary(merged)
summary(draft)
summary(combine)

table(draft$homeState)
table(combine$homeState)
summary(merged$homeState)



# Keep only data from year 2000 and beyond
nfl <- merged %>%
  filter(combineYear >= 2000)
range(nfl$combineYear, na.rm = TRUE)
nrow(nfl)

summary(nfl)

#group the data according to the positions and then impute missing values accordingly

nfl <- nfl %>%
  group_by(position) %>%
  mutate(
    combineHeight = ifelse(is.na(combineHeight), median(combineHeight, na.rm = TRUE), combineHeight),
    combineWeight = ifelse(is.na(combineWeight), median(combineWeight, na.rm = TRUE), combineWeight),
    combine40yd   = ifelse(is.na(combine40yd), median(combine40yd, na.rm = TRUE), combine40yd),
    combineVert   = ifelse(is.na(combineVert), median(combineVert, na.rm = TRUE), combineVert),
    combineBench  = ifelse(is.na(combineBench), median(combineBench, na.rm = TRUE), combineBench),
    combineShuttle = ifelse(is.na(combineShuttle), median(combineShuttle, na.rm = TRUE), combineShuttle),
    combineBroad  = ifelse(is.na(combineBroad), median(combineBroad, na.rm = TRUE), combineBroad),
    combine3cone  = ifelse(is.na(combine3cone), median(combine3cone, na.rm = TRUE), combine3cone),
    combineHand = ifelse(is.na(combineHand), median(combineHand, na.rm = TRUE), combineHand),
    dob = ifelse(is.na(dob), median(dob, na.rm = TRUE), dob),
    combineArm = ifelse(is.na(combineArm), median(combineArm, na.rm = TRUE), combineArm),
    combine60ydShuttle = ifelse(is.na(combine60ydShuttle), median(combine60ydShuttle, na.rm = TRUE), combine60ydShuttle),
    combineWonderlic = ifelse(is.na(combineWonderlic), median(combineWonderlic, na.rm = TRUE), combineWonderlic),
    round = ifelse(is.na(round), median(round, na.rm = TRUE), round),
    pick = ifelse(is.na(pick), median(pick, na.rm = TRUE), pick),
    ageAtDraft = ifelse(is.na(ageAtDraft), median(ageAtDraft, na.rm = TRUE), ageAtDraft)
  ) %>%
  ungroup()

summary(nfl)


#since there's still some more missing values, impute and replace with the overall median values across all positions

nfl <- nfl %>%
  mutate(
    combine40yd   = ifelse(is.na(combine40yd), median(combine40yd, na.rm = TRUE), combine40yd),
    combineVert   = ifelse(is.na(combineVert), median(combineVert, na.rm = TRUE), combineVert),
    combineBench  = ifelse(is.na(combineBench), median(combineBench, na.rm = TRUE), combineBench),
    combineShuttle = ifelse(is.na(combineShuttle), median(combineShuttle, na.rm = TRUE), combineShuttle),
    combineBroad  = ifelse(is.na(combineBroad), median(combineBroad, na.rm = TRUE), combineBroad),
    combine3cone  = ifelse(is.na(combine3cone), median(combine3cone, na.rm = TRUE), combine3cone),
    combineWonderlic = ifelse(is.na(combineWonderlic), median(combineWonderlic, na.rm = TRUE), combineWonderlic),
    ageAtDraft = ifelse(is.na(ageAtDraft), median(ageAtDraft, na.rm = TRUE), ageAtDraft),
    combineHand = ifelse(is.na(combineHand), median(combineHand, na.rm = TRUE), combineHand),
    dob = ifelse(is.na(dob), median(dob, na.rm = TRUE), dob),
    combineArm = ifelse(is.na(combineArm), median(combineArm, na.rm = TRUE), combineArm),
    combine60ydShuttle   = ifelse(is.na(combine60ydShuttle), median(combine60ydShuttle, na.rm = TRUE), combine60ydShuttle),
    round   = ifelse(is.na(round), median(round, na.rm = TRUE), round),
    pick   = ifelse(is.na(pick), median(pick, na.rm = TRUE), pick),
  )
summary(nfl)







