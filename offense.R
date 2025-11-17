# Load tidyverse for data manipulation
rm(list=ls())
library(tidyverse)
library(rpart)
library(rpart.plot)
library(pROC)
library(dplyr)
library(randomForest)
library(tidymodels)


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

nfl <- nfl %>%
  filter(homeCountry == "USA")

nfl <- nfl %>%
  mutate(
    homeState = case_when(
      homeState == "AS" ~ "AK",
      TRUE ~ homeState))

# creating a regions column
nfl <- nfl %>%
  mutate(
    region = case_when(
      homeState %in% c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL", "MI", "IN", "OH") ~ "Midwest",
      homeState %in% c("ME","VT","NH","MA","CT","RI","NY","PA","NJ","DE") ~ "Northeast",
      homeState %in% c("TX","OK","AR","LA","MS","AL","GA","FL","TN","KY","SC","NC","VA","WV","DC","MD") ~ "South",
      homeState %in% c("WA","OR","CA","NV","ID","MT","WY","UT","CO","AZ","NM","HI","AK") ~ "West",
      TRUE ~ "Other"  # fallback for any states not matched
    )
  )

nfl <- nfl %>% 
  filter(region != "Other")


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
    combine3cone  = ifelse(is.na(combine3cone), median(combine3cone, na.rm = TRUE), combine3cone),
    ageAtDraft = ifelse(is.na(ageAtDraft), median(ageAtDraft, na.rm = TRUE), ageAtDraft)
  ) %>%
  ungroup()


#since there's still some more missing values, impute and replace with the overall median values across all positions

nfl <- nfl %>%
  mutate(
    combine40yd   = ifelse(is.na(combine40yd), median(combine40yd, na.rm = TRUE), combine40yd),
    combineVert   = ifelse(is.na(combineVert), median(combineVert, na.rm = TRUE), combineVert),
    combineBench  = ifelse(is.na(combineBench), median(combineBench, na.rm = TRUE), combineBench),
    combineShuttle = ifelse(is.na(combineShuttle), median(combineShuttle, na.rm = TRUE), combineShuttle),
    combineBroad  = ifelse(is.na(combineBroad), median(combineBroad, na.rm = TRUE), combineBroad),
    combine3cone  = ifelse(is.na(combine3cone), median(combine3cone, na.rm = TRUE), combine3cone),
    ageAtDraft = ifelse(is.na(ageAtDraft), median(ageAtDraft, na.rm = TRUE), ageAtDraft),
  )

# adding in a bmi variable
nfl$combineBMI <- (703 * nfl$combineWeight) / (nfl$combineHeight^2)

nfl$top_three_round_bin <- ifelse(nfl$round %in% c(1, 2, 3), 1, 0)
nfl$top_three_round_bin[is.na(nfl$round)] <- 0

nfl$top_three_round_class <- factor(nfl$top_three_round_bin, 
                                    levels = c(0,1),
                                    labels = c("Not Drafted", "Drafted") )

offensive_positions <- c("C", "OG", "OL", "OT", "QB", "RB", "FB", "TE", "WR")
nfl_offense <- nfl %>%
  filter(position %in% offensive_positions)


nfl_offense <- nfl_offense %>%
  select(combineHeight, combineWeight, combineBMI, ageAtDraft,
         combine40yd, combineVert, combineBench,
         combineShuttle, combineBroad, combine3cone,
         region, top_three_round_bin, top_three_round_class)

RNGkind(sample.kind = "default")
set.seed(2291352)

train.idx <- sample(x = 1:nrow(nfl_offense), size = 0.7*nrow(nfl_offense))
train.df <- nfl_offense[train.idx,]
test.df <- nfl_offense[-train.idx,] 

set.seed(172172172)
ctree <- rpart(top_three_round_class ~ combineHeight + combineWeight + combineBMI + ageAtDraft + combine40yd + combineVert + combineBench + combineShuttle + combineBroad + combine3cone + region, # assumption: want to use every remaining variable as an x 
               data = train.df,
               method = "class")

rpart.plot(ctree)

# tuning the tree and making it large
ctree <- rpart(top_three_round_class ~combineHeight + combineWeight + combineBMI + ageAtDraft + combine40yd + combineVert + combineBench + combineShuttle + combineBroad + combine3cone + region, # assumption: want to use every remaining variable as an x 
               data = train.df,
               method = "class",
               control = rpart.control(cp=0.0001, minsplit = 1))
optimalcp <- ctree$cptable[which.min(ctree$cptable[,"xerror"]),"CP"]
tunedtree <- rpart::prune(ctree, cp = optimalcp)
rpart.plot(tunedtree)

# make an ROC curve for final tree
pi_hat <- predict(tunedtree, test.df, type="prob")[,"Drafted"]
rocCurve <- roc(response = test.df$top_three_round_class, #supply truth in test set
                predictor = pi_hat, # supply predicted probabilities
                levels = c("Not Drafted", "Drafted")) #(negative, positive)
plot(rocCurve, print.thres = TRUE, print.auc = FALSE)

# for our tuned tree, 
# our Specificity is 0.946
# our sensitivity is 0.169
# so our tree will correctly prefict 94.6% of the non top-3 drafted players
# our tree will correctly presuct 16.9% of the top-3 drafted players 

# save column of categorical predictions 
test.df$draft_pred <- predict(tunedtree, test.df, type = "class")
summary(test.df$draft_pred)


# starting on the forest

myforest <- randomForest(top_three_round_class ~ combineHeight + combineWeight + combineBMI + ageAtDraft + combine40yd + combineVert + combineBench + combineShuttle + combineBroad + combine3cone + region,# recall notes on the syntax
                         data = train.df, # training data
                         ntree = 500, 
                         mtry = 3,# choose m - sqrt(11)
                         importance = TRUE)

# step 1: define the model (with mtry as a tunable parameter)
rf_model <- rand_forest(mtry = tune(), # tune() tells it ot tune mtry parameter
                        trees = 500) %>% # fix B (as large as you can afford)
  set_mode("classification") %>%  # not "regression", which is for a numeric Y
  set_engine("randomForest") # note: there are multiple packages that run RFs

# step 2: create a recipe
# here: be mindful! know what ~ means - what are your x veriables?
rf_rec <- recipe(top_three_round_class ~ combineHeight + combineWeight + combineBMI + ageAtDraft + combine40yd + combineVert + combineBench + combineShuttle + combineBroad + combine3cone + region, data=train.df) # use trianing data set

# step 3: create the workflow
rf_wf <- workflow() %>%
  add_model(rf_model) %>% # from step 1
  add_recipe(rf_rec) # from step 2

# step 4: create folds for cross validation (see previous illistration)
folds <- vfold_cv(train.df, v=5) # splits training data into 5 folds 

# step 5: tune random forest
rf_tuned <- tune_grid(
  rf_wf, # workflow from step 3
  resamples = folds, # folds created in step 4
  grid = tibble(mtry = c(1:11)), # think: what is possible here? **** based on the # of variables 
  metrics = metric_set(roc_auc) # could add accuracy here if oob apprach is desired 
)

# step 6: extract AUC and/or OOB error estimates
rf_results <- rf_tuned %>%
  collect_metrics()

ggplot(data = rf_results) + 
  geom_line(aes(x=mtry,y=mean)) +
  labs(x="m (mtry) value", y = "Area Under the Curve (AUC)") + 
  theme_bw() + 
  scale_x_continuous(breaks = c(1:12))

best_params <- select_best(rf_tuned, metric = "roc_auc")

final_forest <- final_forest <- randomForest(top_three_round_class ~ combineHeight + combineWeight + combineBMI + ageAtDraft + combine40yd + combineVert + combineBench + combineShuttle + combineBroad + combine3cone + region,
                                             data=train.df,
                                             ntree = 500,
                                             mtry = best_params %>% pull(mtry),
                                             importance = TRUE)

# it is looking like 8? is the best mtry value 

#(7) make an ROC curve for your final forest. What is AUC? Pi*? Spec and Sens
pi_hat <- predict(final_forest, test.df, type = "prob")[,"Drafted"]
rocCurve <- roc(response = test.df$top_three_round_class,
                predictor = pi_hat,
                levels = c("Not Drafted", "Drafted")) # negative, positive
plot(rocCurve, print.thres = TRUE, print.auc = FALSE)
# AUC is
# Pi* is 
# Specificity is 0.803 true negatives
# Sensitivity is 0.0.553 true positives 

# (8) Save a column of forest_preds 
test.df$forest_preds <- predict(final_forest, test.df, type = "class")
view(test.df)

# (9) variable importance plot 
varImpPlot(final_forest, type=1)
"
age at draft
combine 40 yd
combine weight 
combine bmi
combine shuttle
3 cone 
height 
broad 
vert 
bench 
region 
" 

# finding the best logistic regression model 

m1 <- glm(top_three_round_bin ~ ageAtDraft,
          data = nfl_offense, family = binomial(link = "logit"))
AIC(m1)
# 2861
m2 <- glm(top_three_round_bin ~ ageAtDraft + combine40yd,
          data = nfl_offense, family = binomial(link = "logit"))
AIC(m2)
# 2859
m3 <- glm(top_three_round_bin ~ ageAtDraft + combine40yd + combineWeight,
          data = nfl_offense, family = binomial(link = "logit"))
AIC(m3)
# 2749
m4 <- glm(top_three_round_bin ~ ageAtDraft + combine40yd + combineWeight + combineBMI,
          data = nfl_offense, family = binomial(link = "logit"))
AIC(m4)
# 2716
m5 <- glm(top_three_round_bin ~ ageAtDraft + combine40yd + combineWeight + combineBMI + combineShuttle,
          data = nfl_offense, family = binomial(link = "logit"))
AIC(m5)
# 2711
m6 <- glm(top_three_round_bin ~ ageAtDraft + combine40yd + combineWeight + combineBMI + combineShuttle + combine3cone,
          data = nfl_offense, family = binomial(link = "logit"))
AIC(m6)
# 2709
m7 <- glm(top_three_round_bin ~ ageAtDraft + combine40yd + combineWeight + combineBMI + combineShuttle + combine3cone + combineHeight,
          data = nfl_offense, family = binomial(link = "logit"))
AIC(m7)
# 2697
m8 <- glm(top_three_round_bin ~ ageAtDraft + combine40yd + combineWeight + combineBMI + combineShuttle + combine3cone + combineHeight + combineBroad,
          data = nfl_offense, family = binomial(link = "logit"))
AIC(m8)
# 2692.144
m9 <- glm(top_three_round_bin ~ ageAtDraft + combine40yd + combineWeight + combineBMI + combineShuttle + combine3cone + combineHeight + combineBroad + combineVert,
          data = nfl_offense, family = binomial(link = "logit"))
AIC(m9)
# 2692.311
# model 8 is the bessdt model

# variables used in the best model:
"
ageAtDraft + 
combine40yd + 
combineWeight + 
combineBMI + 
combineShuttle + 
combine3cone + 
combineHeight + 
combineBroad
"