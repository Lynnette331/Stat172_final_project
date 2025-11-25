
#Clear environment
rm(list = ls())

#Load packages
library(tidyverse)
library(pROC)
library(glmnet)

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
    dob = ifelse(is.na(dob), median(dob, na.rm = TRUE), dob)
  )

# adding in a bmi variable
nfl$combineBMI <- (703 * nfl$combineWeight) / (nfl$combineHeight^2)

nfl$top_three_round_bin <- ifelse(nfl$round %in% c(1, 2, 3), 1, 0)
nfl$top_three_round_bin[is.na(nfl$round)] <- 0

nfl$top_three_round_class <- factor(nfl$top_three_round_bin, 
                                    levels = c(0,1),
                                    labels = c("Not Drafted", "Drafted") )



# remove all the columns you don't want to work with
nfl_model <- nfl[, !names(nfl) %in% c("playerID", "combinePosition", "ageAtDraft", "combineHand", 
                   "nameFirst", "nameLast", "nameFull", "position", 
                   "nflId", "region", "college", "playerProfileUrl", "homeCity",
                   "homeState", "homeCountry", "highSchool", "hsCity", "hsState", 
                   "hsCountry", "combine60ydShuttle", "combineWonderlic", "draftTeam", "combineArm",
                   "top_three_round_class", "round", "drafted", "pick", "heightInches", "weight"
                   )]



# Fix for Date columns specifically
nfl_model <- nfl_model %>%
  mutate(dob = if_else(is.na(dob), median(dob, na.rm = TRUE), dob))

# Now you can continue with your train/test split
RNGkind(sample.kind = "default")
set.seed(23591)
train.idx <- sample(x = 1:nrow(nfl_model), size = 0.7*nrow(nfl_model))
train.df <- nfl_model[train.idx,]
test.df <- nfl_model[-train.idx,]


# Clean BOTH train and test data to fix the NA values error
train.df <- train.df %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.character), ~ifelse(is.na(.), "Unknown", .))) %>%
  mutate(across(where(function(x) inherits(x, "Date")), ~if_else(is.na(.), median(., na.rm = TRUE), .)))


test.df <- test.df %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.character), ~ifelse(is.na(.), "Unknown", .))) %>%
  mutate(across(where(function(x) inherits(x, "Date")), ~if_else(is.na(.), median(., na.rm = TRUE), .)))

#Start off with a traditional logistic regression fit with MLE
lr_mle <- glm(top_three_round_bin ~ ., 
              data = nfl_model,
              family = binomial(link = "logit"))

lr_ml_coefs <- coef(lr_mle)


# Create model matrices
x.train <- model.matrix(top_three_round_bin ~ ., data = train.df)[,-1]
x.test <- model.matrix(top_three_round_bin ~ ., data = test.df)[,-1]

#create variables of 0/1 variable
y.train <- as.vector(train.df$top_three_round_bin)
y.test <- as.vector(test.df$top_three_round_bin)


lr_lasso_cv <- cv.glmnet(x.train, y.train, 
                         family = "binomial"(link = "logit"),
                         alpha = 1) 


lr_ridge_cv <- cv.glmnet(x.train, y.train, 
                         family = "binomial"(link = "logit"),
                         alpha = 0)

#plot results from cross validation procedures
plot(lr_lasso_cv, sign.lambda = 1)
plot(lr_ridge_cv, sign.lambda = 1)


#save the "best" lambdas
best_lasso_lambda <- lr_lasso_cv$lambda.min
best_ridge_lambda <- lr_ridge_cv$lambda.min

lr_ridge_coefs <- coef(lr_ridge_cv, s = "lambda.min") %>% as.matrix()
lr_lasso_coefs <- coef(lr_lasso_cv, s = "lambda.min") %>% as.matrix()

lr_lasso_coefs
lr_ridge_coefs


#plot
ggplot() +
  geom_point(aes(x = lr_ml_coefs, y = lr_ridge_coefs)) + 
  geom_abline(aes(intercept = 0, slope = 1)) + 
  xlim(c(-10,10)) + ylim(c(-10,10))

ggplot() +
  geom_point(aes(x = lr_ml_coefs, y = lr_lasso_coefs)) + 
  geom_abline(aes(intercept = 0, slope = 1)) + 
  xlim(c(-10,10)) + ylim(c(-10,10))

#Fit final ridge and lasso models
final_lasso <- glmnet(x.train, y.train, 
                      family = binomial(link = "logit"),
                      alpha = 1,
                      lambda = best_lasso_lambda)


final_ridge <- glmnet(x.train, y.train, 
                      family = binomial(link = "logit"),
                      alpha = 0,
                      lambda = best_ridge_lambda)

test.df.preds <- test.df %>% 
  mutate(mle_pred = predict(lr_mle, test.df, type = "response"),
         lasso_pred = predict(final_lasso, x.test, type = "response")[,1],
         ridge_pred = predict(final_ridge, x.test, type = "response")[,1])

cor(test.df.preds$mle_pred, test.df.preds$lasso_pred)
plot(test.df.preds$mle_pred, test.df.preds$lasso_pred)


#Finally make an ROC
mle_rocCurve <- roc(response = as.factor(test.df.preds$top_three_round_bin),
                    predictor = test.df.preds$mle_pred,
                    levels = c("0", "1"))

lasso_rocCurve <- roc(response = as.factor(test.df.preds$top_three_round_bin),
                    predictor = test.df.preds$lasso_pred,
                    levels = c("0", "1"))

ridge_rocCurve <- roc(response = as.factor(test.df.preds$top_three_round_bin),
                    predictor = test.df.,.preds$ridge_pred,
                    levels = c("0", "1"))

#extract AUC 
mle_AUC <- auc(mle_rocCurve)
lasso_AUC <- auc(lasso_rocCurve)
ridge_AUC <- auc(ridge_rocCurve)


# Print results
cat("Model Performance (AUC):\n")
cat("MLE Logistic Regression:", round(mle_AUC, 4), "\n")
cat("LASSO Regression:", round(lasso_AUC, 4), "\n")
cat("Ridge Regression:", round(ridge_AUC, 4), "\n")

# Plot all ROC curves together
plot(mle_rocCurve, col = "red", main = "ROC Curves Comparison")
plot(lasso_rocCurve, col = "blue", add = TRUE)
plot(ridge_rocCurve, col = "green", add = TRUE)

# Add legend
legend("bottomright", 
       legend = c(paste("MLE (AUC =", round(mle_AUC, 3), ")"),
                  paste("LASSO (AUC =", round(lasso_AUC, 3), ")"),
                  paste("Ridge (AUC =", round(ridge_AUC, 3), ")")),
       col = c("red", "blue", "green"),
       lty = 1)













