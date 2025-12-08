

source("src/cleaning.R")


# remove all the columns you don't want to work with
nfl_model <- nfl[, !names(nfl) %in% c("playerID", "combinePosition", "ageAtDraft", "combineHand", 
                   "nameFirst", "nameLast", "nameFull", "position", 
                   "nflId", "region", "college", "playerProfileUrl", "homeCity",
                   "homeState", "homeCountry", "highSchool", "hsCity", "hsState", 
                   "hsCountry", "combine60ydShuttle", "combineWonderlic", "draftTeam", "combineArm",
                   "drafted_class", "round", "drafted", "pick", "heightInches", "weight", "top_three_round_class"
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
              data = train.df,
              family = binomial(link = "logit"))

lr_ml_coefs <- coef(lr_mle)


# Create model matrices
x.train <- model.matrix(top_three_round_bin ~ ., data = train.df)[,-1]
x.test <- model.matrix(top_three_round_bin ~ ., data = test.df)[,-1]

#create vectors of 0/1 variable
y.train <- as.vector(train.df$top_three_round_bin) %>% as.vector
y.test <- as.vector(test.df$top_three_round_bin)


lr_lasso_cv <- cv.glmnet(x.train, y.train, 
                         family = binomial(link = "logit"),
                         alpha = 1) 


lr_ridge_cv <- cv.glmnet(x.train, y.train, 
                         family = binomial(link = "logit"),
                         alpha = 0)

#plot results from cross validation procedures
pdf("output/lassocv.pdf")
plot(lr_lasso_cv, sign.lambda = 1)
dev.off()
pdf("output/ridgecv.pdf")
plot(lr_ridge_cv, sign.lambda = 1)
dev.off()

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
ggsave("output/tesetdfpreds$MLEpred.pdf")

#Finally make an ROC
mle_rocCurve <- roc(response = as.factor(test.df.preds$top_three_round_bin),
                    predictor = test.df.preds$mle_pred,
                    levels = c("0", "1"))

lasso_rocCurve <- roc(response = as.factor(test.df.preds$top_three_round_bin),
                    predictor = test.df.preds$lasso_pred,
                    levels = c("0", "1"))

ridge_rocCurve <- roc(response = as.factor(test.df.preds$top_three_round_bin),
                    predictor = test.df.preds$ridge_pred,
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
pdf("output/lasso_mle_ridge_ROC.pdf")
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
dev.off()











