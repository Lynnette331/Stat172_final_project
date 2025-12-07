source("src/cleaning.R")

nfl_model <- nfl %>%
  select(combineHeight, combineWeight, combineBMI, ageAtDraft,
         combine40yd, combineVert, combineBench,
         combineShuttle, combineBroad, combine3cone,
         region, top_three_round_bin)

summary(nfl_model)

nfl_model$top_three_round_class <- factor(nfl_model$top_three_round_bin, 
                                        levels = c(0,1),
                                        labels = c("Not Drafted", "Drafted") )

# fitting a tree to predict getting drafted in round one in general 
RNGkind(sample.kind = "default")
set.seed(2291352)

train.idx <- sample(x = 1:nrow(nfl_model), size = 0.7*nrow(nfl_model))
train.df <- nfl_model[train.idx,]
test.df <- nfl_model[-train.idx,] 

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
# our Specificity is 0.633
# our sensitivity is 0.623
# so our tree will correctly prefict 73.1% of the non top-3 drafted players
# our tree will correctly presuct 48.6% of the top-3 drafted players 

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
ggsave("output/mtry_tuning_plot.pdf")

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
plot(rocCurve, print.thres = TRUE, print.auc = TRUE)
# AUC is
# Pi* is 
# Specificity is 0.686 true negatives
# Sensitivity is 0.614 true positives 

# (8) Save a column of forest_preds 
test.df$forest_preds <- predict(final_forest, test.df, type = "class")
view(test.df)

# (9) variable importance plot 
varImpPlot(final_forest, type=1)
# in order of importance for top 3 rounds all positions:
" 40 yard dash
weight
age 
height
BMI
3cone
vertical
broad
bench
shuttle
region"

# fitting a logistic regression 

m1 <- glm(top_three_round_bin ~ combine40yd,
          data = nfl_model, family = binomial(link = "logit"))
AIC(m1)
# 5859
m2 <- glm(top_three_round_bin ~ combine40yd + combineWeight,
          data = nfl_model, family = binomial(link = "logit"))
AIC(m2)
# 5554
m3 <- glm(top_three_round_bin ~ combine40yd + combineWeight + ageAtDraft,
          data = nfl_model, family = binomial(link = "logit"))
AIC(m3)
# 5414
m4 <- glm(top_three_round_bin ~ combine40yd + combineWeight + ageAtDraft + combineHeight,
          data = nfl_model, family = binomial(link = "logit")) 
AIC(m4)
# 5393
m5  <- glm(top_three_round_bin ~ combine40yd + combineWeight + ageAtDraft + combineHeight + combineBMI,
           data = nfl_model, family = binomial(link = "logit")) 
AIC(m5)
# 5363
m6 <- glm(top_three_round_bin ~ combine40yd + combineWeight + ageAtDraft + combineHeight + combineBMI + combine3cone,
          data = nfl_model, family = binomial(link = "logit")) 
AIC(m6)
# 5337
m7 <- glm(top_three_round_bin ~ combine40yd + combineWeight + ageAtDraft + combineHeight + combineBMI + combine3cone + combineVert,
          data = nfl_model, family = binomial(link = "logit")) 
AIC(m7)
# 5325
m8 <- glm(top_three_round_bin ~ combine40yd + combineWeight + ageAtDraft + combineHeight + combineBMI + combine3cone + combineVert + combineBroad,
          data = nfl_model, family = binomial(link = "logit")) 
AIC(m8)
# 5316
m9 <- glm(top_three_round_bin ~ combine40yd + combineWeight + ageAtDraft + combineHeight + combineBMI + combine3cone + combineVert + combineBroad + combineBench,
          data = nfl_model, family = binomial(link = "logit"))
AIC(m9)
# 5299.661
m10 <- glm(top_three_round_bin ~ combine40yd + combineWeight + ageAtDraft + combineHeight + combineBMI + combine3cone + combineVert + combineBroad + combineBench + combineShuttle,
          data = nfl_model, family = binomial(link = "logit"))
AIC(m10)
summary(m10)
# 5299.249
# model 10 is our model with the lowest AIC
# variables in the model
"
combine40yd + 
combineWeight + 
ageAtDraft + 
combineHeight + 
combineBMI + 
combine3cone + 
combineVert + 
combineBroad + 
combineBench + 
combineShuttle
"
