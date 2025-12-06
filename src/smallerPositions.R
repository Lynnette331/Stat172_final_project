source("src/cleaning.R")

positions <- c("QB", "RB", "FB", "WR", "TE", "DB", "S", "LB", "OLB", "K", "P", "LS")
nfl_smaller <- nfl %>%
  filter(position %in% positions)

nfl_smaller <- nfl_smaller %>%
  select(combineHeight, combineWeight, combineBMI, ageAtDraft,
         combine40yd, combineVert, combineBench,
         combineShuttle, combineBroad, combine3cone,
         region, top_three_round_bin, drafted_class)

RNGkind(sample.kind = "default")
set.seed(2291352)

train.idx <- sample(x = 1:nrow(nfl_smaller), size = 0.7*nrow(nfl_smaller))
train.df <- nfl_smaller[train.idx,]
test.df <- nfl_smaller[-train.idx,] 

set.seed(172172172)
ctree <- rpart(drafted_class ~ combineHeight + combineWeight + combineBMI + ageAtDraft + combine40yd + combineVert + combineBench + combineShuttle + combineBroad + combine3cone + region, # assumption: want to use every remaining variable as an x 
               data = train.df,
               method = "class")

rpart.plot(ctree)
ggsave("output/smallerPositions_ctree.pdf")

# tuning the tree and making it large
ctree <- rpart(drafted_class ~ combineHeight + combineWeight + combineBMI + ageAtDraft + combine40yd + combineVert + combineBench + combineShuttle + combineBroad + combine3cone + region, # assumption: want to use every remaining variable as an x 
               data = train.df,
               method = "class",
               control = rpart.control(cp=0.0001, minsplit = 1))
optimalcp <- ctree$cptable[which.min(ctree$cptable[,"xerror"]),"CP"]
tunedtree <- rpart::prune(ctree, cp = optimalcp)
rpart.plot(tunedtree)
ggsave("output/smallerPositions_tunedtree.pdf")

# make an ROC curve for final tree
pi_hat <- predict(tunedtree, test.df, type="prob")[,"Drafted"]
rocCurve <- roc(response = test.df$drafted_class, 
                predictor = pi_hat, 
                levels = c("Not Drafted", "Drafted"))
plot(rocCurve, print.thres = TRUE, print.auc = FALSE)
ggsave("output/tunedtree_rocCurve.pdf")

# for our tuned tree, 
# our Specificity is 0.683
# our sensitivity is 0.589
# so our tree will correctly predict 68.3 % of the non top-3 drafted players
# our tree will correctly predict 58.9% of the top-3 drafted players 

# save column of categorical predictions 
test.df$draft_pred <- predict(tunedtree, test.df, type = "class")
summary(test.df$draft_pred)

myforest <- randomForest(drafted_class ~ combineHeight + combineWeight + combineBMI + ageAtDraft + combine40yd + combineVert + combineBench + combineShuttle + combineBroad + combine3cone + region,
                         data = train.df, 
                         ntree = 500, 
                         mtry = 3,
                         importance = TRUE)

# step 1: define the model (with mtry as a tunable parameter)
rf_model <- rand_forest(mtry = tune(), 
                        trees = 500) %>% 
  set_mode("classification") %>% 
  set_engine("randomForest") 

# step 2: create a recipe
rf_rec <- recipe(drafted_class ~ combineHeight + combineWeight + combineBMI + ageAtDraft + combine40yd + combineVert + combineBench + combineShuttle + combineBroad + combine3cone + region, data=train.df) 

# step 3: create the workflow
rf_wf <- workflow() %>%
  add_model(rf_model) %>% 
  add_recipe(rf_rec) 

# step 4: create folds for cross validation 
folds <- vfold_cv(train.df, v=5) 

# step 5: tune random forest
rf_tuned <- tune_grid(
  rf_wf, 
  resamples = folds, 
  grid = tibble(mtry = c(1:11)), 
  metrics = metric_set(roc_auc) 
)

# step 6: extract AUC and/or OOB error estimates
rf_results <- rf_tuned %>%
  collect_metrics()

ggplot(data = rf_results) + 
  geom_line(aes(x=mtry,y=mean)) +
  labs(x="m (mtry) value", y = "Area Under the Curve (AUC)") + 
  theme_bw() + 
  scale_x_continuous(breaks = c(1:12))

ggsave("output/random_forest_smallerPositions.pdf")

best_params <- select_best(rf_tuned, metric = "roc_auc")

final_forest <- final_forest <- randomForest(drafted_class ~ combineHeight + combineWeight + combineBMI + ageAtDraft + combine40yd + combineVert + combineBench + combineShuttle + combineBroad + combine3cone + region,
                                             data=train.df,
                                             ntree = 500,
                                             mtry = best_params %>% pull(mtry),
                                             importance = TRUE)
                                             
saveRDS(final_forest, file = "final_forest_smallerPositions.rds")
# it is looking like 2 is the best mtry value 

#(7) make an ROC curve for your final forest. 
pi_hat <- predict(final_forest, test.df, type = "prob")[,"Drafted"]
rocCurve <- roc(response = test.df$drafted_class,
                predictor = pi_hat,
                levels = c("Not Drafted", "Drafted")) 
plot(rocCurve, print.thres = TRUE, print.auc = TRUE)
ggsave("output/final_forest_rocCurve_smallerPositions.pdf")


# Specificity is 0.628 true negatives
# Sensitivity is 0.674 true positives 

# (8) Save a column of forest_preds 
test.df$forest_preds <- predict(final_forest, test.df, type = "class")
view(test.df)

# (9) variable importance plot 
varImpPlot(final_forest, type=1)

"
combine40Yd
ageAtDraft
combineBroad
combine3Cone
combineHeight
combineBMI
combineVert
combineSHuttle
combineWeight
combineBench
region
"

# finding a GLM that best fits this data
m1 <- glm(top_three_round_bin ~ combine40yd,
          data = nfl_smaller, family = binomial(link = "logit"))
AIC(m1)
# 3823
m2 <- glm(top_three_round_bin ~ combine40yd + ageAtDraft,
          data = nfl_smaller, family = binomial(link = "logit")) 
AIC(m2)
# 3736
m3 <- glm(top_three_round_bin ~ combine40yd + ageAtDraft + combineBroad,
          data = nfl_smaller, family = binomial(link = "logit")) 
AIC(m3)
# 3705
m4 <- glm(top_three_round_bin ~ combine40yd + ageAtDraft + combineBroad + combine3cone,
          data = nfl_smaller, family = binomial(link = "logit")) 
AIC(m4)
# 3699
m5 <- glm(top_three_round_bin ~ combine40yd + ageAtDraft + combineBroad + combine3cone + combineHeight,
          data = nfl_smaller, family = binomial(link = "logit")) 
AIC(m5)
# 3601
m6 <- glm(top_three_round_bin ~ combine40yd + ageAtDraft + combineBroad + combine3cone + combineHeight + combineBMI,
          data = nfl_smaller, family = binomial(link = "logit")) 
AIC(m6)
# 3572
m7 <- glm(top_three_round_bin ~ combine40yd + ageAtDraft + combineBroad + combine3cone + combineHeight + combineBMI + combineVert,
          data = nfl_smaller, family = binomial(link = "logit")) 
AIC(m7)
# 3570
m8 <- glm(top_three_round_bin ~ combine40yd + ageAtDraft + combineBroad + combine3cone + combineHeight + combineBMI + combineVert + combineShuttle,
          data = nfl_smaller, family = binomial(link = "logit")) 
AIC(m8)
# 3566
m9 <- glm(top_three_round_bin ~ combine40yd + ageAtDraft + combineBroad + combine3cone + combineHeight + combineBMI + combineVert + combineShuttle + combineWeight,
          data = nfl_smaller, family = binomial(link = "logit")) 
AIC(m9)
# 3561
m10 <- glm(top_three_round_bin ~ combine40yd + ageAtDraft + combineBroad + combine3cone + combineHeight + combineBMI + combineVert + combineShuttle + combineWeight + combineBench,
          data = nfl_smaller, family = binomial(link = "logit")) 
AIC(m10)
# 3540
m11 <- glm(top_three_round_bin ~ combine40yd + ageAtDraft + combineBroad + combine3cone + combineHeight + combineBMI + combineVert + combineShuttle + combineWeight + combineBench + region,
           data = nfl_smaller, family = binomial(link = "logit")) 
AIC(m11)
# 3539
# the model that best fits the data uses all the variables 