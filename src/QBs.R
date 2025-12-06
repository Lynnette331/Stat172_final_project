source("src/cleaning.R")

qb <- c("QB")
nfl_qb <- nfl %>%
  filter(position %in% qb)


nfl_qb <- nfl_qb %>%
  select(combineHeight, combineWeight, combineBMI, ageAtDraft,
         combine40yd, combineVert, combineBench,
         combineShuttle, combineBroad, combine3cone,
         region, top_three_round_bin, top_three_round_class)

RNGkind(sample.kind = "default")
set.seed(2291352)

train.idx <- sample(x = 1:nrow(nfl_qb), size = 0.7*nrow(nfl_qb))
train.df <- nfl_qb[train.idx,]
test.df <- nfl_qb[-train.idx,] 

set.seed(172172172)
ctree <- rpart(top_three_round_class ~ combineHeight + combineWeight + combineBMI + ageAtDraft + combine40yd + combineVert + combineBench + combineShuttle + combineBroad + combine3cone + region, # assumption: want to use every remaining variable as an x 
               data = train.df,
               method = "class")

rpart.plot(ctree)
ggsave("output/QBs_ctree.pdf")

# tuning the tree and making it large
ctree <- rpart(top_three_round_class ~combineHeight + combineWeight + combineBMI + ageAtDraft + combine40yd + combineVert + combineBench + combineShuttle + combineBroad + combine3cone + region, # assumption: want to use every remaining variable as an x 
               data = train.df,
               method = "class",
               control = rpart.control(cp=0.0001, minsplit = 1))
optimalcp <- ctree$cptable[which.min(ctree$cptable[,"xerror"]),"CP"]
tunedtree <- rpart::prune(ctree, cp = optimalcp)
rpart.plot(tunedtree)
ggsave("output/defense_tuned_ctree.pdf")

# make an ROC curve for final tree
pi_hat <- predict(tunedtree, test.df, type="prob")[,"Drafted"]
rocCurve <- roc(response = test.df$top_three_round_class, #supply truth in test set
                predictor = pi_hat, # supply predicted probabilities
                levels = c("Not Drafted", "Drafted")) #(negative, positive)
plot(rocCurve, print.thres = TRUE, print.auc = FALSE)
ggsave("output/QBs_rocCurve.pdf")

# for our tuned tree, 
# our Specificity is 1.000
# our sensitivity is 0.150
# so our tree will correctly predict 100% of the non top-3 drafted players
# our tree will correctly predict 15% of the top-3 drafted players 

# save column of categorical predictions 
test.df$draft_pred <- predict(tunedtree, test.df, type = "class")
summary(test.df$draft_pred)


# starting on the forest
myforest <- randomForest(top_three_round_class ~ combineHeight + combineWeight + combineBMI + ageAtDraft + combine40yd + combineVert + combineBench + combineShuttle + combineBroad + combine3cone + region,
                         data = train.df, 
                         ntree = 500, 
                         mtry = 3,
                         importance = TRUE)

# step 1: define the model 
rf_model <- rand_forest(mtry = tune(), 
                        trees = 500) %>% 
  set_mode("classification") %>%  
  set_engine("randomForest") 

# step 2: create a recipe
rf_rec <- recipe(top_three_round_class ~ combineHeight + combineWeight + combineBMI + ageAtDraft + combine40yd + combineVert + combineBench + combineShuttle + combineBroad + combine3cone + region, data=train.df) 

# step 3: create the workflow
rf_wf <- workflow() %>%
  add_model(rf_model) %>% 
  add_recipe(rf_rec) 

# step 4: create folds for cross validation 
folds <- vfold_cv(train.df, v=5)  

# step 5: tune random forest
rf_tuned <- tune_grid(
  rf_wf, # workflow from step 3
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
ggsave("output/offense_rf_results.pdf")

best_params <- select_best(rf_tuned, metric = "roc_auc")

final_forest <- final_forest <- randomForest(top_three_round_class ~ combineHeight + combineWeight + combineBMI + ageAtDraft + combine40yd + combineVert + combineBench + combineShuttle + combineBroad + combine3cone + region,
                                             data=train.df,
                                             ntree = 500,
                                             mtry = best_params %>% pull(mtry),
                                             importance = TRUE)
saveRDS(final_forest, file = "final_forest_QBs.rds")
# it is looking like 3 is the best mtry value 

#(7) make an ROC curve for your final forest. What is AUC? Pi*? Spec and Sens
pi_hat <- predict(final_forest, test.df, type = "prob")[,"Drafted"]
rocCurve <- roc(response = test.df$top_three_round_class,
                predictor = pi_hat,
                levels = c("Not Drafted", "Drafted"))
plot(rocCurve, print.thres = TRUE, print.auc =TRUE)
ggsave("output/offense_final_forest_rocCurve.pdf")
# AUC is 0.722
# Pi* is 0.338
# Specificity is 0.775 true negatives
# Sensitivity is 0.650 true positives 

# (8) Save a column of forest_preds 
test.df$forest_preds <- predict(final_forest, test.df, type = "class")
view(test.df)

# (9) variable importance plot 
varImpPlot(final_forest, type=1)
"
ageATDraft
combineBroad
region
combineHeight
combineWeight
combineVert
combineBMI
combineShuttle
combine40yd
combine3cone
combineBench
"

# Make a Logistic regression using the variable importance plot
m1 <- glm(top_three_round_bin ~ ageAtDraft,
          data = nfl_qb, family = binomial(link = "logit"))
AIC(m1)
# 335
m2 <- glm(top_three_round_bin ~ ageAtDraft + combineBroad,
          data = nfl_qb, family = binomial(link = "logit"))
AIC(m2)
# 323
m3 <- glm(top_three_round_bin ~ ageAtDraft + combineBroad + region,
          data = nfl_qb, family = binomial(link = "logit"))
AIC(m3)
# 326

# the model that fits this data best only includes age and broad jump 
summary(m2)
# our equation is 1.70846 - 0.49129(ageAtDraft) + 0.08048(combineBroad)
