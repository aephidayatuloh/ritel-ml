library(tidyverse)

abt <- read_csv("data/abt.csv")

ritel_transform <- abt %>% 
  # buat variable target sebagai factor
  mutate(
    next_buy = factor(next_buy, levels = c(1, 0), 
                      labels = c("Yes", "No"))
    ) 

ritel_transform %>% 
  glimpse()


# Machine Learning ---------------------------------------------------


library(tidymodels)

# Split Data ---------------------------------------------------------

set.seed(123)
ritel_split <- ritel_transform %>% 
  initial_split(prop = 0.7, strata = next_buy)

ritel_split

ritel_training <- ritel_split %>% 
  training() %>% 
  select(-MemberID)

ritel_testing <- ritel_split %>% 
  testing()

cv_folds <- vfold_cv(ritel_training,
                     v = 5, repeats = 3,
                     strata = next_buy)

ritel_training %>% 
  count(next_buy) %>% 
  mutate(pct = n/sum(n))

ritel_testing %>% 
  count(next_buy) %>% 
  mutate(pct = n/sum(n))

########################## Training Model ##########################


# Data Preprocess with {recipes} -------------------------------------

ritel_recipe <- recipe(next_buy ~ ., data = ritel_training) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  # step_pca(all_numeric_predictors()) %>% 
  step_dummy(all_factor_predictors()) 

ritel_recipe

ritel_recipe %>% 
  tidy()

ritel_recipe %>% 
  prep() %>% 
  bake(new_data = NULL)


# Logistics Regression ----------------------------------------------

# Model Specification
reglog_spec <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

# Workflow
reglog_workflow <- 
  workflow() %>% 
  add_recipe(ritel_recipe) %>% 
  add_model(reglog_spec)

# Model fitting
reglog_model <- reglog_workflow %>% 
  fit(data = ritel_training)

reglog_model %>% 
  tidy() %>% 
  print(n = Inf)


# Performance -------------------------------------------------------

# Bind training data with prediction results
reglog_train <- reglog_model %>% 
  augment(new_data = ritel_training)

# Bind testing data with prediction results
reglog_pred <- reglog_model %>% 
  augment(new_data = ritel_testing)

# Performance Metrics
reglog_pred %>% 
  count(next_buy)

reglog_pred %>% 
  count(.pred_class)

# Confusion Matrix
reglog_train %>% 
  conf_mat(truth = next_buy, estimate = .pred_class)

reglog_pred %>% 
  conf_mat(truth = next_buy, estimate = .pred_class)

reglog_pred %>% 
  conf_mat(truth = next_buy, estimate = .pred_class) %>% 
  autoplot()

reglog_pred %>% 
  conf_mat(truth = next_buy, estimate = .pred_class) %>% 
  autoplot(type = "heatmap")

reglog_pred %>% 
  conf_mat(truth = next_buy, estimate = .pred_class) %>% 
  tidy() %>% 
  mutate(pct = value/nrow(reglog_pred))

# Accuracy
reglog_train %>% 
  accuracy(truth = next_buy, estimate = .pred_class)

reglog_pred %>% 
  accuracy(truth = next_buy, estimate = .pred_class)

# Sensitivity/Recall
reglog_train %>% 
  sensitivity(truth = next_buy, estimate = .pred_class)

reglog_pred %>% 
  sensitivity(truth = next_buy, estimate = .pred_class)

reglog_pred %>% 
  recall(truth = next_buy, estimate = .pred_class)

# Specificity
reglog_train %>% 
  specificity(truth = next_buy, estimate = .pred_class)

reglog_pred %>% 
  specificity(truth = next_buy, estimate = .pred_class)

# Precision
reglog_train %>% 
  precision(truth = next_buy, estimate = .pred_class)

reglog_pred %>% 
  precision(truth = next_buy, estimate = .pred_class)

# F1 Score
reglog_pred %>% 
  f_meas(truth = next_buy, estimate = .pred_class)

# ROC & AUC
reglog_pred %>% 
  roc_curve(truth = next_buy, .pred_Yes) %>% 
  autoplot()

reglog_train %>% 
  roc_auc(truth = next_buy, .pred_Yes)
reglog_pred %>% 
  roc_auc(truth = next_buy, .pred_Yes)

eval_metrics_class <- metric_set(accuracy, bal_accuracy, 
                           sensitivity, specificity, 
                           precision, recall, f_meas)

reglog_pred %>% 
  eval_metrics_class(truth = next_buy, estimate = .pred_class) %>% 
  bind_rows(
    reglog_pred %>% 
      roc_auc(truth = next_buy, .pred_Yes)
  )


# Dectree -----------------------------------------------------------

dectree_spec <- decision_tree() %>% 
  set_engine("rpart", model = TRUE) %>% 
  set_mode("classification")

dectree_workflow <- 
  workflow() %>% 
  add_recipe(ritel_recipe) %>% 
  add_model(dectree_spec)

dectree_model <- dectree_workflow %>% 
  fit(data = ritel_training)

library(rpart.plot)
dectree_model %>% 
  extract_fit_engine() %>% 
  rpart.plot(type = 5, extra = 104, tweak = 1.2)


# Performance -------------------------------------------------------

dectree_pred <- dectree_model %>% 
  augment(new_data = ritel_testing)

dectree_pred %>% 
  count(next_buy)

dectree_pred %>% 
  count(.pred_class)

dectree_pred %>% 
  conf_mat(truth = next_buy, estimate = .pred_class) 

dectree_pred %>% 
  conf_mat(truth = next_buy, estimate = .pred_class) %>% 
  autoplot()

dectree_pred %>% 
  conf_mat(truth = next_buy, estimate = .pred_class) %>% 
  autoplot(type = "heatmap")

dectree_pred %>% 
  conf_mat(truth = next_buy, estimate = .pred_class) %>% 
  tidy() %>% 
  mutate(pct = value/nrow(dectree_pred))

dectree_pred %>% 
  eval_metrics_class(truth = next_buy, estimate = .pred_class) %>% 
  bind_rows(
    dectree_pred %>% 
      roc_auc(truth = next_buy, .pred_Yes)
  )

reglog_pred %>% 
  eval_metrics_class(truth = next_buy, estimate = .pred_class) %>% 
  bind_rows(
    reglog_pred %>% 
      roc_auc(truth = next_buy, .pred_Yes)
  )

# Random Forest ------------------------------------------------------

rf_spec <- rand_forest() %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

rf_workflow <- 
  workflow() %>% 
  add_recipe(ritel_recipe) %>% 
  add_model(rf_spec)

rf_model <- rf_workflow %>% 
  fit(data = ritel_training)

# Performance --------------------------------------------------------

rf_pred <- rf_model %>% 
  augment(new_data = ritel_testing)

rf_pred %>% 
  count(next_buy)

rf_pred %>% 
  count(.pred_class)

rf_pred %>% 
  conf_mat(truth = next_buy, estimate = .pred_class) 

rf_pred %>% 
  conf_mat(truth = next_buy, estimate = .pred_class) %>% 
  autoplot()

rf_pred %>% 
  conf_mat(truth = next_buy, estimate = .pred_class) %>% 
  autoplot(type = "heatmap")

rf_pred %>% 
  conf_mat(truth = next_buy, estimate = .pred_class) %>% 
  tidy() %>% 
  mutate(pct = value/nrow(rf_pred))

rf_pred %>% 
  eval_metrics_class(truth = next_buy, estimate = .pred_class) %>% 
  bind_rows(
    rf_pred %>% 
      roc_auc(truth = next_buy, .pred_Yes)
  )

# Hyperparameter Tunning multimodels ---------------------------------------

reglog_spec <- logistic_reg(penalty = tune(), 
                            mixture = tune()) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")

dectree_spec <- decision_tree(cost_complexity = tune(), 
                              tree_depth = tune(), 
                              min_n = tune()) %>% 
  set_engine("rpart", model = TRUE) %>% 
  set_mode("classification")

rf_spec <- rand_forest(mtry = tune(), 
                       min_n = tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

xgb_spec <- boost_tree(mtry = tune(), 
                       min_n = tune(), 
                       tree_depth = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")



model_sets <- workflow_set(
  preproc = list(basic = ritel_recipe), 
  models = list(logreg = reglog_spec, 
                dtree = dectree_spec, 
                rf = rf_spec, 
                xgb = xgb_spec), 
  cross = TRUE
)

library(finetune)

race_ctrl <-
  control_race(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
  )

race_metrics <- metric_set(accuracy, roc_auc)

race_results <- model_sets %>%
  workflow_map(
    "tune_race_anova",
    seed = 123,
    resamples = cv_folds,
    grid = 10,
    metrics = race_metrics,
    control = race_ctrl, 
    verbose = TRUE
  )

# Performance --------------------------------------------------------

race_results %>% 
  autoplot()

race_results %>% 
  autoplot(
    rank_metric = "roc_auc",  
    metric = "roc_auc",
    select_best = TRUE) +
  geom_text(aes(y = mean - 0.005, label = wflow_id), 
            angle = 90, hjust = 1) +
  lims(y = c(0.85, 0.95)) +
  theme(legend.position = "none")


# Finalizing Model

best_param <- race_results %>% 
  extract_workflow_set_result("basic_xgb") %>% 
  select_best(metric = "roc_auc")

best_param


final_result <- race_results %>% 
  extract_workflow("basic_xgb") %>% 
  finalize_workflow(best_param) %>% 
  last_fit(split = ritel_split)

collect_metrics(final_result)

final_model <- final_result %>% 
  extract_workflow()


best_model_pred <- final_model %>% 
  augment(new_data = ritel_testing)

best_model_pred %>% 
  count(next_buy)

best_model_pred %>% 
  count(.pred_class)

best_model_pred %>% 
  conf_mat(truth = next_buy, estimate = .pred_class) 

best_model_pred %>% 
  conf_mat(truth = next_buy, estimate = .pred_class) %>% 
  autoplot()

best_model_pred %>% 
  conf_mat(truth = next_buy, estimate = .pred_class) %>% 
  autoplot(type = "heatmap")

best_model_pred %>% 
  conf_mat(truth = next_buy, estimate = .pred_class) %>% 
  tidy() %>% 
  mutate(pct = value/nrow(best_model_pred))

best_model_pred %>% 
  accuracy(truth = next_buy, estimate = .pred_class)

best_model_pred %>% 
  eval_metrics_class(truth = next_buy, estimate = .pred_class)

best_model_pred %>% 
  roc_curve(truth = next_buy, .pred_Yes) %>% 
  autoplot()

best_model_pred %>% 
  roc_auc(truth = next_buy, .pred_Yes)

# Compare ------------------------------------------------------------

reglog_auc <- reglog_pred %>% 
  roc_auc(truth = next_buy, .pred_Yes) %>% 
  pull(.estimate)

reglog <- reglog_pred %>% 
  roc_curve(truth = next_buy, .pred_Yes) %>% 
  autoplot() + 
  labs(subtitle = paste("AUC =", round(reglog_auc, 4))) + 
  ggtitle("Logistic Regression")


dectree_auc <- dectree_pred %>% 
  roc_auc(truth = next_buy, .pred_Yes) %>% 
  pull(.estimate)

dtree <- dectree_pred %>% 
  roc_curve(truth = next_buy, .pred_Yes) %>% 
  autoplot() + 
  labs(subtitle = paste("AUC =", round(dectree_auc, 4))) + 
  ggtitle("Decision Tree")


rf_auc <- rf_pred %>% 
  roc_auc(truth = next_buy, .pred_Yes) %>% 
  pull(.estimate)

rf <- rf_pred %>% 
  roc_curve(truth = next_buy, .pred_Yes) %>% 
  autoplot() + 
  labs(subtitle = paste("AUC =", round(rf_auc, 4))) + 
  ggtitle("Random Forest")


xgb_auc <- best_model_pred %>% 
  roc_auc(truth = next_buy, .pred_Yes) %>% 
  pull(.estimate)

xgb <- best_model_pred %>% 
  roc_curve(truth = next_buy, .pred_Yes) %>% 
  autoplot() + 
  labs(subtitle = paste("AUC =", round(xgb_auc, 4))) + 
  ggtitle("XGBoost")

library(gridExtra)
grid.arrange(reglog, dtree, rf, xgb, ncol = 2)

compare_roc <- bind_rows(
  reglog_pred %>% 
    roc_curve(truth = next_buy, .pred_Yes) %>% 
    mutate(Algorithm = "Reglog"),
  dectree_pred %>% 
    roc_curve(truth = next_buy, .pred_Yes) %>% 
    mutate(Algorithm = "DecTree"),
  rf_pred %>% 
    roc_curve(truth = next_buy, .pred_Yes) %>% 
    mutate(Algorithm = "RF"),
  best_model_pred %>%
    roc_curve(truth = next_buy, .pred_Yes) %>%
    mutate(Algorithm = "XGBoost")
)

compare_roc %>% 
  ggplot(aes(x = 1 - specificity, 
             y = sensitivity, 
             color = Algorithm)) + 
  geom_line() + 
  coord_fixed() + 
  theme_light()


# Model Interpetation -----------------------------------------------

library(DALEXtra)

explainer <- 
  explain_tidymodels(
    model = final_model, 
    data = ritel_training, 
    y = ritel_training$next_buy == "Yes",
    verbose = TRUE, 
    label = "Ritel Next Buy Prediction"
    )

set.seed(1)
x <- ritel_testing %>% 
  slice_sample(n = 3, by = next_buy)
new_obs <- x %>% 
  mutate(row_id = row.names(.)) %>% 
  column_to_rownames("row_id")

final_model %>% 
  predict(new_obs)

library(modelStudio)
modelStudio(
  explainer = explainer, 
  new_observation = new_obs, 
  new_observation_y = new_obs$next_buy, 
  max_features = 15, 
  facet_dim = c(2, 3)
  )











