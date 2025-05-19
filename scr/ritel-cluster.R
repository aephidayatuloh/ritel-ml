library(tidyverse)
library(tidymodels)
library(tidyclust)

# K-Means

abt_cluster <- abt %>% 
  select(-MemberID, -next_buy)

pca <- abt_cluster %>% 
  prcomp(center = TRUE, scale. = TRUE)

pca %>% 
  broom::tidy("pcs")

# Find optimum k
set.seed(1234)

ritel_cv <- abt_cluster %>% 
  vfold_cv(v = 5)

kmeans_spec_tune <- k_means(num_clusters = tune()) %>% 
  set_engine("stats")

cluster_recipe <- abt_cluster %>% 
  recipe(~ .) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_pca(threshold = 0.9)

ritel_wf <- workflow(
  preprocessor = cluster_recipe, 
  spec = kmeans_spec_tune
  )

maxk <- 10
grid <- tibble(num_clusters = 2:maxk)

find_k_wss <- 
  ritel_wf %>% 
  tune_cluster(
    resamples = ritel_cv, 
    grid = grid, 
    control = control_grid(save_pred = TRUE, extract = identity),
    metrics = cluster_metric_set(sse_within_total, sse_ratio)
    )

find_k_wss %>% 
  collect_metrics()

find_k_wss %>% 
  autoplot()

# cluster_ctrl <- control_grid(
#   save_pred = TRUE, 
#   extract = identity
#   )
# 
# find_k_silhouette <-
#   ritel_wf %>%
#   tune_cluster(
#     resamples = ritel_cv,
#     grid = grid,
#     control = cluster_ctrl, 
#     metrics = cluster_metric_set(silhouette_avg)
#   )
# 
# find_k_silhouette %>%
#   collect_metrics()
# 
# find_k_silhouette %>%
#   autoplot()

optimk <- 6
kmeans_spec <- k_means(num_clusters = optimk) %>% 
  set_engine("stats")

ritel_wf <- ritel_wf %>% 
  update_model(spec = kmeans_spec)

ritel_kmeans <- ritel_wf %>% 
  fit(data = abt_cluster)

ritel_kmeans %>% 
  extract_centroids() %>% 
  print(width = Inf)


ritel_kmeans %>% 
  extract_cluster_assignment()

cluster_result <- ritel_kmeans %>% 
  augment(new_data = abtx)

cluster_result %>% 
  count(.pred_cluster) %>% 
  mutate(pct = n/sum(n))


cluster_result %>% 
  count(.pred_cluster) %>% 
  mutate(Variable = "member") %>% 
  pivot_wider(id_cols = Variable, 
              names_from = .pred_cluster, 
              values_from = n) %>% 
  bind_rows(
    cluster_result %>% 
      count(.pred_cluster) %>% 
      mutate(Variable = "percent member", 
             pct = n/sum(n)) %>% 
      pivot_wider(id_cols = Variable, names_from = .pred_cluster, values_from = pct), 
    cluster_result %>% 
      select(-MemberID) %>% 
      rownames_to_column("id") %>% 
      pivot_longer(cols = -c(id, .pred_cluster), names_to = "Variable", values_to = "Values") %>% 
      group_by(.pred_cluster, Variable) %>% 
      summarise(
        avg = mean(Values), 
        .groups = "drop"
      ) %>% 
      pivot_wider(id_cols = Variable, names_from = .pred_cluster, values_from = avg)
  ) 
