# Decision Tree for Exploration 
library(rpart)
library(rpart.plot)
library(yardstick)

dtree <- cluster_result %>% 
  select(-.pred_cluster) %>% 
  rpart(formula = cluster_label ~ ., 
        data = ., 
        control = rpart.control(
          maxdepth = 4, 
          minsplit = 1
        )
  )

dtree %>% 
  rpart.plot()

dtree %>% 
  rpart.plot(
    type = 4, 
    extra = 104, 
    tweak = 1.1, 
    under = TRUE, 
    box.palette = "auto", 
    fallen.leaves = TRUE, 
    legend.cex = 0.1
  )

final_cs_cluster <- cs_cluster %>% 
  mutate(class_tree = dtree %>% 
           predict(cs_cluster, type = "class"), 
         prob_tree = dtree %>% 
           predict(cs_cluster, type = "prob")
  )

final_cs_cluster %>% 
  conf_mat(truth = .pred_cluster, estimate = class_tree)

final_cs_cluster %>% 
  accuracy(truth = .pred_cluster, estimate = class_tree)

final_cs_cluster %>% 
  specificity(truth = .pred_cluster, estimate = class_tree)

final_cs_cluster %>% 
  sensitivity(truth = .pred_cluster, estimate = class_tree)
