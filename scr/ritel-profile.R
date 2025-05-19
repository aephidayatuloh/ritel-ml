source("scr/fn_cluster_profile.R")

cluster_result  %>%  
  count(.pred_cluster) %>%  
  mutate(pct = n/sum(n)) %>%  
  ggplot(aes(x = .pred_cluster, y = n, fill = .pred_cluster)) + 
  geom_col(alpha = 0.75, show.legend = FALSE) + 
  geom_text(aes(label = percent(pct, accuracy = 0.1)), 
            vjust = -0.25, size = 8) + 
  scale_y_continuous(labels = comma_format(), limits = c(0, 17000)) +
  labs(y = "Jumlah Anggota Cluster") + 
  theme_bw()

p_NoChild <- cluster_result %>% 
  cluster_profiling(
    .variable = NoOfChild, 
    .cluster = .pred_cluster, 
    discrete = TRUE
  )

p_recency <- cluster_result %>% 
  cluster_profiling(
    .variable = recency, 
    .cluster = .pred_cluster, 
    discrete = FALSE, 
    pop.box.width = 0.001, 
    cluster.box.width = 0.2
  )

p_avg_monetary <- cluster_result %>% 
  cluster_profiling(
    .variable = avg_monetary, 
    .cluster = .pred_cluster, 
    discrete = FALSE, 
    pop.box.width = 0.0005, 
    cluster.box.width = 0.2
  )

p_monthly_freq <- cluster_result %>% 
  cluster_profiling(
    .variable = monthly_freq, 
    .cluster = .pred_cluster, 
    discrete = FALSE, 
    pop.box.width = 0.3, 
    cluster.box.width = 0.2
  )

p_interpurchase <- cluster_result %>% 
  cluster_profiling(
    .variable = interpurchase,
    pop.box.width = 0.005, 
    cluster.box.width = 0.2
  )

p_freq_last3m <- cluster_result %>% 
  cluster_profiling(
    freq_last3m, 
    pop.box.width = 0.01, 
    cluster.box.width = 0.2
  )

p_tenure <- cluster_result %>% 
  cluster_profiling(
    tenure, 
    pop.box.width = 0.005, 
    cluster.box.width = 0.2
  )

grid.arrange(
  p_NoChild, 
  p_recency, 
  p_tenure, 
  p_avg_monetary, 
  p_monthly_freq, 
  p_interpurchase, 
  p_freq_last3m, 
  ncol = 4
)


# Labeling ----------------------------------------------------------------

cluster_result <- cluster_result %>% 
  mutate(
    cluster_label = case_when(
      # Pelanggan loyal, belanja rutin, spending sedang
      .pred_cluster == "Cluster_1" ~ "Regular mid-spenders", 
      
      # Pelanggan loyalitas tinggi tapi frekuensi belanja menurun
      .pred_cluster == "Cluster_2" ~ "Established Family Shoppers", 
      .pred_cluster == "Cluster_3" ~ "Almost-Churn", 
      .pred_cluster == "Cluster_4" ~ "High-Value Active", 
      .pred_cluster == "Cluster_5" ~ "Premium with Toddlers", 
      .pred_cluster == "Cluster_6" ~ "New Potential")
  )
