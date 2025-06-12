# ----------------------------------------------------
# BEMM463 Final Report - R Script
# Candidate Number: 030665
# This script performs customer segmentation for Chestnut Ridge
# ----------------------------------------------------

# Load required libraries
library(tidyverse)
library(NbClust)
library(flexclust)

# ----------------------------------------------------
# Task 1.1: Import Data and Perform Descriptive Analysis
# ----------------------------------------------------
retailer <- read.csv("retailer.csv")  # Ensure the file is in the working directory
summary(retailer)
View(retailer)  # Optional: opens data viewer in RStudio

# ----------------------------------------------------
# Task 1.2: Normalize Selected Variables Using Z-Score
# ----------------------------------------------------
selected_vars <- retailer %>%
  select(variety_of_choice, electronics, furniture, quality_of_service, low_prices, return_policy)

normalized_data <- scale(selected_vars)
summary(normalized_data)

# ----------------------------------------------------
# Task 1.3 - 1.8: Hierarchical Clustering with Ward’s Method
# ----------------------------------------------------
set.seed(123)
distance_matrix <- dist(normalized_data, method = "euclidean")
hc_model <- hclust(distance_matrix, method = "ward.D2")

# Plot dendrogram for 3 clusters
plot(hc_model, main = "Dendrogram using Ward's Method (3 Clusters)", xlab = "", sub = "")
rect.hclust(hc_model, k = 3, border = 2:4)

# ----------------------------------------------------
# Task 1.9: Number of Observations in 3 Clusters
# ----------------------------------------------------
clusters_3 <- cutree(hc_model, k = 3)
table(clusters_3)

# ----------------------------------------------------
# Task 1.10: K-Means Clustering with k = 3
# ----------------------------------------------------
set.seed(123)
kmeans_model_3 <- kmeans(normalized_data, centers = 3, nstart = 100, iter.max = 1000)
table(kmeans_model_3$cluster)

# ----------------------------------------------------
# Task 1.11-1.12: 4-Cluster Hierarchical and K-Means Comparison
# ----------------------------------------------------
# Dendrogram for 4-cluster solution
plot(hc_model, main = "Dendrogram - 4 Clusters", xlab = "", sub = "")
rect.hclust(hc_model, k = 4, border = 2:5)

# Hierarchical clustering with 4 clusters
clusters_4 <- cutree(hc_model, k = 4)
table(clusters_4)

# K-means with 4 clusters
set.seed(123)
kmeans_model_4 <- kmeans(normalized_data, centers = 4, nstart = 100, iter.max = 1000)
table(kmeans_model_4$cluster)

# ----------------------------------------------------
# Task 1.13: Optimal Cluster Validation with NbClust
# ----------------------------------------------------
nbclust_results <- NbClust(normalized_data, min.nc = 2, max.nc = 6, method = "ward.D2")

# ----------------------------------------------------
# Task 2.1: Cluster Profiling using Flexclust
# ----------------------------------------------------
# Attach cluster labels from K-means (3 clusters)
cluster_labels <- kmeans_model_3$cluster
retailer_clustered <- cbind(selected_vars, cluster = cluster_labels)

# Compute average values per cluster
cluster_means <- aggregate(. ~ cluster, data = retailer_clustered, mean)
print(cluster_means)

# Save to CSV (optional for report)
write.csv(cluster_means, "cluster_means.csv", row.names = FALSE)

# Flexclust profile plot
kcca_model <- as.kcca(kmeans_model_3, normalized_data)
plot(kcca_model, data = normalized_data, main = "Flexclust Profile Plot")

# ----------------------------------------------------
# Task 2.2 and Task 3: Segment Insights and Strategy (in report)
# ----------------------------------------------------
# Interpretation of segments and GE Matrix evaluation to be completed in the Word report.


