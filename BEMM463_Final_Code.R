# BEMM463 Final Report - R Script
# Candidate Number: 740054869
# Script to perform clustering analysis and segmentation for Chestnut Ridge case study
# Load necessary libraries
install.packages("tidyverse")
install.packages("conflicted")
install.packages("factoextra")
install.packages("NbClust")
install.packages("flexclust")

library(tidyverse)
library(conflicted)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(factoextra)
library(NbClust)
library(flexclust)

# Resolve function conflicts
conflict_default_conflicts(strict = TRUE)
conflict_prefer_all("tidyverse", quiet = FALSE)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

# Load and inspect data
retailer_data <- read_csv("retailer.csv.csv")
retailer_data <- na.omit(retailer_data)

# Select relevant columns and normalize
clustering_data <- retailer_data %>%
  select(electronics, quality_of_service, low_prices, income, age)
scaled_data <- scale(clustering_data)

# Hierarchical clustering
distance_matrix <- dist(scaled_data)
hc_model <- hclust(distance_matrix, method = "ward.D2")

# Save dendrogram
png("dendrogram_day4.png", width = 1000, height = 800)
plot(hc_model, main = "Hierarchical Clustering Dendrogram", xlab = "", sub = "", labels = FALSE, hang = -1)
dev.off()

# K-means clustering (3 clusters)
set.seed(123)
kmeans_model <- kmeans(scaled_data, centers = 3, nstart = 25)
retailer_data$kmeans_cluster <- kmeans_model$cluster
table(retailer_data$kmeans_cluster)  # Output: 70, 63, 67

# Visualize 3-cluster result
fviz_cluster(kmeans_model, data = scaled_data,
             ellipse.type = "euclid",
             palette = "jco",
             ggtheme = theme_minimal())

# Cluster profiling
cluster_summary <- aggregate(clustering_data, by = list(Cluster = retailer_data$kmeans_cluster), mean)
write.csv(cluster_summary, "cluster_summary_day5.csv", row.names = FALSE)

# Bar plot of cluster means
retailer_data$kmeans_cluster <- factor(retailer_data$kmeans_cluster)
retailer_data_melted <- melt(retailer_data,
                             id.vars = "kmeans_cluster",
                             measure.vars = c("electronics", "quality_of_service", "low_prices", "income", "age"))

ggplot(retailer_data_melted, aes(x = variable, y = value, fill = kmeans_cluster)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  labs(title = "Average Variable Scores by Cluster", x = "Feature", y = "Average Value") +
  theme_minimal()

# 4-cluster solution for comparison
set.seed(123)
kmeans_4 <- kmeans(scaled_data, centers = 4, iter.max = 1000, nstart = 100)
table(kmeans_4$cluster)  # Output: 28, 67, 65, 40

# NbClust for optimal cluster suggestion
set.seed(123)
nb_results <- NbClust(scaled_data, distance = "euclidean", min.nc = 2, max.nc = 6, method = "ward.D2")
png("nbclust_cluster_suggestion.png", width = 1000, height = 800)
barplot(table(nb_results$Best.nc[1,]), xlab = "Number of Clusters", ylab = "Number of Criteria",
        main = "NbClust - Cluster Number Suggestion")
dev.off()

# Flexclust profile plot
km_flex <- as.kcca(kmeans_model, data = scaled_data)
png("flexclust_profile_plot.png", width = 1000, height = 800)
plot(km_flex, main = "Cluster Profile Plot (flexclust)")
dev.off()
