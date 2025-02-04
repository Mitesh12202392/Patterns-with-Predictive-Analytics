#  ------KNN model------

data(faithful)
head(faithful)

faithful$waiting_label <- ifelse(faithful$waiting > median(faithful$waiting), "long", "short")

faithful$waiting_label <- as.factor(faithful$waiting_label)

faithful$eruptions <- scale(faithful$eruptions)

set.seed(123)
train_indices <- sample(1:nrow(faithful), size = 0.7 * nrow(faithful))
train_data <- faithful[train_indices, ]
test_data <- faithful[-train_indices, ]

library(class)

k <- 5
knn_predictions <- knn(train = train_data[, "eruptions", drop = FALSE],
                       test = test_data[, "eruptions", drop = FALSE],
                       cl = train_data$waiting_label, 
                       k = k)

knn_accuracy <- mean(knn_predictions == test_data$waiting_label)
cat("KNN Accuracy:", knn_accuracy, "\n")

#  ------Naive Bayes model------

library(e1071)

nb_model <- naiveBayes(waiting_label ~ eruptions, data = train_data)

nb_predictions <- predict(nb_model, newdata = test_data)

nb_accuracy <- mean(nb_predictions == test_data$waiting_label)
cat("Naive Bayes Accuracy:", nb_accuracy, "\n")

#Comparing KNN and Naive Bayes model
table(KNN = knn_predictions, Actual = test_data$waiting_label)
table(NB = nb_predictions, Actual = test_data$waiting_label)


#Clustering: K-means and Hierarchical 

#  ------K-means Clustering------

data(faithful)

faithful_normalized <- scale(faithful)
head(faithful_normalized)

set.seed(123)
wss <- sapply(1:10, function(k) {
  kmeans(faithful_normalized, centers = k, nstart = 10)$tot.withinss
})

plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method for Determining k")

kmeans_result <- kmeans(faithful_normalized, centers = 2, nstart = 10)

faithful$cluster <- as.factor(kmeans_result$cluster)

library(ggplot2)
ggplot(faithful, aes(x = eruptions, y = waiting, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "K-Means Clustering of faithful Dataset", 
       x = "Eruption Duration", 
       y = "Waiting Time")

#  ------Hierarchical Clustering------

distance_matrix <- dist(faithful_normalized)

hclust_result <- hclust(distance_matrix, method = "ward.D2")

plot(hclust_result, main = "Hierarchical Clustering Dendrogram",
     xlab = "", sub = "", cex = 0.8)

hclust_clusters <- cutree(hclust_result, k = 2)

faithful$hcluster <- as.factor(hclust_clusters)

ggplot(faithful, aes(x = eruptions, y = waiting, color = hcluster)) +
  geom_point(size = 3) +
  labs(title = "Hierarchical Clustering of faithful Dataset", 
       x = "Eruption Duration", 
       y = "Waiting Time")

# Comparing the cluster assignments from K-Means and Hierarchical Clustering
table(KMeans = faithful$cluster, Hierarchical = faithful$hcluster)