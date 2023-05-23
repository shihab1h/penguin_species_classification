###############################################################################
# PROJECT DETAILS


#------------------------------------------------------------------------------
# ADMINSTRATIVE

# Name:         Shihab Hamati
# Matricola:    985941

# Module:       Statistical Learning
# Exam Date:    03 Nov 2022

# Part 2:       Unsupervised Learning

#------------------------------------------------------------------------------
# REFERENCES

# Description:  
# - A dataset consisting of 4 physical measurements of Antarctic penguins
# - It is known to consist of 3 species
# - This information is hidden from the clustering functions
# - It is used in lieu of experts to explore the meaning of clustering results



###############################################################################
# LIBRARIES

library(palmerpenguins)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(factoextra)
library(hopkins)
library(cluster)
library(ggfortify)



###############################################################################
# DATA SETUP

head(penguins)
str(penguins)
summary(penguins)


#------------------------------------------------------------------------------
# CLEAN DATASET

penguins[complete.cases(penguins), ]
data_full <- na.omit(penguins)
colnames(data_full)
data <- data_full[,3:6]
summary(data)



###############################################################################
# EXPLORATORY DATA ANALYSIS (EDA)

#------------------------------------------------------------------------------
# EACH NUMERIC FEATURE INDEPENDENTLY

p1a <- ggplot(data = data, aes(x = bill_length_mm)) +
  geom_histogram(aes(y = ..density..), colour="black", fill="white") + 
  geom_density(alpha = 0.1, fill = "red") + 
  xlab("Bill Length (mm)") +
  theme_minimal()

p1b <- ggplot(data = data, aes(x = bill_depth_mm)) +
  geom_histogram(aes(y = ..density..), colour="black", fill="white") + 
  geom_density(alpha = 0.1, fill = "yellow") + 
  xlab("Bill Depth (mm)") + 
  theme_minimal()

p1c <- ggplot(data = data, aes(x = flipper_length_mm)) +
  geom_histogram(aes(y = ..density..), colour="black", fill="white") + 
  geom_density(alpha = 0.1, fill = "green") + 
  xlab("Flipper Length (mm)") + 
  theme_minimal()

p1d <- ggplot(data = data, aes(x = body_mass_g)) +
  geom_histogram(aes(y = ..density..), colour="black", fill="white") + 
  geom_density(alpha = 0.1, fill = "blue") + 
  xlab("Body Mass (g)") + 
  theme_minimal()

grid.arrange(p1a, p1b, p1c, p1d, 
             ncol=2, top = "Distributions of Individual Penguin Features")


#------------------------------------------------------------------------------
# HOW FEATURES DIFFER ACROSS SPECIES

p2a <- ggplot(data = data_full, aes(x = bill_length_mm, 
                               group = species, fill = species)) +
  geom_histogram(position = "identity", alpha = 0.5) + 
  theme_minimal()

p2b <- ggplot(data = data_full, aes(x = bill_depth_mm, 
                               group = species, fill = species)) +
  geom_histogram(position = "identity", alpha = 0.5) + 
  theme_minimal()

p2c <- ggplot(data = data_full, aes(x = flipper_length_mm, 
                               group = species, fill = species)) +
  geom_histogram(position = "identity", alpha = 0.5) + 
  theme_minimal()

p2d <- ggplot(data = data_full, aes(x = body_mass_g, 
                               group = species, fill = species)) +
  geom_histogram(position = "identity", alpha = 0.5) + 
  theme_minimal()

grid.arrange(p2a, p2b, p2c, p2d, 
             ncol = 2, top = "Distributions Differences across Species")


#------------------------------------------------------------------------------
# HOW FEATURES DIFFER ACROSS SEX

p3a <- ggplot(data = data_full, aes(x = bill_length_mm, 
                                    group = sex, fill = sex)) +
  geom_histogram(position = "identity", alpha = 0.5) + 
  theme_minimal()

p3b <- ggplot(data = data_full, aes(x = bill_depth_mm, 
                                    group = sex, fill = sex)) +
  geom_histogram(position = "identity", alpha = 0.5) + 
  theme_minimal()

p3c <- ggplot(data = data_full, aes(x = flipper_length_mm, 
                                    group = sex, fill = sex)) +
  geom_histogram(position = "identity", alpha = 0.5) + 
  theme_minimal()

p3d <- ggplot(data = data_full, aes(x = body_mass_g, 
                                    group = sex, fill = sex)) +
  geom_histogram(position = "identity", alpha = 0.5) + 
  theme_minimal()

grid.arrange(p3a, p3b, p3c, p3d, 
             ncol = 2, top = "Distributions Differences across Sex")


#------------------------------------------------------------------------------
# Importance of Scaling Features

p4a <- boxplot(data, main = "Original Features")

data_scaled <- data.frame(scale(data))
summary(data_scaled)

p4b <- boxplot(data_scaled, main = "Scaled Features")


#------------------------------------------------------------------------------
# Corellelogram

corrplot(cor(data_scaled), method = 'number')



###############################################################################
# CLUSTERING:
# K-MEANS & HIERARCHICAL

#------------------------------------------------------------------------------
# Clusterability Test

set.seed(100)
hopkins(data_scaled) 
# indicates our dataset is very clusterable 
# since 0.96 is between 0.7-1, refer to documentation

#------------------------------------------------------------------------------
# Visualizing Variations across all Features

heatmap(as.matrix(data_scaled), cexCol = 1) 
# appears we might have 2-3 clusters

#------------------------------------------------------------------------------
# Identifying Optimal Number of Clusters
# METHOD 1: Elbow Method of WITHIN CLUSTER SUM of SQUARES (WCSS)

wcssplot <- function(data, kmax){
  wcss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (k in 2: kmax){
    set.seed(100)
    wcss[k] <- sum(kmeans(data, centers = k)$withinss)}
  plot(1:kmax, wcss, type="b", xlab="Number of Clusters",
       ylab="Within Cluster Sum of Squares")
  }

wcssplot(data_scaled, kmax = 8) 
# k = 3 appears to be an elbow in the WCSS plot

#..............................................................................
# K-MEANS CLUSTERING (k = 3)

set.seed(100)
kmeans_fit <- kmeans(data_scaled, 3)

fviz_cluster(kmeans_fit, data = data_scaled,
             main = "Cluster Plot - K-Means Clustering")

#..............................................................................
# Exploring the meaning of the clusters 
# - usually a target variable might not be present
# - the target variable is used after the clustering, in lieu of experts

table(kmeans_fit$cluster, data_full$species)
# clusters appear to line up with species


#------------------------------------------------------------------------------
# Identifying Optimal Number of Clusters
# METHOD 2: AVERAGE SILHOUETTE SCORE

sil_score <- function(k){
  km <- kmeans(data_scaled, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(data_scaled))
  mean(ss[, 3])
}

silplot <- function(kmax){
  k <- 2:kmax
  avg_silhouettes <- sapply(k, sil_score)
  plot(k,avg_silhouettes, type = 'b', xlab = "Clusters")
}

silplot(8)
# k = 2 clusters is optimal as it has highest avg sil score

#..............................................................................
# K-Means Clustering (k = 2)

set.seed(100)
kmeans_fit_k2 <- kmeans(data, 2)

fviz_cluster(kmeans_fit_k2, data = data,
             main = "Cluster Plot - K-Means Clustering")

#..............................................................................
# Exploring the meaning of the 2 clusters 

table(kmeans_fit_k2$cluster, data_full$species) 
# k = 2 seems to discriminate between Gentoo species and non-Gentoo 

table(kmeans_fit_k2$cluster, data_full$sex) 
# not clustered across gender

table(kmeans_fit_k2$cluster, data_full$island)
# appears to cluster across Biscoe island and the other 2 islands

table(data_full$species, data_full$island)
# however the species distribution is highly correlated to island 
# so this is a Gentoo vs non-Gentoo species clustering


#------------------------------------------------------------------------------
# Identifying Optimal Number of Clusters
# METHOD 3: HIERARCHICAL CLUSTERING

distances <- dist(data_scaled, method = "euclidean")
hier_fit <- hclust(distances, method="ward.D2")

plot(hier_fit, labels = FALSE)

# Multiple cut-off heights appear to yield an acceptable clustering result
abline(h = 30, col="blue", lty = 2)
abline(h = 15, col="red") # middle option is chosen, yields k = 3
abline(h = 8, col="blue", lty = 2)

# Re-plot dendogram with the 3 clusters
plot(hier_fit, labels = FALSE)
rect.hclust(hier_fit, k = 3, border = "red") 

# Assign the 3 clusters
h_clusters <- cutree(hier_fit, k = 3)

fviz_cluster(list(data = data_scaled, cluster = h_clusters), 
             main = "Cluster Plot - Hierarchical Clustering")

#..............................................................................
# Exploring the meaning of the 3 clusters 

table(h_clusters, data_full$species) # also appears to align with species



###############################################################################
# PRINCIPAL COMPONENT ANALYSIS (PCA)

pca_decomp <- prcomp(data_scaled)

pairs(data_full[colnames(data)])
      
# Plotting all pairs of numeric features (original data)
colors <- c('red', 'green', 'blue')[unclass(data_full$species)]
pairs(data_full[colnames(data)], 
      main = "Original Data Combinations (colored by species)", 
      col = colors)

# Plotting across the top 2 principal components
autoplot(pca_decomp, data = data_full, colour = 'species') + theme_minimal()
autoplot(pca_decomp, data = data_full) + theme_minimal()

# ..with loadings
autoplot(pca_decomp, data = data_full, colour = 'species', 
         loadings = TRUE, loadings.label = TRUE) + theme_minimal()
autoplot(pca_decomp, data = data_full, 
         loadings = TRUE, loadings.label = TRUE) + theme_minimal()

#..............................................................................
# Extract variance explained by each PC
# and the composition of each PC

pca_var <- get_pca_var(pca_decomp)

p5a <- fviz_contrib(pca_decomp, "var", axes = 1, xtickslab.rt = 0) +
  scale_x_discrete(limits = colnames(data_scaled))

p5b <- fviz_contrib(pca_decomp, "var", axes = 2, xtickslab.rt = 0) +
  scale_x_discrete(limits = colnames(data_scaled)) 

p5c <- fviz_contrib(pca_decomp, "var", axes = 3, xtickslab.rt = 0) +
  scale_x_discrete(limits = colnames(data_scaled)) 

p5d <- fviz_contrib(pca_decomp, "var", axes = 4, xtickslab.rt = 0) +
  scale_x_discrete(limits = colnames(data_scaled)) 

grid.arrange(p5a, p5b, p5c, p5d, ncol=2, 
             top = "Contribution of Features to each Principal Component")

#..............................................................................
# Kaiser Criterion: How many PCs to keep?

pca_eigen <- get_eigenvalue(pca_decomp)
barplot(pca_eigen$eigenvalue, names.arg = c("PC1", "PC2", "PC3", "PC4"), 
        main = "Eigenvalues of the Principal Components")
abline(h = 1, col = "red", lty = 2)
# Kaiser criterion: keep PCs with Eigenvalues > 1
# Only PC1 is retained

#..............................................................................
# Number of Clusters

wcssplot(pca_decomp$x[,1, drop = FALSE], kmax = 8) # elbow wcss
fviz_nbclust(pca_decomp$x, FUNcluster = kmeans, k.max = 8) # silhouette

# Elbow WCSS and Silhouette both suggest k = 2 clusters for PC1 data

#..............................................................................
# Comparisons (PCA+Clustering vs Clustering on full dataset)

# Comparing performance of 2-means clustering: PC1-reduced dataset vs Original
set.seed(100)
pca_kmeans_fit <- kmeans(pca_decomp$x[,1], 2)
table(pca_kmeans_fit$cluster, data_full$species) 
table(kmeans_fit_k2$cluster, data_full$species)

# Performing K-means clustering using the suggested 1 PC components 
# yields slightly less species clustering than using the whole dataset
# But if the range of features was huge, this could provide an efficient
# approach to achieving comparable clustering with less features and dimensions

set.seed(100)
pca_kmeans_fit <- kmeans(pca_decomp$x[,1], 3)
table(pca_kmeans_fit$cluster, data_full$species) 
table(kmeans_fit$cluster, data_full$species)



###############################################################################
# COMMENTARY

# While the data plots and Hopkins test suggest the clusterability of the data,
# different approaches point to varying optimal number of clusters, albeit
# close: 2 or 3 cluster groups

# In a real scenario, and since no "target" or "response" variable exists to
# give feedback to tuning the hyperparameter k (num of clusters), the groups
# have to be examined by experts in an attempt to understand what traits
# differentiate the clusters and how many clusters is practical and relevant



###############################################################################
# END