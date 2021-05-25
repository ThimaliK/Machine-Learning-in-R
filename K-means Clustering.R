setwd("C:\Users\ACER\L05\ML")

library (readxl) # to read the dataset
library(tidyverse) # to view dataframes
library(ggplot2) # for box plots
library(cluster) # for k-means clustering
library (tidyr) # for outlier replacements
library(factoextra) # to visualize clustered groups

# original data set ------------------------------------------------------------

original_vehicles_dataset <- read_excel("vehicles.xlsx")   # all columns
summary(original_vehicles_dataset)
view(original_vehicles_dataset)

# Replacing Outliers -----------------------------------------------------------

# Detecting Outliers -----

#bus
original_vehicles_dataset %>%
  pivot_longer(2:19, names_to = "labels") %>%
  filter(Class == "bus") %>%
  ggplot(aes(Class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection: Bus")

#van
original_vehicles_dataset %>%
  pivot_longer(2:19, names_to = "labels") %>%
  filter(Class == "van") %>%
  #mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(Class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection: Van")

#saab
original_vehicles_dataset %>%
  pivot_longer(2:19, names_to = "labels") %>%
  filter(Class == "saab") %>%
  ggplot(aes(Class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection: Saab")

#opel
original_vehicles_dataset %>%
  pivot_longer(2:19, names_to = "labels") %>%
  filter(Class == "opel") %>%
  ggplot(aes(Class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection: Opel")

# Outlier Replacement -----

#bus
vehicles_bus = original_vehicles_dataset %>%
  filter(Class == "bus") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

vehicles_bus %>%
  pivot_longer(2:19, names_to = "labels") %>%
  filter(Class == "bus") %>%
  ggplot(aes(Class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "After outlier replacement: Bus")

#van
vehicles_van = original_vehicles_dataset %>%
  filter(Class == "van") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

vehicles_van %>%
  pivot_longer(2:19, names_to = "labels") %>%
  filter(Class == "van") %>%
  ggplot(aes(Class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "After outlier replacement: Van")

#saab
vehicles_saab = original_vehicles_dataset %>%
  filter(Class == "saab") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

vehicles_saab %>%
  pivot_longer(2:19, names_to = "labels") %>%
  filter(Class == "saab") %>%
  ggplot(aes(Class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "After outlier replacement: Saab")

#opel
vehicles_opel = original_vehicles_dataset %>%
  filter(Class == "opel") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

vehicles_opel %>%
  pivot_longer(2:19, names_to = "labels") %>%
  filter(Class == "opel") %>%
  ggplot(aes(Class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "After outlier replacement: Opel")

# full data frame after replacing outliers--------------------------------------

view(vehicles_bus)
full_dataframe <- rbind(vehicles_bus, vehicles_opel, vehicles_saab, vehicles_van)
view(full_dataframe)

outlier_replaced_full_df <- full_dataframe
summary(outlier_replaced_full_df)

# Data Normalization -----------------------------------------------------------

Class <- full_dataframe$Class

# Z-score
normalized_features <- as.data.frame(scale(full_dataframe[2:19]))

# min max
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
normalized_features <- lapply(full_dataframe[2:19], normalize)

normalized_full_dataset <- cbind(Class, data.frame(normalized_features))

view(normalized_full_dataset)

summary(normalized_full_dataset)


# Reducing Input Dimentionality - PCA ------------------------------------------

df_vehicles_pca <- prcomp(normalized_full_dataset[2:19])
summary(df_vehicles_pca)

plot(df_vehicles_pca, xlab("Principle Component Analysis"))
plot(df_vehicles_pca, type='l', xlab("Principle Component Analysis"))

pca_vehicles_data <- data.frame(df_vehicles_pca$x[,1:6])
View(pca_vehicles_data)

# Elbow Method -----------------------------------------------------------------

tot.withinss <- vector(mode="character", length=10)                     
for (i in 1:10){
  vehiclesCluster <- kmeans(pca_vehicles_data, center=i, nstart=20)
  tot.withinss[i] <- vehiclesCluster$tot.withinss
}

plot(1:10, tot.withinss, type="b", pch=4)

# distance ---------------------------------------------------------------------

distance <- get_dist(pca_vehicles_data, method="manhattan")
fviz_dist(distance, gradient = list(low="red", mid="green", high="blue"))

# k-means clustering -----------------------------------------------------------

set.seed(12345)

kc <- kmeans(pca_vehicles_data, 4, nstart = 25)
kc

fviz_cluster(kc, pca_vehicles_data)

# Evaluation of the results ----------------------------------------------------

confusion_matrix <- table(Class, kc$cluster)
print(confusion_matrix)

kc$centers