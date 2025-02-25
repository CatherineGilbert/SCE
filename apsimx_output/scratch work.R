#Create new trials file for testing dates and maturities














data <- final_dt2

# Function to calculate sums of squares
sums_of_squares <- function(data, groups) {
  # Overall mean of the data
  overall_mean <- colMeans(data)
  
  # Split the data into groups
  split_data <- split(data, groups)
  
  # Initialize WSS, BSS, and TSS
  WSS <- vector()
  BSS <- 0
  TSS <- 0
  
  # Calculate WSS and BSS
  for (group in split_data) {
    group_means <- colMeans(group) # Group means
    n_j <- nrow(group) # Number of observations in the group
    
    # Within Sums of Squares (WSS)
    squared_diffs <- sweep(group, 2, group_means, "-")^2
    WSS <- append(WSS, sum(squared_diffs))
    
    # Between Sums of Squares (BSS)
    BSS <- BSS + n_j * sum((group_means - overall_mean)^2)
  }
  
  # Total Sums of Squares (TSS)
  TSS <- sum(sweep(data, 2, overall_mean, "-")^2)
  
  names(WSS) <- names(split_data)
  
  # Return results
  return(list(
    totss = TSS, # Total Sums of Squares
    withinss = WSS, # Within-Cluster Sums of Squares
    tot.withinss = sum(WSS), # Total Within Sums of Squares
    betweenss = BSS # Between Sums of Squares
  ))
}

# Calculate sums of squares
results <- sums_of_squares(data, final_x$Site)
print(results)
results$betweenss / results$totss 


site_wss <- tibble(Site = names(results$withinss), WithinSS = as.numeric(results$withinss))

left_join(trials_x, site_wss) %>% esquisser()


# Load necessary libraries
library(ggplot2)
library(FactoMineR)
library(factoextra)

category <- final_x$Site

# Perform PCA on numeric variables
pca_result <- PCA(final_dt2, scale.unit = TRUE, graph = FALSE)

# Extract principal component scores
pca_scores <- as.data.frame(pca_result$ind$coord)
pca_scores$category <- category

# Plot PCA results with categorical variable
ggplot(pca_scores, aes(x = Dim.1, y = Dim.2, color = category)) +
  geom_text(label = gfinal$ID_Loc, size = 3) +
  theme_minimal() +
  labs(title = "PCA of Numeric Variables",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Category")

# Perform ANOVA on PC1 scores
anova_result <- aov(Dim.1 ~ category, data = pca_scores)
summary(anova_result)

# Perform ANOVA on PC2 scores
anova_result <- aov(Dim.2 ~ category, data = pca_scores)
summary(anova_result)

# Load the vegan library
library(vegan)

# Perform PERMANOVA
adonis_result <- adonis2(final_dt2 ~ category, data = gfinal, method = "euclidean")
print(adonis_result)

cca_dt <- final_dt %>%
  column_to_rownames("ID") %>%
  select(!any_of(correlated))

# Perform CCA
cca_result <- cca(cca_dt ~ category)
cca_result

#constrained cca shows the proportion of variance explained by the predictor variables
# in this case, 26% of variation in the 


# Explanation:
#   PCA: Reduces the dimensionality of the numeric variables into principal components (PCs). The first few PCs capture most of the variation in the data.
#   Visualization: Coloring the PCA plot by the categorical variable helps visually assess whether the groups are separated along the PCs.
#   ANOVA: Tests whether the means of the principal component scores differ significantly across the groups of the categorical variable.
#   PERMANOVA: Tests whether the overall variation in the numeric variables is significantly influenced by the categorical variable.
#   CCA: Explicitly models the relationship between the numeric variables and the categorical variable.
# 
# Output:
#   PCA Plot: A scatterplot of the first two principal components, colored by the categorical variable.
#   ANOVA Results: P-values indicating whether the categorical variable significantly influences the principal component scores.
#   PERMANOVA Results: P-values indicating whether the categorical variable significantly influences the overall variation in the numeric variables.
#   CCA Results: Summary of the relationship between the numeric variables and the categorical variable.
# Let me know if you need further clarification!


manova_result <- manova(as.matrix(final_dt2) ~ as.factor(category))
summary(manova_result)
