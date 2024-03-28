# Load necessary libraries
library(readr)
library(tidyverse)
library(modeldata)
library(GGally)


#load data
data <- read.csv("D:/_MIAA/MEIA/csv/apple_quality.csv")

# Attempt to convert Acidity to numeric and identify non-numeric values
data$Acidity_numeric <- as.numeric(as.character(data$Acidity))
non_numeric <- is.na(data$Acidity_numeric) & !is.na(data$Acidity)

# Display the non-numeric values
data$Acidity[non_numeric]


# Replace NAs with the mean or median of the column (mean mais representativa, acho eu)
data$Acidity_numeric[is.na(data$Acidity_numeric)] <- mean(data$Acidity_numeric, na.rm = TRUE)
# Or use median
# data$Acidity_numeric[is.na(data$Acidity_numeric)] <- median(data$Acidity_numeric, na.rm = TRUE)


# Optionally, you might want to replace the original Acidity column with the cleaned numeric version
data$Acidity <- data$Acidity_numeric
data$Acidity_numeric <- NULL  # Remove the temporary column

# Final check to ensure no NAs remain
summary(data)




# Check for NA values
summary(data)

# Remove rows with any NA values
data <- na.omit(data)

# Check data
summary(data)

#Visualizing Distributions of Individual Variables
#histograms
# Using the base R plotting system
hist(data$Size, main="Distribution of Size", xlab="Size")

# If you prefer ggplot2
library(ggplot2)
ggplot(data, aes(x=Size)) +
  geom_histogram(binwidth=0.5, fill="blue", color="black") +
  theme_minimal() +
  labs(title="Distribution of Size", x="Size", y="Count")

#Density plots
# Using ggplot2 for a density plot
ggplot(data, aes(x=Sweetness)) +
  geom_density(fill="green") +
  labs(title="Density of Sweetness", x="Sweetness")


#Visualizing Relationships Between Variables
# scatter plots
plot(data$Sweetness, data$Crunchiness, main="Sweetness vs Crunchiness", xlab="Sweetness", ylab="Crunchiness")

#Pair Plots
# Requires the GGally package for ggpairs()
ggpairs(data[,c("Size", "Weight", "Sweetness", "Crunchiness", "Juiciness", "Ripeness", "Acidity")])

#correlation matrix
# Calculate correlation matrix
cor_matrix <- cor(data[,sapply(data, is.numeric)])

# Use ggplot2 to create a heatmap
library(reshape2)
melted_cor_matrix <- melt(cor_matrix)
ggplot(melted_cor_matrix, aes(Var1, Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(midpoint=0, low="blue", mid="white", high="red") +
  theme_minimal() +
  labs(title="Correlation Matrix Heatmap")

#Categorical Variables
# Bar plot for Quality
table(data$Quality)  # To see the distribution
barplot(table(data$Quality), main="Distribution of Apple Quality", xlab="Quality", ylab="Frequency")



#Handling Categorical Variables
data$QualityNumeric <- ifelse(data$Quality == "good", 1, 0)


#Splitting Data into Training and Testing Sets

library(caret)

# Converted 'Quality' into a numeric variable 'QualityNumeric'

independent_vars <- c("Size", "Weight", "Sweetness", "Crunchiness", "Juiciness", "Ripeness", "Acidity")
dependent_var_linear <- "Sweetness" # For linear regression
dependent_var_logistic <- "QualityNumeric" # For logistic regression

# Data splitting for linear regression
set.seed(123) # For reproducibility
indexes <- createDataPartition(data[[dependent_var_linear]], p=0.8, list=FALSE)
train_data_linear <- data[indexes,]
test_data_linear <- data[-indexes,]

# Data splitting for logistic regression
set.seed(123)
indexes <- createDataPartition(data[[dependent_var_logistic]], p=0.8, list=FALSE)
train_data_logistic <- data[indexes,]
test_data_logistic <- data[-indexes,]


#Normalizing or Scaling Features
# Scaling for linear regression using the scale function
# This is not always necessary, but it's beneficial for some models

# Scaling training data
train_data_linear_scaled <- as.data.frame(scale(train_data_linear[, independent_vars]))
# Add the dependent variable back
train_data_linear_scaled$Sweetness <- train_data_linear$Sweetness

# Scaling testing data
test_data_linear_scaled <- as.data.frame(scale(test_data_linear[, independent_vars]))
# Add the dependent variable back
test_data_linear_scaled$Sweetness <- test_data_linear$Sweetness

#Work with acidity

#Clean the Acidity Column

#load data
data <- read.csv("D:/_MIAA/MEIA/csv/apple_quality.csv")
# Convert Acidity to numeric, handling potential conversion issues
data$Acidity <- as.numeric(as.character(data$Acidity))

# Check for NA values that were introduced and decide how to handle them
# For simplicity, we'll replace NAs with the median, but you could also remove these rows
data$Acidity[is.na(data$Acidity)] <- median(data$Acidity, na.rm = TRUE)


#Perform EDA with Visualizations
# Histograms for each numeric variable
par(mfrow=c(3,3)) # Organize plots in a 3x3 grid
hist(data$Size, main="Size", xlab="Size")
hist(data$Weight, main="Weight", xlab="Weight")
hist(data$Sweetness, main="Sweetness", xlab="Sweetness")
hist(data$Crunchiness, main="Crunchiness", xlab="Crunchiness")
hist(data$Juiciness, main="Juiciness", xlab="Juiciness")
hist(data$Ripeness, main="Ripeness", xlab="Ripeness")
hist(data$Acidity, main="Acidity", xlab="Acidity")

#Visualizing Relationships Between Variables
# Pairs plot to visualize relationships
pairs(~Size + Weight + Sweetness + Crunchiness + Juiciness + Ripeness + Acidity, data=data)

#Distribution of the Categorical Variable
# Bar plot for Quality
quality_distribution <- table(data$Quality)
barplot(quality_distribution, main="Distribution of Quality", xlab="Quality", ylab="Count")

#Correlation MAtrix
# Calculate and visualize the correlation matrix
cor_matrix <- cor(data[,sapply(data, is.numeric)]) # Compute correlation matrix for numeric columns
library(corrplot)
corrplot(cor_matrix, method="circle")

#Continuous Linear Regression
# Prepare data
# Data is already loaded and cleaned, including the Acidity conversion
# Exclude 'A_id' and 'Quality' from the analysis
regression_data <- data[, !(names(data) %in% c("A_id", "Quality"))]

#Fit the Linear Regression Model
# Fit the linear model
model <- lm(Sweetness ~ ., data=regression_data)

# Display the summary of the model to see coefficients, R-squared, etc.
summary(model)

#Interpret the Model Output


#




#Evaluate the Model's Assumptions

# Plot diagnostics for the linear model
par(mfrow=c(2,2))
plot(model)


#These plots provide a visual way to check some of the key assumptions:


#Make Predictions
# Predictions on the same data (for demonstration)
predictions <- predict(model, regression_data)

# Compare predictions with actual Sweetness values
head(predictions)
head(regression_data$Sweetness)


#Binary Logistic Regression

#Prepare the Data
# Coding 'Quality' as binary (good=1, bad=0)
data$QualityNumeric <- ifelse(data$Quality == "good", 1, 0)

# Select only the relevant columns for the logistic regression model 
logistic_data <- data[, c("Size", "Weight", "Sweetness", "Crunchiness", "Juiciness", "Ripeness", "Acidity", "QualityNumeric")]


#Fit the Logistic Regression Model
# Fit the logistic regression model
logistic_model <- glm(QualityNumeric ~ ., data=logistic_data, family=binomial)

# Display the summary of the model
summary(logistic_model)

##Interpret the Model Output



#Model Diagnostics and Evaluation
# Predictions
fitted_results <- predict(logistic_model, type="response")
predicted_class <- ifelse(fitted_results > 0.5, 1, 0)

# Confusion Matrix
library(caret)
confusionMatrix(as.factor(predicted_class), as.factor(logistic_data$QualityNumeric))


#Making Predictions
# Example prediction for new data
new_data <- data.frame(Size = c(0.5), Weight = c(-0.2), Sweetness = c(1.0), Crunchiness = c(0.8), Juiciness = c(0.5), Ripeness = c(-0.3), Acidity = c(0.4))
predicted_quality <- predict(logistic_model, newdata = new_data, type = "response")
predicted_class_new <- ifelse(predicted_quality > 0.5, "good", "bad")
predicted_class_new





