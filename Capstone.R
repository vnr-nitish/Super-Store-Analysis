install.packages("dplyr")
install.packages("ggplot2")
install.packages("data.table")
install.packages("caTools")
install.packages('reshape2')

library(reshape2)
library(dplyr)      # for data manipulation
library(ggplot2)    # for plotting
library(data.table) # for efficient data handling
library(caTools)

library(readr)
df <- read_csv("C:/Users/hp/Downloads/Super Store.csv")
View(df)

# Display the first few rows
head(df)

# Display the unique values in each categorical column
unique(df$`Ship Mode`)
unique(df$Segment)
unique(df$Country)
unique(df$Category)
unique(df$`Sub-Category`)
unique(df$Region)

# Statistical description of the data
summary(df)

# Information about the dataset structure
str(df)

# Checking for missing values
colSums(is.na(df))

# Load the scales library for formatting
library(scales)

# Sales analysis based on region (Bar Plot)
sales_by_region <- df %>% group_by(Region) %>% summarize(Sales = sum(Sales))
ggplot(sales_by_region, aes(x = Region, y = Sales, fill = Region)) + 
  geom_bar(stat = "identity") + D
labs(title = "Sales Analysis by Region", x = "Region", y = "Sales") +
  scale_y_continuous(labels = scales::comma) +  # format y-axis
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

# Profit analysis based on region (Bar Plot)
profit_by_region <- df %>% group_by(Region) %>% summarize(Profit = sum(Profit))
ggplot(profit_by_region, aes(x = Region, y = Profit, fill = Region)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Profit Analysis by Region", x = "Region", y = "Profit") +
  scale_y_continuous(labels = scales::comma) +  # format y-axis
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

# Sales analysis based on region (Pie Chart)
ggplot(sales_by_region, aes(x = "", y = Sales, fill = Region)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Sales Analysis by Region (Pie Chart)") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

# Profit analysis based on region (Pie Chart)
ggplot(profit_by_region, aes(x = "", y = Profit, fill = Region)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Profit Analysis by Region (Pie Chart)") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

# Sales analysis based on segment (Bar Plot)
sales_by_segment <- df %>% group_by(Segment) %>% summarize(Sales = sum(Sales))
ggplot(sales_by_segment, aes(x = Segment, y = Sales, fill = Segment)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Sales Analysis by Segment", x = "Segment", y = "Sales") +
  scale_y_continuous(labels = scales::comma) +  # format y-axis
  scale_fill_manual(values = c("dodgerblue", "tomato", "goldenrod")) +
  theme_minimal()

# Profit analysis based on segment (Bar Plot)
profit_by_segment <- df %>% group_by(Segment) %>% summarize(Profit = sum(Profit))
ggplot(profit_by_segment, aes(x = Segment, y = Profit, fill = Segment)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Profit Analysis by Segment", x = "Segment", y = "Profit") +
  scale_y_continuous(labels = scales::comma) +  # format y-axis
  scale_fill_manual(values = c("dodgerblue", "tomato", "goldenrod")) +
  theme_minimal()


# Sales analysis based on category (Bar Plot)
sales_by_category <- df %>% group_by(Category) %>% summarize(Sales = sum(Sales))
ggplot(sales_by_category, aes(x = Category, y = Sales, fill = Category)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Sales Analysis by Category", x = "Category", y = "Sales") +
  scale_y_continuous(labels = scales::comma) +  # format y-axis
  scale_fill_manual(values = c("skyblue", "lightcoral", "palegreen")) +
  theme_minimal()

# Profit analysis based on category (Bar Plot)
profit_by_category <- df %>% group_by(Category) %>% summarize(Profit = sum(Profit))
ggplot(profit_by_category, aes(x = Category, y = Profit, fill = Category)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Profit Analysis by Category", x = "Category", y = "Profit") +
  scale_y_continuous(labels = scales::comma) +  # format y-axis
  scale_fill_manual(values = c("skyblue", "lightcoral", "palegreen")) +
  theme_minimal()

# Sales and Profit Pie Charts for Category
ggplot(sales_by_category, aes(x = "", y = Sales, fill = Category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Sales Analysis by Category (Pie Chart)") +
  scale_fill_manual(values = c("skyblue", "lightcoral", "palegreen")) +
  theme_minimal()

ggplot(profit_by_category, aes(x = "", y = Profit, fill = Category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Profit Analysis by Category (Pie Chart)") +
  scale_fill_manual(values = c("skyblue", "lightcoral", "palegreen")) +
  theme_minimal()

# Sales analysis based on state (Bar Plot)
sales_by_state <- df %>%
  group_by(State) %>%
  summarize(Sales = sum(Sales))
ggplot(sales_by_state, aes(x = State, y = Sales)) +
  geom_bar(stat = "identity") +
  labs(title = "Sales Analysis by State", x = "State", y = "Sales") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Profit analysis based on state (Bar Plot)
profit_by_state <- df %>%
  group_by(State) %>%
  summarize(Profit = sum(Profit))
ggplot(profit_by_state, aes(x = State, y = Profit)) +
  geom_bar(stat = "identity") +
  labs(title = "Profit Analysis by State", x = "State", y = "Profit") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Step 1: Create a binary outcome for "High Profit"
# Using median profit value as the threshold to classify high and low profit
threshold <- median(df$Profit, na.rm = TRUE)
df <- df %>%
  mutate(High_Profit = ifelse(Profit > threshold, 1, 0))  # 1 for high profit, 0 for low profit

# Step 2: Split the data into training and testing sets
set.seed(123)  # For reproducibility
sample <- sample.split(df$High_Profit, SplitRatio = 0.7)
train <- subset(df, sample == TRUE)
test <- subset(df, sample == FALSE)

# Step 3: Fit the logistic regression model
# Predicting High_Profit based on Sales, Discount, and Quantity
model <- glm(High_Profit ~ Sales + Discount + Quantity, data = train, family = binomial)

# Summary of the model to check coefficients and model fit
summary(model)

# Step 4: Predict on the test set
# Get predicted probabilities for the test set
pred_prob <- predict(model, test, type = "response")

# Convert probabilities to binary predictions with a threshold of 0.5
pred_class <- ifelse(pred_prob > 0.5, 1, 0)

# Step 5: Evaluate the model
# Confusion matrix to check accuracy
confusion_matrix <- table(Predicted = pred_class, Actual = test$High_Profit)
print(confusion_matrix)

# Extract values from confusion matrix
TP <- confusion_matrix[2, 2]  # True Positives
TN <- confusion_matrix[1, 1]  # True Negatives
FP <- confusion_matrix[2, 1]  # False Positives
FN <- confusion_matrix[1, 2]  # False Negatives

# Calculate precision and recall
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * ((precision * recall) / (precision + recall))

cat("Precision:", round(precision, 2), "\n")
cat("Recall:", round(recall, 2), "\n")
cat("F1 Score:", round(f1_score, 2), "\n")

# Calculate accuracy
accuracy <- mean(pred_class == test$High_Profit)
print(paste("Accuracy:", round(accuracy, 2)))

# Selecting numerical columns from the dataset
numerical_data <- df %>% select(Sales, Quantity, Discount, Profit)

# Calculate the correlation matrix
correlation_matrix <- cor(numerical_data, use = "complete.obs")
print("Correlation Matrix:")
print(correlation_matrix)

# Reshape for ggplot
correlation_melted <- melt(correlation_matrix)

# Plot heatmap
ggplot(data = correlation_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Matrix Heatmap", x = "Variables", y = "Variables")

# Step 6: Optional Visualization
# Plot predicted probabilities and actual outcomes for test data
ggplot(test, aes(x = pred_prob, fill = factor(High_Profit))) +
  geom_histogram(position = "dodge", bins = 30) +
  labs(title = "Predicted Probability vs Actual Outcome", x = "Predicted Probability", fill = "High Profit") +
  theme_minimal()