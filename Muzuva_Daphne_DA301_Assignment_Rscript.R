read.csv(file.choose())
setwd("C:/Users/daphn/OneDrive/Documents/LSE/LSE_DA301_Modules/LSE_DA301_Assignment_files")
getwd()
setwd("C:/Users/daphn/OneDrive/Documents/LSE/LSE_DA301_Modules/LSE_DA301_Assignment_files")
# Import the CSV file
turtle_sales <- read.csv("turtle_sales.csv")
# Check the first few rows of the imported data
head(turtle_sales)
summary(turtle_sales)

# Load the required libraries
library(tidyverse)
# Use the filter function from dplyr
dplyr::filter(data_frame, conditions)

library(tidyverse)

# Import the CSV file
turtle_sales <- read.csv("turtle_sales.csv")
# Convert to a tibble (optional but recommended for dplyr functions)
turtle_sales <- as_tibble(turtle_sales)

library(dplyr)

# Create a subset of the data frame without the specified columns
subset_data <- turtle_sales %>%
  select(-Ranking, -Year, -Genre, -Publisher)

# Create a summary of the new data frame
data_summary <- summary(subset_data)

# Load the ggplot2 library
library(ggplot2)

# Create a scatterplot
ggplot(subset_data, aes(x = Quantity, y = Price)) +
  geom_point() +
  labs(title = "Scatterplot of Quantity vs. Price",
       x = "Quantity",
       y = "Price")

# Load the ggplot2 library
library(ggplot2)

# Create a scatterplot
ggplot(subset_data, aes(x = Quantity, y = Price)) +
  geom_point() +
  labs(title = "Scatterplot of Quantity vs. Price",
       x = "Quantity",
       y = "Price")
# Display the structure of subset_data
str(subset_data)

# Load the ggplot2 library
library(ggplot2)

# Create a scatterplot for NA_Sales vs. EU_Sales
ggplot(subset_data, aes(x = NA_Sales, y = EU_Sales)) +
  geom_point() +
  labs(title = "Scatterplot of NA Sales vs. EU Sales",
       x = "NA Sales",
       y = "EU Sales")

# Create a histogram for NA_Sales
hist(subset_data$NA_Sales, 
     main = "Histogram of NA Sales",
     xlab = "NA Sales",
     ylab = "Frequency")

# Create a boxplot for Global_Sales
boxplot(subset_data$Global_Sales, 
        main = "Boxplot of Global Sales",
        ylab = "Global Sales")

# Create a boxplot for NA_Sales
boxplot(subset_data$NA_Sales, 
        main = "Boxplot of NA Sales",
        ylab = "NA Sales")

# Create a boxplot for EU_Sales
boxplot(subset_data$EU_Sales, 
        main = "Boxplot of EU Sales",
        ylab = "EU Sales")

# Create a histogram for Global_Sales
hist(subset_data$Global_Sales, 
     main = "Histogram of Global Sales",
     xlab = "Global Sales",
     ylab = "Frequency")

# Create a histogram for EU_Sales
hist(subset_data$EU_Sales, 
     main = "Histogram of EU Sales",
     xlab = "EU Sales",
     ylab = "Frequency")

# Load the ggplot2 library
library(ggplot2)

# Create a stacked histogram for all three sales columns
ggplot(subset_data, aes(x = NA_Sales + EU_Sales + Global_Sales)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.5) +
  labs(title = "Stacked Histogram of Sales Comparison",
       x = "Sales",
       y = "Frequency") +
  facet_grid(. ~ variable, scales = "free_x")

# Load the required libraries
library(reshape2)
library(ggplot2)

# Reshape the data into a long format
melted_data <- melt(subset_data, id.vars = "Year",
                    measure.vars = c("NA_Sales", "EU_Sales", "Global_Sales"))

# Create a stacked histogram
ggplot(melted_data, aes(x = value, fill = variable)) +
  geom_histogram(binwidth = 1, position = "stack") +
  labs(title = "Stacked Histogram of Sales Comparison",
       x = "Sales",
       y = "Frequency") +
  scale_fill_manual(values = c("NA_Sales" = "blue", "EU_Sales" = "red", "Global_Sales" = "green"))

# Load the ggplot2 library
library(ggplot2)

# Create a histogram comparing EU_Sales to Global_Sales
ggplot(subset_data, aes(x = EU_Sales - Global_Sales)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.5) +
  labs(title = "Histogram of EU Sales Minus Global Sales",
       x = "EU Sales - Global Sales",
       y = "Frequency")

# Calculate the percentage of NA_Sales relative to Global_Sales
subset_data$NA_Sales_Percentage <- (subset_data$NA_Sales / subset_data$Global_Sales) * 100

# Load the ggplot2 library
library(ggplot2)

# Create a histogram of NA_Sales as a percentage of Global_Sales
ggplot(subset_data, aes(x = NA_Sales_Percentage)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.5) +
  labs(title = "Histogram of NA Sales as Percentage of Global Sales",
       x = "NA Sales as Percentage of Global Sales (%)",
       y = "Frequency")


# Load the ggplot2 library
library(ggplot2)

# Create a line graph to compare NA_Sales, EU_Sales, and Global_Sales over time
ggplot(subset_data, aes(x = Year)) +
  geom_line(aes(y = NA_Sales, color = "NA Sales"), size = 1) +
  geom_line(aes(y = EU_Sales, color = "EU Sales"), size = 1) +
  geom_line(aes(y = Global_Sales, color = "Global Sales"), size = 1) +
  labs(title = "Comparison of Sales Over Time",
       x = "Year",
       y = "Sales") +
  scale_color_manual(values = c("NA Sales" = "blue", "EU Sales" = "red", "Global Sales" = "green")) +
  theme_minimal()

# Load the ggplot2 library
library(ggplot2)

# Create a line graph to compare NA_Sales, EU_Sales, and Global_Sales
ggplot(subset_data, aes(x = 1:nrow(subset_data))) +
  geom_line(aes(y = NA_Sales, color = "NA Sales"), size = 1) +
  geom_line(aes(y = EU_Sales, color = "EU Sales"), size = 1) +
  geom_line(aes(y = Global_Sales, color = "Global Sales"), size = 1) +
  labs(title = "Comparison of Sales",
       x = "Data Point",
       y = "Sales") +
  scale_color_manual(values = c("NA Sales" = "blue", "EU Sales" = "red", "Global Sales" = "green")) +
  theme_minimal()

# Module 5
# Load and explore the data frame you prepared in Module 4.
setwd("C:/Users/daphn/OneDrive/Documents/LSE/LSE_DA301_Modules/LSE_DA301_Assignment_files")
# Import the CSV file
turtle_sales <- read.csv("turtle_sales.csv")
# Check the first few rows of the imported data
head(turtle_sales)
summary(turtle_sales)

# View the data frame to sense-check the data set.
# Check the first few rows of the imported data
head(turtle_sales)
summary(turtle_sales)

# Determine the min, max and mean values of all the sales data 

# Calculate the minimum, maximum, and mean values for each sales column
min_na_sales <- min(turtle_sales$NA_Sales)
max_na_sales <- max(turtle_sales$NA_Sales)
mean_na_sales <- mean(turtle_sales$NA_Sales)

min_eu_sales <- min(turtle_sales$EU_Sales)
max_eu_sales <- max(turtle_sales$EU_Sales)
mean_eu_sales <- mean(turtle_sales$EU_Sales)

min_global_sales <- min(turtle_sales$Global_Sales)
max_global_sales <- max(turtle_sales$Global_Sales)
mean_global_sales <- mean(turtle_sales$Global_Sales)

# Print the results
cat("Minimum NA_Sales:", min_na_sales, "\n")
cat("Maximum NA_Sales:", max_na_sales, "\n")
cat("Mean NA_Sales:", mean_na_sales, "\n\n")

cat("Minimum EU_Sales:", min_eu_sales, "\n")
cat("Maximum EU_Sales:", max_eu_sales, "\n")
cat("Mean EU_Sales:", mean_eu_sales, "\n\n")

cat("Minimum Global_Sales:", min_global_sales, "\n")
cat("Maximum Global_Sales:", max_global_sales, "\n")
cat("Mean Global_Sales:", mean_global_sales, "\n")

# Create a summary of the data frame.
summary(turtle_sales)

# Determine the impact on sales per product_id.
# Use the group_by, apply(), and/or aggregate functions 
# to sum the values grouped by product. 
# Create a summary of the new data frame

# Load the dplyr library
library(dplyr)

# Group the data by product_id and calculate the sum of sales for each product
sales_summary <- turtle_sales %>%
  group_by(Product) %>%
  summarize(
    Total_NA_Sales = sum(NA_Sales),
    Total_EU_Sales = sum(EU_Sales),
    Total_Global_Sales = sum(Global_Sales)
  )

# Print the summary of the new data frame
summary(sales_summary)

# Create plots to review and determine insights into the data set.
# Create scatterplots, histograms, and boxplots to gain insights 
# into the sales data.
# Note your observations and diagrams to provide insights

# Create scatterplots to understand the relationships between 
# different sales columns, 

# Load the ggplot2 library
library(ggplot2)

# Scatterplot of NA_Sales vs. EU_Sales
ggplot(turtle_sales, aes(x = NA_Sales, y = EU_Sales)) +
  geom_point() +
  labs(title = "Scatterplot of NA Sales vs. EU Sales",
       x = "NA Sales",
       y = "EU Sales")

# Histogram of NA_Sales
ggplot(turtle_sales, aes(x = NA_Sales)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.5) +
  labs(title = "Histogram of NA Sales",
       x = "NA Sales",
       y = "Frequency")

# Boxplot of EU_Sales
ggplot(turtle_sales, aes(y = EU_Sales)) +
  geom_boxplot(fill = "red") +
  labs(title = "Boxplot of EU Sales",
       x = "",
       y = "EU Sales")

# To determine the normality of the sales data, 
# create Q-Q plots, perform the Shapiro-Wilk test, 
# calculate skewness and kurtosis, and check for correlations 
# between sales data columns, use R's statistical functions 
# visualization libraries

# Normality check
# Load necessary libraries
library(ggplot2)
library(stats)

# Create Q-Q plots for all sales data columns
qqnorm(turtle_sales$NA_Sales, main = "Q-Q Plot for NA Sales")
qqline(turtle_sales$NA_Sales, col = "red")

qqnorm(turtle_sales$EU_Sales, main = "Q-Q Plot for EU Sales")
qqline(turtle_sales$EU_Sales, col = "red")

qqnorm(turtle_sales$Global_Sales, main = "Q-Q Plot for Global Sales")
qqline(turtle_sales$Global_Sales, col = "red")

# Perform Shapiro-Wilk test for normality
shapiro.test(turtle_sales$NA_Sales)
shapiro.test(turtle_sales$EU_Sales)
shapiro.test(turtle_sales$Global_Sales)

install.packages("moments")
library(moments)

# Calculate skewness and kurtosis for all sales data columns
skew_na_sales <- skewness(turtle_sales$NA_Sales)
kurt_na_sales <- kurtosis(turtle_sales$NA_Sales)

skew_eu_sales <- skewness(turtle_sales$EU_Sales)
kurt_eu_sales <- kurtosis(turtle_sales$EU_Sales)

skew_global_sales <- skewness(turtle_sales$Global_Sales)
kurt_global_sales <- kurtosis(turtle_sales$Global_Sales)

# Print skewness and kurtosis values
cat("NA Sales Skewness:", skew_na_sales, "Kurtosis:", kurt_na_sales, "\n")
cat("EU Sales Skewness:", skew_eu_sales, "Kurtosis:", kurt_eu_sales, "\n")
cat("Global Sales Skewness:", skew_global_sales, "Kurtosis:", kurt_global_sales, "\n")

# Positive skewness indicates a right-skewed distribution, 
# while negative skewness indicates a left-skewed distribution. 
# Kurtosis measures the "tailedness" of the distribution.

# Calculate correlations between sales data columns
correlation_matrix <- cor(turtle_sales[c("NA_Sales", "EU_Sales", "Global_Sales")])

# Print the correlation matrix
correlation_matrix

# The correlation matrix shows the correlation coefficients between
# the sales data columns. Positive values indicate positive 
# correlation, negative values indicate negative correlation, 
# and values close to zero indicate weak or no correlation

# Create plots to gain insights into the sales data. 
# Choose the type of plot that best suits the data set 
# Compare all the sales data (columns) for any correlation(s).
# Add a trend line to the plots for ease of interpretation.

# Create scatterplots and correlation matrices. 
# Scatterplots will help visualize relationships, and adding 
# trend lines can make these relationships more interpretable.

# Scatterplots with Trend Lines:

# Load libraries
library(ggplot2)

# Create scatterplots with trend lines for sales data
ggplot(turtle_sales, aes(x = NA_Sales, y = EU_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of NA Sales vs. EU Sales",
       x = "NA Sales",
       y = "EU Sales")

ggplot(turtle_sales, aes(x = NA_Sales, y = Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatterplot of NA Sales vs. Global Sales",
       x = "NA Sales",
       y = "Global Sales")

ggplot(turtle_sales, aes(x = EU_Sales, y = Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(title = "Scatterplot of EU Sales vs. Global Sales",
       x = "EU Sales",
       y = "Global Sales")

# In these scatterplots, we're comparing "NA_Sales" with "EU_Sales,"
# "NA_Sales" with "Global_Sales," and "EU_Sales" with "Global_Sales." 
# We use geom_smooth() with the "lm" method to add trend lines, 
# making it easier to interpret relationships.

# Correlation Matrix
# Calculate correlations between sales data columns
correlation_matrix <- cor(turtle_sales[c("NA_Sales", "EU_Sales", "Global_Sales")])

# Print the correlation matrix
correlation_matrix

# The correlation matrix will show correlation coefficients between
# sales data columns. Positive values indicate positive correlations,
# negative values indicate negative correlations, and values close 
# to zero indicate weak or no correlation.

# Scatterplots with trend lines allow us to visually assess the 
# relationships between different sales columns. The trend lines 
# show the direction and strength of the relationship, making it 
# easier to interpret.

# The correlation matrix quantifies the relationships between sales
# data columns. It provides numerical values for correlations, 
# helping us understand the degree to which one sales column is 
# related to another.

# By combining these visualizations and correlation calculations, 
# we gain insights into the sales data, identify trends, and 
# understand the strength and direction of relationships between 
# different sales regions (NA, EU, Global).

# Create a simple linear regression model, 
# Determine the correlation between the sales columns, 
# Create plots to visualize the linear regression using the lm() function in R. 

# Calculate correlations between sales data columns
correlation_matrix <- cor(turtle_sales[c("NA_Sales", "EU_Sales", "Global_Sales")])

# Print the correlation matrix
correlation_matrix

# Load necessary libraries
library(ggplot2)

# Create a scatterplot with the linear regression line
ggplot(turtle_sales, aes(x = NA_Sales, y = Global_Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Linear Regression: Global Sales vs. NA Sales",
       x = "NA Sales",
       y = "Global Sales")

# Create a multiple linear regression model, select only the 
# numeric columns, determine the correlation between the sales 
# columns, and view the output, 
  
# Select only numeric columns
numeric_columns <- turtle_sales[, sapply(turtle_sales, is.numeric)]

# Calculate correlations between numeric columns
correlation_matrix <- cor(numeric_columns)

# Print the correlation matrix
correlation_matrix

# Create a multiple linear regression model
multiple_linear_model <- lm(Global_Sales ~ NA_Sales + EU_Sales, data = turtle_sales)

# View the summary of the multiple linear regression model
summary(multiple_linear_model)

# To predict global sales based on provided values of NA_Sales and
# EU_Sales sums and compare the predictions to the observed values,
# we use the multiple linear regression model 

# Load necessary libraries
library(dplyr)

# Define the sums of NA_Sales and EU_Sales for each scenario
scenarios <- data.frame(NA_Sales = c(34.02, 3.93, 2.73, 2.26, 22.08),
                        EU_Sales = c(23.80, 1.56, 0.65, 0.97, 0.52))

# Predict Global_Sales for each scenario using the multiple linear regression model
predictions <- predict(multiple_linear_model, newdata = scenarios)

# Create a data frame to compare observed and predicted values
comparison <- data.frame(Observed_Global_Sales = c(67.80, 33.00, 29.40, 27.10, 25.70),
                         Predicted_Global_Sales = predictions)

# Print the comparison
print(comparison)
 






