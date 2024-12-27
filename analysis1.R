# Load required packages
library(dplyr)
library(ggplot2)
library(car)

# Load the dataset
data_clean <- read.csv("path_to_your_file/train.csv")

# Inspect the dataset
str(data_clean)
summary(data_clean)

# Data Cleaning and Preprocessing ----------------------------------------

# Handle missing values (example: drop rows with missing Postal Code)
data_clean <- data_clean %>% drop_na(Postal.Code)

# Convert columns to appropriate types
data_clean$Order.Date <- as.Date(data_clean$Order.Date, format = "%d/%m/%Y")
data_clean$Ship.Date <- as.Date(data_clean$Ship.Date, format = "%d/%m/%Y")
data_clean$Ship.Mode <- as.factor(data_clean$Ship.Mode)
data_clean$Region <- as.factor(data_clean$Region)
data_clean$Sales <- as.numeric(as.character(data_clean$Sales))

# Remove duplicates
data_clean <- data_clean[!duplicated(data_clean), ]

# Check structure after cleaning
str(data_clean)

# Hypothesis 1: Does Shipping Mode Affect Sales? ------------------------

# Perform One-Way ANOVA
anova_ship_mode <- aov(Sales ~ Ship.Mode, data = data_clean)
summary(anova_ship_mode)

# Tukey's HSD Test for pairwise comparisons (if ANOVA is significant)
tukey_ship_mode <- TukeyHSD(anova_ship_mode)
print(tukey_ship_mode)

# Visualize Sales by Ship Mode
ggplot(data_clean, aes(x = Ship.Mode, y = Sales)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Sales by Shipping Mode", x = "Shipping Mode", y = "Sales") +
  theme_minimal()

# Hypothesis 2: Does Region Influence Sales? ----------------------------

# Perform One-Way ANOVA
anova_region <- aov(Sales ~ Region, data = data_clean)
summary(anova_region)

# Tukey's HSD Test for pairwise comparisons (if ANOVA is significant)
tukey_region <- TukeyHSD(anova_region)
print(tukey_region)

# Visualize Sales by Region
ggplot(data_clean, aes(x = Region, y = Sales)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Sales by Region", x = "Region", y = "Sales") +
  theme_minimal()

# Assumptions Validation -----------------------------------------------

# Normality Check
# Shapiro-Wilk test for Sales within each Ship Mode
normality_results <- data_clean %>%
  group_by(Ship.Mode) %>%
  summarise(p_value = shapiro.test(Sales)$p.value)
print(normality_results)

# Shapiro-Wilk test for Sales within each Region
normality_results_region <- data_clean %>%
  group_by(Region) %>%
  summarise(p_value = shapiro.test(Sales)$p.value)
print(normality_results_region)

# Homogeneity of Variance
# Levene's Test for Ship Mode
leveneTest(Sales ~ Ship.Mode, data = data_clean)

# Levene's Test for Region
leveneTest(Sales ~ Region, data = data_clean)
