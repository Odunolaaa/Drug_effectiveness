# Load necessary libraries
library(readr)
library(dplyr)

# Load the data
Drug_clean <- read_csv("C:/Users/atite/Downloads/drug_dataset/Drug_clean.csv")

drug_data <- Drug_clean

# View the first few rows of the data
head(drug_data)

# Check the structure of the data
str(drug_data)

# Get a summary of the data
summary(drug_data)

# Check for missing values
sum(is.na(drug_data))

# Summary statistics for numeric variables
drug_data %>%
  summarise(
    Avg_Effectiveness = mean(Effective),
    Avg_EaseOfUse = mean(EaseOfUse),
    Avg_Satisfaction = mean(Satisfaction),
    Total_Reviews = sum(Reviews)
  )


# Average performance by condition
performance_by_condition <- drug_data %>%
  group_by(Condition) %>%
  summarise(
    Avg_Effectiveness = mean(Effective),
    Avg_EaseOfUse = mean(EaseOfUse),
    Avg_Satisfaction = mean(Satisfaction)
  )

# View the result
print(performance_by_condition)

# Evaluation by Drug Type
performance_by_type <- drug_data %>%
  group_by(Drug) %>%
  summarise(
    Avg_Effectiveness = mean(Effective, na.rm = TRUE),
    Avg_EaseOfUse = mean(EaseOfUse, na.rm = TRUE),
    Avg_Satisfaction = mean(Satisfaction, na.rm = TRUE)
  )

# View the result to confirm it was created successfully
print(performance_by_type)
# Load ggplot2 for visualization
library(ggplot2)

# Boxplot for effectiveness by condition
ggplot(drug_data, aes(x = Condition, y = Effective)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Effectiveness by Condition", y = "Effectiveness", x = "Condition")

# Bar plot for average satisfaction by drug type
ggplot(performance_by_type, aes(x = Drug, y = Avg_Satisfaction)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Satisfaction by Drug Type", y = "Average Satisfaction", x = "Drug Type")


# Linear regression to predict satisfaction
model <- lm(Satisfaction ~ Effective + EaseOfUse, data = drug_data)
summary(model)


# Load ggplot2 for visualization
library(ggplot2)

# Scatter plot with the fitted line for Satisfaction vs. Effectiveness
ggplot(drug_data, aes(x = Effective, y = Satisfaction)) +
  geom_point(color = "blue", alpha = 0.5) +  # Points for actual data
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Fitted line
  labs(title = "Satisfaction vs. Effectiveness",
       x = "Effectiveness",
       y = "Satisfaction") +
  theme_minimal()
#

# Scatter plot with the fitted line for Satisfaction vs. Ease of Use
ggplot(drug_data, aes(x = EaseOfUse, y = Satisfaction)) +
  geom_point(color = "green", alpha = 0.5) +  # Points for actual data
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Fitted line
  labs(title = "Satisfaction vs. Ease of Use",
       x = "Ease of Use",
       y = "Satisfaction") +
  theme_minimal()


# Plotting residuals vs. fitted values
ggplot(model, aes(x = .fitted, y = .resid)) +
  geom_point(color = "purple", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# Find the top 10 drugs by average satisfaction
top_10_drugs <- drug_data %>%
  group_by(Drug) %>%
  summarise(Avg_Satisfaction = mean(Satisfaction, na.rm = TRUE)) %>%
  top_n(10, Avg_Satisfaction)

# Bar plot for top 10 drugs by average satisfaction
ggplot(top_10_drugs, aes(x = reorder(Drug, Avg_Satisfaction), y = Avg_Satisfaction, fill = Drug)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip the axes for better readability
  labs(title = "Top 10 Drugs by Average Satisfaction",
       x = "Drug",
       y = "Average Satisfaction") +
  theme_minimal() +
  theme(legend.position = "none")


# Dot plot for top 10 drugs by average satisfaction
ggplot(top_10_drugs, aes(x = reorder(Drug, Avg_Satisfaction), y = Avg_Satisfaction)) +
  geom_point(size = 4, color = "blue") +
  coord_flip() +  # Flip the axes for better readability
  labs(title = "Top 10 Drugs by Average Satisfaction",
       x = "Drug",
       y = "Average Satisfaction") +
  theme_minimal()


# Lollipop chart for top 10 drugs by average satisfaction
ggplot(top_10_drugs, aes(x = reorder(Drug, Avg_Satisfaction), y = Avg_Satisfaction)) +
  geom_segment(aes(x = reorder(Drug, Avg_Satisfaction), xend = reorder(Drug, Avg_Satisfaction), y = 0, yend = Avg_Satisfaction), color = "grey") +
  geom_point(size = 4, color = "blue") +
  coord_flip() +  # Flip the axes for better readability
  labs(title = "Top 10 Drugs by Average Satisfaction",
       x = "Drug",
       y = "Average Satisfaction") +
  theme_minimal()


# Create a shorter label by taking the first letters of each drug in the combination
top_10_drugs$Short_Drug <- sapply(strsplit(top_10_drugs$Drug, "-"), function(x) paste(substr(x, 1, 3), collapse = "-"))

# Plot with the shortened labels
ggplot(top_10_drugs, aes(x = reorder(Short_Drug, Avg_Satisfaction), y = Avg_Satisfaction, fill = Short_Drug)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Drug Combinations by Average Satisfaction",
       x = "Drug (Abbreviated)",
       y = "Average Satisfaction") +
  theme_minimal() +
  theme(legend.position = "none")

# Install and load the writexl package if you haven't already
# install.packages("writexl")
install.packages("writexl")
library(writexl)

# Create the abbreviation table
abbreviation_table <- top_10_drugs %>%
  select(Drug, Short_Drug, Avg_Satisfaction)

# Save the table to an Excel file
write_xlsx(abbreviation_table, path = "Drug_Abbreviation_Table.xlsx")

# Confirm that the file is saved successfully
print("The Drug Abbreviation Table has been saved as Drug_Abbreviation_Table.xlsx")

# Aggregate by condition or drug type instead of individual drugs
average_satisfaction_by_condition <- drug_data %>%
  group_by(Condition) %>%
  summarise(Avg_Satisfaction = mean(Satisfaction, na.rm = TRUE))

# Bar plot for average satisfaction by condition
ggplot(average_satisfaction_by_condition, aes(x = reorder(Condition, Avg_Satisfaction), y = Avg_Satisfaction, fill = Condition)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Average Satisfaction by Condition",
       x = "Condition",
       y = "Average Satisfaction") +
  theme_minimal() +
  theme(legend.position = "none")
# Install and load the necessary package
# install.packages("wordcloud")
library(wordcloud)


# Install and load treemap package
install.packages("treemap")
library(treemap)

# Treemap for top 10 drug combinations
treemap(top_10_drugs,
        index = c("Drug"),
        vSize = "Avg_Satisfaction",
        vColor = "Avg_Satisfaction",
        type = "value",
        palette = "Blues",
        title = "Treemap of Top 10 Drug Combinations by Average Satisfaction",
        border.col = "white")

# dplyr
library(dplyr)

# Find the top 5 drugs by average satisfaction
top_5_drugs <- drug_data %>%
  group_by(Drug, Condition) %>%
  summarise(Avg_Satisfaction = mean(Satisfaction, na.rm = TRUE)) %>%
  top_n(5, Avg_Satisfaction)

# Aggregate the data to find average satisfaction for each Drug-Condition pair
average_satisfaction <- drug_data %>%
  group_by(Drug, Condition) %>%
  summarise(Avg_Satisfaction = mean(Satisfaction, na.rm = TRUE)) %>%
  ungroup()

# Find the top 5 drugs based on overall average satisfaction
top_5_drugs <- average_satisfaction %>%
  group_by(Drug) %>%
  summarise(Avg_Satisfaction = mean(Avg_Satisfaction, na.rm = TRUE)) %>%
  top_n(5, Avg_Satisfaction) %>%
  pull(Drug)

# Find the top 5 conditions based on overall average satisfaction
top_5_conditions <- average_satisfaction %>%
  group_by(Condition) %>%
  summarise(Avg_Satisfaction = mean(Avg_Satisfaction, na.rm = TRUE)) %>%
  top_n(5, Avg_Satisfaction) %>%
  pull(Condition)

# Filter the data to include only the top 5 drugs and top 5 conditions
filtered_data <- average_satisfaction %>%
  filter(Drug %in% top_5_drugs & Condition %in% top_5_conditions)

# Create a shorter label by taking the first letters of each drug in the combination
filtered_data$Short_Drug <- sapply(strsplit(filtered_data$Drug, "-"), function(x) paste(substr(x, 1, 3), collapse = "-"))


library(ggplot2)

# Create the heatmap
ggplot(filtered_data, aes(x = Condition, y = Short_Drug, fill = Avg_Satisfaction)) +
  geom_tile(color = "white") +  # Create the heatmap tiles
  scale_fill_gradient(low = "blue", high = "red") +  # Color gradient for satisfaction
  labs(title = "Heatmap of Top 5 Drugs by Top 5 Conditions",
       x = "Condition",
       y = "Drug",
       fill = "Avg Satisfaction") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
