# Load required packages
library(dplyr)
library(ggplot2)
library(readr)

# Load the data
lake_anna_beach <- read_csv("~/Yolo_Code/prtm_yolo/datasets/Lake_Anna_Beach/Lake_Anna_Beach.csv")
detections <- read_csv("~/Yolo_Code/prtm_yolo/Code/detections_pivot_sum_by_class_beach.csv")

# Merge the datasets
merged_data <- lake_anna_beach %>%
  inner_join(detections, by = c("File" = "filename"))

# View the merged data
head(merged_data)

# Add a column that aggregates all Counter columns
merged_data <- merged_data %>%
  mutate(Counter_sum = Counter0 + Counter1 + Counter2 + Counter3 + Counter4)

# Create a new variable for the sum of humans and people
merged_data$Sum_Humans_People <- merged_data$Human + merged_data$People

# View the data with the new column
head(merged_data)

# Convert DateTime to a Date-Time object
merged_data$DateTime <- as.POSIXct(merged_data$DateTime, format="%Y-%m-%d %H:%M:%S")

# Plot the time series with multiple lines
ggplot(merged_data, aes(x = DateTime)) +
  geom_line(aes(y = Counter_sum, color = "Counters_People")) +
  geom_line(aes(y = Boat, color = "Boat")) +
  geom_line(aes(y = Human, color = "Human")) +
  geom_line(aes(y = People, color = "People")) +
  geom_line(aes(y = Sum_Humans_People, color = "Humans + People")) +
  labs(title = "Time Series of Various Counts",
       x = "DateTime",
       y = "Counts") +
  scale_color_manual(values = c("Counters_People" = "blue", "Boat" = "red", "Human" = "green", "People" = "purple", "Humans + People" = "black")) +
  theme_minimal()

# Fit a linear model with Counter_sum as the independent variable
lm_sum <- lm(Sum_Humans_People ~ Counter_sum, data = merged_data)

# Display the summary of the model
summary(lm_sum)

# Scatter plot with regression line for humans
ggplot(merged_data, aes(x = Counter_sum + People, y = Sum_Humans_People)) +
  geom_point(color = "blue") +  # Plot the data points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add the regression line
  labs(title = "Linear Regression of Counter_sum on Humans",
       y = "Humans + People",
       x = "Counter_sum") +
  theme_minimal()