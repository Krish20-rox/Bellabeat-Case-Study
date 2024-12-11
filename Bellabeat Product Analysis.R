library(tidyverse)

# Load the activity data
activity <- read_csv("dailyActivity_merged.csv")

# Randomly assign products based on user ID
set.seed(42)  # For reproducibility
activity <- activity %>%
  mutate(Product = ifelse(Id %in% sample(unique(Id), length(unique(Id)) / 2), 
                          "Leaf Urban", "Time"))

# Modify previous code to include Product
Activity <- activity %>% 
  select(-ActivityDate) %>% 
  rename(ActivityDayConsistent = ActivityDateConsistent)

head(Activity)
colnames(Activity)
glimpse(Activity)

# Business Task: Analyze activity data for different products

# Summary statistics by Product
product_summary <- activity %>%
  group_by(Product) %>%
  summarise(
    Avg_Steps = mean(TotalSteps, na.rm = TRUE),
    Median_Steps = median(TotalSteps, na.rm = TRUE),
    Avg_Calories = mean(Calories, na.rm = TRUE),
    Median_Calories = median(Calories, na.rm = TRUE),
    Avg_Sedentary_Hours = mean(SedentaryHours, na.rm = TRUE),
    Median_Sedentary_Hours = median(SedentaryHours, na.rm = TRUE),
    Avg_Active_min = mean(VeryActiveMinutes, na.rm = TRUE),
    Median_Active_min = median(VeryActiveMinutes, na.rm = TRUE)
  )

print(product_summary)


# Visualization of metrics with Product facet
ggplot(activity, aes(x = TotalSteps, y = Calories, color = Product)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = 'black', lwd = 2) +
  labs(title = "Total Steps vs Calories by Product", x = "Total Steps", y = "Calories")

ggplot(activity, aes(x = Product, y = TotalSteps, fill = Product)) +
  geom_boxplot() +
  labs(title = "Total Steps Distribution by Product", x = "Product", y = "Total Steps") +
  theme_minimal()

ggplot(activity, aes(x = Product, y = Calories, fill = Product)) +
  geom_boxplot() +
  labs(title = "Calories Distribution by Product", x = "Product", y = "Calories") +
  theme_minimal()

# Steps trend over the week by Product
ggplot(activity, aes(x = Day, y = TotalSteps, color = Product, group = Product)) +
  stat_summary(fun = "mean", geom = "line", size = 1) +
  labs(title = "Average Daily Steps by Product", x = "Day", y = "Average Steps") +
  theme_minimal()

# Sedentary hours by product
ggplot(activity, aes(x = Product, y = SedentaryHours, fill = Product)) +
  geom_boxplot() +
  labs(title = "Sedentary Hours Distribution by Product", x = "Product", y = "Sedentary Hours") +
  theme_minimal()

# Density plot of Very Active Minutes by Product
ggplot(activity, aes(x = VeryActiveMinutes, color = Product, fill = Product)) +
  geom_density(alpha = 0.4) +
  labs(title = "Density Plot of Very Active Minutes by Product", 
       x = "Very Active Minutes", 
       y = "Density") +
  theme_minimal() +
  scale_color_manual(values = c("Leaf Urban" = "steelblue", "Time" = "orange")) +
  scale_fill_manual(values = c("Leaf Urban" = "steelblue", "Time" = "orange"))