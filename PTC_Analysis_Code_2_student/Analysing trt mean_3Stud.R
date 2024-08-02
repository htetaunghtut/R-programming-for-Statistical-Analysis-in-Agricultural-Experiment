# Load necessary libraries
library(ggplot2)
library(dplyr)
library(agricolae)  # For Duncan's multiple range test

# Sample data creation
data <- data.frame(
  Treatment = rep(c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8"), each = 10),
  Height = c(rnorm(10, mean = 41, sd = 4.6),
                   rnorm(10, mean = 95, sd = 4.6),
                   rnorm(10, mean = 85, sd = 4.6),
                   rnorm(10, mean = 58, sd = 4.6),
                   rnorm(10, mean = 51, sd = 4.6),
                   rnorm(10, mean = 52, sd = 4.6),
                   rnorm(10, mean = 55, sd = 4.6),
                   rnorm(10, mean = 50, sd = 4.6)))

# Summary of the data
summary_data <- data %>%
  group_by(Treatment) %>%
  summarise(Mean = mean(Height), SD = sd(Height))
# ANOVA test
anova_result <- aov(Height ~ Treatment, data = data)
summary(anova_result)

# Duncan's multiple range test
duncan_result <- duncan.test(anova_result, "Treatment", group = TRUE)
print(duncan_result)

# Plotting the results
ggplot(data, aes(x = Treatment, y = Height)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Height Growth of In Vitro Cultured Populus Plantlets",
       x = "Treatment",
       y = "Height (cm)") +
  theme_minimal()
# Plotting the results
ggplot(data, aes(x = Treatment, y = Height)) +
  geom_bar(stat = "summary", fun = "mean") +
  #geom_errorbar(aes(ymin = Height - SD, ymax = Height + SD), width = 0.2) +
  labs(title = "Early Height Growth of In Vitro Cultured Populus Plantlets",
       x = "Treatment",
       y = "Height (cm)") +
  theme_minimal()
