# Load necessary libraries
library(ggplot2)
library(dplyr)
library(agricolae)  # For post-hoc tests

# Sample data creation
data <- data.frame(
  Treatment = rep(c("Gerlite 0.5", "Gerlite 1.0", "Gerlite 1.5"), each = 10),
  LateralRoots = c(rnorm(10, mean = 3.2, sd = 0.2),
                  rnorm(10, mean = 4.0, sd = 0.4),
                  rnorm(10, mean = 4.3, sd = 0.15)))
# Summary of the data
summary_data <- data %>%
  group_by(Treatment) %>%
  summarise(Mean = mean(LateralRoots), SD = sd(LateralRoots))

# ANOVA test
anova_result <- aov(LateralRoots ~ Treatment, data = data)
summary(anova_result)

# Post-hoc test (Tukey's HSD)
tukey_result <- HSD.test(anova_result, "Treatment", group = TRUE)
print(tukey_result)
# Duncan's multiple range test
duncan_result <- duncan.test(anova_result, "Treatment", group = TRUE)
print(duncan_result)
# Plotting the results
ggplot(data, aes(x = Treatment, y = LateralRoots)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Average Number of Lateral Root Initiations",
       x = "Gerlite (g/L)",
       y = "Number of Lateral Roots") +
  theme_minimal()
