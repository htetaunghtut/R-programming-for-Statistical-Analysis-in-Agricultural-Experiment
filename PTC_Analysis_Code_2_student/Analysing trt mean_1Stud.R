# Load necessary libraries
library(ggplot2)
library(dplyr)
library(agricolae)  # For post-hoc tests

# Sample data creation
# Assuming you have a data frame 'data' with columns 'Treatment' and 'Response'
set.seed(123)  # For reproducibility
data <- data.frame(
  Treatment = rep(c("A", "B", "C", "D"), each = 10),
  Response = c(rnorm(10, mean = 20, sd = 5),
               rnorm(10, mean = 25, sd = 5),
               rnorm(10, mean = 30, sd = 5),
               rnorm(10, mean = 22, sd = 5))
)

# Summary of the data
summary_data <- data %>%
  group_by(Treatment) %>%
  summarise(Mean = mean(Response), SD = sd(Response))

# ANOVA test
anova_result <- aov(Response ~ Treatment, data = data)
summary(anova_result)

# Post-hoc test (Tukey's HSD)
tukey_result <- HSD.test(anova_result, "Treatment", group = TRUE)
print(tukey_result)

# Plotting the results
ggplot(data, aes(x = Treatment, y = Response)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Response by Treatment", x = "Treatment", y = "Response") +
  theme_minimal()
