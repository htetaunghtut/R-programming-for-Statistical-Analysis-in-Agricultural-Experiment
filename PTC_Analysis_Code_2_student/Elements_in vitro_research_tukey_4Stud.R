# Load necessary libraries
library(agricolae)      # For the LSD test and Duncan's test
library(multcompView)   # For generating letter displays

# Set seed for reproducibility
set.seed(123)

# Create initial petunia data frame with 10 replications
petunia_data <- data.frame(
  Cytokinin = rep(c("Zeatin", "BA", "ZeatinRiboside", "2iP", "TDZ", "Kinetin"), each = 10),
  ShootLength = c(
    rnorm(10, mean = 17.6, sd = 1),  # Zeatin
    rnorm(10, mean = 11, sd = 1),   # BA
    rnorm(10, mean = 8.5, sd = 1),   # ZeatinRiboside
    rnorm(10, mean = 14.3, sd = 1),  # 2iP
    rnorm(10, mean = 1.8, sd = 0.7),  # TDZ
    rnorm(10, mean = 4.1, sd = 1)   # Kinetin
  )
)

# Convert Cytokinin to factor
petunia_data$Cytokinin <- as.factor(petunia_data$Cytokinin)

# Perform ANOVA
anova_results <- aov(ShootLength ~ Cytokinin, data = petunia_data)

# Tukey HSD Test
tukey_results <- TukeyHSD(anova_results)
print("Tukey HSD Results:")
print(tukey_results)

# Extract the p-values for the Tukey HSD comparisons
p_values <- tukey_results$Cytokinin[, "p adj"]

# Create a compact letter display
letters <- multcompLetters(p_values, compare = "<")

# Combine means and letters into a data frame
means <- tapply(petunia_data$ShootLength, petunia_data$Cytokinin, mean)
results <- data.frame(
  Cytokinin = names(means),
  Mean = means,
  Letters = letters$Letters
)

# Print the results with means and corresponding letters
print("Means with Corresponding Letters:")
print(results)
