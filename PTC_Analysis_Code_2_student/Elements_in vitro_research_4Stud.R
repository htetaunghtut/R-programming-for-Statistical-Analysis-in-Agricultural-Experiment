# Load necessary libraries
library(ggplot2)
library(dplyr)
library(agricolae)  # For post-hoc tests

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

# Display the initial data
print("Initial Petunia Data:")
print(head(petunia_data, 10))


# Assuming we want to create 10 blocks for each cytokinin
n_blocks <- 10  # Number of blocks
petunia_data$Block <- rep(1:n_blocks, each = nrow(petunia_data) / n_blocks)

# Adjust the Block assignment to ensure it is repeated for each Cytokinin
petunia_data$Block <- rep(rep(1:n_blocks, each = 1), times = 6)

# Display the updated data frame with blocks
print("Petunia Data with Block Column:")
print(head(petunia_data, 20))  # Display the first 20 rows

  
# 1. Completely Randomized Design (CR)
cr_aov <- aov(ShootLength ~ Cytokinin, data = petunia_data)
summary(cr_aov)

# 2. Randomized Complete Block Design (RCB)
rcb_aov <- aov(ShootLength ~ Cytokinin + Block, data = petunia_data)
summary(rcb_aov)

# 3. Incomplete Block Design (LB)
# Assuming that you have a design that allows for incomplete blocking
lb_aov <- aov(ShootLength ~ Cytokinin + Error(Block/Cytokinin), data = petunia_data)
summary(lb_aov)


# Display means for each treatment
means <- aggregate(ShootLength ~ Cytokinin, data = petunia_data, FUN = mean)
print(means)

# Calculate average shoot length for each cytokinin
summary_data <- petunia_data %>%
  group_by(Cytokinin) %>%
  summarize(AverageShootLength = mean(ShootLength), .groups = 'drop')

# Create bar graph
ggplot(summary_data, aes(x = Cytokinin, y = AverageShootLength, fill = Cytokinin)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Shoot Length by Cytokinin",
       x = "Cytokinin",
       y = "Average Shoot Length (mm)") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend for clarity

# Mean compariosn
# Perform ANOVA
anova_results <- aov(ShootLength ~ Cytokinin + Block, data = petunia_data)
summary(anova_results)

# Tukey HSD Test
tukey_results <- TukeyHSD(cr_aov)
print("Tukey HSD Results:")
print(tukey_results)

# Duncan's Multiple Range Test
duncan_results <- duncan.test(anova_results, "Cytokinin", group = TRUE)
print("Duncan's Multiple Range Test Results:")
print(duncan_results)

# Least Significant Difference (LSD)
lsd_results <- LSD.test(anova_results, "Cytokinin", console = TRUE)
print("LSD Test Results:")
print(lsd_results)
