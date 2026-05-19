# ANOVA with post-hoc test for RCA mapping project
# Blake Feist - created 13 May 2026

library(dplyr)
library(ggplot2)
library(nortest)
library(readr)
library(rstatix)
library(tidyr)

# Load the data
HSP_ANOVA_data_wide <- read_csv("~/Documents/Projects/Ecosystem Science/RCAs/HSPs/HSP_ANOVA_data_wide.csv")

# Pivot the data by taking all columns and putting them into a 'Group' (species) column and a 'Value' (HSP anomaly) column
HSP_ANOVA_data <- pivot_longer(HSP_ANOVA_data_wide, 
                          cols = everything(), 
                          names_to = "Species", 
                          values_to = "HSP_anom")

# Check the structure (ensure 'Species' is a factor)
str(HSP_ANOVA_data)

### Fit the standard ANOVA model
res_anova <- aov(HSP_anom ~ Species, data = HSP_ANOVA_data)

# View the results
summary(res_anova)

# The Tukey Honest Significant Difference test adjusts for multiple comparisons so you don't accidentally find "false positives."

# Run Tukey HSD
post_hoc <- TukeyHSD(res_anova)

# View the results
print(post_hoc)

# How to read the output:
#   diff: The difference between the means of the two groups.
# lwr / upr: The lower and upper bounds of the 95% confidence interval.
# p adj: The adjusted p-value. If this is < 0.05, the difference between those two specific groups is significant.

# Visualizing the Results
# Basic boxplot
boxplot(HSP_anom ~ Species, data = HSP_ANOVA_data, 
        main = "Anomaly by species",
        col = c("#fbb4ae", "#b3cde3", "#ccebc5"),
        xlab = "Species", ylab = "Anomaly")

# Plotting the Tukey HSD intervals
plot(post_hoc)

# checking assumptions
# ANOVA assumes your data is normally distributed and has equal variance. You can quickly check this with diagnostic plots
par(mfrow = c(1, 2)) # Split screen
plot(res_anova, which = 1:2)

# Normal Q-Q: Points should roughly follow the diagonal line.
# Residuals vs Fitted: The spread should be roughly equal across the horizontal line (Homoscedasticity).

# Check normality with a p-value. Run the Shapiro-Wilk test on residuals. Throws an error if residuals are >5,000.
shapiro.test(residuals(res_anova))

# Perform the K-S test and compare the residuals to a normal distribution (throws an error, can't use)
ks.test(residuals(res_anova), "pnorm", mean(residuals(res_anova)), sd(residuals(res_anova)))

# do the Anderson-Darling Test since the dataset is large and has ties. It is more sensitive to the "tails" of the distribution and handles ties much better.
ad.test(residuals(res_anova))

### Better ANOVA work flow given have large dataset. It's safer to assume  group variances might not be perfectly equal.
# Use oneway.test(), which doesn't require equal variances.
# 'Safer' ANOVA for large/real-world data
res_welch <- oneway.test(HSP_anom ~ Species, data = HSP_ANOVA_data, var.equal = FALSE)
print(res_welch)

# View the results
summary(res_welch)

# Post-Hoc: If significant, run  Games-Howell test (the large-sample version of Tukey)
games_howell_test(HSP_ANOVA_data, HSP_anom ~ Species)

# Visualizing Group Differences ("Joy" Plot)
ggplot(HSP_ANOVA_data, aes(x = HSP_anom, fill = Species)) +
  geom_density(alpha = 0.4) + # alpha makes colors transparent
  theme_minimal() +
  labs(title = "Density Distribution of Scores by Species",
       subtitle = "Visualizing differences found in Welch ANOVA",
       x = "Measurement (HSP_anom)",
       y = "Density")

## Plotting the Games-Howell Post-Hoc Results
# The Games-Howell test (the post-hoc for Welch) doesn't produce a density curve; it produces Confidence Intervals.
# To visualize the actual "gaps" between groups, plot the results of the post-hoc test
# If used the rstatix package for Games-Howell:
gh_results <- games_howell_test(HSP_ANOVA_data, HSP_anom ~ Species)

# A simple bar plot showing the mean differences
ggplot(gh_results, aes(x = paste(group1, "-", group2), y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Games-Howell Post-Hoc Comparisons",
       x = "Species Comparison",
       y = "Mean Difference (with 95% CI)")

## Why use these together?
#  The Density Plot shows the "big picture"—it tells the story of how your 6,000+ points are spread out.
# The Error Bar Plot (from the post-hoc) tells the "statistical story"—it confirms which differences are
# actually significant. If the error bar does not cross the red dashed line (0), that pair of groups is
# significantly different.
# Quick Reality Check:
# Does your density plot show the groups mostly sitting on top of each other, or are the "peaks" clearly separated?
# With 6,000 rows, even a tiny separation in those peaks will likely result in a highly significant p-value.

## Create a Matrix of Adjusted P-Values, which will show the p-values for every possible group combination.
# 1. Run the Games-Howell test
gh_results <- games_howell_test(HSP_ANOVA_data, HSP_anom ~ Species) # as before

# 2. Pivot the results into a matrix format
p_matrix <- gh_results %>%
  select(group1, group2, p.adj) %>%
  pivot_wider(names_from = group2, values_from = p.adj)

# View the matrix
print(p_matrix)

# 3. Create a heatmap of the matrix instead
# Get our alphabetical list of groups
group_list <- sort(unique(c(gh_results$group1, gh_results$group2)))
n_groups <- length(group_list)

HSP_heatmap_matrix <- ggplot(gh_results, aes(x = group2, y = group1, fill = p.adj)) +
  geom_tile(color = "white") + # White borders between filled tiles
  scale_fill_gradient(low = "#de2d26", high = "blue", name = "Adj P-Value") +
  
  # Set up the axes
  scale_x_discrete(position = "top", limits = group_list) + 
  scale_y_discrete(limits = rev(group_list)) + 
  
  # Draw crisp grid borders exactly on the outer edges of the cells
  geom_vline(xintercept = seq(0.5, n_groups + 0.5, by = 1), color = "gray90", linewidth = 0.1) +
  geom_hline(yintercept = seq(0.5, n_groups + 0.5, by = 1), color = "gray90", linewidth = 0.1) +
  
  geom_text(aes(label = round(p.adj, 3)), color = "black", size = 1) +
  theme_minimal() +
  labs(title = "Post-Hoc Pairwise Comparison Matrix", 
       x = "", 
       y = "") +
  theme(
    # Turn off the default theme grids so they don't clash with our new borders
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Axis styling
    axis.text.x.top = element_text(size = 10, vjust = 0, angle = 45, hjust = 0),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 16, face = "bold")
  )

# 3. save the matrix heatmap as a high-res file in your working directory
ggsave("HSP heatmap matrix.png", plot = HSP_heatmap_matrix, width = 7, height = 4, dpi = 600)

## save the outputs for the paper!
# 1. Export the density plot (black lines default, yuck)
# Assign  plot to an object name (e.g., my_plot)
HSP_density_plot <- ggplot(HSP_ANOVA_data, aes(x = HSP_anom, fill = Species)) +
  geom_density(alpha = 0.4) + 
  theme_minimal(base_size = 12) + # increases font size for readability in print
  labs(title = "Density Distribution of HSP Anomaly by Species",
       x = "Measurement (HSP Anomaly)",
       y = "Density")

# 1. Export the density plot (just colors, black line around plots removed)
# Assign  plot to an object name (e.g., my_plot)
HSP_density_plot <- ggplot(HSP_ANOVA_data, aes(x = HSP_anom, fill = Species)) +
  # color = NA removes the border lines entirely
  geom_density(alpha = 0.4, color = NA) + 
  theme_minimal(base_size = 12) + 
  # This limits the axis AND tells R exactly where to put the numbers
  scale_x_continuous(limits = c(-0.2, 0.35), 
                     breaks = seq(-0.2, 0.35, by = 0.1)) +
  # This line forces the y-axis limits
  ylim(0, 100) +
  labs(title = "Density Distribution of HSP Anomaly by Species",
       x = "Measurement (HSP Anomaly)",
       y = "Density")

# 2. Save density plot as a high-res file in your working directory
ggsave("HSP density_plot.png", plot = HSP_density_plot, width = 7, height = 5, dpi = 600)

# 3. save the matrix as a csv for Excel import (not super useful)
write.csv(p_matrix, "post_hoc_matrix.csv", row.names = TRUE)