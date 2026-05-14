# ANOVA with post-hoc test for RCA mapping project
# Blake Feist - created 13 May 2026

# Load the data
HSP_ANOVA_data <- read_csv("~/Documents/Projects/Ecosystem Science/RCAs/HSPs/HSP ANOVA data.csv")

# Check the structure (ensure 'Species' is a factor)
str(HSP_ANOVA_data)

# Fit the ANOVA model
res_anova <- aov(Anom ~ Species, data = HSP_ANOVA_data)

# View the results
summary(res_anova)

## The Tukey Honest Significant Difference test adjusts for multiple comparisons so you don't accidentally find "false positives."

# Run Tukey HSD
post_hoc <- TukeyHSD(res_anova)

# View the results
print(post_hoc)

# How to read the output:
#   diff: The difference between the means of the two groups.
# 
# lwr / upr: The lower and upper bounds of the 95% confidence interval.
# 
# p adj: The adjusted p-value. If this is < 0.05, the difference between those two specific groups is significant.

## Visualizing the Results
# Basic boxplot
boxplot(Anom ~ Species, data = HSP_ANOVA_data, 
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