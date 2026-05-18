# Paired t-test for RCA mapping project
# Blake Feist - created 15 May 2026

library(dplyr)
library(tidyr)
library(ggplot2)

### STEP 1: Load the data
main_data_wide <- read_csv("~/Documents/Projects/Ecosystem Science/RCAs/HSPs/HSP_Zscore_data_wide.csv")
baselines      <- read.csv("~/Documents/Projects/Ecosystem Science/RCAs/HSPs/regional_baselines.csv")

# Pivot the data from WIDE to LONG format
main_data_long <- main_data_wide %>%
  pivot_longer(
    cols = starts_with("mean_"),     # Tells R to grab all your species density columns
    names_to = "Species_ID",         # The new column that will hold the species names
    values_to = "Mean_Density"       # The new column that will hold the density numbers
  )

# Merge with baselines and calculate the spatial Z-Scores
df_monthly_z <- main_data_long %>%
  left_join(baselines, by = "Species_ID") %>%
  mutate(
    Z_Score = (Mean_Density - Regional_Mean) / Regional_SD
  )

# Quick sanity check: look at the newly reshaped data
head(df_monthly_z)

### STEP 2: The Hotspot Performance Evaluation (Annual)
# To test if closures targeted above-average densities while protecting against month-to-month
# time-dependency, collapse the data by your existing Year column and run your tests.

# Collapse monthly data into annual averages
df_annual <- df_monthly_z %>%
  group_by(year, Species_ID) %>%
  summarise(
    Avg_Z_Score = mean(Z_Score),
    .groups = "drop"
  )

# Run a One-Sample T-Test with a protective safety check (n() >= 2)
species_performance <- df_annual %>%
  group_by(Species_ID) %>%
  summarise(
    Years_Present       = n(),  # Counts how many years this species has data
    Mean_Z_Across_Years = mean(Avg_Z_Score),
    
    # The fix: Only run t.test if there are at least 2 years of data
    p_value = if(n() >= 2) {
      t.test(Avg_Z_Score, mu = 0, alternative = "greater")$p.value
    } else {
      NA  # Returns NA instead of crashing the script
    },
    .groups = "drop"
  )

# View your results table
print(species_performance)

### Step 3: Create the Species Interaction Matrix (Monthly)
# To look at fine-scale species co-occurrence patterns, pivot your full monthly dataset
# wide and run a Spearman rank correlation across your 220 Closure_ID timestamps.

# Pivot the monthly data wide by your YYYY-MM Closure_ID
df_monthly_wide <- df_monthly_z %>%
  select(Closure_ID, Species_ID, Z_Score) %>%
  pivot_wider(names_from = Species_ID, values_from = Z_Score)

# Generate the correlation matrix (dropping the ID column)
species_matrix <- df_monthly_wide %>%
  select(-Closure_ID) %>%
  cor(method = "spearman", use = "complete.obs")

# Convert the matrix to long format for your custom upper-right ggplot
interaction_long <- as.data.frame(species_matrix) %>%
  mutate(Species1 = rownames(.)) %>%
  pivot_longer(-Species1, names_to = "Species2", values_to = "Correlation") %>%
  # Enforce alphabetical filtering to isolate the upper-right triangle
  filter(Species1 < Species2)

### Step 4: Plot the Interaction Heatmap
# This uses the exact layout modifications you requested: it clusters your species in the
# upper-right corner, keeps the axes alphabetical, rotates the top labels by 45 degrees, uses
# thin custom grid lines that frame the blocks perfectly, and keeps the tiles perfectly square.

# Get your clean sorted list of species names
species_list <- sort(unique(c(interaction_long$Species1, interaction_long$Species2)))
n_species <- length(species_list)

ggplot(interaction_long, aes(x = Species2, y = Species1, fill = Correlation)) +
  geom_tile(color = "white") +
  
  # Diverging palette (Blue = Avoidance/Deep vs Shallow, Red = High Co-occurrence)
  scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", 
                       midpoint = 0, limit = c(-1,1), name = "Spatial\nOverlap (rho)") +
  
  # Align axes alphabetically from A to Z
  scale_x_discrete(position = "top", limits = species_list) + 
  scale_y_discrete(limits = rev(species_list)) + 
  
  # Thin grid borders drawn perfectly between the cell edges
  geom_vline(xintercept = seq(0.5, n_species + 0.5, by = 1), color = "gray90", linewidth = 0.25) +
  geom_hline(yintercept = seq(0.5, n_species + 0.5, by = 1), color = "gray90", linewidth = 0.25) +
  
  theme_minimal() +
  labs(title = "Species Spatial Co-Occurrence Within Fishery Closures (2002-2020)", 
       x = "Species B", y = "Species A") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # 45-degree rotated top labels
    axis.text.x.top = element_text(size = 9, angle = 45, hjust = 0, vjust = -0.5),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 12, face = "bold"),
    
    # Keeps cells perfectly square so text/labels don't stretch awkwardly
    coord_fixed() 
  )