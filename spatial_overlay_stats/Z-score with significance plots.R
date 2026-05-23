# Generate plots of SDM and HSP z-scores with p-values from one-sample T-Test for RCA mapping project
# Blake Feist - created 18 May 2026

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

### Pre-step
# assign the species names lookup and order and handle missing species alignment
species_order_map <- c(
  "mean_pauc" = "Bocaccio",
  "mean_pinn" = "Canary",
  "mean_levi" = "Cowcod",
  "mean_cram" = "Darkblotched",
  "mean_alut" = "Pacific perch",
  "mean_ento" = "Widow",
  "mean_rube" = "Yelloweye",
  "mean_auro" = "Aurora",
  "mean_rufu" = "Bank",
  "mean_mela" = "Black",
  "mean_omus" = "Blackgill",
  "mean_ctus" = "Blackspotted",
  "mean_myst" = "Blue",
  "mean_auri" = "Brown",
  "mean_guta" = "CA scorpionfish",
  "mean_good" = "Chilipepper",
  "mean_nebu" = "China",
  "mean_caur" = "Copper",
  "mean_diac" = "Deacon",
  "mean_carn" = "Gopher",
  "mean_chlo" = "Greenspotted",
  "mean_elon" = "Greenstriped",
  "mean_alti" = "Longspine thorny",
  "mean_zace" = "Sharpchin",
  "mean_jord" = "Shortbelly",
  "mean_bore" = "Shortraker",
  "mean_alas" = "Shortspine thorny",
  "mean_dipl" = "Splitnose",
  "mean_cons" = "Starry",
  "mean_saxi" = "Stripetail",
  "mean_mini" = "Vermilion",
  "mean_flav" = "Yellowtail"
)

### Step 1: Load CSVs
# Load performance CSV files
setwd("~/Documents/GitHub/RCA_closure_areas/spatial_overlay_stats/")
raw_sdm <- read.csv("SDM_species_performance.csv")
raw_hsp <- read.csv("HSP_species_performance.csv")

# Prep SDM performance data
data_sdm <- raw_sdm %>%
  mutate(
    Model_Source = "Species Distribution Model (SDM)",
    Clean_Name   = species_order_map[Species_ID]
  )

# Prep HSP performance data
data_hsp <- raw_hsp %>%
  mutate(
    Model_Source = "Habitat Suitability Probability (HSP)",
    Clean_Name   = species_order_map[Species_ID]
  )

# 4. Stack them vertically into one master data frame
combined_plot_data <- bind_rows(data_sdm, data_hsp) %>%
  mutate(Significance = ifelse(p_value < 0.05, "Significant (p < 0.05)", "Not Significant")) %>%
  filter(!is.na(Clean_Name)) %>%
  
  # --- THE FIX TO SWAP LEFT AND RIGHT PANELS ---
  # Explicitly set the factor levels to force SDM to the left and HSP to the right
  mutate(Model_Source = factor(Model_Source, levels = c(
    "Species Distribution Model (SDM)", 
    "Habitat Suitability Probability (HSP)"
  )))

# 5. Lock the order to match the species_order_map in reverse (Same as before)
explicit_order <- unique(species_order_map)
combined_plot_data$Clean_Name <- factor(combined_plot_data$Clean_Name, levels = rev(explicit_order))


### Step 2: Render Side-by-Side Comparison (leave out "z_score_plot <-" to just generate the plot in R Studio)
z_score_plot <- ggplot(combined_plot_data, aes(x = Clean_Name, y = Mean_Z_Across_Years, fill = Significance)) +
  # Draw the bars
  geom_bar(stat = "identity", width = 0.75, color = "white", linewidth = 0.2) +
  
  # Solid zero baseline across both panels
  geom_hline(yintercept = 0, color = "gray40", linetype = "solid", linewidth = 0.6) +
  
  # Flip coordinates so species common names read horizontally
  coord_flip() +
  
  # Split into side-by-side panels based on the two framework names
  facet_wrap(~ Model_Source, ncol = 2) + 
  
  # Color palette
  scale_fill_manual(values = c("Significant (p < 0.05)" = "#b2182b", 
                               "Not Significant" = "gray75")) +
  theme_minimal() +
  labs(
    title = "Comparison of RCA Closure Targeting Performance",
    subtitle = "Side-by-side evaluation of standardized density metrics (Z-Scores) across frameworks",
    x = NULL, 
    y = "Mean standardized density within RCA closures (Z-Score)",
    fill = ""
  ) +
  theme(
    panel.grid.major.y = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linetype = "dashed"),
    
    # Text and Axis formatting
    axis.text.y = element_text(size = 9, face = "bold", color = "black"), 
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 11, face = "bold", vjust = -1),
    
    # Panel header styling
    strip.text = element_text(size = 11, face = "bold", color = "gray20"),
    strip.background = element_rect(fill = "gray95", color = "transparent"),
    
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 10, color = "gray30", margin = margin(b = 15)),
    legend.position = "bottom"
  )

# Save plot as high-resolution PNG
ggsave(
  filename = "SDM & HSP z-scores plot.png",   # The name of your output file
  plot = z_score_plot,                        # Tells R which plot object to save
  width = 10,                                 # Physical width in inches
  height = 7,                                 # Physical height in inches
  units = "in",                               # Sets the measuring unit to inches ("in", "cm", or "mm")
  dpi = 600                                   # CRITICAL: Sets resolution to 600 DPI
)