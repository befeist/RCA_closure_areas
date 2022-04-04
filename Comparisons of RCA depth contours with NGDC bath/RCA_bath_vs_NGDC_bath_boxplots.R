# code for generating boxplots
# Blake Feist - 13 Aug 2020
# lifted code from https://www.tutorialspoint.com/r/r_boxplots.htm for making boxplots
# lifted code from https://bookdown.org/rdpeng/exdata/plotting-and-color-in-r.html for creating/applying color ramps

# handy libraries to load
library(tidyverse)

# set working directory where file is located
setwd("~/Documents/GitHub/RCA_closure_areas/Comparisons of RCA depth contours with NGDC bath")

# load .csv file of NOAA OLE RCA bathymetry isobath vertices overlaid on NGDC bathymetry grid to dataframe named rca
rca <- read.table("RCA_line_densified_vertices_RAW.csv", h=TRUE, sep=",")

# checkout the NGDC_M (NGDC depth at RCA isobath vertex in meters) and DEPT_CAT_M (RCA isobath depth category in meters) attributes
input <- rca[,c('NGDC_M','DEPT_CAT_M')]
print(head(input))

# option a: create boxplot using DEPT_CAT_M for the x-axis categories (boxes/whiskers) and NGDC_M as the values represented in each box/whisker
boxplot(NGDC_M ~ DEPT_CAT_M, data = rca, xlab = "RCA depth contour (m)", ylab = "NGDC depth at vertex (m, vertices densified at 100m intervals)", main = "RCA vs. NGDC Isobaths")

# option b: create boxplot with notch using DEPT_CAT_M for the x-axis categories (boxes/whiskers) and NGDC_M as the values represented in each box/whisker
# also make the boxes go from light blue (shallower) to dark blue (deeper)

# load RColorBrewer library
library(RColorBrewer)

# see various RColorBrewer palettes
display.brewer.all()

# load palette choice and set to function cols 
# green to blue palette
# cols <- brewer.pal(3, "GnBu")

# blue palette
cols <- brewer.pal(3, "Blues")

# interpolate (create color ramp) "GnBu" palette function to pal
pal <- colorRampPalette(cols)

# create boxplot from data and use the pal function to create the 11 colors needed
boxplot(NGDC_M ~ DEPT_CAT_M, data = rca,
  xlab = "RCA depth contour (m)",
  ylab = "NGDC depth at vertex (m, vertices densified at 100m intervals)",
  main = "RCA vs. NGDC Isobaths",
  notch = TRUE,
  varwidth = TRUE,
  col = pal(11)
)