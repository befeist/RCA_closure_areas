## clear workspace
rm(list = ls())
options(dplyr.summarise.inform=F)

## setup
################################################################################

## packages
library(tidyverse)
library(sf)
library(openxlsx)
library(rgdal)
library(data.table)
library(lubridate)

## directories
data_path       <- '/Volumes/GoogleDrive/My Drive/Ruttenberg et al collaboration/'
coord_path      <- paste0(data_path, 'RCA_Coordinate_CSV_Files_cleaned_2002_21/')

## files
rca_data        <- 'Historical_trawl_RCA_2002-2021_CEW_LC_01Oct2021.xlsx'
rca_gdb         <- 'RCA_Mapping_Project_4Cal_Poly.gdb'

## specify inputs
rca_crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

## load files
## ------------------------------------------

## gdb
gdb_layers      <- sf::st_layers(dsn = paste0(data_path, rca_gdb))

## print names
gdb_layers$name

rca_spf <- purrr::map(gdb_layers$name, ~st_read(dsn = paste0(data_path, rca_gdb), layer = .))

rca_spf2 <- tibble(layer_name = gdb_layers$name,
                   sp_info = rca_spf) 

## extract polygons
rca_polys <- rca_spf2$sp_info[[46]]

## compare areas, 2010-2017
rca_vms <- rca_polys %>%
  select(SiteID, SiteName, EffectiveDateStart, EffectiveDateEnd, NorthernBoundary:SeawardBoundary, Shape_Area) %>%
  mutate(start_year = year(EffectiveDateStart),
         end_year = year(EffectiveDateEnd)) %>%
  filter(start_year >= 2010 & start_year <= 2017) %>%
  ## compare within same northern and southern boundaries
  mutate(ns_bound = paste(NorthernBoundary, SouthernBoundary, sep = "-")) %>%
  ## arrange by ns boundary and start year
  arrange(ns_bound, EffectiveDateStart)







