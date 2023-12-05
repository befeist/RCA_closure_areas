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
library(cowplot)

## directories
# data_path       <- '/Users/traceymangin/Library/CloudStorage/GoogleDrive-tmangin@ucsb.edu/My\ Drive/Ruttenberg et al collaboration/'
data_path       <- '/Users/tracey/Library/CloudStorage/GoogleDrive-tmangin@ucsb.edu/My\ Drive/Ruttenberg et al collaboration/'
coord_path      <- paste0(data_path, 'RCA_Coordinate_CSV_Files_cleaned_2002_21/')

## files
rca_data        <- 'Historical_trawl_RCA_2002-2021_CEW_LC_01Oct2021.xlsx'
rca_gdb         <- 'RCA_Mapping_Project_4Cal_Poly.gdb'
north_bound     <- 'revised-sp/north-south-bounds/eez_north.shp'
south_bound     <- 'revised-sp/north-south-bounds/eez_south.shp'

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

## create df of northern/southern boundaries (Latitude_DM) and one isobath for shoreline
north_bound     <- 'revised-sp/north-south-bounds/eez_north.shp'
south_bound     <- 'revised-sp/north-south-bounds/eez_south.shp'

## extract latitude boundaries (EEZ_shoreline_and_Landmarks_Geo)
lat_df <- rca_spf[[37]] %>%
  mutate(Latitude_DM = ifelse(LineType == "EEZ", "U.S. EEZ Boundary", 
                              ifelse(LineType == "shore", "Shoreline", Latitude_DM)),
         boundary_type = ifelse(Latitude_DM == "Shoreline", "isobath", 'latitude'),
         area_name = Latitude_DM) %>%
  select(boundary_type,
         boundary_name = Latitude_DM,
         area_name,
         shape = Shape) 

## read in northern and southern us boundaries
north_eez <- read_sf(file.path(data_path, north_bound)) %>%
  st_transform(rca_crs) %>%
  filter(OBJECTID == 1) %>%
  mutate(boundary_type = 'latitude',
         boundary_name = 'U.S. EEZ Boundary North',
         area_name = 'U.S. EEZ Boundary North') %>%
  select(boundary_type, boundary_name, area_name, shape = geometry)

south_eez <- read_sf(file.path(data_path, south_bound)) %>%
  st_transform(rca_crs) %>%
  filter(OBJECTID == 1) %>%
  mutate(boundary_type = 'latitude',
         boundary_name = 'U.S. EEZ Boundary South',
         area_name = 'U.S. EEZ Boundary South') %>%
  select(boundary_type, boundary_name, area_name, shape = geometry)

## bind
lat_df <- rbind(lat_df, north_eez, south_eez)


## extract polygons that NOAA created (RCA_Polygons_2002_21)
## --------------------------------------------
rca_polys <- rca_spf2$sp_info[[46]] 

rca_polys <- st_as_sf(rca_polys) %>%
  mutate(geometry = Shape)

## compare areas, 2010-2019
rca_vms <- rca_polys %>%
  select(SiteName, EffectiveDateStart, EffectiveDateEnd, NorthernBoundary:FRN_Citation) %>%
  mutate(start_year = year(EffectiveDateStart),
         end_year = year(EffectiveDateEnd)) %>%
  filter(start_year >= 2010 & start_year < 2020) %>%
  # filter(!SouthernBoundary %in% c("45 46.00 N", "48 10.00 N", "46 16.00 N")) %>%
  ## group
  group_by(SiteName, EffectiveDateStart, EffectiveDateEnd, start_year, end_year, NorthernBoundary, SouthernBoundary, ShorewardBoundary,
           SeawardBoundary, ShorewardFilename, SeawardFilename) %>%
  summarize(geometry = st_union(Shape)) %>%
  ungroup() %>%
  mutate(ns_bound = paste(NorthernBoundary, SouthernBoundary, sep = "-"))

## divide up "40 10.00 N-US EEZ Boundary", "US EEZ Boundary-45 46.00 N", "US EEZ Boundary-46 16.00 N"
div_rcas <- rca_vms %>%
  filter(ns_bound %in% c("40 10.00 N-US EEZ Boundary", "US EEZ Boundary-45 46.00 N", "US EEZ Boundary-46 16.00 N"))

## bounding boxes
## ---------------------------------------------------------

## 40 10.00 N-US EEZ Boundary
bb_3427 <- lat_df %>% filter(boundary_name == "34 27.00' N")
bb_4010 <- lat_df %>% filter(boundary_name == "40 10.00' N")
bb_eez_s <- lat_df %>% filter(boundary_name == "U.S. EEZ Boundary South")

bbox_1 <- st_union(bb_4010, bb_3427) %>% 
  st_bbox()

bbox_2 <- st_union(bb_3427, bb_eez_s) %>% 
  st_bbox()

## "US EEZ Boundary-45 46.00 N"; "US EEZ Boundary-46 16.00 N"
bb_4546 <- lat_df %>% filter(boundary_name == "45 46.00' N")
bb_4810 <- lat_df %>% filter(boundary_name == "48 10.00' N")
bb_eez_n <- lat_df %>% filter(boundary_name == "U.S. EEZ Boundary North")

## make 4810 the min
bb_eez_n <- 

bbox_4548 <- st_union(bb_4810, bb_4546) %>%
  st_bbox()

# bbox_48eez <- st_union(bb_eez_n, bb_4810) %>%
#   st_bbox()

bbox_48eez <- st_bbox(c(xmin = -129.1293, ymin = 48.16667, xmax = -124.7330, ymax = 48.5059))



## make function to break these up 

div_rcas_l <- list()

for(i in 1:nrow(div_rcas)) {
  
  print(i)
  
  tmp_rca <- div_rcas[i,]
  
  print(tmp_rca$ns_bound[1])
  
  if(tmp_rca$ns_bound[1] == "40 10.00 N-US EEZ Boundary") {
  
  tmp_rca1 <- tmp_rca %>%
    mutate(SouthernBoundary = "34 27.00 N",
           ns_bound = paste(NorthernBoundary, SouthernBoundary, sep = "-"))
  
  tmp_rca1 <- st_crop(tmp_rca1, bbox_1) %>%
    st_cast("MULTIPOLYGON")
  
  tmp_rca2 <- tmp_rca %>%
    mutate(NorthernBoundary = "34 27.00 N",
           ns_bound = paste(NorthernBoundary, SouthernBoundary, sep = "-"))
  
  tmp_rca2 <- st_crop(tmp_rca2, bbox_2) %>%
    st_cast("MULTIPOLYGON")
  
  tmp_rca_bind <- rbind(tmp_rca1, tmp_rca2)
  
  print(ncol(tmp_rca_bind))
  
  div_rcas_l[[i]] <- tmp_rca_bind
  
  } else {
    
    tmp_rca1 <- tmp_rca %>%
      mutate(SouthernBoundary = "45 46.00 N",
             NorthernBoundary = "48 10.00 N",
             ns_bound = paste(NorthernBoundary, SouthernBoundary, sep = "-"))
    
    tmp_rca1 <- st_crop(tmp_rca1, bbox_4548) %>%
      st_cast("MULTIPOLYGON")
    
    tmp_rca2 <- tmp_rca %>%
      mutate(SouthernBoundary = "48 10.00 N",
             NorthernBoundary = "US EEZ Boundary",
             ns_bound = paste(NorthernBoundary, SouthernBoundary, sep = "-"))
    
    tmp_rca2 <- st_crop(tmp_rca2, bbox_48eez) %>%
      st_cast("MULTIPOLYGON")
    
    tmp_rca_bind <- rbind(tmp_rca1, tmp_rca2)
    
    print(ncol(tmp_rca_bind))
    
    div_rcas_l[[i]] <- tmp_rca_bind
    
    
  }
  
}

div_rcas_all <- rbindlist(div_rcas_l) %>%
  st_as_sf() %>%
  st_transform(rca_crs) 

## adjusted df
rcas_adj <- rca_vms %>%
  filter(!ns_bound %in% c("40 10.00 N-US EEZ Boundary", "US EEZ Boundary-45 46.00 N", "US EEZ Boundary-46 16.00 N")) %>%
  rbind(div_rcas_all) %>%
  mutate(month = month(EffectiveDateStart),
         yearmo = paste0(start_year, month)) %>%
  arrange(ns_bound, EffectiveDateStart)


plot_areas <- unique(rcas_adj$ns_bound)

for(i in 1:nrow(rcas_adj)) {
  
  tmp_rca <- rcas_adj[i,]
  
  region <- tmp_rca$ns_bound %>% as.character()
  
  temp_fig <- ggplot(tmp_rca) +
    geom_sf(aes(fill = yearmo), lwd = 0, alpha = 0.3) +
    labs(title = region) +
    theme(legend.position = "bottom")
  
  ggsave(temp_fig, filename = paste0(data_path, "diagnostic/closure-polys/west-coast/", region, i, ".pdf"),
         width = 10,
         height = 10,
         units = "in")
  
}

## compare january closures for all ns-boundaries
## --------------------------------------------------

ns_areas <- unique(rcas_adj$ns_bound)

shoreline <- rca_spf[[1]] %>%
  st_union()


for(i in 12:12) {
  
  for(j in 2010:2018) {
    
    for(k in 1:length(ns_areas)){
    
      ## filter
      rca_tmp <- rcas_adj %>%
        filter(start_year == j | start_year == j + 1,
               month == i,
               ns_bound == ns_areas[k])
    
      ## create two multipolygons for each year
      rca_tmp2 <- rca_tmp %>%
        group_by(start_year, ns_bound, month) %>%
        summarize(geometry = st_union(geometry)) %>%
        ungroup() %>%
        mutate(year = as.character(start_year))
      
      year1 <- rca_tmp2 %>%
        filter(start_year == j)
      
      year2 <- rca_tmp2 %>%
        filter(start_year == j + 1)
      
      open_tmp <- st_difference(year1$geometry, year2$geometry)
      
      if(length(open_tmp) > 0) {
      
      open_tmp_sf <- st_sf(start_year = 2017,
                              ns_bound = ns_areas[k], 
                              month = i,
                              geometry = open_tmp,
                              year = "open")
      
      ## bind
      rca_tmp2 <- rbind(rca_tmp2, open_tmp_sf)
      
      }
      
      close_tmp <- st_difference(year2$geometry, year1$geometry)
      
      if(length(close_tmp) > 0) {
      
      close_tmp_sf <- st_sf(start_year = 2017,
                           ns_bound = ns_areas[k], 
                           month = i,
                           geometry = close_tmp,
                           year = "close")
      
      
      ## bind
      rca_tmp2 <- rbind(rca_tmp2, close_tmp_sf) 
      
      }
      
      
      ## bounding box for shoreline
      shoreline_tmp <- st_crop(shoreline, st_bbox(rca_tmp2))
      
      temp_fig1 <- ggplot(rca_tmp2 %>% filter(!year %in% c("open", "close"))) +
        geom_sf(aes(fill = year), lwd = 0, alpha = 0.4) +
        geom_sf(data = shoreline_tmp) +
        theme_bw() +
        ggtitle(paste0(ns_areas[k], " - month ", i)) +
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              plot.title = element_text(hjust = 0))
      
      temp_fig2 <- ggplot(rca_tmp2 %>% filter(year %in% c("open", "close"))) +
        geom_sf(aes(fill = year), lwd = 0, alpha = 0.4) +
        scale_fill_manual(values = c("close" = "red", "open" = "green")) +
        geom_sf(data = shoreline_tmp) +
        theme_bw() +
        theme(legend.position = "bottom",
              legend.title = element_blank())
      
      ## plot together
      maps_temp <- plot_grid(
        temp_fig1,
        temp_fig2,
        align = "v",
        ncol = 1)
      
      ggsave(maps_temp,
             filename = paste0(data_path, "diagnostic/comparisons/", i, "/", ns_areas[k], "-", i, "-", j, ".pdf"),
             dpi = 300,
             device = 'pdf')
    
    }
    
  }
  
  
}










# 
# ggplot() +
#   geom_sf(data = rca_vms %>% filter(SiteName %in% test[3]), aes(fill = SiteName), alpha = 0.3) +
#   # labs(title = tmp_rca_name) +
#   theme(legend.position = "none")
# 
# 
# 
#   ## compare within same northern and southern boundaries
#   mutate(ns_bound = paste(NorthernBoundary, SouthernBoundary, sep = "-")) %>%
#   ## arrange by ns boundary and start year
#   arrange(ns_bound, EffectiveDateStart)







