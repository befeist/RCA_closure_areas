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

## create df of northern/southern boundaries (Latitude_DM) and one isobath for shoreline
north_bound     <- 'revised-sp/north-south-bounds/eez_north.shp'
south_bound     <- 'revised-sp/north-south-bounds/eez_south.shp'

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


## extract polygons
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
  filter(!SouthernBoundary %in% c("45 46.00 N", "48 10.00 N", "46 16.00 N")) %>%
  ## group
  group_by(SiteName, EffectiveDateStart, EffectiveDateEnd, start_year, end_year, NorthernBoundary, SouthernBoundary, ShorewardBoundary,
           SeawardBoundary, ShorewardFilename, SeawardFilename) %>%
  summarize(geometry = st_union(Shape)) %>%
  ungroup() %>%
  mutate(ns_bound = paste(NorthernBoundary, SouthernBoundary, sep = "-"))

## divide up "40 10.00 N-US EEZ Boundary"
div_rcas <- rca_vms %>%
  filter(ns_bound == "40 10.00 N-US EEZ Boundary")

## bounding boxes
bb_3427 <- lat_df %>% filter(boundary_name == "34 27.00' N")
bb_4010 <- lat_df %>% filter(boundary_name == "40 10.00' N")
bb_eez <- lat_df %>% filter(boundary_name == "U.S. EEZ Boundary South")

bbox_1 <- st_union(bb_4010, bb_3427) %>% 
  st_bbox()

bbox_2 <- st_union(bb_3427, bb_eez) %>% 
  st_bbox()

## make function to break these up 

div_rcas_l <- list()

for(i in 1:nrow(div_rcas)) {
  
  print(i)
  
  tmp_rca <- div_rcas[i,]
  
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
  
}

div_rcas_all <- rbindlist(div_rcas_l) %>%
  st_as_sf() %>%
  st_transform(rca_crs) 

## adjusted df
rcas_adj <- rca_vms %>%
  filter(ns_bound != "40 10.00 N-US EEZ Boundary") %>%
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
  
  ggsave(temp_fig, filename = paste0(data_path, "diagnostic/closure-polys/", region, i, ".pdf"),
         width = 10,
         height = 10,
         units = "in")
  
}






ggplot() +
  geom_sf(data = rca_vms %>% filter(SiteName %in% test[3]), aes(fill = SiteName), alpha = 0.3) +
  # labs(title = tmp_rca_name) +
  theme(legend.position = "none")



  ## compare within same northern and southern boundaries
  mutate(ns_bound = paste(NorthernBoundary, SouthernBoundary, sep = "-")) %>%
  ## arrange by ns boundary and start year
  arrange(ns_bound, EffectiveDateStart)







