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
library(stringr)

## directories
data_path       <- '/Volumes/GoogleDrive/My Drive/Ruttenberg et al collaboration/'
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

## regulatory information
legis_df_orig   <- read.xlsx(paste0(data_path, rca_data),
                             sheet = 1,
                             startRow = 3,
                             cols = c(1, 3:23))

## information for building polygons
poly_df_orig    <- read.xlsx(paste0(data_path, rca_data),
                             sheet = 4)

## gdb
gdb_layers      <- sf::st_layers(dsn = paste0(data_path, rca_gdb))

## print names
gdb_layers$name

rca_spf <- purrr::map(gdb_layers$name, ~st_read(dsn = paste0(data_path, rca_gdb), layer = .))

rca_spf2 <- tibble(layer_name = gdb_layers$name,
                   sp_info = rca_spf)

## line test
test_shorew <- rca_spf2[18,2][1]
test_shorew <- unnest(test_shorew, sp_info) %>% st_as_sf()

test_seaw <- rca_spf2[3,2][1]
test_seaw <- unnest(test_seaw, sp_info) %>% st_as_sf()



## master lines
master_lines <- rca_spf2[45,2][1]

master_lines <- unnest(master_lines, sp_info) 
  
## line test


## isobath files
iso_list <- list.files(coord_path)

iso_out_list <- list()

for (i in 1:length(iso_list)) {

  print(i)

  iso_tmp_name <- iso_list[i]

  iso_tmp_df <- read_csv(paste0(coord_path, iso_tmp_name)) %>%
    mutate(isobath = iso_tmp_name) %>%
    select(isobath, id_area:lon_dd)

  iso_out_list[[i]] <- iso_tmp_df

}

iso_out_all <- rbindlist(iso_out_list) %>%
  rename(longitude = lon_dd,
         latitude = lat_dd)

iso_area_name <- iso_out_all %>%
  select(isobath, area_name)

# iso_out_sp <- st_as_sf(x = iso_out_all,
#                        coords = c("longitude", "latitude"),
#                        crs = rca_crs)

## clean dfs
## ------------------------------------------

# ## regulatory data --------------------------
# 
# legis_df <- janitor::clean_names(legis_df_orig) %>%
#   fill(year) %>%
#   filter(row_number() <= 104) %>%
#   mutate(year = ifelse(year == '2012a', '2012', year))
# 
# ## 2017 and 2018 have the same information, make separate entries for both years
# legis_2017 <- legis_df %>%
#   filter(year == '2017-18') %>%
#   mutate(year = '2017')
# 
# ## make separate entries for 2017 and 2018
# legis_df2 <- legis_df %>%
#   mutate(year = ifelse(year == '2017-18', '2018', year)) %>%
#   rbind(legis_2017) %>%
#   mutate(year = as.integer(year)) %>%
#   arrange(-year) %>%
#   select(year, area, cfr_citation, year_end_frn_citation_s)
# 
## poly df
poly_df <- janitor::clean_names(poly_df_orig) %>%
  select(-completed) %>%
  mutate(SiteName = paste("TrawlRCA", year_month, land_ref, northern_boundary, southern_boundary, shoreward_boundary, seaward_boundary, sep = "_")) %>%
  pivot_longer(northern_boundary:seaward_boundary, names_to = 'boundary_dir', values_to = 'boundary_name') %>%
  mutate(boundary_dir = str_remove(boundary_dir, '_boundary'),
         ## fix case for joining later
         boundary_name = ifelse(boundary_name == "U.S. EEZ boundary", "U.S. EEZ Boundary", boundary_name),
         ## add spacing for joining later
         boundary_name = ifelse(boundary_name == "45 46.00'N", "45 46.00' N", boundary_name),
         ## adjust shoreline entry for joining later
         boundary_name = ifelse(boundary_name == '0 fm (100fm_010119.csv)', '100fm_010119.csv',
                                ifelse(boundary_name == "0 fm (Shoreline)", "Shoreline", boundary_name))) %>%
  pivot_wider(names_from = boundary_dir, values_from = boundary_name) %>%
  ## update site name id
  mutate(SiteName = paste("TrawlRCA", year_month, land_ref, northern, southern, shoreward, seaward, sep = "_"),
         ns_bounds = paste(northern, southern, sep = "_")) %>%
  pivot_longer(northern:seaward, names_to = 'boundary_dir', values_to = 'boundary_name') %>%
  ## add column for joining later
  mutate(boundary_type = ifelse(boundary_dir %in% c('northern', 'southern'), 'latitude', 'isobath')) %>%
  ## change 0 fm () for matching
  mutate(boundary_name = gsub("[()]", "", boundary_name),
         boundary_name = str_remove(boundary_name, pattern = "0 fm ")) %>%
  select(SiteName, year_month, land_ref, ns_bounds, boundary_type, boundary_dir, boundary_name) %>%
  mutate(boundary_name = ifelse(boundary_name == "U.S. EEZ Boundary" & boundary_dir == "northern", "U.S. EEZ Boundary North",
                                ifelse(boundary_name == "U.S. EEZ Boundary" & boundary_dir == "southern", "U.S. EEZ Boundary South", boundary_name)))


## View poly info
rca_cases <- janitor::clean_names(poly_df_orig) %>%
  mutate(year = as.numeric(substr(year_month, 1, 4)),
         mo = as.numeric(substr(year_month, 6, 7))) %>%
  filter(year >= 2010 & year <= 2017) %>%
  arrange(year, mo)


## polygon generation info ----------------------
## ----------------------------------------------

# create df of northern/southern boundaries (Latitude_DM) and one isobath for shoreline
lat_df <- rca_spf[[37]] %>%
  mutate(Latitude_DM = ifelse(LineType == "EEZ", "U.S. EEZ Boundary",
                              ifelse(LineType == "shore", "Shoreline", Latitude_DM)),
         boundary_type = ifelse(Latitude_DM == "Shoreline", "isobath", 'latitude'),
         area_name = Latitude_DM) %>%
  select(boundary_type,
         boundary_name = Latitude_DM,
         area_name,
         shape = Shape)

# ## create df of northern/southern boundaries (Latitude_DM) and one isobath for shoreline
# unique_lat_df <- master_lines %>%
#   filter(Latitude_D != "") %>%
#   select(Latitude_D) %>%
#   unique() 
# 
# ## vector
# lat_vec <- unique_lat_df$Latitude_D
# 
# lat_df <- st_as_sf(master_lines) %>%
#   filter(Latitude_D %in% lat_vec) %>%
#   select(FID_FtoL_EEZ_RCALines_x_PFMC_Geo, Latitude_D, Shape)
#   mutate(Latitude_DM = ifelse(LineType == "EEZ", "U.S. EEZ Boundary",
#                               ifelse(LineType == "shore", "Shoreline", Latitude_DM)),
#          boundary_type = ifelse(Latitude_DM == "Shoreline", "isobath", 'latitude'),
#          area_name = Latitude_DM) %>%
#   select(boundary_type,
#          boundary_name = Latitude_DM,
#          area_name,
#          shape = Shape)





## check to see if all northern/southern boundaries are in lat_df
setdiff(unique(poly_df %>% filter(boundary_dir %in% c('northern', 'southern')) %>% pull(boundary_name)),
        unique(lat_df %>% pull(boundary_name)))

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
lat_df <- rbind(lat_df, north_eez, south_eez) %>%
  filter(boundary_type != "isobath")


## create shoreline with islands in case needed
shoreline_isl <- rca_spf[[1]] %>%
  st_combine()

shoreline_isl_df <- tibble(boundary_type = "isobath",
                           boundary_name = "Shoreline",
                           shape = shoreline_isl) %>%
  st_as_sf()

## isobaths
## ------------------------------------------------

## unique isobaths in poly df
iso_polys <- poly_df %>%
  filter(boundary_type == "isobath") %>%
  select(boundary_name) %>%
  unique()

missing_isos <- setdiff(iso_polys %>% select(boundary_name) %>% unique(),
                        master_lines %>% select(boundary_name = CoordFileName) %>% unique())

## filter masterline for the boundaries needed
iso_filt <- master_lines %>%
  filter(CoordFileName %in% iso_polys$boundary_name)

## test
test_line <- iso_filt %>% 
  filter(CoordFileName %in% c("100fm_010119.csv", "150fm_010119.csv")) %>% 
  select(CoordFileName, Shape) %>%
  st_as_sf() %>%
  group_by(CoordFileName) %>%
  summarise(geometry = st_union(Shape)) %>%
  ungroup() %>%
  rename(boundary_type = CoordFileName)
  
## test lat
lat_test <- lat_df %>%
  filter(boundary_name %in% c("U.S. EEZ Boundary North", "46 16.00' N")) %>%
  mutate(boundary_type = ifelse(boundary_name == "46 16.00' N", "southern", "northern")) %>%
  select(boundary_type, geometry = shape) %>%
  rbind(test_line)

## crop ----------------------------------------------------------
## get bounding box for north south
north_bb <- lat_test %>% filter(boundary_type == "northern")
south_bb <- lat_test %>% filter(boundary_type == "southern")

bbox_ns <- st_union(north_bb, south_bb) %>% 
  st_bbox()

## crop seaward and shoreward
seaward_crop <- lat_test %>% filter(boundary_type == "150fm_010119.csv")
seaward_crop <- st_crop(seaward_crop, bbox_ns)

## crop seaward and shoreward
shore_crop <- lat_test %>% filter(boundary_type == "100fm_010119.csv")
shore_crop <- st_crop(shore_crop, bbox_ns)

## combine
adj_poly_lines <- rbind(north_bb, south_bb, seaward_crop, shore_crop)

test_polyi <- st_intersection(adj_poly_lines$geometry)
test_poly <- st_polygonize(st_union(test_polyi))
rca_polygon2 <- st_collection_extract(st_polygonize(st_union(test_polyi)))


# ## filter iso_out_sp for isobaths needed
# iso_out_sp_filt <- iso_out_sp %>%
#   filter(isobath %in% iso_polys$boundary_name) 
# 
# ## check to see what is missing
# missing_isos <- setdiff(iso_polys %>% pull(boundary_name), unique(iso_out_sp_filt %>% pull(isobath)))
# 
# ## filter iso_out_sp for isobaths needed
# 
# ## isobath vector
# isobath_vec <- unique(iso_out_sp_filt$isobath)
# 
# ## create lines from iso_out_sp
# iso_out_lines <- iso_out_sp_filt %>%
#   group_by(isobath, area_name) %>%
#   summarise(do_union = FALSE) %>% ## is this setting correct?
#   st_cast("LINESTRING") %>%
#   ungroup()
# 
# areas <- c("Coastwide", "Channel Islands", "Cordell Bank", "Santa Catalina", "Sante Clemente Island",
#            "Sante Clemente Island", "Seamounts")
# 
# iso_out_lines_area <- iso_out_lines %>%
#   mutate(area = str_extract(area_name, pattern = "Coastwide"),
#          area = ifelse(is.na(area), str_extract(area_name, "Channel Islands"), area),
#          area = ifelse(is.na(area), str_extract(area_name, "Cordell Bank"), area),
#          area = ifelse(is.na(area), str_extract(area_name, "Santa Catalina"), area),
#          area = ifelse(is.na(area), str_extract(area_name, "Sante Clemente Island"), area),
#          area = ifelse(is.na(area), str_extract(area_name, "DBCA"), area),
#          area = ifelse(is.na(area), str_extract(area_name, "Seamounts"), area)) %>%
#   select(isobath, area, area_name, geometry)
# 
# 
# for(i in 1:length(isobath_vec)) {
#   
#   tmp_iso_name <- isobath_vec[i]
#   
#   tmp_iso_df <- iso_out_lines %>%
#     filter(isobath == tmp_iso_name)
#   
#   temp_fig <- ggplot(tmp_iso_df) +
#     geom_sf(aes(color = area_name)) +
#     labs(title = tmp_iso_name) +
#     theme(legend.position = "bottom")
#   
#   ggsave(temp_fig, filename = paste0(data_path, "diagnostic/isobaths/", tmp_iso_name, ".pdf"))
#   
#   
# }
# 
# 
# isobath_vec <- unique(iso_out_sp_filt$isobath)
# 
# for(i in 1:length(unique(iso_out_sp_filt$isobath))) {
#   
#   tmp_iso_name <- isobath_vec[i]
#   
#   tmp_iso_df <- iso_out_sp_filt %>%
#     filter(isobath == tmp_iso_name)
#   
#   temp_fig <- ggplot(tmp_iso_df) +
#     geom_sf(aes(color = id_area)) +
#     labs(title = tmp_iso_name) +
#     theme(legend.position = "none")
#   
#   ggsave(temp_fig, filename = paste0(data_path, "diagnostic/isobaths/", tmp_iso_name, "_points.pdf"))
#   
#   
# }
# 
# 
# ## combine lats and isobaths, plot polygons
# ## -----------------------------------------------
# 
# iso_df <- iso_out_lines %>%
#   mutate(boundary_type = "isobath",
#          boundary_name = isobath) %>%
#   select(area_name, boundary_type, boundary_name, shape = geometry)
# 
# boundary_df <- rbind(lat_df, iso_df)
# 
# ## join with poly_df
# full_boundary_df <- poly_df %>%
#   left_join(boundary_df) %>%
#   st_as_sf()
