## ---------------------------
##
## Script name: RCA data for polygon generation
##
## Author: Code by Cullen. Adapted by Pol.
##
## Date Created: 2024-03-20
##
## ---------------------------
##
## Notes:
##   This codes implements the code available in poligonize.qmd 
##   to generate the necessary objects needed to create the polygons.
##
## ---------------------------


## directories
data_path <- here::here('RCA_data_exploration/Ruttenberg et al collaboration')
coord_path <- here::here(data_path, 'RCA_Coordinate_CSV_Files_cleaned_2002_21')

## files
rca_data_fn  <- 'Historical_trawl_RCA_2002-2021_CEW_LC_01Oct2021.xlsx'
cal_poly_fn  <- 'RCA_Mapping_Project_4Cal_Poly.gdb'
eez_north_fn <- 'revised-sp/north-south-bounds/eez_north.shp'
eez_south_fn <- 'revised-sp/north-south-bounds/eez_south.shp'

## specify inputs
rca_crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
mollweide <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"



## Regulatory boundary points
rca_legis_points_df <- coord_path |> 
  list.files(full.names = TRUE) |> 
  readr::read_csv(
    locale = locale(encoding = "ISO-8859-1"), 
    id = "boundary_name", 
    show_col_types=FALSE)  |> 
  dplyr::mutate(
    area_name = stringr::str_replace_all(area_name, "-C", "- C"),
    boundary_name = basename(boundary_name),
    boundary_type = "isobath", 
    land_ref = case_when(
      str_detect(area_name, "Coastwide") ~ "mainland",
      str_detect(area_name, "Washington") ~ "mainland",
      # str_detect(area_name, "Cordell") ~ "mainland",
      str_detect(area_name, "DBCA") ~ "mainland",
      str_detect(area_name, "Petrale") ~ "mainland",
      TRUE ~ "islands"
    )) |> 
  dplyr::rename(lat = lat_dd, lon = lon_dd) |> 
  dplyr::filter(area_name != "150-fm (274-m) Contour - Seamounts - Lasuen Knoll") 

## Create line strings for each isobath and area_name
rca_lines_sf <- rca_legis_points_df |> 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)  |>  
  dplyr::group_by(boundary_type, boundary_name, area_name, land_ref) |>
  dplyr::arrange(id_area) |> 
  dplyr::summarize(geometry = st_combine(geometry)) |> 
  sf::st_cast("LINESTRING") 

rca_lines_sf |> 
  tibble::as_tibble() |>
  dplyr::select(boundary_name, area_name, land_ref) |> 
  readr::write_csv(here::here(data_path, "rca_lines_names.csv"))

rca_lines_vec <- rca_lines_sf |> 
  terra::vect()

max_extent <- terra::ext(rca_lines_vec)
xmin <- terra::xmin(max_extent)
xmax <- terra::xmax(max_extent)
ymin <- terra::ymin(max_extent)
ymax <- terra::ymax(max_extent)


## Load north america shapefile for plotting
north_america <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |> 
  dplyr::filter(admin %in% c("United States of America", "Canada", "Mexico"))



##
cal_poly_fp <- here::here(data_path, cal_poly_fn)
cal_poly_layers <- terra::vector_layers(cal_poly_fp)

ca_poly_rca_vect_list <- purrr::map(
  cal_poly_layers,
  ~terra::vect(cal_poly_fp, layer = .),
  .progress=TRUE
)

cal_poly_layers <- stringr::str_remove_all(cal_poly_layers, "rca")
cal_poly_layers <- stringr::str_remove_all(cal_poly_layers, "_line")

ca_poly_rca_vect_df <- tibble::tibble(
  layer_name = cal_poly_layers,
  vect_info = ca_poly_rca_vect_list
) |> 
  dplyr::filter(layer_name != "Landmark_boundary_geocoordinates_table")

shoreline_eez <- ca_poly_rca_vect_df |> 
  dplyr::filter(layer_name == "EEZ_and_shoreline_Geo") |> 
  dplyr::pull(vect_info)

shoreline <- shoreline_eez[[1]] |> 
  sf::st_as_sf() |>
  tidyr::replace_na(replace = list(Name = "Shoreline")) |> 
  dplyr::group_by(AreaType) |>
  dplyr::summarize(geometry = sf::st_union(geometry)) |> 
  dplyr::mutate(
    boundary_type = "isobath",
    boundary_name = "Shoreline",
    area_name = "Shoreline",
    land_ref = ifelse(AreaType == "water", "mainland", "islands")) |> 
  dplyr::select(boundary_type, boundary_name, area_name, land_ref, geometry)


north_eez <- here::here(data_path, eez_north_fn) |> 
  sf::read_sf() |>
  dplyr::mutate(
    boundary_type = 'latitude',
    boundary_name = 'U.S. EEZ Boundary North',
    area_name = 'U.S. EEZ Boundary North') 

south_eez <- here::here(data_path, eez_south_fn) |> 
  sf::read_sf() |>
  dplyr::mutate(
    boundary_type = 'latitude',
    boundary_name = 'U.S. EEZ Boundary South',
    area_name = 'U.S. EEZ Boundary South') 

eez_sf <- rbind(north_eez, south_eez) |> 
  sf::st_transform(rca_crs) |>
  dplyr::filter(OBJECTID == 1)|>
  dplyr::select(boundary_type, boundary_name, area_name, geometry) |> 
  dplyr::mutate(land_ref = "mainland")


## information for building polygons
polygon_df_orig <- here::here(data_path, rca_data_fn) |> 
  openxlsx::read.xlsx(sheet = 4) |> 
  janitor::clean_names() 

## Create clean data frame of polygon metadata
polygon_df <- polygon_df_orig |> 
  dplyr::select(-completed) |>
  dplyr::filter(shoreward_boundary != "N/A") |>
  dplyr::mutate(
    SiteName = paste(## Create unique site name for id
      "TrawlRCA", year_month, land_ref, northern_boundary,
      southern_boundary, shoreward_boundary, seaward_boundary,
      sep = "_")) |>
  tidyr::pivot_longer(
    northern_boundary:seaward_boundary,
    names_to = 'boundary_dir', 
    values_to = 'boundary_name') |>
  dplyr::mutate(
    boundary_dir = stringr::str_remove(boundary_dir, '_boundary'),
    boundary_name = dplyr::case_when(
      boundary_name == "U.S. EEZ boundary" ~ "U.S. EEZ Boundary",
      boundary_name == "45 46.00'N" ~ "45 46.00' N",
      boundary_name == '0 fm (100fm_010119.csv)' ~ '100fm_010119.csv',
      boundary_name == "0 fm (Shoreline)" ~ "Shoreline",
      TRUE ~ stringr::str_remove(gsub("[()]", "", boundary_name), pattern = "0 fm "))) |>
  tidyr::pivot_wider(
    names_from = boundary_dir, 
    values_from = boundary_name) |>
  dplyr::mutate(## Update unique site name for id
    SiteName = paste("TrawlRCA", year_month, land_ref, northern, 
                     southern, shoreward, seaward, sep = "_"),
    ns_bounds = paste(northern, southern, sep = "_")) |>
  tidyr::pivot_longer(
    northern:seaward, 
    names_to = 'boundary_dir', 
    values_to = 'boundary_name') |>
  dplyr::mutate(
    boundary_type = dplyr::case_when( ## add column for joining later
      boundary_dir %in% c('northern', 'southern') ~ 'latitude',
      TRUE ~ 'isobath'),
    boundary_name = dplyr::case_when(
      boundary_name == "U.S. EEZ Boundary" & boundary_dir == "northern" ~ 
        "U.S. EEZ Boundary North",
      boundary_name == "U.S. EEZ Boundary" & boundary_dir == "southern" ~ 
        "U.S. EEZ Boundary South",
      TRUE ~ boundary_name)) |>
  dplyr::select(SiteName, year_month, land_ref, ns_bounds, 
                boundary_type, boundary_dir, boundary_name)

north_south_lats <- polygon_df |> 
  dplyr::filter(
    boundary_type == 'latitude',
    !boundary_name %in% c(
      'U.S. EEZ Boundary North', 
      'U.S. EEZ Boundary South')) |> 
  dplyr::distinct(boundary_name) |> 
  dplyr::arrange(boundary_name) |> 
  dplyr::pull(boundary_name)


convert_to_decimal <- function(lat_str) {
  # Split the string by space and apostrophe to extract degrees, minutes, and direction
  parts <- unlist(strsplit(lat_str, " |'"))
  degree <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  direction <- parts[3]
  
  # Convert to decimal degrees
  decimal_deg <- degree + (minutes / 60)
  
  # Adjust sign for southern latitudes
  if (direction == "S") {
    decimal_deg <- -decimal_deg
  }
  
  return(decimal_deg)
}


# Function to create an sf data frame of lines
create_sf_lines <- function(
    north_south_lats, 
    xmin = -130, 
    xmax = -117
) {
  # Convert latitudes to decimal degrees
  decimal_lats <- sapply(north_south_lats, convert_to_decimal)
  
  make_lat_line <- function(lat) {
    st_linestring(
      matrix(c(xmin, lat, xmax, lat),
             ncol = 2, byrow = TRUE))
  }
  # Create lines within the specified extent
  lines_list <- lapply(decimal_lats, make_lat_line)
  
  # Create an sf data frame
  sf_lines <- st_sf(
    boundary_type = "latitude", 
    boundary_name = north_south_lats, 
    area_name = north_south_lats,
    geometry = st_sfc(lines_list)
  )
  
  # Set CRS to EPSG:4326
  sf_lines <- st_set_crs(sf_lines, 4326)
  
  return(sf_lines)
}

lat_sf <- create_sf_lines(
  north_south_lats, #xmin, xmax,
) |> 
  dplyr::mutate(land_ref = "mainland")

n_s_boundaries_sf <- rbind(eez_sf, lat_sf)