## ---------------------------
##
## Script name: RCA lines creation
##
## Author: Code by Cullen. Adapted by Pol.
##
## Date Created: 2024-03-20
##
## ---------------------------
##
## Notes:
##   This codes implements the code available in poligonize.qmd 
##   that generates the RCA boundary lines of interest.
##
## ---------------------------


## directories
# data_path <- here::here('RCA_data_exploration/Ruttenberg et al collaboration')
# coord_path <- here::here(data_path, 'RCA_Coordinate_CSV_Files_cleaned_2002_21')
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
# cal_poly_fp <- here::here(data_path, cal_poly_fn)
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