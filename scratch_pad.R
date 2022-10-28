library(sf)
library(ggplot2)
library(leaflet)
library(dplyr)

spatial_data <- public_df

spatial_data_intersecting <- spatial_data %>%
  st_intersection()

intersecting_points <- spatial_data_intersecting %>%
  filter(st_geometry_type(shape) == "POINT")

intersecting_linestrings <- spatial_data_intersecting %>%
  filter(st_geometry_type(shape) == "LINESTRING")

leaflet() %>%
  addPolylines(data = intersecting_linestrings,
               fillOpacity = 0,
               weight = 0.5) %>%
  addMarkers(data = intersecting_points)

## ------------------------------------------------------------
## Connect boundaries
## -----------------------------------------------------------

## -------------------------------------------------------
## echelle
## -------------------------------------------------------

library(nngeo)

# Connect what isn't connected
connect_shoreward_northern <- st_connect(spatial_data[1,], spatial_data[4,]) %>% 
  st_as_sf() %>% 
  mutate(boundary_dir = "connecting_bound") %>% 
  rename(shape = x)

# Union
spatial_data_connect <- spatial_data %>%  
  bind_rows(connect_shoreward_northern) 

# Check everything is connected
spatial_data_intersecting <- spatial_data_connect %>%
  st_intersection()

intersecting_points <- spatial_data_intersecting %>%
  filter(st_geometry_type(shape) == "POINT")

intersecting_linestrings <- spatial_data_intersecting %>%
  filter(st_geometry_type(shape) == "LINESTRING")

leaflet() %>%
  addPolylines(data = intersecting_linestrings,
               fillOpacity = 0,
               weight = 0.5) %>%
  addMarkers(data = intersecting_points)

test <- st_polygonize(st_union(spatial_data_intersecting))
rca_polygon2 <- st_collection_extract(st_polygonize(st_union(rca_polygoni)))








# pull seaward and shoreward linestrings

x <- public_df

seaw <- x %>% filter(boundary_dir=="seaward")
shore <- x %>% filter(boundary_dir=="shoreward")

# append shoreward to seaward line manually (to "close the polygon")
seaw_line <- seaw %>% st_coordinates() %>% .[,1:2]
shoreward_line <- shore %>% st_coordinates() %>%  .[,1:2]
# reverse the order of one of the lines, so we're continuing to draw around the outside of the polygon
shoreward_line <- shoreward_line[order(nrow(shoreward_line):1),]

# re-construct the polygon now, using the appended linestrings
pol <- seaw_line %>% 
  # join the shoreward line
  rbind(shoreward_line) %>% 
  # make into a complete linestring, then into a polygon
  st_linestring() %>% 
  st_cast("POLYGON") %>% 
  st_sfc(crs=4326)

ggplot(pol)+geom_sf() +geom_sf(data = public_df, aes(color = boundary_dir))






