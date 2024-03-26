## ---------------------------
##
## Script name: RCA function for polygon generation
##
## Author: Pol Carbó Mestre.
##
## Date Created: 2024-03-25
##
## ---------------------------
##
## Notes:
##   This is the most updated version of the function developed in rca_poly_new_approach 
##   Version 3 fixes the issues associated with  Isobath/Shoreline Latitude Split, 
##   Shoreline 'MULTILINESTRING' Structure and Isolines vs. North and South Borders.
##
## ---------------------------

rca_lines_to_polygons_v1 <- function(longitude_lines, latitude_lines) {
  
  # Clip RCA boundary lines based on latitude delimitation lines.
  ## Find min and max latitude values from latitude lines.
  coords = st_coordinates(st_geometry(latitude_lines))
  latitudes = coords[,2] # The latitude values are in the second column of 'coords'
  min_latitude = min(latitudes)
  max_latitude = max(latitudes)
  ## Create a bbox that will intersect the RCA lines.
  bbox <- st_as_sfc(st_bbox(c(xmin = -180, xmax = 180, 
                              ymin = min_latitude, ymax = max_latitude), 
                            crs = st_crs(longitude_lines)))
  clipped_lines <- st_intersection(longitude_lines, bbox)
  print(clipped_lines)
  
  # NOTE: depending on the region the previous clip can generate MULTILINESTRING, instead of simple LINESTRING. This is due, for example, to the existence of islands.
  
  # Below we convert MULTILINESTRING to multiple LINESTRINGs with independent iteration numbers per boundary_name. This is important as later, we will need to associate strings by iterations.
  expanded_clipped_lines <- function(sfc) {
    expanded_features <- list()
    boundary_counters <- list() # To store counters for each boundary_name
    
    for (i in seq_len(nrow(sfc))) {
      feature <- sfc[i, ]
      geometry <- st_geometry(feature)
      boundary_name <- feature$boundary_name
      
      ## Initialize or increment the counter for the current boundary_name
      if (!boundary_name %in% names(boundary_counters)) {
        boundary_counters[[boundary_name]] <- 1 # Initialize if not present
      } else {
        boundary_counters[[boundary_name]] <- boundary_counters[[boundary_name]] + 1 # Increment if already present
      }
      
      if (inherits(geometry, "sfc_MULTILINESTRING")) {
        linestrings <- st_cast(geometry, "LINESTRING")
        
        for (j in seq_len(length(linestrings))) {
          new_feature <- feature
          st_geometry(new_feature) <- linestrings[j]
          new_feature$iteration <- boundary_counters[[boundary_name]] # Set iteration number from boundary-specific counter
          expanded_features[[length(expanded_features) + 1]] <- new_feature
          boundary_counters[[boundary_name]] <- boundary_counters[[boundary_name]] + 1 # Increment for next linestring
        }
      } else {
        feature$iteration <- boundary_counters[[boundary_name]] # Set iteration for original linestrings
        expanded_features[[length(expanded_features) + 1]] <- feature
      }
    }
    
    do.call(rbind, expanded_features)
  }
  
  # Apply the function to the clipped RCA lines
  expanded_clipped_lines <- expanded_clipped_lines(clipped_lines)
  print(expanded_clipped_lines)
  
  
  # NOTE: In some cases the latitude lines can generate two or more independent polygons. As a result we need to treat each o the generated lines associated with those polygons independently. Besides, to facilitate the polygon generation, the nodes coordinates stored in the geometry variable must follow a certain order. The code below, takes one of the two associated lines of the polygon and reverse the coordinates sequence.
  
  # The following code does that for each unique iteration (i.e. for each independent polygon). Here we are assuming that polygons can originate from one or two lines. (TO BE CONFIRMED)
  unique_iterations <- unique(expanded_clipped_lines$iteration)
  ## Loop through each unique iteration number
  for(iteration in unique_iterations) {
    # Filter lines by iteration number
    lines_in_iteration <- expanded_clipped_lines[expanded_clipped_lines$iteration == iteration, ]
    # Check if there are at least two lines to work with
    if(nrow(lines_in_iteration) > 1) {
      # Select the second line
      geom <- st_geometry(lines_in_iteration)[[2]]
      # Perform the reversing procedure on this second line
      coords <- st_coordinates(geom)
      reversed_coords <- coords[nrow(coords):1, ][,1:2]
      reversed_linestring <- st_linestring(as.matrix(reversed_coords))
      reversed_geom <- st_sfc(reversed_linestring, crs = 4326)
      # Update the original sf object
      which_to_replace <- which(expanded_clipped_lines$iteration == iteration)[2]
      st_geometry(expanded_clipped_lines)[which_to_replace] <- reversed_geom
    }
  }
  print(expanded_clipped_lines)
  
  # Now it is the moment to combine strings by iteration value
  sf_data_combined <- expanded_clipped_lines %>%
    group_by(iteration) %>%
    summarize(
      geometry = st_combine(geometry),
      ## VARIABLE VALUES MUST BE RETHINKED
      boundary_type = paste(unique(boundary_type), collapse=", "),
      boundary_name = paste(unique(boundary_name), collapse=", "),
      area_name = paste(unique(area_name), collapse=", "),
      land_ref = paste(unique(land_ref), collapse=", "),
      .groups = 'drop') %>%
    mutate(geometry = st_cast(geometry, "MULTILINESTRING"))
  
  # Finally the function below will generate the polygon or polygons based on each existing row in the sf, converting the MULTILINESTRING into a POLYGON
  line_to_polygon <- function(line) {
    # Extract coordinates
    coords <- st_coordinates(line) 
    coords <- coords[,1:2]
    # Ensure the polygon is closed by repeating the first point at the end if necessary
    if (!identical(coords[1,], coords[nrow(coords),])) {
      coords <- rbind(coords, coords[1,])
    }
    # Create a POLYGON from coordinates
    polygon <- st_polygon(list(coords))
    return(polygon)
  }
  
  # Apply the function to each MULTILINESTRING in the expanded_clipped_lines object
  polygons <- st_sfc(lapply(sf_data_combined$geometry, line_to_polygon), crs = st_crs(sf_data_combined))
  # Create a new sf object for the  polygons
  polygon_sf <- st_sf(data.frame(id = 1:length(polygons)), geometry = polygons)
  print(polygon_sf)
  
  # Visualize in leaflet to explore result
  map <- leaflet() %>%
    addTiles() %>%
    addPolygons(data = polygon_sf, color = "red", weight=0) %>% 
    addPolylines(data = expanded_clipped_lines, color = "red", weight=1) %>% 
    addPolylines(data = selected_latitude_lines, color = "blue", weight=2)
  
  print(map)
  
  return((polygon_sf))
}