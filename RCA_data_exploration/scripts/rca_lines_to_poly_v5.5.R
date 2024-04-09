## ---------------------------
##
## Script name: RCA function for polygon generation
##
## Author: Pol Carbó Mestre.
##
## Date Created: 2024-04-04
##
## ---------------------------
##
## Notes:
##   This is the most updated version of the function developed in rca_poly_new_approach 
##   Version 5.5 fixes the EEZ border contours
##
## ---------------------------

rca_lines_to_polygons_v5.5 <- function(longitude_lines, latitude_lines,eez_poly) {
  # Clip RCA boundary lines based on latitude delimitation lines.
  
  geometries <- st_geometry(latitude_lines)
  
  # Initialize an empty vector to store max latitudes
  max_latitudes <- numeric(length(geometries))
  
  # Loop through each geometry to find the max latitude
  for (i in seq_along(geometries)) {
    # Extract coordinates for each geometry
    coords <- st_coordinates(geometries[[i]])
    # The Y coordinates (latitude) are in the second column
    latitudes <- coords[, "Y"]
    # Find the maximum latitude for this geometry
    max_latitudes[i] <- max(latitudes)
  }
  
  # You can then add these max latitudes back to your original data frame, if needed
  # latitude_lines$max_latitude <- max_latitudes
  
  boundary_condition_s <- any(latitude_lines$boundary_name == "U.S. EEZ Boundary South")
  # Set min_latitude based on the condition
  min_latitude <- ifelse(boundary_condition_s, 30, min(max_latitudes))
  
  boundary_condition_n <- any(latitude_lines$boundary_name == "U.S. EEZ Boundary Nouth")
  max_latitude <- ifelse(boundary_condition_n, 48.6, max(max_latitudes))
  
  # min_latitude = min(max_latitudes)
  # max_latitude = max(max_latitudes)
  ## Create a bbox that will intersect the RCA lines.
  bbox <- st_as_sfc(st_bbox(c(xmin = -130, xmax = -115, 
                              ymin = min_latitude, ymax = max_latitude), 
                            crs = st_crs(longitude_lines)))
  
  
  ### Patch -----
  # This is a bit redundant but seems to work
  if (any(latitude_lines$boundary_name == "U.S. EEZ Boundary South")){
    difference <- st_intersection(eez_poly,bbox)
    pre_clipped_lines <- st_intersection(longitude_lines, bbox)
    clipped_lines <- st_intersection(pre_clipped_lines, difference)
  } else {
    #   difference <- st_intersection(eez_poly,bbox)
    # pre_clipped_lines <- st_intersection(longitude_lines, bbox)
    clipped_lines <- st_intersection(longitude_lines, bbox)
  }
  
  ### Patch -----
  
  
  
  
  MultilineFromIntersections <- function(lat_line, clipped_lines) {
  # Perform intersection
  coastline_intersect <- st_intersection(latitude_lines, longitude_lines)
  
  # Extract points from intersection geometries
  # print("Extracting points from intersection geometries")
  extract_points <- function(coastline_intersect) {
    # Extract points from intersection geometries
    all_points_list <- lapply(1:length(coastline_intersect$geometry), function(i) {
      geom <- coastline_intersect$geometry[i]
      # Extract coordinates depending on geometry type
      coords <- st_coordinates(geom)
      # Select only the X and Y columns
      coords_df <- data.frame(X = coords[, "X"], Y = coords[, "Y"])
      # Create an sf object of points for each geometry
      st_as_sf(x = coords_df, coords = c("X", "Y"), crs = st_crs(coastline_intersect))
    })
    
    points_sf <- do.call(rbind, all_points_list)
    return(points_sf)
  }
  
  # Combine all sf objects into one
  points_sf <- extract_points(coastline_intersect)
  
  
  ## Extract latitudes
  latitudes <- st_coordinates(points_sf)[,2]
  
  if (length(latitudes) %% 2 != 0 | !any(duplicated(latitudes)) | length(unique(latitudes)) == 1) {
    
    # latitudes_rounded <- ceiling(latitudes * 10) / 10 # Define tolerance
    modified_latitudes <- ifelse(latitudes > 48.16668, 48.3, 
                                 ifelse(latitudes < 34.44999, 32.4, latitudes))
    #34.45000
    lat_counts <- table(modified_latitudes)
    odd_even <- ifelse(lat_counts %% 2 == 1, "odd", "even")
    unique_lats <- unique(modified_latitudes)
    ## Identify max and min latitude
    max_lat <- max(unique_lats)
    min_lat <- min(unique_lats)
    ## Check if max and/or min latitude have odd occurrences
    max_lat_odd <- odd_even[as.character(max_lat)] == "odd"
    min_lat_odd <- odd_even[as.character(min_lat)] == "odd"
    ## Get latitude border presenting the problem
    find_matching_latitude_indices <- function(sf_object, latitude) {
      
      row_indices <- points_sf %>%
        st_coordinates() %>%
        {which(.[, "Y"] == latitude, arr.ind = TRUE)}
      
      point_to_fix <- points_sf[row_indices,]
      
      matching_indices <- st_intersects(st_buffer(point_to_fix, 1e-6),sf_object) %>% 
        as.numeric()
      
      return(matching_indices)
    }
    
    # Below we solve the intersection issue on the border presenting the problem by searching the
    # nearest points between the lines
    # print("Lack of intersections detected. Solving problem...")
    process_latitude_lines <- function(latitude_lines, latitude) {
      ## Selecting latitude line affected
      matching_indices <- find_matching_latitude_indices(latitude_lines, latitude)
      selected_row <- latitude_lines[matching_indices,]
      ## Searching closest lines for all objects
      nearest_points <- st_nearest_points(clipped_lines,selected_row) %>%
        st_as_sf()
      
      lengths <- as.vector(st_length(nearest_points))
      # Filter out zero-length lines
      non_zero_lengths <- lengths[lengths > 0]
      # Find the shortest non-zero length
      shortest_non_zero_length <- min(non_zero_lengths)
      # Find the index(es) of the LINESTRING(s) with the shortest non-zero length
      shortest_index <- which(lengths == shortest_non_zero_length)
      # Select the shortest non-zero length LINESTRING from the collection
      shortest_line <- nearest_points[shortest_index, ]
      ## Add shortest line to our latitude line so intersection occurs
      latitude_intersect <- selected_row %>%
        st_union(shortest_line)
      ## Update latitude lines
      latitude_lines <- rbind(latitude_lines[-matching_indices,],
                              latitude_intersect %>% st_cast("LINESTRING"))
      return(latitude_lines)
    }
    
    
    
    # Now, we will evaluate the affected latitude lines and apply the fuctions defined above
    if (max_lat_odd & min_lat_odd) {
      # Both the maximum and minimum latitudes have an odd number of points
      updated_max_latitude_lines <- process_latitude_lines(latitude_lines, max(unique(latitudes)))
      
      is_max_lat_in_line <- function(line, max_lat) {
        coords <- st_coordinates(line)
        lat_min <- min(coords[,2])
        lat_max <- max(coords[,2])
        return(max_lat >= lat_min && max_lat <= lat_max)
      }
      
      # Apply the function to filter rows
      updated_max_latitude_lines_filtered <- updated_max_latitude_lines %>%
        filter(sapply(geometry, is_max_lat_in_line, max_lat = max_lat))
      
      
      updated_min_latitude_lines <- process_latitude_lines(latitude_lines, min(unique(latitudes)))
      
      is_min_lat_in_line <- function(line, min_lat) {
        coords <- st_coordinates(line)
        lat_min <- min(coords[,2])
        return(min_lat >= lat_min)
      }
      
      # Apply the function to filter rows
      updated_min_latitude_lines_filtered <- updated_min_latitude_lines %>%
        filter(sapply(geometry, is_min_lat_in_line, min_lat = min_lat)) 
      
      updated_latitude_lines <- rbind(updated_min_latitude_lines_filtered,
                                      updated_max_latitude_lines_filtered)
      
      ## Regenerate points_sf
      coastline_intersect <- st_intersection(updated_latitude_lines, clipped_lines)
      points_sf <- extract_points(coastline_intersect)
      
      
    } else if (max_lat_odd == TRUE & min_lat_odd == FALSE) { # The maximum latitude has an odd number of points
      updated_latitude_lines <- process_latitude_lines(latitude_lines, max(unique(latitudes)))
      ## Regenerate points_sf
      coastline_intersect <- st_intersection(updated_latitude_lines, clipped_lines)
      points_sf <- extract_points(coastline_intersect)
      
    } else if (min_lat_odd == TRUE & max_lat_odd == FALSE) { # The minimum latitude has an odd number of points.
      updated_latitude_lines <- process_latitude_lines(latitude_lines, min(unique(latitudes)))
      
      coastline_intersect <- st_intersection(updated_latitude_lines, clipped_lines)
      points_sf <- extract_points(coastline_intersect)
      
      
      
    } else if (max_lat == min_lat){
      
      lats <- sapply(st_geometry(latitude_lines), function(geom) {
        median(st_coordinates(geom)[, "Y"])
      })
      
      matching_indices <- which(lats == max(unique(latitudes)))
      
      selected_row <- latitude_lines[-matching_indices,]
      ## Searching closest lines for all objects
      nearest_points <- st_nearest_points(clipped_lines,selected_row) %>%
        st_as_sf()
      
      lengths <- as.vector(st_length(nearest_points))
      
      # Select the  non-zero length LINESTRINGs from the collection
      new_lines <- nearest_points[which(lengths > 0),] 
      
      ## Add shortest line to our latitude line so intersection occurs
      latitude_intersect <- st_union(selected_row, new_lines)
      
      ## Update latitude lines
      updated_latitude_lines <- rbind(latitude_lines[matching_indices,],
                                      latitude_intersect %>% st_cast("LINESTRING"))
      coastline_intersect <- st_intersection(updated_latitude_lines, clipped_lines)
      points_sf <- extract_points(coastline_intersect)
    }
  } else {
    updated_latitude_lines <- latitude_lines
  }
  
  
  # Once we have the intersection points in the latitude borders, we can transform them into lines
  # print("Transforming points into lines")
  ## Sort points by longitude to guaranty they construct according to the vertices to fill
  coords <- st_coordinates(points_sf)
  sorted_coords <- coords[order(coords[, 'X']), ]
  sorted_coords <- sorted_coords[order(sorted_coords[, 'Y']), ] # Get X sorting by each latitude value
  ## Recreate sorted points as an sf object
  sorted_points_sf <- st_sf(geometry = st_sfc(lapply(1:nrow(sorted_coords), function(i) {
    st_point(sorted_coords[i, ])
  }), crs = st_crs(points_sf)))
  ## Get updated coordinates after sorting
  coords <- st_coordinates(sorted_points_sf)
  ## Create LINESTRINGs from pairs of sorted points
  lines_list <- vector("list", length = nrow(sorted_points_sf) / 2) # Initialize
  for (i in seq(1, nrow(sorted_points_sf), by = 2)) {
    if (i < nrow(sorted_points_sf)) {
      lines_list[[i]] <- st_linestring(coords[i:(i+1), ])
    }
  }
  lines_list <- lines_list[!sapply(lines_list, is.null)]  # Filter out any NULL elements
  ## Combine into a single MULTILINESTRING
  multiline <- st_sfc(lines_list, crs = st_crs(points_sf))
  multiline_sf <- st_sf(geometry = multiline)
  
  # 
  if(any(latitude_lines$boundary_name == "U.S. EEZ Boundary North")) {
    
    
    
    # Find the index of the geometry with the maximum latitude
    min_lats <- sapply(st_geometry(multiline_sf), function(geom) {
      min(st_coordinates(geom)[, "Y"])
    })
    
    overall_min_lat <- min(min_lats)
    min_lat_idx <- which(min_lats == overall_min_lat)
    
    
    # Creating a bounding box and modifying it
    bbox_mul <- st_bbox(multiline_sf[-min_lat_idx, ])
    bbox_mul["ymin"] <- min_latitude  # min_latitude is defined above
    bbox_mul["ymax"] <- max_latitude  # max_latitude is defined above
    bbox_mul_sf <- st_as_sfc(bbox_mul)
    
    # Intersecting with the north EEZ line
    
    north_eez_line <- filter(updated_latitude_lines, boundary_name == "U.S. EEZ Boundary North")
    
    intersections <- st_intersects(bbox_mul_sf,north_eez_line)
    intersecting_rows <- north_eez_line[unlist(intersections), ] %>%
      st_union() %>%
      st_sf()
    
    # Replace the geometry in multiline_sf
    multiline_sf[-min_lat_idx, ] <- intersecting_rows
    
    
  } else if (any(latitude_lines$boundary_name == "U.S. EEZ Boundary South")) {
    
    # Find the index of the geometry with the maximum latitude
    max_lats <- sapply(st_geometry(multiline_sf), function(geom) {
      max(st_coordinates(geom)[, "Y"])
    })
    
    overall_max_lat <- max(max_lats)
    max_lat_idx <- which(max_lats == overall_max_lat)
    
    
    # Creating a bounding box and modifying it
    bbox_mul <- st_bbox(multiline_sf[-max_lat_idx, ])
    bbox_mul["ymin"] <- min_latitude  # min_latitude is defined above
    bbox_mul["ymax"] <- max_latitude  # max_latitude is defined above
    bbox_mul_sf <- st_as_sfc(bbox_mul)
    
    # Intersecting with the north EEZ line
    
    south_eez_line <- filter(updated_latitude_lines, boundary_name == "U.S. EEZ Boundary South")
    
    # intersections <- st_intersects(bbox_mul_sf,south_eez_line)
    # intersecting_rows <- south_eez_line[unlist(intersections), ] %>%
    #   st_union() %>%
    #   st_sf()
    # 
    # # Replace the geometry in multiline_sf
    # multiline_sf[-max_lat_idx, ] <- intersecting_rows
    # 
    
    ezz_intersections <- st_intersects(south_eez_line)
    
    # Check if any geometry only intersects with itself
    only_self_intersects <- sapply(1:length(ezz_intersections), function(i) {
      length(ezz_intersections[[i]]) == 1 && ezz_intersections[[i]] == i
    })
    
    # Condition to return FALSE if any geometry only intersects with itself
    if(any(only_self_intersects)) {
      
    } else {
      
      intersections <- st_intersects(bbox_mul_sf, south_eez_line)
      intersecting_rows <- south_eez_line[unlist(intersections), ] %>%
        st_union() %>%
        st_sf()
      
      # Replace the geometry in multiline_sf
      multiline_sf[-max_lat_idx, ] <- st_combine(intersecting_rows) %>% st_sf()
    }
    
    
  }
  
    return(multiline_sf)
  }


  multiline_sf <- MultilineFromIntersections(latitude_lines, clipped_lines)
  
  
  
  # Create polygon from lines
  # print("Creating polygons from lines")
  ## Combine into a MULTILINESTRING
  multilinestring <- st_union(multiline_sf$geometry)
  target_geometry <- clipped_lines %>% 
    .$geometry 
  combined_geometry <- st_union(target_geometry, multilinestring)
  ## Polygonize the combined MULTILINESTRINGs
  polygon_sf <- st_combine(st_union(combined_geometry)) %>% 
    st_polygonize() %>% 
    st_collection_extract("POLYGON") %>% 
    st_as_sf() %>% 
    rename(geometry = x) %>% 
    mutate(id = row_number(),.before=geometry)
  
  # Clip out any inner polygon
  # To do it we will evaluate relationships between polygons based on whether they intersect with coastline features
  ## Get information on intersection polygons
  
  
  if (any(unlist(st_intersects(polygon_sf, coastline)))){
    
    coastline_poly <- st_combine(st_union(coastline)) %>% 
      st_polygonize() %>% 
      st_collection_extract("POLYGON") %>% 
      st_as_sf() %>% 
      rename(geometry = x) %>% 
      mutate(id = row_number(),.before=geometry)
    
    
    coastline_oi_index <- st_intersects(polygon_sf, coastline_poly)
    coastline_oi <- coastline_poly[unlist(coastline_oi_index),]
    no_land_rca <- st_difference(polygon_sf, st_union(coastline_poly))
    
    
    closed_rca_lines <- st_combine(st_union(clipped_lines)) %>% 
      st_polygonize() %>% 
      st_collection_extract("POLYGON") %>% 
      st_as_sf() %>% 
      rename(geometry = x) %>% 
      mutate(id = row_number(),.before=geometry)
    
    areas <- as.vector(st_area(closed_rca_lines))
    # Exclude polygons with an area greater than 1e9 (easy fix, there must be a better way)
    filtered_closed_rca <- closed_rca_lines[areas <= 3e9, ]
    
    
    if(dim(filtered_closed_rca)[1] == 0) {
      
      rca_coastline_polygons_filtered <- no_land_rca
    } else {
      intersections <- st_intersects(coastline, filtered_closed_rca)
      areas_to_remove <- filtered_closed_rca[unlist(intersections), ] 
      rca_coastline_polygons_filtered <- st_difference(no_land_rca, st_union(areas_to_remove))
    }
    
  
  } else {
    rca_coastline_polygons_filtered <- polygon_sf
  }
  
  
  # map <- leaflet() %>%
  #   addTiles() %>%
  #   addPolygons(data = rca_coastline_polygons_filtered, color = "red", weight=1) %>%
  #   addPolylines(data = selected_latitude_lines, color = "blue", weight=2) %>%
  #   addPolylines(data = clipped_lines, color = "orange", weight=2)
  # 
  # print(map)
  return(rca_coastline_polygons_filtered)
}