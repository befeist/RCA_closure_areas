## ---------------------------
##
## Script name: Function for RCA polygon generation
## Author: Pol Carbó Mestre.
## Contact: pcarbomestre@bren.ucsb.edu
## Date Created: 2024-04-08
##
## ---------------------------
##
## Notes:
##   This is the most updated version of the function developed in rca_generation_testing. 
##   Version 6 constitutes the final version used to generate the RCA polygons. However, some updates
##   However, some updates could be made in terms of optimization. Sections tagged with -- to review --
##   are examples on where improvements could be made.
##
## ---------------------------

rca_lines_to_polygons_v6 <- function(longitude_lines, latitude_lines,eez_poly) {
  
  # Clip RCA boundary lines based on latitude delimitation lines.
  ## Get one latitude value by latitude border.
  geometries <- st_geometry(latitude_lines)
  max_latitudes <- numeric(length(geometries))
    for (i in seq_along(geometries)) {
    coords <- st_coordinates(geometries[[i]])
    latitudes <- coords[, "Y"]
    max_latitudes[i] <- max(latitudes) ## -- to review --
  }
  ## Define min and max latitudes for the bbox
  ### North and South EEZ borders extend across a range of latitude values.
  ### Here, we set a condition to avoid the bounding box cutting the RCA lines at the EEZ borders, 
  ### which will receive special treatment as outlined in another section of the code.  
  boundary_condition_s <- any(latitude_lines$boundary_name == "U.S. EEZ Boundary South")
  min_latitude <- ifelse(boundary_condition_s, 30, min(max_latitudes))
  boundary_condition_n <- any(latitude_lines$boundary_name == "U.S. EEZ Boundary Nouth")
  max_latitude <- ifelse(boundary_condition_n, 48.6, max(max_latitudes))
  ## Create the bbox that will intersect the RCA lines.
  bbox <- st_as_sfc(st_bbox(c(xmin = -130, xmax = -115, 
                              ymin = min_latitude, ymax = max_latitude),
                            crs = st_crs(longitude_lines)))
  ## Clip RCA lines
  ### For reasons not yet fully understood, the southern border of the EEZ requires special treatment.
  if (any(latitude_lines$boundary_name == "U.S. EEZ Boundary South")){
    difference <- st_intersection(eez_poly, bbox)
    pre_clipped_lines <- st_intersection(longitude_lines, bbox)
    clipped_lines <- st_intersection(pre_clipped_lines, difference) ## -- to review --
  } else {
    clipped_lines <- st_intersection(longitude_lines, bbox)
  }
  
  # The approach used is based on the recreation of the latitudinal lines defining the RCAs.
  # Since these lines can be split by the coastline or other RCA line characteristics,
  # we have defined a series of steps and conditions that recreate these lines and combine them with the RCA lines.
  
  MultilineFromIntersections <- function(lat_line, clipped_lines) {
    
    # Find intersecting points
    point_intersect <- st_intersection(latitude_lines, longitude_lines)
    # Extract points from intersection geometries
    ## The extraction could be made straightforward from the point_intersect object. 
    ## However, after encountering issues with some resulting lines, we have implemented the following function 
    ## that seems to work for all cases.    
    extract_points <- function(point_intersect) {
      all_points_list <- lapply(1:length(point_intersect$geometry), function(i) {
        geom <- point_intersect$geometry[i]
        coords <- st_coordinates(geom)
        coords_df <- data.frame(X = coords[, "X"], Y = coords[, "Y"])
        st_as_sf(x = coords_df, coords = c("X", "Y"), crs = st_crs(point_intersect))
      })
      points_sf <- do.call(rbind, all_points_list)
      return(points_sf)
    }
    
    points_sf <- extract_points(point_intersect)
    
    # Update missing points
    ## After some deep exploration of the RCA lines in relationship with the latitude limits, 
    ## we have observed cases where the RCA lines (longitude lines) do not intersect the latitude lines. 
    ## Consequently, some intersection points cannot be found.    
    ## The code below handle those cases and updates the points_sf object.
    
    ### first conditional evaluating missing points
    latitudes <- st_coordinates(points_sf)[,2]
    if (length(latitudes) %% 2 != 0 | !any(duplicated(latitudes)) | length(unique(latitudes)) == 1) {
      
      # Handling latitude diferences in EEZ borthers
      modified_latitudes <- ifelse(latitudes > 48.16668, 48.3, 
                                   ifelse(latitudes < 34.44999, 32.4, latitudes))
      # Defining parity of lat points
      lat_counts <- table(modified_latitudes)
      odd_even <- ifelse(lat_counts %% 2 == 1, "odd", "even")
      unique_lats <- unique(modified_latitudes)
      max_lat <- max(unique_lats)
      min_lat <- min(unique_lats)
      ## Check if max and/or min latitude have odd occurrences
      max_lat_odd <- odd_even[as.character(max_lat)] == "odd"
      min_lat_odd <- odd_even[as.character(min_lat)] == "odd"
      # Defining functions that targets and prepares the data needed to update points_sf
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
      
      ## Solve the intersection issue on the border presenting the problem 
      ## by searching the nearest points between the lines
      process_latitude_lines <- function(latitude_lines, latitude) {
        # Selecting latitude line affected
        matching_indices <- find_matching_latitude_indices(latitude_lines, latitude)
        selected_row <- latitude_lines[matching_indices,]
        # Searching closest lines for all objects
        nearest_points <- st_nearest_points(clipped_lines,selected_row) %>% ## -- to review --
          st_as_sf()
        # Filter out zero-length lines
        lengths <- as.vector(st_length(nearest_points))
        non_zero_lengths <- lengths[lengths > 0]
        # Find the shortest non-zero length
        shortest_non_zero_length <- min(non_zero_lengths)
        # Find the index(es) of the LINESTRING(s) with the shortest non-zero length
        shortest_index <- which(lengths == shortest_non_zero_length)
        # Select the shortest non-zero length LINESTRING from the collection
        shortest_line <- nearest_points[shortest_index, ]
        # Add shortest line to our latitude line so intersection occurs
        latitude_intersect <- selected_row %>%
          st_union(shortest_line)
        # Update latitude lines
        latitude_lines <- rbind(latitude_lines[-matching_indices,],
                                latitude_intersect %>% st_cast("LINESTRING"))
        return(latitude_lines)
      }
      
      # Below we apply the defined functions depending on the affected latitude lines
      ## Several conditions can be met as defined by the conditional statements bellow
      if (max_lat_odd & min_lat_odd) { # Both the maximum and minimum latitudes have an odd number of points
        # Northern line
        updated_max_latitude_lines <- process_latitude_lines(latitude_lines, max(unique(latitudes)))
        is_max_lat_in_line <- function(line, max_lat) {
          coords <- st_coordinates(line)
          lat_min <- min(coords[,2])
          lat_max <- max(coords[,2])
          return(max_lat >= lat_min && max_lat <= lat_max)
        }
        ## Apply the function to filter rows
        updated_max_latitude_lines_filtered <- updated_max_latitude_lines %>%
          filter(sapply(geometry, is_max_lat_in_line, max_lat = max_lat))

        # Southern line
        updated_min_latitude_lines <- process_latitude_lines(latitude_lines, min(unique(latitudes)))
        is_min_lat_in_line <- function(line, min_lat) {
          coords <- st_coordinates(line)
          lat_min <- min(coords[,2])
          return(min_lat >= lat_min)
        }
        updated_min_latitude_lines_filtered <- updated_min_latitude_lines %>%
          filter(sapply(geometry, is_min_lat_in_line, min_lat = min_lat)) 
        
        # Update latitude lines object
        updated_latitude_lines <- rbind(updated_min_latitude_lines_filtered,
                                        updated_max_latitude_lines_filtered)
        # Update points_sf
        point_intersect <- st_intersection(updated_latitude_lines, clipped_lines)
        points_sf <- extract_points(point_intersect)
        
      } else if (max_lat_odd == TRUE & min_lat_odd == FALSE) { # The maximum latitude has an odd number of points
        updated_latitude_lines <- process_latitude_lines(latitude_lines, max(unique(latitudes)))
        point_intersect <- st_intersection(updated_latitude_lines, clipped_lines)
        points_sf <- extract_points(point_intersect)
        
      } else if (min_lat_odd == TRUE & max_lat_odd == FALSE) { # The minimum latitude has an odd number of points.
        updated_latitude_lines <- process_latitude_lines(latitude_lines, min(unique(latitudes)))
        point_intersect <- st_intersection(updated_latitude_lines, clipped_lines)
        points_sf <- extract_points(point_intersect)
        
      } else if (max_lat == min_lat){ # No points in one of the latitude lines
        # Search affected line
        lats <- sapply(st_geometry(latitude_lines), function(geom) {
          median(st_coordinates(geom)[, "Y"])
        })
        matching_indices <- which(lats == max(unique(latitudes)))
        selected_row <- latitude_lines[-matching_indices,]
        # Searching closest lines
        nearest_points <- st_nearest_points(clipped_lines,selected_row) %>%
          st_as_sf()
        lengths <- as.vector(st_length(nearest_points))
        new_lines <- nearest_points[which(lengths > 0),] 
        latitude_intersect <- st_union(selected_row, new_lines)
        # Update latitude lines
        updated_latitude_lines <- rbind(latitude_lines[matching_indices,],
                                        latitude_intersect %>% st_cast("LINESTRING"))
        # Update points_sf
        point_intersect <- st_intersection(updated_latitude_lines, clipped_lines)
        points_sf <- extract_points(point_intersect)
      }
    } else {
      updated_latitude_lines <- latitude_lines
    }
    
    # Points to lines
    ## Once we have the intersection points in the latitude borders, we can transform them into lines.
    ### Sort points by longitude to guaranty they construct according to the vertices to fill
    coords <- st_coordinates(points_sf)
    sorted_coords <- coords[order(coords[, 'X']), ]
    sorted_coords <- sorted_coords[order(sorted_coords[, 'Y']), ]
    ### Recreate sorted points as an sf object
    sorted_points_sf <- st_sf(geometry = st_sfc(lapply(1:nrow(sorted_coords), function(i) {
      st_point(sorted_coords[i, ])
    }), crs = st_crs(points_sf)))
    ### Get updated coordinates after sorting
    coords <- st_coordinates(sorted_points_sf)
    ### Create LINESTRINGs from pairs of sorted points
    lines_list <- vector("list", length = nrow(sorted_points_sf) / 2) # Initialize
    for (i in seq(1, nrow(sorted_points_sf), by = 2)) {
      if (i < nrow(sorted_points_sf)) {
        lines_list[[i]] <- st_linestring(coords[i:(i+1), ])
      }
    }
    lines_list <- lines_list[!sapply(lines_list, is.null)]  # Filter out any NULL elements
    ### Combine into a single MULTILINESTRING
    multiline <- st_sfc(lines_list, crs = st_crs(points_sf))
    multiline_sf <- st_sf(geometry = multiline)
    
    
    # Update EEZ contours
    ## The resulting multiline_sf represents the new latitude lines that will be used to construct the polygon.
    ## This is true for RCAs where the north and south bounds are simple latitude lines. However, for those areas 
    ## where the EEZ sets the latitude limits, the polygons will have to adapt their shapes accordingly.
    ## The code below sets a condition to reevaluate the multiline_sf in those cases.
    
    if(any(latitude_lines$boundary_name == "U.S. EEZ Boundary North")) { # Adapt Northern EEZ border
      # Find the index of the geometry with the maximum latitude
      min_lats <- sapply(st_geometry(multiline_sf), function(geom) {
        min(st_coordinates(geom)[, "Y"])
      })
      overall_min_lat <- min(min_lats)
      min_lat_idx <- which(min_lats == overall_min_lat)
      # Creating a bbox to clip the EEZ
      bbox_mul <- st_bbox(multiline_sf[-min_lat_idx, ])
      bbox_mul["ymin"] <- min_latitude  # min_latitude was defined earlier
      bbox_mul["ymax"] <- max_latitude  # max_latitude was defined earlier
      bbox_mul_sf <- st_as_sfc(bbox_mul)
      # Clipping the north EEZ line
      north_eez_line <- filter(updated_latitude_lines, boundary_name == "U.S. EEZ Boundary North")
      intersections <- st_intersects(bbox_mul_sf,north_eez_line) ## -- to review --
      intersecting_rows <- north_eez_line[unlist(intersections), ] %>%
        st_union() %>%
        st_sf()
      # Replace the geometry in multiline_sf
      multiline_sf[-min_lat_idx, ] <- intersecting_rows
      
    } else if (any(latitude_lines$boundary_name == "U.S. EEZ Boundary South")) { # Adapt Southern EEZ border
      # Find the index of the geometry with the maximum latitude
      max_lats <- sapply(st_geometry(multiline_sf), function(geom) {
        max(st_coordinates(geom)[, "Y"])
      })
      overall_max_lat <- max(max_lats)
      max_lat_idx <- which(max_lats == overall_max_lat)
      # Creating a bbox to clip the EEZ
      bbox_mul <- st_bbox(multiline_sf[-max_lat_idx, ])
      bbox_mul["ymin"] <- min_latitude  
      bbox_mul["ymax"] <- max_latitude  
      bbox_mul_sf <- st_as_sfc(bbox_mul)
      # Clipping the north EEZ line
      south_eez_line <- filter(updated_latitude_lines, boundary_name == "U.S. EEZ Boundary South")
      ezz_intersections <- st_intersects(south_eez_line)
      
      # Problems were detected for the southern border where some elements of the updated_latitude_lines
      # are not properly connected, and therefore do not intersect, preventing the future polygonization.
      # Therefore, an exception is flagged.
      ## NOTES: (1) This has been observed to occur when the distance between RCA lines and latitude limits is at the microscale.
      ## This could be due to Spatial lat lon objects not being prepared to work at such a small scale.
      ## Since these distances are too small, we have opted to omit them and use the original multiline_sf,
      ## which won't substantially change the areas. (2) This requires further inspection since it could also be caused
      ## by, at that scale, higher proximity of other RCA nodes to the latitude lines (related to st_nearest_points).
      
      ## Check if any geometry only intersects with itself
      only_self_intersects <- sapply(1:length(ezz_intersections), function(i) {
        length(ezz_intersections[[i]]) == 1 && ezz_intersections[[i]] == i
      })
      
      if(any(only_self_intersects)) { 
        # Intersection not occurring, then using original multiline_sf
      } else { # Intersection occurs so we can proceed as with the Northen border.
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
  
  # Create polygon from lines
  ## Apply function to get the north and south lines needed for polygonization
  multiline_sf <- MultilineFromIntersections(latitude_lines, clipped_lines)
  ## Combine area delimitation lines into a MULTILINESTRING
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
  ## Some areas include inner polygons that do not constitute part of the RCA.
  ## These can be islands or coastal features, such as harbors.
  ## Additionally, self-closed RCA lines can result in inner polygons not related to RCAs.
  ## Below, we evaluate the relationships between polygons to discard those not pertaining to RCAs.
  ### Since it has been observed that all those affected areas are related to coastal features, we will base our analysis on
  ### whether they do intersect with coastline features.
  
  if (any(unlist(st_intersects(polygon_sf, coastline)))){
    # Generate islands and coastal features polygons.
    coastline_poly <- st_combine(st_union(coastline)) %>%
      st_polygonize() %>%
      st_collection_extract("POLYGON") %>%
      st_as_sf() %>%
      rename(geometry = x) %>%
      mutate(id = row_number(),.before=geometry)
    # Select those coastal polygons intersecting with our preliminary RCA.
    coastline_oi_index <- st_intersects(polygon_sf, coastline_poly)
    coastline_oi <- coastline_poly[unlist(coastline_oi_index),]
    # Select RCA subareas that are not in contact with land.
    no_land_rca <- st_difference(polygon_sf, st_union(coastline_poly))
    
    # Remove those RCA features that should not be generated if RCA lines do not self-intersect
    ## or if isolines do not intersect each other. To avoid associated problems with features like the Channel Islands,
    ## a basic approach has been used to discard them by adding a surface threshold that excludes them.
    ### Exclude polygons with an area greater than 3e9m (easy fix, there must be a better way).
    closed_intersections <- st_intersects(no_land_rca, sparse = FALSE)
    
    if (is.null(closed_intersections)){ ## -- to review -- some of it could be redundant
      rca_coastline_polygons_filtered <- no_land_rca
    } else {
      intersects_with_others <- apply(closed_intersections, 1, function(row) {
        sum(row) > 1  # Count TRUEs per row, excluding self
      })
      intersecting_polygons_indices <- which(intersects_with_others)
      selected_polygons <- no_land_rca[intersecting_polygons_indices, ]
      
      areas <- as.vector(st_area(selected_polygons)) 
      filtered_closed_rca <- selected_polygons[areas <= 3e9, ]
      
      selected_intersections <- st_intersects(coastline, filtered_closed_rca, sparse = FALSE)
      
      if(all(!selected_intersections)) { # No self-closing RCA features considered 
        rca_coastline_polygons_filtered <- no_land_rca
      } else { # Update any self-closing RCA feature. 
        intersection_indices <- apply(selected_intersections, 1, function(row) which(row)) %>%  unlist()
        areas_to_remove <- filtered_closed_rca[unlist(intersection_indices), ]
        rca_coastline_polygons_filtered <- st_difference(no_land_rca, st_union(areas_to_remove))
      }
    }
    
  } else {
    rca_coastline_polygons_filtered <- polygon_sf
  }
  
  # Plot for individual inspection. Deactivate when running for all instructions
  # map <- leaflet() %>%
  #   addTiles() %>%
  #   addPolygons(data = rca_coastline_polygons_filtered, color = "red", weight=1) %>%
  #   addPolylines(data = selected_latitude_lines, color = "blue", weight=2) %>%
  #   addPolylines(data = clipped_lines, color = "orange", weight=2)
  # 
  # print(map)
  
  return(rca_coastline_polygons_filtered)
}