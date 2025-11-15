library(sp)

## ENVIRONMENT
generate_grid <- function(size) {
  grid           <- matrix(0, size[1], size[2])
  rownames(grid) <- seq(1, size[1])
  colnames(grid) <- seq(1, size[2])
  return(grid)
}

## SEED ENVIRONMENT
seed_grid <- function(grid, region) {
  seeded_grid <- grid
  for (coordinates in region) {
    seeded_grid[coordinates[1], coordinates[2]] <- abs(seeded_grid[coordinates[1], coordinates[2]] - 1) 
  }
  return(seeded_grid)
}

# Rotate a list of seed coordinates around a pivot point
rotate_point <- function(point_coordinates, pivot_coordinates, angle_degrees) {
  angle_rad <- angle_degrees * (pi/180)
  rotation_matrix <- matrix(c(cos(angle_rad), -sin(angle_rad), 
                              sin(angle_rad),  cos(angle_rad)), nrow=2, byrow=TRUE)
  as.vector(rotation_matrix %*% (point_coordinates - pivot_coordinates) + pivot_coordinates)
}

# Generate triangular seed
triangle_seed <- function(top, size, angle=0, grid_dim=NULL) {
  
  # Step 1: Define triangle corners
  height <- size - 1
  base_half_width <- height # so it's symmetric
  
  corners <- list(
    c(top[1], top[2]),                                  
    c(top[1] - base_half_width, top[2] + height),
    c(top[1] + base_half_width, top[2] + height)
  )
  
  # Rotate corners around 'top' (pivot)
  rotated_corners <- lapply(corners, function(pt) {
    rotate_point(pt, pivot=top, angle_degrees=angle)
  })
  rotated_corners_mat <- do.call(rbind, rotated_corners)
  
  # Find bounding box
  min_x <- floor(min(rotated_corners_mat[,1]))
  max_x <- ceiling(max(rotated_corners_mat[,1]))
  min_y <- floor(min(rotated_corners_mat[,2]))
  max_y <- ceiling(max(rotated_corners_mat[,2]))
  
  # Test all points in bounding box
  coords <- list()
  for (x in min_x:max_x) {
    for (y in min_y:max_y) {
      inside <- point.in.polygon(x, y, rotated_corners_mat[,1], rotated_corners_mat[,2])
      if (inside > 0) {
        coords <- append(coords, list(c(x, y)))
      }
    }
  }
  
  # Wrap if grid_dim provided
  if (!is.null(grid_dim)) {
    coords <- lapply(coords, function(cell) {
      c(((cell[1]-1) %% grid_dim[1]) + 1, ((cell[2]-1) %% grid_dim[2]) + 1)
    })
  }
  
  return(coords)
}

# Generate rectangular seed
rectangle_seed <- function(centre, width, height, angle=0, grid_dim=NULL) {
  
  # Step 1: Define rectangle corners around (0,0)
  half_w <- (width - 1) / 2
  half_h <- (height - 1) / 2
  corners <- list(
    c(centre[1] - half_h, centre[2] - half_w),
    c(centre[1] - half_h, centre[2] + half_w),
    c(centre[1] + half_h, centre[2] + half_w),
    c(centre[1] + half_h, centre[2] - half_w)
  )
  
  # Step 2: Rotate corners around centre
  rotated_corners <- lapply(corners, function(pt) {
    rotate_point(pt, pivot=centre, angle_degrees=angle)
  })
  rotated_corners_mat <- do.call(rbind, rotated_corners)
  
  # Step 3: Find bounding box to speed things up
  min_x <- floor(min(rotated_corners_mat[,1]))
  max_x <- ceiling(max(rotated_corners_mat[,1]))
  min_y <- floor(min(rotated_corners_mat[,2]))
  max_y <- ceiling(max(rotated_corners_mat[,2]))
  
  # Test all points in bounding box
  coords <- list()
  for (x in min_x:max_x) {
    for (y in min_y:max_y) {
      inside <- point.in.polygon(x, y, rotated_corners_mat[,1], rotated_corners_mat[,2])
      if (inside > 0) {
        coords <- append(coords, list(c(x, y)))
      }
    }
  }
  
  # Wrap if grid_dim provided
  if (!is.null(grid_dim)) {
    coords <- lapply(coords, function(cell) {
      c(((cell[1]-1) %% grid_dim[1]) + 1, ((cell[2]-1) %% grid_dim[2]) + 1)
    })
  }
  
  return(coords)
}

# wrapper around shapes generators
generate_seed <- function(parameters, grid_dim) {
  if (parameters[["name"]]== "triangle") {
    seed <-triangle_seed(top=parameters[["top"]], size=parameters[["size"]], angle=parameters[["angle"]], grid_dim=grid_dim)
  } else if  (parameters[["name"]] == "rectangle") {
    seed <-rectangle_seed(centre=parameters[["centre"]], width=parameters[["width"]], height=parameters[["height"]], angle=parameters[["angle"]], grid_dim=grid_dim)
  } else if (parameters[["name"]] == "semicircle") {
    seed <-semicircle_seed(centre=parameters[["centre"]], radius=parameters[["radius"]], angle=parameters[["angle"]], grid_dim=grid_dim)
  } else {
    print("Seed shape not found")
  }
  
  return(seed)
}
