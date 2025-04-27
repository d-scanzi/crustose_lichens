## NEIGHBOURS
check_neighbours <- function(grid, coordinates, wrap = TRUE) {
  grid_row_number    <- nrow(grid)
  grid_column_number <- ncol(grid)
  
  if (wrap) {
    row_indices <- (coordinates[1] + c(-1, 0, 1) - 1) %% grid_row_number + 1
    col_indices <- (coordinates[2] + c(-1, 0, 1) - 1) %% grid_column_number + 1
  } else {
    row_indices <- coordinates[1] + c(-1, 0, 1)
    col_indices <- coordinates[2] + c(-1, 0, 1)
    
    # Only keep indices that are within bounds
    row_indices <- row_indices[row_indices >= 1 & row_indices <= grid_row_number]
    col_indices <- col_indices[col_indices >= 1 & col_indices <= grid_column_number]
  }
  
  neighborhood <- grid[row_indices, col_indices, drop = FALSE]
  
  # Subtract the value of the central cell if it's included
  total <- sum(neighborhood)
  if (coordinates[1] %in% row_indices && coordinates[2] %in% col_indices) {
    total <- total - grid[coordinates[1], coordinates[2]]
  }
  
  total
}

## EVOLVE COLONY

# Update single cell
update_cell <- function(grid, coordinates, survive, born, wrap=TRUE) {
  # Check neighbours
  
  cell_neighbours <-check_neighbours(grid, coordinates, wrap=wrap)
  if (grid[coordinates[1], coordinates[2]] == 1) {
    if (!cell_neighbours %in% survive) {
      return(0)
    }
  } else {
    if (cell_neighbours %in% born) {
      return(1)
    }
  }
  
  return(grid[coordinates[1], coordinates[2]])
}

# Update entire grid once
new_generation <- function(grid, survive, born, wrap=TRUE) {
  row_number    <- nrow(grid)
  column_number <- ncol(grid)
  new_grid <- grid
  for (row in 1:row_number) {
    for (column in 1:column_number) {
      new_grid[row, column] <- update_cell(grid, c(row, column), survive=survive, born=born, wrap=wrap)
    }
  }
  return(new_grid)
}

# Evolve grid for N generations
evolve <- function(grid, generations, survive=c(2,3), born=c(3), wrap=TRUE) {
  current_grid <- grid
  all_grids    <- list(grid)
  for (generation in 1:generations) {
    current_grid <- new_generation(current_grid, survive=survive, born=born, wrap=wrap)
    all_grids <- append(all_grids, list(current_grid))
  }
  return(list("final_generation"=current_grid, "all_generations"=all_grids))
}

# Smooth results across generations
smooth_generations <- function(generation_list) {
  generations <- length(generation_list)
  generation_data <- list()
  for (generation in 1:generations) {
    current_grid <- generation_list[[generation]]
    current_grid_long <- melt(current_grid)
    current_grid_long["generation"] <- generation
    generation_data <- append(generation_data, list(current_grid_long))
  }
  
  generation_data_df <- Reduce(rbind, generation_data)
  
  evolution_smoothed <- generation_data_df %>% 
    group_by(Var1, Var2) %>% 
    summarise(result = mean(value)) %>% 
    ungroup()
  
  return(evolution_smoothed)
}

## MULTIPLE POPULATIONS
generate_colonies <- function(shapes, number, grid_dim) {
  colonies <- list()
  for (colony in 1:number) {
    current_shape <- sample(shapes, 1)
    pivot_point   <- c(sample(1:grid_dim[1], 1), sample(1:grid_dim[2], 1))
    angle         <- sample(0:360, 1)
    
    if (current_shape == "triangle") {
      triangle_size <- sample(2:6, 1)
      parameters <- list(name=current_shape, top=pivot_point, size=triangle_size, angle=angle, grid_dim=grid_dim)
    } else if (current_shape == "rectangle") {
      width  <- sample(4:12, 1)
      height <- sample(4:12, 1)
      parameters <- list(name=current_shape, centre=pivot_point, width=width, height=height, angle=angle, grid_dim=grid_dim)
    } else if (current_shape == "semicircle") {
      radius <- sample(4:12)
      parameters <- list(name=current_shape, centre=pivot_point, radius=radius, angle=angle, grid_dim=grid_dim)
    }
    
    colonies <- append(colonies, list(parameters))
  }
  return(colonies)
}

multiple_pupulations <- function(grid, populations, generations, survive, born, colours, wrap=TRUE) {
  final_generations <- list()
  
  grid_rows    <- nrow(grid)
  grid_columns <- ncol(grid)
  for (population in 1:length(populations)) {
    population_first_generation <- grid
    # seed current population
    population_seed <- generate_seed(populations[[population]], c(grid_rows, grid_columns))
    population_first_generation <- seed_grid(population_first_generation, population_seed)
    # Evolve population
    population_evolution <- evolve(population_first_generation, generations[population], survive=survive, born=born, wrap=wrap)
    # Smooth across generations
    population_evolution_smoothed <- smooth_generations(population_evolution$all_generations)
    # Add color information
    min_value <- min(population_evolution_smoothed$result)
    max_value <- max(population_evolution_smoothed$result)
    
    palette <- scales::pal_seq_gradient(low=colours[[population]][1], high=colours[[population]][2])
    colour_mapper <- scales::col_numeric(palette=palette, domain=c(min_value, max_value))
    
    population_evolution_smoothed <- population_evolution_smoothed %>%
      mutate(colour     = colour_mapper(result),
             population = population)
    # Store info
    final_generations <- append(final_generations, list(population_evolution_smoothed))
    
  }
  
  evolution_data <- Reduce(rbind, final_generations)
  return(evolution_data)
}
