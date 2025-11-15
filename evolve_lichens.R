monrose_kernel <- matrix(c(1,1,1,
                           1,0,1,
                           1,1,1), nrow=3, byrow=TRUE)

## NEIGHBOURS
count_neighbours <- function(grid, wrap=TRUE) {
  mode_type <- if (wrap) "circular" else "constant"
  neighbour_matrix <- filter2(grid, monrose_kernel, boundary=mode_type)
  return(neighbour_matrix)
}

## EVOLVE COLONY

# Update entire grid once
new_generation <- function(grid, survive, born, wrap=TRUE) {
  neighbours <- round(count_neighbours(grid, wrap=wrap))
  
  grid_rows    <- nrow(grid)
  grid_columns <- ncol(grid)
  
  survive_mask <- (grid == 1) & (neighbours %in% survive)
  born_mask    <- (grid == 0) & (neighbours %in% born)
  
  new_grid <- matrix(0, nrow = nrow(grid), ncol = ncol(grid))
  new_grid[survive_mask | born_mask] <- 1
  
  return(new_grid)
}

# Evolve grid for N generations
evolve <- function(grid, generations, survive=c(2,3), born=c(3), wrap=TRUE) {
  current_grid <- grid
  all_grids    <- array(rep(0, length(grid)*generations), c(nrow(grid), ncol(grid), generations))
  for (generation in 1:generations) {
    current_grid <- new_generation(current_grid, survive=survive, born=born, wrap=wrap)
    all_grids[, , generation] <- current_grid
  }
  return(list("final_generation"=current_grid, "all_generations"=all_grids))
}

# Smooth results across generations
smooth_generations_mean <- function(generation_matrix) {
  generations_n         <- dim(generation_matrix)[3]
  generations_weights   <- 1:generations_n
  evolution_smoothed    <- apply(generation_matrix, c(1, 2), stats::weighted.mean, w=generations_weights)
  return(evolution_smoothed)
}

smooth_generations_conv <- function(generation_matrix, sigma = 3) {
  time_points  <- dim(generation_matrix)[3]
  kernel       <- dnorm(seq(-3, 3, length.out = time_points), sd = sigma)
  kernel       <- kernel / sum(kernel)
  weighted_sum <- apply(generation_matrix, c(1, 2), function(x) sum(x * kernel))
  return(weighted_sum)
}

smooth_generations <- function(generation_matrix, type="mean") {
  if (type == "mean") {
    smooth_generations_mean(generation_matrix)
  } else if (type == "conv") {
    smooth_generations_conv(generation_matrix)
  }
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

multiple_pupulations <- function(grid, populations, generations, survive, born, colours, smooth_type="mean", wrap=TRUE) {
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
    population_evolution_smoothed <- smooth_generations(population_evolution$all_generations, type=smooth_type)
    
    # Add color information
    min_value <- min(population_evolution_smoothed)
    max_value <- max(population_evolution_smoothed)
    
    palette <- scales::pal_seq_gradient(low=colours[[population]][1], high=colours[[population]][2])
    colour_mapper <- scales::col_numeric(palette=palette, domain=c(min_value, max_value))
    
    population_evolution_smoothed <- reshape2::melt(population_evolution_smoothed)
    
    population_evolution_smoothed <- population_evolution_smoothed %>%
      mutate(colour     = colour_mapper(value),
             population = population)
    # Store info
    final_generations <- append(final_generations, list(population_evolution_smoothed))
    
  }
  
  evolution_data <- data.table::rbindlist(final_generations)
  return(evolution_data)
}
