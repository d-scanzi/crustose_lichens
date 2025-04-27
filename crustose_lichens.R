library(tidyverse)
library(reshape2)
library(gganimate)
library(colorjam)


source("initial_environment.R")
source("evolve_lichens.R")

## GENERATE AUTOMATA

# Initial settings
grid_size   <- c(100, 200)
generations <- 100
first_generation <- generate_grid(grid_size)

# seed_coordinates <- triangle_seed(c(50,50), 10, angle=32, c(grid_size, grid_size))
# seed_coordinates <- rectangle_seed(c(50,50), 20, 20, 30, c(grid_size, grid_size))


# seed_coordinates <- generate_seed(first_generation)

first_generation <- seed_grid(first_generation, seed_coordinates)

evolutions <- evolve(first_generation, generations, survive=c(0, 1, 2, 3, 4, 5, 6, 7, 8), born=c(3, 6, 7,8))
grid_long <- smooth_generations(evolutions$all_generations)

grid_long <- melt(first_generation)

# Plot final generation
grid_long %>% 
  ggplot(aes(x=Var1, y=Var2, fill=result)) +
  geom_tile() +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none")

## SMOOTHING TEST
evolution_smoothed <- smooth_generations(evolutions$all_generations)

evolution_smoothed %>% 
  ggplot(aes(x=Var1, y=Var2, fill=result)) +
  geom_raster() +
  scale_fill_gradient(low="black", high="#d7c53c") +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none")

## MULTIPLE POPULATIONS

for (k in 1:20) {
  
  grid_size       <- c(100, 141)
  colonies_number <- sample(1:5, 1)
  survive         <- c(0, 1, 2, 3, 4, 5, 6, 7, 8)
  born            <- c(3, 6, 7,8)
  
  generations <- sample(100:150, colonies_number)
  environment <- generate_grid(grid_size)
  
  colonies <- generate_colonies(c("triangle", "rectangle"), colonies_number, grid_size)
  
  background_color <- "black"
  lichen_colours   <- c("#d7c53c", "#852513", "#d2f6f2", "#dd7d32", "#916035", 
                        "#ffc4a8", "#fa8d74", "#f2fedc", "#fcf9f0", "#ff518c",
                        "#702f5b", "#185300", "#4f852e", "#88726c", "#4a7e92",
                        "#ad8410", "#f9f871", "#d9d5a0")
  
  colonies_colour <- sample(lichen_colours, size=colonies_number, replace=FALSE)
  colours <- lapply(colonies_colour, function(x) {c(background_color, x)})
  
  lichens <- multiple_pupulations(environment, colonies, generations, survive, born, colours, wrap=TRUE)
  
  final <- lichens %>% 
    filter(colour != background_color) %>% 
    group_by(Var1, Var2) %>% 
    mutate(blended = blend_colors(colour))
  
  final %>% 
    ggplot(aes(x=Var1, y=Var2)) +
    geom_raster(fill=final$blended) +
    coord_equal() +
    theme_void() +
    theme(legend.position = "none")
  
  picture_name <- paste0("lichen_", k, ".svg")
  
  ggsave(picture_name,
         path = "/home/daniele/Pictures/lichens_art")
}


# Background: black -> MOODY results