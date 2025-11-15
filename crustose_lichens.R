library(tidyverse)
library(reshape2)
library(gganimate)
library(colorjam)
library(EBImage)


source("initial_environment.R")
source("evolve_lichens.R")

## SETTINGS FOR LICHENS
smooth_type <- "conv" # "conv" = smoothed, "mean" = more pixel art style
grid_size       <- c(100, 100)
colonies_number <- sample(1:5, 1)
survive         <- c(0, 1, 2, 3, 4, 5, 6, 7, 8)
born            <- c(3, 6, 7, 8)

generations <- sample(100:150, colonies_number)
environment <- generate_grid(grid_size)

colonies <- generate_colonies(c("triangle", "rectangle"), colonies_number, grid_size)

background_color <- "black"
lichen_colours   <- c("#d7c53c", "#852513", "#d2f6f2", "#dd7d32", "#916035", 
                      "#ffc4a8", "#fdf0a0", "#f2fedc", "#fcf9f0", "#ff518c",
                      "#d11020", "#185300", "#4f852e", "#88726c", "#4a7e92",
                      "#ad8410", "#f9f871", "#d9d5a0", "#fc734e", "#ba2100")

colonies_colour <- sample(lichen_colours, size=colonies_number, replace=FALSE)
colours <- lapply(colonies_colour, function(x) {c(background_color, x)})

## GENERATE LICHENS

# Generate populations
lichens <- multiple_pupulations(environment, colonies, generations, survive, born, colours, smooth_type="conv",  wrap=TRUE)

# Obtain nice colour blending (might take a while)
final <- lichens %>% 
  group_by(Var1, Var2) %>% 
  mutate(blended = blend_colors(colour)) 

# Generate plot
final %>% 
  ggplot(aes(x=Var1, y=Var2)) +
  geom_raster(fill=final$blended) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none")


