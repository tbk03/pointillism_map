library(tidyverse)
library(sf)

# read in garden data (Local Authorities)
gardens <- readxl::read_xlsx("../datasets/ons_green_space/osprivateoutdoorspacereferencetables.xlsx",
                  sheet = "lad_garden_in") %>%
  janitor::clean_names() %>%
  select(lad_code, region_name,
         perc_garden = percentage_of_addresses_with_private_outdoor_space)


# read in Local Authority boundaries
lad_bound <- read_sf("../datasets/local_authority_districts/Local_Authority_Districts_(December_2019)_Boundaries_UK_BGC.shp") %>%
  janitor::clean_names()

# create single table with garden and spatial data
gardens_spatial <- lad_bound %>%
  left_join(gardens, by = c("lad19cd" = "lad_code")) %>%
  # filter(region_name == "London")
  filter(!is.na(perc_garden))



# partition local authorities by area
# (large local authorities to be represented with a dot)
partition_bound <- 500000000

gardens_spatial_grouped <- gardens_spatial %>%
  select(perc_garden, st_areasha) %>%
  mutate(is_small = if_else(st_areasha < partition_bound,
                            TRUE, FALSE)) %>%
  group_by(is_small)

# create seperate dataframes for small and large local authorities
split_df <- group_split(gardens_spatial_grouped)

# polygons to plotted as per standard choropleth
filled_polygons <- split_df[[2]]

# LA boundaries to host circles (for large LAs)
empty_polygons <- split_df[[1]]

# the circles themselves

# calculate radius of circles to give are equal to the largest polygons
radius <- sqrt(partition_bound / pi)
centroids <- st_centroid(empty_polygons) %>%
  select(perc_garden)

circles <- st_buffer(centroids, dist = radius)



# create plot

bound_width <- 0.15

ggplot() +
  geom_sf(data = filled_polygons, mapping = aes(fill = perc_garden),
          size = bound_width, colour = "grey80") +
  geom_sf(data = empty_polygons, fill = "white",
          colour = "grey80", size = bound_width) +
  geom_sf(data = circles, mapping = aes(fill = perc_garden), size = 0) +
  #scale_fill_viridis_c(option = "magma") +
  scale_fill_gradient2(low = "black", mid = "white", high = "green") +
  ggthemes::theme_map()
  # plot LAs represented with fill colour

  # plot LAs represented with a dot
