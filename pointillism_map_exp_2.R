library(tidyverse)
library(sf)

# read in garden data (Local Authorities)
gardens <- readxl::read_xlsx("../datasets/ons_green_space/osprivateoutdoorspacereferencetables.xlsx",
                             sheet = "msoa_garden_in") %>%
  janitor::clean_names() %>%
  select(msoa_code, region_name,
         perc_garden = percentage_of_addresses_with_private_outdoor_space)


# read in Local Authority boundaries
lad_bound <- read_sf("../datasets/msoas/Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Generalised_Clipped_(BGC)_EW_V3.shp") %>%
  janitor::clean_names()

# create single table with garden and spatial data
gardens_spatial <- lad_bound %>%
  left_join(gardens, by = c("msoa11cd" = "msoa_code")) %>%
  filter(region_name == "London") %>%

  #bin perc garden
  # mutate(perc_garden_bin = cut(perc_garden,
  #                              breaks = c(0,0.2,0.4,0.6,0.8,1)))

  mutate(perc_garden_bin = cut(perc_garden,
                               breaks = seq(0,1,0.1))) %>%
  mutate(garden_anomoly = perc_garden - mean(perc_garden))

# partition local authorities by area
# (large local authorities to be represented with a dot)
partition_bound <- 1500000

gardens_spatial_grouped <- gardens_spatial %>%
  select(perc_garden, perc_garden_bin, shape_are, garden_anomoly) %>%
  mutate(is_small = if_else(shape_are < partition_bound,
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
ave_polygon_size <- mean(filled_polygons$shape_are)
radius <- sqrt(ave_polygon_size / pi)
centroids <- st_centroid(empty_polygons) %>%
  select(perc_garden, perc_garden_bin, garden_anomoly)

circles <- st_buffer(centroids, dist = radius)



# create plot

bound_width <- 0.25
bound_colour <- "grey70"
pallette_five_colour <- c('#a03670', '#925e6e', '#7f7c6c', '#639868', '#25b164')
pallette_ten_colour <- c('#52002e', '#700447', '#891e5c', '#a03670', '#b74c85', '#cf629a', '#e778b0', '#fa91c9', '#ffb0e9', '#ffd1ff')

ggplot() +
  geom_sf(data = filled_polygons, mapping = aes(fill = perc_garden_bin),
          size = bound_width, colour = bound_colour) +
  geom_sf(data = empty_polygons, fill = "white",
          colour = bound_colour, size = bound_width) +
  geom_sf(data = circles, mapping = aes(fill = perc_garden_bin), size = 0) +
  #scale_fill_viridis_c(option = "magma") +
  #scale_fill_gradient2(low = '#a03670', mid = "white", high = '#25b164') +
  #scale_fill_gradient2(low = "black", mid = "white", high = "green") +
  scale_fill_manual(values = pallette_ten_colour,
                    labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                               "60-69", "70-79", "80-89", "90-100"),
                    name = "Percentage\nof households\nwithout\na garden",
                    guide = guide_legend(reverse = TRUE)) +
  ggthemes::theme_map() +
  theme(plot.background = element_rect(fill = "white"))

ggsave("pointillist_choropleth.jpg",
       units = "mm",
       height = 210, width = 297)



ggplot() +
  geom_sf(data = gardens_spatial, mapping = aes(fill = perc_garden_bin),
          size = bound_width, colour = bound_colour) +

  scale_fill_manual(values = pallette_ten_colour,
                    labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                               "60-69", "70-79", "80-89", "90-100"),
                    name = "Percentage\nof households\nwithout\na garden",
                    guide = guide_legend(reverse = TRUE)) +
  ggthemes::theme_map() +
  theme(plot.background = element_rect(fill = "white"))

ggsave("standard_choropleth.jpg",
       units = "mm",
       height = 210, width = 297)
