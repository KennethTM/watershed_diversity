source("libs_and_funcs.R")

#Data for figures
fig_data <- readRDS(paste0(getwd(), "/figures/fig_gis_data.rds"))
fish_basin_species_count  <- fig_data[[1]]
all_model_data  <- fig_data[[2]]
dk_basins <- fig_data[[3]]
dk_border <- fig_data[[4]]

#Basin richness
basin_richness_freq <- fish_basin_species_count %>% 
  st_drop_geometry() %>% 
  ggplot(aes(n_spec_basin))+
  geom_histogram(fill = NA, col = "black")+
  ylab("Frequency")+
  xlab("Basin area (km2)")

basin_richness_map <- dk_basins %>% 
  left_join(st_drop_geometry(select(fish_basin_species_count, basin_id, n_spec_basin))) %>% 
  ggplot()+
  geom_sf(data = dk_border, fill = NA, col = "black")+
  geom_sf(aes(fill = n_spec_basin), col = "black", size = 0.25)+
  scale_fill_viridis_c(na.value="white", option = "E", name = "Basin richness")

basin_richness_area <- fish_basin_species_count %>% 
  st_drop_geometry() %>% 
  ggplot(aes(basin_area_m2, n_spec_basin))+
  geom_point(alpha = 0.3)+
  geom_density2d()+
  scale_x_log10()+
  ylab("Basin richness")+
  xlab("Basin area (log10(km2))")

basin_fig <- basin_richness_map + basin_richness_freq + basin_richness_area + plot_layout(ncol = 1) + plot_annotation(tag_levels = "A")

ggsave(paste0(getwd(), "/figures/basin_richness.png"), basin_fig, units = "mm", width = 129, height = 200)

#Investigated lakes and age
lake_age_bins <- all_model_data %>% 
  filter(!is.na(year_established)) %>% 
  mutate(age_bins = cut(year-year_established, breaks = c(0, 10, 20, 50, 100), 
                        labels = c("0-10", "10-20", "20-50", "50-100"),
                        include.lowest = TRUE))

lake_map_age <- all_model_data %>% 
  ggplot() +
  geom_sf(data = dk_border, col = "black")+
  geom_sf(aes(col = year-year_established), col = "grey")+
  geom_sf(data = lake_age_bins, aes(col = age_bins), alpha = 0.5)+
  scale_colour_viridis_d(option = "D", name = "Lake age")+
  theme(legend.position = c(0.8, 0.8))

ggsave(paste0(getwd(), "/figures/lake_map_age.png"), lake_map_age, units = "mm", width = 129, height = 100)
