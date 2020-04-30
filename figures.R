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
  xlab("Basin richness")

basin_richness_map <- dk_basins %>% 
  left_join(st_drop_geometry(select(fish_basin_species_count, basin_id, n_spec_basin))) %>% 
  ggplot()+
  geom_sf(data = dk_border, fill = NA, col = "black")+
  geom_sf(aes(fill = n_spec_basin), col = "black", size = 0.25)+
  scale_fill_viridis_c(na.value="white", option = "D", name = "Basin richness", direction = -1)

df <- fish_basin_species_count %>% 
  mutate(basin_area_log10 = log10(basin_area_m2)) %>% 
  cbind(st_coordinates(st_centroid(.))) %>% 
  st_drop_geometry()

# m0 <- glm(n_spec_basin ~ basin_area_log10, family = "quasipoisson", data = df)
# summary(m0)

# library(mgcv);library(gratia)
# m <- gam(n_spec_basin ~ s(basin_area_log10) + s(X, Y), family = "poisson", data = df)
# summary(m)
# plot(m)
# draw(m)
# appraise(m)

basin_richness_area <- fish_basin_species_count %>% 
  mutate(basin_area_log10 = log10(basin_area_m2)) %>% 
  st_drop_geometry() %>% 
  ggplot(aes(basin_area_log10, n_spec_basin))+
  geom_point(alpha = 0.3)+
  geom_density2d()+
  #geom_smooth(method = 'glm', method.args = list(family = 'poisson'))+
  ylab("Basin richness")+
  xlab("Basin area (log10(km2))")

basin_fig <- basin_richness_map + basin_richness_freq + basin_richness_area + plot_layout(ncol = 1) + plot_annotation(tag_levels = "A")

ggsave(paste0(getwd(), "/figures/basin_richness.png"), basin_fig, units = "mm", width = 129, height = 200)

#Investigated lakes and age
lake_map_age <- all_model_data %>% 
  mutate(lake_age = ifelse(is.na(year_established), 9999, year-year_established),
         lake_age_bins = cut(lake_age, breaks = c(0, 10, 20, 50, 100, 10000), 
                        labels = c("0-10", "10-20", "20-50", "50-100", "Unknown"),
                        include.lowest = TRUE)) %>% 
  ggplot() +
  geom_sf(data = dk_border, col = "black")+
  geom_sf(aes(col = lake_age_bins))+
  scale_colour_viridis_d(option = "D", name = "Lake age", direction = -1)+
  theme(legend.position = c(0.8, 0.8))

ggsave(paste0(getwd(), "/figures/lake_map_age.png"), lake_map_age, units = "mm", width = 129, height = 100)
