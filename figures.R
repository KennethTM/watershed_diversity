source("libs_and_funcs.R")

#Data for figures
model_and_fig_data <- readRDS(paste0(getwd(), "/data_processed/model_and_fig_data.rds"))
basin_data <- model_and_fig_data[[1]]
all_model_data <- model_and_fig_data[[2]]

dk_border <- st_read(dsn = gis_database, layer = "dk_border") 

dk_basins <- st_read(dsn = gis_database, layer = "dk_basins") %>% 
  mutate(basin_area_m2 = as.numeric(st_area(GEOMETRY)),
         basin_circum_m = as.numeric(st_length(st_cast(GEOMETRY, "MULTILINESTRING"))))

dk_iceage <- st_read(dsn = gis_database, layer = "dk_iceage") 

dk_iceage_cut <- st_intersection(dk_iceage %>% st_cast("LINESTRING"), dk_border) %>% 
  st_collection_extract("LINESTRING")

#Basin richness
basin_richness_freq <- basin_data %>% 
  st_drop_geometry() %>% 
  ggplot(aes(n_spec_basin))+
  geom_histogram(fill = NA, col = "black", binwidth = 2)+
  scale_x_continuous(limits = c(1, 41))+
  ylab("Frequency")+
  xlab("Basin richness")

basin_richness_map <- dk_basins %>% 
  left_join(st_drop_geometry(select(basin_data, basin_id, n_spec_basin))) %>% 
  ggplot()+
  geom_sf(data = dk_border, fill = NA, col = "black")+
  geom_sf(aes(fill = n_spec_basin), col = "black", size = 0.25)+
  geom_sf(data = dk_iceage_cut, aes(linetype = "Ice age"), col = "white", linetype = 2, show.legend = FALSE)+
  #scale_linetype_manual(values = c(2))+
  #scale_fill_gradient(low = grey(1), high = grey(0.1))
  scale_fill_viridis_c(na.value="white", option = "D", name = "Basin richness", direction = -1, begin = 0.2)

# basin_richness_area <- basin_data %>% 
#   mutate(basin_sum_lake_area_m2 = ifelse(is.na(basin_sum_lake_area_m2), 0, basin_sum_lake_area_m2),
#          basin_sum_lake_area_m2_log10 = log10(basin_sum_lake_area_m2+1)) %>% 
#   st_drop_geometry() %>% 
#   ggplot(aes(basin_sum_lake_area_m2_log10, n_spec_basin))+
#   geom_point(alpha = 0.3)+
#   geom_density2d(col = "coral")+
#   #geom_smooth(method = 'glm', method.args = list(family = 'poisson'))+
#   ylab("Basin richness")+
#   xlab("Basin lake area (log10+1(km2))")

basin_fig <- basin_richness_map + basin_richness_freq + plot_layout(ncol = 1) + plot_annotation(tag_levels = "A")

ggsave(paste0(getwd(), "/figures/basin_richness.png"), basin_fig, units = "mm", width = 129, height = 150)



#Investigated lakes and age
lake_map_age_df <- all_model_data %>% 
  mutate(lake_age = ifelse(is.na(year_established), 9999, year-year_established),
         lake_age_bins = cut(lake_age, breaks = c(-1, 10, 20, 50, 100, 10000), 
                        labels = c("0-10", "10-20", "20-50", "50-100", "Natural")),
         spec_prop = n_spec_lake/n_spec_basin,
         nat_or_art = ifelse(lake_age_bins == "Natural", "Natural", "New"))

lake_map_age <- lake_map_age_df %>% 
  ggplot() +
  geom_sf(data = dk_border, col = "black")+
  #geom_sf(data = dk_iceage_cut, aes(linetype = "Ice age"), col = "grey")+
  geom_sf(aes(col = lake_age_bins), size = 0.7)+
  geom_sf(data = dk_iceage_cut, aes(linetype = "Ice age"), col = "black", linetype = 2, show.legend = FALSE)+
  #scale_colour_viridis_d(option = "D", name = "Lake age (years)", direction = -1)+
  scale_colour_manual(values = c(viridisLite::viridis(4, direction = -1), "coral"), name = "Lake age (years)")+
  #theme(legend.position = c(0.8, 0.75))+
  guides(linetype = guide_legend(title = NULL, order = 2), colour = guide_legend(order = 1))

lake_spec_prop <- lake_map_age_df %>% 
  st_drop_geometry() %>% 
  ggplot()+
  #geom_density(aes(spec_prop), fill = NA)+
  geom_freqpoly(aes(spec_prop, col = nat_or_art))+
  geom_histogram(aes(spec_prop), fill = NA, col = "black", binwidth = 0.04)+
  scale_color_manual(values = c("coral", "black"), name = "Lake class")+
  ylab("Frequency")+
  xlab("Lake:basin richness ratio")#+ lav legend title
  #theme(legend.title = element_blank(), legend.position = c(0.7, 0.8))

lake_map_age_df %>% 
  st_drop_geometry() %>% 
  ggplot(aes(y=spec_prop, x = n_spec_basin, col = lake_age_bins)) +
  geom_point()+
  ylab("Lake:basin richness ratio")+
  xlab("Basin species richness")+
  scale_colour_manual(values = c(viridisLite::viridis(4, direction = -1), "coral"), name = "Lake age (years)")

slake_fig <- lake_map_age + lake_spec_prop + plot_layout(ncol = 1) + plot_annotation(tag_levels = "A")

ggsave(paste0(getwd(), "/figures/lake_richness.png"), lake_fig, units = "mm", width = 129, height = 150)
