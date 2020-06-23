source("libs_and_funcs.R")

#Data for figures
model_and_fig_data <- readRDS(paste0(getwd(), "/data_processed/model_and_fig_data.rds"))

#Percent cover of DK's area
#(sum(st_drop_geometry(basin_data)$basin_area_m2)*10^-9)/(42531525552*10^-9)

all_model_data <- model_and_fig_data[[2]]

dk_border <- st_read(dsn = gis_database, layer = "dk_border") 

dk_basins <- st_read(dsn = gis_database, layer = "dk_basins") %>% 
  mutate(basin_area_m2 = as.numeric(st_area(GEOMETRY)),
         basin_circum_m = as.numeric(st_length(st_cast(GEOMETRY, "MULTILINESTRING"))))

dk_iceage <- st_read(dsn = gis_database, layer = "dk_iceage") 

dk_iceage_cut <- st_intersection(dk_iceage %>% st_cast("LINESTRING"), dk_border) %>% 
  st_collection_extract("LINESTRING")

#Basin richness plots
basin_data <- model_and_fig_data[[1]] %>% 
  filter(!is.na(basin_sum_lake_area_m2))

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
  geom_sf(data = dk_iceage_cut, aes(linetype = "Ice age"), col = "red", linetype = 1, show.legend = FALSE)+
  scale_fill_viridis_c(na.value="white", option = "D", name = "Basin richness", direction = -1, begin = 0.2)

basin_fig <- basin_richness_map + basin_richness_freq + plot_layout(ncol = 1) + plot_annotation(tag_levels = "A")

ggsave(paste0(getwd(), "/figures/basin_richness.png"), basin_fig, units = "mm", width = 129, height = 150)
ggsave(paste0(getwd(), "/figures/basin_richness.svg"), basin_fig, units = "mm", width = 129, height = 150)

#Lake richness plots

#Get observation as also used for modeling
lake_map_age_df <- all_model_data %>% 
  mutate(lake_age = ifelse(is.na(year_established), 9999, year-year_established),
         lake_age_bins = cut(lake_age, breaks = c(-1, 10, 20, 50, 100, 10000), 
                        labels = c("0-10", "10-20", "20-50", "50-100", "Natural")),
         spec_prop = n_spec_lake/n_spec_basin,
         nat_or_art = factor(ifelse(lake_age_bins == "Natural", "Natural", "New"), levels = c("New", "Natural"))) %>% 
  filter(!is.na(basin_id)) %>% 
  select(-tn_mg_l, -tp_mg_l, -plant_area_perc, -zmax_m, -alk_meq_l,
         -lake_circum_log10, -plant_vol_perc, -zmean_m, -secchi_depth_m,
         -year_established) %>% 
  na.omit()
  
lake_map_age <- lake_map_age_df %>% 
  ggplot() +
  geom_sf(data = dk_border, col = "black", fill = NA)+
  geom_sf(aes(col = lake_age_bins), size = 0.7)+
  geom_sf(data = dk_iceage_cut, aes(linetype = "Ice age"), col = "red", linetype = 1, show.legend = FALSE)+
  scale_colour_manual(values = c(viridisLite::viridis(4, direction = -1), "coral"), name = "Lake age (years)")+
  guides(linetype = guide_legend(title = NULL, order = 2), colour = guide_legend(order = 1))

lake_spec_prop <- lake_map_age_df %>% 
  st_drop_geometry() %>% 
  ggplot()+
  geom_freqpoly(aes(spec_prop, col = nat_or_art))+
  geom_histogram(aes(spec_prop), fill = NA, col = "black", binwidth = 0.04)+
  scale_color_manual(values = c(viridisLite::viridis(1, begin = 0.5, end = 0.6), "coral"), name = "Lake group")+
  ylab("Frequency")+
  xlab("Lake:basin richness ratio")

lake_spec_prop_basin_rich <- lake_map_age_df %>% 
  st_drop_geometry() %>% 
  ggplot(aes(y=spec_prop, x = n_spec_basin, col = basin_area_m2)) +
  geom_vline(xintercept = 10, linetype = 3)+
  geom_point(size = 0.7)+
  ylab("Lake:basin richness ratio")+
  xlab("Basin species richness")+
  scale_colour_viridis_c(trans = "log10", name = expression(log[10]*"(basin area [m"^{2}*"])"), option = "D", begin = 0.1)

lake_fig <- lake_map_age + lake_spec_prop + lake_spec_prop_basin_rich + 
  plot_layout(ncol = 1) + 
  plot_annotation(tag_levels = "A")

ggsave(paste0(getwd(), "/figures/lake_richness.png"), lake_fig, units = "mm", width = 129, height = 200)
ggsave(paste0(getwd(), "/figures/lake_richness.svg"), lake_fig, units = "mm", width = 129, height = 200)
