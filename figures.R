source("libs_and_funcs.R")

#Load common spatial data
dk_border <- st_read(dsn = gis_database, layer = "dk_border") 
dk_iceage <- st_read(dsn = gis_database, layer = "dk_iceage") 

dk_iceage_cut <- dk_iceage %>% 
  st_cast("LINESTRING") %>% 
  st_intersection(dk_border) %>% 
  st_collection_extract("LINESTRING")

model_data_raw <- read_csv(paste0(getwd(), "/data_processed/lake_species_all.csv")) 

fish_species_lakes <- st_read(gis_database, layer = "fish_species_lakes")

#Figure 1
#Drainage basin species richness
basins <- st_read(gis_database, layer = "basins")
basin_species_count <- read_csv(paste0(getwd(), "/data_processed/basin_species_count.csv"))

basins_count <- basins %>% 
  left_join(basin_species_count) %>% 
  mutate(n_spec_basin = ifelse(is.na(n_spec_basin), 0, n_spec_basin)) %>% 
  st_crop(st_bbox(dk_border))

table(basins_count$n_spec_basin)

basin_freq <- basins_count %>% 
  ggplot(aes(n_spec_basin))+
  geom_histogram(col = "white", fill = "grey", binwidth = 1)+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  coord_cartesian(ylim=c(0, 160))+
  annotate("text", x = 0, y = 160, label = "831", size=3)+
  ylab("Frequency")+
  xlab("Basin richness")

xlabs <- seq(8, 12, 1)
ylabs <- seq(54.5, 57.5, 0.5)

basin_richness <- basins_count %>% 
  ggplot()+
  geom_sf(data = dk_border, fill = NA, col = "black")+
  geom_sf(aes(fill = n_spec_basin), col = "black", size = 0.20)+
  geom_sf(data = dk_iceage_cut, aes(linetype = "Ice age"), col = "coral", linetype = 1, show.legend = FALSE)+
  scale_fill_viridis_c(na.value="white", option = "D", name = "Species richness", direction = -1, begin = 0.2)+
  scale_x_continuous(breaks = xlabs, labels = paste0(xlabs,'°E')) +
  scale_y_continuous(breaks = ylabs, labels = paste0(ylabs,'°N'))+
  theme(legend.position = "bottom")+
  guides(fill=guide_colorbar(title.position = "top", barwidth = 8, title.hjust = 0.5))

figure_1 <- basin_richness + basin_freq + plot_layout(ncol = 1) + plot_annotation(tag_levels = "A")

figure_1

ggsave(paste0(getwd(), "/figures/figure_1.png"), figure_1, units = "mm", width = 84, height = 160)

#Figure 2
#Lake richness plot
lake_coords <- fish_species_lakes %>% 
  select(gml_id) %>% 
  distinct()

figure_2_data <- model_data_raw %>% 
  mutate(lake_age = ifelse(is.na(year_established), 9999, year_sample - year_established),
         lake_age_bins = cut(lake_age, breaks = c(-1, 10, 20, 50, 100, 10000), 
                             labels = c("0-10", "10-20", "20-50", "50-100", "Natural")),
         lake_cat = factor(ifelse(lake_age_bins == "Natural", "Natural", "New"), levels = c("New", "Natural"))) %>% 
  select(gml_id, n_spec_lake, lake_cat, lake_age_bins, lake_age) %>% 
  left_join(lake_coords) %>% 
  st_as_sf() %>% 
  st_crop(st_bbox(dk_border))

lake_map <- figure_2_data %>% 
  ggplot() +
  geom_sf(data = dk_border, col = "grey", fill = NA)+
  geom_sf(aes(col = lake_age_bins), size = 0.7)+
  geom_sf(data = dk_iceage_cut, aes(linetype = "Ice age"), col = "coral", linetype = 1, show.legend = FALSE)+
  scale_colour_manual(values = c(viridisLite::viridis(4, direction = -1, begin = 0.3), "black"), name = "Lake age (years)")+
  guides(linetype = guide_legend(title = NULL, order = 2), colour = guide_legend(order = 1))+
  scale_x_continuous(breaks = xlabs, labels = paste0(xlabs,'°E')) +
  scale_y_continuous(breaks = ylabs, labels = paste0(ylabs,'°N'))

lake_freq <- figure_2_data %>% 
  st_drop_geometry() %>% 
  ggplot(aes(x=n_spec_lake))+
  geom_histogram(fill = "grey", col = "white", binwidth = 1)+
  geom_density(aes(y=..count.., col = lake_cat), position = position_stack())+
  scale_color_manual(values = c(viridisLite::viridis(4, direction = -1, begin = 0.3)[2], "black"), name = "Lake group")+
  ylab("Frequency")+
  xlab("Species richness")+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

figure_2 <- lake_map + lake_freq + plot_layout(ncol = 1) + plot_annotation(tag_levels = "A")

figure_2

ggsave(paste0(getwd(), "/figures/figure_2.png"), figure_2, units = "mm", width = 129, height = 160)








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
  
xlabs <- seq(8, 12, 1)
ylabs <- seq(54.5, 57.5, 0.5)

lake_map_age <- lake_map_age_df %>% 
  ggplot() +
  geom_sf(data = dk_border, col = "black", fill = NA)+
  geom_sf(aes(col = lake_age_bins), size = 0.7)+
  geom_sf(data = dk_iceage_cut, aes(linetype = "Ice age"), col = "red", linetype = 1, show.legend = FALSE)+
  scale_colour_manual(values = c(viridisLite::viridis(4, direction = -1), "coral"), name = "Lake age (years)")+
  guides(linetype = guide_legend(title = NULL, order = 2), colour = guide_legend(order = 1))+
  scale_x_continuous(breaks = xlabs, labels = paste0(xlabs,'°E')) +
  scale_y_continuous(breaks = ylabs, labels = paste0(ylabs,'°N'))

lake_spec_prop <- lake_map_age_df %>% 
  st_drop_geometry() %>% 
  ggplot()+
  geom_freqpoly(aes(spec_prop, col = nat_or_art))+
  geom_histogram(aes(spec_prop), fill = NA, col = "black", binwidth = 0.04)+
  scale_color_manual(values = c(viridisLite::viridis(1, begin = 0.5, end = 0.6), "coral"), name = "Lake group")+
  ylab("Frequency")+
  xlab("Richness ratio")

lake_spec_prop_basin_rich <- lake_map_age_df %>% 
  st_drop_geometry() %>% 
  ggplot(aes(y=spec_prop, x = n_spec_basin, col = basin_area_m2)) +
  geom_vline(xintercept = 10, linetype = 3)+
  geom_point(size = 0.7)+
  ylab("Richness ratio")+
  xlab("Basin species richness")+
  scale_colour_viridis_c(trans = "log10", name = expression(log[10]*"(basin area [m"^{2}*"])"), option = "D", begin = 0.1)

lake_fig <- lake_map_age + lake_spec_prop + lake_spec_prop_basin_rich + 
  plot_layout(ncol = 1) + 
  plot_annotation(tag_levels = "A")

ggsave(paste0(getwd(), "/figures/lake_richness.png"), lake_fig, units = "mm", width = 129, height = 200)
ggsave(paste0(getwd(), "/figures/lake_richness.svg"), lake_fig, units = "mm", width = 129, height = 200)
