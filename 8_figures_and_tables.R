source("0_libs_and_funcs.R")

#Load common spatial data
dk_border <- st_read(dsn = gis_database, layer = "dk_border") 
dk_iceage <- st_read(dsn = gis_database, layer = "dk_iceage") 

dk_iceage_cut <- dk_iceage %>% 
  st_cast("LINESTRING") %>% 
  st_intersection(dk_border) %>% 
  st_collection_extract("LINESTRING")

model_data_psem <- read_csv(paste0(getwd(), "/data_processed/model_data_psem.csv")) %>% 
  mutate(lake_age_bins = factor(lake_age_bins, levels = c("0-10", "10-20", ">20", "Unknown")))

fish_species_lakes <- st_read(dsn = gis_database, layer = "fish_species_lakes")

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
  coord_cartesian(ylim=c(0, 155))+
  annotate("segment", x = 0, xend = 2, y = 155, yend = 160, colour = "black")+
  annotate("text", x = 4, y = 160, label = "829", size=3)+
  ylab("Frequency")+
  xlab("Basin richness")

#xlabs <- seq(8, 12, 1)
#ylabs <- seq(54.5, 57.5, 0.5)

basin_richness <- basins_count %>% 
  ggplot()+
  geom_sf(data = dk_border, fill = NA, col = "black")+
  geom_sf(aes(fill = n_spec_basin), col = "black", size = 0.20)+
  geom_sf(data = dk_iceage_cut, aes(linetype = "Ice age"), col = "coral", linetype = 1, show.legend = FALSE)+
  scale_fill_viridis_c(na.value="white", option = "D", name = "Species richness", direction = -1, begin = 0.2)+
  #scale_x_continuous(breaks = xlabs, labels = paste0(xlabs,'°E')) +
  #scale_y_continuous(breaks = ylabs, labels = paste0(ylabs,'°N'))+
  theme(legend.position = "bottom")+
  guides(fill=guide_colorbar(title.position = "top", barwidth = 8, title.hjust = 0.5))

figure_1 <- basin_richness + basin_freq + plot_layout(ncol = 1, heights = c(1.5, 1), widths=1) + plot_annotation(tag_levels = "A")

figure_1

ggsave(paste0(getwd(), "/figures/figure_1.png"), figure_1, units = "mm", width = 84, height = 160)

#Figure 2
#Lake richness plot
lake_coords <- fish_species_lakes %>% 
  select(gml_id) %>% 
  distinct()

figure_2_data <- model_data_psem %>% 
  mutate(lake_cat = factor(ifelse(lake_natural == 1, "Natural", "New"), levels = c("New", "Natural"))) %>% 
  select(gml_id, n_spec_lake, lake_cat, lake_age_bins, lake_age, lake_stream_connect) %>% 
  left_join(lake_coords) %>% 
  st_as_sf()

lake_map <- figure_2_data %>% 
  ggplot() +
  geom_sf(data = dk_border, col = "grey", fill = NA)+
  geom_sf(aes(col = lake_age_bins, shape = lake_cat), size = 0.7)+
  geom_sf(data = dk_iceage_cut, aes(linetype = "Ice age"), col = "coral", linetype = 1, show.legend = FALSE)+
  scale_colour_manual(values = c(viridisLite::viridis(3, direction = -1, begin = 0.3), "black"), name = "Lake age (years)")+
  scale_shape_manual(values = c("Natural" = 1, "New" = 19), name = "Lake group")+
  guides(linetype = guide_legend(title = NULL, order = 2), colour = guide_legend(order = 1))

lake_freq <- figure_2_data %>% 
  st_drop_geometry() %>% 
  ggplot(aes(x=n_spec_lake))+
  geom_histogram(fill = "grey", col = "white", binwidth = 1)+
  geom_density(aes(y=..count.., col = lake_cat), position = position_stack())+
  scale_color_manual(values = c(viridisLite::viridis(4, direction = -1, begin = 0.3)[2], "black"), name = "Lake group")+
  ylab("Frequency")+
  xlab("Species richness")+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

#Spec vs age plot
spec_vs_age_data <- figure_2_data %>% 
  st_drop_geometry() %>% 
  filter(lake_cat=="New")

spec_vs_age_glm <- glm(n_spec_lake ~ lake_age * lake_stream_connect, family = "poisson", data = spec_vs_age_data)
summary(spec_vs_age_glm)

# ilink <- family(spec_vs_age_glm)$linkinv
# 
# glm_pred_df <- data.frame(lake_age = 0:30) %>% 
#   mutate(fit_link = predict(spec_vs_age_glm, newdata= ., se.fit=TRUE)$fit, 
#          se_link = predict(spec_vs_age_glm, newdata= ., se.fit=TRUE)$se.fit) %>% 
#   mutate(fit_resp  = ilink(fit_link),
#          right_upr = ilink(fit_link + (2 * se_link)),
#          right_lwr = ilink(fit_link - (2 * se_link)))

spec_vs_age <- figure_2_data %>% 
  filter(lake_cat=="New") %>% 
  st_drop_geometry() %>% 
  mutate(`Stream network` = ifelse(lake_stream_connect == 1, "Connected", "Not connected")) %>% 
  ggplot(aes(lake_age, n_spec_lake, col=`Stream network`))+
  #geom_ribbon(data = glm_pred_df, inherit.aes = FALSE, aes(x=lake_age, ymin=right_lwr, ymax=right_upr), fill=grey(0.7))+
  #geom_line(data = glm_pred_df, aes(y=fit_resp), size=1)+
  geom_point()+
  scale_color_manual(values = c("Connected" = "dodgerblue", "Not connected" ="grey"))+
  ylab("Species richness")+
  xlab("Lake age (years)")

figure_2 <- lake_map + lake_freq + spec_vs_age + plot_layout(ncol = 1, heights = c(1.4, 1, 1), widths = 1) + plot_annotation(tag_levels = "A")

figure_2

ggsave(paste0(getwd(), "/figures/figure_2.png"), figure_2, units = "mm", width = 129, height = 234)

#Figure 3 PSEM MODEL

#Figure 4 PCOA
fish_species_wide <- fish_species_lakes %>%
  st_drop_geometry() %>% 
  as_tibble() %>% 
  right_join(model_data_psem) %>% 
  select(gml_id, lake_natural, lake_age_bins, fish_id, lake_stream_connect) %>% 
  na.omit() %>% #remove no catch lakes
  mutate(presence = 1) %>% 
  spread(fish_id, presence)
  
spec_matrix <- fish_species_wide %>% 
  select(-gml_id, -lake_natural, -lake_age_bins, -lake_stream_connect) %>% 
  as.matrix()
spec_matrix[is.na(spec_matrix)] <- 0

spec_bray <- vegdist(spec_matrix, method="bray", binary = TRUE)
spec_pcoa <- cmdscale(spec_bray, k=(nrow(spec_matrix)-1), eig=TRUE)

pcoa_data <- data.frame(dim1 = spec_pcoa$points[,1], 
                        dim2 = spec_pcoa$points[,2],
                        lake_age_bins = fish_species_wide$lake_age_bins,
                        lake_stream_connect = fish_species_wide$lake_stream_connect) %>% 
  mutate(lake_cat = factor(ifelse(lake_age_bins == "Unknown", "Natural", "New"), levels = c("New", "Natural")))

pcoa_hull <- pcoa_data %>%
  group_by(lake_cat) %>% 
  slice(chull(dim1, dim2))

figure_4 <- pcoa_data %>% 
  ggplot(aes(dim1, dim2, col=lake_age_bins, shape = lake_cat)) +
  geom_polygon(data = pcoa_hull, fill=NA, col="black", aes(linetype=lake_cat))+
  geom_point()+
  scale_linetype_manual(values=c("Natural"=1, "New"= 3), name = "Lake group")+
  scale_colour_manual(values = c(viridisLite::viridis(3, direction = -1, begin = 0.3), "black"), name = "Lake age (years)")+
  scale_shape_manual(values = c("Natural" = 1, "New" = 19), name = "Lake group")+
  guides(linetype = guide_legend(order = 3), colour = guide_legend(order = 1), shape = guide_legend(order = 2))+
  xlab("1st PCoA dimension")+
  ylab("2nd PCoA dimension")

figure_4

ggsave(paste0(getwd(), "/figures/figure_4.png"), figure_4, units = "mm", width = 129, height = 100)



#Figure 5 watershed/lake species
#Load and add to basin and lake species lists for species specific analysis
#Only include lake and basins used in modeling
# fish_unique_edit <- read_xlsx(paste0(getwd(), "/data_raw/", "fish_unique_edit_EK.xlsx")) %>% 
#   select(name = name_to_use, name_novana = name_local_novana, name_atlas = latin_and_atlas,
#          fish_id = ID, action = `how(0=do_nothing)(1=remove_species)(2=remove_lake)`) %>% 
#   filter(action == 0) %>% 
#   select(name_atlas, fish_id) %>% 
#   distinct() %>% 
#   slice(-25)
# 
# bas <- read_csv(paste0(getwd(), "/data_raw/basin_species_list.csv"))
# 
# lak <- read_csv(paste0(getwd(), "/data_raw/lake_species_list.csv")) 
# 
# lak_sub <- bind_rows(add_column(natural_lakes, system = "natural"), 
#                      add_column(new_lakes, system = "new")) %>% 
#   mutate(basin_id = as.numeric(as.character(basin_id_fact))) %>% 
#   select(system, basin_id, site_id) %>% 
#   left_join(select(lak, site_id, fish_id)) %>% 
#   distinct()
# 
# bas_sub <- bas %>%
#   select(basin_id, fish_id) %>% 
#   filter(basin_id %in% lak_sub$basin_id) %>% 
#   distinct()
# 
# lak_per_bas <- lak_sub %>% 
#   select(system, basin_id, site_id) %>% 
#   distinct() %>% 
#   group_by(system, basin_id) %>% 
#   summarise(n_lake = n())
# 
# lak_per_spec <- lak_sub %>%
#   group_by(system, fish_id) %>%
#   summarise(n_lake_spec = n())
# 
# bas_per_spec <- bas_sub %>%
#   group_by(fish_id) %>%
#   summarise(n_bas_spec = n())
# 
# new_lake_freq <- left_join(bas_sub, lak_sub) %>%
#   filter(system == "new" | is.na(system)) %>% 
#   group_by(fish_id, basin_id) %>% 
#   summarise(n_occur = sum(!is.na(site_id))) %>%
#   left_join(filter(lak_per_bas, system == "new")) %>%
#   na.omit() %>% 
#   summarise(spec_mean = mean(n_occur/n_lake)) %>% 
#   left_join(fish_unique_edit) 
# 
# nat_lake_freq <- left_join(bas_sub, lak_sub) %>%
#   filter(system == "natural" | is.na(system)) %>% 
#   group_by(fish_id, basin_id) %>% 
#   summarise(n_occur = sum(!is.na(site_id))) %>%
#   left_join(filter(lak_per_bas, system == "natural")) %>%
#   na.omit() %>% 
#   summarise(spec_mean = mean(n_occur/n_lake)) %>% 
#   left_join(fish_unique_edit) 
# 
# spec_freq_plot <- bind_rows(add_column(nat_lake_freq, system = "natural"), 
#                             add_column(new_lake_freq, system = "new")) %>% 
#   left_join(lak_per_spec) %>% 
#   left_join(bas_per_spec) %>%
#   na.omit() %>% 
#   mutate(label = gsub("_", " ", name_atlas),
#          lake_cat = ifelse(system == "natural", "Natural", "New"),
#          lake_cat_fact = factor(ifelse(lake_cat == "Natural", "Natural", "New"), levels = c("New", "Natural")),
#          label_n_bas = paste0(label, " (", n_bas_spec, ")"),
#          n_lakes_sys = ifelse(lake_cat == "New", nrow(new_lakes), nrow(natural_lakes)),
#          n_lake_spec_prop = n_lake_spec/n_lakes_sys*100) %>% 
#   #ggplot(aes(x = reorder(label_n_bas, spec_mean), y = spec_mean, col = lake_cat_fact, size = n_lake_spec))+
#   ggplot(aes(x = reorder(label_n_bas, spec_mean), y = spec_mean, col = lake_cat_fact, size = n_lake_spec_prop))+
#   geom_point()+
#   scale_colour_manual(values = c(viridisLite::viridis(1, begin = 0.5, end = 0.6), "coral"), name = "Lake age (years)")+
#   scale_size(name = "Occurrences (%)", breaks = seq(0, 100, 25))+
#   scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1))+
#   coord_flip()+
#   xlab("Species")+
#   ylab("Average frequency")+
#   theme(panel.grid.major.y = element_line(size = 0.5, colour = "grey"),
#         legend.position = c(0.75, 0.24),
#         axis.text.y = element_text(face = "italic"),
#         legend.background = element_rect(colour = "black", size = 0.25))
# 
# ggsave(paste0(getwd(), "/figures/lake_spec_freq.png"), spec_freq_plot, units = "mm", width = 129, height = 150)
# ggsave(paste0(getwd(), "/figures/lake_spec_freq.svg"), spec_freq_plot, units = "mm", width = 129, height = 150)

#Table 1
#Species richness and lake drivers in natural and new danish lakes

#Supplementary material

#Table S1
#Fish species included in the analysis
fish_species_unique_edit <- read_xlsx(paste0(getwd(), "/data_raw/fish_species_unique.xlsx"))
lake_fish_ids <- read_csv(paste0(getwd(), "/data_processed/lake_fish_ids.csv"))

#Table S2
#Drainage basin predictor variables
