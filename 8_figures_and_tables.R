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

model_data_proc <- read_csv(paste0(getwd(), "/data_processed/model_data_proc.csv")) 

fish_species_lakes <- st_read(dsn = gis_database, layer = "fish_species_lakes")

lake_fish_ids <- read_csv(paste0(getwd(), "/data_processed/lake_fish_ids.csv"))
fish_species_unique_edit <- read_xlsx(paste0(getwd(), "/data_raw/fish_species_unique.xlsx"))

#Supplementary material

#Table S1
#Fish species included in the analysis
#27 species
table_s1_species <- fish_species_unique_edit %>% 
  filter(fish_id %in% lake_fish_ids$fish_id) %>% 
  select(name_atlas, fish_id) %>% 
  distinct() %>% 
  filter(name_atlas != "Gynmocephalus_cernua") %>% 
  mutate(name_atlas = gsub("_", " ", name_atlas)) 

table_s1_species %>% 
  select(-fish_id) %>% 
  write_csv(paste0(getwd(), "/figures/table_s1.csv"))

#Tables in manuscript

#Table 1
#Drainage basin species richness and variables
#98 basins
table_1 <- model_data_proc %>% 
  filter(gml_id %in% model_data_psem$gml_id) %>%  
  select(basin_lake_area_m2, basin_stream_length_m, basin_arti, basin_agri, basin_elev_mean_m,
         basin_slope_prc, basin_area_m2, basin_circum_m, salinity, basin_elev_range_m, ice_covered, n_spec_basin) %>% 
  distinct() %>% 
  mutate(basin_lake_area_m2 = basin_lake_area_m2*10^-4,
         basin_area_m2 = basin_area_m2*10^-4,
         basin_stream_length_m = basin_stream_length_m*10^-3,
         basin_circum_m = basin_circum_m*10^-3,
         basin_arti = basin_arti*100,
         basin_agri = basin_agri*100) %>% 
  gather(variable, value) %>% 
  group_by(variable) %>% 
  summarise(Minimum=min(value), 
            #`1st quantile` = quantile(value, 0.25), 
            #Median=median(value), 
            Mean=mean(value), 
            #`3rd quantile` = quantile(value, 0.75), 
            Maximum=max(value)) %>% 
  mutate(Note = case_when(variable == "n_spec_basin" ~ "Response",
                          variable %in% c("basin_area_m2", "basin_circum_m", "basin_elev_range_m", "basin_stream_length_m") ~ "Discarded",
                          TRUE ~ "Predictor")) %>% 
  mutate(Minimum = signif(Minimum, 2),
         Mean = signif(Mean, 2),
         Maximum = signif(Maximum, 2)) %>% 
  rename(Variable = variable) %>% 
  arrange(desc(Note)) %>% 
  relocate(Note, .after = Variable)

table_1

write_csv(table_1, paste0(getwd(), "/figures/table_1.csv"))

#Table 2
#Species richness and lake drivers in natural and new danish lakes
#193 natural + 34 new
table_2 <- model_data_proc %>% 
  filter(gml_id %in% model_data_psem$gml_id) %>%
  mutate(lake_cat = ifelse(lake_natural == 1, "Natural", "New")) %>% 
  select(lake_elev_m, shoreline_m, bathy_area, bathy_vol, bathy_zmean, bathy_zmax,
         alk_mmol_l, chla_ug_l, tn_mg_l, ph_ph, tp_mg_l, secchi_depth_m,
         n_spec_lake, lake_stream_connect, lake_cat) %>% 
  mutate(bathy_vol = bathy_vol*10^-3,
         bathy_area = bathy_area*10^-4,
         shoreline_m = shoreline_m*10^-3) %>% 
  gather(variable, value, -lake_cat) %>% 
  group_by(variable, lake_cat) %>% 
  summarise(Minimum=min(value), 
            #`1st quantile` = quantile(value, 0.25), 
            #Median=median(value), 
            Mean=mean(value), 
            #`3rd quantile` = quantile(value, 0.75), 
            Maximum=max(value)) %>% 
  ungroup() %>% 
  gather(stat, value, Minimum:Maximum) %>% 
  mutate(lake_cat_stat = paste0(lake_cat, "-", stat),
         value_round = signif(value, 2)) %>% 
  select(variable, lake_cat_stat, value_round) %>% 
  spread(lake_cat_stat, value_round) %>% 
  mutate(Note = case_when(variable == "n_spec_lake" ~ "Response",
                          variable %in% c("bathy_zmean", "bathy_vol", "shoreline_m", "chla_ug_l") ~ "Discarded",
                          TRUE ~ "Predictor")) %>% 
  rename(Variable = variable) %>% 
  arrange(desc(Note)) %>% 
  relocate(Note, .after = Variable)

write_csv(table_2, paste0(getwd(), "/figures/table_2.csv"))

#Figures in manuscript

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
  geom_density(aes(y=..count.., linetype = lake_cat), position = position_stack())+
  scale_linetype_manual(values=c("Natural"=1, "New"= 3), name = "Lake group")+
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

ggsave(paste0(getwd(), "/figures/figure_2.png"), figure_2, units = "mm", width = 129, height = 200)

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

#Figure 5 basin-lake species
#Load and add to basin and lake species lists for species specific analysis
lake_species <- read_csv(paste0(getwd(), "/data_processed/lake_species.csv")) %>% 
  filter(is.na(fish_id) | fish_id %in% lake_fish_ids$fish_id & gml_id %in% model_data_psem$gml_id) %>% 
  left_join(model_data_psem[,c("gml_id", "lake_natural")]) %>% 
  mutate(lake_cat = ifelse(lake_natural == 1, "Natural", "New")) %>% 
  select(-lake_natural)

basin_species <- read_csv(paste0(getwd(), "/data_processed/basin_species.csv")) %>% 
  filter(fish_id %in% lake_fish_ids$fish_id & basin_id %in% model_data_psem$basin_id) %>% 
  left_join(model_data_psem[,c("basin_id", "lake_natural")]) %>% 
  mutate(lake_cat = ifelse(lake_natural == 1, "Natural", "New")) %>% 
  select(-lake_natural) %>% 
  distinct()

lake_per_basin <- lake_species %>% 
  select(lake_cat, basin_id, gml_id) %>% 
  distinct() %>% 
  group_by(basin_id, lake_cat) %>% 
  summarise(n_lake = n())

basin_per_spec <- basin_species %>% 
  select(basin_id, fish_id) %>% 
  distinct() %>% 
  group_by(fish_id) %>% 
  summarise(n_basin_spec = n())

figure_5_data <- left_join(basin_species, lake_species) %>% 
  group_by(fish_id, lake_cat, basin_id) %>% 
  summarise(n_occur = sum(!is.na(gml_id))) %>% 
  left_join(lake_per_basin) %>% 
  summarise(spec_mean = mean(n_occur/n_lake)) %>% 
  filter(spec_mean > 0) %>% 
  left_join(basin_per_spec) %>% 
  left_join(table_s1_species) %>% 
  mutate(name_label = paste0(name_atlas, " (", n_basin_spec, ")"))

figure_5 <- figure_5_data %>% 
  ggplot(aes(x = reorder(name_label, spec_mean), y = spec_mean, shape = lake_cat))+
  geom_point()+
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1))+
  coord_flip()+
  scale_shape_manual(values = c("Natural" = 1, "New" = 19), name = "Lake group")+
  xlab("Species")+
  ylab("Average frequency")+
  theme(panel.grid.major.y = element_line(size = 0.5, colour = "grey"),
        legend.position = c(0.7, 0.15),
        axis.text.y = element_text(face = "italic"),
        legend.background = element_rect(colour = "black", size = 0.25))

figure_5

ggsave(paste0(getwd(), "/figures/figure_5.png"), figure_5, units = "mm", width = 129, height = 150)
