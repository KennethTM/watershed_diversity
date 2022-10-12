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

#Mann Whitney U-test
wilcox.test(model_data_psem$n_spec_lake ~ model_data_psem$lake_age_bins == "Unknown")

#Kruskal Wallis test
kw_data <- model_data_psem |> 
  mutate(connection = ifelse(lake_stream_connect == 0, "disconnected", "connected"),
         natural = ifelse(lake_natural == 1, "natural", "new"),
         groups = paste0(connection, "_", natural)) |> 
  select(n_spec_lake, groups)

kw_test <- kruskal.test(n_spec_lake ~ groups, data = kw_data)

dunnTest(n_spec_lake ~ groups, data = kw_data, method="bonferroni")

#Mean age
mean(model_data_psem[model_data_psem$lake_age_bins != "Unknown", ]$lake_age)

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
  mutate(basin_lake_area_m2 = basin_lake_area_m2*10^-6,
         basin_area_m2 = basin_area_m2*10^-6,
         basin_stream_length_m = basin_stream_length_m*10^-3,
         basin_circum_m = basin_circum_m*10^-3,
         basin_arti = basin_arti*100,
         basin_agri = basin_agri*100) %>% 
  gather(variable, value) %>% 
  group_by(variable) %>% 
  summarise(Minimum=min(value), 
            Mean=mean(value), 
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
  mutate(bathy_vol = bathy_vol*10^-6,
         bathy_area = bathy_area*10^-6,
         shoreline_m = shoreline_m*10^-3) %>% 
  gather(variable, value, -lake_cat) %>% 
  group_by(variable, lake_cat) %>% 
  summarise(Minimum=min(value), 
            Mean=mean(value), 
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
  st_crop(st_bbox(dk_border)) |> 
  st_cast("MULTIPOLYGON") |> 
  ms_simplify(keep=0.02)

#Stats for drainage basin species richness
table(basins_count$n_spec_basin)
summary(basins_count$n_spec_basin)
summary(basins_count$n_spec_basin[basins_count$n_spec_basin > 0])

basin_freq <- basins_count %>% 
  ggplot(aes(n_spec_basin))+
  geom_histogram(col = "black", fill = "white", binwidth = 1)+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  coord_cartesian(ylim=c(0, 155))+
  annotate("segment", x = 0, xend = 2, y = 155, yend = 160, colour = "black")+
  annotate("text", x = 4, y = 160, label = "829", size=3)+
  ylab("Frequency")+
  xlab("Basin richness")

basin_richness <- basins_count %>% 
  ggplot()+
  geom_sf(data = dk_border, fill = NA, col = "black")+
  geom_sf(aes(fill = n_spec_basin), col = "black", size = 0.1)+
  geom_sf(data = dk_iceage_cut, aes(linetype = "Ice age"), col = "coral", linetype = 1, show.legend = FALSE)+
  scale_fill_viridis_c(na.value="white", option = "D", name = "Species richness", direction = -1, begin = 0.2)+
  theme(legend.position = "bottom")+
  guides(fill=guide_colorbar(title.position = "top", barwidth = 8, title.hjust = 0.5))

figure_1 <- basin_richness + basin_freq + plot_layout(ncol = 1, heights = c(1.5, 1), widths=1) + plot_annotation(tag_levels = "A")

figure_1

ggsave(paste0(getwd(), "/figures/figure_1.png"), figure_1, units = "mm", width = 84, height = 160)
ggsave(paste0(getwd(), "/figures/figure_1.pdf"), figure_1, units = "mm", width = 84, height = 160)

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
  geom_sf(aes(col = lake_cat), size = 0.7)+
  geom_sf(data = dk_iceage_cut, aes(linetype = "Ice age"), col = "coral", linetype = 1, show.legend = FALSE)+
  scale_colour_manual(values = c("Natural" = "black", "New" = "dodgerblue"), name = "Lake group")

groups_boxplot <- model_data_psem |> 
  mutate(connection = ifelse(lake_stream_connect == 0, "Not connected", "Connected"),
         natural = ifelse(lake_natural == 1, "Natural", "New"),
         groups = paste0(natural, "\n", connection),
         signif_groups = case_when(groups == "Natural\nConnected" ~ "A",
                                   groups == "Natural\nNot connected" ~ "B",
                                   groups == "New\nConnected" ~ "A",
                                   groups == "New\nNot connected" ~ "B")) |> 
  select(n_spec_lake, groups, signif_groups) |> 
  group_by(groups) |> 
  add_tally() |> 
  mutate(tally_label = paste0("n=", n),
         groups_short = gsub("Connected", "Conn.", groups),
         groups_short = gsub("connected", "conn.", groups_short)) |> 
  ggplot(aes(groups_short, n_spec_lake))+
  geom_boxplot(coef=2)+
  geom_text(aes(y=17, label = tally_label), check_overlap=TRUE)+
  geom_text(aes(y=15, label = signif_groups), check_overlap=TRUE)+
  ylab("Species richness")+
  xlab("Lake group")

#Spec vs age plot
spec_vs_age_data <- figure_2_data %>% 
  st_drop_geometry() %>% 
  filter(lake_cat=="New")

spec_vs_age_glm <- glm(n_spec_lake ~ lake_age * lake_stream_connect, family = "poisson", data = spec_vs_age_data)
summary(spec_vs_age_glm)

spec_vs_age <- figure_2_data %>% 
  filter(lake_cat=="New") %>% 
  st_drop_geometry() %>% 
  mutate(`Stream network` = ifelse(lake_stream_connect == 1, "Connected", "Not connected")) %>% 
  ggplot(aes(lake_age, n_spec_lake, shape=`Stream network`))+
  geom_point()+
  scale_shape_manual(values = c("Connected" = 19, "Not connected" = 1))+
  ylab("Species richness")+
  xlab("Lake age (years)")

figure_2 <- lake_map + groups_boxplot + spec_vs_age + plot_layout(ncol = 1, heights = c(1.4, 1, 1), widths = 1) + plot_annotation(tag_levels = "A")

figure_2

ggsave(paste0(getwd(), "/figures/figure_2.png"), figure_2, units = "mm", width = 129, height = 200)
ggsave(paste0(getwd(), "/figures/figure_2.pdf"), figure_2, units = "mm", width = 129, height = 200)

#Figure 3 PSEM MODEL

#Figure 4 NMDS
fish_species_wide <- fish_species_lakes %>%
  st_drop_geometry() %>% 
  as_tibble() %>% 
  right_join(model_data_psem) %>% 
  select(gml_id, lake_natural, lake_age_bins, fish_id, lake_stream_connect) %>% 
  na.omit() %>% #remove no catch lakes
  mutate(presence = 1) %>% 
  spread(fish_id, presence) %>% 
  slice(-197) |> #remove outlier that affects nmds analysis and visualization
  mutate(lake_cat = factor(ifelse(lake_age_bins == "Unknown", "Natural", "New"), levels = c("New", "Natural")),
         stream_network = ifelse(lake_stream_connect == 1, "Connected", "Not connected"),
         groups = factor(paste(lake_cat, stream_network)))
  
spec_matrix <- fish_species_wide %>% 
  select(-gml_id, -lake_natural, -lake_age_bins, -lake_stream_connect, 
         -lake_cat, -stream_network, -groups) %>% 
  as.matrix()
spec_matrix[is.na(spec_matrix)] <- 0

spec_bray <- vegdist(spec_matrix, method="bray")

perm_test <- pairwise.adonis(spec_bray, fish_species_wide$groups, p.adjust.m = "holm")
perm_test

spec_nmds <- metaMDS(spec_bray, distance = "bray", k = 3, maxit = 1000, trymax = 500, wascores = TRUE, noshare=TRUE)

nmds_data <- cbind(dim1 = spec_nmds$points[, 1], 
                   dim2 = spec_nmds$points[, 2],
                   fish_species_wide)

group_colors <- RColorBrewer::brewer.pal(10, "Paired")[c(4, 3, 2, 1)]

figure_4 <- nmds_data %>% 
  mutate(`Lake group` = gsub("connected", "conn.", str_to_sentence(groups)),
         `Lake group` = factor(`Lake group`, levels = c("Natural conn.", "Natural not conn.", "New conn.", "New not conn."))) |> 
  ggplot(aes(dim1, dim2, col = `Lake group`)) +
  geom_point()+
  stat_ellipse()+
  scale_color_manual(values=c("Natural conn." = group_colors[1],
                              "Natural not conn." = group_colors[2],
                              "New conn." = group_colors[3],
                              "New not conn." = group_colors[4]))+
  guides(linetype = guide_legend(order = 3), colour = guide_legend(order = 1), shape = guide_legend(order = 2))+
  xlab("NMDS1")+
  ylab("NMDS2")

figure_4

ggsave(paste0(getwd(), "/figures/figure_4.png"), figure_4, units = "mm", width = 129, height = 100)
ggsave(paste0(getwd(), "/figures/figure_5.png"), figure_4, units = "mm", width = 129, height = 100)

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
  ggplot(aes(x = reorder(name_label, spec_mean), y = spec_mean, col = lake_cat))+
  geom_point()+
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1))+
  coord_flip()+
  scale_color_manual(values = c("Natural" = "black", "New" = "dodgerblue"), name = "Lake group")+
  xlab("Species")+
  ylab("Average frequency")+
  theme(panel.grid.major.y = element_line(size = 0.5, colour = "grey"),
        legend.position = c(0.7, 0.15),
        axis.text.y = element_text(face = "italic"),
        legend.background = element_rect(colour = "black", size = 0.25))

figure_5

ggsave(paste0(getwd(), "/figures/figure_5.png"), figure_5, units = "mm", width = 129, height = 150)
ggsave(paste0(getwd(), "/figures/figure_5.pdf"), figure_5, units = "mm", width = 129, height = 150)
