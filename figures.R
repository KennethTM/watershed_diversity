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
  coord_cartesian(ylim=c(0, 155))+
  annotate("segment", x = 0, xend = 2, y = 155, yend = 160, colour = "black")+
  annotate("text", x = 4, y = 160, label = "831", size=3)+
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
         lake_age_bins = cut(lake_age, 
                             breaks = c(-1, 10, 20, 200, 10000), 
                             labels = c("0-10", "10-20", ">20", "Unknown")),
         lake_cat = factor(ifelse(lake_age_bins == "Unknown", "Natural", "New"), levels = c("New", "Natural"))) %>% 
  select(gml_id, n_spec_lake, lake_cat, lake_age_bins, lake_age) %>% 
  left_join(lake_coords) %>% 
  st_as_sf() %>% 
  st_crop(st_bbox(dk_border))

lake_map <- figure_2_data %>% 
  ggplot() +
  geom_sf(data = dk_border, col = "grey", fill = NA)+
  geom_sf(aes(col = lake_age_bins, shape = lake_cat), size = 0.7)+
  geom_sf(data = dk_iceage_cut, aes(linetype = "Ice age"), col = "coral", linetype = 1, show.legend = FALSE)+
  scale_colour_manual(values = c(viridisLite::viridis(3, direction = -1, begin = 0.3), "black"), name = "Lake age (years)")+
  scale_shape_manual(values = c("Natural" = 1, "New" = 19), name = "Lake group")+
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


#Spec vs age plot
spec_vs_age_data <- figure_2_data %>% 
  filter(lake_cat=="New" & lake_age <= 30)
spec_vs_age_glm <- glm(n_spec_lake ~ lake_age, family = "poisson", data = spec_vs_age_data)
summary(spec_vs_age_glm)

ilink <- family(spec_vs_age_glm)$linkinv

glm_pred_df <- data.frame(lake_age = 0:30) %>% 
  mutate(fit_link = predict(spec_vs_age_glm, newdata= ., se.fit=TRUE)$fit, 
         se_link = predict(spec_vs_age_glm, newdata= ., se.fit=TRUE)$se.fit) %>% 
  mutate(fit_resp  = ilink(fit_link),
         right_upr = ilink(fit_link + (2 * se_link)),
         right_lwr = ilink(fit_link - (2 * se_link)))

spec_vs_age <- figure_2_data %>% 
  filter(lake_cat=="New") %>% 
  st_drop_geometry() %>% 
  ggplot(aes(lake_age, n_spec_lake))+
  geom_ribbon(data = glm_pred_df, inherit.aes = FALSE, aes(x=lake_age, ymin=right_lwr, ymax=right_upr), fill=grey(0.7))+
  geom_line(data = glm_pred_df, aes(y=fit_resp), size=1)+
  geom_point()+
  ylab("Species richness")+
  xlab("Lake age (years)")

figure_2 <- lake_map + lake_freq + spec_vs_age + plot_layout(ncol = 1) + plot_annotation(tag_levels = "A")

figure_2

ggsave(paste0(getwd(), "/figures/figure_2.png"), figure_2, units = "mm", width = 129, height = 234)

#Figure 3 PSEM MODEL

#Figure 4 watershed/lake species

#Figure 5 PCOA
library(vegan);library(viridisLite)
fish_species_wide <- fish_species_lakes %>% 
  st_drop_geometry() %>% 
  left_join(model_data_raw[, c("gml_id", "year_established")]) %>% 
  mutate(lake_age = ifelse(is.na(year_established), 9999, year_sample - year_established),
         lake_age_bins = cut(lake_age, 
                             breaks = c(-1, 10, 20, 200, 10000), 
                             labels = c("0-10", "10-20", ">20", "Unknown")),
         lake_group = factor(ifelse(lake_age_bins == "Unknown", "Natural", "New"), levels = c("New", "Natural"))) %>% 
  select(gml_id, lake_group, lake_age_bins, fish_id) %>% 
  na.omit() %>% #remove no catch lakes
  mutate(presence = 1) %>% 
  spread(fish_id, presence)
  
spec_matrix <- fish_species_wide %>% 
  select(-gml_id, -lake_group, -lake_age_bins) %>% 
  as.matrix()
spec_matrix[is.na(spec_matrix)] <- 0

spec_bray <- vegdist(spec_matrix, method="bray", binary = TRUE)
spec_pcoa <- cmdscale(spec_bray, k=(nrow(spec_matrix)-1), eig=TRUE)

pcoa_data <- data.frame(dim1 = spec_pcoa$points[,1], 
                        dim2 = spec_pcoa$points[,2],
                        lake_age_bins = fish_species_wide$lake_age_bins) %>% 
  mutate(lake_cat = factor(ifelse(lake_age_bins == "Unknown", "Natural", "New"), levels = c("New", "Natural")))

pcoa_hull <- pcoa_data %>%
  group_by(lake_cat) %>% 
  slice(chull(dim1, dim2))

figure_5 <- pcoa_data %>% 
  ggplot(aes(dim1, dim2, col=lake_age_bins, shape = lake_cat)) +
  geom_polygon(data = pcoa_hull, fill=NA, col="black", aes(linetype=lake_cat))+
  geom_point()+
  scale_linetype_manual(values=c("Natural"=1, "New"= 3), name = "Lake group")+
  scale_colour_manual(values = c(viridisLite::viridis(3, direction = -1, begin = 0.3), "black"), name = "Lake age (years)")+
  scale_shape_manual(values = c("Natural" = 1, "New" = 19), name = "Lake group")+
  guides(linetype = guide_legend(order = 3), colour = guide_legend(order = 1), shape = guide_legend(order = 2))+
  xlab("1st PCoA dimension")+
  ylab("2nd PCoA dimension")

ggsave(paste0(getwd(), "/figures/figure_5.png"), figure_5, units = "mm", width = 129, height = 100)
