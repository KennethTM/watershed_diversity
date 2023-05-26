source("0_libs_and_funcs.R")

#Figurer til Vand & Jord

#Figure 3
dk_border <- st_read(dsn = gis_database, layer = "dk_border")
dk_iceage <- st_read(dsn = gis_database, layer = "dk_iceage")
basins <- st_read(gis_database, layer = "basins")
basin_species_count <- read_csv(paste0(getwd(), "/data_processed/basin_species_count.csv"))

dk_iceage_cut <- dk_iceage %>% 
    st_cast("LINESTRING") %>%
    st_intersection(dk_border) %>%
    st_collection_extract("LINESTRING")

basins_count <- basins %>% 
  left_join(basin_species_count) %>% 
  mutate(n_spec_basin = ifelse(is.na(n_spec_basin), 0, n_spec_basin)) %>% 
  st_crop(st_bbox(dk_border)) |> 
  st_cast("MULTIPOLYGON") |> 
  ms_simplify(keep=0.02)

figur_3 <- basins_count %>% 
  ggplot()+
  geom_sf(data = dk_border, fill = NA, col = "black")+
  geom_sf(aes(fill = n_spec_basin), col = "black", size = 0.1)+
  geom_sf(data = dk_iceage_cut, aes(linetype = "Ice age"), col = "red", linetype = 1, show.legend = FALSE)+
  scale_fill_viridis_c(na.value="white", option = "D", name = "Antal arter", direction = -1, begin = 0.2)+
  theme(legend.position = "bottom")+
  guides(fill=guide_colorbar(ticks = FALSE, title.position = "top", barwidth = 8, title.hjust = 0.5))

figur_3

ggsave(paste0(getwd(), "/figures_danish/figur_2.png"), figur_3, units = "mm", width = 84, height = 100)
ggsave(paste0(getwd(), "/figures_danish/figur_2.pdf"), figur_3, units = "mm", width = 84, height = 100)

#Figure 4
model_data_psem <- read_csv(paste0(getwd(), "/data_processed/model_data_psem.csv"))

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

figur_4_a <- model_data_psem |>
  mutate(lake_cat = factor(ifelse(lake_natural == 1, "Naturlig", "Ny"), levels = c("Ny", "Naturlig")),
         connection = ifelse(lake_stream_connect == 0, "- vandløb", "+ vandløb"),
         groups = paste0(lake_cat, "\n", connection),
         signif_groups = case_when(groups == "Naturlig\n+ vandløb" ~ "A",
                                   groups == "Naturlig\n- vandløb" ~ "B",
                                   groups == "Ny\n+ vandløb" ~ "A",
                                   groups == "Ny\n- vandløb" ~ "B")) |>
  select(n_spec_lake, groups, signif_groups) |>
  group_by(groups) |>
  add_tally() |>
  mutate(tally_label = paste0("n=", n),
         groups = factor(groups, levels=c("Naturlig\n+ vandløb", "Naturlig\n- vandløb", "Ny\n+ vandløb", "Ny\n- vandløb"))) |>
  ggplot(aes(groups, n_spec_lake))+
  geom_boxplot(coef=2)+
  geom_text(aes(y=17, label = tally_label), check_overlap=TRUE)+
  geom_text(aes(y=15, label = signif_groups), check_overlap=TRUE)+
  ylab("Antal arter")+
  xlab(NULL)

#Spec vs age plot
spec_vs_age_data <- model_data_psem |> 
  filter(lake_natural == 0)

spec_vs_age_glm <- glm(n_spec_lake ~ lake_age * lake_stream_connect, family = "poisson", data = spec_vs_age_data)
summary(spec_vs_age_glm)

figur_4_b <- spec_vs_age_data %>%
  mutate(Vandløbssystem = ifelse(lake_stream_connect == 1, "+ vandløb", "- vandløb")) %>%
  ggplot(aes(lake_age, n_spec_lake, shape=Vandløbssystem))+
  geom_point()+
  scale_shape_manual(values = c("+ vandløb" = 19, "- vandløb" = 1))+
  ylab("Antal arter")+
  xlab("Sø alder (år)")+
  theme(legend.position = "bottom", legend.title = element_blank())

figur_4 <- figur_4_a/figur_4_b + plot_annotation(tag_levels = "A")

figur_4

ggsave(paste0(getwd(), "/figures_danish/figur_4.png"), figur_4, units = "mm", width = 84, height = 140)
ggsave(paste0(getwd(), "/figures_danish/figur_4.pdf"), figur_4, units = "mm", width = 84, height = 140)

#Figure 2 PSEM MODEL

#Figure 1
lake_fish_ids <- read_csv(paste0(getwd(), "/data_processed/lake_fish_ids.csv"))
fish_species_unique_edit <- read_xlsx(paste0(getwd(), "/data_raw/fish_species_unique.xlsx"))

#Species names
species_list <- fish_species_unique_edit %>% 
  select(fish_id, name_fish_id) |> 
  distinct() |> 
  na.omit()

basin_species <- read_csv(paste0(getwd(), "/data_processed/basin_species.csv")) %>% 
  filter(fish_id %in% lake_fish_ids$fish_id,
         basin_id %in% basins_count$basin_id) |> 
  distinct()

#hyppighed af arter ifht oplande med fisk
basin_with_fish <- length(unique(basin_species$basin_id))

figur_1 <- basin_species |> 
  group_by(fish_id) |> 
  summarise(occur = n()/basin_with_fish) |> 
  left_join(species_list) |> 
  mutate(fish_name = sub("_", " ", name_fish_id),
         fish_name = sub("Sølvkaruds/guldfisk", "Sølvkarusse", fish_name),
         fish_name = sub("Karuds", "Karusse", fish_name),
         perc = 100*occur) |> 
  ggplot(aes(reorder(fish_name, perc), perc))+
  geom_col(fill="white", col="black", width=1)+
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+
  scale_x_discrete(expand = expansion(mult = c(0.03, 0.03)))+
  ylab("Forekomst i vandoplande med fisk (%)")+
  xlab("Arter")+
  coord_flip()

ggsave(paste0(getwd(), "/figures_danish/figur_1.png"), figur_1, units = "mm", width = 129, height = 130)
ggsave(paste0(getwd(), "/figures_danish/figur_1.pdf"), figur_1, units = "mm", width = 129, height = 130)
