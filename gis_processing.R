source("libs_and_funcs.R")

#Processing and combining spatial layers
#Read layers from database
#Calculate attributes for basins and lakes
#st_layers(gis_database)
dk_border <- st_read(dsn = gis_database, layer = "dk_border") 
dk_streams <- st_read(dsn = gis_database, layer = "dk_streams") %>% 
  mutate(stream_length_m = as.numeric(st_length(GEOMETRY)))
dk_lakes <- st_read(dsn = gis_database, layer = "dk_lakes") %>% 
  mutate(lake_area_m2 = as.numeric(st_area(geometry)))
fish_species_lakes <- st_read(dsn = gis_database, layer = "fish_species_lakes")
fish_species_basin <- st_read(dsn = gis_database, layer = "fish_species_basin") 

lake_secchi_chem <- st_read(dsn = gis_database, layer = "lake_secchi_chem") 
lake_secchi_chem_edit <- st_read(dsn = gis_database, layer = "lake_secchi_chem_edit") 
lake_plants <- st_read(dsn = gis_database, layer = "lake_plants") 
lake_plants_edit <- st_read(dsn = gis_database, layer = "lake_plants_edit") 

dk_basins <- st_read(dsn = gis_database, layer = "dk_basins") %>% 
  st_make_valid() %>% 
  mutate(basin_area_m2 = as.numeric(st_area(GEOMETRY)),
         basin_circum_m = as.numeric(st_length(st_cast(GEOMETRY, "MULTILINESTRING"))))

#Only do calc for watersheds overlapping lake subset, consider using centroid for now
# dk_basins_watercover <- dk_basins %>% 
#   st_join(select(dk_streams, stream_length_m)) %>% 
#   st_join(select(dk_lakes, lake_area_m2)) %>% 
#   st_drop_geometry() %>% 
#   group_by(basin_id) %>% 
#   summarise(sum_stream_length_m = sum(stream_length_m),
#             sum_lake_area_m2 = sum(lake_area_m2))
  
dk_lakes_subset <- st_read(dsn = gis_database, layer = "dk_lakes_subset") %>% 
  mutate(lake_area_m2 = as.numeric(st_area(GEOMETRY)),
         lake_circum_m = as.numeric(st_length(st_cast(GEOMETRY, "MULTILINESTRING"))),
         lake_dev_ind = lake_circum_m/(2*sqrt(pi*lake_area_m2)),
         lake_stream_connect = lengths(st_intersects(st_buffer(GEOMETRY, 2), dk_streams)))

#Read newlakes data and age of other new/re-established lakes
chem_newlakes <- readRDS(paste0(getwd(), "/data_raw/chem_newlakes.rds")) %>% 
  rename(secchi_depth_m_1 = secchi_depth_m, pH_pH_1 = pH_pH, alk_meq_l_1 = alk_meq_l,
         chla_ug_l_1 = chla_ug_l, tp_mg_l_1 = tp_mg_l, tn_mg_l_1 = tn_mg_l)
new_lakes_age <- read_xlsx(paste0(getwd(), "/data_raw/new_lakes_age.xlsx")) %>% 
  rename(established_lars = year) %>% 
  na.omit() %>% 
  st_as_sf(coords = c("lat", "long"), crs = 4326) %>% 
  st_transform(dk_epsg)

dk_lakes_subset_age_dist <- dk_lakes_subset %>% 
  st_join(new_lakes_age) %>% 
  mutate(dist_to_sea_m = as.numeric(st_distance(st_centroid(GEOMETRY), st_cast(dk_border, "MULTILINESTRING")))) #Brug polygon i stedet for centroid

#Read data from danish fish atlas
fish_atlas <- read_delim(paste0(getwd(), "/data_raw/Atlas_data_danish_latin_id_basin.csv"), delim = " ") %>% 
  select(basin_id, fish_id = ID) %>% 
  na.omit() %>% 
  distinct()

#Basin fish data from monitoring
fish_monitoring <- fish_species_basin %>% 
  st_join(dk_basins) %>% 
  select(basin_id, fish_id) %>% 
  st_drop_geometry() %>% 
  na.omit() %>% 
  distinct() %>% 
  tbl_df()

#Count species richness in each basin
fish_basin_species_count <- bind_rows(fish_atlas, fish_monitoring) %>% 
  group_by(basin_id) %>% 
  summarise(n_spec_basin = ifelse(all(is.na(fish_id)), 0, length(unique(fish_id)))) %>% 
  left_join(dk_basins) %>% 
  st_as_sf()

#Count species richness in each lakes
fish_lake_species_count <- fish_species_lakes %>%
  group_by(system, site_id, gml_id, elevation, year) %>%
  summarise(n_spec_lake = ifelse(all(is.na(fish_id)), 0, length(unique(fish_id)))) %>%
  ungroup()

#Merge plant data and secchi_chem data
#Join secchi_chem and plants to lake polys
df_secchi_chem <- dk_lakes_subset %>%
  st_join(select(fish_lake_species_count, system, site_id, year)) %>%
  st_join(lake_secchi_chem_edit) %>%
  st_drop_geometry() %>%
  left_join(st_drop_geometry(lake_secchi_chem)) %>% 
  select(gml_id, year, alk_meq_l_2 = alk_mmol_l, chla_ug_l_2 = chla_ug_l, tn_mg_l_2 =tn_mg_l, 
         ph_ph_2 = ph_ph, tp_mg_l_2 = tp_mg_l, secchi_depth_m_2 = secchi_depth_m) %>% 
  na.omit() %>% 
  group_by(gml_id, year) %>% 
  summarise_all(list(mean)) %>% 
  ungroup()

df_plants <- dk_lakes_subset %>%
  st_join(select(fish_lake_species_count, system, site_id, year)) %>%
  st_join(lake_plants_edit) %>%
  st_drop_geometry() %>%
  left_join(st_drop_geometry(lake_plants)) %>% 
  select(gml_id, year, zmean_m, plant_area_perc, plant_vol_perc) %>% 
  na.omit() %>% 
  group_by(gml_id, year) %>% 
  summarise_all(list(mean)) %>% 
  ungroup()

#Merge data
all_model_data <- fish_lake_species_count %>% 
  st_join(fish_basin_species_count) %>% 
  st_join(select(dk_lakes_subset_age_dist, -elevation, -gml_id, -site)) %>% 
  left_join(select(chem_newlakes, system, site_id, established)) %>% 
  left_join(df_secchi_chem) %>% 
  left_join(df_plants) %>% 
  left_join(chem_newlakes) %>% 
  #left_join(dk_basins_watercover) %>% 
  mutate(year_established = coalesce(established, established_lars),
         secchi_depth_m = coalesce(secchi_depth_m_1, secchi_depth_m_2),
         pH_pH = coalesce(pH_pH_1, ph_ph_2),
         alk_meq_l = coalesce(alk_meq_l_1, alk_meq_l_2),
         chla_ug_l = coalesce(chla_ug_l_1, chla_ug_l_2),
         tp_mg_l = coalesce(tp_mg_l_1, tp_mg_l_2),
         tn_mg_l_1 = coalesce(tn_mg_l_1, tn_mg_l_2)) %>% 
  select(-contains("_1"), -contains("_2"),-established, -established_lars) %>% 
  mutate(lake_basin_area_ratio = lake_area_m2/basin_area_m2,
         lake_basin_spec_ratio = n_spec_lake/n_spec_basin,
         basin_area_log10 = log10(basin_area_m2),
         lake_area_log10 = log10(lake_area_m2),
         basin_circum_log10 = log10(basin_circum_m),
         lake_circum_log10 = log10(lake_circum_m),
         lake_stream_connect_binary = factor(ifelse(lake_stream_connect == 0, 0, 1)),
         basin_id_fact = factor(basin_id)) %>% 
  st_crop(st_bbox(dk_border))

#Write data to .rds file
saveRDS(all_model_data, paste0(getwd(), "/data_processed/all_model_data.rds"))

#Save list with data for figures
fig_data <- list(fish_basin_species_count, all_model_data, dk_basins) #dk_border missing
saveRDS(fig_data, paste0(getwd(), "/figures/fig_data.rds"))

