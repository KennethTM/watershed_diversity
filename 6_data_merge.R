source("0_libs_and_funcs.R")

#Combine basin and lake level data from different sources

#Lake species count 
fish_species_lakes <- st_read(gis_database, layer = "fish_species_lakes")

#Identify all species found in lake surveys and write to file
lake_fish_ids <- fish_species_lakes %>% 
  st_drop_geometry() %>% 
  select(fish_id) %>% 
  distinct() %>% 
  na.omit()

write_csv(lake_fish_ids, paste0(getwd(), "/data_processed/lake_fish_ids.csv"))

#Count fish species
lake_species_count <- fish_species_lakes %>% 
  st_drop_geometry() %>% 
  group_by(gml_id, site_id, site_name) %>% 
  summarise(n_spec_lake = ifelse(all(is.na(fish_id)), 0, length(unique(fish_id))),
            year_sample = first(year_sample),
            year_established = first(year_established),
            lake_elev_m = first(lake_elev_m),
            lake_area_m2 = first(lake_area_m2),
            shoreline_m = first(shoreline_m)) %>% 
  ungroup()

#Basin data
basins <- st_read(gis_database, layer = "basins")
fish_species_basin <- st_read(dsn = gis_database, layer = "fish_species_basin") 
atlas_clean <- read_csv(paste0(getwd(), "/data_processed/atlas_clean.csv"))

#Basin species count
basin_species <- fish_species_basin %>% 
  st_join(basins) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(basin_id, fish_id) %>% 
  na.omit() %>% 
  bind_rows(atlas_clean) %>% 
  filter(fish_id %in% lake_fish_ids$fish_id) 

basin_species_count <- basin_species %>% 
  group_by(basin_id) %>% 
  summarise(n_spec_basin = ifelse(all(is.na(fish_id)), 0, length(unique(fish_id))))

#Write basin species and count to files
write_csv(basin_species[, c("basin_id", "fish_id")], paste0(getwd(), "/data_processed/basin_species.csv"))
write_csv(basin_species_count, paste0(getwd(), "/data_processed/basin_species_count.csv"))

#Join bathy data
lake_bathy_df <- st_read(gis_database, layer = "lake_bathy") %>% 
  st_drop_geometry() %>% 
  mutate(site_id = as.character(site_id))

newlake_bathy <- read_csv("data_raw/bathy_newlakes.csv") %>% 
  select(site_name, bathy_area = lake_area_m2, bathy_vol = lake_vol_m3, bathy_zmean = mean_depth_m, bathy_zmax = max_depth_m)

lake_species_count_bathy <- lake_species_count %>% 
  left_join(lake_bathy_df) %>% 
  left_join(newlake_bathy, by = c("site_name" = "site_name")) %>% 
  mutate(bathy_area = coalesce(bathy_area.x, bathy_area.y),
         bathy_vol = coalesce(bathy_vol.x, bathy_vol.y),
         bathy_zmean = coalesce(bathy_zmean.x, bathy_zmean.y),
         bathy_zmax = coalesce(bathy_zmax.x, bathy_zmax.y)) %>% 
  select(-contains(".x"), -contains(".y"))

#Join chemistry data
dk_streams <- st_read(dsn = gis_database, layer = "dk_streams")

dk_lakes_subset <- st_read(gis_database, layer = "dk_lakes_subset") %>% 
  mutate(lake_stream_connect = lengths(st_intersects(GEOMETRY, dk_streams)))

chem_newlakes <- read_csv(paste0(getwd(), "/data_processed/chem_newlakes.csv")) %>% 
  rename(alk_mmol_l=alk_meq_l, ph_ph=pH_pH)

lake_chem_gml <- st_read(gis_database, layer = "lake_chem") %>% 
  rename(year_sample = year) %>% 
  st_join(dk_lakes_subset) %>% 
  filter(!is.na(gml_id)) %>% 
  select(gml_id, year_sample, alk_mmol_l, chla_ug_l, tn_mg_l, ph_ph, tp_mg_l, secchi_depth_m) %>% 
  st_drop_geometry() %>% 
  group_by(gml_id, year_sample) %>% 
  summarise_all(list(mean),  na.rm = TRUE)

lake_chem_1 <- lake_species_count %>% 
  filter(!str_detect(site_id, "newlakes_")) %>% 
  left_join(lake_chem_gml)

lake_chem_2 <- lake_species_count %>% 
  filter(str_detect(site_id, "newlakes_")) %>% 
  left_join(chem_newlakes)

lake_species_count_chem <- bind_rows(lake_chem_1, lake_chem_2)

#Additional lakes ages
new_lakes_other <- read_xlsx(paste0(getwd(), "/data_raw/new_lakes_other.xlsx")) 

new_lakes_other_sf <- new_lakes_other %>% 
  select(-site_name) %>% 
  na.omit() %>% 
  st_as_sf(coords=c("lat", "lon"), crs = 4326) %>% 
  st_transform(dk_epsg)

dk_lakes_subset_age <- dk_lakes_subset %>% 
  st_join(new_lakes_other_sf) %>% 
  select(gml_id, year_established_2 = year_established) %>% 
  st_drop_geometry() %>% 
  na.omit()

#Lakes basin data
basin_attributes <- read_csv(paste0(getwd(), "/data_processed/basin_attributes.csv"))

lake_basin_id <- dk_lakes_subset %>% 
  select(gml_id) %>% 
  st_centroid() %>% 
  st_join(basins[, "basin_id"]) %>%
  st_drop_geometry()

basin_attr_all <- basin_attributes %>% 
  left_join(basin_species_count) %>% 
  left_join(st_drop_geometry(basins)) %>% 
  left_join(lake_basin_id)

#Collect all data
all_data_merge <- lake_species_count_bathy %>% 
  left_join(lake_species_count_chem) %>% 
  left_join(dk_lakes_subset_age) %>% 
  left_join(st_drop_geometry(dk_lakes_subset[,c("gml_id", "lake_stream_connect")])) %>% 
  mutate(year_established = coalesce(year_established, year_established_2)) %>% 
  select(-year_established_2) %>% 
  left_join(basin_attr_all) %>% 
  rename(lake_stream_connect_n = lake_stream_connect) %>% 
  mutate(lake_stream_connect = ifelse(lake_stream_connect_n > 0, 1, 0))

#Write csv
write_csv(all_data_merge, paste0(getwd(), "/data_processed/model_data_raw.csv"))

#Write lake species list to file
lake_species <- fish_species_lakes %>% 
  st_drop_geometry() %>% 
  select(gml_id, fish_id) %>% 
  left_join(lake_basin_id)

write_csv(lake_species, paste0(getwd(), "/data_processed/lake_species.csv"))
