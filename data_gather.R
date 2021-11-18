source("libs_and_funcs.R")


#IS BORNHOLM CUT OUT??


#Lake species count 
fish_species_lakes <- st_read(gis_database, layer = "fish_species_lakes")

lake_fish_ids <- fish_species_lakes$fish_id %>% unique() %>% na.omit()

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
basin_species_count <- fish_species_basin %>% 
  st_join(basins) %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(basin_id, fish_id) %>% 
  na.omit() %>% 
  bind_rows(atlas_clean) %>% 
  filter(fish_id %in% lake_fish_ids) %>% 
  group_by(basin_id) %>% 
  summarise(n_spec_basin = ifelse(all(is.na(fish_id)), 0, length(unique(fish_id))))

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

#Addtional lakes ages
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
basin_attr <- readRDS(paste0(getwd(), "/data_processed/basin_attr.rds"))

lake_basin_id <- dk_lakes_subset %>% 
  select(gml_id) %>% 
  st_centroid() %>% 
  st_join(basins[, "basin_id"]) %>%
  st_drop_geometry()

basin_attr_all <- basin_attr %>% 
  left_join(basin_species_count) %>% 
  left_join(st_drop_geometry(basins)) %>% 
  left_join(lake_basin_id)

#Collect all data
lake_species_all <- lake_species_count_bathy %>% 
  left_join(lake_species_count_chem) %>% 
  left_join(dk_lakes_subset_age) %>% 
  left_join(st_drop_geometry(dk_lakes_subset[,c("gml_id", "lake_stream_connect")])) %>% 
  mutate(year_established = coalesce(year_established, year_established_2)) %>% 
  select(-year_established_2) %>% 
  left_join(basin_attr_all)

#Write csv
write_csv(lake_species_all, paste0(getwd(), "/data_processed/lake_species_all.csv"))
