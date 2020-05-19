source("libs_and_funcs.R")

#Processing and combining spatial layers
#Read layers from database
#Calculate attributes for basins and lakes
#st_layers(gis_database)

dk_border <- st_read(dsn = gis_database, layer = "dk_border") 
dk_iceage <- st_read(dsn = gis_database, layer = "dk_iceage") 
dk_streams <- st_read(dsn = gis_database, layer = "dk_streams") %>% 
  mutate(stream_length_m = as.numeric(st_length(GEOMETRY)))
dk_lakes <- st_read(dsn = gis_database, layer = "dk_lakes_edit") %>%
  rename(lake_area_m2 = area)
fish_species_lakes <- st_read(dsn = gis_database, layer = "fish_species_lakes")
fish_species_basin <- st_read(dsn = gis_database, layer = "fish_species_basin") 

lake_secchi_chem <- st_read(dsn = gis_database, layer = "lake_secchi_chem") 
lake_secchi_chem_edit <- st_read(dsn = gis_database, layer = "lake_secchi_chem_edit") 
lake_plants <- st_read(dsn = gis_database, layer = "lake_plants") 
lake_plants_edit <- st_read(dsn = gis_database, layer = "lake_plants_edit") 

dk_basins <- st_read(dsn = gis_database, layer = "dk_basins") %>% 
  mutate(basin_area_m2 = as.numeric(st_area(GEOMETRY)),
         basin_circum_m = as.numeric(st_length(st_cast(GEOMETRY, "MULTILINESTRING"))))

#calc distance from lake to outlet per basin!
dk_basins_outlets <- st_read(dsn = gis_database, layer = "dk_dem_streams_points") %>%
  filter(cat == 2) %>%
  cbind(., st_coordinates(.)) %>%
  select(outlet_x = X, outlet_y = Y) %>% 
  st_join(dk_basins) %>%
  st_drop_geometry() %>% 
  na.omit() %>%
  group_by(basin_id) %>% 
  summarise(outlet_x = first(outlet_x), outlet_y = first(outlet_y))

dk_lakes_subset <- st_read(dsn = gis_database, layer = "dk_lakes_subset") %>% 
  mutate(lake_area_m2 = as.numeric(st_area(GEOMETRY)),
         lake_circum_m = as.numeric(st_length(st_cast(GEOMETRY, "MULTILINESTRING"))),
         lake_dev_ind = lake_circum_m/(2*sqrt(pi*lake_area_m2)),
         lake_stream_connect = lengths(st_intersects(GEOMETRY, dk_streams)))

#Compute stream length and lake area in basin 
#Using centroids for now
dk_streams_centroid <- st_centroid(select(dk_streams, stream_length_m))

dk_lakes_centroid <- st_centroid(select(dk_lakes, lake_area_m2))

dk_basins_lake_area <- dk_basins %>%
  st_join(dk_lakes_centroid) %>%
  st_drop_geometry() %>%
  select(basin_id, lake_area_m2) %>%
  group_by(basin_id) %>%
  summarise(basin_sum_lake_area_m2 = sum(lake_area_m2), basin_mean_lake_area_m2 = mean(lake_area_m2))

dk_basins_stream_length <- dk_basins %>%
  st_join(dk_streams_centroid) %>%
  st_drop_geometry() %>%
  select(basin_id, stream_length_m) %>%
  group_by(basin_id) %>%
  summarise(basin_sum_stream_length_m = sum(stream_length_m))

#Read newlakes data and age of other new/re-established lakes
chem_newlakes <- readRDS(paste0(getwd(), "/data_raw/chem_newlakes.rds")) %>% 
  rename(secchi_depth_m_1 = secchi_depth_m, pH_pH_1 = pH_pH, alk_meq_l_1 = alk_meq_l,
         chla_ug_l_1 = chla_ug_l, tp_mg_l_1 = tp_mg_l, tn_mg_l_1 = tn_mg_l)

new_lakes_age <- read_xlsx(paste0(getwd(), "/data_raw/new_lakes_age.xlsx")) %>% 
  rename(established_lars = year) %>% 
  na.omit() %>% 
  st_as_sf(coords = c("lat", "long"), crs = 4326) %>% 
  st_transform(dk_epsg)

dk_lakes_subset_age <- dk_lakes_subset %>% 
  st_join(new_lakes_age) 

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
  left_join(dk_basins_stream_length) %>% 
  left_join(dk_basins_lake_area) %>% 
  left_join(dk_basins_outlets) %>% 
  st_as_sf() %>% 
  mutate(basin_ice_covered = factor(st_intersects(dk_iceage, st_centroid(GEOMETRY), sparse = FALSE))) # %>%  cbind(., st_coordinates(st_centroid(.)))? compute basin centroid coords also?

#Count species richness in each lakes
fish_lake_species_count <- fish_species_lakes %>%
  group_by(system, site_id, gml_id, elevation, year) %>%
  summarise(n_spec_lake = ifelse(all(is.na(fish_id)), 0, length(unique(fish_id)))) %>%
  ungroup()

#Merge plant data and secchi_chem data
#Join secchi_chem and plants to lake polys

#One lake (Tryggelev Nor) with wrong site_id_2
#Remove "wrong" site_id_2, and replace "right" with "wrong"
lake_secchi_chem_cor <- lake_secchi_chem %>% 
  st_drop_geometry() %>% 
  filter(!(site_id_2 == 47000318)) %>% 
  mutate(site_id_2 = ifelse(site_id_2 == 47000020, 47000318, site_id_2))

df_secchi_chem <- dk_lakes_subset %>%
  st_join(select(fish_lake_species_count, system, site_id, year)) %>%
  st_join(lake_secchi_chem_edit) %>%
  st_drop_geometry() %>%
  left_join(lake_secchi_chem_cor) %>% 
  select(gml_id, year, alk_meq_l_2 = alk_mmol_l, chla_ug_l_2 = chla_ug_l, tn_mg_l_2 =tn_mg_l, 
         ph_ph_2 = ph_ph, tp_mg_l_2 = tp_mg_l, secchi_depth_m_2 = secchi_depth_m) %>% 
  group_by(gml_id, year) %>% 
  summarise_all(list(mean), na.rm = TRUE) %>% 
  ungroup()

df_plants <- dk_lakes_subset %>%
  st_join(select(fish_lake_species_count, system, site_id, year)) %>%
  st_join(lake_plants_edit) %>%
  st_drop_geometry() %>%
  left_join(st_drop_geometry(lake_plants)) %>% 
  select(gml_id, year, zmean_m, zmax_m = max_depth_m, plant_area_perc, plant_vol_perc) %>%
  group_by(gml_id, year) %>% 
  summarise_all(list(mean), na.rm = TRUE) %>% 
  ungroup()

#Assign lake just outside a basin to the nearby basin
#No basin intersecting 5 lakes because of low (0 or below) elevation in the elevation model
basin_id_cor <- tribble(~system, ~site_id, ~basin_id_cor,
                        #"lake", 9000212, , Lønnerup Fjord
                        #"lake", 47000195, , udenfor opland  Keldsnor, Langeland
                        #"lake", 54000038, 1513, sø uden fisk i opland uden fisk
                        "newlake", 15, 1723,
                        "newlake", 20, 1258)

#Merge data
all_model_data <- fish_lake_species_count %>% 
  st_join(select(fish_basin_species_count, basin_id)) %>% 
  left_join(basin_id_cor) %>% 
  mutate(basin_id = coalesce(basin_id, basin_id_cor)) %>% 
  select(-basin_id_cor) %>% 
  left_join(st_drop_geometry(fish_basin_species_count)) %>% 
  st_join(select(dk_lakes_subset_age, -elevation, -gml_id, -site)) %>% 
  left_join(select(chem_newlakes, system, site_id, established)) %>% 
  left_join(df_secchi_chem) %>% 
  left_join(df_plants) %>% 
  left_join(chem_newlakes) %>% 
  mutate(year_established = coalesce(established, established_lars),
         secchi_depth_m = coalesce(secchi_depth_m_1, secchi_depth_m_2),
         pH_pH = coalesce(pH_pH_1, ph_ph_2),
         alk_meq_l = coalesce(alk_meq_l_1, alk_meq_l_2),
         chla_ug_l = coalesce(chla_ug_l_1, chla_ug_l_2),
         tp_mg_l = coalesce(tp_mg_l_1, tp_mg_l_2),
         tn_mg_l = coalesce(tn_mg_l_1, tn_mg_l_2)) %>% 
  select(-contains("_1"), -contains("_2"),-established, -established_lars) %>% 
  mutate(spec_proportion = n_spec_lake/n_spec_basin,
         lake_basin_area_ratio = lake_area_m2/basin_area_m2,
         basin_area_log10 = log10(basin_area_m2),
         lake_area_log10 = log10(lake_area_m2),
         basin_circum_log10 = log10(basin_circum_m),
         lake_circum_log10 = log10(lake_circum_m),
         lake_stream_connect_binary = factor(ifelse(lake_stream_connect == 0, 0, 1)),
         basin_mean_lake_area_m2 = ifelse(is.na(basin_mean_lake_area_m2), 0, basin_mean_lake_area_m2),
         basin_sum_lake_area_m2 = ifelse(is.na(basin_sum_lake_area_m2), 0, basin_sum_lake_area_m2),
         basin_sum_stream_length_m = ifelse(is.na(basin_sum_stream_length_m), 0, basin_sum_stream_length_m),
         basin_id_fact = factor(basin_id),
         lake_ice_covered = factor(st_intersects(GEOMETRY, dk_iceage, sparse=FALSE)),
         ) %>% 
  st_crop(st_bbox(dk_border))

#Calculate distance from each lake to basin outlet
outlet_distance <- all_model_data %>%
  filter(!is.na(outlet_x)) %>%
  mutate(outlet_dist_m = as.numeric(st_distance(GEOMETRY, st_as_sf(data.frame(x = outlet_x, y = outlet_y), crs = dk_epsg, coords = c("x", "y")), by_element = TRUE))) %>%
  select(system, site_id, outlet_dist_m) %>%
  st_drop_geometry() %>% 
  mutate(outlet_dist_m = ifelse(site_id == 15, 500, outlet_dist_m)) #set outlet to 500 (~0.5*Bundssø dist) for Mjelssø due to corrected basin

#Join back to data frame
all_model_data_dist <- all_model_data %>%
  left_join(outlet_distance)

#Save list with data for modeling and figures
model_and_fig_data <- list(fish_basin_species_count, all_model_data)
saveRDS(model_and_fig_data, paste0(getwd(), "/data_processed/model_and_fig_data.rds"))

#Write files with fish data and lake name to files for further inspection
#Lake names and id's
# lake_names <- readRDS(paste0(getwd(), "/data_raw/lake_names.rds"))
# 
# all_model_data %>%
#   left_join(lake_names) %>%
#   write.csv2(paste0(getwd(), "/data_raw/all_model_data_names_3.csv"))

