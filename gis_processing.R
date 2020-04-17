source("libs_and_funcs.R")

#Processing and combining spatial layers
#Read layers from database
#Calculate attributes for basins and lakes
#st_layers(gis_database)
dk_streams <- st_read(dsn = gis_database, layer = "dk_streams")
fish_species_lakes <- st_read(dsn = gis_database, layer = "fish_species_lakes")
fish_species_basin <- st_read(dsn = gis_database, layer = "fish_species_basin") 
lake_secchi_chem <- st_read(dsn = gis_database, layer = "lake_secchi_chem") 
lake_plants <- st_read(dsn = gis_database, layer = "lake_plants") 


dk_basins <- st_read(dsn = gis_database, layer = "dk_basins") %>% 
  st_make_valid() %>% 
  mutate(basin_area_m2 = as.numeric(st_area(GEOMETRY)),
         basin_circum_m = as.numeric(st_length(st_cast(GEOMETRY, "MULTILINESTRING"))))
         
dk_lakes_subset <- st_read(dsn = gis_database, layer = "dk_lakes_subset") %>% 
  mutate(lake_area_m2 = as.numeric(st_area(GEOMETRY)),
         lake_circum_m = as.numeric(st_length(st_cast(GEOMETRY, "MULTILINESTRING"))),
         lake_dev_ind = lake_circum_m/(2*sqrt(pi*lake_area_m2)),
         lake_stream_connect = lengths(st_intersects(st_buffer(GEOMETRY, 2), dk_streams)))

#Read data from danish fish atlas
fish_atlas <- read_delim(paste0(getwd(), "/data_raw/Atlas_data_danish_latin_id_basin.csv"), delim = " ") %>% 
  select(basin_id, fish_id = ID) %>% 
  na.omit() %>% 
  distinct()

#basin fish data from monitoring
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


#Join secchi_chem and plants to lake polys
df_secchi_chem <- dk_lakes_subset %>% 
  st_join(lake_secchi_chem) %>%
  st_drop_geometry() %>% 
  select(gml_id, year, alk_mmol_l, chla_ug_l, tn_mg_l, ph_ph, tp_mg_l, secchi_depth_m)

df_plants <- dk_lakes_subset %>% 
  st_join(lake_plants) %>%
  st_drop_geometry() %>% 
  select(gml_id, year, zmean_m, mean_plant_height_m, plant_area_m2, plant_vol_m3, plant_area_perc, plant_vol_perc)


#Merge data
model_data <- fish_lake_species_count %>% 
  st_join(fish_basin_species_count) %>% 
  st_join(select(dk_lakes_subset, -elevation, -gml_id)) %>% 
  na.omit()
  

#Align coordinates med lake subset
  #left_join(df_secchi_chem) 
  #left_join(df_plants)
  
model_df <- model_data %>% 
  st_drop_geometry() %>% 
  mutate(lake_basin_area_ratio = lake_area_m2/basin_area_m2,
         lake_basin_spec_ratio = n_spec_lake/n_spec_basin,
         basin_area_log10 = log10(basin_area_m2),
         lake_area_log10 = log10(lake_area_m2),
         basin_circum_log10 = log10(basin_circum_m),
         lake_circum_log10 = log10(lake_circum_m),
         lake_stream_connect_binary = factor(ifelse(lake_stream_connect == 0, 0, 1)),
         basin_id_fact = factor(basin_id)) %>% 
  select(n_spec_lake, n_spec_basin, system, lake_stream_connect_binary, lake_basin_area_ratio, 
         basin_area_log10, lake_area_log10, elevation, basin_circum_log10, lake_circum_log10, lake_dev_ind)

#Binomial glm model
mod_0 <- glm(cbind(n_spec_lake, n_spec_basin) ~ ., data = model_df, family = "binomial")
summary(mod_0)




