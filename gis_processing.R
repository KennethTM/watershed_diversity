source("libs_and_funcs.R")

#Processing and combining spatial layers
#Read layers from database
#st_layers(gis_database)

dk_streams <- st_read(dsn = gis_database, layer = "dk_streams")
fish_species_lakes <- st_read(dsn = gis_database, layer = "fish_species_lakes")
fish_species_basin <- st_read(dsn = gis_database, layer = "fish_species_basin") 
dk_basins <- st_read(dsn = gis_database, layer = "dk_basins") %>% 
  mutate(basin_area_m2 = as.numeric(st_area(GEOMETRY))) %>% 
  st_cast("MULTILINESTRING") %>% 
  mutate(basin_circum_m = as.numeric(st_length(GEOMETRY))) %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_make_valid() 
dk_lakes_subset <- st_read(dsn = gis_database, layer = "dk_lakes_subset") %>% 
  mutate(lake_area_m2 = as.numeric(st_area(GEOMETRY))) %>% 
  st_cast("MULTILINESTRING") %>% 
  mutate(lake_circum_m = as.numeric(st_length(GEOMETRY)),
         lake_dev_ind = lake_circum_m/(2*sqrt(pi*lake_area_m2))) %>% 
  st_cast("POLYGON")

#Buffer lakes and examine if they intersect stream network
dk_lakes_subset_buffer <- dk_lakes_subset %>% 
  st_buffer(2)

lake_stream_connect <- lengths(st_intersects(dk_lakes_subset_buffer, dk_streams))

#Collect data for lake basins
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
  summarise(n_spec_basin = ifelse(all(is.na(fish_id)), 0, length(unique(fish_id))))

#Collect data for each lake and country species number
#Count number of fish caught for each sampling
fish_lake_species_count <- fish_species_lakes %>%
  group_by(system, site_id, gml_id, elevation) %>%
  summarise(n_spec_lake = ifelse(all(is.na(fish_id)), 0, length(unique(fish_id)))) %>%
  ungroup()







#Add basin id's and attributes
fish_lake_species_count %>% 
  st_join(dk_basins) %>% filter(is.na(basin_id))











#Join secchi_chem and plants to lake polys
