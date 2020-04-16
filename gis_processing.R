source("libs_and_funcs.R")

#Processing and combining spatial layers
#Read layers from database
st_layers(gis_database)

dk_lakes_subset <- st_read(dsn = gis_database, layer = "dk_lakes_subset")
dk_basins <- st_read(dsn = gis_database, layer = "dk_basins")
dk_streams <- st_read(dsn = gis_database, layer = "dk_streams")
fish_species_lakes <- st_read(dsn = gis_database, layer = "fish_species_lakes")
fish_species_basin <- st_read(dsn = gis_database, layer = "fish_species_basin") %>% 
  select(-system, -site_id, -name)

#Collect data for lake basins
#Read data from danish fish atlas
fish_atlas <- read_delim(paste0(getwd(), "/data_raw/Atlas_data_danish_latin_id_basin.csv"), delim = " ") %>% 
  select(basin_id, fish_id = ID) %>% 
  na.omit() %>% 
  distinct() %>% 
  left_join(dk_basins) %>% 
  st_as_sf()

#Count species richness in each basin
fish_basin_species_count <- dk_basins %>% 
  st_join(fish_species_basin) %>% 
  rbind(fish_atlas) %>% 
  group_by(basin_id) %>% 
  summarise(n_spec_basin = ifelse(all(is.na(fish_id)), 0, length(unique(fish_id)))) %>%
  ungroup() %>% 
  mutate(basin_area = as.numeric(st_area(GEOMETRY))) %>% 
  st_cast("MULTILINESTRING") %>% 
  mutate(basin_circum = as.numeric(st_length(GEOMETRY))) %>% 
  st_cast("MULTIPOLYGON")





#Collect data for each lake and country species number
#Count number of fish caught for each sampling
fish_lake_species_count <- fish_species_lakes %>%
  group_by(system, site_id, gml_id, elevation) %>%
  summarise(n_spec_lake = ifelse(all(is.na(fish_id)), 0, length(unique(fish_id)))) %>%
  ungroup() %>% 
  st_join(fish_basin_species_count) #NAs?!?!!?

#Buffer lakes and examine if they intersect stream network
dk_lakes_subset_buffer <- dk_lakes_subset %>% 
  st_buffer(2)

lake_stream_connect <- lengths(st_intersects(dk_lakes_subset_buffer, dk_streams))

#Calculate lake attributes
dk_lakes_attr <- dk_lakes_subset %>% 
  mutate(lake_area_m2 = as.numeric(st_area(GEOMETRY))) %>% 
  st_cast("MULTILINESTRING") %>% 
  mutate(lake_circum_m = as.numeric(st_length(GEOMETRY)),
         lake_dev_ind = lake_circum_m/(2*sqrt(pi*lake_area_m2))) %>% 
  st_cast("POLYGON") %>% 
  add_column(lake_stream_connect = lake_stream_connect)
  








#Join secchi_chem and plants to lake polys
#Use lake poly to check for intersect with stream network (buffer 1/2 m?)