source("libs_and_funcs.R")

#Processing and combining spatial layers
#Read layers from database
#Calculate attributes for basins and lakes
#st_layers(gis_database)
dk_streams <- st_read(dsn = gis_database, layer = "dk_streams")
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
         
dk_lakes_subset <- st_read(dsn = gis_database, layer = "dk_lakes_subset") %>% 
  mutate(lake_area_m2 = as.numeric(st_area(GEOMETRY)),
         lake_circum_m = as.numeric(st_length(st_cast(GEOMETRY, "MULTILINESTRING"))),
         lake_dev_ind = lake_circum_m/(2*sqrt(pi*lake_area_m2)),
         lake_stream_connect = lengths(st_intersects(st_buffer(GEOMETRY, 2), dk_streams)))

#Read newlakes data and age of other new/re-established lakes
chem_newlakes <- readRDS(paste0(getwd(), "/data_raw/chem_newlakes.rds"))
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






#Merge plant data and secchi_chem data
#New lake age data

# #Join secchi_chem and plants to lake polys
# df_secchi_chem <- dk_lakes_subset %>% 
#   st_join(select(fish_lake_species_count, system, site_id, year)) %>% 
#   st_join(lake_secchi_chem_edit) %>% 
#   st_drop_geometry() %>% 
#   left_join(st_drop_geometry(lake_secchi_chem))
#   select(gml_id, year, alk_mmol_l, chla_ug_l, tn_mg_l, ph_ph, tp_mg_l, secchi_depth_m)
# 
# df_plants <- dk_lakes_subset %>% 
#   st_join(select(fish_lake_species_count, system, site_id, year)) %>% 
#   st_join(lake_plants_edit) %>% 
#   st_drop_geometry() %>% 
#   left_join(st_drop_geometry(lake_plants))
#   select(gml_id, year, zmean_m, mean_plant_height_m, plant_area_m2, plant_vol_m3, plant_area_perc, plant_vol_perc)







#Merge data
model_data <- fish_lake_species_count %>% 
  st_join(fish_basin_species_count) %>% 
  st_join(select(dk_lakes_subset_age, -elevation, -gml_id)) %>% 
  left_join(select(chem_newlakes, system, site_id, established)) %>% 
  mutate(year_established = coalesce(established, established_lars)) %>% 
  select(-established, -established_lars) #lav year som category??
  
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
         basin_area_log10, lake_area_log10, elevation, basin_circum_log10, lake_circum_log10, lake_dev_ind, basin_id_fact) #%>% 
  #mutate_at(vars(lake_basin_area_ratio, basin_area_log10, lake_area_log10, elevation, basin_circum_log10, lake_circum_log10, lake_dev_ind), list(scale))

#Binomial glmm model
library(lme4)
glob_mod <- glmer(n_spec_lake/n_spec_basin~lake_stream_connect_binary+basin_area_log10+elevation+(1|basin_id_fact),
                  weights=n_spec_basin, data=model_df, family="binomial")
summary(glob_mod)
plot(glob_mod)
