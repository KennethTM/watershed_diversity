source("libs_and_funcs.R")

#Prepare fish data for further analysis:
#"New lakes project" data
#Lake monitoring data
#Stream monitoring data
#Danish fish atlas data

#Read downloaded fish data, clean and save to gis_database
#For lake fish monitoring both weigth and net tables are needed to also include zero-catch samples
fish_weight_lake <- read_xlsx(paste0(getwd(), "/data_raw/", "odaforalle_fish_weight_lake.xlsx"))
fish_net_lake <- read_xlsx(paste0(getwd(), "/data_raw/", "odaforalle_fish_net_lake.xlsx"))

fish_lake_species_raw <- left_join(fish_net_lake, fish_weight_lake) %>%
  mutate(system = "lake", date = ymd(Dato), year_sample = year(date),
         Xutm_Euref89_Zone32 = as.numeric(Xutm_Euref89_Zone32), Yutm_Euref89_Zone32 = as.numeric(Yutm_Euref89_Zone32),
         name_danish = gsub(" ", "_", `Dansk navn`),
         site_id = as.character(ObservationsStedNr)) %>% 
  select(system, site_id, site_name = ObservationsStedNavn, year_sample,
         Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, name_danish) %>% 
  distinct() %>% 
  filter(!is.na(Xutm_Euref89_Zone32))

fish_lake_species <- fish_lake_species_raw %>% 
  nest(data = c("name_danish")) %>% 
  mutate(na_if_zerocatch = map(data, keep_na_for_zero_catch)) %>% 
  select(-data) %>% 
  unnest(na_if_zerocatch)

#Stream fish, used to define basin species pool
fish_stream_raw <- read_xlsx(paste0(getwd(), "/data_raw/", "odaforalle_fish_stream.xlsx"))
fish_stream_species <- fish_stream_raw %>% 
  mutate(system = "stream", date = ymd(Dato), year_sample = year(date),
         Xutm_Euref89_Zone32 = as.numeric(Xutm_Euref89_Zone32), 
         Yutm_Euref89_Zone32 = as.numeric(Yutm_Euref89_Zone32),
         name_danish = gsub(" ", "_", Fiskart),
         site_id = as.character(ObservationsStedNr)) %>% 
  select(system, site_id, year_sample,
         Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, name_danish) %>% 
  distinct() %>% 
  na.omit()

#Fish in new lakes
fish_newlakes_raw <- read_xlsx(paste0(getwd(), "/data_raw/new_lakes_project.xlsx"), sheet = "new_lakes_project") %>% 
  group_by(site_name) %>% 
  mutate(system = "lake",
         site_id = paste0("newlakes_proj_", cur_group_id())) %>% 
  ungroup()

fish_newlakes_raw_zone32 <- fish_newlakes_raw %>% 
  filter(utm_zone == 32)

fish_newlakes_raw_zone33 <- fish_newlakes_raw %>% 
  filter(utm_zone == 33) 

fish_newlakes_raw_zone33_to_zone32 <- fish_newlakes_raw_zone33 %>% 
  st_as_sf(coords=c("utm_x", "utm_y"), crs = (dk_epsg+1)) %>% 
  st_transform(dk_epsg) %>% 
  bind_cols(as.data.frame(st_coordinates(.))) %>% 
  st_drop_geometry() %>% 
  rename(utm_x=X, utm_y=Y)

fish_newlakes <- bind_rows(fish_newlakes_raw_zone32, fish_newlakes_raw_zone33_to_zone32) %>% 
  select(system, site_id, site_name, year_established, year_sample,
         Xutm_Euref89_Zone32 = utm_x, Yutm_Euref89_Zone32 = utm_y, name_danish) %>% 
  distinct()

chem_newlakes <- fish_newlakes_raw %>% 
  select(site_id, secchi_depth_m:tn_mg_l) %>% 
  distinct()

#Fish species from both lakes and streams monitoring data and new lakes
fish_species <- bind_rows(fish_lake_species, fish_stream_species, fish_newlakes)

#Unique species found in raw data
fish_species_unique <- fish_species %>%
  distinct(name_danish) %>%
  arrange(name_danish) %>%
  na.omit()

#Write to file and add new columns with id's and actions
#Editted list saved as "fish_species_unique.xlsx"
#write_csv(fish_species_unique, paste0(getwd(), "/data_raw/fish_species_unique.csv"))

#Fish species for further analysis and create fish species id
#Actions: 0=do_nothing, 1=remove_species, 2=remove_lake (brackish or marine)
fish_species_unique_edit <- read_xlsx(paste0(getwd(), "/data_raw/fish_species_unique.xlsx")) %>% 
  rename(action = `how(0=do_nothing)(1=remove_species)(2=remove_lake)`)

fish_species_remove_lake <- filter(fish_species_unique_edit, action == 2)

site_id_remove <- fish_species %>% 
  filter(name_danish %in% fish_species_remove_lake$name_danish) %>% 
  pull(site_id) %>% 
  unique()

fish_species_valid <- fish_species_unique_edit %>% 
  filter(action == 0 & !is.na(name_danish))

#Join to table with species id's, keep valid fish species and NA
#Keep observations between 1990 and 2020
#Join with fish_id's
fish_species_sub <- fish_species %>% 
  filter(!(site_id %in% site_id_remove),
         name_danish %in% fish_species_valid$name_danish | is.na(name_danish)) %>% 
  filter(between(year_sample, 1990, 2020)) %>% 
  left_join(fish_species_valid[,c("name_danish", "fish_id")])

#Write species data to gis database
#Used to determine basin species richness
fish_species_sub_sf <- fish_species_sub %>% 
  st_as_sf(coords = c("Xutm_Euref89_Zone32", "Yutm_Euref89_Zone32"), crs = dk_epsg)

st_write(fish_species_sub_sf, dsn = gis_database, layer = "fish_species_basin", delete_layer = TRUE)




# #Read atlas fish data, join with names and write to file
# atlas_raw <- read_xlsx(paste0(getwd(), "/data_raw/atlasdata_oplande.xlsx")) %>% 
#   mutate(name_atlas = gsub(" ", "_", species_latin)) %>% 
#   left_join(select(fish_unique_edit, name_atlas, fish_id, action)) %>% 
#   filter(action == 0)
# 
# atlas_raw %>% 
#   select(basin_id, fish_id) %>% 
#   distinct() %>% 
#   saveRDS(paste0(getwd(), "/data_raw/atlasdata_ids.rds"))




#Lakes for investigation is sample with highest richness after 2006
fish_species_richest_survey <- fish_species_sub %>% 
  filter(year >= 2006,
         system %in% c("lake", "newlake")) %>% 
  group_by(system, site_id, year) %>% 
  summarise(n_spec = ifelse(all(is.na(fish_id)), 0, length(unique(fish_id)))) %>% 
  summarise(year_max_rich = year[which.max(n_spec)])  %>% 
  ungroup()

#site_id's to be exclude
#Two double sampling from the same lake
#Different sampling in same lake (Utterslev Mose)
fish_lake_doubles <- c(c(14000045, 20000218),
                       c(53000046, 53000047))

#Lakes for investigation
fish_species_lakes_raw <- fish_species_richest_survey %>% 
  rename(year = year_max_rich) %>% 
  left_join(fish_species_sub) %>% 
  distinct() %>% 
  filter(!(site_id %in% fish_lake_doubles))

#Write lake raw species data to gis database
fish_species_lakes_raw_sf <- fish_species_lakes_raw %>% 
  st_as_sf(coords = c("Xutm_Euref89_Zone32", "Yutm_Euref89_Zone32"), crs = dk_epsg)

st_write(fish_species_lakes_raw_sf, dsn = gis_database, layer = "fish_species_lakes_raw", delete_layer = TRUE)

#Write files with with problems which should be reviewed and fixed manually
#Multiple fish surveys in same lake polygon
#Missing lake polygons
#Problems and actions are listed in "Fishdata with no polygon and with multiple poly.xlsx"
#Use raw fish data to identify fish site/lake polygon problems
dk_lakes <- st_read(dsn = gis_database, layer = "dk_lakes")

#Lakes with fish data but no lake polygon
fish_species_lakes_raw_sf %>% 
  select(system, site_id) %>% 
  distinct(.keep_all = TRUE) %>% 
  st_join(dk_lakes) %>% 
  filter(is.na(gml_id)) %>%  
  st_write(paste0(getwd(), "/data_raw/fish_data_with_no_polygon.sqlite"), delete_dsn = TRUE)

#Some polygons are shared between site_id's
fish_survey_with_shared_poly <- fish_species_lakes_raw_sf %>% 
  select(system, site_id) %>% 
  distinct(.keep_all = TRUE) %>% 
  st_join(dk_lakes, left = FALSE) %>% 
  group_by(gml_id) %>% 
  add_tally() %>% 
  filter(n > 1)

fish_survey_with_shared_poly %>% 
  st_write(paste0(getwd(), "/data_raw/fish_data_with_more_than_one_polygon.sqlite"), delete_dsn = TRUE) 

#Use gml_id for polygons which contain multiple fish surveys
#Save and create cutline layer in google earth
dk_lakes %>% 
  filter(gml_id %in% fish_survey_with_shared_poly$gml_id) %>% 
  st_write(paste0(getwd(), "/data_raw/polygons_with_multiple_fish_surveys.kml"), delete_dsn = TRUE) 
# 
# #Add also a missing polygon to national lake polygon layer (OpenStreetMap)
# lillelund_engso <- st_read(paste0(getwd(), "/data_raw/lillelund_engso.kmz")) %>% 
#   mutate(gml_id = "lillelund_engso", 
#          elevation = 0) %>% 
#   select(gml_id, elevation) %>% 
#   st_zm() %>% 
#   st_transform(dk_epsg)
# 
# #Read cutline layers
# fish_lake_cutlines <- st_read(paste0(getwd(), "/data_raw/fish_lake_cutlines.kmz")) %>% 
#   st_zm() %>% 
#   st_transform(dk_epsg) %>% 
#   st_union() %>% 
#   st_sfc()

# #Cut lake polygons
# dk_lakes_cut <- dk_lakes %>% 
#   filter(gml_id %in% fish_survey_with_shared_poly$gml_id) %>% 
#   st_split(fish_lake_cutlines) %>% 
#   st_collection_extract(type = "POLYGON") %>% 
#   mutate(gml_id = paste0(gml_id, "_", row_number(), "_edit"))
# 
# #Add to original data
# dk_lakes_edit <- dk_lakes %>% 
#   filter(!(gml_id %in% fish_survey_with_shared_poly$gml_id)) %>% 
#   rbind(dk_lakes_cut) %>% 
#   rbind(lillelund_engso)

#Edit coordinates for lakes where sampling point coordinates are wrong
#Coordinates of fish surveys visually inspected for mismatch between point and lake polygon
lake_id_new_coord <- read_xlsx(paste0(getwd(), "/data_raw/Fishdata with no polygon and with multiple poly.xlsx"), sheet = 1) %>% 
  na.omit() %>% 
  select(-ogc_fid, -note) %>% 
  rename(x = new_x, y = new_y) %>% 
  add_column(system = "lake")

lake_id_new_coord_zone32 <- lake_id_new_coord %>% 
  filter(zone == 32)

lake_id_new_coord_zone33_to_32 <- lake_id_new_coord %>% 
  filter(zone == 33) %>% 
  st_as_sf(coords=c("x", "y"), crs = dk_epsg+1) %>% 
  st_transform(dk_epsg) %>% 
  bind_cols(as.data.frame(st_coordinates(.))) %>% 
  st_drop_geometry() %>% 
  rename(x=X, y=Y)

#Edit coordinates in fish_species_lake_raw layers
fish_species_lakes_edit <- bind_rows(lake_id_new_coord_zone32, lake_id_new_coord_zone33_to_32) %>% 
  select(-zone) %>% 
  right_join(fish_species_lakes_raw) %>% 
  mutate(Xutm_Euref89_Zone32_cor = coalesce(x, Xutm_Euref89_Zone32),
         Yutm_Euref89_Zone32_cor = coalesce(y, Yutm_Euref89_Zone32)) %>% 
  select(-x, -y, -Xutm_Euref89_Zone32, -Yutm_Euref89_Zone32) %>% 
  filter(!(site_id %in% fish_lake_doubles))

#Write lake species data to gis database with polygon id (gml_id)
fish_species_lakes_edit_sf <- fish_species_lakes_edit %>% 
  st_as_sf(coords = c("Xutm_Euref89_Zone32_cor", "Yutm_Euref89_Zone32_cor"), crs = dk_epsg) %>% 
  st_join(dk_lakes_edit)

st_write(fish_species_lakes_edit_sf, dsn = gis_database, layer = "fish_species_lakes", delete_layer = TRUE)

#Write subset of dk_lakes polygons only including lakes with fish data
dk_lakes_subset <- dk_lakes_edit %>% 
  filter(gml_id %in% fish_species_lakes_edit_sf$gml_id) 

st_write(dk_lakes_subset, dsn = gis_database, layer = "dk_lakes_subset", delete_layer = TRUE)

#Write editted dk_lakes_layer to file 
dk_lakes_edit_with_area <- dk_lakes_edit %>% 
  mutate(area=as.numeric(st_area(geometry)))

st_write(dk_lakes_edit_with_area, dsn = gis_database, layer = "dk_lakes_edit", delete_layer = TRUE)
