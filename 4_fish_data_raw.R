source("0_libs_and_funcs.R")

dk_border <- st_read(gis_database, layer = "dk_border")

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

write_csv(chem_newlakes, paste0(getwd(), "/data_processed/chem_newlakes.csv"))

#Fish species from both lakes and streams monitoring data and new lakes
fish_species <- bind_rows(fish_lake_species, fish_stream_species, fish_newlakes)

#Unique species found in raw data
fish_species_unique <- fish_species %>%
  distinct(name_danish) %>%
  arrange(name_danish) %>%
  na.omit()

#Create fish species id and remove hybrids, exotic species or non-freshwater species
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
         between(year_sample, 1990, 2020)) %>% 
  filter(name_danish %in% fish_species_valid$name_danish | is.na(name_danish)) %>% 
  left_join(fish_species_valid[, c("name_danish", "fish_id")])

#Write species data to gis database
#Used to determine basin species richness
fish_species_sub_sf <- fish_species_sub %>% 
  st_as_sf(coords = c("Xutm_Euref89_Zone32", "Yutm_Euref89_Zone32"), crs = dk_epsg) %>% 
  st_crop(dk_border)

st_write(fish_species_sub_sf, dsn = gis_database, layer = "fish_species_basin", delete_layer = TRUE)

#Read atlas basin fish data
atlas_raw <- read_csv(paste0(getwd(), "/data_raw/", "atlas_basin_data.csv")) %>% 
  na.omit()

atlas_clean <- atlas_raw %>% 
  mutate(name_atlas = gsub(" ", "_", name_atlas)) %>% 
  left_join(fish_species_unique_edit[, c("name_atlas", "fish_id", "action")]) %>%
  filter(action == 0 & !is.na(fish_id)) %>% 
  select(-action)

write_csv(atlas_clean, paste0(getwd(), "/data_processed/atlas_clean.csv"))

#Data for Table S1 data
table_s1_monitoring <- fish_species |> 
  select(name_danish, x=Xutm_Euref89_Zone32, y = Yutm_Euref89_Zone32) |>
  na.omit() |> 
  st_as_sf(crs=25832, coords=c("x", "y")) |> 
  st_join(basins[,c("basin_id")]) |> 
  st_drop_geometry() |>
  na.omit() |> 
  distinct() |> 
  arrange(name_danish) |> 
  rename(name = name_danish)

#Write to file and match names in monitoring data and atlas data
atlas_raw |> 
  arrange(name_atlas) |> 
  rename(name = name_atlas) |> 
  bind_rows(table_s1_monitoring) |> 
  write_csv("figures/table_s1_step_1.csv")

#Table S1 continued
table_s1_clean <- read_excel("figures/table_s1_revised.xlsx")

table_s1_clean |> 
  distinct() |> 
  group_by(name) |> 
  summarise(pct_basin = n()/894*100) |> 
  write_csv("figures/table_s1_step_2.csv")

#Lakes for investigation is year with highest richness after 2006
fish_species_richest_survey <- fish_species_sub %>% 
  filter(year_sample >= 2006,
         system == "lake") %>% 
  group_by(system, site_id, year_sample) %>% 
  summarise(n_spec = ifelse(all(is.na(fish_id)), 0, length(unique(fish_id)))) %>% 
  summarise(year_max_rich = year_sample[which.max(n_spec)])  %>% 
  ungroup()

#site_id's to be exclude
#Two double sampling from the same lake
#Different sampling in same lake (Utterslev Mose)
fish_lake_doubles <- c(c(14000045, 20000218),
                       c(53000046, 53000047))

#Lakes for investigation
fish_species_lakes_raw <- fish_species_richest_survey %>% 
  rename(year_sample = year_max_rich) %>% 
  left_join(fish_species_sub) %>% 
  filter(!(site_id %in% fish_lake_doubles))

#Write lake raw species data to gis database
fish_species_lakes_raw_sf <- fish_species_lakes_raw %>% 
  st_as_sf(coords = c("Xutm_Euref89_Zone32", "Yutm_Euref89_Zone32"), crs = dk_epsg) %>% 
  st_crop(dk_border)

st_write(fish_species_lakes_raw_sf, dsn = gis_database, layer = "fish_species_lakes_raw", delete_layer = TRUE)

#Write files with with problems which should be reviewed and fixed manually
#Multiple fish surveys in same lake polygon
#Missing lake polygons
#Notes are in "files_to_fix_notes.xlsx"
#Use raw fish data to identify fish site/lake polygon problems
dk_lakes <- st_read(dsn = gis_database, layer = "dk_lakes")

#Lakes with fish data but no lake polygon
fish_species_lakes_raw_sf %>% 
  select(system, site_id) %>% 
  distinct(.keep_all = TRUE) %>% 
  st_join(dk_lakes) %>% 
  filter(is.na(gml_id)) %>%  
  st_write(paste0(getwd(), "/data_raw/files_to_fix/fish_data_with_no_polygon.sqlite"), delete_dsn = TRUE)

#Some polygons are shared between site_id's
fish_survey_with_shared_poly <- fish_species_lakes_raw_sf %>% 
  select(system, site_id) %>% 
  distinct(.keep_all = TRUE) %>% 
  st_join(dk_lakes, left = FALSE) %>% 
  group_by(gml_id) %>% 
  add_tally() %>% 
  filter(n > 1)

fish_survey_with_shared_poly %>% 
  st_write(paste0(getwd(), "/data_raw/files_to_fix/fish_data_with_more_than_one_polygon.sqlite"), delete_dsn = TRUE) 

#Use gml_id for polygons which contain multiple fish surveys
#Save and create cutline layer in google earth
dk_lakes %>% 
  filter(gml_id %in% fish_survey_with_shared_poly$gml_id) %>% 
  st_write(paste0(getwd(), "/data_raw/files_to_fix/polygons_with_multiple_fish_surveys.kml"), delete_dsn = TRUE) 

#Edit coordinates for lakes where sampling point coordinates are wrong
#Coordinates of fish surveys visually inspected for mismatch between point and lake polygon
lake_id_new_coord <- read_xlsx(paste0(getwd(), "/data_raw/files_to_fix_notes.xlsx"), sheet = 1) %>% 
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
  mutate(site_id = as.character(site_id)) %>% 
  right_join(fish_species_lakes_raw) %>% 
  mutate(Xutm_Euref89_Zone32_cor = coalesce(x, Xutm_Euref89_Zone32),
         Yutm_Euref89_Zone32_cor = coalesce(y, Yutm_Euref89_Zone32)) %>% 
  select(-x, -y, -Xutm_Euref89_Zone32, -Yutm_Euref89_Zone32)

#Write lake species data to gis database with polygon id (gml_id)
fish_species_lakes_edit_sf <- fish_species_lakes_edit %>% 
  st_as_sf(coords = c("Xutm_Euref89_Zone32_cor", "Yutm_Euref89_Zone32_cor"), crs = dk_epsg) %>% 
  st_join(dk_lakes) %>% 
  st_crop(dk_border)

st_write(fish_species_lakes_edit_sf, dsn = gis_database, layer = "fish_species_lakes", delete_layer = TRUE)

#Write subset of dk_lakes polygons only including lakes with fish data
dk_lakes_subset <- dk_lakes %>% 
  filter(gml_id %in% fish_species_lakes_edit_sf$gml_id) 

st_write(dk_lakes_subset, dsn = gis_database, layer = "dk_lakes_subset", delete_layer = TRUE)
