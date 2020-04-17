source("libs_and_funcs.R")

#Download files to data_raw folder:
#EU-DEM v1.1 (25 m resolution)
#Denmark polygon
#Danish lakes and streams polygon/line layers
#Fish monitoring data ("Sø/Fisk/Længdevægt" and "Vandløb/Fisk/Længde" on www.odaforalle.dk dataset for lakes and streams respectively)
#Lake chemistry and secchi depths
#Lake submerged macrophyte data

#Write vector files for further processing to gis_database

#Download Denmark polygon, cut Bornholm and reproject
dk_border_raw <- getData("GADM", country = "DNK", level = 0, path = paste0(getwd(), "/data_raw"))

dk_border <- dk_border_raw %>% 
  st_as_sf() %>% 	
  st_crop(xmin = 8, ymin = 54.56, xmax = 14, ymax = 57.76) %>% 	
  st_transform(dk_epsg)	

st_write(dk_border, dsn = gis_database, layer = "dk_border", delete_layer = TRUE)#, dataset_options = "SPATIALITE=YES"

#Reproject and cut EU-DEM using dk_poly
#Cut and reproject
gdalwarp(srcfile = paste0(getwd(), "/data_raw/eu_dem_v11_E40N30/eu_dem_v11_E40N30.TIF"), 
         dstfile = paste0(getwd(), "/data_processed/dk_dem_25.tif"),
         co = "COMPRESS=LZW", 
         tr = c(25, 25), 
         tap = TRUE,
         r = "bilinear", 
         dstnodata = -9999,
         cutline = gis_database,
         cl = "dk_border",
         t_srs = paste0("EPSG:", dk_epsg),
         crop_to_cutline = TRUE,
         overwrite = TRUE)

#Reproject, make 2d and clean lake and stream vector layers. Add to gis_database
ogr2ogr(paste0(getwd(), "/data_raw/DK_StandingWater.gml"),
        gis_database,
        nln = "dk_lakes",
        update = TRUE,
        overwrite = TRUE,
        t_srs = paste0("EPSG:", dk_epsg),
        dim = 2,
        select = "gml_id, elevation",
        lco = "geometry_name=GEOMETRY")

ogr2ogr(paste0(getwd(), "/data_raw/DK_WatercourseLink.gml"),
        gis_database,
        nln = "dk_streams",
        update = TRUE,
        overwrite = TRUE,
        t_srs = paste0("EPSG:", dk_epsg),
        dim = 2,
        select = "gml_id")

#Read downloaded fish data, clean and save to gis_database
#Format to columns: system, site_id, year, Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, name_local
#For lake fish monitoring both weigth and net tables are needed to also include zero-catch samples
fish_weight_lake <- read_xlsx(paste0(getwd(), "/data_raw/", "odaforalle_fish_weight_lake.xlsx"))
fish_net_lake <- read_xlsx(paste0(getwd(), "/data_raw/", "odaforalle_fish_net_lake.xlsx"))

fish_lake_species_raw <- left_join(fish_net_lake, fish_weight_lake) %>%
  mutate(system = "lake", date = ymd(Dato), year = year(date),
         Xutm_Euref89_Zone32 = as.numeric(Xutm_Euref89_Zone32), Yutm_Euref89_Zone32 = as.numeric(Yutm_Euref89_Zone32)) %>% 
  select(system, site_id = ObservationsStedNr, year,
         Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, name_novana = `Dansk navn`) %>% 
  distinct() %>% 
  filter(!is.na(Xutm_Euref89_Zone32))

keep_na_for_zero_catch <- function(spec_col){
  if(all(is.na(spec_col[[1]]))){
    df <- tibble(col_name = NA)
    names(df) <- names(spec_col)
    return(df)
  }else{
    return(na.omit(spec_col))
  }
}

fish_lake_species <- fish_lake_species_raw %>% 
  nest(data = c("name_novana")) %>% 
  mutate(na_if_zerocatch = map(data, keep_na_for_zero_catch)) %>% 
  select(-data) %>% 
  unnest(na_if_zerocatch)

#Stream fish, used to define basin species pool
fish_stream_raw <- read_xlsx(paste0(getwd(), "/data_raw/", "odaforalle_fish_stream.xlsx"))
fish_stream_species <- fish_stream_raw %>% 
  mutate(system = "stream", date = ymd(Dato), year = year(date),
         Xutm_Euref89_Zone32 = as.numeric(Xutm_Euref89_Zone32), Yutm_Euref89_Zone32 = as.numeric(Yutm_Euref89_Zone32)) %>% 
  select(system, site_id = ObservationsStedNr, year,
         Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, name_novana = Fiskart) %>% 
  distinct() %>% 
  na.omit()

#Fish in new lakes
fish_newlakes_raw <- read_xlsx(paste0(getwd(), "/data_raw/", "Samlet arter nye søer.xlsx")) %>% 
  mutate(system = "newlake", site_id = group_indices(., Lokalitetsnavn))
  
fish_newlakes_raw_zone32 <- fish_newlakes_raw %>% 
  filter(`UTM zone` == 32)

fish_newlakes_raw_zone33 <- fish_newlakes_raw %>% 
  filter(`UTM zone` == 33) 

fish_newlakes_raw_zone33_to_zone32 <- fish_newlakes_raw_zone33 %>% 
  st_as_sf(coords=c("x", "y"), crs = dk_epsg+1) %>% 
  st_transform(dk_epsg) %>% 
  bind_cols(as.data.frame(st_coordinates(.))) %>% 
  st_drop_geometry() %>% 
  rename(x=X, y=Y)

fish_newlakes <- bind_rows(fish_newlakes_raw_zone32, fish_newlakes_raw_zone33_to_zone32) %>% 
  select(system, site_id, year_established = established,
         Xutm_Euref89_Zone32 = x, Yutm_Euref89_Zone32 = y, name_novana = Dansk_navn) %>% 
  distinct() %>% 
  mutate(year = 2018) #Avg year of sampling in new lakes

fish_species <- bind_rows(fish_lake_species, fish_stream_species, fish_newlakes)

#Identify fish species for further analysis
#Valid/invalid fish species are edited manually and fish_id columns are added 
#Editted list saved as "fish_unique_edit_final.xlsx"
#fish_unique <- fish_species %>% distinct(name_local) %>% arrange(name_local) %>% na.omit()
#write_csv(fish_unique, paste0(getwd(), "/data_raw/", "fish_unique.csv"))

#Select fish species for further analysis
#Create fish species id
#Actions: 0=do_nothing, 1=remove_species, 2=remove_lake (brackish)
fish_unique_edit <- read_xlsx(paste0(getwd(), "/data_raw/", "fish_unique_edit_final.xlsx")) %>% 
  select(name = name_to_use, name_novana = name_local_novana, name_atlas = latin_and_atlas,
         fish_id = ID, action = `how(0=do_nothing)(1=remove_species)(2=remove_lake)`) %>% 
  select(-name_atlas)
  
invalid_lakes <- fish_species %>% 
  filter(name_novana %in% filter(fish_unique_edit, action == 2)$name_novana) %>% 
  pull(site_id) %>% 
  unique()

valid_fish_species <- fish_unique_edit %>% 
  filter(action == 0 & !is.na(name_novana))

#Join to table with species id's, keep valid fish species and NA
fish_species_sub <- fish_species %>% 
  left_join(valid_fish_species) %>% 
  filter(name_novana %in% c(valid_fish_species[valid_fish_species$action == 0,]$name_novana, NA)) %>% 
  filter(!(site_id %in% invalid_lakes)) %>% 
  select(-name_novana, -year_established, -action)

#Basin fish species pool is all species in streams and lakes found since 1990
fish_species_basin <- fish_species_sub %>% 
  filter(year >= 1990) %>% 
  select(-year) %>% 
  na.omit() %>% 
  distinct()

#Write basin species data to gis database
fish_species_basin_sf <- fish_species_basin %>% 
  st_as_sf(coords = c("Xutm_Euref89_Zone32", "Yutm_Euref89_Zone32"), crs = dk_epsg)

st_write(fish_species_basin_sf, dsn = gis_database, layer = "fish_species_basin", delete_layer = TRUE)

#Lakes for investigation is all the latest sampling for each lakes after 2006
fish_species_latest_survey <- fish_species_sub %>% 
  filter(system %in% c("lake", "newlake")) %>% 
  select(system, site_id, year) %>% 
  group_by(system, site_id) %>% 
  summarise(year_max = max(year)) %>% 
  ungroup() %>% 
  filter(year_max >= 2006)

#site_id's to be exclude
#Two double sampling from the same lake
#Different sampling in same lake (Utterslev Mose)
fish_lake_doubles <- c(c(14000045, 20000218),
                       c(53000046, 53000047))

#Lakes for investigation
fish_species_lakes_raw <- fish_species_latest_survey %>% 
  rename(year = year_max) %>% 
  left_join(fish_species_sub) %>% 
  distinct() %>% 
  filter(!(site_id %in% fish_lake_doubles))

#Write lake raw species data to gis database
fish_species_lakes_raw_sf <- fish_species_lakes_raw %>% 
  st_as_sf(coords = c("Xutm_Euref89_Zone32", "Yutm_Euref89_Zone32"), crs = dk_epsg)

st_write(fish_species_lakes_raw_sf, dsn = gis_database, layer = "fish_species_lakes_raw", delete_layer = TRUE)

#Write files with with problems
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

#Add also a missing polygon to national lake polygon layer (OpenStreetMap)
lillelund_engso <- st_read(paste0(getwd(), "/data_raw/","lillelund_engso.kmz")) %>% 
  mutate(gml_id = "lillelund_engso", 
         elevation = 0) %>% 
  select(gml_id, elevation) %>% 
  st_zm() %>% 
  st_transform(dk_epsg)

#Read cutline layers
fish_lake_cutlines <- st_read(paste0(getwd(), "/data_raw/fish_lake_cutlines.kmz")) %>% 
  st_zm() %>% 
  st_transform(dk_epsg) %>% 
  st_union() %>% 
  st_sfc()

#Cut lake polygons
dk_lakes_cut <- dk_lakes %>% 
  filter(gml_id %in% fish_survey_with_shared_poly$gml_id) %>% 
  st_split(fish_lake_cutlines) %>% 
  st_collection_extract(type = "POLYGON") %>% 
  mutate(gml_id = paste0(gml_id, "_", row_number(), "_edit"))

#Add to original data
dk_lakes_edit <- dk_lakes %>% 
  filter(!(gml_id %in% fish_survey_with_shared_poly$gml_id)) %>% 
  rbind(dk_lakes_cut) %>% 
  rbind(lillelund_engso)

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

#Secchi depth and chemistry data
#lake id's in secchi and chemistry data do not match that of fish data
lake_secchi_raw <- read_xlsx(paste0(getwd(), "/data_raw/", "odaforalle_secchi_lake.xlsx"))
lake_chem_raw <- read_xlsx(paste0(getwd(), "/data_raw/", "odaforalle_chemistry_lake.xlsx"))

#Combine data, average across depth and calculate weighted mean across year
lake_secchi_chem <- bind_rows(lake_chem_raw, lake_secchi_raw) %>% 
  mutate(system = "lake", date = ymd(Startdato), year = year(date),
         Xutm_Euref89_Zone32 = as.numeric(coalesce(Xutm_Euref89_Zone32, Xutm_Euref89_Zone32_ZONE32)), 
         Yutm_Euref89_Zone32 = as.numeric(coalesce(Yutm_Euref89_Zone32, Yutm_Euref89_Zone32_ZONE32)),
         Parameter_cor = ifelse(Parameter == "Chlorophyl (ukorrigeret)", "Chlorophyl A", Parameter),
         var_unit = paste0(Parameter_cor, "_", Enhed),
         avg_sample_depth_m_chem = parse_number(`GennemsnitsDybde i m`, locale = locale(decimal_mark = ",")),
         avg_sample_depth_m = ifelse(is.na(avg_sample_depth_m_chem), 0, avg_sample_depth_m_chem)) %>% 
  select(system, name = ObservationsStedNavn, site_id_2 = ObservationsStedNr, date, year, Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, 
         avg_sample_depth_m, var_unit, value = Resultat) %>% 
  group_by(system, name, site_id_2, Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, year, var_unit, date) %>% 
  summarise(value_mean = mean(value)) %>% 
  spread(var_unit, value_mean) %>% 
  rename(alk_mmol_l = `Alkalinitet,total TA_mmol/l`, chla_ug_l = `Chlorophyl A_µg/l`, tn_mg_l = `Nitrogen,total N_mg/l`, 
         ph_ph = pH_pH, tp_mg_l = `Phosphor, total-P_mg/l`, secchi_depth_m = Sigtdybde_m) %>% 
  filter(between(secchi_depth_m, 0, 20),
         between(chla_ug_l, 0, 500),
         between(alk_mmol_l, 0, 15),
         between(tn_mg_l, 0, 50),
         between(tp_mg_l, 0, 50),
         between(ph_ph, 2, 12),
         Yutm_Euref89_Zone32 > 700000,
         year >= 2006) %>% 
  group_by(system, name, site_id_2, Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, year) %>% 
  summarise_at(vars(alk_mmol_l, chla_ug_l, tn_mg_l, ph_ph, tp_mg_l, secchi_depth_m),
               ~weighted.mean(., w = yday(date))) %>%
  ungroup() 

lake_secchi_chem_sf <- lake_secchi_chem %>% 
  st_as_sf(coords = c("Xutm_Euref89_Zone32", "Yutm_Euref89_Zone32"), crs = dk_epsg)

#Write raw secchi_chem stations to .sqlite file and align coordinates
# lake_secchi_chem_sf %>%
#   select(site_id_2, name) %>%
#   distinct(.keep_all = TRUE) %>%
#   st_write(paste0(getwd(), "/data_raw/lake_secchi_chem_raw.sqlite"), delete_dsn = TRUE)

st_write(lake_secchi_chem_sf, dsn = gis_database, layer = "lake_secchi_chem", delete_layer = TRUE)

#Lake submerged macrophyte and mean depth data
#Downloaded from miljøportalen, not all open as odaforalle
sv_files <- list.files(paste0(getwd(), "/data_raw/veg_data"), full.names = TRUE)
names(sv_files) <- str_sub(basename(sv_files), end = -5)

#Load all downloaded files
sv_list1 <- lapply(sv_files[-c(7, 8, 12)], function(path){read_table(path, locale = locale(decimal_mark = ",", encoding = "ISO-8859-1"))})
sv_list2 <- lapply(sv_files[c(7, 8, 12)], function(path){read_table2(path, locale = locale(decimal_mark = ",", encoding = "ISO-8859-1"))})
sv_list <- c(sv_list1, sv_list2)

#Join nescessary files
lake_plants_raw <- sv_list$sv_bertotal %>% 
  select(-X10) %>% 
  left_join(select(sv_list$std_enhed, enhed_std = kode, enhed = betegn)) %>% 
  left_join(select(sv_list$sv_berparam, param_std = kode, param = betegn)) %>% 
  left_join(sv_list$sv_dybdeomr) %>% 
  left_join(sv_list$sv_tilsyn2) %>% 
  filter(dybdeomr_nr == 0 & bertype == 0) %>% 
  mutate(system = "lake", date = dmy(startdato), year = year(date),
         param_unit = paste0(param, "_", enhed)) %>% 
  select(system, recopl_id, year, dybdeomr_id, resultat, param_unit) %>% 
  spread(param_unit, resultat) %>% 
  left_join(select(sv_list$so_cstation3, navn, recopl_id, utm_zone, utm_x, utm_y))

lake_plants <- lake_plants_raw %>% 
  mutate(zmean_m = Søvolumen_m3/Søreal_m2) %>% 
  select(system, year, site_id_3 = recopl_id, name = navn, utm_zone, utm_x, utm_y, area_m2 = Søreal_m2,
         zmean_m, volume_m3 = Søvolumen_m3, mean_plant_height_m = `Middel plantehøjde_m`,
         plant_area_m2 = `Plantedækket areal_m2`, plant_vol_m3 = `Plantefyldt volumen_m3`,
         plant_area_perc = `Relativ plantedækket areal_pct.`, plant_vol_perc = `Relativ plantefyldt volumen_pct.`) %>% 
  distinct()

#Convert all coords to utm zone 32
lake_plants_zone32 <- lake_plants %>% 
  filter(utm_zone == "U32")

lake_plants_zone33_to_32 <- lake_plants %>% 
  filter(utm_zone == "U33") %>% 
  st_as_sf(coords=c("utm_x", "utm_y"), crs = dk_epsg+1) %>% 
  st_transform(dk_epsg) %>% 
  bind_cols(as.data.frame(st_coordinates(.))) %>% 
  st_drop_geometry() %>% 
  rename(utm_x=X, utm_y=Y)

#Convert to spatial and write to database
lake_plants_sf <- bind_rows(lake_plants_zone32, lake_plants_zone33_to_32) %>% 
  select(-utm_zone) %>% 
  filter(between(utm_x, 4*10^5, 9*10^5),
         between(utm_y, 6010000, 6400000)) %>% 
  st_as_sf(coords = c("utm_x", "utm_y"), crs = dk_epsg)

# lake_plants_sf %>%
#   select(site_id_3, name) %>%
#   distinct(.keep_all = TRUE) %>%
#   st_write(paste0(getwd(), "/data_raw/lake_plants_raw.sqlite"), delete_dsn = TRUE)

st_write(lake_plants_sf, dsn = gis_database, layer = "lake_plants", delete_layer = TRUE)
