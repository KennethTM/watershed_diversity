source("libs_and_funcs.R")

#Download files to data_raw folder:
#EU-DEM v1.1 (25 m resolution)
#Denmark polygon
#Fish monitoring data ("Sø/Fisk/Længdevægt" and "Vandløb/Fish/Længde" dataset for lakes and streams respectively)
#Danish lakes and streams
#DK-DTM 10 m (later, if finer scale watershed delineation is needed)

#Write vector files for further processing to gis_database

#Download Denmark polygon, cut Bornholm and reproject
dk_border_raw <- getData("GADM", country = "DNK", level = 0, path = paste0(getwd(), "/data_raw"))

dk_border <- dk_border_raw %>% 
  st_as_sf() %>% 
  st_crop(xmin = 8, ymin = 54.56, xmax = 14, ymax = 57.76) %>% 
  st_transform(dk_epsg)

st_write(dk_border, dsn = gis_database, layer = "dk_border", delete_layer = TRUE)

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
        select = "gml_id,elevation")

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
         Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, name_local = `Dansk navn`) %>% 
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
  nest(data = c("name_local")) %>% 
  mutate(na_if_zerocatch = map(data, keep_na_for_zero_catch)) %>% 
  select(-data) %>% 
  unnest(na_if_zerocatch)

# #Count number of fish caught for each sampling
# fish_lake_species_count <- fish_lake_species %>% 
#   group_by(system, site_id, Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, year) %>% 
#   summarise(n_spec = ifelse(any(is.na(name_local)), 0, length(unique(name_local)))) %>% 
#   ungroup()

#Stream fish, used to define basin species pool
fish_stream_raw <- read_xlsx(paste0(getwd(), "/data_raw/", "odaforalle_fish_stream.xlsx"))
fish_stream_species <- fish_stream_raw %>% 
  mutate(system = "stream", date = ymd(Dato), year = year(date),
         Xutm_Euref89_Zone32 = as.numeric(Xutm_Euref89_Zone32), Yutm_Euref89_Zone32 = as.numeric(Yutm_Euref89_Zone32)) %>% 
  select(system, site_id = ObservationsStedNr, year,
         Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, name_local = Fiskart) %>% 
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
         Xutm_Euref89_Zone32 = x, Yutm_Euref89_Zone32 = y, name_local = Dansk_navn) %>% 
  distinct() %>% 
  mutate(year = 2018) #Avg year of sampling in new lakes

fish_species <- bind_rows(fish_lake_species, fish_stream_species, fish_newlakes)

#Identify fish species for further analysis
fish_unique <- fish_species %>% distinct(name_local) %>% arrange(name_local) %>% na.omit()
write_csv(fish_unique, paste0(getwd(), "/data_raw/", "fish_unique.csv"))

#Select fish species for further analysis
#Create fish species id
fish_unique_edit <- read_xlsx(paste0(getwd(), "/data_raw/", "fish_unique_edit.xlsx")) %>% 
  filter(keep == "y") %>% 
  select(name_local, fish_id = ID)

#Join to table with species id's, keep "y" fish and NA
fish_species_sub <- fish_species %>% 
  left_join(fish_unique_edit) %>% 
  filter(name_local %in% c(fish_unique_edit$name_local, NA)) %>% 
  select(-name_local, -year_established)

#Basin fish species pool is all species in streams and lakes found since 1990
fish_species_basin <- fish_species_sub %>% 
  filter(year > 1990) %>% 
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

#Lakes for further investigation
fish_species_lakes <- fish_species_latest_survey %>% 
  left_join(fish_species_sub, by = c("system" = "system", "site_id" = "site_id", "year_max" = "year")) %>% 
  rename(year = year_max) %>% 
  distinct()
  
#Write basin species data to gis database
fish_species_lakes_sf <- fish_species_lakes %>% 
  st_as_sf(coords = c("Xutm_Euref89_Zone32", "Yutm_Euref89_Zone32"), crs = dk_epsg)

st_write(fish_species_lakes_sf, dsn = gis_database, layer = "fish_species_lakes", delete_layer = TRUE)

#Secchi depth data 
lake_secchi <- read_xlsx(paste0(getwd(), "/data_raw/", "odaforalle_secchi_lake.xlsx")) %>% 
  mutate(system = "lake", date = ymd(Startdato), year = year(date),
         Xutm_Euref89_Zone32 = as.numeric(Xutm_Euref89_Zone32_ZONE32), 
         Yutm_Euref89_Zone32 = as.numeric(Yutm_Euref89_Zone32_ZONE32)) %>% 
  select(system, site_id = ObservationsStedNr, year,
         Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, secchi_depth_m = Resultat) %>% 
  filter(!is.na(Xutm_Euref89_Zone32),
         Yutm_Euref89_Zone32 > 700000,
         secchi_depth_m < 20,
         year >= 2006) #Remove one errornous value 

lake_secchi_sf <- lake_secchi %>% 
  st_as_sf(coords = c("Xutm_Euref89_Zone32", "Yutm_Euref89_Zone32"), crs = dk_epsg)

st_write(lake_secchi_sf, dsn = gis_database, layer = "lake_secchi", delete_layer = TRUE)

#Chemistry data
#Consider filter more on outliers in vars e.g. alkalinity
lake_chem <- read_xlsx(paste0(getwd(), "/data_raw/", "odaforalle_chemistry_lake.xlsx")) %>% 
  mutate(system = "lake", date = ymd(Startdato), year = year(date),
         Xutm_Euref89_Zone32 = as.numeric(Xutm_Euref89_Zone32), 
         Yutm_Euref89_Zone32 = as.numeric(Yutm_Euref89_Zone32),
         Parameter_cor = ifelse(Parameter == "Chlorophyl (ukorrigeret)", "Chlorophyl A", Parameter),
         var_unit = paste0(Parameter_cor, "_", Enhed),
         avg_sample_depth_m = parse_number(`GennemsnitsDybde i m`, locale = locale(decimal_mark = ","))) %>% 
  select(system, site_id = ObservationsStedNr, date, year, Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, 
         avg_sample_depth_m, var_unit, value = Resultat) %>% 
  group_by(system, site_id, date, year, Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, var_unit) %>% #Average across depth
  summarise(value_mean = mean(value)) %>% 
  spread(var_unit, value_mean) %>% 
  rename(alk_mmol_l = `Alkalinitet,total TA_mmol/l`, chla_ug_l = `Chlorophyl A_µg/l`, 
         tn_mg_l = `Nitrogen,total N_mg/l`, ph_ph = pH_pH, tp_mg_l = `Phosphor, total-P_mg/l`) %>% 
  group_by(system, site_id, Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, year) %>% 
  summarise_at(vars(alk_mmol_l, chla_ug_l, tn_mg_l, ph_ph, tp_mg_l),
               ~weighted.mean(., w = yday(date), na.rm = TRUE)) %>% #Weighted mean across year
  ungroup() %>% 
  filter(Yutm_Euref89_Zone32 > 700000,
         year >= 2006)

lake_chem_sf <- lake_chem %>% 
  st_as_sf(coords = c("Xutm_Euref89_Zone32", "Yutm_Euref89_Zone32"), crs = dk_epsg)

st_write(lake_chem_sf, dsn = gis_database, layer = "lake_chem", delete_layer = TRUE)







#Lake macrophyte??
