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
fish_lake_raw <- read_xlsx(paste0(getwd(), "/data_raw/", "odaforalle_fish_lake.xlsx"))

fish_lake_species <- fish_lake_raw %>% 
  mutate(system = "lake", date = ymd(Dato), year = year(date),
         Xutm_Euref89_Zone32 = as.numeric(Xutm_Euref89_Zone32), Yutm_Euref89_Zone32 = as.numeric(Yutm_Euref89_Zone32)) %>% 
  select(system, site_id = ObservationsStedNr, year,
         Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, name_local = `Dansk navn`) %>% 
  distinct() %>% 
  filter(!is.na(Xutm_Euref89_Zone32))

fish_stream_raw <- read_xlsx(paste0(getwd(), "/data_raw/", "odaforalle_fish_stream.xlsx"))

fish_stream_species <- fish_stream_raw %>% 
  mutate(system = "stream", date = ymd(Dato), year = year(date),
         Xutm_Euref89_Zone32 = as.numeric(Xutm_Euref89_Zone32), Yutm_Euref89_Zone32 = as.numeric(Yutm_Euref89_Zone32)) %>% 
  select(system, site_id = ObservationsStedNr, year,
         Xutm_Euref89_Zone32, Yutm_Euref89_Zone32, name_local = Fiskart) %>% 
  distinct()

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

fish_species <- bind_rows(fish_lake_species, fish_stream_species, fish_newlakes) %>% 
  mutate(name_local = ifelse(is.na(name_local), "none", name_local))

#Identify fish species for further analysis
fish_unique <- fish_species %>% distinct(name_local) %>% arrange(name_local)
write_csv(fish_unique, paste0(getwd(), "/data_raw/", "fish_unique.csv"))

#Select fish species for further analysis
#Create fish species id
fish_unique_edit <- read_xlsx(paste0(getwd(), "/data_raw/", "fish_unique_edit.xlsx")) %>% 
  filter(keep == "y") %>% 
  select(name_local, fish_id = ID)

#Join to table
fish_species_sub <- fish_species %>% 
  right_join(fish_unique_edit) %>% 
  select(-name_local, -year_established)

#Basin fish species pool is all species in streams and lakes found since 1990
fish_species_basin <- fish_species_sub %>% 
  filter(year > 1990) %>% 
  select(-year) %>% 
  distinct()

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
  select(-year_max) %>% 
  distinct()
  
#Write basin and lake fish species to gis_database
fish_species_basin_sf <- fish_species_basin %>% 
  st_as_sf(coords = c("Xutm_Euref89_Zone32", "Yutm_Euref89_Zone32"), crs = dk_epsg)

st_write(fish_species_basin_sf, dsn = gis_database, layer = "fish_species_basin", delete_layer = TRUE)

fish_species_lakes_sf <- fish_species_lakes %>% 
  st_as_sf(coords = c("Xutm_Euref89_Zone32", "Yutm_Euref89_Zone32"), crs = dk_epsg)

st_write(fish_species_lakes_sf, dsn = gis_database, layer = "fish_species_lakes", delete_layer = TRUE)


#Lake macrophyte, secchi depth and chemistry?? - load, clean and write to gis_database
