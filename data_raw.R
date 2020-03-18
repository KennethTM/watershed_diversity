source("libs_and_funcs.R")

#Download files to data_raw folder:
#EU-DEM v1.1 (25 m resolution)
#Denmark polygon
#Fish monitoring data ("Sø/Fisk/Længdevægt" and "Vandløb/Fish/Længde" dataset for lakes and streams respectively)
#Danish lakes and streams
#DK-DTM 10 m (later)

#Lake macrophyte, secchi depth and chemistry??

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
fish_lake_raw <- read_xlsx(paste0(getwd(), "/data_raw/", "odaforalle_fish_lake.xlsx"))

fish_lake_species <- fish_lake_raw %>% 
  select(site_id = ObservationsStedNr, name_local = `Dansk navn`, Xutm_Euref89_Zone32, Yutm_Euref89_Zone32) %>% 
  distinct() %>% 
  mutate(system = "lake", Xutm_Euref89_Zone32 = as.numeric(Xutm_Euref89_Zone32), Yutm_Euref89_Zone32 = as.numeric(Yutm_Euref89_Zone32))

fish_stream_raw <- read_xlsx(paste0(getwd(), "/data_raw/", "odaforalle_fish_stream.xlsx"))

fish_stream_species <- fish_stream_raw %>% 
  select(site_id = ObservationsStedNr, name_local = Fiskart, 
         Xutm_Euref89_Zone32, Yutm_Euref89_Zone32) %>% 
  distinct() %>% 
  mutate(system = "stream", Xutm_Euref89_Zone32 = as.numeric(Xutm_Euref89_Zone32), Yutm_Euref89_Zone32 = as.numeric(Yutm_Euref89_Zone32))

fish_newlakes_raw <- read_xlsx(paste0(getwd(), "/data_raw/", "Samlet arter nye søer.xlsx")) %>% 
  mutate(system = "newlake", site_id = group_indices(., Lokalitetsnavn)) 
  
fish_newlakes_raw_zone32 <- fish_newlakes_raw %>% 
  filter(`UTM zone` == 32) %>% 
  select(system, site_id, Xutm_Euref89_Zone32 = x, Yutm_Euref89_Zone32 = y, name_local = Dansk_navn)

fish_newlakes_raw_zone33 <- fish_newlakes_raw %>% 
  filter(`UTM zone` == 33) 

fish_newlakes_raw_zone33_to_zone32 <- fish_newlakes_raw_zone33 %>% 
  st_as_sf(coords=c("x", "y"), crs = dk_epsg+1) %>% 
  st_transform(dk_epsg) %>% 
  bind_cols(as.data.frame(st_coordinates(.))) %>% 
  st_drop_geometry() %>% 
  select(system, site_id, Xutm_Euref89_Zone32 = X, Yutm_Euref89_Zone32 = Y, name_local = Dansk_navn)

fish_newlakes <- bind_rows(fish_newlakes_raw_zone32, fish_newlakes_raw_zone33_to_zone32)

fish_species <- bind_rows(fish_lake_species, fish_stream_species, fish_newlakes)

#Identify fish species for further analysis
fish_unique <- fish_species %>% distinct(name_local) %>% arrange(name_local)
write_csv(fish_unique, paste0(getwd(), "/data_raw/", "fish_unique.csv"))

#Create fish species and site id


#Write fish species and coordinates to gis_database

