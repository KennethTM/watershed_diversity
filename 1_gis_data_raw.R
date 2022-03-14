source("0_libs_and_funcs.R")

#Write vector files for further processing to gis_database

#Download Denmark polygon, cut Bornholm and reproject
dk_border_raw <- raster::getData("GADM", country = "DNK", level = 0, path = paste0(getwd(), "/data_raw"))

dk_border <- dk_border_raw %>% 
  st_as_sf() %>% 	
  st_crop(xmin = 8, ymin = 54.6, xmax = 14, ymax = 57.8) %>% 	
  st_transform(dk_epsg)	

st_write(dk_border, dsn = gis_database, layer = "dk_border", delete_layer = TRUE)

#Lines of ice progression during last ice age (data from GEUS)
ice_poly <- st_read(paste0(getwd(), "/data_raw/Isrande/Isrand_poly.shp")) %>% 
  slice(1) %>% 
  st_transform(dk_epsg) %>% 
  st_crop(dk_border)

st_write(ice_poly, dsn = gis_database, layer = "dk_iceage", delete_layer = TRUE)

#Create national 10 m dem (1.6 m dem originally) for basin delineation
dhym <- paste0(getwd(), "/data_raw/dhym_rain.vrt")
gdalwarp(srcfile = dhym,
         dstfile = paste0(getwd(), "/data_raw/dhym_10m.tif"),
         cutline = gis_database,
         cl = "dk_border",
         overwrite = TRUE,
         dstnodata = -9999,
         r = "min",
         co = c("COMPRESS=LZW", "BIGTIFF=YES"),
         tr = c(10, 10),
         multi = TRUE,
         wm = 4000)

#Calculate DEM slopes (percent)
gdaldem("slope",
        paste0(getwd(), "/data_raw/dhym_10m.tif"),
        paste0(getwd(), "/data_raw/dhym_10m_slope.tif"),
        compute_edges = TRUE,
        co = "COMPRESS=LZW",
        p=TRUE)

#Reproject, make 2d and clean stream vector layer, add to gis_database
dk_streams_raw <- st_read(paste0(getwd(), "/data_raw/DK_WatercourseLink.gml"))

dk_streams_clean <- dk_streams_raw %>% 
  st_zm() %>% 
  select(gml_id) %>% 
  st_transform(dk_epsg) %>% 
  mutate(stream_length_m = as.numeric(st_length(centrelineGeometry))) %>% 
  st_crop(dk_border)

st_write(dk_streams_clean, dsn = gis_database, layer = "dk_streams", delete_layer = TRUE)

#Read, clean and edit lake vector layer, add to gis_database
dk_lakes_raw <- st_read(paste0(getwd(), "/data_raw/DK_StandingWater.gml"))

dk_lakes_clean <- dk_lakes_raw %>% 
  st_zm() %>% 
  select(gml_id, elevation) %>% 
  st_transform(dk_epsg) %>% 
  st_crop(dk_border)

#Add also a missing polygon to national lake polygon layer (OpenStreetMap)
lillelund_engso <- st_read(paste0(getwd(), "/data_raw/lillelund_engso.kmz")) %>% 
  mutate(gml_id = "lillelund_engso", elevation = 0) %>% 
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
dk_lakes_cut <- dk_lakes_clean %>% 
  rbind(lillelund_engso) %>% 
  st_split(fish_lake_cutlines) %>% 
  st_collection_extract(type = "POLYGON") %>% 
  mutate(gml_id = paste0(gml_id, "_", row_number(), "_edit"),
         lake_area_m2 = as.numeric(st_area(geometry)),
         shoreline_m = as.numeric(st_length(st_cast(geometry, "MULTILINESTRING")))) %>% 
  rename(lake_elev_m = elevation)

st_write(dk_lakes_cut, dsn = gis_database, layer = "dk_lakes", delete_layer = TRUE)

#CLC 2012
clc_legend <- read.csv(paste0(getwd(), "/data_raw/DK_CORINE_SHP_UTM32-WGS84/clc_legend.csv"), colClasses = "character")

clc <- st_read(paste0(getwd(), "/data_raw/DK_CORINE_SHP_UTM32-WGS84/CLC12_DK.shp")) %>%
  st_transform(dk_epsg) %>%
  left_join(clc_legend, by = c("CODE_12"="CLC_CODE")) %>%
  set_names(str_to_lower(names(.))) %>%
  mutate(clc_code = parse_number(as.character(code_12)))

st_write(clc, gis_database, layer = "corine_land_cover", delete_layer = TRUE)
