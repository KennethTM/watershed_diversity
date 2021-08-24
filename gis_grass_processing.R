source("libs_and_funcs.R")

dhym_10m <- paste0(getwd(), "/data_raw/dhym_10m.tif")

dhym_raster <- raster(dhym_10m)

#Find grass install
grass_path <- findGRASS()

linkGRASS7(dhym_raster, default_GRASS7 = grass_path, 
           gisdbase = paste0(getwd(), "/gis_grass_database"),
           location = "dhym_10m", gisdbase_exist = TRUE)

#Import dem
execGRASS("r.in.gdal", flags = c("overwrite", "o"), parameters = list(input = dhym_10m, output = "dem"))

#Extract streams and flow directions using r.watershed
execGRASS("r.watershed", flags = c("overwrite", "b", "s", "a", "m"),
          parameters = list(elevation = "dem",
                            stream = "dem_stream",
                            accumulation="dem_acc",
                            drainage = "dem_drain",
                            threshold = 10^4,
                            memory = 16000))

#Determine major watersheds using r.stream.basins
execGRASS("r.stream.basins", flags = c("overwrite", "l", "c", "m"), 
          parameters = list(direction = "dem_drain",
                            stream_rast = "dem_stream",
                            basins = "dem_basins",
                            memory = 16000))

#Convert raster to vector file
execGRASS("r.to.vect", flags = c("overwrite", "s"), 
          parameters = list(input = "dem_basins",
                            output = "dem_basins",
                            type = "area"))

#Import vector file to R and write to gis_database
use_sf()

dem_basins_sf <- readVECT("dem_basins") %>% 
  select(basin_id = value) %>% 
  group_by(basin_id) %>% 
  summarise() %>% 
  st_transform(dk_epsg) %>% 
  st_make_valid() %>% 
  st_cast("MULTIPOLYGON") %>% 
  mutate(basin_area_m2 = as.numeric(st_area(geom)),
         basin_circum_m = as.numeric(st_length(st_cast(geom, "MULTILINESTRING"))))

st_write(dem_basins_sf, gis_database, layer = "basins", delete_layer = TRUE)
