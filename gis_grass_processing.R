source("libs_and_funcs.R")

#Use dem 25 m DEM as template
#Use 10 m DEM later perhaps, memory swap might needed for hydrological algorithms
dk_dem_25_path <- paste0(getwd(), "/data_processed/dk_dem_25.tif")
dk_dem_25 <- raster(dk_dem_25_path)

#Find grass install
grass_path <- findGRASS()

#Setup grass environment
#https://grasswiki.osgeo.org/wiki/R_statistics/rgrass7#GRASS_within_R
link2GI::linkGRASS7(dk_dem_25,
                    default_GRASS7 = c("/usr/lib/grass78", grass_path$version, grass_path$installation_type),
                    gisdbase = paste0(getwd(), "/gis_grass_database"), 
                    location = "dk_25",
                    gisdbase_exist = TRUE)
use_sp()

#Import dem
execGRASS("r.in.gdal", flags = c("overwrite"), parameters = list(input = dk_dem_25_path, output = "dem"))

#Corrections using hydrodem
execGRASS("r.hydrodem", flags = c("overwrite"), parameters = list(input = "dem", output = "dem_hydrodem", memory = 8000))

#Extract streams and flow directions using r.watershed
execGRASS("r.watershed", flags = c("overwrite", "b"),
          parameters = list(elevation = "dem_hydrodem", 
                            accumulation = "dem_acc",
                            stream = "dem_stream",
                            drainage = "dem_drain",
                            threshold = 1000,
                            memory = 8000))

#Determine major watersheds using r.stream.basins
execGRASS("r.stream.basins", flags = c("overwrite", "l", "c"), 
          parameters = list(direction = "dem_drain",
                            stream_rast = "dem_stream",
                            basins = "dem_basins",
                            memory = 8000))

#Convert raster to vector file
execGRASS("r.to.vect", flags = c("s", "overwrite"), 
          parameters = list(input = "dem_basins",
                            output = "dem_basins",
                            type = "area"))

#extract elevation range for each basin
execGRASS("v.rast.stats", 
          parameters = list(map = "dem_basins",
                            raster = "dem_hydrodem",
                            column_prefix = "dem",
                            method =  "range,average"))

#Import vector file to R
dem_basins_sp <- readVECT("dem_basins")

dem_basins_sf <- dem_basins_sp %>% 
  st_as_sf() %>% 
  select(basin_id = value, dem_range, dem_average) %>% 
  group_by(basin_id) %>% 
  summarise(elev_range_m = max(dem_range), elev_mean_m = mean(dem_average)) %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_set_crs(dk_epsg) %>% 
  st_make_valid()

st_write(dem_basins_sf, dsn = gis_database, layer = "dk_basins", delete_layer = TRUE)

#Extract vector stream network as vector
execGRASS("r.stream.extract", flags = c("overwrite"), 
          parameters = list(elevation = "dem_hydrodem",
                            accumulation = "dem_acc",
                            threshold = 100,
                            stream_vector = "dem_stream_vect",
                            memory = 4000))

#Export line vector and save to gis database
stream_line_vect <- readVECT("dem_stream_vect", type = "line", layer = "1") %>% 
  st_as_sf() %>% 
  st_set_crs(dk_epsg)

st_write(stream_line_vect, dsn = gis_database, layer = "dk_dem_streams_lines", delete_layer = TRUE)

#Export point vector and save to gis database
#Attribute var "cat" = 2 is stream/basin outlets
stream_point_vect <- readVECT("dem_stream_vect", type = "point", layer = "2") %>% 
  st_as_sf() %>% 
  st_set_crs(dk_epsg)

st_write(stream_point_vect, dsn = gis_database, layer = "dk_dem_streams_points", delete_layer = TRUE)
