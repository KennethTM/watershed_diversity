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

#Import dem
execGRASS("r.in.gdal", flags = c("o"), parameters = list(input = dk_dem_25_path, output = "dem"))

#Corrections using hydrodem
execGRASS("r.hydrodem", flags = c("overwrite"), parameters = list(input = "dem", output = "dem_hydrodem", memory = 4000))

#Extract streams and flow directions using r.watershed
execGRASS("r.watershed", flags = c("overwrite", "b"),
          parameters = list(elevation = "dem_hydrodem", 
                            accumulation = "dem_acc",
                            stream = "dem_stream",
                            drainage = "dem_drain",
                            threshold = 1000,
                            memory = 4000))

#Determine major watersheds using r.stream.basins
execGRASS("r.stream.basins", flags = c("overwrite", "l", "c"), 
          parameters = list(direction = "dem_drain",
                            stream_rast = "dem_stream",
                            basins = "dem_basins",
                            memory = 4000))

#Convert raster to vector file
execGRASS("r.to.vect", flags = c("s", "overwrite"), 
          parameters = list(input = "dem_basins",
                            output = "dem_basins",
                            type = "area"))

#Import vector file to R
use_sp()
dem_basins_sp <- readVECT("dem_basins")

dem_basins_sf <- dem_basins_sp %>% 
  st_as_sf() %>% 
  select(-cat) %>% 
  mutate(area = as.numeric(st_area(geometry))) %>% 
  filter(area>625) %>% 
  group_by(value) %>% 
  summarise() %>% 
  select(basin_id = value) %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_set_crs(dk_epsg)

st_write(dem_basins_sf, dsn = gis_database, layer = "basins", delete_layer = TRUE)

#Delineate lake watersheds




#join fish and lakes, extract the polys, export to grass, delineate watersheds, export and collect in R, write to database

lakes <- st_read(dsn = gis_database, layer = "dk_lakes")
fish <- st_read(dsn = gis_database, layer = "fish_species_lakes")

tmp <- fish %>% 
  select(system, site_id) %>% 
  distinct(.keep_all = TRUE) %>% 
  st_join(lakes, left = FALSE) 

pull(gml_id)

