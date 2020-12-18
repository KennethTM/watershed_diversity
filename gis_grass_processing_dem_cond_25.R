
source("libs_and_funcs.R")

#Use dem 25 m DEM as template
#Use 10 m DEM later perhaps, memory swap might needed for hydrological algorithms
dk_dem_25_path <- paste0(getwd(), "/data_processed/Skjern_basin_cond.tif")
dk_dem_25 <- raster(dk_dem_25_path)

#Find grass install
grass_path <- findGRASS()

#Setup grass environment
#https://grasswiki.osgeo.org/wiki/R_statistics/rgrass7#GRASS_within_R
link2GI::linkGRASS7(dk_dem_25,
                    default_GRASS7 = c("/usr/lib/grass78", grass_path$version, grass_path$installation_type),
                    gisdbase = paste0(getwd(), "/gis_grass_database"), 
                    location = "dk_25_cond",
                    gisdbase_exist = TRUE)
use_sp()

#Import dem
execGRASS("r.in.gdal", flags = c("overwrite"), parameters = list(input = dk_dem_25_path, output = "dem"))

#Corrections using hydrodem
execGRASS("r.hydrodem", flags = c("overwrite", "c"), parameters = list(input = "dem", output = "dem_hydrodem", memory = 8000))

#Extract streams and flow directions using r.watershed
execGRASS("r.watershed", flags = c("overwrite", "b", "s", "a"),
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
execGRASS("r.to.vect", flags = c("overwrite", "s"), 
          parameters = list(input = "dem_basins",
                            output = "dem_basins",
                            type = "area"))

#extract elevation range for each basin
execGRASS("v.rast.stats", flags = c("c"),
          parameters = list(map = "dem_basins",
                            raster = "dem_hydrodem",
                            column_prefix = "dem",
                            method =  "min,max,range,average"))

#Import vector file to R
dem_basins_sp <- readVECT("dem_basins")

dem_basins_sf <- dem_basins_sp %>% 
  st_as_sf() %>% 
  st_cast("POLYGON") %>% 
  select(basin_id = value, dem_minimum, dem_maximum, dem_range, dem_average) %>% 
  mutate(area=as.numeric(st_area(geometry))) %>% 
  group_by(basin_id) %>% 
  slice(which.max(area)) %>%  #removes small artifacts from vectorization 
  ungroup() %>% 
  st_make_valid()

#Read cutline layers
cut1 <- st_read("konge_ribe.kmz")
cut2 <- st_read("Vid_aa_syd.kmz")

cutlines <- do.call(rbind, list(cut1, cut2)) %>%
  st_zm() %>%
  st_transform(st_crs(dem_basins_sf)) %>%
  st_union() %>%
  st_sfc()

#Cut basin polygons
dem_basins_cut_sf <- dem_basins_sf %>%
  st_split(cutlines) %>%
  st_collection_extract(type = "POLYGON") %>% 
  mutate(basin_id = row_number(),
         basin_area_m2 = as.numeric(st_area(geometry))) %>% 
  rename(basin_elevation_range_m = dem_range,
         basin_elevation_average_m = dem_average,
         basin_elevation_minimum_m = dem_minimum,
         basin_elevation_maximum_m = dem_maximum) %>% 
  select(-area)

#Read layers from database
#Calculate attributes for basins and lakes
dk_streams <- st_read(dsn = gis_database, layer = "dk_streams") %>% 
  mutate(stream_length_m = as.numeric(st_length(GEOMETRY)))

dk_lakes <- st_read(dsn = gis_database, layer = "dk_lakes_edit") %>%
  rename(lake_area_m2 = area)

dk_streams_centroid <- st_centroid(select(dk_streams, stream_length_m))

dk_lakes_centroid <- st_centroid(select(dk_lakes, lake_area_m2))

dk_lakes_attr <- dem_basins_cut_sf %>%
  st_transform(st_crs(dk_lakes)) %>% 
  st_join(dk_lakes_centroid, left=FALSE) %>% 
  st_drop_geometry() %>% 
  group_by(basin_id) %>% 
  summarise(basin_lake_number = n(), basin_lake_area_m2 = sum(lake_area_m2), lake_max_area_m2 = max(lake_area_m2))

dk_stream_attr <- dem_basins_cut_sf %>%
  st_transform(st_crs(dk_lakes)) %>% 
  st_join(dk_streams_centroid, left=FALSE) %>% 
  st_drop_geometry() %>% 
  group_by(basin_id) %>% 
  summarise(basin_stream_length_m = sum(stream_length_m))

dem_basins_cut_attr_sf <- dem_basins_cut_sf %>% 
  left_join(dk_lakes_attr) %>% 
  left_join(dk_stream_attr)

st_write(dem_basins_cut_attr_sf, "dk_basins_cond_attr.kml", delete_dsn = TRUE)

dem_basins_cut_attr_sf %>% 
  st_drop_geometry() %>% 
  write.csv("dk_basins_cond_attr.csv")

