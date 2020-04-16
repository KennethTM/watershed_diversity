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

#Import vector file to R
dem_basins_sp <- readVECT("dem_basins")

dem_basins_sf <- dem_basins_sp %>% 
  st_as_sf() %>% 
  select(basin_id = value) %>% 
  group_by(basin_id) %>% 
  summarise() %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_set_crs(dk_epsg)

st_write(dem_basins_sf, dsn = gis_database, layer = "dk_basins", delete_layer = TRUE)

# #Delineate lake watersheds
# #Find lakes with fish species data
# dk_lakes <- st_read(dsn = gis_database, layer = "dk_lakes")
# fish_species_lakes <- st_read(dsn = gis_database, layer = "fish_species_lakes")
# 
# fish_species_lakes_polys <- fish_species_lakes %>% 
#   select(system, site_id) %>% 
#   distinct(.keep_all = TRUE) %>% 
#   st_join(dk_lakes, left = FALSE) 
# 
# #Extract lake polygons and export to grass
# dk_lakes_sub <- dk_lakes %>% 
#   filter(gml_id %in% fish_species_lakes_polys$gml_id) %>% 
#   mutate(lake_basin_id = 1:n())
# 
# writeVECT(as(dk_lakes_sub[, "lake_basin_id"], "Spatial"), "dk_lakes_sub", v.in.ogr_flags = "overwrite")
# 
# #Save lake subset in database
# st_write(dk_lakes_sub, dsn = gis_database, layer = "dk_lakes_sub", delete_layer = TRUE)
# 
# #Delineate watersheds using gml_id for lake polygons id's
# lake_basin_indexes <- dk_lakes_sub$lake_basin_id
# for(i in lake_basin_indexes){
#   print(paste0("Creating watershed for lake_basin_id ", i, " or gml_id ", dk_lakes_sub$gml_id[i]))
#   
#   execGRASS("v.extract", flags = c("overwrite"), parameters = list(input = "dk_lakes_sub@PERMANENT",
#                                                                    output = paste0("tmp_", i),
#                                                                    type = "area",
#                                                                    layer = "1",
#                                                                    new = -1,
#                                                                    where = paste0("lake_basin_id = ", i)))
#   
#   execGRASS("v.to.rast", flags = c("overwrite"), parameters = list(input = paste0("tmp_", i),
#                                                                    output = paste0("tmp_", i),
#                                                                    type = "area",
#                                                                    layer = "1",
#                                                                    use = "val",
#                                                                    value = 1))
#   
#   execGRASS("r.stream.basins", flags = "overwrite", parameters = list(direction = "dem_drain",
#                                                                       stream_rast = paste0("tmp_", i),
#                                                                       basins = paste0("tmp_", i, "_basin"),
#                                                                       memory = 4000))
#   
#   execGRASS("r.to.vect", flags = c("s", "overwrite"), parameters = list(input = paste0("tmp_", i, "_basin"),
#                                                                         output = paste0("tmp_", i, "_basin"),
#                                                                         type = "area"))
# }
# 
# #Read lake basins into R and write to gis database
# lake_basin_list <- vector("list", length = length(lake_basin_indexes))
# for(i in seq_along(lake_basin_indexes)){
#   grass_vect <- tryCatch(readVECT(paste0("tmp_", lake_basin_indexes[i], "_basin")),
#                          error = function(e){NULL}) #Basin for two lakes cannot be delineated
#   if(is.null(grass_vect)){
#     lake_basin_list[[i]] <- grass_vect
#   }else{
#     lake_basin_list[[i]] <- grass_vect %>% 
#       st_as_sf() %>% 
#       add_column(lake_basin_id = lake_basin_indexes[i])
#   }
# }
# 
# lake_basin_sf <- do.call(rbind, lake_basin_list) %>% 
#   select(lake_basin_id) %>% 
#   group_by(lake_basin_id) %>% 
#   summarise() %>% 
#   st_cast("MULTIPOLYGON") %>% 
#   left_join(st_drop_geometry(dk_lakes_sub)) %>% 
#   st_set_crs(dk_epsg)
# 
# st_write(lake_basin_sf, dsn = gis_database, layer = "lake_basins", delete_layer = TRUE)
# 
# #Clean grass database for temporary files
# execGRASS("g.remove", flags = "f", parameters = list(type = "raster", pattern = "tmp_*"))
# execGRASS("g.remove", flags = "f", parameters = list(type = "vector", pattern = "tmp_*"))
