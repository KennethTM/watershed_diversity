source("libs_and_funcs.R")

#Calculate attributes for each basin:
#Ice-cover during last ice age
#Stream length and lake area
#CLC agriculture and artifical area
#DEM morphometry

#Load data
basins <- st_read(gis_database, layer = "basins")
dk_iceage <- st_read(gis_database, layer = "dk_iceage")
dk_streams <- st_read(dsn = gis_database, layer = "dk_streams")
dk_lakes <- st_read(dsn = gis_database, layer = "dk_lakes")
corine_land_cover <- st_read(dsn = gis_database, layer = "corine_land_cover")
dk_border <- st_read(gis_database, layer = "dk_border")

#Ice cover
ice_covered <- factor(st_intersects(dk_iceage, st_centroid(basins), sparse = FALSE))

#Basin stream length and lake area
#Use feature centroids for faster join
dk_streams_centroid <- st_centroid(select(dk_streams, stream_length_m))
dk_lakes_centroid <- st_centroid(select(dk_lakes, lake_area_m2))

basin_lake_area <- basins %>%
  st_join(dk_lakes_centroid) %>%
  st_drop_geometry() %>%
  select(basin_id, lake_area_m2) %>%
  group_by(basin_id) %>%
  summarise(basin_lake_area_m2 = sum(lake_area_m2)) %>% 
  mutate(basin_lake_area_m2 = ifelse(is.na(basin_lake_area_m2), 0, basin_lake_area_m2))

basin_stream_length <- basins %>%
  st_join(dk_streams_centroid) %>%
  st_drop_geometry() %>%
  select(basin_id, stream_length_m) %>%
  group_by(basin_id) %>%
  summarise(basin_stream_length_m = sum(stream_length_m)) %>% 
  mutate(basin_stream_length_m = ifelse(is.na(basin_stream_length_m), 0, basin_stream_length_m))

#Corine land cover
raster_template <- raster(dk_border, res = 50)
raster_clc <- fasterize(corine_land_cover, raster_template, field = "clc_code", fun = "first")
clc_arti <- (raster_clc %in% c(111, 112, 121, 122, 123, 124, 131, 132, 133, 141, 142))
clc_agri <- (raster_clc %in% c(211, 212, 213))

basin_arti <- exact_extract(clc_arti, basins, fun = "mean")
basin_agri <- exact_extract(clc_agri, basins, fun = "mean")

#DEM attributes
elev <- raster(paste0(getwd(), "/data_raw/dhym_10m.tif"))
slope <- raster(paste0(getwd(), "/data_raw/dhym_10m_slope.tif"))

basin_elev <- exact_extract(elev, basins, fun = c("min", "mean", "max"))
names(basin_elev) <- paste0("basin_elev_", names(basin_elev), "_m")
basin_slope <- exact_extract(slope, basins, fun = "mean")

#Combine attributes to data.frame and save
basin_attr <- bind_cols(basin_id = basins$basin_id, 
                        ice_covered = ice_covered, 
                        basin_lake_area[, "basin_lake_area_m2"],
                        basin_stream_length[, "basin_stream_length_m"],
                        basin_arti = basin_arti,
                        basin_agri = basin_agri,
                        basin_elev,
                        basin_slope_prc = basin_slope)

#Save data.frame
saveRDS(basin_attr, paste0(getwd(), "/data_processed/basin_attr.rds"))
