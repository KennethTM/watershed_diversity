source("0_libs_and_funcs.R")

#Calculate attributes for each basin:
#Ice-cover during last ice age
#Stream length and lake area
#CLC agriculture and artifical area
#DEM morphometry

#Load data
dk_border <- st_read(gis_database, layer = "dk_border")
dk_iceage <- st_read(gis_database, layer = "dk_iceage")
dk_streams <- st_read(gis_database, layer = "dk_streams")
dk_lakes <- st_read(gis_database, layer = "dk_lakes")
corine_land_cover <- st_read(gis_database, layer = "corine_land_cover")
basins <- st_read(gis_database, layer = "basins") %>% 
  st_crop(dk_border) %>% 
  st_cast("MULTIPOLYGON")

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

#Salinity at basin outlet
#https://resources.marine.copernicus.eu/product-detail/GLOBAL_REANALYSIS_PHY_001_031/INFORMATION
#https://help.marine.copernicus.eu/en/articles/4444611-how-to-cite-or-reference-copernicus-marine-products-and-services#publication
sal_stack <- raster(paste0(getwd(), "/data_raw/global-reanalysis-phy-001-030-monthly_1638544155158.nc"), varname="so")
sal_mean <- calc(sal_stack, mean)

sal_df <- as.data.frame(sal_mean, xy=TRUE) %>% 
  as_tibble() %>% 
  rename(salinity=layer)

sal_df_notna <- sal_df %>% 
  filter(!is.na(salinity)) %>% 
  st_as_sf(coords=c("x", "y"), crs = st_crs(sal_mean)) %>% 
  st_transform(dk_epsg)

sal_df_na <- sal_df %>% 
  filter(is.na(salinity)) %>% 
  st_as_sf(coords=c("x", "y"), crs = st_crs(sal_mean)) %>% 
  st_transform(dk_epsg) %>% 
  select(-salinity) %>% 
  st_join(sal_df_notna, join=st_nearest_feature)

sal_df_filled <- rbind(sal_df_notna, sal_df_na)

basin_salinity <- basins %>% 
  st_centroid() %>% 
  st_join(sal_df_filled, join=st_nearest_feature) %>% 
  st_drop_geometry() %>% 
  select(basin_id, salinity) %>% 
  as_tibble()

#Combine attributes to data.frame and save
basin_attributes <- bind_cols(basin_id = basins$basin_id, ice_covered = ice_covered, 
                              basin_lake_area[, "basin_lake_area_m2"], basin_stream_length[, "basin_stream_length_m"],
                              basin_arti = basin_arti, basin_agri = basin_agri,
                              basin_elev, basin_slope_prc = basin_slope) %>% 
  left_join(basin_salinity) %>% 
  mutate(basin_elev_range_m = basin_elev_max_m-basin_elev_min_m,
         ice_covered = ifelse(ice_covered == TRUE, 1, 0))

#Save data.frame
write_csv(basin_attributes, paste0(getwd(), "/data_processed/basin_attributes.csv"))
