#Network modeling using grainscape and igraph

source("libs_and_funcs.R")

#Load some data
model_and_fig_data <- readRDS(paste0(getwd(), "/data_processed/model_and_fig_data.rds"))
lake_df <- model_and_fig_data[[2]]

dk_basins <- st_read(dsn = gis_database, layer = "dk_basins")

dk_dem <- raster(paste0(getwd(), "/data_processed/dk_dem_25.tif"))
dk_dem_bbox <- as.numeric(st_bbox(dk_dem))
rast_res <- c(10, 10)

#Buffer stream lines because networking algorithm does behave well in diagonal connected raster cells (4 vs 8 connected?)
dk_streams_buf <- st_read(dsn = gis_database, layer = "dk_streams") %>% 
  st_buffer(5)
st_write(dk_streams_buf, paste0(getwd(), "/data_raw/dk_streams_buffer.sqlite"), delete_dsn = TRUE)

#Rasterize streams (network derived from dem) and lakes
gdal_rasterize(src_datasource = paste0(getwd(), "/data_raw/dk_streams_buffer.sqlite"),
               dst_filename = paste0(getwd(), "/data_raw/dk_streams.tif"),
               burn = 2, at = TRUE, a_nodata = 0, tap = TRUE,
               te = dk_dem_bbox, tr = rast_res,
               co = "COMPRESS=LZW")

#Only rasterize lakes above 1000 m2
gdal_rasterize(src_datasource = gis_database, l = "dk_lakes_edit",
               dst_filename = paste0(getwd(), "/data_raw/dk_lakes.tif"),
               burn = 1, at = TRUE, a_nodata = 0, tap = TRUE,
               te = dk_dem_bbox, tr = rast_res, where = "area >= 1000",
               co = "COMPRESS=LZW")

#Load data prepped raster files
rast_lakes <- raster(paste0(getwd(), "/data_raw/dk_lakes.tif"))
rast_streams <- raster(paste0(getwd(), "/data_raw/dk_streams.tif"))

#104 unique basins
basin_idx <- sort(unique(na.omit(lake_df$basin_id)))
basin_result_list <- vector("list", length = length(basin_idx))

#Calculate network (stream and lakes) statistics for each basin 
for(i in basin_idx){
  print(paste0("Starting basin id ", i, " at ", Sys.time()))
  basin <- dk_basins %>% 
    filter(basin_id == i)
  
  basin_sites <- lake_df %>% 
    filter(basin_id == i)
  
  basin_lakes <- crop(rast_lakes, basin)
  basin_streams <- crop(rast_streams, basin)
  
  rast_resist <- basin_streams
  rast_resist[] <- ifelse(!is.na(basin_lakes[]), 1, basin_streams[])
  rast_resist_mask <- mask(rast_resist, basin)

  skip_to_next <- FALSE
  tryCatch(mpg_graph <- MPG(rast_resist_mask, patch = (rast_resist_mask == 1)), error = function(e){skip_to_next <<- TRUE})
  if(skip_to_next){print("Skipping..."); next}  

  mpg_degree <- degree(mpg_graph@mpg)
  mpg_degree_weight <- strength(mpg_graph@mpg, weights = edge_attr(mpg_graph@mpg)$lcpPerimWeight)
  mpg_between <- betweenness(mpg_graph@mpg)
  mpg_degree_norm <- degree(mpg_graph@mpg, normalized = TRUE)
  mpg_between_norm <- betweenness(mpg_graph@mpg, normalized = TRUE)
  
  df_metrics <- data.frame(vertex_attr(mpg_graph@mpg),
                           degree = mpg_degree, 
                           degree_weight = mpg_degree_weight,
                           between = mpg_between,
                           degree_norm = mpg_degree_norm,
                           between_norm = mpg_between_norm)
  
  basin_sites_patch_id <- raster::extract(mpg_graph@patchId, as(basin_sites, "Spatial"))
  
  basin_sites_with_attr <- basin_sites %>% 
    add_column(patch_id = basin_sites_patch_id) %>% 
    left_join(select(df_metrics, patch_id = patchId, degree, degree_weight, between, degree_norm, between_norm), by = "patch_id")
  
  i_n <- which(basin_idx == i)

  basin_result_list[[i_n]] <- basin_sites_with_attr
  
  print(paste0("Completed ", i_n, " of ", length(basin_idx), " basins"))
  
}

basin_result_df <- do.call(rbind, basin_result_list)

#Multiply degree_weight by 5 to get meters (cost 2)
basin_result_df_sub <- basin_result_df %>% 
  st_drop_geometry() %>% 
  select(site_id, degree, degree_weight, between, degree_norm, between_norm)

saveRDS(basin_result_df_sub, paste0(getwd(), "/data_processed/gis_network_attr.rds"))
