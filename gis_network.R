#Network modeling using grainscape and igraph
#http://www.alexchubaty.com/grainscape/articles/grainscape_vignette.html

source("libs_and_funcs.R")
library(grainscape)
library(igraph)

#Load some data
all_model_data <- readRDS(paste0(getwd(), "/data_processed/all_model_data.rds"))

dk_basins <- st_read(dsn = gis_database, layer = "dk_basins")

dk_dem <- raster(paste0(getwd(), "/data_processed/dk_dem_25.tif"))
dk_dem_bbox <- as.numeric(st_bbox(dk_dem))
dk_dem_res <- res(dk_dem)

#Prep some spatial data
#write a subset of lake polygons without the smallest lakes
dk_lakes_sub <- st_read(dsn = gis_database, layer = "dk_lakes") %>% 
  mutate(area=as.numeric(st_area(geometry))) %>% 
  filter(area >= 1000)
st_write(dk_lakes_sub, paste0(getwd(), "/data_raw/dk_lakes_size_subset.sqlite"), delete_dsn = TRUE)

#rasterize streams (network derived from dem) and lakes
gdal_rasterize(src_datasource =  gis_database, l = "dk_dem_streams_lines",
               dst_filename = paste0(getwd(), "/data_raw/dk_streams.tif"),
               burn = 1, init = 0, te = dk_dem_bbox, tr = dk_dem_res, tap = TRUE,
               co = "COMPRESS=LZW")

gdal_rasterize(src_datasource =  paste0(getwd(), "/data_raw/dk_lakes_size_subset.sqlite"),
               dst_filename = paste0(getwd(), "/data_raw/dk_lakes.tif"),
               burn = 2, init = 0, at = TRUE,
               co = "COMPRESS=LZW", te = dk_dem_bbox, tr = dk_dem_res, tap = TRUE)

#Load data prepped raster files
rast_lakes <- raster(paste0(getwd(), "/data_raw/dk_lakes.tif"))
rast_streams <- raster(paste0(getwd(), "/data_raw/dk_streams.tif"))

#104 unique basins
basin_idx <- sort(unique(na.omit(all_model_data$basin_id)))
basin_result_list <- vector("list", length = length(basin_idx))

#make parallel - ~10 hours without
for(i in basin_idx){
  print(paste0("Starting basin id ", i, " at ", Sys.time()))
  basin <- dk_basins %>% 
    filter(basin_id == 155)
  
  basin_sites <- all_model_data %>% 
    filter(basin_id == 155)
  
  basin_lakes <- crop(rast_lakes, basin)
  basin_streams <- crop(rast_streams, basin)
  basin_streams[basin_streams == 0] <- NA
  basin_streams[basin_streams == 1] <- 2
  #basin_dem <- crop(dk_dem, basin)
  #basin_dem[basin_dem < 0] <- 0
  #basin_streams_elev <- basin_dem*basin_streams
  
  #rast_resist <- basin_streams_elev
  #rast_resist[] <- ifelse(values(basin_lakes == 2), 1000, basin_streams_elev[])
  
  rast_resist[] <- ifelse(values(basin_lakes == 2), 1, basin_streams[])
  rast_resist_mask <- mask(rast_resist, basin)
  rast_patch_mask <- (rast_resist_mask %in% c(1, 2)) 
  rast_resist_mask[is.na(rast_resist_mask)] <- 1000
  
  mpg_graph <- MPG(rast_resist_mask, patch = rast_patch_mask)
  
  #plot(mpg_graph@mpg)
  #mapview(rast_resist_mask, method = "ngb")
  
  plot(mpg_graph, quick = "mpgPlot", theme = FALSE) + 
    geom_text(data = ggGS(mpg_graph, "nodes"),
              aes(x = x, y = y,
                  label = patchId),
              size = 2) +
    ggtitle("Planar 2D; Resistance surface")
  
  skip_to_next <- FALSE
  tryCatch(mpg_graph <- MPG(rast_resist, patch = rast_patch), error = function(e){ skip_to_next <<- TRUE})
  if(skip_to_next){print("Skipping..."); next}  

  #plot(mpg_graph, quick = "mpgPlot", theme = FALSE)

  #mpg_graph_thresh <- threshold(mpg_graph, doThresh = 9999)

  #plot(mpg_graph_thresh$th[[1]])
  
  mpg_degree <- degree(mpg_graph@mpg)
  mpg_degree_weight <- strength(mpg_graph@mpg, weights = edge_attr(mpg_graph@mpg)$lcpPerimWeight)
  mpg_eigen <- eigen_centrality(mpg_graph@mpg)$vector
  mpg_eigen_weight <- eigen_centrality(mpg_graph@mpg, weights = edge_attr(mpg_graph@mpg)$lcpPerimWeight)$vector
  mpg_page <- page_rank(mpg_graph@mpg)$vector
  mpg_page_weight <- page_rank(mpg_graph@mpg, weights = edge_attr(mpg_graph@mpg)$lcpPerimWeight)$vector
  mpg_between <- betweenness(mpg_graph@mpg)
  mpg_between_weight <- betweenness(mpg_graph@mpg, weights=edge_attr(mpg_graph@mpg)$lcpPerimWeight)
  
  # mpg_degree <- degree(mpg_graph_thresh$th[[1]])
  # mpg_degree_weight <- strength(mpg_graph_thresh$th[[1]], weights=edge_attr(mpg_graph_thresh$th[[1]])$lcpPerimWeight)
  # mpg_between <- betweenness(mpg_graph_thresh$th[[1]])
  # mpg_between_weight <- betweenness(mpg_graph_thresh$th[[1]], weights=edge_attr(mpg_graph_thresh$th[[1]])$lcpPerimWeight)
  
  df_metrics <- data.frame(vertex_attr(mpg_graph_thresh$th[[1]]),
                           degree = mpg_degree, 
                           degree_weight = mpg_degree_weight,
                           eigen = mpg_eigen,
                           eigen_weight = mpg_eigen,
                           page = mpg_page,
                           page_weight = mpg_page_weight,
                           between = mpg_between, 
                           between_weight = mpg_between_weight)
  
  basin_sites_patch_id <- raster::extract(mpg_graph@patchId, as(basin_sites, "Spatial"))
  
  basin_sites_with_attr <- basin_sites %>% 
    add_column(patch_id = basin_sites_patch_id) %>% 
    left_join(select(df_metrics, patch_id = patchId, degree, degree_weight, eigen, eigen_weight, page, page_weight, between, between_weight))
  
  i_n <- which(basin_idx == i)
  
  print(paste0("Completed ", i_n, " of ", length(basin_idx), " basins"))
  
  basin_result_list[[i_n]] <- basin_sites_with_attr
}

basin_result_df <- do.call(rbind, basin_result_list)

basin_result_df_sub <- basin_result_df %>% 
  st_drop_geometry() %>% 
  select(site_id, patch_id, degree, degree_weight, betweenness, betweenness_weight)

saveRDS(basin_result_df_sub, paste0(getwd(), "/data_processed/network_data.rds"))

