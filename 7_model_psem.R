source("0_libs_and_funcs.R")

#Preprocessing and modeling of data using piecewise structural equation model

model_data_raw <- read_csv(paste0(getwd(), "/data_processed/model_data_raw.csv"))

model_data_proc <- model_data_raw %>% 
  filter(!is.na(basin_id)) %>% 
  mutate(lake_age = ifelse(is.na(year_established), 9999, year_sample - year_established),
         lake_age_bins = cut(lake_age, 
                             breaks = c(-1, 10, 20, 200, 10000), 
                             labels = c("0-10", "10-20", ">20", "Unknown")),
         lake_age_bins = factor(lake_age_bins, levels = c("Unknown", "0-10", "10-20", ">20")),
         lake_natural = ifelse(lake_age_bins == "Unknown", 1, 0)) %>% 
  select(-year_established, -year_sample, -basin_elev_max_m, 
         -basin_elev_min_m, -lake_stream_connect_n, -lake_area_m2)

write_csv(model_data_proc, paste0(getwd(), "/data_processed/model_data_proc.csv"))

#Plot distributions and transform accordingly
model_data_trans <- model_data_proc %>% 
  mutate(basin_stream_length_m = log10(1 + basin_stream_length_m)) %>% 
  mutate_at(vars(basin_area_m2, basin_circum_m, basin_lake_area_m2, basin_slope_prc, 
                 bathy_area, bathy_vol, bathy_zmax, bathy_zmean, 
                 chla_ug_l, salinity, secchi_depth_m, shoreline_m, tn_mg_l, tp_mg_l), list(log10)) 

model_data_trans %>% 
  select(-gml_id, -site_id, -site_name, -basin_id, 
         -n_spec_lake, -n_spec_basin, -lake_age_bins, -lake_age) %>% 
  gather(variable, value) %>% 
  ggplot(aes(value))+
  geom_density()+
  facet_wrap(variable~., scales = "free")

#Correlation analysis for basin level predictor variables
basin_corr <- model_data_trans %>% 
  select(basin_lake_area_m2, basin_stream_length_m, basin_arti, basin_agri, basin_elev_mean_m,
         basin_slope_prc, basin_area_m2, basin_circum_m, salinity, basin_elev_range_m) %>% 
  select(-basin_area_m2, -basin_circum_m, -basin_elev_range_m, -basin_stream_length_m)

#Additional basin vars
#ice_covered

#Iteratively drop variables until VIF < 3
corvif(basin_corr)

#Correlation analysis for lake level predictor variables
lake_corr <- model_data_trans %>% 
  select(lake_elev_m, shoreline_m, bathy_area, bathy_vol, bathy_zmean, bathy_zmax,
         alk_mmol_l, chla_ug_l, tn_mg_l, ph_ph, tp_mg_l, secchi_depth_m) %>% 
  select(-bathy_zmean, -bathy_vol, -shoreline_m, -chla_ug_l)

#Additional lake vars
#lake_stream_connect, lake_natural

#Iteratively drop variables until VIF < 3
corvif(lake_corr)

#drop observations with missing
model_data_psem <- model_data_trans %>% 
  filter(!is.na(tn_mg_l),
         !is.na(ph_ph),
         !is.na(bathy_area),
         !is.na(alk_mmol_l),
         !is.na(secchi_depth_m)) %>%
  select(-basin_area_m2, -basin_circum_m, -basin_elev_range_m, -basin_stream_length_m,
         -bathy_zmean, -bathy_vol, -shoreline_m, -chla_ug_l)

summary(model_data_psem)

write_csv(model_data_psem, paste0(getwd(), "/data_processed/model_data_psem.csv"))

#Specify piecewise structural equation model
psem_mod <- psem(
  glmer(n_spec_basin ~ basin_lake_area_m2+basin_arti+basin_agri+
          basin_elev_mean_m+basin_slope_prc+salinity+ice_covered+(1|basin_id),
        data = model_data_psem, family = "poisson", nAGQ=0),
  
  glmer(n_spec_lake ~ n_spec_basin+lake_elev_m+bathy_area+bathy_zmax+
          alk_mmol_l+tn_mg_l+ph_ph+tp_mg_l+secchi_depth_m+
          lake_stream_connect+lake_natural+lake_stream_connect:lake_natural+(1|basin_id), 
        data = model_data_psem, family = "poisson", nAGQ=0)
)
psem_sum <- summary(psem_mod)
psem_sum

#Save results of model summary
sink(paste0(getwd(), "/data_processed/psem_summary.txt"))
psem_sum
dev.off()

#Normalize standardized parameter estimates to adjust line widths (range 0.01 - 0.1) in figure
as.data.frame(coefs(psem_mod)) %>% 
  select(1:3) %>% 
  mutate(estimate_abs = abs(Estimate),
         line_widths = (0.1 - 0.01) / (max(estimate_abs)-min(estimate_abs)) * (estimate_abs - max(estimate_abs)) + 0.1)

# #Emmeans estimates
# lake_level <- glmer(n_spec_lake ~ n_spec_basin+lake_elev_m+bathy_area+bathy_zmax+
#               alk_mmol_l+tn_mg_l+ph_ph+tp_mg_l+secchi_depth_m+
#               lake_stream_connect+lake_natural+lake_stream_connect:lake_natural+(1|basin_id),
#             data = mutate(model_data_psem,
#                           lake_stream_connect = as.factor(ifelse(lake_stream_connect == 1, "connect", "disconnect")),
#                           lake_natural = as.factor(ifelse(lake_natural == 1, "natural", "new"))), family = "poisson", nAGQ=0)
# 
# lake_level_em <- emmeans(lake_level,  ~ lake_natural*lake_stream_connect)
# 
# emmip(lake_level, lake_natural ~ lake_stream_connect)
# emmeans(lake_level, specs= pairwise ~ lake_natural * lake_stream_connect)
