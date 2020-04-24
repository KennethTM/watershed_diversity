source("libs_and_funcs.R")

library(lme4)

#Load data
all_model_data <- readRDS(paste0(getwd(), "/figures/all_model_data.rds"))

#Set up data
model_df <- all_model_data %>% 
  st_drop_geometry() %>% 
  select(basin_id_fact, n_spec_lake, n_spec_basin, lake_stream_connect_binary, basin_area_log10, 
         elevation)

#Binomial glmm model med basin som mixed effect
glob_mod <- glmer(n_spec_lake/n_spec_basin~lake_stream_connect_binary+basin_area_log10+elevation+(1|basin_id_fact),
                  weights=n_spec_basin, data=model_df, family="binomial")
summary(glob_mod)
plot(glob_mod)

