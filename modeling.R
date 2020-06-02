source("libs_and_funcs.R")

#Load data
model_and_fig_data <- readRDS(paste0(getwd(), "/data_processed/model_and_fig_data.rds"))

#Load network metrics
network_attr <- readRDS(paste0(getwd(), "/data_processed/gis_network_attr.rds"))

#Basin level data
#Replace NA's in lake and stream sums/mean with 0
#log10 transform some variables
basin_df <- model_and_fig_data[[1]] %>% 
  st_drop_geometry() %>% 
  mutate_at(vars(basin_sum_stream_length_m, basin_sum_lake_area_m2, basin_mean_lake_area_m2), ~ifelse(is.na(.), 0, .)) %>% 
  mutate(log10_basin_sum_stream_length_m = log10(basin_sum_stream_length_m+1),
         log10_basin_sum_lake_area_m2 = log10(basin_sum_lake_area_m2+1),
         log10_basin_mean_lake_area_m2 = log10(basin_mean_lake_area_m2+1),
         log10_basin_area_m2 = log10(basin_area_m2),
         log10_basin_area_m2 = log10(basin_area_m2),
         log10_basin_circum_m = log10(basin_circum_m),
         water_basin_ratio = basin_sum_lake_area_m2/basin_area_m2)

summary(basin_df)

#Examine correlations
basin_preds <- basin_df %>% 
  select(-basin_id, -basin_ice_covered, -n_spec_basin, 
         -basin_sum_stream_length_m, -basin_sum_lake_area_m2,
         -basin_circum_m, -basin_area_m2, -log10_basin_circum_m, -log10_basin_area_m2,
         -log10_basin_mean_lake_area_m2, -elev_mean_m)

basin_cor <- cor(basin_preds)

corrplot.mixed(basin_cor)

#Calculate variance inflation factors (VIF) (Zuur MEE 2009)
#Drop variables such that maximum VIF is not higher than 3 
corvif(basin_preds)

#Model basin species richness using poisson GAM
basin_gam_df <- basin_df %>% 
  select(basin_ice_covered, n_spec_basin) %>% 
  bind_cols(basin_preds)

m0 <- gam(n_spec_basin ~ s(elev_range_m) + basin_ice_covered + 
            s(log10_basin_sum_lake_area_m2) + s(log10_basin_sum_stream_length_m) + s(basin_mean_lake_area_m2)+
            s(water_basin_ratio) + s(outlet_x, outlet_y), family = "poisson", method = "REML", data = basin_gam_df)

summary(m0)
plot(m0)

m1 <- gam(n_spec_basin ~ s(elev_range_m) + 
            s(log10_basin_sum_lake_area_m2) + s(log10_basin_sum_stream_length_m)+
            s(outlet_x, outlet_y), family = "quasipoisson", method = "REML", data = basin_gam_df, select = TRUE)

summary(m1)
plot(m1)

gamviz <- getViz(m2)
plot(sm(gamviz, 1))+l_ciPoly()+l_fitLine()
plot(sm(gamviz, 2))+l_ciPoly()+l_fitLine()
plot(sm(gamviz, 3))+l_ciPoly()+l_fitLine()
plot(sm(gamviz, 4))+l_fitRaster()+l_points()









#Lake level data
#Set NA network metrics to zero (not connected), except for Mjelssø which was outside area and thus not evaluated
lake_df <- model_and_fig_data[[2]] %>% 
  left_join(network_attr) %>% 
  mutate_at(vars(degree, degree_weight, between), ~ifelse(is.na(.), 0, .)) %>% 
  mutate_at(vars(degree, degree_weight, between), ~ifelse(site_id == 15, NA, .)) %>% 
  mutate(log10_basin_sum_stream_length_m = log10(basin_sum_stream_length_m+1),
         log10_basin_sum_lake_area_m2 = log10(basin_sum_lake_area_m2+1),
         log10_basin_mean_lake_area_m2 = log10(basin_mean_lake_area_m2+1),
         water_basin_ratio = basin_sum_lake_area_m2/basin_area_m2) %>% 
  select(-basin_sum_stream_length_m, -basin_sum_lake_area_m2, -basin_mean_lake_area_m2)

#Set up data
model_df <- lake_df %>% 
  st_drop_geometry() %>% 
  filter(!is.na(basin_id)) %>%
  select(-year_established, -year, -gml_id, -n_spec_lake, -system, -site_id, -basin_id, -lake_stream_connect,
         -outlet_x, -outlet_y, -basin_ice_covered) %>% 
  select(-lake_area_m2, -lake_circum_m, -basin_area_m2, -basin_circum_m)

#Examine correlations
cont_preds <- model_df %>% 
  select(-basin_id_fact, -lake_ice_covered, -lake_stream_connect_binary, -n_spec_basin, -spec_proportion) %>% 
  select(-basin_area_log10, -lake_circum_log10, -basin_circum_log10, -log10_basin_sum_stream_length_m,
         -plant_vol_perc, -water_basin_ratio, -degree_weight, -elev_range_m, -zmean_m, -log10_basin_mean_lake_area_m2,
         -tp_mg_l, -lake_basin_area_ratio)

cond_preds_cor <- cor(cont_preds, use = "pairwise.complete.obs")
corrplot.mixed(cond_preds_cor)

#Calculate variance inflation factors (VIF) (Zuur MEE 2009)
#Drop variables such that maximum VIF is not higher than 3 
corvif(cont_preds)

#Binomial model
model_df_scale <- cont_preds %>% 
  mutate_all(list(scale)) %>% 
  bind_cols(select(model_df, basin_id_fact, lake_ice_covered, lake_stream_connect_binary, n_spec_basin, spec_proportion)) %>% 
  select(-plant_area_perc, -zmax_m, -alk_meq_l, -tn_mg_l) %>% #Removed as it is not important
  na.omit()

#Fit ordinary glm
global_glm <- glm(spec_proportion~elevation+elev_mean_m+lake_dev_ind+
                    secchi_depth_m+pH_pH+chla_ug_l+lake_area_log10+degree+
                    between+log10_basin_sum_lake_area_m2+lake_ice_covered+
                    lake_stream_connect_binary,
                 weights=n_spec_basin, 
                 data=model_df_scale, 
                 family="binomial")
summary(global_glm)

#Fit glmm with basin_id as random
global_glmm <- glmer(spec_proportion~elevation+elev_mean_m+lake_dev_ind+
                       secchi_depth_m+pH_pH+chla_ug_l+lake_area_log10+degree+
                       between+log10_basin_sum_lake_area_m2+lake_ice_covered+
                       lake_stream_connect_binary+(1|basin_id_fact),
                  weights=n_spec_basin, 
                  data=model_df_scale, 
                  family="binomial")
summary(global_glmm)

#Test for inclusion of random effect
#p-value 0.0519. Use random effect as LRT test is conservative
anova(global_glmm, global_glm)

#Refit glmm with better accuracy 
glmm_0 <- update(global_glmm, nAGQ = 4)
  
summary(glmm_0)

#Check for overdspersion (residual deviance / df) should approximate 1 
#https://esajournals.onlinelibrary.wiley.com/doi/full/10.1890/10-0340.1
deviance(glmm_0)/df.residual(glmm_0) #deviance 242.9 in summary - same conclusion

#Another function, same conclusion
#https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun(glmm_0)

#Backwards elimination of variables untill all terms are significant
drop1(glmm_0, test = "Chisq")
glmm_1 <- update(glmm_0, . ~ . -chla_ug_l)

drop1(glmm_1, test = "Chisq")
glmm_2 <- update(glmm_1, . ~ . -secchi_depth_m)

drop1(glmm_2, test = "Chisq")
glmm_3 <- update(glmm_2, . ~ . -between)

drop1(glmm_3, test = "Chisq")
glmm_4 <- update(glmm_3, . ~ . -lake_dev_ind)

drop1(glmm_4, test = "Chisq")
glmm_5 <- update(glmm_4, . ~ . -degree)

drop1(glmm_5, test = "Chisq")
glmm_6 <- update(glmm_5, . ~ . -lake_ice_covered)

drop1(glmm_6, test = "Chisq")

#include squared terms
glmm_7 <- update(glmm_6, . ~ . + I(lake_area_log10^2)+I(log10_basin_sum_lake_area_m2^2)) 
drop1(glmm_7, test = "Chisq")

glmm_8 <- update(glmm_7, . ~ . -elev_mean_m)
drop1(glmm_8, test = "Chisq")

summary(glmm_8)

#calculate goodness of fit
# glm_6 <- glm(spec_proportion ~ elevation + pH_pH + basin_area_log10 + lake_area_log10 +  
#                lake_stream_connect_binary + I(pH_pH^2) + I(lake_area_log10^2),
#                   weights=n_spec_basin, 
#                   data=model_df_scale, 
#                   family="binomial")
# 
# library(piecewiseSEM)
# rsquared(glm_6)

r.squaredGLMM(glmm_8)

r2.corr.mer <- function(m){
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  summary(lmfit)$r.squared
}
r2.corr.mer(glmm_8)

#Validate model
plot(glmm_8)

conf_intervals <- confint(glmm_8, method = "profile")

deviance(glmm_8)/df.residual(glmm_8) 

#Plots
ph <- ggeffect(glmm_8, terms = "pH_pH [all]")
elev <- ggeffect(glmm_8, terms = "elevation [all]")
basin_lake_area <- ggeffect(glmm_8, terms = "log10_basin_sum_lake_area_m2 [all]")
lake_area <- ggeffect(glmm_8, terms = "lake_area_log10 [all]")
connect <- ggeffect(glmm_8, terms = "lake_stream_connect_binary")

ph_plot <- ggplot(data = tbl_df(ph), aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "coral")+
  geom_line() +
  geom_point(data = model_df_scale, aes(pH_pH, spec_proportion), alpha = 0.2)+
  xlab("pH")+
  ylab("Richness")

elev_plot <- ggplot(data = tbl_df(elev), aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "coral")+
  geom_line() +
  geom_point(data = model_df_scale, aes(elevation, spec_proportion), alpha = 0.2)+
  xlab("Elevation")+
  ylab("Richness")

basin_lake <- ggplot(data = tbl_df(basin_lake_area), aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "coral")+
  geom_line() +
  geom_point(data = model_df_scale, aes(log10_basin_sum_lake_area_m2, spec_proportion), alpha = 0.2)+
  xlab("Basin area")+
  ylab("Richness")

lake_area_plot <- ggplot(data = tbl_df(lake_area), aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "coral")+
  geom_line() +
  geom_point(data = model_df_scale, aes(lake_area_log10, spec_proportion), alpha = 0.2)+
  xlab("Lake area")+
  ylab("Richness")

connect_plot <- ggplot(data = tbl_df(connect), aes(x, predicted)) +
  geom_linerange(aes(x = x, ymin = conf.low, ymax = conf.high), col = "coral", size = 2)+
  geom_point(aes(y=predicted))+
  geom_jitter(data = model_df_scale, aes(lake_stream_connect_binary, spec_proportion), alpha = 0.2,width = 0.25)+
  xlab("Stream connect")+
  ylab("Richness")

obs_pred_plot <- model_df_scale %>% 
  mutate(fitted_values = fitted(glmm_5)) %>% 
  ggplot(aes(fitted_values, spec_proportion)) +
  geom_abline(slope=1, intercept = 0, linetype = 2)+
  geom_point(alpha=0.2)+
  ylim(0, 1)+
  xlim(0, 1)+
  ylab("Observed richness")+
  xlab("Predicted richness")

glmer_plot <- ph_plot+elev_plot+basin_lake+lake_area_plot+connect_plot+obs_pred_plot+plot_layout(ncol = 2)

ggsave(paste0(getwd(), "/figures/glmer_plot.png"), glmer_plot, units = "mm", width = 174, height = 200)

