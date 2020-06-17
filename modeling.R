source("libs_and_funcs.R")

#Load data
model_and_fig_data <- readRDS(paste0(getwd(), "/data_processed/model_and_fig_data.rds"))

#Load network metrics
network_attr <- readRDS(paste0(getwd(), "/data_processed/gis_network_attr.rds"))

#Basin level modeling

#Replace NA's in lake and stream sums/mean with 0
#log10 and sqrt transform some variables
basin_df <- model_and_fig_data[[1]] %>% 
  st_drop_geometry() %>% 
  filter(!is.na(basin_sum_lake_area_m2)) %>% 
  mutate(basin_sum_stream_length_m = ifelse(is.na(basin_sum_stream_length_m), 0, basin_sum_stream_length_m)) %>% 
  mutate(log10_basin_sum_stream_length_m = log10(basin_sum_stream_length_m+1),
         log10_basin_sum_lake_area_m2 = log10(basin_sum_lake_area_m2),
         log10_basin_mean_lake_area_m2 = log10(basin_mean_lake_area_m2),
         log10_basin_area_m2 = log10(basin_area_m2),
         log10_basin_circum_m = log10(basin_circum_m),
         sqrt_basin_prop_arti = sqrt(basin_prop_arti),
         sqrt_basin_prop_agri = sqrt(basin_prop_agri))
  
summary(basin_df)

#Examine correlations
basin_preds <- basin_df %>% 
  select(-basin_id, -basin_ice_covered, -n_spec_basin,
         -centroid_x, -centroid_y,
         -basin_prop_arti, -basin_prop_agri,
         -basin_circum_m, -basin_area_m2, 
         -basin_sum_stream_length_m, -basin_sum_lake_area_m2, -basin_mean_lake_area_m2) %>% 
  select(-log10_basin_area_m2, -log10_basin_mean_lake_area_m2, -elev_mean_m, -log10_basin_circum_m)
         
basin_preds %>% 
  gather(variable, value) %>% 
  ggplot(aes(value))+
  geom_histogram()+
  facet_wrap(variable~., scales = "free")

basin_cor <- cor(basin_preds)

corrplot.mixed(basin_cor)

#Calculate variance inflation factors (VIF) (Zuur MEE 2009)
#Drop variables such that maximum VIF is not higher than 3 
corvif(basin_preds)

#Model basin species richness using poisson GAM
basin_gam_df <- basin_df %>% 
  select(basin_ice_covered, n_spec_basin) %>% 
  bind_cols(basin_preds)

summary(basin_gam_df)

m0 <- gam(n_spec_basin ~ s(elev_range_m) + basin_ice_covered + 
            s(log10_basin_sum_lake_area_m2) + s(log10_basin_sum_stream_length_m)+
            s(sqrt_basin_prop_arti)+s(sqrt_basin_prop_agri)+
            s(outlet_x, outlet_y), 
          family = "poisson", method = "REML", data = basin_gam_df, select = TRUE, gamma = 1.4)

summary(m0)

#Test for overdispersion
deviance(m0)/df.residual(m0)

#Refit with quasipoisson
m1 <- gam(n_spec_basin ~ s(elev_range_m) + basin_ice_covered + 
            s(log10_basin_sum_lake_area_m2) + s(log10_basin_sum_stream_length_m)+
            s(sqrt_basin_prop_arti)+s(sqrt_basin_prop_agri)+
            s(outlet_x, outlet_y), 
          family = "quasipoisson", method = "REML", data = basin_gam_df, select = TRUE, gamma = 1.4)

summary(m1)

#all terms significant, keep model

gamviz <- getViz(m1)

check(gamviz)

#funciton for adding model intercept on original scale to partial smooth plots
add_model_int <- function(x){round(exp(x + as.numeric(coef(m1)[1])), 0)} 

#print(plot(gamviz, allTerms = T), pages = 1)

p1 <- plot(sm(gamviz, 1))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = add_model_int, limits = c(-1.2, 1.2))+
  xlab("Elevation range (m)")+
  ylab("Basin richness")+
  theme_pub

p2 <- plot(sm(gamviz, 2))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = add_model_int, limits = c(-1.2, 1.2))+
  xlab(expression(log[10]*"(basin lake area [m"^{2}*"])"))+
  ylab("Basin richness")+
  theme_pub

p3 <- plot(sm(gamviz, 3))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = add_model_int, limits = c(-1.2, 1.2))+
  xlab(expression(log[10]*"(basin stream length + 1 [m])"))+
  ylab("Basin richness")+
  theme_pub

p4 <- plot(sm(gamviz, 4))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = add_model_int, limits = c(-1.2, 1.2))+
  xlab(expression(sqrt("prop. artificial area")))+
  ylab("Basin richness")+
  theme_pub

p5 <- plot(sm(gamviz, 5))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = add_model_int, limits = c(-1.2, 1.2))+
  xlab(expression(sqrt("prop. agricultural area")))+
  ylab("Basin richness")+
  theme_pub

p6 <- plot(sm(gamviz, 6))+
  l_fitRaster(pTrans = function(.p) ifelse(.p < 0.05, 1, 0.5), show.legend = FALSE)+
  scale_fill_gradient2(low = "blue", high = "orange", mid = "grey", na.value = "white")+
  l_points()+
  ggtitle(NULL)+
  guides(fill = guide_colorbar(title = "Basin richness"))+ 
  xlab("X")+
  ylab("Y")+
  scale_y_continuous(labels = function(val){round(val/10^5, 1)}) +
  scale_x_continuous(labels = function(val){round(val/10^4, 1)}) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_text(colour = "white"), 
        strip.background = element_rect(fill = "white"))

p7 <- plot(pterm(gamviz, 1))+
  l_fitPoints()+
  l_ciBar()+
  scale_y_continuous(labels = add_model_int, limits = c(-1.2, 1.2))+
  scale_x_discrete(labels = c("Not ice covered", "Ice covered"))+
  xlab("Ice covered")+
  ylab("Basin richness")+
  theme_pub

p8 <- data.frame(basin_gam_df, pred = predict(m1, type = "response")) %>% 
  ggplot(aes(n_spec_basin, pred))+
  geom_abline(slope = 1, intercept = 0, linetype = 3)+
  geom_point(alpha = 0.25)+
  xlim(0, 42)+
  ylim(0, 42)+
  ylab("Predicted basin richness")+
  xlab("Observed basin richness")+
  theme_pub
  
basin_gam_allplots <- gridPrint(p1, p2, p3, p4, p5, p6, p7, p8, ncol=2)

ggsave(paste0(getwd(), "/figures/basin_gam.png"), basin_gam_allplots, units = "mm", width = 174, height = 234)

#Lake level modeling

#Set NA network metrics to zero (not connected), except for Mjelssø which was outside area and thus not evaluated
lake_df <- model_and_fig_data[[2]] %>% 
  left_join(network_attr) %>% 
  mutate_at(vars(degree, degree_weight, between), ~ifelse(is.na(.), 0, .)) %>% 
  mutate_at(vars(degree, degree_weight, between), ~ifelse(site_id == 15, NA, .)) %>% 
  mutate(age = year-year_established) %>% 
  bind_cols(data.frame(st_coordinates(.))) %>% 
  rename(lake_x = X, lake_y = Y) %>% 
  filter(!is.na(basin_id)) %>% 
  select(site_id, lake_x, lake_y, age, basin_id_fact, spec_proportion, n_spec_basin, 
         elevation, lake_area_log10, lake_circum_log10, lake_stream_connect_binary, lake_stream_connect, lake_ice_covered,
         degree, degree_weight, between, zmean_m, zmax_m, plant_area_perc, plant_vol_perc,
         secchi_depth_m, pH_pH, alk_meq_l, chla_ug_l, tp_mg_l, tn_mg_l, outlet_dist_m) %>% 
  st_drop_geometry()

summary(lake_df)

#Examine distribution and correlations
lake_preds <- lake_df %>% 
  select(-site_id, -age, -basin_id_fact, -spec_proportion, -n_spec_basin, -lake_ice_covered, -lake_stream_connect_binary) %>% 
  mutate_at(vars(between, degree, degree_weight, lake_stream_connect,plant_vol_perc, plant_area_perc), list(~sqrt(.))) %>% 
  mutate_at(vars(chla_ug_l, zmean_m, zmax_m, tn_mg_l, tp_mg_l, outlet_dist_m), list(~log10(.))) %>% 
  select(-lake_circum_log10, -plant_vol_perc, -zmean_m, -degree, -secchi_depth_m) #remove variables based on VIF

lake_preds %>% 
  gather(variable, value) %>% 
  ggplot(aes(value))+
  geom_histogram()+
  facet_wrap(variable~., scales = "free")

cond_preds_cor <- cor(lake_preds, use = "pairwise.complete.obs")
corrplot.mixed(cond_preds_cor)

#Calculate variance inflation factors (VIF)
#Drop variables such that maximum VIF is not higher than 3 
corvif(lake_preds)

#Binomial model
#All basin with <10 species excluded
lake_model_df <- lake_preds %>% 
  bind_cols(select(lake_df, site_id, age, basin_id_fact, spec_proportion, n_spec_basin, lake_ice_covered, lake_stream_connect_binary)) %>% 
  filter(n_spec_basin >= 10) %>% 
  select(-tn_mg_l, -tp_mg_l, -plant_area_perc, -zmax_m, -alk_meq_l) 

summary(lake_model_df)

#Split data into two sets: natural lakes or lakes more than 100 years old and new lakes (less than 100 years old)
natural_lakes <- lake_model_df %>% 
  filter(is.na(age) | age > 100) %>% 
  select(-age) %>% 
  na.omit()

new_lakes <- lake_model_df %>% 
  filter(age < 100) %>% 
  na.omit()

#Check if data from plant monitoring has significant effects or if they should be dropped to include more observations
#(variable are dropped from lake_model_df above)
# include_plant_data <- gam(spec_proportion ~ s(elevation) + s(lake_area_log10) + s(lake_stream_connect) +
#                             s(degree_weight)+s(between)+
#                             s(pH_pH) + s(chla_ug_l)+
#                             s(outlet_dist_m)+
#                             s(tp_mg_l)+s(tn_mg_l)+s(plant_area_perc)+s(zmax_m)+s(alk_meq_l)+
#                             #s(basin_id_fact, bs = "re")+
#                             lake_ice_covered+lake_stream_connect_binary+
#                             s(lake_x, lake_y),
#                           family = "binomial",
#                           weights=n_spec_basin,
#                           method = "REML",
#                           data = natural_lakes, select = TRUE, gamma = 1.4)
# summary(include_plant_data)

#No apparent signifincant effects of tn_mg_l, tp_mg_l, plant_area_perc, zmax_m and alk_meq_l
#Remove these variables to get more observations for modeling

#Fit gam again with random intercept
lake_m0 <- gam(spec_proportion ~ s(elevation) + s(lake_area_log10) + s(lake_stream_connect) +
                 s(degree_weight)+s(between)+
                 s(pH_pH) + s(chla_ug_l)+
                 s(outlet_dist_m)+
                 s(basin_id_fact, bs = "re")+
                 lake_ice_covered+lake_stream_connect_binary+
                 s(lake_x, lake_y),
               family = "binomial", 
               weights=n_spec_basin, 
               method = "REML", 
               data = natural_lakes, select = TRUE, gamma = 1.4)
summary(lake_m0)
gam.check(lake_m0)

#Test for under-overdispersion, values close to 1 indicate no problems, also examine diagnostic plots
deviance(lake_m0)/df.residual(lake_m0)
#gam.check(lake_m0)

lake_m1 <- gam(spec_proportion ~ s(elevation) + s(lake_area_log10) + s(lake_stream_connect) +
                 s(degree_weight)+s(between)+
                 s(pH_pH) + s(chla_ug_l)+
                 s(outlet_dist_m)+
                 s(basin_id_fact, bs = "re")+
                 lake_ice_covered+lake_stream_connect_binary+
                 s(lake_x, lake_y),
               family = "quasibinomial", 
               weights=n_spec_basin, 
               method = "REML", 
               data = natural_lakes, select = TRUE, gamma = 1.4)

summary(lake_m1)
#gam.check(lake_m1)

#Quasi distribution improves diagnostic plots

#Fit without terms which were penalized to zero
lake_m2 <- gam(spec_proportion ~ s(elevation) + s(lake_area_log10) +
                 s(between)+
                 s(pH_pH) + s(chla_ug_l)+
                 s(outlet_dist_m)+
                 lake_ice_covered+lake_stream_connect_binary+
                 s(lake_x, lake_y),
               family = "quasibinomial", 
               weights=n_spec_basin, 
               method = "REML", 
               data = natural_lakes, select = TRUE, gamma = 1.4)

summary(lake_m2)

#Fit without non-significant parametric terms 
lake_m3 <- gam(spec_proportion ~ s(elevation) + s(lake_area_log10) +
                 s(between)+
                 s(pH_pH) + s(chla_ug_l)+
                 s(outlet_dist_m)+
                 s(lake_x, lake_y),
               family = "quasibinomial", 
               weights=n_spec_basin, 
               method = "REML", 
               data = natural_lakes, select = TRUE, gamma = 1.4)

summary(lake_m3)

gam.check(lake_m3)

#Keep model 3

#Use model to predict richness for new lakes
new_lakes_preds <- new_lakes %>%
  mutate(gam_preds = predict(lake_m3, newdata = ., type = "response"),
         resid_preds = gam_preds-spec_proportion)

train_mae <- mean(abs(fitted(lake_m3)-natural_lakes$spec_proportion))
train_rmse <- sqrt(mean((fitted(lake_m3)-natural_lakes$spec_proportion)^2))
pred_mae <- mean(abs(new_lakes_preds$gam_preds-new_lakes_preds$spec_proportion))
pred_rmse <- sqrt(mean((new_lakes_preds$gam_preds-new_lakes_preds$spec_proportion)^2))

lake_gamviz <- getViz(lake_m3)

#Add model intercept on original scale
lake_model_int <- function(x){round(binomial()$linkinv(x + as.numeric(coef(lake_m2)[1])), 2)} 

#print(plot(lake_gamviz, allTerms = T), pages = 1)

#Create plots
lake_p1 <- plot(sm(lake_gamviz, 1))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = lake_model_int, limits = c(-2.5, 0.9))+
  xlab("Elevation (m)")+
  ylab("Lake:basin prop.")+
  theme_pub

lake_p2 <- plot(sm(lake_gamviz, 2))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = lake_model_int, limits = c(-2.5, 0.9))+
  xlab(expression(log[10]*"(lake area [m"^{2}*"])"))+
  ylab("Lake:basin prop.")+
  theme_pub

lake_p3 <- plot(sm(lake_gamviz, 3))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = lake_model_int, limits = c(-2.5, 0.9))+
  xlab(expression(sqrt("Betweenness score")))+
  ylab("Lake:basin prop.")+
  theme_pub

lake_p4 <- plot(sm(lake_gamviz, 4))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = lake_model_int, limits = c(-2.5, 0.9))+
  xlab("pH")+
  ylab("Lake:basin prop.")+
  theme_pub

lake_p5 <- plot(sm(lake_gamviz, 5))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = lake_model_int, limits = c(-2.5, 0.9))+
  xlab(expression(log[10]*"(Chl. a ["*mu*g*L^{-1}*"])"))+
  ylab("Lake:basin prop.")+
  theme_pub

lake_p6 <- plot(sm(lake_gamviz, 6))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = lake_model_int, limits = c(-2.5, 0.9))+
  xlab(expression(sqrt("Distance to basin outlet (m)")))+
  ylab("Lake:basin prop.")+
  theme_pub

lake_p7 <- plot(sm(lake_gamviz, 7))+
  l_fitRaster(pTrans = function(.p) ifelse(.p < 0.05, 1, 0.5), show.legend = FALSE)+
  l_points(shape=19, alpha = 0.5, size = 0.2)+
  scale_fill_gradient2(low = "blue", high = "orange", mid = "grey", na.value = "white")+
  ggtitle(NULL)+
  guides(fill = guide_colorbar(title = "Lake:basin prop."))+ 
  xlab("X")+
  ylab("Y")+
  scale_y_continuous(labels = function(val){round(val/10^5, 1)}) +
  scale_x_continuous(labels = function(val){round(val/10^4, 1)}) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_text(colour = "white"), 
        strip.background = element_rect(fill = "white"))

lake_p8 <- data.frame(natural_lakes, pred = predict(lake_m2, type = "response")) %>% 
  ggplot(aes(spec_proportion, pred, col = "Natural"))+
  geom_abline(slope = 1, intercept = 0, linetype = 3)+
  geom_point()+
  geom_point(data = new_lakes_preds, aes(spec_proportion, gam_preds, col = "New"))+
  scale_color_manual(values = c("coral", viridisLite::viridis(1, begin = 0.5, end = 0.6)), name = "Lake group")+
  xlim(0, 0.7)+
  ylim(0, 0.7)+
  ylab("Predicted")+
  xlab("Observed")+
  theme_pub+
  theme(legend.position = c(0.3, 0.76))

lake_gam_allplots <- gridPrint(lake_p1, lake_p2, lake_p3, lake_p4, lake_p5, lake_p6, lake_p7, lake_p8, ncol=2)

ggsave(paste0(getwd(), "/figures/lake_gam.png"), lake_gam_allplots, units = "mm", width = 174, height = 234)

#Analysis of residuals from new lake observations
mod_resid <- lm(resid_preds~age*lake_stream_connect_binary, data = new_lakes_preds)
#plot(mod_resid)
summary(mod_resid)
anova(mod_resid)

residual_plot <- new_lakes_preds %>%
  mutate(connect_label = ifelse(lake_stream_connect_binary == 0, "Not connected", "Connected")) %>% 
  ggplot(aes(age, resid_preds, shape = connect_label))+
  geom_hline(yintercept = 0, linetype = 3)+
  geom_point()+
  scale_shape_manual(values = c(1, 19))+
  xlab("Lake age (years)")+
  ylab("Lake model residuals (pred. - obs.)")+
  theme_pub+
  theme(legend.title = element_blank(), legend.position = c(0.75, 0.15))

ggsave(paste0(getwd(), "/figures/lake_resids.png"), residual_plot, units = "mm", width = 84, height = 84)

#Load and add to basin and lake species lists for species specific analysis
#Only include lake and basins used in modeling
# bas <- read_csv(paste0(getwd(), "/data_raw/basin_species_list.csv"))
# lak <- read_csv(paste0(getwd(), "/data_raw/lake_species_list.csv")) %>% 
#   filter(!is.na(fish_id))
# 
# lake_model_df %>% 
#   select(site_id, basin_id = basin_id_fact) %>% 
#   left_join(lak) %>%
#   write_csv(paste0(getwd(), "/data_raw/lake_species_list_edit.csv"))
# 
# bas %>% 
#   filter(basin_id %in% pull(lake_model_df, basin_id_fact)) %>% 
#   write_csv(paste0(getwd(), "/data_raw/basin_species_list_edit.csv"))
