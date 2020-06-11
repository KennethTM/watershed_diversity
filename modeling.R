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

#test for overdispersion
deviance(m0)/df.residual(m0)

#refit with quasipoisson
m1 <- gam(n_spec_basin ~ s(elev_range_m) + basin_ice_covered + 
            s(log10_basin_sum_lake_area_m2) + s(log10_basin_sum_stream_length_m)+
            s(sqrt_basin_prop_arti)+s(sqrt_basin_prop_agri)+
            s(outlet_x, outlet_y), 
          family = "quasipoisson", method = "REML", data = basin_gam_df, select = TRUE, gamma = 1.4)

summary(m1)

gamviz <- getViz(m1)

#check(gamviz)

add_model_int <- function(x){round(exp(x + as.numeric(coef(m1)[1])), 0)} #add model intercept on original scale

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
  #l_fitRaster(show.legend = FALSE)+
  #l_pvRaster()
  #l_fitContour()+
  #l_fitRaster(alpha = 0.5)+
  #l_fitRaster(pTrans = function(.p) .p<0.05)+
  l_fitRaster(pTrans = function(.p) ifelse(.p < 0.05, 1, 0.5), show.legend = FALSE)+
  scale_fill_gradient2(low = "blue", high = "orange", mid = "grey", na.value = "white")+
  l_points()+
  #scale_fill_viridis_c(option = "B", na.value = "white", labels = add_model_int)+
  ggtitle(NULL)+
  guides(fill = guide_colorbar(title = "Basin richness"))+ 
  xlab("X")+
  ylab("Y")+
  theme_pub

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




#tjek dist til sea for søer
#gennemgå sømodel igen!!!
#lav ensartet preproc for basin og sø model
#drop basins med < 5 eller 10 arter




#Lake level data
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
  mutate_at(vars(between, degree, degree_weight, lake_stream_connect,plant_vol_perc, plant_area_perc), list(~log10(. + 1))) %>% 
  mutate_at(vars(chla_ug_l, zmean_m, zmax_m, tn_mg_l, tp_mg_l, outlet_dist_m), list(~log10(.))) %>% 
  select(-lake_circum_log10, -plant_vol_perc, -zmean_m, -degree, -secchi_depth_m) #remove variables based on VIF

lake_preds %>% 
  gather(variable, value) %>% 
  ggplot(aes(value))+
  geom_histogram()+
  facet_wrap(variable~., scales = "free")

cond_preds_cor <- cor(lake_preds, use = "pairwise.complete.obs")
corrplot.mixed(cond_preds_cor)

#Calculate variance inflation factors (VIF) (Zuur MEE 2009)
#Drop variables such that maximum VIF is not higher than 3 
corvif(lake_preds)

#Binomial model
lake_model_df <- lake_preds %>% 
  #mutate_all(scale) %>% 
  bind_cols(select(lake_df, site_id, age, basin_id_fact, spec_proportion, n_spec_basin, lake_ice_covered, lake_stream_connect_binary)) %>% 
  filter(n_spec_basin > 10) %>% 
  select(-tn_mg_l, -tp_mg_l, -plant_area_perc, -zmax_m, -alk_meq_l) #%>% #remove variable which are not important 

summary(lake_model_df)

natural_lakes <- lake_model_df %>% 
  #mutate(lake_age = ifelse(is.na(age), "natural", "artif")) %>% 
  filter(is.na(age) | age > 100) %>% 
  select(-age) %>% 
  na.omit()

new_lakes <- lake_model_df %>% 
  filter(age < 100) %>% 
  #select(-age) %>% 
  na.omit()

#fit gam with random intercept
lake_m0 <- gam(spec_proportion ~ s(elevation) + s(lake_area_log10) + s(lake_stream_connect) +
                 s(degree_weight)+s(between)+
                 s(pH_pH) + s(chla_ug_l)+
                 s(outlet_dist_m)+
                 #s(tp_mg_l)+s(tn_mg_l)+s(plant_area_perc)+s(zmax_m)+s(alk_meq_l)+
                 s(basin_id_fact, bs = "re")+lake_ice_covered+
                 lake_stream_connect_binary+
                 s(lake_x, lake_y),
               family = "binomial", 
               weights=n_spec_basin, 
               method = "REML", 
               data = natural_lakes, select = TRUE, gamma = 1.4)

summary(lake_m0)
#plot(lake_m0)

#test for overdispersion - close to 1 indicate no overdispersion
deviance(lake_m0)/df.residual(lake_m0)

#refit without terms penalized to zero
lake_m1 <- gam(spec_proportion ~ s(elevation) + s(lake_area_log10) +
                 #s(lake_stream_connect) +s(degree_weight)+
                 s(between)+
                 s(pH_pH) + s(chla_ug_l)+
                 s(outlet_dist_m)+
                 #s(tp_mg_l)+s(tn_mg_l)+s(plant_area_perc)+s(zmax_m)+s(alk_meq_l)+
                 #s(basin_id_fact, bs = "re")+
                 #lake_ice_covered+
                 lake_stream_connect_binary+
                 s(lake_x, lake_y),
               family = "quasibinomial", 
               weights=n_spec_basin, 
               method = "REML", 
               data = natural_lakes, select = TRUE, gamma = 1.4)

summary(lake_m1)

#refit without non-significant terms and update input data
lake_m2 <- gam(spec_proportion ~ 
                 s(elevation) + 
                 s(lake_area_log10) +
                 #s(lake_stream_connect) +
                 #s(degree_weight)+
                 s(between)+
                 s(pH_pH) +
                 s(chla_ug_l)+
                 s(outlet_dist_m)+
                 #s(tp_mg_l)+s(tn_mg_l)+s(plant_area_perc)+s(zmax_m)+s(alk_meq_l)+
                 #s(basin_id_fact, bs = "re")+
                 #lake_ice_covered+
                 #lake_stream_connect_binary+
                 s(lake_x, lake_y),
               family = "quasibinomial", weights=n_spec_basin, 
               method = "REML", 
               data = natural_lakes, select = TRUE, gamma = 1.4)

summary(lake_m2)
#plot(lake_m2)

new_lakes_preds <- new_lakes %>%
  mutate(gam_preds = predict(lake_m2, newdata = ., type = "response"),
         resid_preds = gam_preds-spec_proportion)

train_mae <- mean(abs(fitted(lake_m2)-natural_lakes$spec_proportion))
train_rmse <- sqrt(mean((fitted(lake_m2)-natural_lakes$spec_proportion)^2))
pred_mae <- mean(abs(new_lakes_preds$gam_preds-new_lakes_preds$spec_proportion))
pred_rmse <- sqrt(mean((new_lakes_preds$gam_preds-new_lakes_preds$spec_proportion)^2))

lake_gamviz <- getViz(lake_m2)

check(lake_gamviz)
#tmp[[4]] +xlim(0, 1)+geom_abline(intercept = 0, slope=1)

lake_model_int <- function(x){round(binomial()$linkinv(x + as.numeric(coef(lake_m2)[1])), 2)} #add model intercept on original scale

#print(plot(lake_gamviz, allTerms = T), pages = 1)

lake_p1 <- plot(sm(lake_gamviz, 1))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = lake_model_int, limits = c(-2.5, 0.7))+
  xlab("Elevation (m)")+
  ylab("Lake:basin prop.")+
  theme_pub

lake_p2 <- plot(sm(lake_gamviz, 2))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = lake_model_int, limits = c(-2.5, 0.7))+
  xlab(expression(log[10]*"(lake area [m"^{2}*"])"))+
  ylab("Lake:basin prop.")+
  theme_pub

lake_p3 <- plot(sm(lake_gamviz, 3))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = lake_model_int, limits = c(-2.5, 0.7))+
  xlab("pH")+
  ylab("Lake:basin prop.")+
  theme_pub

lake_p4 <- plot(sm(lake_gamviz, 4))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = lake_model_int, limits = c(-2.5, 0.7))+
  xlab(expression(log[10]*"(Chl. a ["*mu*g*L^{-1}*"])"))+
  ylab("Lake:basin prop.")+
  theme_pub

lake_p5 <- plot(sm(lake_gamviz, 5))+
  l_fitRaster(pTrans = function(.p) ifelse(.p < 0.05, 1, 0.5), show.legend = FALSE)+
  l_points(shape=19, alpha = 0.5, size = 0.2)+
  scale_fill_gradient2(low = "blue", high = "orange", mid = "grey", na.value = "white")+
  ggtitle(NULL)+
  guides(fill = guide_colorbar(title = "Lake:basin prop."))+ 
  xlab("X")+
  ylab("Y")+
  theme_pub

lake_p6 <- data.frame(natural_lakes, pred = predict(lake_m2, type = "response")) %>% 
  ggplot(aes(spec_proportion, pred))+
  geom_abline(slope = 1, intercept = 0, linetype = 3)+
  geom_point(col = "coral", alpha = 0.5)+
  geom_point(data = new_lakes_preds, aes(spec_proportion, gam_preds), alpha=0.5)+
  xlim(0, 1)+
  ylim(0, 1)+
  ylab("Predicted lake:basin prop.")+
  xlab("Observed lake:basin prop.")+
  theme_pub

lake_p7 <- new_lakes_preds %>%
  mutate(connect_label = ifelse(lake_stream_connect_binary == 0, "Not connected", "Connected")) %>% 
  ggplot(aes(age, resid_preds, shape = connect_label))+
  geom_hline(yintercept = 0, linetype = 3)+
  geom_point()+
  scale_shape_manual(values = c(1, 19))+
  xlab("Lake age (years)")+
  ylab("Residuals (pred. - obs.)")+
  theme_pub+
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.2))

lake_gam_allplots <- gridPrint(lake_p1, lake_p2, lake_p3, lake_p4, lake_p5, lake_p6, lake_p7, ncol=2)

ggsave(paste0(getwd(), "/figures/lake_gam.png"), lake_gam_allplots, units = "mm", width = 174, height = 234)

#Analysis of residuals 
summary(lm(resid_preds~age+lake_stream_connect_binary, data = new_lakes_preds))






# #Fit ordinary glm
# global_glm <- glm(spec_proportion ~ elevation + lake_area_log10 + lake_stream_connect + 
#                     #degree_weight+between+
#                     pH_pH + 
#                     #chla_ug_l+
#                     #tp_mg_l + tn_mg_l+plant_area_perc+zmax_m+alk_meq_l+
#                     #lake_ice_covered+
#                     lake_stream_connect_binary,
#                  weights=n_spec_basin, 
#                  data=natural_lakes, 
#                  family="binomial")
# summary(global_glm)
# 
# #Fit glmm with basin_id as random
# global_glmm <- glmer(spec_proportion ~ elevation + lake_area_log10 + lake_stream_connect + 
#                        #degree_weight+between+
#                        pH_pH + 
#                        #chla_ug_l+
#                        #tp_mg_l + tn_mg_l+plant_area_perc+zmax_m+alk_meq_l+
#                        #lake_ice_covered+
#                        lake_stream_connect_binary+(1|basin_id_fact),
#                   weights=n_spec_basin, 
#                   data=natural_lakes, 
#                   family="binomial")
# summary(global_glmm)
# 
# #Test for inclusion of random effect
# #p-value 0.0519. Use random effect as LRT test is conservative
# anova(global_glmm, global_glm)
# 
# #Refit glmm with better accuracy 
# glmm_0 <- update(global_glmm, nAGQ = 4)
#   
# summary(glmm_0)
# 
# #Check for overdspersion (residual deviance / df) should approximate 1 
# #https://esajournals.onlinelibrary.wiley.com/doi/full/10.1890/10-0340.1
# deviance(glmm_0)/df.residual(glmm_0) #deviance 242.9 in summary - same conclusion
# 
# #Another function, same conclusion
# #https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
# overdisp_fun <- function(model) {
#   rdf <- df.residual(model)
#   rp <- residuals(model,type="pearson")
#   Pearson.chisq <- sum(rp^2)
#   prat <- Pearson.chisq/rdf
#   pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
#   c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
# }
# 
# overdisp_fun(glmm_0)
# 
# #Backwards elimination of variables untill all terms are significant
# drop1(glmm_0, test = "Chisq")
# glmm_1 <- update(glmm_0, . ~ . -secchi_depth_m)
# 
# drop1(glmm_1, test = "Chisq")
# glmm_2 <- update(glmm_1, . ~ . -lake_dev_ind)
# 
# drop1(glmm_2, test = "Chisq")
# glmm_3 <- update(glmm_2, . ~ . -lake_ice_covered)
# 
# drop1(glmm_3, test = "Chisq")
# glmm_4 <- update(glmm_3, . ~ . -between)
# 
# drop1(glmm_4, test = "Chisq")
# glmm_5 <- update(glmm_4, . ~ . -degree)
# 
# drop1(glmm_5, test = "Chisq")
# glmm_6 <- update(glmm_5, . ~ . -chla_ug_l)
# 
# drop1(glmm_6, test = "Chisq")
# 
# #include squared terms
# glmm_7 <- update(glmm_6, . ~ . +pH_pH:alk_meq_l) #+I(pH_pH^2)
# drop1(glmm_7, test = "Chisq")
# 
# 
# #calculate goodness of fit
# # glm_6 <- glm(spec_proportion ~ elevation + pH_pH + basin_area_log10 + lake_area_log10 +  
# #                lake_stream_connect_binary + I(pH_pH^2) + I(lake_area_log10^2),
# #                   weights=n_spec_basin, 
# #                   data=model_df_scale, 
# #                   family="binomial")
# # 
# # library(piecewiseSEM)
# # rsquared(glm_6)
# 
# r.squaredGLMM(glmm_7)
# 
# r2.corr.mer <- function(m){
#   lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
#   summary(lmfit)$r.squared
# }
# r2.corr.mer(glmm_7)
# 
# #Validate model
# plot(glmm_7)
# 
# conf_intervals <- confint(glmm_7, method = "profile")
# 
# deviance(glmm_7)/df.residual(glmm_7) 
# 
# #Plots
# ph <- ggeffect(glmm_7, terms = "pH_pH [all]")
# elev <- ggeffect(glmm_7, terms = "elevation [all]")
# #basin_lake_area <- ggeffect(glmm_7, terms = "log10_basin_sum_lake_area_m2 [all]")
# #lake_area <- ggeffect(glmm_7, terms = "lake_area_log10 [all]")
# connect <- ggeffect(glmm_7, terms = "lake_stream_connect_binary")
# 
# ph_plot <- ggplot(data = tbl_df(ph), aes(x, predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "coral")+
#   geom_line() +
#   geom_point(data = model_df_scale, aes(pH_pH, spec_proportion), alpha = 0.2)+
#   xlab("pH")+
#   ylab("Richness")
# 
# elev_plot <- ggplot(data = tbl_df(elev), aes(x, predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "coral")+
#   geom_line() +
#   geom_point(data = model_df_scale, aes(elevation, spec_proportion), alpha = 0.2)+
#   xlab("Elevation")+
#   ylab("Richness")
# 
# basin_lake <- ggplot(data = tbl_df(basin_lake_area), aes(x, predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "coral")+
#   geom_line() +
#   geom_point(data = model_df_scale, aes(log10_basin_sum_lake_area_m2, spec_proportion), alpha = 0.2)+
#   xlab("Basin area")+
#   ylab("Richness")
# 
# lake_area_plot <- ggplot(data = tbl_df(lake_area), aes(x, predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "coral")+
#   geom_line() +
#   geom_point(data = model_df_scale, aes(lake_area_log10, spec_proportion), alpha = 0.2)+
#   xlab("Lake area")+
#   ylab("Richness")
# 
# connect_plot <- ggplot(data = tbl_df(connect), aes(x, predicted)) +
#   geom_linerange(aes(x = x, ymin = conf.low, ymax = conf.high), col = "coral", size = 2)+
#   geom_point(aes(y=predicted))+
#   geom_jitter(data = model_df_scale, aes(lake_stream_connect_binary, spec_proportion), alpha = 0.2,width = 0.25)+
#   xlab("Stream connect")+
#   ylab("Richness")
# 
# obs_pred_plot <- model_df_scale %>% 
#   mutate(fitted_values = fitted(glmm_5)) %>% 
#   ggplot(aes(fitted_values, spec_proportion)) +
#   geom_abline(slope=1, intercept = 0, linetype = 2)+
#   geom_point(alpha=0.2)+
#   ylim(0, 1)+
#   xlim(0, 1)+
#   ylab("Observed richness")+
#   xlab("Predicted richness")
# 
# glmer_plot <- ph_plot+elev_plot+basin_lake+lake_area_plot+connect_plot+obs_pred_plot+plot_layout(ncol = 2)
# 
# ggsave(paste0(getwd(), "/figures/glmer_plot.png"), glmer_plot, units = "mm", width = 174, height = 200)
# 
