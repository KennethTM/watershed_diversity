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

#basin summary stats note
# model_and_fig_data[[1]] %>% 
#   st_drop_geometry() %>% 
#   filter(!is.na(basin_sum_lake_area_m2)) %>% 
#   mutate(basin_sum_stream_length_m = ifelse(is.na(basin_sum_stream_length_m), 0, basin_sum_stream_length_m))  %>% 
#   tbl_df() %>% 
#   select(elev_range_m, basin_prop_agri, basin_prop_arti, basin_sum_stream_length_m, basin_sum_lake_area_m2) %>% 
#   summary()
  

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

#Test for overdispersion, close to 1 keep model familiy
deviance(m0)/df.residual(m0)

#all terms significant, keep model

gamviz <- getViz(m0)

check(gamviz)

#funciton for adding model intercept on original scale to partial smooth plots
add_model_int <- function(x){round(exp(x + as.numeric(coef(m0)[1])), 0)} 

#print(plot(gamviz, allTerms = T), pages = 1)

p1 <- plot(sm(gamviz, 1))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = add_model_int, limits = c(-1.3, 1.3))+
  xlab("Elevation range (m)")+
  ylab("Basin richness")+
  theme_pub

p2 <- plot(sm(gamviz, 2))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = add_model_int, limits = c(-1.3, 1.3))+
  xlab(expression(log[10]*"(basin lake area [m"^{2}*"])"))+
  ylab("Basin richness")+
  theme_pub

p3 <- plot(sm(gamviz, 3))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = add_model_int, limits = c(-1.3, 1.3))+
  xlab(expression(log[10]*"(basin stream length + 1 [m])"))+
  ylab("")+
  theme_pub

p4 <- plot(sm(gamviz, 4))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = add_model_int, limits = c(-1.3, 1.3))+
  xlab(expression(sqrt("prop. artificial area")))+
  ylab("Basin richness")+
  theme_pub

p5 <- plot(sm(gamviz, 5))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = add_model_int, limits = c(-1.3, 1.3))+
  xlab(expression(sqrt("prop. agricultural area")))+
  ylab("")+
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
  scale_y_continuous(labels = add_model_int, limits = c(-1.3, 1.3))+
  scale_x_discrete(labels = c("Not ice covered", "Ice covered"))+
  xlab("")+
  ylab("")+
  theme_pub

p8 <- data.frame(basin_gam_df, pred = predict(m0, type = "response")) %>% 
  ggplot(aes(n_spec_basin, pred))+
  geom_abline(slope = 1, intercept = 0, linetype = 3)+
  geom_point(alpha = 0.25)+
  xlim(0, 42)+
  ylim(0, 42)+
  ylab("Predicted")+
  xlab("Observed")+
  theme_pub
  
basin_plot_list <- lapply(list(p2, p3, p4, p5, p1, p7, p6), function(x){x$ggObj})

basin_gam_plot <- wrap_plots(basin_plot_list) + p8 + plot_layout(ncol = 2) + plot_annotation(tag_levels = "A")

ggsave(paste0(getwd(), "/figures/basin_gam.png"), basin_gam_plot, units = "mm", width = 174, height = 234)
ggsave(paste0(getwd(), "/figures/basin_gam.svg"), basin_gam_plot, units = "mm", width = 174, height = 234)

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
         degree, degree_weight, between, 
         #degree_norm, between_norm, 
         zmean_m, zmax_m, plant_area_perc, plant_vol_perc,
         secchi_depth_m, pH_pH, alk_meq_l, chla_ug_l, tp_mg_l, tn_mg_l, outlet_dist_m) %>% 
  st_drop_geometry()

summary(lake_df)

#Write model data incl names
# lake_names <- readRDS(paste0(getwd(), "/data_raw/lake_names.rds"))
# lake_df %>%
#   left_join(lake_names) %>%
#   write.csv2(paste0(getwd(), "/data_raw/all_model_data_names_19082020.csv"))

#Examine distribution and correlations
lake_preds <- lake_df %>% 
  select(-site_id, -age, -basin_id_fact, -spec_proportion, -n_spec_basin, -lake_ice_covered, -lake_stream_connect_binary) %>% 
  mutate_at(vars(between, degree, degree_weight, lake_stream_connect,plant_vol_perc, plant_area_perc), list(~log10(1+.))) %>% 
  mutate_at(vars(chla_ug_l, zmean_m, zmax_m, tn_mg_l, tp_mg_l, outlet_dist_m), list(~log10(.))) %>% 
  select(-lake_circum_log10, -plant_vol_perc, -zmean_m, -degree_weight, -secchi_depth_m, -degree) #remove variables based on VIF

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
#All basins with <10 species excluded
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
# include_plant_data <- gam(spec_proportion ~ s(elevation) + s(lake_area_log10) +
#                             s(degree_weight)+s(between)+
#                             s(pH_pH) + s(chla_ug_l)+
#                             s(outlet_dist_m)+
#                             s(tp_mg_l)+s(tn_mg_l)+s(plant_area_perc)+s(zmax_m)+s(alk_meq_l)+
#                             #s(basin_id_fact, bs = "re")+
#                             #lake_ice_covered+lake_stream_connect_binary+
#                             s(lake_x, lake_y),
#                           family = "binomial",
#                           weights=n_spec_basin,
#                           method = "REML",
#                           data = natural_lakes, select = TRUE, gamma = 1.4)
# summary(include_plant_data)

#No apparent signifincant effects of tn_mg_l, tp_mg_l, plant_area_perc, zmax_m and alk_meq_l
#Remove these variables to get more observations for modeling

#Fit gam again with random intercept
lake_m0 <- gam(spec_proportion ~ s(elevation) + s(lake_area_log10) +
                 s(lake_stream_connect)+s(between)+
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
check(getViz(lake_m0))

#Test for under-overdispersion, values close to 1 indicate no problems, also examine diagnostic plots
deviance(lake_m0)/df.residual(lake_m0)
check(getViz(lake_m0))

lake_m1 <- gam(spec_proportion ~ s(elevation) + s(lake_area_log10) +
                 s(lake_stream_connect)+s(between)+
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
check(getViz(lake_m1))

#Quasi-distribution improves diagnostic plots

#Fit without terms which were penalized to zero
lake_m2 <- gam(spec_proportion ~ s(elevation) + s(lake_area_log10) +
                 +s(between)+
                 s(pH_pH) + s(chla_ug_l)+
                 s(outlet_dist_m)+
                 lake_ice_covered+lake_stream_connect_binary,
               family = "quasibinomial", 
               weights=n_spec_basin, 
               method = "REML", 
               data = natural_lakes, select = TRUE, gamma = 1.4)

summary(lake_m2)

#Fit without non-significant parametric terms 
lake_m3 <- gam(spec_proportion ~s(elevation) + s(lake_area_log10) +
                 +s(between)+
                 s(pH_pH) + s(chla_ug_l)+
                 s(outlet_dist_m),
               family = "quasibinomial", 
               weights=n_spec_basin, 
               method = "REML", 
               data = natural_lakes, select = TRUE, gamma = 1.4)

summary(lake_m3)

check(getViz(lake_m3))

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
  scale_y_continuous(labels = lake_model_int, limits = c(-2.6, 1.3))+
  xlab("Elevation (m)")+
  ylab("Richness ratio")+
  theme_pub

lake_p2 <- plot(sm(lake_gamviz, 2))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = lake_model_int, limits = c(-2.6, 1.3))+
  xlab(expression(log[10]*"(lake area [m"^{2}*"])"))+
  ylab("")+
  theme_pub

lake_p3 <- plot(sm(lake_gamviz, 3))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = lake_model_int, limits = c(-2.6, 1.3))+
  xlab(expression(sqrt("Betweenness")))+
  ylab("Richness ratio")+
  theme_pub

lake_p4 <- plot(sm(lake_gamviz, 4))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = lake_model_int, limits = c(-2.6, 1.3))+
  xlab("pH")+
  ylab("Richness ratio")+
  theme_pub

lake_p5 <- plot(sm(lake_gamviz, 5))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = lake_model_int, limits = c(-2.6, 1.3))+
  xlab(expression(log[10]*"(Chl. a ["*mu*g*L^{-1}*"])"))+
  ylab("")+
  theme_pub

lake_p6 <- plot(sm(lake_gamviz, 6))+
  l_ciPoly()+
  l_fitLine()+
  l_rug()+
  scale_y_continuous(labels = lake_model_int, limits = c(-2.6, 1.3))+
  xlab(expression(sqrt("Distance to basin outlet (m)")))+
  ylab("")+
  theme_pub

# lake_p7 <- plot(sm(lake_gamviz, 7))+
#   l_fitRaster(pTrans = function(.p) ifelse(.p < 0.05, 1, 0.5), show.legend = FALSE)+
#   l_points(shape=19, alpha = 0.5, size = 0.2)+
#   scale_fill_gradient2(low = "blue", high = "orange", mid = "grey", na.value = "white")+
#   ggtitle(NULL)+
#   guides(fill = guide_colorbar(title = "Lake:basin prop."))+ 
#   xlab("X")+
#   ylab("Y")+
#   scale_y_continuous(labels = function(val){round(val/10^5, 1)}) +
#   scale_x_continuous(labels = function(val){round(val/10^4, 1)}) +
#   theme_bw() + 
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         axis.text = element_text(colour = "white"), 
#         strip.background = element_rect(fill = "white"))

lake_p8 <- data.frame(natural_lakes, pred = predict(lake_m2, type = "response")) %>% 
  ggplot(aes(spec_proportion, pred, col = "Natural"))+
  geom_abline(slope = 1, intercept = 0, linetype = 3)+
  geom_point()+
  geom_point(data = new_lakes_preds, aes(spec_proportion, gam_preds, col = "New"))+
  scale_color_manual(values = c("coral", viridisLite::viridis(1, begin = 0.5, end = 0.6)), name = "Lake group")+
  xlim(0, 1)+
  ylim(0, 1)+
  ylab("Predicted")+
  xlab("Observed")+
  theme_pub+
  theme(legend.position =  c(0.80, 0.30),
        legend.background = element_rect(colour = "black", size = 0.25)) #c(0.3, 0.76)

lake_plot_list <- lapply(list(lake_p4, lake_p6, lake_p1, lake_p2, lake_p3, lake_p5), function(x){x$ggObj})

lake_gam_plot <- wrap_plots(lake_plot_list) + lake_p8 + plot_layout(ncol = 2) + plot_annotation(tag_levels = "A")

ggsave(paste0(getwd(), "/figures/lake_gam.png"), lake_gam_plot, units = "mm", width = 174, height = 234)
ggsave(paste0(getwd(), "/figures/lake_gam.svg"), lake_gam_plot, units = "mm", width = 174, height = 234)

#Analysis of residuals from new lake observations
mod_resid_0 <- lm(resid_preds~age*lake_stream_connect_binary-1, data = new_lakes_preds)
mod_resid_1 <- lm(resid_preds~age+lake_stream_connect_binary-1, data = new_lakes_preds)
anova(mod_resid_1, mod_resid_0)
#plot(mod_resid)
summary(mod_resid_1)

residual_plot <- new_lakes_preds %>%
  mutate(connect_label = ifelse(lake_stream_connect_binary == 0, "Not connected", "Connected")) %>% 
  ggplot(aes(age, resid_preds, shape = connect_label))+
  geom_hline(yintercept = 0, linetype = 3)+
  geom_point(col = viridisLite::viridis(1, begin = 0.5, end = 0.6))+
  scale_shape_manual(values = c(1, 19))+
  xlab("Lake age (years)")+
  ylab("Lake model residuals (pred. - obs.)")+
  theme_pub+
  theme(legend.title = element_blank(), legend.position = "bottom")

ggsave(paste0(getwd(), "/figures/lake_resids.png"), residual_plot, units = "mm", width = 84, height = 84)
ggsave(paste0(getwd(), "/figures/lake_resids.svg"), residual_plot, units = "mm", width = 84, height = 84)

#t-test of residuals and lake ages
hist(new_lakes_preds$resid_preds)
t.test(new_lakes_preds$resid_preds)

lake_spec_test <- bind_rows(add_column(natural_lakes, system = "natural"), 
          add_column(new_lakes, system = "new")) %>% 
  mutate(n_spec_lake = spec_proportion*n_spec_basin) 

hist(lake_spec_test[lake_spec_test$system == "new",]$n_spec_lake)
hist(lake_spec_test[lake_spec_test$system == "natural",]$n_spec_lake)
wilcox.test(n_spec_lake~system, data = lake_spec_test)

hist(lake_spec_test[lake_spec_test$system == "new",]$spec_proportion)
hist(lake_spec_test[lake_spec_test$system == "natural",]$spec_proportion)
wilcox.test(spec_proportion~system, data = lake_spec_test)

#richness_ratio_test <- glm(spec_proportion~system, data = lake_spec_test, weights = n_spec_basin, family="binomial")
#summary(richness_ratio_test)

#Load and add to basin and lake species lists for species specific analysis
#Only include lake and basins used in modeling
fish_unique_edit <- read_xlsx(paste0(getwd(), "/data_raw/", "fish_unique_edit_EK.xlsx")) %>% 
  select(name = name_to_use, name_novana = name_local_novana, name_atlas = latin_and_atlas,
         fish_id = ID, action = `how(0=do_nothing)(1=remove_species)(2=remove_lake)`) %>% 
  filter(action == 0) %>% 
  select(name_atlas, fish_id) %>% 
  distinct() %>% 
  slice(-25)
  
bas <- read_csv(paste0(getwd(), "/data_raw/basin_species_list.csv"))

lak <- read_csv(paste0(getwd(), "/data_raw/lake_species_list.csv")) 

lak_sub <- bind_rows(add_column(natural_lakes, system = "natural"), 
                     add_column(new_lakes, system = "new")) %>% 
  mutate(basin_id = as.numeric(as.character(basin_id_fact))) %>% 
  select(system, basin_id, site_id) %>% 
  left_join(select(lak, site_id, fish_id)) %>% 
  distinct()

bas_sub <- bas %>%
  select(basin_id, fish_id) %>% 
  filter(basin_id %in% lak_sub$basin_id) %>% 
  distinct()

lak_per_bas <- lak_sub %>% 
  select(system, basin_id, site_id) %>% 
  distinct() %>% 
  group_by(system, basin_id) %>% 
  summarise(n_lake = n())

lak_per_spec <- lak_sub %>%
  group_by(system, fish_id) %>%
  summarise(n_lake_spec = n())

bas_per_spec <- bas_sub %>%
  group_by(fish_id) %>%
  summarise(n_bas_spec = n())

new_lake_freq <- left_join(bas_sub, lak_sub) %>%
  filter(system == "new" | is.na(system)) %>% 
  group_by(fish_id, basin_id) %>% 
  summarise(n_occur = sum(!is.na(site_id))) %>%
  left_join(filter(lak_per_bas, system == "new")) %>%
  na.omit() %>% 
  summarise(spec_mean = mean(n_occur/n_lake)) %>% 
  left_join(fish_unique_edit) 

nat_lake_freq <- left_join(bas_sub, lak_sub) %>%
  filter(system == "natural" | is.na(system)) %>% 
  group_by(fish_id, basin_id) %>% 
  summarise(n_occur = sum(!is.na(site_id))) %>%
  left_join(filter(lak_per_bas, system == "natural")) %>%
  na.omit() %>% 
  summarise(spec_mean = mean(n_occur/n_lake)) %>% 
  left_join(fish_unique_edit) 

spec_freq_plot <- bind_rows(add_column(nat_lake_freq, system = "natural"), 
                            add_column(new_lake_freq, system = "new")) %>% 
  left_join(lak_per_spec) %>% 
  left_join(bas_per_spec) %>%
  na.omit() %>% 
  mutate(label = gsub("_", " ", name_atlas),
         lake_cat = ifelse(system == "natural", "Natural", "New"),
         lake_cat_fact = factor(ifelse(lake_cat == "Natural", "Natural", "New"), levels = c("New", "Natural")),
         label_n_bas = paste0(label, " (", n_bas_spec, ")"),
         n_lakes_sys = ifelse(lake_cat == "New", nrow(new_lakes), nrow(natural_lakes)),
         n_lake_spec_prop = n_lake_spec/n_lakes_sys*100) %>% 
  #ggplot(aes(x = reorder(label_n_bas, spec_mean), y = spec_mean, col = lake_cat_fact, size = n_lake_spec))+
  ggplot(aes(x = reorder(label_n_bas, spec_mean), y = spec_mean, col = lake_cat_fact, size = n_lake_spec_prop))+
  geom_point()+
  scale_colour_manual(values = c(viridisLite::viridis(1, begin = 0.5, end = 0.6), "coral"), name = "Lake age (years)")+
  scale_size(name = "Occurrences (%)", breaks = seq(0, 100, 25))+
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1))+
  coord_flip()+
  xlab("Species")+
  ylab("Average frequency")+
  theme(panel.grid.major.y = element_line(size = 0.5, colour = "grey"),
        legend.position = c(0.75, 0.24),
        axis.text.y = element_text(face = "italic"),
        legend.background = element_rect(colour = "black", size = 0.25))

ggsave(paste0(getwd(), "/figures/lake_spec_freq.png"), spec_freq_plot, units = "mm", width = 129, height = 150)
ggsave(paste0(getwd(), "/figures/lake_spec_freq.svg"), spec_freq_plot, units = "mm", width = 129, height = 150)

#lake stats table
bind_rows(add_column(natural_lakes, system = "natural"), 
          add_column(new_lakes, system = "new")) %>% 
  mutate(basin_id = as.numeric(as.character(basin_id_fact))) %>% 
  select(system, basin_id, site_id) %>% 
  left_join(lake_df) %>% 
  group_by(system) %>% 
  summarise_if(is.numeric, list("mean" = mean, "min" = min, "max" = max), na.rm = TRUE) %>% View()
  write_csv("lake_vars_table.csv")
