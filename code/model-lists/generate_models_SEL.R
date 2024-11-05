## Create tibble of various model settings to iterate over
## For model selection, 24 models total (12 each linear & cubic spline)
## Each set of 3 includes a) no variant era, b) additive variant era term, 
## or c) variant era interaction term
# 1-3. Ct mean only
# 4-6. Ct mean and st.dev.
# 7-9. Ct mean and skewness 
# 10-12. Ct mean, st.dev. and skewness

models_basic <- tibble(
  group_vars = list(c()), gr_var = "gr_roll", direct_var = "growing", 
  model = list(c("sm_mean_ct",
                 "sm_mean_ct + var_era", 
                 "sm_mean_ct * var_era", 
                 "sm_mean_ct + sm_sd_ct", 
                 "(sm_mean_ct + sm_sd_ct) + var_era", 
                 "(sm_mean_ct + sm_sd_ct) * var_era", 
                 "sm_mean_ct + sm_skew_ct", 
                 "(sm_mean_ct + sm_skew_ct) + var_era", 
                 "(sm_mean_ct + sm_skew_ct) * var_era", 
                 "sm_mean_ct + sm_sd_ct + sm_skew_ct", 
                 "(sm_mean_ct + sm_sd_ct + sm_skew_ct) + var_era", 
                 "(sm_mean_ct + sm_sd_ct + sm_skew_ct) * var_era" 
                 )), 
  model_data = c("model_dat_daily"), 
  test_data = list(NULL), 
  plot_label = list(c("dm_", "dmw_", "dmv_", "dmsd_", "dmsdw_", "dmsdv_", "dmsk_", "dmskw_", "dmskv_", "dmsb_", "dmsbw_", "dmsbv_"))) %>% 
  unnest(c(model, plot_label)) %>%
  # mutate(group_vars = as.character(group_vars), 
  #        test_data = as.character(test_data))
  mutate(test_data = as.character(test_data))


models_cr <- tibble(
  group_vars = list(c()), gr_var = "gr_roll", direct_var = "growing", 
  model = list(c("s(sm_mean_ct, bs='cs')", 
                 "s(sm_mean_ct, bs='cs') + var_era", 
                 "s(sm_mean_ct, bs='cs', by=var_era)", 
                 "s(sm_mean_ct, bs='cs') + s(sm_sd_ct, bs='cs')", 
                 "s(sm_mean_ct, bs='cs') + s(sm_sd_ct, bs='cs') + var_era", 
                 "s(sm_mean_ct, bs='cs', by=var_era) + s(sm_sd_ct, bs='cs', by=var_era)", 
                 "s(sm_mean_ct, bs='cs') + s(sm_skew_ct, bs='cs')", 
                 "s(sm_mean_ct, bs='cs') + s(sm_skew_ct, bs='cs') + var_era", 
                 "s(sm_mean_ct, bs='cs', by=var_era) + s(sm_skew_ct, bs='cs', by=var_era)",
                 "s(sm_mean_ct, bs='cs') + s(sm_sd_ct, bs='cs') + s(sm_skew_ct, bs='cs')", 
                 "s(sm_mean_ct, bs='cs') + s(sm_sd_ct, bs='cs') + s(sm_skew_ct, bs='cs') + var_era", 
                 "s(sm_mean_ct, bs='cs', by=var_era) + s(sm_sd_ct, bs='cs', by=var_era) + s(sm_skew_ct, bs='cs', by=var_era)"
                 )), 
  model_data = c("model_dat_daily"), 
  test_data = list(NULL), 
  plot_label = list(c("dm_", "dmw_", "dmv_", "dmsd_", "dmsdw_", "dmsdv_", "dmsk_", "dmskw_", "dmskv_", "dmsb_", "dmsbw_", "dmsbv_"))) %>% 
  unnest(c(model, plot_label)) %>%
  mutate(plot_label = str_c(plot_label, "cr_")) %>% 
  # mutate(group_vars = as.character(group_vars), 
  #        test_data = as.character(test_data))
  mutate(test_data = as.character(test_data))


## Thin plate regression splines, not included in final model selection set
# models_tp <- tibble(
#   group_vars = list(c()), gr_var = "gr_roll", direct_var = "growing", 
#   model = list(c("s(sm_mean_ct, bs='ts')", 
#                  "s(sm_mean_ct, bs='ts') + var_era", 
#                  "s(sm_mean_ct, bs='ts', by=var_era)", 
#                  "s(sm_mean_ct, bs='ts') + s(sm_sd_ct, bs='ts')", 
#                  "s(sm_mean_ct, bs='ts') + s(sm_sd_ct, bs='ts') + var_era", 
#                  "s(sm_mean_ct, bs='ts', by=var_era) + s(sm_sd_ct, bs='ts', by=var_era)", 
#                  "s(sm_mean_ct, bs='ts') + s(sm_skew_ct, bs='ts')", 
#                  "s(sm_mean_ct, bs='ts') + s(sm_skew_ct, bs='ts') + var_era", 
#                  "s(sm_mean_ct, bs='ts', by=var_era) + s(sm_skew_ct, bs='ts', by=var_era)",
#                  "s(sm_mean_ct, bs='ts') + s(sm_sd_ct, bs='ts') + s(sm_skew_ct, bs='ts')", 
#                  "s(sm_mean_ct, bs='ts') + s(sm_sd_ct, bs='ts') + s(sm_skew_ct, bs='ts') + var_era", 
#                  "s(sm_mean_ct, bs='ts', by=var_era) + s(sm_sd_ct, bs='ts', by=var_era) + s(sm_skew_ct, bs='ts', by=var_era)"
#                  )), 
#   model_data = c("model_dat_daily"), 
#   test_data = list(NULL), 
#   plot_label = list(c("dm_", "dmw_", "dmv_", "dmsd_", "dmsdw_", "dmsdv_", "dmsk_", "dmskw_", "dmskv_", "dmsb_", "dmsbw_", "dmsbv_"))) %>% 
#   unnest(c(model, plot_label)) %>%
#   mutate(plot_label = str_c(plot_label, "tp_")) %>% 
#   # mutate(group_vars = as.character(group_vars), 
#   #        test_data = as.character(test_data))
#   mutate(test_data = as.character(test_data))


models_comb <- bind_rows(models_basic, models_cr)

models_tt <- models_comb %>% 
  mutate(model_data = str_c(model_data, str_glue(" %>% filter(coll_date < '{split_date}')")), 
         test_data = str_glue("model_dat_daily %>% filter(coll_date >= '{split_date}')"), 
         plot_label = str_c(plot_label, "tt_"))

models <- bind_rows(models_comb, models_tt) %>% select(plot_label, everything())
remove(models_basic, models_cr, models_comb, models_tt)
