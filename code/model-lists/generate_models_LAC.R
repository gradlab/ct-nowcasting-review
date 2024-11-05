## Create tibble of various model settings to iterate over
## For LAC data, 7 models total
# 1. Main model
# 2. Main model + age
# 3. Main model w/o var_era
# 4. Main model w/ full var_era interaction
# 5. Mean + SD (instead of skew)
# 6. Mean alone
# 7. Stratified model by symptom status
# 8. Stratified model w/ asymptomatic or no symptom status data only
# 9. Stratified model w/ immunologically naive patients only

models_basic <- tibble(
  group_vars = list(c()), gr_var = "gr_roll", direct_var = "growing", 
  model = list(c("(sm_mean_ct + sm_skew_ct) + var_era", 
                 "(sm_mean_ct + sm_skew_ct) + var_era + sm_mean_age", 
                 "sm_mean_ct + sm_skew_ct", 
                 "(sm_mean_ct + sm_skew_ct) * var_era", 
                 "(sm_mean_ct + sm_sd_ct) + var_era", 
                 "sm_mean_ct + var_era")), 
  model_data = c("model_dat_daily"), 
  test_data = list(NULL), 
  plot_label = list(c("dmskw_", "dmskw_a_", "dmsk_", "dmskv_", "dmsdw_", "dmw_")), 
  daily=TRUE) %>% 
  unnest(model, plot_label) %>%
  mutate(test_data = as.character(test_data))

models_subgroup <- tibble(
  group_vars = list(c(list("symptomatic"), list("symptomatic"), list("naive"))), gr_var = "gr_roll", direct_var = "growing", 
  model = list(c("(sm_mean_ct_FALSE + sm_mean_ct_NA + sm_mean_ct_TRUE + sm_skew_ct_FALSE + sm_skew_ct_NA + sm_skew_ct_TRUE) + var_era", 
                 "(sm_mean_ct_FALSE + sm_mean_ct_NA + sm_skew_ct_FALSE + sm_skew_ct_NA) + var_era", 
                 "(sm_mean_ct_TRUE + sm_skew_ct_TRUE) + var_era")), 
  model_data = c("model_dat_daily"), 
  test_data = list(NULL), 
  plot_label = list(c("dmskw_sym_", "dmskw_asyn_", "dmskw_nv_")), 
  daily=TRUE) %>% 
  unnest(group_vars, model, plot_label) %>%
  mutate(test_data = as.character(test_data))

models_comb <- bind_rows(models_basic, models_subgroup)

## Cubic spline models
models_basic_cr <- tibble(
  group_vars = list(c()), gr_var = "gr_roll", direct_var = "growing", 
  model = list(c("s(sm_mean_ct, bs='cs') + s(sm_skew_ct, bs='cs') + var_era", 
                 "s(sm_mean_ct, bs='cs') + s(sm_skew_ct, bs='cs') + var_era + sm_mean_age", 
                 "s(sm_mean_ct, bs='cs') + s(sm_skew_ct, bs='cs')", 
                 "s(sm_mean_ct, bs='cs', by=var_era) + s(sm_skew_ct, bs='cs', by=var_era)", 
                 "s(sm_mean_ct, bs='cs') + s(sm_sd_ct, bs='cs') + var_era", 
                 "s(sm_mean_ct, bs='cs') + var_era")), 
  model_data = c("model_dat_daily"), 
  test_data = list(NULL), 
  plot_label = list(c("dmskw_", "dmskw_a_", "dmsk_", "dmskv_", "dmsdw_", "dmw_")), 
  daily=TRUE) %>% 
  unnest(model, plot_label) %>%
  mutate(test_data = as.character(test_data))

models_subgroup_cr <- tibble(
  group_vars = list(c(list("symptomatic"), list("symptomatic"), list("naive"))), gr_var = "gr_roll", direct_var = "growing", 
  model = list(c("s(sm_mean_ct_FALSE, bs='cs') + s(sm_mean_ct_NA, bs='cs') + s(sm_mean_ct_TRUE, bs='cs') + s(sm_skew_ct_FALSE, bs='cs') + s(sm_skew_ct_NA, bs='cs') + s(sm_skew_ct_TRUE, bs='cs') + var_era", 
                 "s(sm_mean_ct_FALSE, bs='cs') + s(sm_mean_ct_NA, bs='cs') + s(sm_skew_ct_FALSE, bs='cs') + s(sm_skew_ct_NA, bs='cs') + var_era", 
                 "s(sm_mean_ct_TRUE, bs='cs') + s(sm_skew_ct_TRUE, bs='cs') + var_era")), 
  model_data = c("model_dat_daily"), 
  test_data = list(NULL), 
  plot_label = list(c("dmskw_sym_", "dmskw_asyn_", "dmskw_nv_")), 
  daily=TRUE) %>% 
  unnest(group_vars, model, plot_label) %>%
  mutate(test_data = as.character(test_data))

models_comb_cr <- bind_rows(models_basic_cr, models_subgroup_cr) %>% mutate(plot_label = str_c(plot_label, "cr_"))

# ## TP spline models
# models_basic_tp <- tibble(
#   group_vars = list(c()), gr_var = "gr_roll", direct_var = "growing", 
#   model = list(c("s(sm_mean_ct, bs='ts') + s(sm_skew_ct, bs='ts') + var_era", 
#                  "s(sm_mean_ct, bs='ts') + s(sm_skew_ct, bs='ts') + var_era + sm_mean_age", 
#                  "s(sm_mean_ct, bs='ts') + s(sm_skew_ct, bs='ts')", 
#                  "s(sm_mean_ct, bs='ts', by=var_era) + s(sm_skew_ct, bs='ts', by=var_era)", 
#                  "s(sm_mean_ct, bs='ts') + s(sm_sd_ct, bs='ts') + var_era", 
#                  "s(sm_mean_ct, bs='ts') + var_era")), 
#   model_data = c("model_dat_daily"), 
#   test_data = list(NULL), 
#   plot_label = list(c("dmskw_", "dmskw_a_", "dmsk_", "dmskv_", "dmsdw_", "dmw_")), 
#   daily=TRUE) %>% 
#   unnest(model, plot_label) %>%
#   mutate(test_data = as.character(test_data))
# 
# models_subgroup_tp <- tibble(
#   group_vars = list(c(list("symptomatic"), list("naive"))), gr_var = "gr_roll", direct_var = "growing", 
#   model = list(c("s(sm_mean_ct_FALSE, bs='ts') + s(sm_skew_ct_FALSE, bs='ts') + var_era", 
#                  "s(sm_mean_ct_TRUE, bs='ts') + s(sm_skew_ct_TRUE, bs='ts') + var_era")), 
#   model_data = c("model_dat_daily"), 
#   test_data = list(NULL), 
#   plot_label = list(c("dmskw_asy_", "dmskw_nv_")), 
#   daily=TRUE) %>% 
#   unnest(group_vars, model, plot_label) %>%
#   mutate(test_data = as.character(test_data))
# 
# models_comb_tp <- bind_rows(models_basic_tp, models_subgroup_tp) %>% mutate(plot_label = str_c(plot_label, "tp_"))

models_comb <- bind_rows(models_comb, models_comb_cr)

models_tt <- models_comb %>% 
  mutate(model_data = str_c(model_data, str_glue(" %>% filter(coll_date < '{split_date}')")), 
         test_data = str_glue("model_dat_daily %>% filter(coll_date >= '{split_date}')"), 
         plot_label = str_c(plot_label, "tt_"))

models <- bind_rows(models_comb, models_tt) %>% select(plot_label, everything())
remove(models_basic, models_subgroup, 
       # models_basic_tp, models_subgroup_tp, 
       models_basic_cr, models_subgroup_cr, 
       models_comb, models_comb_cr, models_tt)

