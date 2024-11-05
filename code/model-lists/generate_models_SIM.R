## Create tibble of various model settings to iterate over
## For simulated data, 9 models total
# 1. Main model, ideal case
# 2. Main model, realistic kinetics
# 3. Main model, realistic variation
# 4. Main model, realistic sampling
# 5. Main model, base case
# 6. Main model, high variation
# 7. Main model, symmetric kinetics
# 8. Stratified model w/ asymptomatic only, base case
# 9. Stratified model w/ asymptomatic only, realistic sampling

models_basic <- tibble(
  group_vars = list(c("set")), 
  gr_var = list(c("gr_roll_idel", "gr_roll_rknt", "gr_roll_rvar", "gr_roll_rsam", "gr_roll_base", "gr_roll_hvar", "gr_roll_sknt")),
  direct_var = list(c("growing_idel", "growing_rknt", "growing_rvar", "growing_rsam", "growing_base", "growing_hvar", "growing_sknt")),
  model = list(c("(sm_mean_ct_idel + sm_skew_ct_idel) + var_era", 
                 "(sm_mean_ct_rknt + sm_skew_ct_rknt) + var_era", 
                 "(sm_mean_ct_rvar + sm_skew_ct_rvar) + var_era", 
                 "(sm_mean_ct_rsam + sm_skew_ct_rsam) + var_era", 
                 "(sm_mean_ct_base + sm_skew_ct_base) + var_era", 
                 "(sm_mean_ct_hvar + sm_skew_ct_hvar) + var_era", 
                 "(sm_mean_ct_sknt + sm_skew_ct_sknt) + var_era")), 
  model_data = c("model_dat_daily"), 
  test_data = list(NULL), 
  plot_label = list(c("dmskw_idel_", "dmskw_rknt_", "dmskw_rvar_", "dmskw_rsam_", "dmskw_base_", "dmskw_hvar_", "dmskw_sknt_")), 
  daily=TRUE) %>% 
  unnest(gr_var, direct_var, model, plot_label) %>%
  # mutate(group_vars = as.character(group_vars), 
  #        test_data = as.character(test_data))
  mutate(test_data = as.character(test_data))

models_subgroup <- tibble(
  group_vars = list(list(c("set", "symptomatic"))), 
  gr_var = list(c("gr_roll_base_FALSE", "gr_roll_rsam_FALSE")), 
  direct_var = list(c("growing_base_FALSE", "growing_rsam_FALSE")), 
  model = list(c("(sm_mean_ct_base_FALSE + sm_skew_ct_base_FALSE) + var_era", 
                 "(sm_mean_ct_rsam_FALSE + sm_skew_ct_rsam_FALSE) + var_era")), 
  model_data = c("model_dat_daily"), 
  test_data = list(NULL), 
  plot_label = list(c("dmskw_asy_base_", "dmskw_asy_rsam_")), 
  daily=TRUE) %>% 
  unnest(group_vars, gr_var, direct_var, model, plot_label) %>%
  mutate(test_data = as.character(test_data))

models_comb <- bind_rows(models_basic, models_subgroup)


models_basic_cr <- tibble(
  group_vars = list(c("set")), 
  gr_var = list(c("gr_roll_idel", "gr_roll_rknt", "gr_roll_rvar", "gr_roll_rsam", "gr_roll_base", "gr_roll_hvar", "gr_roll_sknt")),
  direct_var = list(c("growing_idel", "growing_rknt", "growing_rvar", "growing_rsam", "growing_base", "growing_hvar", "growing_sknt")),
  model = list(c("s(sm_mean_ct_idel, bs='cs') + s(sm_skew_ct_idel, bs='cs') + var_era", 
                 "s(sm_mean_ct_rknt, bs='cs') + s(sm_skew_ct_rknt, bs='cs') + var_era", 
                 "s(sm_mean_ct_rvar, bs='cs') + s(sm_skew_ct_rvar, bs='cs') + var_era", 
                 "s(sm_mean_ct_rsam, bs='cs') + s(sm_skew_ct_rsam, bs='cs') + var_era", 
                 "s(sm_mean_ct_base, bs='cs') + s(sm_skew_ct_base, bs='cs') + var_era", 
                 "s(sm_mean_ct_hvar, bs='cs') + s(sm_skew_ct_hvar, bs='cs') + var_era", 
                 "s(sm_mean_ct_sknt, bs='cs') + s(sm_skew_ct_sknt, bs='cs') + var_era" 
  )), 
  model_data = c("model_dat_daily"), 
  test_data = list(NULL), 
  plot_label = list(c("dmskw_idel_", "dmskw_rknt_", "dmskw_rvar_", "dmskw_rsam_", "dmskw_base_", "dmskw_hvar_", "dmskw_sknt_")), 
  daily=TRUE) %>% 
  unnest(gr_var, direct_var, model, plot_label) %>%
  mutate(test_data = as.character(test_data))


models_subgroup_cr <- tibble(
  group_vars = list(list(c("set", "symptomatic"))), 
  gr_var = list(c("gr_roll_base_FALSE", "gr_roll_rsam_FALSE")), 
  direct_var = list(c("growing_base_FALSE", "growing_rsam_FALSE")), 
  model = list(c("s(sm_mean_ct_base_FALSE, bs='cs') + s(sm_skew_ct_base_FALSE, bs='cs') + var_era", 
                 "s(sm_mean_ct_rsam_FALSE, bs='cs') + s(sm_skew_ct_rsam_FALSE, bs='cs') + var_era")), 
  model_data = c("model_dat_daily"), 
  test_data = list(NULL), 
  plot_label = list(c("dmskw_asy_base_", "dmskw_asy_rsam_")), 
  daily=TRUE) %>% 
  unnest(group_vars, gr_var, direct_var, model, plot_label) %>%
  mutate(test_data = as.character(test_data))


models_comb_cr <- bind_rows(models_basic_cr, models_subgroup_cr) %>% mutate(plot_label = str_c(plot_label, "cr_"))


## Thin plate regression splines, not included in final model selection set
# models_basic_tp <- tibble(
#   group_vars = list(c("set")), 
#   gr_var = list(c("gr_roll_idel", "gr_roll_rknt", "gr_roll_rvar", "gr_roll_rsam", "gr_roll_base", "gr_roll_hvar", "gr_roll_sknt")),
#   direct_var = list(c("growing_idel", "growing_rknt", "growing_rvar", "growing_rsam", "growing_base", "growing_hvar", "growing_sknt")),
#   model = list(c("s(sm_mean_ct_idel, bs='ts') + s(sm_skew_ct_idel, bs='ts') + var_era", 
#                  "s(sm_mean_ct_rknt, bs='ts') + s(sm_skew_ct_rknt, bs='ts') + var_era", 
#                  "s(sm_mean_ct_rvar, bs='ts') + s(sm_skew_ct_rvar, bs='ts') + var_era", 
#                  "s(sm_mean_ct_rsam, bs='ts') + s(sm_skew_ct_rsam, bs='ts') + var_era", 
#                  "s(sm_mean_ct_base, bs='ts') + s(sm_skew_ct_base, bs='ts') + var_era", 
#                  "s(sm_mean_ct_hvar, bs='ts') + s(sm_skew_ct_hvar, bs='ts') + var_era", 
#                  "s(sm_mean_ct_sknt, bs='ts') + s(sm_skew_ct_sknt, bs='ts') + var_era" 
#                  )), 
#   model_data = c("model_dat_daily"), 
#   test_data = list(NULL), 
#   plot_label = list(c("dmskw_idel_", "dmskw_rknt_", "dmskw_rvar_", "dmskw_rsam_", "dmskw_base_", "dmskw_hvar_", "dmskw_sknt_")), 
#   daily=TRUE) %>% 
#   unnest(gr_var, direct_var, model, plot_label) %>%
#   mutate(test_data = as.character(test_data))

# models_subgroup_tp <- tibble(
#   group_vars = list(list(c("set", "symptomatic"))), 
#   gr_var = list(c("gr_roll_base_FALSE", "gr_roll_rsam_FALSE")), 
#   direct_var = list(c("growing_base_FALSE", "growing_rsam_FALSE")), 
#   model = list(c("s(sm_mean_ct_base_FALSE, bs='ts') + s(sm_skew_ct_base_FALSE, bs='ts') + var_era", 
#                  "s(sm_mean_ct_rsam_FALSE, bs='ts') + s(sm_skew_ct_rsam_FALSE, bs='ts') + var_era")), 
#   model_data = c("model_dat_daily"), 
#   test_data = list(NULL), 
#   plot_label = list(c("dmskw_asy_base_", "dmskw_asy_rsam_")), 
#   daily=TRUE) %>% 
#   unnest(group_vars, gr_var, direct_var, model, plot_label) %>%
#   mutate(test_data = as.character(test_data))
# 
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
       models_comb, models_comb_tp, models_comb_cr, models_tt)

