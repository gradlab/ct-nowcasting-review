## Create combined daily and weekly Ct and incidence dataframes
comb_dat_daily <- dat_pos %>%
  arrange(coll_date) %>%
  group_by(coll_date, across(all_of(grouping_vars))) %>%
  summarise(mean_ct = mean(Ct_value),
            skew_ct = moments::skewness(Ct_value),
            sd_ct = sd(Ct_value),
            mean_age = mean(age, na.rm=TRUE),
            N = n()) %>% 
  filter(N >= min_samples) %>% 
  complete_dates(coll_date, grouping_vars) %>% 
  group_by(across(all_of(grouping_vars))) %>%
  mutate(sm_mean_ct = zoo::rollapplyr(mean_ct, rollmean_window, mean, na.rm=TRUE, partial=TRUE),
         sm_skew_ct = zoo::rollapplyr(skew_ct, rollmean_window, mean, na.rm=TRUE, partial=TRUE),
         sm_sd_ct = zoo::rollapplyr(sd_ct, rollmean_window, mean, na.rm=TRUE, partial=TRUE),
         sm_mean_age = zoo::rollapplyr(mean_age, rollmean_window, mean, na.rm=TRUE, partial=TRUE),
         sm_N = zoo::rollapplyr(N, rollmean_window, mean, na.rm=TRUE, partial=TRUE),
         across(-coll_date, ~ ifelse(is.nan(.), NA, .)), # Replace NaN values (na_if() not working)
         across(mean_ct:sd_ct|sm_mean_ct:sm_sd_ct, \(x) na_if(x, Inf)),
         across(mean_ct:sd_ct|sm_mean_ct:sm_sd_ct, \(x) na_if(x, -Inf))) %>%
  # left_join(dat %>% summarise_prop_pos(result, coll_date, grouping_vars)) %>%  # Merge in test positivity
  left_join(case_dat_daily %>% rename("coll_date"="date")) %>%
  add_variant_eras(variant_eras, coll_date) %>%
  ungroup()


# comb_dat_weekly <- dat_pos %>% 
#   arrange(coll_wk) %>% 
#   group_by(coll_wk, across(all_of(grouping_vars))) %>%
#   summarise(mean_ct = mean(Ct_value), 
#             skew_ct = moments::skewness(Ct_value),
#             sd_ct = sd(Ct_value), 
#             mean_age = mean(age, na.rm=TRUE), 
#             N = n()) %>% 
#   ungroup() %>% 
#   mutate(across(-coll_wk, ~ ifelse(is.nan(.), NA, .)), # Replace NaN values (na_if() not working))
#          across(mean_ct:sd_ct, \(x) na_if(x, Inf)), 
#          across(mean_ct:sd_ct, \(x) na_if(x, -Inf))) %>% 
#   # left_join(dat %>% summarise_prop_pos(result, coll_wk, grouping_vars)) %>%  # Merge in test positivity
#   left_join(case_dat_weekly, by=c("coll_wk"="date")) %>% 
#   add_variant_eras(variant_eras, coll_wk)

# ## Specify ID columns for pivoting
# id_cols <- c("coll_date", "var_era", "gr_roll", "growing")

## Add positivity & immune history data if available (and incl. w/ ID columns)
if (exists("prop_pos_daily")) {
  comb_dat_daily <- comb_dat_daily %>% left_join(prop_pos_daily)
  # comb_dat_weekly <- comb_dat_weekly %>% left_join(prop_pos_weekly)
  id_cols <- id_cols %>% append("prop_pos")
}
if (exists("prop_naive_daily")) {
  comb_dat_daily <- comb_dat_daily %>% left_join(prop_naive_daily)
  # comb_dat_weekly <- comb_dat_weekly %>% left_join(prop_naive_weekly)
  id_cols <- id_cols %>% append("prop_naive")
}
if (hosp==TRUE) id_cols <- append(id_cols, c("gr_roll_hosp", "growing_hosp"))
if (ww==TRUE) id_cols <- append(id_cols, c("gr_roll_ww", "growing_ww"))

## Specify `values_from` columns for pivoting
value_cols <- c("gr", "gr_roll", "sm_gr", "sm_gr_roll", "inflection", "growing",
                "sm_mean_ct", "sm_skew_ct", "sm_sd_ct", "sm_mean_age", "sm_N",
                "mean_ct", "skew_ct", "sd_ct", "mean_age", "N")

## Exclude any ID columns to prevent pivoting error
value_cols <- value_cols[!(value_cols %in% id_cols)]

## Create pivoted data for model fitting
model_dat_daily <- comb_dat_daily %>%
  pivot_model_data(dep_vars=c(),
                   grouping_vars,
                   values_from=value_cols,
                   idcols=id_cols) %>%
  mutate(coll_mo = round_date(coll_date, "month"))





# model_dat_daily <- comb_dat_daily %>%
#   pivot_model_data(dep_vars=dep_vars, 
#                    grouping_vars,
#                    values_from=c("sm_mean_ct", "sm_median_ct", "sm_skew_ct", "sm_gskew_ct", "sm_sd_ct", "sm_mean_age", "sm_N"),
#                    idcols=id_cols) %>%
#   mutate(coll_mo = round_date(coll_date, "month"))

# ## Create pivoted data for model fitting
# model_dat_weekly <- comb_dat_weekly %>%
#   pivot_model_data(dep_vars=c(),  ##### CHANGE THESE
#                    grouping_vars,
#                    values_from=c("gr_wk", "grow_wk", "mean_ct", "skew_ct", "sd_ct", "mean_age", "N"),
#                    datevar=coll_wk,
#                    idcols=id_cols) %>%
#   mutate(coll_mo = round_date(coll_wk, "month"))


## OLD VERSION
# comb_dat_daily <- dat_pos %>% 
#   group_by(coll_date, across(all_of(grouping_vars))) %>%  # SPECIFY BY DATE OR WEEK
#   summarise_cts(Ct_value) %>% 
#   complete_dates(coll_date, grouping_vars) %>%  # IF BY WEEK, SPECIFY PERIOD=7
#   group_by(across(all_of(grouping_vars))) %>% 
#   mutate(across(all_of(ct_stats), ~ zoo::rollapply(.x, rollmean_window, mean, na.rm=TRUE, partial=TRUE, fill=NA), .names = "sm_{col}")) %>% 
#   mutate(across(-coll_date, ~ ifelse(is.nan(.), NA, .))) %>%  # Replace NaN values (na_if() not working)
#   left_join(dat %>% summarise_prop_pos(result, coll_date, grouping_vars)) %>%  # Merge in test positivity
#   left_join(case_dat_daily, by=c("coll_date"="date")) %>% 
#   add_variant_eras(variant_eras, coll_date) %>% 
#   ungroup()
# 
# comb_dat_weekly <- dat_pos %>% 
#   group_by(coll_wk, across(all_of(grouping_vars))) %>%  # SPECIFY BY DATE OR WEEK
#   summarise_cts(Ct_value) %>% 
#   complete_dates(coll_wk, grouping_vars, period=7) %>%  # IF BY WEEK, SPECIFY PERIOD=7
#   group_by(across(all_of(grouping_vars))) %>% 
#   mutate(across(all_of(ct_stats), ~ zoo::rollapply(.x, rollmean_window, mean, na.rm=TRUE, partial=TRUE, fill=NA), .names = "sm_{col}")) %>% 
#   mutate(across(-coll_wk, ~ ifelse(is.nan(.), NA, .))) %>%  # Replace NaN values (na_if() not working)
#   left_join(dat %>% summarise_prop_pos(result, coll_wk, grouping_vars)) %>%  # Merge in test positivity
#   left_join(case_dat_weekly, by=c("coll_wk"="date")) %>% 
#   add_variant_eras(variant_eras, coll_wk) %>% 
#   ungroup()




# grouping_vars <- c("loc_type")
# rollmean_window <- 3
# test <- dat_pos %>% 
#   arrange(coll_date) %>% 
#   slice(1:1000) %>% 
#   group_by(across(all_of(grouping_vars))) %>% 
#   # group_by(coll_date, across(all_of(grouping_vars))) %>%  # SPECIFY BY DATE OR WEEK
#   # run_by(k="3 days", idx="coll_date") %>%
#   # mutate(sm_mean_ct = runner(x=Ct_value, f=mean)) %>%
#   mutate(sm_mean_ct = runner(x=Ct_value, k=rollmean_window, idx=coll_date, f=mean)) %>%
#   # summarise(sm_mean_ct = mean(Ct_value))
#   group_by(coll_date, across(all_of(grouping_vars))) %>% 
#   summarise(mean_ct = mean(Ct_value), 
#             sm_mean_ct = last(sm_mean_ct), 
#             count = n(), 
#             total = sum(Ct_value)) %>% 
#   group_by(across(all_of(grouping_vars))) %>% 
#   mutate(cumtotal = cumsum(total), cumcount = cumsum(count), cummean = cumtotal / cumcount)



