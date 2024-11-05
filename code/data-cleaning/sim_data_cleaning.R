
if (update_data == TRUE) {
  ## Create list of synthetic datasets and codes
  dat_files <- list(
    c("idel", str_glue("{dat_folder}/ideal.csv")),
    c("rknt", str_glue("{dat_folder}/realistic_kinetics.csv")),
    c("rvar", str_glue("{dat_folder}/realistic_variation.csv")),
    c("rsam", str_glue("{dat_folder}/realistic_sampling.csv")),
    c("base", str_glue("{dat_folder}/realistic_all.csv")), 
    c("hvar", str_glue("{dat_folder}/extreme_variation.csv")), 
    c("sknt", str_glue("{dat_folder}/symmetric_kinetics.csv"))
  )
  
  ## Initialise containers
  iter_dat <- vector("list", length(dat_files))
  
  ## Read and combine synthetic data files
  for (j in 1:length(dat_files)) {
    iter_dat[[j]] <- read_csv(dat_files[[j]][[2]]) %>% mutate(set = dat_files[[j]][[1]]) %>% select(set, everything())
  }
  
  dat <- bind_rows(iter_dat)
  
  ## Add collection month and integer date
  dat <- dat %>% 
    # mutate(coll_date = ymd("2020-01-29") + sampled_time,
    mutate(inf_date = infection_date, 
           onset_date = inf_date + (onset_time - infection_time),
           coll_date = inf_date + (sampled_time - infection_time),
           coll_wk = floor_date(coll_date,"week"), 
           coll_mo = floor_date(coll_date,"month"), 
           coll_date_int = as.numeric(coll_date), 
           rep_delay = sampled_time - infection_time, 
           symptomatic = ifelse(is_symp == 1, TRUE, FALSE), 
           surveillance = as.factor(surveillance), 
           surveillance = fct_recode(surveillance, "sym" = "symptom-based testing", "rnd" = "random testing")) %>% 
    rename(Ct_value = ct_obs, 
           test_delay = confirmation_delay) %>% 
    select(set, coll_date, coll_wk, coll_mo, coll_date_int, onset_date, inf_date, test_delay, rep_delay, symptomatic, surveillance, Ct_value) %>% 
    mutate(age = NA)
    
  ## Output combined, cleaned CSV file
  dat %>% write_csv(str_glue("{dat_folder}/sim_cts_cleaned.csv"))
  
  ### FOR ANALYSIS, NOT SAVED TO CLEAN DATASET
  ## Generate a calendar
  data_calendar <- expand_grid(date=seq(min(dat$coll_date),max(dat$coll_date),by="1 day"))
  data_calendar <- data_calendar %>% 
    mutate(coll_date_int = as.numeric(date)) %>% 
    mutate(day_of_wk = wday(date,label=TRUE))
  
  ## Add CDC MMWR epi_wk indicator and cumulative EpiWeeks since 2020
  data_calendar <- data_calendar %>% 
    mutate(epi_yr = MMWRweek::MMWRweek(date)$MMWRyear, 
           epi_wk = MMWRweek::MMWRweek(date)$MMWRweek) %>% 
    mutate(epi_wk_comb = case_when(epi_yr == 2021 ~ epi_wk + 53, 
                                   epi_yr == 2022 ~ epi_wk + 53 + 52, 
                                   epi_yr == 2023 ~ epi_wk + 53 + 52 + 52, 
                                   TRUE ~ epi_wk))
  
  ## Create weekly version with start-of-week dates
  wk_data_calendar <- data_calendar %>% group_by(epi_wk_comb) %>% summarise(across(-day_of_wk, ~ min(.)))
  
  dat <- dat %>% left_join(data_calendar, by=c("coll_date"="date", "coll_date_int"="coll_date_int"))
  
  ## Split into variant eras
  dat <- dat %>% mutate(var_era = ifelse(coll_date < "2021-07-01","Alpha",
                                         ifelse(coll_date < "2021-12-01","Delta","Omicron"))) %>% 
    mutate(var_era = fct_recode(var_era))
  
  ## Identify only positive results w/ Ct values
  dat_pos <- dat %>% 
    filter(!is.na(Ct_value)) %>% 
    filter(Ct_value > 0 & Ct_value < 38, 
           coll_date >= "2020-03-29",  ## Cut off head to match MGB data
           coll_date < "2023-03-01") %>%  ## Cut off tail end
    mutate(age = NA)
  
  ## Generate incidence curves from reported
  combined_dat <- dat %>% 
    group_by(set) %>% 
    filter(!is.na(Ct_value),
           Ct_value > 0 & Ct_value < 38, 
           surveillance == "sym", 
           coll_date >= "2020-03-29",
           coll_date < "2023-03-01") %>% 
    group_by(set, coll_date) %>% 
    summarise(Pos_New = n()) %>% 
    mutate(mean_7day = zoo::rollmeanr(Pos_New, 7, fill=NA)) %>% 
    rename(date = coll_date) %>% 
    left_join(data_calendar)
  
  ## Calculate daily growth rates
  case_dat_daily <- combined_dat %>% 
    mutate(gr = log(Pos_New/lag(Pos_New,1)),
           gr_roll = log(mean_7day/lag(mean_7day,1))) %>% 
    mutate(sm_gr = zoo::rollmeanr(gr, rollmean_window, fill = NA),
           sm_gr_roll = zoo::rollmeanr(gr_roll, rollmean_window, fill = NA), 
           sm_gr_roll_c = zoo::rollapply(gr_roll, rollmean_window, mean, na.rm=TRUE, partial=TRUE),
           sm_gr_roll_d2 = lead(sm_gr_roll_c, inf_settings[[1]]) - lag(sm_gr_roll_c, inf_settings[[1]]), 
           inflection = (abs(sm_gr_roll_d2) >= inf_settings[[2]]), 
           growing = as.numeric(sm_gr > 0)) %>% 
    mutate(across(starts_with('gr')|starts_with('sm'), ~ ifelse(is.nan(.), NA, .)),  # Replace NaN values (na_if() not working)
           across(starts_with('gr')|starts_with('sm'), \(x) na_if(x, Inf)),  # Also replace Inf values
           across(starts_with('gr')|starts_with('sm'), \(x) na_if(x, -Inf)))
  
  # ## Create weekly version
  # case_dat_weekly <- combined_dat %>% 
  #   group_by(set, epi_wk_comb) %>% 
  #   summarise(Pos_wk = sum(Pos_New)) %>%
  #   mutate(gr_wk = log(Pos_wk/lag(Pos_wk,1)), 
  #          grow_wk = as.numeric(gr_wk > 0)) %>% 
  #   mutate(across(gr_wk:grow_wk, ~ ifelse(is.nan(.), NA, .)),  # Replace NaN values (na_if() not working)
  #          across(gr_wk:grow_wk, \(x) na_if(x, Inf)),  # Also replace Inf values
  #          across(gr_wk:grow_wk, \(x) na_if(x, -Inf))) %>% 
  #   left_join(wk_data_calendar)
  
  ## Downsample results to more realistic number of total points
  dat_pos <- dat_pos %>% 
    group_by(set) %>% 
    slice_sample(n=250000)
  
  ## Clean up
  remove(data_calendar, wk_data_calendar)
  
  saveRDS(dat, file=str_glue("{dat_folder}/dat.rds"))
  saveRDS(dat_pos, file=str_glue("{dat_folder}/dat_pos.rds"))
  saveRDS(case_dat_daily, file=str_glue("{dat_folder}/case_dat_daily.rds"))
  remove(dat)
  # saveRDS(case_dat_weekly, file=str_glue("{dat_folder}/case_dat_weekly.rds"))
} else {
  # dat <- readRDS(str_glue("{dat_folder}/dat.rds"))
  dat_pos <- readRDS(str_glue("{dat_folder}/dat_pos.rds"))
  case_dat_daily <- readRDS(str_glue("{dat_folder}/case_dat_daily.rds"))
  # case_dat_weekly <- readRDS(str_glue("{dat_folder}/case_dat_weekly.rds"))
}

##### ENDING OUTPUT: case_dat_daily, case_dat_weekly, combined_dat
# 
# ## Read in simulated case data
# combined_dat <- read_csv(str_glue("{dat_folder}/case_curve_{runname}.csv"))
# 
# combined_dat <- combined_dat %>% 
#   mutate(date = ymd("2021-01-01") + date) %>% 
#   rename(Pos_New = n_reported, 
#          Pos_New_true = n_onsets) %>% 
#   mutate(mean_7day = zoo::rollmean(Pos_New, rollmean_window, fill=NA), 
#          mean_7day_true = zoo::rollmean(Pos_New_true, rollmean_window, fill=NA))
# 
# ## Generate a calendar 
# all_calendar <- expand_grid(date=seq(min(combined_dat$date),max(combined_dat$date),by="1 day"))
# all_calendar <- all_calendar %>% 
#   mutate(coll_date_int = as.numeric(date)) %>% 
#   mutate(day_of_wk = wday(date,label=TRUE))
# 
# ## Add CDC MMWR EpiWeek indicator and cumulative EpiWeeks since 2020
# all_calendar <- all_calendar %>% 
#   mutate(epi_yr = MMWRweek::MMWRweek(date)$MMWRyear, 
#          epi_wk = MMWRweek::MMWRweek(date)$MMWRweek) %>% 
#   mutate(epi_wk_comb = case_when(epi_yr == 2021 ~ epi_wk + 53, 
#                                  epi_yr == 2022 ~ epi_wk + 53 + 52, 
#                                  epi_yr == 2023 ~ epi_wk + 53 + 52 + 52, 
#                                  TRUE ~ epi_wk))
# 
# ## Combine into single dataframe w/ calendar
# combined_dat <- combined_dat %>% 
#   left_join(all_calendar)
# 
# ## Calculate daily growth rates
# case_dat_daily <- combined_dat %>% 
#   mutate(gr = log(Pos_New/lag(Pos_New,1)),
#          gr_roll = log(mean_7day/lag(mean_7day,1)), 
#          gr_true = log(Pos_New_true/lag(Pos_New_true,1)), 
#          gr_roll_true = log(mean_7day_true/lag(mean_7day_true,1))) %>% 
#   mutate(sm_gr = zoo::rollmean(gr, rollmean_window, fill = NA),
#          sm_gr_roll = zoo::rollmean(gr_roll, rollmean_window, fill = NA), 
#          growing = as.numeric(gr_roll > 0), 
#          sm_gr_true = zoo::rollmean(gr_true, rollmean_window, fill = NA),
#          sm_gr_roll_true = zoo::rollmean(gr_roll_true, rollmean_window, fill = NA), 
#          growing_true = as.numeric(gr_roll_true > 0)) %>% 
#   mutate(across(gr:growing_true, ~ ifelse(is.nan(.), NA, .)),  # Replace NaN values (na_if() not working)
#          across(gr:growing_true, na_if, Inf),  # Also replace Inf values
#          across(gr:growing_true, na_if, -Inf))
# 
# ## Create weekly version
# case_dat_weekly <- combined_dat %>% 
#   group_by(epi_wk_comb) %>% 
#   summarise(Pos_wk = sum(Pos_New), 
#             Pos_wk_true = ifelse(sum(Pos_New_true, na.rm=TRUE) == 0, NA, sum(Pos_New_true, na.rm=TRUE))) %>%
#   mutate(gr_wk = log(Pos_wk/lag(Pos_wk,1)), 
#          grow_wk = as.numeric(gr_wk > 0), 
#          gr_wk_true = log(Pos_wk_true/lag(Pos_wk_true,1)), 
#          grow_wk_true = as.numeric(gr_wk_true > 0)) %>% 
#   mutate(across(gr_wk:grow_wk_true, ~ ifelse(is.nan(.), NA, .)),  # Replace NaN values (na_if() not working)
#          across(gr_wk:grow_wk_true, na_if, Inf),  # Also replace Inf values
#          across(gr_wk:grow_wk_true, na_if, -Inf))
# 
# ## Create weekly calendar
# week_calendar <- all_calendar %>% group_by(epi_wk_comb) %>% summarise(across(-day_of_wk, ~ min(.)))
# case_dat_weekly <- case_dat_weekly %>% left_join(week_calendar)
# 
# ## Clean up
# remove(all_calendar)
