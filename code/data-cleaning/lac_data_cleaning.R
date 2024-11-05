
if (update_data == TRUE) {
  cat("Reading and cleaning raw data...\n")
  ## Read in main dataset
  raw_dat <- haven::read_sas(str_glue("{dat_folder}/lac_fulgent_ct.sas7bdat"))
  
  dat <- raw_dat %>%
    rename(onset_date = symptom_begin, 
           zip = Zip, 
           coll_date = elr_col, 
           gender = Sex, 
           race = Reported_Race, 
           hisp_eth = Ethnicity) %>% 
    mutate(onset_date = replace(onset_date, onset_date == "NA", NA), 
           onset_date = as_date(onset_date), 
           onset_date = replace(onset_date, onset_date == "1960-01-01", NA),  # Replace erroneous onset date entries
           across(starts_with("symptom"),  ~ case_when(.x == "Y" ~ TRUE, 
                                                       .x == "N" ~ FALSE, 
                                                       .x == "S" ~ NA,  # Records with symptoms skipped reported none
                                                       TRUE ~ NA))) %>% 
    mutate(symptomatic = if_any(starts_with("symptom")),  # Classify as symptomatic if any symptoms shown
           age = replace(age, age > 110, NA),  # Convert erroneous ages to NA
           across(c(vax_status, vax_status_addtl, gender, race), as_factor), 
           hisp_eth = case_when(hisp_eth == "Hispanic or Latino" ~ TRUE, 
                                hisp_eth == "Not Hispanic or Latino" ~ FALSE, 
                                TRUE ~ NA)) %>% 
    mutate(gender = case_when(gender == "Female" ~ "F",
                              gender == "Male" ~ "M", 
                              gender %in% c("Other", "Transgender (F to M)", "Transgender (M to F)", "Unknown", "") ~ NA, 
                              TRUE ~ gender), 
           race = fct_collapse(race, "Other" = c("Other", "Multiple Race"), NULL = c("", "Unknown")), 
           vaccinated = fct_recode(vax_status_addtl,
                                   "Unvax" = "0_Unvaccinated", 
                                   "Partial" = "1_Partially Vaccinated", 
                                   "Full" = "2_Complete primary series; no additional/booster dose", 
                                   "Boost" = "3_Complete primary series + additional/booster dose"), 
           naive = case_when(vaccinated == "Unvax" & NCVConfReinfect == "" ~ TRUE, 
                             TRUE ~ FALSE), 
           coll_wk = floor_date(coll_date,"week"), 
           coll_mo = floor_date(coll_date,"month"), 
           coll_date_int = as.numeric(coll_date), 
           Ct_value = pmin(N1_CT, N2_CT, SC2N_CT, na.rm=TRUE)) %>% 
    select(-starts_with("symptom_"), -c("loinc_test", "vax_status", "vax_status_addtl")) %>% 
    select(-starts_with("FACILITY"))
  
  ## Add within-person positive test number index
  dat <- dat %>% 
    group_by(person_id_case) %>%
    arrange(coll_date, .by_group=TRUE) %>%
    mutate(pos_idx = row_number(person_id_case)) %>%
    group_by(incid_case) %>% 
    arrange(coll_date, .by_group=TRUE) %>% 
    mutate(pos_idx_case = row_number(incid_case))
  
  ## Add days between positive tests
  dat <- dat %>% 
    ungroup() %>% 
    arrange(person_id_case, coll_date) %>% 
    mutate(lag_coll_date = lag(coll_date), 
           not_first = case_when(pos_idx > 1 ~ 1)) %>% 
    mutate(days_btwn_pos = (coll_date - lag_coll_date) * not_first) %>% 
    select(-c("lag_coll_date", "not_first"))
  
  
  ## Subset data with sequential measurements within course of case
  mult_test_indices <- dat %>% filter(pos_idx_case > 1)
  mult_test_cases <- dat %>% filter(incid_case %in% (mult_test_indices$incid_case))
  
  ## Output combined, cleaned CSV file
  dat %>% write_csv(str_glue("{dat_folder}/LAC_cts_cleaned.csv"))
  mult_test_cases %>% write_csv(str_glue("{dat_folder}/LAC_multiple_test_cts.csv"))
  saveRDS(dat, file=str_glue("{dat_folder}/dat.rds"))
  
  ## Identify only positive results w/ Ct values
  dat_pos <- dat %>%
    filter(pos_idx_case == 1, 
           Ct_value != 45) %>% 
    mutate(test_delay = coll_date - onset_date) %>% 
    mutate(onset_date = case_when(test_delay >= 725 ~ as_date(onset_date + 730), 
                                  test_delay >= 360 ~ as_date(onset_date + 365), 
                                  TRUE ~ onset_date)) %>% 
    mutate(test_delay = coll_date - onset_date) %>% 
    select(ACCESSION_ID, acc_date, coll_date, TESTCODE, test_result, onset_date, incid_case, person_id_case, 
           age, NCVConfReinfect, symptomatic:Ct_value, test_delay)
  
  cat("Remaining positives missing Ct values:", nrow(dat %>% filter(is.na(Ct_value))), "\n")
  cat("Remaining positives with Ct values:", nrow(dat_pos), "\n")
  
  ## Clean up
  dat_pos %>% write_csv(str_glue("./data-public/{key}_cts_clean.csv"))
  remove(mult_test_indices, raw_dat, dat)
} else {
  # dat <- readRDS(str_glue("{dat_folder}/dat.rds"))
  cat("Skipping raw data cleaning, reading clean data...\n")
  dat_pos <- read_csv(str_glue("./data-public/{key}_cts_clean.csv"), col_types=list(
    vaccinated = col_factor(c("Unvax", "Full", "Partial", "Boost")))) %>% 
    mutate(test_delay = coll_date - onset_date)  # Reset column to duration(days) type
}

  
cat("Processing clean data...\n")

### FOR ANALYSIS, NOT SAVED TO CLEAN DATASET
## Generate a calendar
data_calendar <- expand_grid(date=seq(min(dat_pos$coll_date),max(dat_pos$coll_date),by="1 day"))
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

## Split into variant eras  
dat_pos <- dat_pos %>% 
  left_join(data_calendar, by=c("coll_date"="date", "coll_date_int"="coll_date_int")) %>% 
  add_variant_eras(variant_eras, coll_date)

## LAC only - calculate proportion naive
prop_naive_daily <- dat_pos %>% 
  arrange(coll_date) %>% 
  complete_dates(coll_date, c()) %>% 
  mutate(prop_naive = zoo::rollapplyr(naive, rollmean_window, mean, na.rm=TRUE, partial=TRUE)) %>% 
  group_by(coll_date) %>% 
  summarise(prop_naive = last(prop_naive))
  # prop_naive_weekly <- dat_pos %>% 
  #   group_by(coll_wk) %>% 
  #   summarise(prop_naive = mean(naive))

## Clean up
remove(data_calendar, wk_data_calendar)
cat("Data processed and ready!\n")
  

  
  
    
#============================================================
#============================================================

# test <- dat %>% 
#   group_by(person_id_case) %>% 
#   mutate(baseline_pos = min(coll_date)) %>% 
#   ungroup() %>% 
#   mutate(baseline_end = baseline_pos + 90) %>% 
#   mutate(not_first_case = (coll_date >= baseline_end)) 
#                             
# test2 %>% filter(not_first_case == TRUE) %>% select(coll_date, baseline_pos, baseline_end, not_first_case, NCVConfReinfect)
# 
# test2 <- test %>% filter(person_id_case %in% (mult_inf$person_id_case))
# 
# test2 <- dat %>% 
#   group_by(person_id_case) %>% 
#   arrange(coll_date) %>% 
#   mutate(days_btwn_pos = coll_date - lag(coll_date, 1))
# 
# test3 <- mult_case %>% filter(coll_date != lab_coll_dt_case & pos_idx_case == 1)
# test3 %>% ggplot() + geom_bar(aes(x=coll_date))
# 
# mult_inf <- dat %>% filter(pos_idx > 1)
# 
# test <- dat %>% filter(pos_idx != pos_idx_case)
# 
# test2 <- dat %>% filter(incid_case %in% test3$incid_case)


## 17356 records w/ all symptoms 'S' and onset date == 'NA'
## 6188 records w/ all symptoms 'S' and onset date blank
## 763 cases w/ all symptoms 'N' but onset date assigned

