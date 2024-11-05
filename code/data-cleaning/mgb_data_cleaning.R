
if (update_data == TRUE) {
  cat("Reading and cleaning raw data...\n")
  ## Define named vector of abbreviations for assay recoding
  assays <- c(BRD = "Broad SARS-CoV-2", 
              CPD = "Cepheid SARS-CoV-2", 
              CPM = "Cepheid SARS-CoV-2/flu/RSV", 
              HLF = "Hologic Fusion SARS-CoV-2", 
              RCB = "Roche cobas SARS-CoV-2", 
              RCM = "Roche cobas SARS-CoV-2/flu", 
              RLM = "Roche Liat SARS-CoV-2/flu")
  
  ## Read in main dataset
  dat <- read_csv(str_glue("{dat_folder}/MGB_clean.csv"))
  
  ## Add collection month and integer date
  dat <- dat %>% 
    mutate(coll_mo = floor_date(coll_date,"month"), 
           coll_date_int = as.numeric(coll_date))
  cat("Total test results, full dataset:", nrow(dat), "\n")
  cat("Total positive test results, full dataset:", nrow(dat %>% filter(result == "Positive")), "\n")
  
  ## Identify individuals with repeated positive results
  rep_pos <- dat %>% 
    filter(result == "Positive") %>% 
    mutate(lag_person = lag(person_id), lag_date = lag(coll_date), lag_index = lag(test_index)) %>% 
    mutate(is_same = case_when(person_id == lag_person ~ 1), 
           days_btwn_pos = (coll_date - lag_date) * is_same) %>% 
    select(-c('lag_person', 'lag_date', 'lag_index', 'is_same')) %>% 
    filter(!is.na(days_btwn_pos))
  cat("Number of non-first positive results:", nrow(rep_pos), "\n")
  
  ## Identify putative repeat positives from same infection
  short_rep_pos <- rep_pos %>% 
    filter(days_btwn_pos <= 60)
  cat("Number of repeat results from single infection episodes:", nrow(short_rep_pos), "\n")
  
  ## Identify potential multiple or long infections
  mult_rep <- rep_pos %>% 
    filter(days_btwn_pos > 60) %>% 
    group_by(person_id) %>% 
    summarise(count = n())
  cat("Individuals with single repeat infection episode:", nrow(mult_rep), "\n")
  mult_rep <- mult_rep %>% filter(count > 1)
  cat("Individuals with multiple repeat infection episodes:", nrow(mult_rep), "\n")
  
  ## Filter out imputed repeat results or long infections
  dat <- dat %>% 
    filter(!(person_id %in% mult_rep$person_id),  # Exclude multiple infection individuals completely
           !(sample_id %in% short_rep_pos$sample_id))  # Use sample_id to retain first positives otherwise
  cat("Total positive test results, repeats removed:", nrow(dat %>% filter(result == "Positive")), "\n")
  
  ## Further data cleaning
  dat <- dat %>% 
    mutate(gender = case_when(gender %in% c('U', 'X') ~ NA, 
                              TRUE ~ gender),
           age = replace(age, age > 110, NA),  # Convert erroneous ages to NA
           hospital = fct_collapse(hospital, 'OTHER' = c(
             'SRB', 'SHCC', 'SHC', 'SHB', 'MVH', 'MEE', 'MCL', 'DFC', 'ICA', 'PMA')), 
           specimen_type = fct_collapse(specimen_type, 'Other' = c(
             'Other', 'Oropharynx', 'NP/OP', 'Not recorded', 'Lower respiratory tract')), 
           loc_cat = fct_collapse(loc_cat, "O" = c("OP", "OS"), "I" = "IP", "E" = "ER"),
           assay = as_factor(assay),  # Convert assay to factor w/ abbreviations
           assay = fct_recode(assay, !!!assays)) %>% 
    rename(zip = zipcode,  # Rename columns to standard names
           loc_type = loc_cat, 
           location = loc_specific, 
           spec_type = specimen_type, 
           Ct_value = Ct_min, 
           coll_wk = coll_week)
  
  ## Output combined, cleaned CSV file
  dat %>% write_csv(str_glue("{dat_folder}/MGB_cts_cleaned.csv"))
  saveRDS(dat, file=str_glue("{dat_folder}/dat.rds"))
  
  ## Identify only positive results w/ Ct values
  dat_pos <- dat %>% 
    filter(result == "Positive", 
           !is.na(Ct_value), 
           Ct_value > 0) %>% 
    mutate(assay = fct_drop(assay)) %>% # Drop unused assay levels
    select(hospital:age, sample_id, loc_type, coll_date, coll_wk, assay:coll_date_int)
  cat("Remaining positives missing Ct values:", nrow(dat %>% filter(result=="Positive" & is.na(Ct_value))), "\n")
  cat("Remaining positives with Ct values:", nrow(dat_pos), "\n")
  
  ## Calculate test positivity
  prop_pos_daily <- dat %>% 
    mutate(positive = result == "Positive") %>% 
    group_by(coll_date) %>% 
    summarise(total = n(), pos = sum(positive)) %>% 
    mutate(roll_total = zoo::rollsumr(total, rollmean_window, fill=NA), 
           roll_pos = zoo::rollsumr(pos, rollmean_window, fill=NA), 
           prop_pos = roll_pos / roll_total) %>% 
    select(coll_date, prop_pos)
  
  prop_pos_weekly <- dat %>%
    mutate(positive = result == "Positive") %>% 
    group_by(coll_wk) %>% 
    summarise(prop_pos = mean(positive))
  
  ## Clean up
  dat_pos %>% write_csv(str_glue("./data-public/{key}_cts_clean.csv"))
  remove(rep_pos, short_rep_pos, mult_rep, dat)
} else {
  # dat <- readRDS(str_glue("{dat_folder}/dat.rds"))
  cat("Skipping raw data cleaning, reading clean data...\n")
  dat_pos <- read_csv(str_glue("./data-public/{key}_cts_clean.csv"), col_types=list(
    hospital = col_factor(c("BWFH", "BWH", "CDH", "OTHER", "MGH", "NCH", "NSMC", "NWH", "WDH")), 
    loc_type = col_factor(c("E", "I", "O")), 
    assay = col_factor(c("CPD", "HLF", "BRD", "CPM", "RLM", "RCB", "RCM")), 
    spec_type = col_factor(c("Other", "Nasal", "Nasopharynx"))
  ))
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

dat_pos <- dat_pos %>% 
  left_join(data_calendar, by=c("coll_date"="date", "coll_date_int"="coll_date_int")) %>% 
  add_variant_eras(variant_eras, coll_date)  # Add variant era splits

## Clean up
# saveRDS(dat_pos, file=str_glue("./data-public/{key}_dat_pos.rds"))
remove(data_calendar, wk_data_calendar)
cat("Data processed and ready!\n")