
if (update_data == TRUE) {
  cat("Reading and cleaning raw data...\n")
  ## Read in and clean data
  dat1 <- read_csv(str_glue("{dat_folder}/tufts_cts_all_20220401.csv"))
  
  ## Read additional confirmatory data
  soft <- read_csv(str_glue("{dat_folder}/tufts_cts_softlab.csv"))
  soft <- soft %>% 
    select(-c('Result_Ct', 'Instrument', 'Source')) %>% 
    rename(Result_Ct = `Alinity_Ct (from initial test)`) %>% 
    mutate(Result_Ct = replace(Result_Ct, str_starts(Test_Result, "Negative"), NA)) %>% 
    mutate(Unique_Test_ID = str_extract(Unique_Test_ID, '\\d+'), 
           Unique_Test_ID = as.numeric(Unique_Test_ID))
  
  ## Merge with main dataset
  dat1 <- bind_rows(dat1, soft)
  
  ## Repeat for newer dataset
  dat2 <- read_csv(str_glue("{dat_folder}/tufts_cts_all_20221111.csv"))
  
  epic <- read_csv(str_glue("{dat_folder}/tufts_cts_epic.csv"))
  epic <- epic %>% 
    select(-c('Result_Ct', 'Instrument', 'Source')) %>% 
    rename(Result_Ct = `Alinity_Ct (from initial test)`) %>% 
    mutate(Result_Ct = replace(Result_Ct, str_starts(Test_Result, "Negative"), NA)) %>% 
    mutate(Unique_Test_ID = str_extract(Unique_Test_ID, '\\d+'), 
           Unique_Test_ID = as.numeric(Unique_Test_ID)) %>% 
    mutate(`Age_(years)` = str_replace_all(`Age_(years)`, '\\d+ mos', '1'),  # change ages in months to 1 year
           `Age_(years)` = str_extract(`Age_(years)`, '\\d+'), 
           `Age_(years)` = as.numeric(`Age_(years)`))
  
  dat2 <- bind_rows(dat2, epic) %>% 
    rename(Ordering_Clinic = `Submitter/Ordering Dept`,
           Stay_Clinic = `Encounter Dept`)
  
  ## Merge older and newer datasets
  dat <- bind_rows(dat1, dat2)
  dat <- dat %>% 
    rename(age=`Age_(years)`) %>% 
    # filter(!is.na(Result_Ct)) %>% 
    mutate(HIS_Patient_Type = fct_recode(HIS_Patient_Type,
                                         "O" = "Outpatient",
                                         "I" = "Inpatient"),
           Test_Result = fct_collapse(Test_Result,
                                      "Negative" = c("Negative@CCVA", "Negative@RCVC", "Negative@RCVL"), 
                                      "Positive" = c("Positive@CCVA", "Positive@RCVC", "Positive@RCVL")), 
           # Symptomatic = fct_collapse(Symptomatic, ## Some erroneous symptom status entries
           #                            `FALSE` = c("n", "N", "no", "No", "NoYes"), 
           #                            `TRUE` = c("y", "Y", "yes", "Yes", "YES", "UnknownYes", "YesYes"), 
           #                            `NA` = c("Unknown", "UnknownUnknown"))
           Symptomatic = case_when(Symptomatic %in% c("n", "N", "no", "No", "NoYes") ~ FALSE,
                                   Symptomatic %in% c("y", "Y", "yes", "Yes", "YES", "UnknownYes", "YesYes") ~ TRUE, 
                                   Symptomatic %in% c("Unknown", "UnknownUnknown") ~ NA)
           ) %>% 
    rename(coll_date = Collection_Date,  # Rename columns to standard names
           symptomatic = Symptomatic, 
           gender = Sex, 
           loc_type = HIS_Patient_Type, 
           location = Ordering_Clinic, 
           result = Test_Result, 
           Ct_value = Result_Ct)
  cat("Total test results, full dataset:", nrow(dat), "\n")
  cat("Total positive test results, full dataset:", nrow(dat %>% filter(result == "Positive")), "\n")
  
  ##### ADD HARMONISATION OF ORDERING & STAY CLINIC NAMES
  
  ## Convert dates to actual dates
  dat <- dat %>% 
    mutate(coll_date = mdy(coll_date), 
           coll_wk = floor_date(coll_date,"week"), 
           coll_mo = floor_date(coll_date,"month"), 
           coll_date_int = as.numeric(coll_date)) %>% 
    mutate(Dataset = ifelse(coll_date < "2022-04-02", "Old", "New")) ## Flag older vs. newer datasets
  
  
  ## Symptom onset date is a bit messy, so do it in two rounds
  dat$Symptom_Onset_Date1 <- mdy(dat$Symptom_Onset_Date)
  dat$Symptom_Onset_Date2 <- ymd(dat$Symptom_Onset_Date)
  dat <- dat %>% mutate(Symptom_Onset_Date_New = as_date(ifelse(is.na(Symptom_Onset_Date),NA, 
                                                                ifelse(!is.na(Symptom_Onset_Date1), Symptom_Onset_Date1, Symptom_Onset_Date2))))
  ## A few individuals with odd symptom onset dates reported
  dat <- dat %>% 
    mutate(Symptom_Onset_Date_New = case_when(Unique_Test_ID == 134640 ~ as_date("2022-01-01"), 
                                              Unique_Test_ID == 154850 ~ as_date("2021-12-26"), 
                                              Unique_Test_ID == 138912 ~ as_date("2021-03-08"), 
                                              Unique_Test_ID == 159695 ~ as_date("2021-04-20"), 
                                              Unique_Test_ID == 159793 ~ as_date("2022-03-15"),
                                              Unique_Test_ID == 152267 ~ as_date("2021-06-20"), 
                                              TRUE ~ Symptom_Onset_Date_New))
  
  ## Clean up combined symptom onset date
  dat <- dat %>% 
    select(-c("Symptom_Onset_Date","Symptom_Onset_Date1","Symptom_Onset_Date2")) %>% 
    mutate(rep_delay = coll_date - Symptom_Onset_Date_New) %>% 
    rename(onset_date = Symptom_Onset_Date_New)
  
  ## A few individuals with erroneous symptom onset dates (mostly wrong year)
  dat <- dat %>% 
    mutate(onset_date = case_when(rep_delay > 965 ~ as_date(onset_date + 1095), 
                                  rep_delay < -915 ~ as_date(onset_date - 1095), 
                                  rep_delay > 665 ~ as_date(onset_date + 730), 
                                  rep_delay < -615 ~ as_date(onset_date - 730), 
                                  rep_delay > 300 ~ as_date(onset_date + 365), 
                                  rep_delay < -250 ~ as_date(onset_date - 365), 
                                  TRUE ~ onset_date)) %>%
    mutate(rep_delay = coll_date - onset_date)
  
  ## Output combined, cleaned CSV file
  dat %>% write_csv(str_glue("{dat_folder}/tufts_cts_combined.csv"))
  saveRDS(dat, file=str_glue("{dat_folder}/dat.rds"))
  
  ## Only use data with a symptomatic label, this should be most
  dat_pos <- dat %>% 
    filter(result == "Positive", 
           !is.na(symptomatic), 
           !is.na(Ct_value), 
           Ct_value > 0) %>% 
    select(Unique_Test_ID, age, coll_date:coll_date_int, onset_date)
  
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
  remove(dat1, dat2, epic, soft, dat)
} else {
  # dat <- readRDS(str_glue("{dat_folder}/dat.rds"))
  cat("Skipping raw data cleaning, reading clean data...\n")
  dat_pos <- read_csv(str_glue("./data-public/{key}_cts_clean.csv"), col_types=list(
    result = col_factor(c("Negative", "Positive")), loc_type = col_factor(c("E", "I", "O"))
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

dat_pos <- dat_pos %>% left_join(data_calendar, by=c("coll_date"="date", "coll_date_int"="coll_date_int"))

## For later use ##### THIS IS WEIRD, FIX IT
data_calendar <- bind_rows(data_calendar %>% mutate(symptomatic=FALSE), data_calendar %>% mutate(symptomatic=TRUE))

## Split into variant eras
dat_pos <- dat_pos %>% add_variant_eras(variant_eras, coll_date)

## Rescale Ct values for early and late period (before and after 2022-05-15) to account for assay change
dat_pos_early <- dat_pos %>% 
  rename(Ct_original = Ct_value) %>% 
  arrange(coll_date) %>% 
  filter(coll_date <= "2022-05-15") %>% 
  mutate(Ct_value = Ct_original)

dat_pos_late <- dat_pos %>% 
  rename(Ct_original = Ct_value) %>% 
  arrange(coll_date) %>% 
  filter(coll_date > "2022-05-15") %>% 
  mutate(Ct_value = scales::rescale(Ct_original, to=range(dat_pos_early$Ct_original)))

dat_pos <- bind_rows(dat_pos_early, dat_pos_late)
remove(dat_pos_early, dat_pos_late)
cat("Final total Ct values used:", nrow(dat_pos), "\n")

## Clean up
# saveRDS(dat_pos, file=str_glue("./data-public/{key}_dat_pos.rds"))
remove(data_calendar, wk_data_calendar)
cat("Data processed and ready!\n")