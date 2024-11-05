library(readxl)
library(googlesheets4)

if (update_data == TRUE) {
  ## Specify downloaded Excel file (from Mass DPH dashboard)
  filename <- "./data-public/covid-19-raw-data-3-2-2023.xlsx"
  
  ## Read raw data from downloaded Excel file
  county_dat <- read_excel(filename, sheet="CountyCasesDeaths (Report Date)")
  case_dat <- read_excel(filename, sheet="CasesByDate (Test Date)")
  hosp_dat <- read_excel(filename, sheet="Hospitalization from Hospitals")
  
  ## Convert date formats
  county_dat$Date <- as_date(county_dat$Date)
  case_dat$Date <- as_date(case_dat$Date)
  hosp_dat$Date <- as_date(hosp_dat$Date)
  
  if (ww == TRUE) {
    ## Read raw wastewater data from Google sheet - will require authentication
    ww_dat <- read_sheet('1UKrPBFedwo-pBhroqP6racz1ReUvhg7L0dEm2wsXoEM', range='A3:E', col_names=c('coll_date', 'S', 'N', 'S7', 'N7'), col_types='Diiii')

    ww_dat <- ww_dat %>%
      mutate(date = coll_date, 
             WW_New = (S + N)/2,  # Note - not clear if WW reflects incidence or prevalence
             S7N = round(exp(zoo::rollmeanr(log(S), rollmean_window, fill = NA, na.rm=TRUE))),  # Recalculate corrected rolling geomeans
             N7N = round(exp(zoo::rollmeanr(log(N), rollmean_window, fill = NA, na.rm=TRUE))),
             S7N = pmax(S7, S7N, na.rm=TRUE), 
             N7N = pmax(N7, N7N, na.rm=TRUE), 
             mean_7day_ww = (S7N + N7N)/2) %>%
      select(date, WW_New, mean_7day_ww)
  }
  
  if (hosp == TRUE) {
    ## Select required data from raw tables
    ma_hosp <- hosp_dat %>% 
      select(c(Date,`New COVID-19 hospitalizations`)) %>% 
      rename(Hosp_New = `New COVID-19 hospitalizations`, 
             date = Date) %>% 
      mutate(mean_7day_hosp = zoo::rollmean(Hosp_New,rollmean_window,fill=NA))
  }
  
  ma_dat <- case_dat %>% 
    set_names(c("date","Pos_Total","Pos_New","mean_7day"))
  
  bos_dat <- county_dat %>%  # Note Boston data not currently usable as reporting interval changed
    select(1:4) %>% 
    set_names(c("date","County","Pos_New","Pos_Total")) %>% 
    filter(County == "Suffolk") %>% 
    mutate(mean_7day_bos = zoo::rollmean(Pos_New,rollmean_window, fill=NA)) %>% 
    rename(Pos_New_bos = Pos_New, 
           Pos_Total_bos = Pos_Total)
  
  
  ## Generate a calendar 
  all_calendar <- expand_grid(date=seq(min(ma_dat$date),max(ma_dat$date),by="1 day"))
  all_calendar <- all_calendar %>% 
    mutate(coll_date_int = as.numeric(date)) %>% 
    mutate(day_of_wk = wday(date,label=TRUE))
  
  ## Add CDC MMWR EpiWeek indicator and cumulative EpiWeeks since 2020
  all_calendar <- all_calendar %>% 
    mutate(epi_yr = MMWRweek::MMWRweek(date)$MMWRyear, 
           epi_wk = MMWRweek::MMWRweek(date)$MMWRweek) %>% 
    mutate(epi_wk_comb = case_when(epi_yr == 2021 ~ epi_wk + 53, 
                                   epi_yr == 2022 ~ epi_wk + 53 + 52, 
                                   epi_yr == 2023 ~ epi_wk + 53 + 52 + 52, 
                                   TRUE ~ epi_wk))
  
  ## Combine into single dataframe w/ calendar
  combined_dat <- ma_dat %>% 
    left_join(bos_dat) %>% 
    left_join(ma_hosp) %>% 
    left_join(ww_dat) %>% 
    left_join(all_calendar)
    
  ## Calculate daily growth rates
  case_dat_daily <- combined_dat %>% 
    mutate(gr = log(Pos_New/lag(Pos_New,1)),
           gr_roll = log(mean_7day/lag(mean_7day,1)), 
           gr_bos = log(Pos_New_bos/lag(Pos_New_bos,1)), 
           gr_roll_bos = log(mean_7day_bos/lag(mean_7day_bos,1)), 
           gr_hosp = log(Hosp_New/lag(Hosp_New,1)), 
           gr_roll_hosp = log(mean_7day_hosp/lag(mean_7day_hosp,1)), 
           gr_ww = log(WW_New/lag(WW_New,1)), 
           gr_roll_ww = log(mean_7day_ww/lag(mean_7day_ww,1))) %>% 
    mutate(sm_gr = zoo::rollapplyr(gr, rollmean_window, mean, na.rm=TRUE, partial=TRUE),
           sm_gr_roll = zoo::rollapplyr(gr_roll, rollmean_window, mean, na.rm=TRUE, partial=TRUE), 
           sm_gr_roll_c = zoo::rollapply(gr_roll, rollmean_window, mean, na.rm=TRUE, partial=TRUE),
           sm_gr_roll_d2 = lead(sm_gr_roll_c, inf_settings[[1]]) - lag(sm_gr_roll_c, inf_settings[[1]]), 
           inflection = (abs(sm_gr_roll_d2) >= inf_settings[[2]]), 
           growing = as.numeric(gr_roll > 0), 
           sm_gr_bos = zoo::rollapplyr(gr_bos, rollmean_window, mean, na.rm=TRUE, partial=TRUE),
           sm_gr_roll_bos = zoo::rollapplyr(gr_roll_bos, rollmean_window, mean, na.rm=TRUE, partial=TRUE), 
           growing_bos = as.numeric(gr_roll_bos > 0), 
           sm_gr_hosp = zoo::rollapplyr(gr_hosp, rollmean_window, mean, na.rm=TRUE, partial=TRUE),
           sm_gr_roll_hosp = zoo::rollapplyr(gr_roll_hosp, rollmean_window, mean, na.rm=TRUE, partial=TRUE), 
           growing_hosp = as.numeric(gr_roll_hosp > 0), 
           sm_gr_ww = zoo::rollapplyr(gr_ww, rollmean_window, mean, na.rm=TRUE, partial=TRUE),
           sm_gr_roll_ww = zoo::rollapplyr(gr_roll_ww, rollmean_window, mean, na.rm=TRUE, partial=TRUE), 
           growing_ww = as.numeric(gr_roll_ww > 0)) %>% 
    mutate(across(starts_with('gr')|starts_with('sm'), ~ ifelse(is.nan(.), NA, .)),  # Replace NaN values (na_if() not working)
           across(starts_with('gr')|starts_with('sm'), \(x) na_if(x, Inf)),  # Also replace Inf values
           across(starts_with('gr')|starts_with('sm'), \(x) na_if(x, -Inf)))

  
  
  # ## Create weekly version
  # case_dat_weekly <- combined_dat %>% 
  #   group_by(epi_wk_comb) %>% 
  #   summarise(Pos_wk = sum(Pos_New), 
  #             Pos_wk_bos = ifelse(sum(Pos_New_bos, na.rm=TRUE) == 0, NA, sum(Pos_New_bos, na.rm=TRUE)), 
  #             Hosp_wk = sum(Hosp_New), 
  #             WW_wk = sum(WW_New)) %>%
  #   mutate(gr_wk = log(Pos_wk/lag(Pos_wk,1)), 
  #          grow_wk = as.numeric(gr_wk > 0), 
  #          gr_wk_bos = log(Pos_wk_bos/lag(Pos_wk_bos,1)), 
  #          grow_wk_bos = as.numeric(gr_wk_bos > 0), 
  #          gr_wk_hosp = log(Hosp_wk/lag(Hosp_wk,1)), 
  #          grow_wk_hosp = as.numeric(gr_wk_hosp > 0), 
  #          gr_wk_ww = log(WW_wk/lag(WW_wk,1)), 
  #          grow_wk_ww = as.numeric(gr_wk_ww > 0)) %>% 
  #   mutate(across(gr_wk:grow_wk_ww, ~ ifelse(is.nan(.), NA, .)),  # Replace NaN values (na_if() not working)
  #          across(gr_wk:grow_wk_ww, \(x) na_if(x, Inf)),  # Also replace Inf values
  #          across(gr_wk:grow_wk_ww, \(x) na_if(x, Inf)))
    
  ## Create weekly calendar
  # week_calendar <- all_calendar %>% group_by(epi_wk_comb) %>% summarise(across(-day_of_wk, ~ min(.)))
  # case_dat_weekly <- case_dat_weekly %>% left_join(week_calendar)
  
  ## Check lags between incidence and hospitalisation
  mean_7day <- combined_dat %>% select(mean_7day, mean_7day_hosp) %>% drop_na() %>% pull(mean_7day)
  mean_7day_hosp <- combined_dat %>% select(mean_7day, mean_7day_hosp) %>% drop_na() %>% pull(mean_7day_hosp)
  hosp_inc <- ccf(mean_7day,mean_7day_hosp,main=paste0("Daily hospitalizations\n vs. Daily incidence"),ylim=c(0,1),cex.main=0.5)
  hosp_inc_lag <- hosp_inc$lag[which.max(abs(hosp_inc$acf))]
  hosp_inc
  cat("Hospitalisation trend is", hosp_inc_lag, "days offset compared to incidence trend\n")
  
  ## Clean up
  remove(county_dat, case_dat, hosp_dat, ma_dat, bos_dat, ma_hosp, ww_dat, all_calendar, mean_7day, mean_7day_hosp)
  saveRDS(case_dat_daily, file=str_glue("{dat_folder}/case_dat_daily.rds"))
  # saveRDS(case_dat_weekly, file=str_glue("{dat_folder}/case_dat_weekly.rds"))
} else {
  case_dat_daily <- readRDS(str_glue("{dat_folder}/case_dat_daily.rds"))
  # case_dat_weekly <- readRDS(str_glue("{dat_folder}/case_dat_weekly.rds"))
}



