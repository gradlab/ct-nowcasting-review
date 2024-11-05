
if (update_data == TRUE) {
  # ## Read raw data from LA Times datadesk Github
  # county_dat <- read_csv("https://raw.githubusercontent.com/datadesk/california-coronavirus-data/master/cdph-county-cases-deaths.csv") %>% 
  #   filter(county == "Los Angeles")
  
  ## Read raw data from LAC DPH Covid dashboard: http://dashboard.publichealth.lacounty.gov/covid19_surveillance_dashboard/
  county_dat <- read_csv("./data-public/LA_County_Covid19_cases_deaths_date_table.csv")
  
  
  ## Read raw hospitalisation data from healthdata.gov dashboard
  hosp_dat <- read_csv("https://healthdata.gov/resource/anag-cw7u.csv?state=CA&$limit=999999") %>% 
    filter(city %in% c("AGOURA HILLS", "ALHAMBRA", "ARCADIA", "ARTESIA", "AVALON", 
                       "AZUSA", "BALDWIN PARK", "BELL", "BELL GARDENS", "BELLFLOWER", 
                       "BEVERLY HILLS", "BRADBURY", "BURBANK", "CALABASAS", "CARSON", 
                       "CERRITOS", "CLAREMONT", "COMMERCE", "COMPTON", "COVINA", 
                       "CUDAHY", "CULVER CITY", "DIAMOND BAR", "DOWNEY", "DUARTE", 
                       "EL MONTE", "EL SEGUNDO", "GARDENA", "GLENDALE", "GLENDORA", 
                       "HAWAIIAN GARDENS", "HAWTHORNE", "HERMOSA BEACH", "HIDDEN HILLS", "HUNTINGTON PARK", 
                       "INDUSTRY", "INGLEWOOD", "IRWINDALE", "LA CAÑADA FLINTRIDGE", "LA MIRADA", 
                       "LA PUENTE", "LA VERNE", "LA HABRA HEIGHTS", "LAKEWOOD", "LANCASTER", 
                       "LAWNDALE", "LOMITA", "LONG BEACH", "LOS ANGELES", "LYNWOOD", 
                       "MALIBU", "MANHATTAN BEACH", "MAYWOOD", "MONROVIA", "MONTEBELLO", 
                       "MONTEREY PARK", "NORWALK", "PALMDALE", "PALOS VERDES ESTATES", "PARAMOUNT", 
                       "PASADENA", "PICO RIVERA", "POMONA", "RANCHO PALOS VERDES", "REDONDO BEACH", 
                       "ROLLING HILLS", "ROLLING HILLS ESTATES", "ROSEMEAD", "SAN DIMAS", "SAN FERNANDO", 
                       "SAN GABRIEL", "SAN MARINO", "SANTA CLARITA", "SANTA FE SPRINGS", "SANTA MONICA", 
                       "SIERRA MADRE", "SIGNAL HILL", "SOUTH EL MONTE", "SOUTH GATE", "SOUTH PASADENA", 
                       "TEMPLE CITY", "TORRANCE", "VERNON", "WALNUT", "WEST COVINA", 
                       "WEST HOLLYWOOD", "WESTLAKE VILLAGE", "WHITTIER")) %>% 
    select(hospital_pk:fips_code, 
           previous_day_admission_adult_covid_confirmed_7_day_sum, 
           previous_day_admission_pediatric_covid_confirmed_7_day_sum, 
           previous_day_covid_ed_visits_7_day_sum, 
           previous_day_admission_adult_covid_suspected_7_day_sum, 
           previous_day_admission_pediatric_covid_suspected_7_day_sum)
  
  
  ## Select required data from raw tables
  # lac_dat <- county_dat %>% 
  #   select(date, confirmed_cases) %>% 
  #   arrange(date) %>% 
  #   mutate(Pos_New = confirmed_cases - lag(confirmed_cases), 
  #          mean_7day = zoo::rollmean(Pos_New, rollmean_window, fill=NA))
  
  lac_dat <- county_dat %>% 
    select(date_use, new_case) %>% 
    arrange(date_use) %>% 
    rename(date = date_use, 
           Pos_New = new_case) %>% 
    mutate(mean_7day = zoo::rollmean(Pos_New, rollmean_window, fill=NA))
  
  lac_hosp <- hosp_dat %>%
    mutate(across(everything(), ~replace(., .== -999999, NA))) %>% 
    mutate(Hosp_New = rowSums(across(c(previous_day_admission_adult_covid_confirmed_7_day_sum,
                                       previous_day_admission_pediatric_covid_confirmed_7_day_sum, 
                                       previous_day_admission_adult_covid_suspected_7_day_sum, 
                                       previous_day_admission_pediatric_covid_suspected_7_day_sum)), 
                              na.rm=TRUE), 
           ED_New = previous_day_covid_ed_visits_7_day_sum, 
           coll_wk = as.Date(collection_week)) %>% 
    group_by(coll_wk) %>% 
    summarise(Hosp_New = sum(Hosp_New, na.rm=TRUE), 
              ED_New = sum(ED_New, na.rm=TRUE))
  
  
  ## Generate a calendar 
  all_calendar <- expand_grid(date=seq(min(lac_dat$date),max(lac_dat$date),by="1 day"))
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
  combined_dat <- lac_dat %>% 
    left_join(lac_hosp, by=c('date'='coll_wk')) %>%
    left_join(all_calendar)
                                                                                                                                                                                                                                                                                                                                                       
                                                                                                                                                                                                                      ## Calculate daily growth rates
  case_dat_daily <- combined_dat %>%                                                           
    mutate(gr = log(Pos_New/lag(Pos_New,1)),
           gr_roll = log(mean_7day/lag(mean_7day,1))) %>%  
           # gr_hosp = log(Hosp_New/lag(Hosp_New,1)), 
           # gr_roll_hosp = log(mean_7day_hosp/lag(mean_7day_hosp,1))) %>% 
    mutate(sm_gr = zoo::rollapplyr(gr, rollmean_window, mean, na.rm=TRUE, partial=TRUE),
           sm_gr_roll = zoo::rollapplyr(gr_roll, rollmean_window, mean, na.rm=TRUE, partial=TRUE), 
           sm_gr_roll_c = zoo::rollapply(gr_roll, rollmean_window, mean, na.rm=TRUE, partial=TRUE),
           sm_gr_roll_d2 = lead(sm_gr_roll_c, inf_settings[[1]]) - lag(sm_gr_roll_c, inf_settings[[1]]), 
           inflection = (abs(sm_gr_roll_d2) >= inf_settings[[2]]), 
           growing = as.numeric(gr_roll > 0)) %>% 
    mutate(across(starts_with('gr')|starts_with('sm'), ~ ifelse(is.nan(.), NA, .)),  # Replace NaN values (na_if() not working)
           across(starts_with('gr')|starts_with('sm'), \(x) na_if(x, Inf)),  # Also replace Inf values
           across(starts_with('gr')|starts_with('sm'), \(x) na_if(x, -Inf)))
           # sm_gr_hosp = zoo::rollmean(gr_hosp, rollmean_window, fill = NA),
           # sm_gr_roll_hosp = zoo::rollmean(gr_roll_hosp, rollmean_window, fill = NA), 
           # growing_hosp = as.numeric(gr_roll_hosp > 0)) %>% 
    # mutate(across(gr:growing_hosp, ~ ifelse(is.nan(.), NA, .)),  # Replace NaN values (na_if() not working)
    #        across(gr:growing_hosp, na_if, Inf),  # Also replace Inf values
    #        across(gr:growing_hosp, na_if, -Inf))
  
  
  # ## Create weekly version
  # case_dat_weekly <- combined_dat %>% 
  #   group_by(epi_wk_comb) %>% 
  #   summarise(Pos_wk = sum(Pos_New),
  #             Hosp_wk = sum(Hosp_New, na.rm=TRUE)) %>%
  #   mutate(gr_wk = log(Pos_wk/lag(Pos_wk,1)), 
  #          grow_wk = as.numeric(gr_wk > 0), 
  #          gr_wk_hosp = log(Hosp_wk/lag(Hosp_wk,1)),
  #          grow_wk_hosp = as.numeric(gr_wk_hosp > 0)) %>%
  #   mutate(across(gr_wk:grow_wk_ww, ~ ifelse(is.nan(.), NA, .)),  # Replace NaN values (na_if() not working)
  #          across(gr_wk:grow_wk_ww, \(x) na_if(x, Inf)),  # Also replace Inf values
  #          across(gr_wk:grow_wk_ww, \(x) na_if(x, Inf)))
    
  # ## Create weekly calendar
  # week_calendar <- all_calendar %>% group_by(epi_wk_comb) %>% summarise(across(-day_of_wk, ~ min(.)))
  # case_dat_weekly <- case_dat_weekly %>% left_join(week_calendar)
  
  ## Clean up
  remove(county_dat, hosp_dat, lac_dat, all_calendar)
  saveRDS(case_dat_daily, file=str_glue("{dat_folder}/case_dat_daily.rds"))
  # saveRDS(case_dat_weekly, file=str_glue("{dat_folder}/case_dat_weekly.rds"))
} else {
  case_dat_daily <- readRDS(str_glue("{dat_folder}/case_dat_daily.rds"))
  # case_dat_weekly <- readRDS(str_glue("{dat_folder}/case_dat_weekly.rds"))
}
