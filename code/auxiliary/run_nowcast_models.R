
if (update_nowcast == TRUE) {
  for (m in nc_slice) {
    ## Subset to specified models, one at a time
    cat("\nFitting nowcast models, round", m, "\n")
    .models <- models %>% slice(m)
    nc <- TRUE
    
    ## Specify dates for nowcast windows and prep model list with sub-models
    nowcast_dates <- unique((dat_pos %>% arrange(coll_wk))$coll_wk)[nc_seq]
    nc_models <- prep_nowcast_models(.models, nowcast_dates, nowcast_window)
    .models <- nc_models
    
    ## Run iterative model fitting
    source("./code/auxiliary/model_iterative.R")
    
    .models <- unnest_multi_models(.models)
    
    save_bak(.models, str_glue("{dat_folder}/{key}_nc_fitted_{m}.rds"))
    
    ## Clean up and free memory
    remove(.models)
    gc()
  }
} else {
  cat("\nSkipping nowcast model fitting!\n\n")
}




