
if (update_basic_analysis == TRUE) {
  ## Set tibble of models to run
  .models <- models
  nc <- FALSE
  
  ## Run iterative model fitting
  source("./code/auxiliary/model_iterative.R")
  
  .models <- unnest_multi_models(.models)
  models_summ_daily <- summarise_multi_models(.models)
  
  print(models_summ_daily)
  
  if (save_basic==TRUE) {
    models_summ_daily %>% save_bak(str_glue("./outputs/{key}_models_summ.rds"))
    models_summ_daily %>% save_bak(str_glue("./outputs/{key}_models_summ.csv"))
  }
  .models %>% save_bak(str_glue("{dat_folder}/{key}_models_fitted.rds"))  
} else {
  cat("\nSkipping basic model fitting!\n\n")
}
