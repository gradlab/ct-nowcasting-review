
if (process_nowcast == TRUE) {
  cat("\nReloading and merging fitted models...\n")
  mdls <- vector("list", length(nc_slice))
  for (m in nc_slice) {
    mdls[[m]] <- readRDS(str_glue("{dat_folder}/{key}_nc_fitted_{m}.rds")) %>% mutate(mdl = m)
  }
  .models <- bind_rows(mdls)
  remove(mdls)
  
  models_summ_daily <- readRDS(str_glue("./outputs/{key}_models_summ.rds"))
  
  cat("Processing model outputs...\n")
  
  ## Calculate GOF and ROC directly from combined nowcast predicted values
  nc_models_summ_out <- .models %>% 
    group_by(mdl) %>% 
    summarise(pred_gr = list(bind_rows(pred_gr_preds_out)), 
              pred_dir = list(bind_rows(pred_direct_preds_out)), 
              gr_var = first(gr_var),  # Pull out specific gr and direction vars to calculate GOF/ROC
              direct_var = first(direct_var)) %>% 
    mutate(pred_gr = map(pred_gr, left_join, case_dat_daily %>% rename(coll_date=date) %>% select(coll_date, inflection)),  # Add inflection point flag
           pred_dir = map(pred_dir, left_join, case_dat_daily %>% rename(coll_date=date) %>% select(coll_date, inflection))) %>%
    mutate(pred_inf = map(pred_gr, filter, inflection==TRUE), 
           pred_dir_inf = map(pred_dir, filter, inflection==TRUE), 
           out = map2(pred_gr, gr_var, calc_gof, "Estimate", "coverage"), 
           inf = map2(pred_inf , gr_var, calc_gof, "Estimate", "coverage")) %>% 
    unnest_wider(out, names_sep="_") %>% 
    unnest_wider(inf, names_sep="_") %>% 
    mutate(out = map2(pred_dir, direct_var, calc_roc, "Estimate"), 
           inf = map2(pred_dir_inf, direct_var, calc_roc, "Estimate")) %>%
    unnest_wider(out, names_sep="_", strict=TRUE) %>% 
    unnest_wider(inf, names_sep="_", strict=TRUE) %>% 
    select(-gr_var, -direct_var)  # gr and direciton vars no longer needed
  
  ## Create full by-index and averaged-by-model summaries
  nc_models_summ_full <- .models %>% select(1:3) %>% left_join(summarise_multi_models(.models))
  nc_models_summ_avg <- summarise_nc_models(.models, models %>% slice(nc_slice), nc_models_summ_full)
  
  ## Create combined output with in-sample and out-of-sample results
  nc_models_summ_out <- nc_models_summ_avg %>% 
    select(mdl, plot_label) %>% 
    left_join(models_summ_daily %>%  # Use actual in-sample values, not averages from nowcast
                slice(1:(nrow(models_summ_daily)/2))) %>% 
    select(mdl:in_nRMSEd, in_spear, in_cover, in_AUC) %>% 
    left_join(nc_models_summ_out) %>% 
    select(mdl:model_data, pred_gr:pred_dir_inf, in_AIC:in_AUC, out_MSE:inf_AUC)
  
  nc_models_summ_out_clean <- nc_models_summ_out %>% 
    select(-(pred_gr:pred_dir_inf), -c("out_MSE", "out_ROC", "inf_MSE", "inf_ROC")) %>% 
    select(mdl:in_spear, out_RMSE:out_spear, inf_RMSE:inf_spear, in_cover, out_cover, inf_cover, in_AUC, out_AUC, inf_AUC)
  
  print(nc_models_summ_out_clean)
  
  cat("Saving summary results...\n")
  nc_models_summ_avg %>% save_bak(str_glue("./outputs/{key}_nc_models_summ_avg.rds"))
  nc_models_summ_full %>% save_bak(str_glue("./outputs/{key}_nc_models_summ_full.rds"))
  nc_models_summ_out %>% save_bak(str_glue("./outputs/{key}_nc_models_summ_out.rds"))
  nc_models_summ_out_clean %>% save_bak(str_glue("./outputs/{key}_nc_models_summ_out_clean.rds"))
  
  nc_models_summ_avg %>% save_bak(str_glue("./outputs/{key}_nc_models_summ_avg.csv"))
  nc_models_summ_full %>% save_bak(str_glue("./outputs/{key}_nc_models_summ_full.csv"))
  nc_models_summ_out_clean %>% save_bak(str_glue("./outputs/{key}_nc_models_summ_out.csv"))
  
  remove(.models)
  
} else {
  cat("\nSkipping results merging!\n\n")
}
