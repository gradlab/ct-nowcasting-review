if (process_nowcast == TRUE) {
  cat("\nReloading and merging fitted models...\n")
  ## Get concatenated predicted values for baseline model (samp_frac == 1) to use as comparison
  comp_preds <- readRDS(str_glue("{dat_folder}/{key}_1_nc_fitted_1.rds")) %>% 
    group_by(mdl) %>% 
    summarise(pred_gr_comp = list(bind_rows(pred_gr_preds_out)), 
              pred_dir_comp = list(bind_rows(pred_direct_preds_out))) %>% 
    select(-mdl)
  
  ## Initialise containers for results by draw number
  mdls_daily <- vector("list", (draws + sum(startsWith(samp_fracs, "t"))))
  nc_mdls_avg <- vector("list", (draws + sum(startsWith(samp_fracs, "t"))))
  nc_mdls_full <- vector("list", (draws + sum(startsWith(samp_fracs, "t"))))
  nc_mdls_out <- vector("list", (draws + sum(startsWith(samp_fracs, "t"))))
  
  for (i in 1:(draws + sum(startsWith(samp_fracs, "t")))) {
    if (i <= draws) { # Process repeated-draw downsampled results
      ki <- sprintf("%03d", i)
      
      ## Iterate over different sample fractions to load and combine model outputs
      mdls <- vector("list", (length(samp_fracs) - sum(startsWith(samp_fracs, "t")))) # Ignore trim fractions
      nc_mdls <- vector("list", (length(samp_fracs) - sum(startsWith(samp_fracs, "t")))) # Ignore trim fractions
      for (j in 1:(length(samp_fracs) - sum(startsWith(samp_fracs, "t")))) {
        f <- samp_fracs[[j]]
        
        mdls[[j]] <- readRDS(str_glue("{dat_folder}/{key}_{f}_{ki}_models_fitted.rds")) %>% mutate(mdl=f) %>% select(mdl, everything())
        nc_mdls[[j]] <- readRDS(str_glue("{dat_folder}/{key}_{f}_{ki}_nc_fitted_1.rds")) %>% mutate(mdl=f)
      }
      .models <- bind_rows(mdls)
      .models_nc <- bind_rows(nc_mdls)
      remove(mdls, nc_mdls)
    } else if (i > draws) { # Process daily-trimmed downsampled results
      f <- Filter(\(x) startsWith(x, "t"), samp_fracs)[[i-draws]]
      
      .models <- readRDS(str_glue("{dat_folder}/{key}_{f}_models_fitted.rds")) %>% mutate(mdl=f) %>% select(mdl, everything())
      .models_nc <- readRDS(str_glue("{dat_folder}/{key}_{f}_nc_fitted_1.rds")) %>% mutate(mdl=f)
    }
    
    cat("Processing model outputs, row", i, "...\n")
    ## Reorder basic models list
    .models <- .models %>% slice(seq(1, nrow(.models)-1, 2), seq(2, nrow(.models), 2))
    
    ## Reprocess and save combined basic models summary
    models_summ_daily <- summarise_multi_models(.models)
    
    ## Process combined nowcast models
    nc_models_summ_out <- .models_nc %>% 
      group_by(mdl) %>% 
      summarise(pred_gr = list(bind_rows(pred_gr_preds_out)), 
                pred_dir = list(bind_rows(pred_direct_preds_out))) %>% 
      mutate(pred_gr = map(pred_gr, left_join, case_dat_daily %>% rename(coll_date=date) %>% select(coll_date, inflection)),  # Add inflection point flag
             pred_dir = map(pred_dir, left_join, case_dat_daily %>% rename(coll_date=date) %>% select(coll_date, inflection))) %>% 
      bind_cols(comp_preds) %>%  # Add results from comparison run
      mutate(pred_gr_comp = map2(pred_gr_comp, pred_gr,  # Limit coll_dates in comparison to those in main output
                                 \(x, y) x %>% filter(coll_date %in% (y %>% drop_na() %>% pull(coll_date)))), 
             pred_dir_comp = map2(pred_dir_comp, pred_dir,
                                 \(x, y) x %>% filter(coll_date %in% (y %>% drop_na() %>% pull(coll_date))))) %>% 
      mutate(pred_inf = map(pred_gr, filter, inflection==TRUE), 
             pred_dir_inf = map(pred_dir, filter, inflection==TRUE), 
             out = map(pred_gr, calc_gof, "gr_roll", "Estimate", "coverage"), 
             inf = map(pred_inf , calc_gof, "gr_roll", "Estimate", "coverage"), 
             comp = map(pred_gr_comp, calc_gof, "gr_roll", "Estimate", "coverage")) %>% 
      unnest_wider(out, names_sep="_") %>% 
      unnest_wider(inf, names_sep="_") %>% 
      unnest_wider(comp, names_sep="_") %>% 
      mutate(out = map(pred_dir, calc_roc, "growing", "Estimate"), 
             inf = map(pred_dir_inf, calc_roc, "growing", "Estimate"), 
             comp = map(pred_dir_comp, calc_roc, "growing", "Estimate")) %>%
      unnest_wider(out, names_sep="_", strict=TRUE) %>% 
      unnest_wider(inf, names_sep="_", strict=TRUE) %>% 
      unnest_wider(comp, names_sep="_", strict=TRUE) %>% 
      mutate(comp_gr_n = map_int(pred_gr_comp, nrow), 
             comp_dir_n = map_int(pred_dir_comp, nrow))
    
    ## Create full by-index and averaged-by-model summaries
    nc_models_summ_full <- .models_nc %>% select(1:3) %>% left_join(summarise_multi_models(.models_nc))
    nc_models_summ_avg <- summarise_nc_models(.models_nc, 
                                              models_summ_daily %>% 
                                                slice(1:(nrow(models_summ_daily)/2)) %>% 
                                                select(plot_label:model_data), 
                                              nc_models_summ_full)
    
    ## Create combined output with in-sample and out-of-sample results
    nc_models_summ_out <- nc_models_summ_avg %>% 
      select(mdl, plot_label) %>% 
      left_join(models_summ_daily %>%  # Use actual in-sample values, not averages from nowcast
                  slice(1:(nrow(models_summ_daily)/2))) %>% 
      select(mdl:in_nRMSEd, in_spear, in_cover, in_AUC) %>% 
      left_join(nc_models_summ_out) %>% 
      select(mdl:model_data, pred_gr:pred_dir_inf, in_AIC:in_AUC, out_MSE:comp_dir_n)
    
    nc_models_summ_out_clean <- nc_models_summ_out %>% 
      select(-(pred_gr:pred_dir_inf), -ends_with(c("_MSE", "_ROC"))) %>% 
      select(mdl:in_spear, out_RMSE:out_spear, inf_RMSE:inf_spear, comp_RMSE:comp_spear, ends_with("cover"), ends_with("AUC"), comp_gr_n:comp_dir_n)
    
    if (i <= draws) {
      mdls_daily[[i]] <- models_summ_daily %>% mutate(draw=i) %>% select(draw, everything())
      nc_mdls_avg[[i]] <- nc_models_summ_avg %>% mutate(draw=i) %>% select(draw, mdl, everything())
      nc_mdls_full[[i]] <- nc_models_summ_full %>% mutate(draw=i) %>% select(draw, mdl, everything())
      nc_mdls_out[[i]] <- nc_models_summ_out_clean %>% mutate(draw=i) %>% select(draw, mdl, everything())
    } else if (i > draws) {
      mdls_daily[[i]] <- models_summ_daily %>% mutate(draw=1) %>% select(draw, everything())
      nc_mdls_avg[[i]] <- nc_models_summ_avg %>% mutate(draw=1) %>% select(draw, mdl, everything())
      nc_mdls_full[[i]] <- nc_models_summ_full %>% mutate(draw=1) %>% select(draw, mdl, everything())
      nc_mdls_out[[i]] <- nc_models_summ_out_clean %>% mutate(draw=1) %>% select(draw, mdl, everything())
    }
    remove(models_summ_daily, nc_models_summ_avg, nc_models_summ_full, nc_models_summ_out, nc_models_summ_out_clean)
  }
  
  ## Merge repeated draw results  
  models_summ_daily <- bind_rows(mdls_daily) %>% arrange(model_data)
  nc_models_summ_avg <- bind_rows(nc_mdls_avg)
  nc_models_summ_full <- bind_rows(nc_mdls_full)
  nc_models_summ_out <- bind_rows(nc_mdls_out)

  remove(.models, .models_nc, mdls_daily, nc_mdls_avg, nc_mdls_full, nc_mdls_out)
  
  cat("Saving full downsample results...\n")
  models_summ_daily %>% save_bak(str_glue("./outputs/{key}_nc_models_summ_avg_all.rds"))
  nc_models_summ_avg %>% save_bak(str_glue("./outputs/{key}_nc_models_summ_avg_all.rds"))
  nc_models_summ_full %>% save_bak(str_glue("./outputs/{key}_nc_models_summ_full_all.rds"))
  nc_models_summ_out %>% save_bak(str_glue("./outputs/{key}_nc_models_summ_out_all.rds"))
  
  cat("Summarising draws...\n")
  models_summ_daily <- models_summ_daily %>% 
    filter(draw==1) %>% 
    select(plot_label:model_data) %>% 
    left_join(models_summ_daily %>% 
                group_by(plot_label) %>% 
                summarise(across(in_AIC:gr_rej, ~ mean(.x, na.rm=TRUE))))
  nc_models_summ_avg <- nc_models_summ_avg %>% 
    filter(draw==1) %>% 
    select(mdl:model) %>% 
    left_join(nc_models_summ_avg %>% 
                group_by(mdl) %>% 
                summarise(across(in_AIC:out_AUC, ~ mean(.x, na.rm=TRUE)))) %>% 
    mutate(mdl = fct_relevel(mdl, samp_fracs))
  nc_models_summ_full <- nc_models_summ_full %>% 
    filter(draw==1) %>% 
    select(mdl:model) %>% 
    left_join(nc_models_summ_full %>% 
                group_by(mdl) %>% 
                summarise(across(in_AIC:gr_rej, ~ mean(.x, na.rm=TRUE)))) %>% 
    mutate(mdl = fct_relevel(mdl, samp_fracs))
  nc_models_summ_out <- nc_models_summ_out %>% 
    filter(draw==1) %>% 
    select(mdl:model) %>% 
    left_join(nc_models_summ_out %>% 
                group_by(mdl) %>% 
                summarise(across(in_AIC:comp_dir_n, ~ mean(.x, na.rm=TRUE)))) %>% 
    mutate(mdl = fct_relevel(mdl, samp_fracs))
  
  cat("Saving summarised results...\n")
  models_summ_daily %>% save_bak(str_glue("./outputs/{key}_nc_models_summ_avg.rds"))
  nc_models_summ_avg %>% save_bak(str_glue("./outputs/{key}_nc_models_summ_avg.rds"))
  nc_models_summ_full %>% save_bak(str_glue("./outputs/{key}_nc_models_summ_full.rds"))
  nc_models_summ_out %>% save_bak(str_glue("./outputs/{key}_nc_models_summ_out.rds"))
  
  nc_models_summ_avg %>% save_bak(str_glue("./outputs/{key}_nc_models_summ_avg.csv"))
  nc_models_summ_full %>% save_bak(str_glue("./outputs/{key}_nc_models_summ_full.csv"))
  nc_models_summ_out %>% save_bak(str_glue("./outputs/{key}_nc_models_summ_out.csv"))

} else {
  cat("\nSkipping results merging!\n\n")
  nc_models_summ_out <- readRDS(str_glue("./outputs/{key}_nc_models_summ_out.rds"))
}


# comp_preds$pred_gr_comp[[1]] %>% left_join(model_dat_daily %>% select(coll_date, sm_N, N)) %>% mutate(resid = abs(gr_roll - Estimate)) %>% 
#   ggplot() +
#   geom_point(aes(x=log(sm_N), y=resid), alpha=0.2)
# 
# 
# comp_preds$pred_gr_comp[[1]] %>% left_join(model_dat_daily %>% select(coll_date, sm_N, N)) %>% mutate(resid = abs(gr_roll - Estimate)) %>% 
#   ggplot() +
#   geom_histogram(aes(x=N), binwidth=1)
# 
# comp_preds$pred_gr_comp[[1]] %>% 
#   left_join(model_dat_daily %>% select(coll_date, sm_N, N)) %>% 
#   mutate(resid = abs(gr_roll - Estimate)) %>% 
#   mutate(bin = case_when(N >= 90 ~ "D", 
#                          N >= 60 ~ "C", 
#                          N >= 30 ~ "B", 
#                          N >= 0 ~ "A")) %>% 
#   group_by(bin) %>% 
#   summarise(spear = spear_func(gr_roll, Estimate), 
#             RMSE = sqrt(mean(resid ** 2, na.rm=TRUE)))
# 
# +
#   xlim(c(0,250)) +
#   ylim(c(0, 0.2))
