

#===============================================================================
# READ AND PREP DATA
#===============================================================================

# ## Group data by day and week
# grouping_vars <- c()
# source("./code/auxiliary/ct_data_grouping.R")

keys <- c("MGB", "LAC", "TFT")

comb_summ <- vector("list", length(keys))

for (i in 1:length(keys)) {
  key <- keys[[i]]
  cat(key)
  
  summ <- readRDS(str_glue("./outputs/{key}_models_summ.rds"))
  
  comb_summ[[i]] <- tibble(
    key = key, 
    .in = list(summ %>% filter(as.character(test_data)=="NULL")),
    .tt = list(summ %>% filter(as.character(test_data)!="NULL")), 
    .nc = list(readRDS(str_glue("./outputs/{key}_nc_models_summ_out.rds")))
  )
  
  remove(summ)
  # summ <- readRDS(str_glue("./outputs/{key}_models_summ.rds"))
  # assign(str_glue("{key}_summ"), summ %>% filter(test_data=="NULL"))
  # assign(str_glue("{key}_summ_tt"), summ %>% filter(test_data!="NULL"))
  # assign(str_glue("{key}_summ_nc"), readRDS(str_glue("./outputs/{key}_nc_models_summ_out.rds")))
  # remove(summ)
}

comb_summ <- bind_rows(comb_summ)

# ## Read in results files
# models <- readRDS(str_glue("{dat_folder}/{key}_models_fitted.rds"))
# 
# ## Pull out predictions
# case_dat <- comb_dat_daily %>% mutate(id = consecutive_id(var_era))

#===============================================================================
# CREATE PAPER SUMMARY TABLES
#===============================================================================

comb_table <- comb_summ %>% 
  # mutate(across(.in:.nc, \(x) x = map(x, \(y) y %>% filter(plot_label=="dmskw_tp_"))))
  mutate(.in = map(.in, \(x) x %>% filter(plot_label=="dmskw_cr_")), 
         .tt = map(.tt, \(x) x %>% filter(plot_label=="dmskw_cr_tt_")), 
         .nc = map(.nc, \(x) x %>% filter(plot_label=="dmskw_cr_")))


## Create concise summary table for main paper
comb_table_main <- comb_table %>% 
  select(key, .nc) %>% 
  mutate(.nc = map(.nc, \(x) x %>% select(in_AIC, ends_with("RMSE"), ends_with("AUC")))) %>% 
  unnest_wider(.nc) %>% 
  mutate(in_AIC = round(in_AIC), 
         across(in_RMSE:inf_AUC, \(x) signif(x, 3))) %>% 
  rename(Dataset = key, 
         `AIC` = in_AIC, 
         `RMSE, in-sample` = in_RMSE, 
         `RMSE, nowcast` = out_RMSE,
         `RMSE, inflection` = inf_RMSE, 
         `AUC, in-sample` = in_AUC, 
         `AUC, nowcast` = out_AUC, 
         `AUC, inflection` = inf_AUC)

## Create detailed summary table for supplement
comb_table_full <- comb_table %>% 
  select(key, .nc, .tt) %>% 
  mutate(.nc = map(.nc, \(x) x %>% select(in_AIC, in_BIC, ends_with("RMSE"), ends_with("spear"), ends_with("cover"), ends_with("AUC"))), 
         .tt = map(.tt, \(x) x %>% select(out_RMSE, out_spear, out_cover, out_AUC))) %>% 
  unnest_wider(.nc) %>% 
  unnest_wider(.tt, names_sep = "_") %>% 
  select(key, in_AIC, in_BIC, ends_with("RMSE"), ends_with("spear"), ends_with("cover"), ends_with("AUC")) %>% 
  mutate(across(in_AIC:in_BIC, \(x) round(x)), 
         across(in_RMSE:.tt_out_AUC, \(x) signif(x, 3))) %>% 
  rename(Dataset = key, 
         `AIC` = in_AIC, 
         `BIC` = in_BIC,
         `RMSE, in-sample` = in_RMSE, 
         `RMSE, nowcast` = out_RMSE,
         `RMSE, inflection` = inf_RMSE, 
         `RMSE, fixed test` = .tt_out_RMSE, 
         `Rho, in-sample` = in_spear, 
         `Rho, nowcast` = out_spear,
         `Rho, inflection` = inf_spear, 
         `Rho, fixed test` = .tt_out_spear, 
         `Coverage, in-sample` = in_cover, 
         `Coverage, nowcast` = out_cover,
         `Coverage, inflection` = inf_cover, 
         `Coverage, fixed test` = .tt_out_cover, 
         `AUC, in-sample` = in_AUC, 
         `AUC, nowcast` = out_AUC, 
         `AUC, inflection` = inf_AUC, 
         `AUC, fixed test` = .tt_out_AUC)

comb_table_main %>% save_bak(str_glue("./outputs/Comb_perf_table.rds"))
comb_table_main %>% save_bak(str_glue("./outputs/Comb_perf_table.csv"))

comb_table_full %>% save_bak(str_glue("./outputs/Comb_perf_table_full.rds"))
comb_table_full %>% save_bak(str_glue("./outputs/Comb_perf_table_full.csv"))


#===============================================================================
# SENSITIVITY ANALYSIS RESULTS
#===============================================================================

## Create sensitivity analysis summary table

## Read in data
sens_keys <- list(MGB=c(main_mdl, "dmskw_o_cr_"), LAC=c(main_mdl, "dmskw_sym_cr_", "dmskw_asyn_cr_", "dmskw_nv_cr_"))

comb_sens <- vector("list", length(sens_keys))

for (i in 1:length(sens_keys)) {
  key <- names(sens_keys)[[i]]
    
  summ <- readRDS(str_glue("./outputs/{key}_models_summ.rds")) %>% filter(as.character(test_data)=="NULL")
  summ_tt <- readRDS(str_glue("./outputs/{key}_models_summ.rds")) %>% filter(as.character(test_data)!="NULL")
  summ_nc <- readRDS(str_glue("./outputs/{key}_nc_models_summ_out.rds"))
  
  sens_dat <- vector("list", length(sens_keys[[key]]))
  
  for (j in 1:length(sens_dat)) {
    lbl <- sens_keys[[key]][[j]]
    cat(lbl)
    
    sens_dat[[j]] <- tibble(
      label = lbl, 
      .in = list(summ %>% filter(plot_label==lbl)),
      .tt = list(summ_tt %>% filter(plot_label==str_glue("{lbl}tt_"))), 
      .nc = list(summ_nc %>% filter(plot_label==lbl))
    )
  }
  comb_sens[[i]] <- bind_rows(sens_dat) %>% mutate(dataset=key) %>% select(dataset, everything())
}

comb_sens <- bind_rows(comb_sens)
remove(sens_dat, summ, summ_tt, summ_nc)

## Format and clean up table
sens_table <- comb_sens %>% 
  select(dataset, label, .nc, .tt) %>% 
  mutate(.nc = map(.nc, \(x) x %>% select(in_AIC, in_BIC, ends_with("RMSE"), ends_with("spear"), ends_with("cover"), ends_with("AUC"))), 
         .tt = map(.tt, \(x) x %>% select(out_RMSE, out_spear, out_cover, out_AUC))) %>% 
  unnest_wider(.nc) %>% 
  unnest_wider(.tt, names_sep = "_") %>% 
  select(dataset, label, in_AIC, in_BIC, ends_with("RMSE"), ends_with("spear"), ends_with("cover"), ends_with("AUC")) %>% 
  mutate(across(in_AIC:in_BIC, \(x) round(x)), 
         across(in_RMSE:.tt_out_AUC, \(x) signif(x, 3))) %>% 
  rename(Dataset = dataset, 
         Model = label,
         `AIC` = in_AIC, 
         `BIC` = in_BIC,
         `RMSE, in-sample` = in_RMSE, 
         `RMSE, nowcast` = out_RMSE,
         `RMSE, inflection` = inf_RMSE, 
         `RMSE, fixed test` = .tt_out_RMSE, 
         `Rho, in-sample` = in_spear, 
         `Rho, nowcast` = out_spear,
         `Rho, inflection` = inf_spear, 
         `Rho, fixed test` = .tt_out_spear, 
         `Coverage, in-sample` = in_cover, 
         `Coverage, nowcast` = out_cover,
         `Coverage, inflection` = inf_cover, 
         `Coverage, fixed test` = .tt_out_cover, 
         `AUC, in-sample` = in_AUC, 
         `AUC, nowcast` = out_AUC, 
         `AUC, inflection` = inf_AUC, 
         `AUC, fixed test` = .tt_out_AUC) %>% 
  mutate(Model = c("Base model", "Outpatient only", "Base model", "Symptom stratified", "Asymptomatic only", "Immunologically naive only"))

sens_table %>% save_bak(str_glue("./outputs/Comb_sens_table.rds"))
sens_table %>% save_bak(str_glue("./outputs/Comb_sens_table.csv"))









#===============================================================================
# CREATE COMBINED FIGURES
#===============================================================================


## Combined TT fit-to-data figure for MGB and LAC
p_MGB_tt <- readRDS("./figures/MGB_fit_tt.rds")
p_LAC_tt <- readRDS("./figures/LAC_fit_tt.rds")
p_MGB_tt / p_LAC_tt

save_bak(p_MGB_tt / p_LAC_tt,
         str_glue("./figures/Comb_fits_tt.{figext}"), 
         width=18, height=7, units="in", dpi=300)

## Combined Ct-GR relationships scatterplot for MGB and LAC
p_MGB_ct_gr <- readRDS("./figures/MGB_ct_gr.rds") + theme(axis.title.x = element_blank(), legend.position="None") + ggtitle("MGB")
p_LAC_ct_gr <- readRDS("./figures/LAC_ct_gr.rds") + ggtitle("LAC")
p_MGB_ct_gr / p_LAC_ct_gr

save_bak(p_MGB_ct_gr / p_LAC_ct_gr,
         str_glue("./figures/Comb_ct_gr.{figext}"), 
         width=10, height=10, units="in", dpi=300)




