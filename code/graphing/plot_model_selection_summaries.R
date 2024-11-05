
#===============================================================================
# READ AND PREP DATA
#===============================================================================

## Read in results data if not already generated
# if (process_nowcast == FALSE) {
#   nc_models_summ_avg <- readRDS(str_glue("./outputs/{key}_nc_models_summ_avg.rds"))
#   nc_models_summ_full <- readRDS(str_glue("./outputs/{key}_nc_models_summ_full.rds"))
#   nc_models_summ_out <- readRDS(str_glue("./outputs/{key}_nc_models_summ_out.rds"))
#   nc_models_summ_out_clean <- readRDS(str_glue("./outputs/{key}_nc_models_summ_out_clean.rds"))
# }

## Read in results summaries
summ <- readRDS(str_glue("./outputs/{key}_models_summ.rds"))
summ_tt <- summ %>% filter(test_data!="NULL")
summ <- summ %>% filter(test_data=="NULL")
summ_nc <- readRDS(str_glue("./outputs/{key}_nc_models_summ_out.rds"))

#===============================================================================
# CREATE PAPER SUMMARY TABLES
#===============================================================================

## Create concise summary table for main paper
comb_table_main <- summ_nc %>% 
  select(model, in_AIC, ends_with("RMSE"), ends_with("AUC")) %>% 
  mutate(spline = rep(c("None", "Cubic"), each=12), 
         model = rep(c("Mean (no variant era)", 
                       "Mean + variant era", 
                       "Mean * variant era", 
                       "Mean + st.dev. (no variant era)", 
                       "Mean + st.dev. + variant era", 
                       "(Mean + st.dev.) * variant era", 
                       "Mean + skew (no variant era)", 
                       "Mean + skew + variant era", 
                       "(Mean + skew) * variant era", 
                       "Mean + s.d. + skew (no variant era)", 
                       "Mean + s.d. + skew + variant era", 
                       "(Mean + s.d. + skew) * variant era"), 
                     2)) %>% 
  mutate(in_AIC = round(in_AIC), 
         across(in_RMSE:inf_AUC, \(x) signif(x, 3))) %>% 
  select(spline, model, everything()) %>% 
  rename(Spline = spline, 
         Model = model,
         `AIC` = in_AIC, 
         `RMSE, in-sample` = in_RMSE, 
         `RMSE, nowcast` = out_RMSE,
         `RMSE, inflection` = inf_RMSE, 
         `AUC, in-sample` = in_AUC, 
         `AUC, nowcast` = out_AUC, 
         `AUC, inflection` = inf_AUC)

## Create detailed summary table for supplement
comb_table_full <- summ_nc %>% 
  select(model, in_AIC, in_BIC, ends_with("RMSE"), ends_with("spear"), ends_with("cover"), ends_with("AUC")) %>% 
  bind_cols(summ_tt %>% select(out_RMSE, out_spear, out_cover, out_AUC) %>% setNames(paste0('.tt_', names(.)))) %>% 
  mutate(spline = rep(c("None", "Cubic"), each=12), 
         model = rep(c("Mean (no variant era)", 
                       "Mean + variant era", 
                       "Mean * variant era", 
                       "Mean + st.dev. (no variant era)", 
                       "Mean + st.dev. + variant era", 
                       "(Mean + st.dev.) * variant era", 
                       "Mean + skew (no variant era)", 
                       "Mean + skew + variant era", 
                       "(Mean + skew) * variant era", 
                       "Mean + s.d. + skew (no variant era)", 
                       "Mean + s.d. + skew + variant era", 
                       "(Mean + s.d. + skew) * variant era"), 
                     2)) %>% 
  select(spline, model, in_AIC, in_BIC, ends_with("RMSE"), ends_with("spear"), ends_with("cover"), ends_with("AUC")) %>% 
  mutate(across(in_AIC:in_BIC, \(x) round(x)), 
         across(in_RMSE:.tt_out_AUC, \(x) signif(x, 3))) %>% 
  rename(Spline = spline, 
         Model = model,
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


comb_table_main %>% save_bak(str_glue("./outputs/{key}_comb_perf_table.rds"))
comb_table_main %>% save_bak(str_glue("./outputs/{key}_comb_perf_table.csv"))

comb_table_full %>% save_bak(str_glue("./outputs/{key}_comb_perf_table_full.rds"))
comb_table_full %>% save_bak(str_glue("./outputs/{key}_comb_perf_table_full.csv"))




#===============================================================================
# EXTRACT RMSES AND STANDARDISE METRICS
#===============================================================================

## Function to standardise performance metric to 0-1 range, higher is better
standardise_metric <- function(metric, reverse=FALSE) {
  std_metric <- ({{ metric }} - min({{ metric }})) / (max({{ metric }}) - min({{ metric }}))
  if (isTRUE(reverse)) std_metric <- 1 - std_metric
  
  return(std_metric)
}

## Create Spearman rank correlation matrix of model performance metrics
nc_models_perf_corrs_daily <- nc_models_summ_out_clean %>%
  select(-(mdl:model_data)) %>%
  corrr::colpair_map(spear_func)

print(nc_models_perf_corrs_daily)
## Note: AICc and AIC, nRMSE and RMSE, and R2 and RMSE are each perfectly correlated
## So drop AICc, nRMSE, and R2 results for clarity
## Also drop coverage results for now as intervals not meaningful

## Classify models by type
model_comparison <- nc_models_summ_out_clean %>% 
  select(-c("in_AICc", "in_nRMSEq", "in_nRMSEd", "in_R2", "out_R2", "inf_R2"), 
         -(in_cover:inf_cover), -(group_vars:direct_var), -model_data) %>% 
  mutate(mdl_type = factor(case_when(str_detect(model, "_sd_") & str_detect(model, "_skew_") ~ "mean_both",
                                     str_detect(model, "_sd_") ~ "mean_stdev", 
                                     str_detect(model, "_skew_") ~ "mean_skew", 
                                     TRUE ~ "mean_only")),
         variant_int = factor(case_when(str_detect(model, "\\+ var_era") ~ "additive",
                                        str_detect(model, "\\* var_era") ~ "interaction",
                                        str_detect(model, "by=var_era") ~ "interaction",
                                        TRUE ~ "none")),
         spline_type = factor(case_when(str_detect(model, "bs='cs'") ~ "cubic",
                                        # str_detect(model, "bs='ts'") ~ "thin_plate",
                                        TRUE ~ "no_spline"))) %>%
  mutate(mdl_type = fct_relevel(mdl_type, c("mean_only", "mean_stdev", "mean_skew", "mean_both")), 
         variant_int = fct_relevel(variant_int, c("none", "additive", "interaction")), 
         spline_type = fct_relevel(spline_type, c("no_spline","cubic")))

## Extract only RMSE values
RMSE_only_raw <- model_comparison %>% 
  select(mdl:model, mdl_type:spline_type, in_RMSE, out_RMSE, inf_RMSE) %>% 
  pivot_longer(in_RMSE:inf_RMSE, names_to="metric")

## Standardise across performance metrics
metrics_std <- model_comparison %>%  
  mutate(across(c(in_AIC, in_BIC, in_RMSE, out_RMSE, inf_RMSE), 
                \(x) standardise_metric(x, reverse=TRUE)),  # Lower is better for these metrics
         across(c(in_spear, out_spear), standardise_metric), 
         across(inf_spear:inf_AUC, standardise_metric)) %>% 
  mutate(var_sp = interaction(variant_int, spline_type, sep="_")) %>% 
  pivot_longer(in_AIC:inf_AUC, names_to="metric") %>% 
  select(mdl:model, mdl_type:var_sp, metric, value)
  # select(mdl:model, mdl_type:spline_type, metric, value)


#===============================================================================
# GRAPH RMSEs AND STANDARDISED PERFORMANCE METRICS
#===============================================================================

## Create dotplot of out-of-sample RMSE and AUC
p_metrics_comp_main <- model_comparison %>% 
  select(mdl:model, mdl_type:spline_type, in_AIC, out_RMSE, out_AUC) %>% 
  pivot_longer(in_AIC:out_AUC, names_to="metric") %>% 
  mutate(metric = fct_recode(as_factor(metric), AIC="in_AIC", RMSE="out_RMSE", AUC="out_AUC"), 
         mdl_type = fct_recode(as_factor(mdl_type), `Mean only`="mean_only", `Mean + St.Dev.`="mean_stdev", `Mean + Skew`="mean_skew", `Mean + St.Dev. + Skew`="mean_both")) %>% 
  ggplot() +
  geom_point(aes(x=value, y=mdl_type, col=spline_type, shape=variant_int), size=2) +
  scale_y_discrete(limits=rev) +
  theme_bw() +
  theme(axis.title = element_blank(), 
        legend.position="bottom", 
        legend.direction="vertical") +
  scale_colour_manual(name="Spline type", values=c("red", "blue"), labels=c("No spline", "Cubic")) +
  scale_shape_manual(name="Variant interation", values=c(16, 17, 15), labels=c("No interaction", "Additive term", "Full interaction")) +
  facet_wrap(vars(metric), scales="free_x")

save_bak(p_metrics_comp_main, 
         str_glue("./figures/{key}_metrics_dotplot_main.{figext}"), 
         width=9, height=6, units="in", dpi=300)


## Create dotplot of standardised performance metrics
p_metrics_comp <- ggplot(metrics_std) +
  geom_point(aes(x=value, y=metric, col=mdl_type, shape=var_sp), size=3) +
  theme_bw() +
  # geom_point(aes(x=value, y=metric, col=variant_int), size=3)
  scale_shape_manual(values=c(7, 10, 9, 15, 16, 18))

save_bak(p_metrics_comp, 
         str_glue("./figures/{key}_metrics_dotplot.{figext}"), 
         width=8, height=10, units="in", dpi=300)


## Create RMSE dotplots
p_r1 <- ggplot(RMSE_only_raw) +
  # geom_point(aes(x=value, y=metric, col=mdl_type, shape=var_sp), size=3) +
  geom_point(aes(x=value, y=metric, col=mdl_type), size=2, alpha=0.5)
# scale_shape_manual(values=c(0, 1, 5, 7, 10, 9, 15, 16, 18))

p_r2 <- ggplot(RMSE_only_raw) +
  # geom_point(aes(x=value, y=metric, col=mdl_type, shape=var_sp), size=3) +
  geom_point(aes(x=value, y=metric, col=variant_int), size=2, alpha=0.5)

p_r3 <- ggplot(RMSE_only_raw) +
  # geom_point(aes(x=value, y=metric, col=mdl_type, shape=var_sp), size=3) +
  geom_point(aes(x=value, y=metric, col=spline_type), size=2, alpha=0.5)

save_bak(p_r1 / p_r2 / p_r3, 
         str_glue("./figures/{key}_RMSE_dotplot.{figext}"), 
         width=6, height=8, units="in", dpi=300)

