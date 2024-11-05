
#===============================================================================
# READ AND PREP DATA
#===============================================================================

## Group data by day and week
grouping_vars <- c("set")
source("./code/auxiliary/ct_data_grouping.R")

## Read in results files
models <- readRDS(str_glue("{dat_folder}/{key}_models_fitted.rds"))

## Pull out predictions
case_dat <- comb_dat_daily %>% mutate(id = consecutive_id(var_era))

## Read in results summaries
summ <- readRDS(str_glue("./outputs/{key}_models_summ.rds"))
summ_tt <- summ %>% filter(test_data!="NULL")
summ <- summ %>% filter(test_data=="NULL")
summ_nc <- readRDS(str_glue("./outputs/{key}_nc_models_summ_out.rds"))

#===============================================================================
# CREATE PAPER SUMMARY TABLES
#===============================================================================

## Clean summary table for main paper
comb_perf_table <- summ_nc %>% 
  select(model, in_AIC, in_RMSE, out_RMSE, inf_RMSE, in_AUC, out_AUC, inf_AUC) %>% 
  mutate(model = rep(c("Ideal condition", 
                       "Realistic kinetics", 
                       "Realistic variation", 
                       "Realistic sampling", 
                       "Baseline condition", 
                       "High variation", 
                       "Symmetric kinetics", 
                       "Baseline, asymptomatic only", 
                       "Realistic sampling, asymptomatic"), 2)) %>%
  mutate(across(in_RMSE:inf_AUC, \(x) signif(x, 3)), 
         in_AIC = round(in_AIC)) %>% 
  rename(Model = model, 
         `AIC` = in_AIC, 
         `RMSE, in-sample` = in_RMSE, 
         `RMSE, nowcast` = out_RMSE,
         `RMSE, inflection` = inf_RMSE, 
         `AUC, in-sample` = in_AUC, 
         `AUC, nowcast` = out_AUC, 
         `AUC, inflection` = inf_AUC)

comb_perf_table %>% save_bak(str_glue("./outputs/{key}_comb_perf_table.rds"))
comb_perf_table %>% save_bak(str_glue("./outputs/{key}_comb_perf_table.csv"))

#===============================================================================
# CREATE & SAVE GRAPHS FOR EACH SIMULATED DATASET
#===============================================================================

sets <- c("idel", "rknt", "rvar", "rsam", "base", "hvar", "sknt")
# , "abse", "arsm")

# for (i in 1:2) {
for (i in 1:length(sets)) {
  set <- sets[[i]]
  cat(i, set, "\n")
  i <- i+9  # Switch to correct (cubic spline) model
  
  ## Read in correct model output and pull predictions
  nc_models <- readRDS(str_glue("{dat_folder}/{key}_nc_fitted_{i}.rds"))
  
  preds <- models[[i, 'pred_gr_preds_out']][[1]] %>% mutate(id = consecutive_id(var_era))
  preds_tr <- (models %>% filter(test_data != "NULL"))[[i, 'pred_gr_preds_in']][[1]] %>% mutate(id = consecutive_id(var_era))
  preds_tt <- (models %>% filter(test_data != "NULL"))[[i, 'pred_gr_preds_out']][[1]] %>% mutate(id = consecutive_id(var_era))
  preds_nc <- comb_dat_daily %>% select(coll_date) %>% left_join(bind_rows(nc_models$pred_gr_preds_out)) %>% mutate(id = consecutive_id(var_era))
  
  observed <- case_dat %>% filter(set==!!set)
  
  # ## Pull out fitted model
  # fit_tt <- (models %>% slice(i+18) %>% pull(pred_gr_fit))[[1]]
  
  ## Summarise data characteristics for each rolling nowcast index
  nc_idx_dat <- nc_models %>% 
    select(idx, gr_var, model_data, test_data) %>% 
    mutate(model_dat = map(model_data, eval_mdl), 
           test_dat = map(test_data, eval_mdl), 
           sample = map(model_dat, str_glue("sm_N_{set}")),
           sample_t = map(test_dat, str_glue("sm_N_{set}")),
           gr = map(model_dat, str_glue("sm_gr_{set}")),
           gr_t = map(test_dat, str_glue("sm_gr_{set}")),
           mean_ct = map(model_dat, str_glue("sm_mean_ct_{set}")),
           skew_ct = map(model_dat, str_glue("sm_skew_ct_{set}")),
           mean_ct_t = map(test_dat, str_glue("sm_mean_ct_{set}")),
           skew_ct_t = map(test_dat, str_glue("sm_skew_ct_{set}")),
           avg_sample = map_dbl(sample, mean, na.rm=TRUE), 
           avg_sample_t = map_dbl(sample_t, mean, na.rm=TRUE), 
           gr_sd = map_dbl(gr, sd, na.rm=TRUE), 
           gr_sd_t = map_dbl(gr_t, sd, na.rm=TRUE)) %>% 
    mutate(spear_mean = map2_dbl(gr, mean_ct, spear_func), 
           spear_skew = map2_dbl(gr, skew_ct, spear_func), 
           spear_mean_t = map2_dbl(gr_t, mean_ct_t, spear_func), 
           spear_skew_t = map2_dbl(gr_t, skew_ct_t, spear_func)) %>% 
    select(idx, gr:spear_skew_t) %>% 
    left_join(nc_models %>%  # Add standardised model performance metrics by index
                select(1:3) %>% 
                left_join(summarise_multi_models(nc_models)) %>% 
                mutate(nc_date = str_sub(model_data, -12, -3),
                       nc_date = as_date(nc_date) + 7) %>% 
                select(idx, nc_date, in_AIC:out_AUC))
  
  ## Create fit-to-data plots
  p_fit_in <- plot_pred_gr(.data=preds, observed=observed) +
    theme(legend.position="none", axis.text.x=element_blank())
  # %>% annotate_perf(preds, case_dat, (summ %>% slice(i)))
  
  p_fit_tt <- plot_pred_gr(.data=preds_tr, observed=observed) +
  # %>% annotate_perf(preds_tt, case_dat, (summ_tt %>% slice(i))) + 
    geom_line(data=preds_tt, aes(x=coll_date, y=Estimate, col="Predicted", group=id), linewidth=1) +
    geom_ribbon(data=preds_tt, aes(x=coll_date, ymin=Q2.5, ymax=Q97.5), alpha=0.3) +
    geom_ribbon(data=preds_tt, aes(x=coll_date, ymin=Q2.5_ns, ymax=Q97.5_ns), alpha=0.15) +
    geom_vline(xintercept=as_date(split_date), linewidth=2, linetype="longdash", alpha=0.5) +
    theme(legend.position="none", axis.text.x=element_blank())
  
  p_fit_nc <- plot_pred_gr(.data=preds_nc, observed=observed)%>% 
    annotate_perf(preds_nc, case_dat, (summ_nc %>% slice(i))) +
    geom_vline(xintercept=(min((preds_nc %>% drop_na())$coll_date)), linewidth=2, linetype="longdash", alpha=0.5) +
    theme(legend.position="bottom", legend.direction="horizontal", legend.box="horizontal") 
  
  
  
  # ## Create dummy grid
  # grid_dat <- expand_grid(sm_mean_ct = seq(min(case_dat$sm_mean_ct, na.rm=TRUE), max(case_dat$sm_mean_ct, na.rm=TRUE), by=0.1), 
  #                         sm_skew_ct = seq(min(case_dat$sm_skew_ct, na.rm=TRUE), max(case_dat$sm_skew_ct, na.rm=TRUE),by=0.01),
  #                         # sm_sd_ct = seq(min(case_dat$sm_sd_ct, na.rm=TRUE), max(case_dat$sm_sd_ct, na.rm=TRUE),by=0.01),
  #                         var_era = (broom.helpers::model_get_xlevels(fit_tt))$var_era) %>%   # Assign only variant era levels present in training model
  #   setNames(c(str_c("sm_mean_ct_", set), str_c("sm_skew_ct_", set), "var_era"))
  # grid_dat$pred_gr <- predict(fit_tt, newdata=grid_dat)  # Add model predictions
  # 
  # 
  # grid_dat_summ_mean <- grid_dat %>% group_by(!!rlang::sym(str_c("sm_mean_ct_", set)), var_era) %>% summarise(lower = quantile(pred_gr, 0.025), median = median(pred_gr), upper = quantile(pred_gr, 0.975)) %>% 
  #   # mutate(name = "Mean") %>% 
  #   rename(value=str_c("sm_mean_ct_", set))
  # grid_dat_summ_skew <- grid_dat %>% group_by(!!rlang::sym(str_c("sm_skew_ct_", set)), var_era) %>% summarise(lower = quantile(pred_gr, 0.025), median = median(pred_gr), upper = quantile(pred_gr, 0.975)) %>% 
  #   # mutate(name = "Skewness") %>% 
  #   rename(value=str_c("sm_skew_ct_", set))
  # 
  # grid_dat_summ <- bind_rows(grid_dat_summ_mean, grid_dat_summ_skew)
  # 
  # ## Plot model fit lines against Ct predictors
  # p_mdl_preds <- grid_dat_summ %>% 
  #   mutate(var_era = as_factor(var_era),
  #          var_era = fct_expand(var_era, c("WT", "Alpha", "Delta", "Omicron")), 
  #          period = "Training") %>%  # Re-add var_era levels for scales
  #   bind_rows(grid_dat_summ %>% 
  #               mutate(var_era = as_factor(var_era),
  #                      var_era = fct_expand(var_era, c("WT", "Alpha", "Delta", "Omicron")), 
  #                      period = "Test")) %>% 
  #   mutate(period=fct_relevel(as_factor(period), c("Training", "Test"))) %>% 
  #   ggplot() +
  #   # geom_point(aes(x=value,y=gr_roll,col=var_era), alpha=0.25) +
  #   geom_point(data=preds_nc %>% 
  #                pivot_longer(!!rlang::sym(str_c("sm_mean_ct_", set)):!!rlang::sym(str_c("sm_skew_ct_", set))) %>% 
  #                mutate(
  #                  # name = recode(name, "sm_mean_ct" = "Mean", "sm_skew_ct" = "Skewness"), 
  #                       period = case_when(coll_date < split_date ~ "Training", 
  #                                          coll_date >= split_date ~ "Test"),
  #                       period=fct_relevel(as_factor(period), c("Training", "Test"))), 
  #              aes(x=value,y=!!rlang::sym(str_c("gr_roll_", set)),col=var_era), alpha=0.25) +
  #   geom_ribbon(aes(x=value, ymin=lower, ymax=upper, fill=var_era), alpha=0.25) +
  #   geom_line(aes(x=value, y=median, col=var_era)) +
  #   theme_main +
  #   facet_grid(rows=vars(period), cols=vars(name), scales="free") + 
  #   scale_y_continuous(limits=c(-0.25,0.25)) +
  #   geom_hline(yintercept=0,linetype="dashed") +
  #   xlab("Statistic (cycle threshold value)") +
  #   ylab("Incidence growth rate") +
  #   scale_colour_manual(name="Variant era", 
  #                       values=c("WT"="black", "Alpha"="orange","Omicron"="red","Delta"="blue"), 
  #                       breaks=c("WT", "Alpha", "Delta", "Omicron"), 
  #                       drop=FALSE) +
  #   scale_fill_manual(name="Variant era", 
  #                     values=c("WT"="black", "Alpha"="orange","Omicron"="red","Delta"="blue"), 
  #                     breaks=c("WT", "Alpha", "Delta", "Omicron"),
  #                     drop=FALSE) +
  #   theme(legend.position="bottom")
  # p_mdl_preds
  # 
  # save_bak(p_mdl_preds, 
  #          str_glue("./figures/{key}_model_preds_{set}.{figext}"), 
  #          width=10, height=9, units="in", dpi=300)
  
  ## Plot Ct distribution by index for training and test data
  p_ct_dist <- nc_idx_dat %>% 
    select(idx, nc_date, spear_mean, spear_skew, spear_mean_t, spear_skew_t) %>% 
    pivot_longer(spear_mean:spear_skew_t) %>% 
    ggplot() +
    geom_point(aes(x=nc_date, y=value, shape=name, col=name), size=3) +
    geom_hline(aes(yintercept=0), linetype="dashed", alpha=0.5) + 
    # scale_y_continuous(limits=c(-0.6, 0.6)) + 
    scale_x_date(limits=c(min(case_dat$coll_date), max(case_dat$coll_date))) +
    scale_shape_manual(labels=c("Mean, training", "Mean, testing", "Skewness, training", "Skewness, testing"),
                       values=c(0, 15, 2, 17)) +
    scale_colour_manual(labels=c("Mean, training", "Mean, testing", "Skewness, training", "Skewness, testing"),
                        values=c("red", "blue", "red", "blue")) +
    theme_main +
    theme(axis.title.x=element_blank(), legend.position="right") +
    labs(shape = "Ct dist. feature", colour = "Ct dist. feature") +
    ylab("Spearman's Rho")
  
  ## Plot performance metrics for training and testing period by index
  p_metrics <- nc_idx_dat %>% 
    select(idx, nc_date, in_RMSE, out_RMSE) %>% 
    # pivot_longer(in_RMSE:out_RMSE) %>%
    ggplot() + 
    geom_segment(aes(x=nc_date, xend=lead(nc_date), y=out_RMSE, yend=out_RMSE), col="purple", linewidth=1.5) +
    # geom_point(aes(x=nc_date, y=out_RMSE), col="purple", linewidth=1.5) +
    # geom_line(aes(x=nc_date, y=out_RMSE), col="purple", alpha=0.3, linewidth=0.5) +
    # geom_line(aes(x=nc_date, y=value, col=name), linewidth=1.5) +
    scale_x_date(breaks=as_date(c("2020-07-01", "2021-01-01", "2021-07-01", "2022-01-01", "2022-07-01", "2023-01-01")), 
                 labels=c("Jul 2020", "Jan 2021", "Jul 2021", "Jan 2022", "Jul 2022", "Jan 2023"), 
                 limits=c(min(case_dat$coll_date), max(case_dat$coll_date))) +
    # scale_y_continuous(trans="reverse") +
    # scale_colour_manual(labels=c("Training", "Testing"),
    #                     values=c("blue", "red")) +
    theme_main +
    theme(axis.title.x=element_blank()) +
    # labs(colour = "RMSE") + 
    ylab("RMSE")
  
  
  save_bak(p_fit_nc / p_metrics,
           str_glue("./figures/{key}_nc_rmse_{set}.{figext}"), 
           width=14, height=6, units="in", dpi=300)
  
  # save_bak(p_fit_nc / p_ct_dist / p_metrics,
  #          str_glue("./figures/{key}_nc_idx_{set}.{figext}"), 
  #          width=18, height=10, units="in", dpi=300)
  # 
  # save_bak(p_fit_in / p_fit_nc / p_metrics,
  #          str_glue("./figures/{key}_in_nc_rmse_{set}.{figext}"), 
  #          width=18, height=10, units="in", dpi=300)
  
  save_bak(p_fit_in / p_fit_tt / p_fit_nc,
           str_glue("./figures/{key}_fits_{set}.{figext}"), 
           width=18, height=10, units="in", dpi=300)
}

#===============================================================================
# CREATE COMBINED SIMDATA FIT-TO-DATA PLOT
#===============================================================================

SIM_mega_plot_data <- vector("list", 5)
SIM_mega_plot_obs <- vector("list", 5)
SIM_mega_plot_tr <- vector("list", 5)
SIM_mega_plot_tt <- vector("list", 5)

for (i in 1:5) {
  set <- sets[[i]]
  cat(i, set, "\n")
  
  ## Read in correct model output and pull predictions
  nc_models <- readRDS(str_glue("{dat_folder}/{key}_nc_fitted_{i+9}.rds"))
  
  preds <- models[[i+9, 'pred_gr_preds_out']][[1]] %>% mutate(id = consecutive_id(var_era))
  preds_nc <- comb_dat_daily %>% select(coll_date) %>% left_join(bind_rows(nc_models$pred_gr_preds_out)) %>% mutate(id = consecutive_id(var_era))
  preds_tr <- (models %>% filter(test_data != "NULL"))[[i+9, 'pred_gr_preds_in']][[1]] %>% mutate(id = consecutive_id(var_era))
  preds_tt <- (models %>% filter(test_data != "NULL"))[[i+9, 'pred_gr_preds_out']][[1]] %>% mutate(id = consecutive_id(var_era))
  
  SIM_mega_plot_data[[i]] <- preds %>% 
    select(coll_date, var_era, id, Estimate:Q97.5_ns) %>% 
    mutate(type = "In-sample") %>% 
    bind_rows(preds_nc %>% 
                select(coll_date, var_era, id, Estimate:Q97.5_ns) %>% 
                mutate(type = "Nowcast")) %>% 
    mutate(set = !!set)
  
  SIM_mega_plot_obs[[i]] <- case_dat %>% 
    filter(set==!!set) %>% 
    mutate(type = "In-sample") %>% 
    bind_rows(case_dat %>% 
                filter(set==!!set) %>% 
                mutate(type = "Nowcast"))
  
  SIM_mega_plot_tr[[i]] <- preds_tr %>% 
    select(coll_date, var_era, id, Estimate:Q97.5_ns) %>% 
    mutate(set = !!set)
  
  SIM_mega_plot_tt[[i]] <- preds_tt %>% 
    select(coll_date, var_era, id, Estimate:Q97.5_ns) %>% 
    mutate(set = !!set)
}

SIM_mega_plot_data <- bind_rows(SIM_mega_plot_data) %>% 
  mutate(set = as_factor(set), 
         set = fct_recode(set, "Ideal condition" = "idel", "Realistic kinetics" = "rknt", "Realistic variation" = "rvar", "Realistic sampling" = "rsam", "Baseline condition" = "base", ))
SIM_mega_plot_obs <- bind_rows(SIM_mega_plot_obs) %>% 
  mutate(set = as_factor(set), 
         set = fct_recode(set, "Ideal condition" = "idel", "Realistic kinetics" = "rknt", "Realistic variation" = "rvar", "Realistic sampling" = "rsam", "Baseline condition" = "base", ))
SIM_mega_plot_tr <- bind_rows(SIM_mega_plot_tr) %>% 
  mutate(set = as_factor(set), 
         set = fct_recode(set, "Ideal condition" = "idel", "Realistic kinetics" = "rknt", "Realistic variation" = "rvar", "Realistic sampling" = "rsam", "Baseline condition" = "base", ))
SIM_mega_plot_tt <- bind_rows(SIM_mega_plot_tt) %>% 
  mutate(set = as_factor(set), 
         set = fct_recode(set, "Ideal condition" = "idel", "Realistic kinetics" = "rknt", "Realistic variation" = "rvar", "Realistic sampling" = "rsam", "Baseline condition" = "base", ))


p_SIM_mega_in <- plot_pred_gr(.data=SIM_mega_plot_data %>% filter(type == "In-sample"), observed=SIM_mega_plot_obs %>% filter(type == "In-sample"), inf=FALSE) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.box="horizontal") +
  facet_wrap(vars(set), ncol=1)
p_SIM_mega_in

p_SIM_mega_tt <- plot_pred_gr(.data=SIM_mega_plot_tr, observed=SIM_mega_plot_obs %>% filter(type == "In-sample"), inf=FALSE) +
  geom_line(data=SIM_mega_plot_tt, aes(x=coll_date, y=Estimate, col="Predicted", group=id), linewidth=1) +
  geom_ribbon(data=SIM_mega_plot_tt, aes(x=coll_date, ymin=Q2.5, ymax=Q97.5), alpha=0.3) +
  geom_ribbon(data=SIM_mega_plot_tt, aes(x=coll_date, ymin=Q2.5_ns, ymax=Q97.5_ns), alpha=0.15) +
  geom_vline(xintercept=as_date(split_date), linewidth=2, linetype="longdash", alpha=0.5) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.box="horizontal") +
  facet_wrap(vars(set), ncol=1)
p_SIM_mega_tt

p_SIM_mega_nc <- plot_pred_gr(.data=SIM_mega_plot_data %>% filter(type == "Nowcast"), observed=SIM_mega_plot_obs %>% filter(type == "Nowcast"), inf=FALSE) +
  geom_vline(xintercept=(min((SIM_mega_plot_data %>% filter(type=="Nowcast") %>% drop_na())$coll_date)), linewidth=2, linetype="longdash", alpha=0.5) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.box="horizontal") +
  facet_wrap(vars(set), ncol=1)
p_SIM_mega_nc

# save_bak(p_SIM_mega, 
#          str_glue("./figures/{key}_fits_combined.{figext}"), 
#          width=10, height=12, units="in", dpi=300)
# 
# save_bak(p_SIM_mega, 
#          str_glue("./figures/{key}_fits_combined_wide.{figext}"), 
#          width=16, height=10, units="in", dpi=300)

save_bak(p_SIM_mega_in, 
         str_glue("./figures/{key}_fits_combined_in.{figext}"), 
         width=8, height=10, units="in", dpi=300)

save_bak(p_SIM_mega_tt, 
         str_glue("./figures/{key}_fits_combined_tt.{figext}"), 
         width=8, height=10, units="in", dpi=300)

save_bak(p_SIM_mega_nc, 
         str_glue("./figures/{key}_fits_combined_nc.{figext}"), 
         width=8, height=10, units="in", dpi=300)




# 
# 
# ## Create dummy grid
# case_dat <- case_dat %>% filter(set == "base")
# 
# grid_dat <- expand_grid(sm_mean_ct_base = seq(min(case_dat$sm_mean_ct, na.rm=TRUE), max(case_dat$sm_mean_ct, na.rm=TRUE), by=0.1), 
#                         sm_skew_ct_base = seq(min(case_dat$sm_skew_ct, na.rm=TRUE), max(case_dat$sm_skew_ct, na.rm=TRUE),by=0.01),
#                         # sm_sd_ct = seq(min(case_dat$sm_sd_ct, na.rm=TRUE), max(case_dat$sm_sd_ct, na.rm=TRUE),by=0.01),
#                         var_era = c("WT", "Alpha", "Delta", "Omicron"))
# grid_dat$pred_gr <- predict(fit_tt, newdata=grid_dat)  # Add model predictions
# 
# 
# grid_dat_summ_mean <- grid_dat %>% group_by(sm_mean_ct_base, var_era) %>% summarise(lower = quantile(pred_gr, 0.025), median = median(pred_gr), upper = quantile(pred_gr, 0.975)) %>% mutate(name = "Mean") %>% rename(value=sm_mean_ct_base)
# grid_dat_summ_skew <- grid_dat %>% group_by(sm_skew_ct_base, var_era) %>% summarise(lower = quantile(pred_gr, 0.025), median = median(pred_gr), upper = quantile(pred_gr, 0.975)) %>% mutate(name = "Skewness") %>% rename(value=sm_skew_ct_base)
# 
# grid_dat_summ <- bind_rows(grid_dat_summ_mean, grid_dat_summ_skew)
# 
# 
# p_test <- grid_dat_summ %>% 
#   ggplot() +
#   # geom_point(aes(x=value,y=gr_roll,col=var_era), alpha=0.25) +
#   geom_point(data=preds_nc %>% 
#                pivot_longer(sm_mean_ct_base:sm_skew_ct_base) %>% 
#                mutate(name = recode(name, "sm_mean_ct_base" = "Mean", "sm_skew_ct_base" = "Skewness")), 
#              aes(x=value,y=gr_roll_base,col=var_era), alpha=0.01) +
#   geom_ribbon(aes(x=value, ymin=lower, ymax=upper, fill=var_era), alpha=0.25) +
#   geom_line(aes(x=value, y=median, col=var_era)) +
#   theme_main +
#   facet_wrap(~name,scales="free",ncol=4) + 
#   scale_y_continuous(limits=c(-0.25,0.25)) +
#   geom_hline(yintercept=0,linetype="dashed") +
#   xlab("Statistic (cycle threshold value)") +
#   ylab("Incidence growth rate") +
#   scale_colour_manual(name="Variant era", 
#                       values=c("WT"="black", "Alpha"="orange","Omicron"="red","Delta"="blue"), 
#                       breaks=c("WT", "Alpha", "Delta", "Omicron")) +
#   scale_fill_manual(name="Variant era", 
#                     values=c("WT"="black", "Alpha"="orange","Omicron"="red","Delta"="blue"), 
#                     breaks=c("WT", "Alpha", "Delta", "Omicron")) +
#   theme(legend.position="right")
# p_test
# 
