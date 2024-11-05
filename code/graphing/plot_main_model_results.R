
#===============================================================================
# READ AND PREP DATA
#===============================================================================

## Group data by day and week
grouping_vars <- c()
source("./code/auxiliary/ct_data_grouping.R")

## Read in results files
models <- readRDS(str_glue("{dat_folder}/{key}_models_fitted.rds"))
idx <- which(models$plot_label == main_mdl)  # Identify which model to use
nc_models <- readRDS(str_glue("{dat_folder}/{key}_nc_fitted_{idx}.rds"))

## Pull out predictions
case_dat <- comb_dat_daily %>% mutate(id = consecutive_id(var_era))
preds <- (models %>% filter(plot_label == main_mdl) %>% pull(pred_gr_preds_out))[[1]] %>% mutate(id = consecutive_id(var_era))
preds_tr <- (models %>% filter(plot_label == str_c(main_mdl, 'tt_')) %>% pull(pred_gr_preds_in))[[1]] %>% mutate(id = consecutive_id(var_era))
preds_tt <- (models %>% filter(plot_label == str_c(main_mdl, 'tt_')) %>% pull(pred_gr_preds_out))[[1]] %>% mutate(id = consecutive_id(var_era))
preds_nc <- comb_dat_daily %>% select(coll_date) %>% left_join(bind_rows(nc_models$pred_gr_preds_out)) %>% mutate(id = consecutive_id(var_era))

## Pull out fitted model
fit_tt <- (models %>% filter(plot_label == str_c(main_mdl, 'tt_')) %>% pull(pred_gr_fit))[[1]]

## Read in results summaries
summ <- readRDS(str_glue("./outputs/{key}_models_summ.rds"))
summ_tt <- summ %>% filter(test_data!="NULL")
summ <- summ %>% filter(test_data=="NULL")
summ_nc <- readRDS(str_glue("./outputs/{key}_nc_models_summ_out.rds"))

## Summarise data characteristics for each rolling nowcast index
nc_idx_dat <- nc_models %>% 
  select(idx, gr_var, model_data, test_data) %>% 
  mutate(model_dat = map(model_data, eval_mdl), 
         test_dat = map(test_data, eval_mdl), 
         sample = map(model_dat, "sm_N"), 
         sample_t = map(test_dat, "sm_N"),
         gr = map(model_dat, "gr_roll"), 
         gr_t = map(test_dat, "gr_roll"), 
         mean_ct = map(model_dat, "sm_mean_ct"), 
         skew_ct = map(model_dat, "sm_skew_ct"), 
         mean_ct_t = map(test_dat, "sm_mean_ct"), 
         skew_ct_t = map(test_dat, "sm_skew_ct"), 
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

#===============================================================================
# CREATE FIT-TO-DATA GRAPHS
#===============================================================================

p_fit_in <- plot_pred_gr(.data=preds, observed=case_dat) +
  theme(legend.position="none", axis.text.x=element_blank())
# %>% annotate_perf(preds, case_dat, (summ %>% slice(1)))

p_fit_tt <- plot_pred_gr(.data=preds_tr, observed=case_dat) +
# %>% annotate_perf(preds_tt, case_dat, (summ_tt %>% slice(1))) + 
  geom_ribbon(data=preds_tt, aes(x=coll_date, ymin=Q2.5, ymax=Q97.5, group=id), alpha=0.3) +
  geom_ribbon(data=preds_tt, aes(x=coll_date, ymin=Q2.5_ns, ymax=Q97.5_ns, group=id), alpha=0.15) +
  geom_line(data=preds_tt, aes(x=coll_date, y=Estimate, col="Predicted", group=id), linewidth=1) +
  geom_vline(xintercept=as_date(split_date), linewidth=2, linetype="longdash", alpha=0.5) +
  theme(legend.position="none", axis.text.x=element_blank())

p_fit_nc <- plot_pred_gr(.data=preds_nc, observed=case_dat) +
  geom_vline(xintercept=(min((preds_nc %>% drop_na())$coll_date)), linewidth=2, linetype="longdash", alpha=0.5) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.box="horizontal")
# %>% annotate_perf(preds_nc, case_dat, (summ_nc %>% slice(1)))


#===============================================================================
# CREATE MODEL FIT GRAPH
#===============================================================================

## Extract variant era coefficents & set base to 0
var_coefs <- setNames(coef(fit_tt)[1:length(broom.helpers::model_get_xlevels(fit_tt)$var_era)], broom.helpers::model_get_xlevels(fit_tt)$var_era)
var_coefs[[1]] <- 0

## Extract model variables
model_vars <- broom.helpers::model_list_variables(fit_tt)$variable

mdl_grid <- vector("list", length(model_vars)-2)

## Pull values for plotting from `plot.gam`
for (i in 1:(length(model_vars)-2)) {
  v <- model_vars[[i+2]]
  mdl_fit <- plot.gam(fit_tt, xlim=range(case_dat %>% select(str_sub(v, 1, 10)), na.rm=TRUE), pages=1)[[i]]
  
  ## Extract values from `plot.gam`
  mdl_plt <- tibble(name = str_sub(mdl_fit$xlab, 1, 10), 
                    value = mdl_fit$x, 
                    estimate = mdl_fit$fit[,1], 
                    lower = mdl_fit$fit[,1] - mdl_fit$se, 
                    upper = mdl_fit$fit[,1] + mdl_fit$se)
  
  for (j in 1:length(var_coefs)) {
    mdl_grid[[i]] <- bind_rows(mdl_grid[[i]], 
                               mdl_plt %>% mutate(estimate = estimate + var_coefs[[j]], 
                                                  lower = lower + var_coefs[[j]],
                                                  upper = upper + var_coefs[[j]], 
                                                  var_era = names(var_coefs)[[j]]))
  }
}
mdl_grid <- bind_rows(mdl_grid)
mdl_grid <- bind_rows(mdl_grid %>% mutate(period = "Test"),  # Duplicate for train/test split
                      mdl_grid %>% mutate(period = "Training")) %>% 
  mutate(name = recode(name, "sm_mean_ct" = "Mean", "sm_skew_ct" = "Skewness"), 
         period=fct_relevel(as_factor(period), c("Training", "Test")), 
         var_era = fct_expand(var_era, c("WT", "Alpha", "Delta", "Omicron")))

## Modify observed data for plotting
observed <- case_dat %>% 
  select(coll_date, gr_roll, sm_mean_ct, sm_skew_ct, var_era) %>% 
  pivot_longer(sm_mean_ct:sm_skew_ct) %>% 
  mutate(name = recode(name, "sm_mean_ct" = "Mean", "sm_skew_ct" = "Skewness"), 
         period = case_when(coll_date < split_date ~ "Training", 
                            coll_date >= split_date ~ "Test"),
         period=fct_relevel(as_factor(period), c("Training", "Test"))) 


## Plot model fit lines against Ct predictors
p_mdl_preds <- mdl_grid %>% 
  ggplot() +
  geom_point(data=observed, aes(x=value,y=gr_roll,col=var_era), alpha=0.25) +
  geom_ribbon(aes(x=value, ymin=lower, ymax=upper, fill=var_era), alpha=0.25) +
  geom_line(aes(x=value, y=estimate, col=var_era)) +
  theme_main +
  facet_grid(rows=vars(period), cols=vars(name),scales="free") + 
  scale_y_continuous(limits=c(-0.25,0.25)) +
  geom_hline(yintercept=0,linetype="dashed") +
  xlab("Statistic (cycle threshold value)") +
  ylab("Incidence growth rate") +
  scale_colour_manual(name="Variant era", 
                      values=c("WT"="black", "Alpha"="orange","Omicron"="red","Delta"="blue"), 
                      breaks=c("WT", "Alpha", "Delta", "Omicron"), 
                      drop=FALSE) +
  scale_fill_manual(name="Variant era", 
                    values=c("WT"="black", "Alpha"="orange","Omicron"="red","Delta"="blue"), 
                    breaks=c("WT", "Alpha", "Delta", "Omicron"),
                    drop=FALSE) +
  theme(legend.position="bottom")
p_mdl_preds

save_bak(p_mdl_preds, 
         str_glue("./figures/{key}_model_preds.{figext}"), 
         width=10, height=9, units="in", dpi=300)

#===============================================================================
# CREATE BY INDEX GRAPHS
#===============================================================================

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
  geom_segment(aes(x=nc_date, xend=nc_date+nowcast_window, y=out_RMSE, yend=out_RMSE), col="purple", linewidth=1.5) +
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

#===============================================================================
# SAVE COMBINED PLOTS & PLOT OBJECTS
#===============================================================================

# save_bak(p_fit_tt, str_glue("./figures/{key}_fit_tt.rds"))

save_bak((p_fit_nc + labs(tag="A")) / (p_metrics + labs(tag="B")),
         str_glue("./figures/{key}_nc_rmse.{figext}"), 
         width=14, height=6, units="in", dpi=300)

save_bak(p_fit_nc / p_ct_dist / p_metrics,
         str_glue("./figures/{key}_nc_idx.{figext}"), 
         width=18, height=10, units="in", dpi=300)

save_bak((p_fit_in + labs(tag="A")) / (p_fit_nc + labs(tag="B")) / (p_metrics + labs(tag="C")),
         str_glue("./figures/{key}_in_nc_rmse.{figext}"), 
         width=18, height=10, units="in", dpi=300)

save_bak((p_fit_in + labs(tag="A")) / (p_fit_tt + labs(tag="B")) / (p_fit_nc + labs(tag="C")),
         str_glue("./figures/{key}_fits.{figext}"), 
         width=14, height=8, units="in", dpi=300)

#===============================================================================
# EXTEND SUPPLEMENTARY DATA CHARACTERISTICS TABLE
#===============================================================================

dat_summ <- readRDS(str_glue("./outputs/{key}_data_summary.rds"))

dat_summ <- c(dat_summ, 
              list(nc_days = nrow(preds_nc %>% drop_na()), 
                   nc_inf = sum((preds_nc %>% drop_na() %>% left_join(comb_dat_daily %>% select(coll_date, inflection)))$inflection), 
                   nc_inf_perc = mean((preds_nc %>% drop_na() %>% left_join(comb_dat_daily %>% select(coll_date, inflection)))$inflection)))

save_bak(dat_summ, str_glue("./outputs/{key}_data_summary.rds"))
