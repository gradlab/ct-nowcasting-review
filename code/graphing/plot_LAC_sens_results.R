
## Group data by day and week
grouping_vars <- c("symptomatic")
source("./code/auxiliary/ct_data_grouping.R")

## Read in results files
models <- readRDS(str_glue("{dat_folder}/{key}_models_fitted.rds"))
idx <- which(models$plot_label == "dmskw_sym_cr_")  # Identify which model to use
nc_models <- readRDS(str_glue("{dat_folder}/{key}_nc_fitted_{idx}.rds"))

## Pull out observed data
case_dat <- comb_dat_daily %>% mutate(id = consecutive_id(var_era))

## Pull out fitted model
fit_tt <- (models %>% filter(plot_label == "dmskw_sym_cr_tt_") %>% pull(pred_gr_fit))[[1]]

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
                    symptomatic = as.logical(str_sub(mdl_fit$xlab, 12, -1)), 
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
         var_era = fct_expand(var_era, c("WT", "Alpha", "Delta", "Omicron")), 
         symptomatic = as_factor(case_when(symptomatic == TRUE ~ "Symptomatic", 
                                           symptomatic == FALSE ~ "Asymptomatic", 
                                           is.na(symptomatic) ~ "Unknown")),
         symptomatic = fct_relevel(symptomatic, "Symptomatic", "Asymptomatic", "Unknown"))
  

## Modify observed data for plotting
observed <- case_dat %>% 
  select(coll_date, symptomatic, gr_roll, sm_mean_ct, sm_skew_ct, var_era) %>% 
  pivot_longer(sm_mean_ct:sm_skew_ct) %>% 
  mutate(name = recode(name, "sm_mean_ct" = "Mean", "sm_skew_ct" = "Skewness"), 
         period = case_when(coll_date < split_date ~ "Training", 
                            coll_date >= split_date ~ "Test"),
         period=fct_relevel(as_factor(period), c("Training", "Test")), 
         symptomatic = as_factor(case_when(symptomatic == TRUE ~ "Symptomatic", 
                                           symptomatic == FALSE ~ "Asymptomatic", 
                                           is.na(symptomatic) ~ "Unknown")), 
         symptomatic = fct_relevel(symptomatic, "Symptomatic", "Asymptomatic", "Unknown"))

## Plot model fit lines against Ct predictors
p_mdl_preds_mean <- mdl_grid %>% 
  filter(name == "Mean") %>% 
  ggplot() +
  geom_point(data=observed %>% filter(name == "Mean"), aes(x=value,y=gr_roll,col=var_era), alpha=0.25) +
  geom_ribbon(aes(x=value, ymin=lower, ymax=upper, fill=var_era), alpha=0.25) +
  geom_line(aes(x=value, y=estimate, col=var_era)) +
  theme_main +
  facet_grid(rows=vars(symptomatic), cols=vars(period),scales="free") + 
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
  theme(legend.position="bottom") +
  ggtitle("A. Mean")

p_mdl_preds_skew <- mdl_grid %>% 
  filter(name == "Skewness") %>% 
  ggplot() +
  geom_point(data=observed %>% filter(name == "Skewness"), aes(x=value,y=gr_roll,col=var_era), alpha=0.25) +
  geom_ribbon(aes(x=value, ymin=lower, ymax=upper, fill=var_era), alpha=0.25) +
  geom_line(aes(x=value, y=estimate, col=var_era)) +
  theme_main +
  facet_grid(rows=vars(symptomatic), cols=vars(period),scales="free") + 
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
  theme(legend.position="bottom") +
  ggtitle("B. Skewness")
p_mdl_preds_mean + p_mdl_preds_skew



save_bak(p_mdl_preds_mean + p_mdl_preds_skew, 
         str_glue("./figures/{key}_model_preds_sym.{figext}"), 
         width=14, height=8, units="in", dpi=300)
