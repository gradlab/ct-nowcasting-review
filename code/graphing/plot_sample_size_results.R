#===============================================================================
# CREATE DATA TABLE FOR SUMMARY FIGURE
#===============================================================================

## Read in baseline model results
comp_preds <- readRDS(str_glue("{dat_folder}/{key}_1_nc_fitted_1.rds")) %>% 
  group_by(mdl) %>% 
  summarise(pred_gr_comp = list(bind_rows(pred_gr_preds_out)), 
            pred_dir_comp = list(bind_rows(pred_direct_preds_out))) %>% 
  select(-mdl)

## Read in TFT results files for external comparison
models <- readRDS(str_glue("{dat_folder_2}/TFT_models_fitted.rds"))
idx <- which(models$plot_label == main_mdl)  # Identify which model to use
nc_ext <- readRDS(str_glue("{dat_folder_2}/TFT_nc_fitted_{idx}.rds")) 

## Combine baseline and external comparison results
nc_ext <- nc_ext %>% 
  group_by(mdl) %>% 
  summarise(pred_gr_ext = list(bind_rows(pred_gr_preds_out)), 
            pred_dir_ext = list(bind_rows(pred_direct_preds_out))) %>% 
  mutate(mdl = "TFT") %>% 
  bind_cols(comp_preds) %>%  # Add results from comparison run
  mutate(pred_gr_comp = map2(pred_gr_comp, pred_gr_ext,  # Limit coll_dates in comparison to those in main output
                             \(x, y) x %>% filter(coll_date %in% (y %>% drop_na() %>% pull(coll_date)))), 
         pred_dir_comp = map2(pred_dir_comp, pred_dir_ext,
                              \(x, y) x %>% filter(coll_date %in% (y %>% drop_na() %>% pull(coll_date))))) %>% 
  mutate(out = map(pred_gr_ext, calc_gof, "gr_roll", "Estimate", "coverage"), 
         comp = map(pred_gr_comp, calc_gof, "gr_roll", "Estimate", "coverage")) %>% 
  unnest_wider(out, names_sep="_") %>% 
  unnest_wider(comp, names_sep="_") %>% 
  mutate(out = map(pred_dir_ext, calc_roc, "growing", "Estimate"), 
         comp = map(pred_dir_comp, calc_roc, "growing", "Estimate")) %>% 
  unnest_wider(out, names_sep="_", strict=TRUE) %>% 
  unnest_wider(comp, names_sep="_", strict=TRUE) %>% 
  mutate(comp_gr_n = map_int(pred_gr_comp, nrow), 
         comp_dir_n = map_int(pred_dir_comp, nrow))

## Read in main sample size analysis model results
nc_models_summ_out_clean <- readRDS(str_glue("./outputs/{key}_nc_models_summ_out.rds"))

## Combine external comparison and main sample size results
ss_comp_mdls <- nc_ext %>% 
  select(mdl, out_RMSE, comp_RMSE, out_AUC, comp_AUC, comp_gr_n) %>% 
  bind_rows(nc_models_summ_out_clean %>% 
              mutate(mdl = as.character(mdl)) %>% 
              select(mdl, out_RMSE, comp_RMSE, out_AUC, comp_AUC, comp_gr_n)) %>% 
  mutate(mdl = fct_relevel(mdl, c("TFT", samp_fracs))) %>%
  pivot_longer(out_RMSE:comp_gr_n) %>% 
  mutate(type = case_when(mdl == "TFT" ~ "Alternate", 
                          mdl %in% c(0.1, 0.25, 0.5, 0.75) ~ "Proportional downsample", 
                          mdl %in% c(25, 50, 100) ~ "Daily max samples", 
                          mdl %in% c("t0.025", "t0.05", "t0.1") ~ "Trimmed daily values"), 
         type = fct_relevel(as_factor(type), "Alternate", "Proportional downsample", "Daily max samples", "Trimmed daily values"))

ss_comp_mdls_full <- nc_ext %>% 
  select(mdl, comp_gr_n, ends_with("RMSE"), ends_with("spear"), ends_with("cover"), ends_with("AUC")) %>% 
  bind_rows(nc_models_summ_out_clean %>% 
              mutate(mdl = as.character(mdl)) %>% 
              select(mdl, comp_gr_n, ends_with("RMSE"), ends_with("spear"), ends_with("cover"), ends_with("AUC")) %>% 
              select(!starts_with("in"))) %>% 
  mutate(mdl = c("Tufts", "10% downsample", "25% downsample", "50% downsample", "75% downsample", 
                 "25 max daily samples", "50 max daily samples", "100 max daily samples", "2.5% trimmed", "5% trimmed", "10% trimmed")) %>% 
  mutate(across(out_RMSE:comp_AUC, \(x) signif(x, 3))) %>% 
  rename(Data = mdl,
         `Days included` = comp_gr_n, 
         `RMSE, nowcast` = out_RMSE,
         `RMSE, comparison` = comp_RMSE, 
         `Rho, nowcast` = out_spear,
         `Rho, comparison` = comp_spear, 
         `Coverage, nowcast` = out_cover,
         `Coverage, comparison` = comp_cover, 
         `AUC, nowcast` = out_AUC, 
         `AUC, comparison` = comp_AUC)

remove(models, idx, nc_ext)

ss_comp_mdls_full %>% save_bak(str_glue("./outputs/{key}_comb_perf_table_full.rds"))
ss_comp_mdls_full %>% save_bak(str_glue("./outputs/{key}_comb_perf_table_full.csv"))


#===============================================================================
# GRAPH COMPARATIVE PERFORMANCE METRICS
#===============================================================================

## Plot RMSE, AUC, and days included by sample size
p_ss_comp_RMSE <- ss_comp_mdls %>%
  mutate(mdl = fct_recode(mdl, "2.5%" = "t0.025", "5%" = "t0.05", "10%" = "t0.1")) %>% 
  ggplot() +
  geom_point(data=. %>% filter(name %in% c("out_RMSE", "comp_RMSE")), aes(x=mdl, y=value, col=name)) + 
  geom_line(data=. %>% filter(name %in% c("out_RMSE", "comp_RMSE")), aes(x=mdl, y=value, group=mdl), alpha=0.5) +
  theme_main +
  theme(axis.title.x=element_blank(), 
        strip.text.x = element_text(size = 10), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey80"), 
        legend.position="none") +
  scale_colour_manual(values=c("red", "blue"), labels=c("Baseline", "Downsampled")) +
  ylab("RMSE") +
  facet_grid(cols=vars(type), scales="free_x", space="free_x")

p_ss_comp_AUC <- ss_comp_mdls %>%
  mutate(mdl = fct_recode(mdl, "2.5%" = "t0.025", "5%" = "t0.05", "10%" = "t0.1")) %>% 
  ggplot() +
  geom_point(data=. %>% filter(name %in% c("out_AUC", "comp_AUC")), aes(x=mdl, y=value, col=name)) +
  geom_line(data=. %>% filter(name %in% c("out_AUC", "comp_AUC")), aes(x=mdl, y=value, group=mdl), alpha=0.5) +
  geom_col(data=. %>% filter(name=="comp_gr_n"), aes(x=mdl, y=(value+1250)/2750), alpha=0.2) + 
  scale_y_continuous(name="AUC", sec.axis=sec_axis(~.*2750 - 1250, name="Number of days included")) +
  coord_cartesian(ylim=c(0.6, 0.8)) +
  theme_main +
  theme(axis.title.x=element_blank(), 
        strip.text.x = element_text(size = 10),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey80"), 
        legend.position="bottom", 
        legend.text = element_text(size=10)) + 
  scale_colour_manual(values=c("red", "blue"), labels=c("Baseline", "Downsampled"), name="Dataset") +
  facet_grid(cols=vars(type), scales="free_x", space="free_x")

p_ss_comp_RMSE / p_ss_comp_AUC

save_bak(p_ss_comp_RMSE / p_ss_comp_AUC, 
         str_glue("./figures/{key}_sample_size_summary.{figext}"),
         width=7.5, height=6, dpi=300)


