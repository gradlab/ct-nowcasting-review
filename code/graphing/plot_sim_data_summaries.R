grouping_vars <- c("set")

## Group data for plotting
source("./code/auxiliary/ct_data_grouping.R")

## Relabel grouped data
scatter_dat_full <- comb_dat_daily %>% 
  filter(set != "sknt",
         set != "hvar") %>% 
  mutate(set = as_factor(set), 
         set = fct_relevel(set, "idel", "rknt", "rvar", "rsam", "base"),
         set = fct_recode(set, 
                          "Ideal condition" = "idel", 
                          "Realistic kinetics" = "rknt", 
                          "Realistic variation" = "rvar", 
                          "Clustered sampling" = "rsam", 
                          "Baseline condition" = "base"))

## Create version with outliers removed
scatter_dat_trunc <- scatter_dat_full %>% 
  group_by(set) %>% 
  filter(mean_ct > quantile(mean_ct, 0.01, na.rm=TRUE) & mean_ct < quantile(mean_ct, 0.99, na.rm=TRUE),
         sm_mean_ct > quantile(sm_mean_ct, 0.01, na.rm=TRUE) & sm_mean_ct < quantile(sm_mean_ct, 0.99, na.rm=TRUE)) %>%
  ungroup() %>% 
  filter(abs(gr_roll) <= 0.25)


###### SCATTERPLOTS TO BE ADDED HERE

p_scatter_full <- ggplot(scatter_dat_full) +
  geom_point(aes(x=sm_mean_ct, y=gr_roll), alpha=0.3) +
  geom_smooth(aes(x=sm_mean_ct, y=gr_roll), method="lm") +
  geom_hline(linetype="dashed", yintercept = 0, col="grey50") +
  facet_wrap(vars(set), scales="free_x")


p_kinetics_main <- dat_pos %>% 
  filter(set != "sknt",
         set != "hvar") %>% 
  mutate(set = as_factor(set), 
         set = fct_relevel(set, "idel", "rknt", "rvar", "rsam", "base"),
         set = fct_recode(set, 
                          "Ideal condition" = "idel", 
                          "Realistic kinetics" = "rknt", 
                          "Realistic variation" = "rvar", 
                          "Clustered sampling" = "rsam", 
                          "Baseline condition" = "base")) %>% 
  group_by(set) %>% 
  slice_sample(prop=0.1) %>% 
  ggplot() +
  geom_jitter(aes(x=rep_delay, y=Ct_value), alpha=0.1) +
  scale_y_continuous(trans="reverse") +
  facet_wrap(vars(set))


save_bak(p_scatter_full / p_kinetics_main, 
         str_glue("./figures/SIM_data_summary_main.{figext}"), 
         width=18, height=12, units="in")
