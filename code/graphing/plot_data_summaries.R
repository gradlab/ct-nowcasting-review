#===============================================================================
# PLOT MAIN DATA DESCRIPTION FIGURE
#===============================================================================

## Create backgorund shading dataframe
dat_shade <- dat_pos %>% 
  filter(coll_date <= max(coll_wk)) %>% 
  group_by(coll_date) %>% 
  summarise(Mean=mean(Ct_value), Skewness=moments::skewness(Ct_value), StDev = sd(Ct_value)) %>% 
  complete_dates(coll_date, c()) %>% 
  fill(everything()) %>% 
  add_variant_eras(variant_eras, coll_date)

## Group data by day and week
grouping_vars <- c()
source("./code/auxiliary/ct_data_grouping.R")

##### SUPERSEDED BY p_ct_band
# ## Violin plot of Ct distribution by week
# p_ct_week <- ggplot(dat_pos) + 
#   geom_tile(data=dat_shade, aes(x=coll_date, y=Mean, width=1, height=Inf, fill=var_era), alpha=0.1) +
#   geom_violin(aes(x=coll_wk,y=Ct_value,group=coll_wk),scale="width",draw_quantiles=c(0.5),fill="grey70", alpha=0.5) + 
#   geom_smooth(aes(x=coll_wk,y=Ct_value),method="loess",span=0.3, se=FALSE) +
#   geom_vline(xintercept=as_date(split_date), linewidth=2, linetype="longdash", alpha=0.5) + 
#   # scale_x_date(breaks="1 month") +
#   # theme(axis.text.x=element_text(angle=45,hjust=1)) +
#   theme(axis.title.x=element_blank(), legend.position="none") +
#   # scale_x_date(limits=c(min(dat_pos$coll_date),max(dat_pos$coll_date))) +
#   scale_y_continuous(trans="reverse") +
#   scale_fill_manual(name="Variant era", 
#                     values=c("WT"="black", "Alpha"="orange","Omicron"="red","Delta"="blue"), 
#                     breaks=c("WT", "Alpha", "Delta", "Omicron")) +
#   ylab("Weekly Ct value distribution")
# # p_ct_week

p_ct_band <- dat_pos %>% 
  group_by(coll_wk) %>% 
  summarise(q = list(quantile(Ct_value, c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975))), 
            Mean = mean(Ct_value, na.rm=TRUE)) %>% 
  unnest_wider(q) %>% 
  add_variant_eras(variant_eras, coll_wk) %>% 
  mutate(id = consecutive_id(var_era)) %>% 
  ggplot() +
  geom_tile(data=dat_shade, aes(x=coll_date, y=Mean, width=1, height=Inf, fill=var_era), alpha=0.1) +
  # geom_line(aes(x=coll_wk, y=Mean), col="blue") + 
  geom_line(aes(x=coll_wk, y=`50%`, group=id), col="black") + 
  geom_ribbon(aes(x=coll_wk, ymin=`25%`, ymax=`75%`, group=id), alpha=0.3) +
  geom_ribbon(aes(x=coll_wk, ymin=`10%`, ymax=`90%`, group=id), alpha=0.2) + 
  geom_ribbon(aes(x=coll_wk, ymin=`5%`, ymax=`95%`, group=id), alpha=0.1) + 
  geom_ribbon(aes(x=coll_wk, ymin=`2.5%`, ymax=`97.5%`, group=id), alpha=0.05) + 
  # geom_vline(xintercept=as_date(split_date), linewidth=2, linetype="longdash", alpha=0.5) + 
  # scale_x_date(breaks="1 month") +
  # theme(axis.text.x=element_text(angle=45,hjust=1)) +
  theme_main +
  theme(axis.title.x=element_blank(), legend.position="none") +
  # scale_x_date(limits=c(min(dat_pos$coll_date),max(dat_pos$coll_date))) +
  scale_y_continuous(trans="reverse") +
  scale_fill_manual(name="Variant era", 
                    values=c("WT"="black", "Alpha"="orange","Omicron"="red","Delta"="blue"), 
                    breaks=c("WT", "Alpha", "Delta", "Omicron")) +
  ylab("Weekly Ct value median &\n50/80/90/95% intervals")
p_ct_band

## Extract x-axis limits
xlims <- ggplot_build(p_ct_band)
xlim_1 <- as_date(xlims$layout$panel_scales_x[[1]]$range$range[[1]])
xlim_2 <- as_date(xlims$layout$panel_scales_x[[1]]$range$range[[2]])

## Combined plot of incidence and incidence growth rate
p_incidence_gr <- ggplot(case_dat_daily) +
  geom_tile(data=dat_shade, aes(x=coll_date, y=Mean, width=1, height=Inf, fill=var_era), alpha=0.1) +
  geom_col(aes(x=date, y=mean_7day), alpha=0.5, col="grey50") +
  geom_line(aes(x=date, y=(gr_roll+0.25)*100000), alpha=0.5) + 
  geom_line(aes(x=date, y=(sm_gr_roll_c+0.25)*100000), col="blue") + 
  # geom_vline(xintercept=as_date(split_date), linewidth=2, linetype="longdash", alpha=0.5) +
  geom_hline(yintercept=25000, linetype="longdash", alpha=0.5) + 
  coord_cartesian(ylim=c(0,50000)) +
  scale_x_date(limits=c(xlim_1, xlim_2)) +
  theme_main +
  theme(axis.title.x=element_blank(), legend.position="none") +
  scale_fill_manual(name="Variant era", 
                    values=c("WT"="black", "Alpha"="orange","Omicron"="red","Delta"="blue"), 
                    breaks=c("WT", "Alpha", "Delta", "Omicron")) +
  scale_y_continuous(name="7-day rolling average incidence", sec.axis=sec_axis(~./100000 -0.25, name="Incidence growth rate"))
p_incidence_gr

## Faceted scatterplot of growth rate against Ct distribution
p_gr_ct <- comb_dat_daily %>% 
  select(coll_date, sm_mean_ct, sm_skew_ct, gr_roll, var_era) %>% 
  pivot_longer(sm_mean_ct:sm_skew_ct) %>% 
  mutate(name = recode(name, "sm_mean_ct" = "Mean", "sm_skew_ct" = "Skewness")) %>% 
  # mutate(name = case_when(coll_date < split_date ~ str_glue("{name}, training"),
  #                         coll_date >= split_date ~ str_glue("{name}, testing")),
  #        name = fct_relevel(name, "Mean, training", "Skewness, training")) %>%
  ggplot() +
  geom_point(aes(x=value,y=gr_roll,col=var_era), alpha=0.25) +
  geom_smooth(aes(x=value,y=gr_roll, col=var_era), method="gam") +
  theme_main +
  facet_wrap(~name,scales="free",ncol=4) + 
  scale_y_continuous(limits=c(-0.25,0.25)) +
  geom_hline(yintercept=0,linetype="dashed") +
  xlab("Statistic (cycle threshold value)") +
  ylab("Incidence growth rate") +
  scale_colour_manual(name="Variant era", 
                      values=c("WT"="black", "Alpha"="orange","Omicron"="red","Delta"="blue"), 
                      breaks=c("WT", "Alpha", "Delta", "Omicron")) +
  theme(legend.position="right")
p_gr_ct

## Set layout for patchwork plot
layout <- "
  AAAAAAA
  BBBBBBB
  CCCCCCD
"

p_comb <- (p_ct_band + labs(tag="A")) / (p_incidence_gr + labs(tag="B")) / (p_gr_ct + labs(tag="C")) + plot_layout(design=layout)
p_comb

save_bak(p_comb, 
         str_glue("./figures/{key}_data_summary.{figext}"), 
         width=10, height=8, units="in", dpi=300)

# remove(dat_shade)

#===============================================================================
# PLOT CROSS-CORRELATION ANALYSES
#===============================================================================

## Calculate cross-correlations for smoothed Ct and GR
mean_7day_ct <- comb_dat_daily %>% select(sm_mean_ct, gr_roll) %>% drop_na() %>% pull(sm_mean_ct)
mean_7day_gr <- comb_dat_daily %>% select(sm_mean_ct, gr_roll) %>% drop_na() %>% pull(gr_roll)

ccf_7day <- ccf(mean_7day_ct,mean_7day_gr,main=paste0("7-day Ct\n vs. 7-day GR"),ylim=c(-0.5,0.2),cex.main=0.5)

#####
# mean_7day_ct_1 <- comb_dat_daily %>% filter(coll_date <= "2021-12-01") %>% select(sm_mean_ct, gr_roll) %>% drop_na() %>% pull(sm_mean_ct)
# mean_7day_gr_1 <- comb_dat_daily %>% filter(coll_date <= "2021-12-01") %>% select(sm_mean_ct, gr_roll) %>% drop_na() %>% pull(gr_roll)
# 
# ccf_7day_1 <- ccf(mean_7day_ct_1,mean_7day_gr_1,main=paste0("7-day Ct\n vs. 7-day GR"),cex.main=0.5)
# cat("Maximum correlation at", ccf_7day_1$lag[which.max(abs(ccf_7day_1$acf))], "days\n")
# 
# 
# mean_7day_ct_2 <- comb_dat_daily %>% filter(coll_date > "2021-12-01") %>% select(sm_mean_ct, gr_roll) %>% drop_na() %>% pull(sm_mean_ct)
# mean_7day_gr_2 <- comb_dat_daily %>% filter(coll_date > "2021-12-01") %>% select(sm_mean_ct, gr_roll) %>% drop_na() %>% pull(gr_roll)
# 
# ccf_7day_2 <- ccf(mean_7day_ct_2,mean_7day_gr_2,main=paste0("7-day Ct\n vs. 7-day GR"),cex.main=0.5)
# cat("Maximum correlation at", ccf_7day_2$lag[which.max(abs(ccf_7day_2$acf))], "days\n")
# 
#####

ccf_7day_vals <- tibble(acf = ccf_7day$acf, lag = ccf_7day$lag)

cat("Maximum correlation at", ccf_7day$lag[which.max(abs(ccf_7day$acf))], "days\n")

## Plot cross-correlations
p_ccf_7day <- ggplot(ccf_7day_vals) + 
  geom_col(aes(x=lag, y=acf)) +
  theme_main +
  xlab("Lag (days)")

##### Incidence GR vs. hospitalisation GR CCF?

## Create faceted scatterplots of growth rate against mean
p_ccf_scatter <-   comb_dat_daily %>% 
  select(coll_date, sm_mean_ct, gr_roll) %>% 
  calculate_lags(gr_roll, 0:14) %>% 
  rename_with(~str_sub(.x, 9), starts_with("gr_roll_")) %>% 
  pivot_longer(str_sub(get_lagged_names("gr_roll", 0:14), 9)) %>% 
  mutate(name = fct_relevel(name, str_sub(get_lagged_names("gr_roll", 0:14), 9))) %>% 
  ggplot() +
  geom_point(aes(x=sm_mean_ct, y=value), alpha=0.05) +
  theme_main +
  facet_wrap(vars(name), nrow=3) +
  xlab("7-day rolling mean Ct value") +
  ylab("Lagged incidence growth rate")
  
## Create faceted matched normalised mean & growth rate lines
p_ccf_lines <- comb_dat_daily %>% 
  select(coll_date, sm_mean_ct, sm_gr_roll_c) %>% 
  filter(coll_date <= "2021-12-31", coll_date >= "2021-01-01") %>% 
  calculate_lags(sm_gr_roll_c, 0:14) %>% 
  rename_with(~str_sub(.x, 14), starts_with("sm_gr_roll_c_")) %>% 
  pivot_longer(str_sub(get_lagged_names("sm_gr_roll_c", 0:14), 14)) %>% 
  mutate(name = fct_relevel(name, str_sub(get_lagged_names("sm_gr_roll_c", 0:14), 14))) %>% 
  ggplot() +
  geom_line(aes(x=coll_date, y=-(sm_mean_ct - mean(sm_mean_ct, na.rm=TRUE))/20), linewidth=1) +
  geom_line(aes(x=coll_date, y=value), colour="blue", linewidth=1.5, alpha=0.5) + 
  scale_y_continuous(name="Lagged incidence growth rate", 
                     sec.axis=sec_axis(~mean((comb_dat_daily %>% filter(coll_date <= "2021-12-31", coll_date >= "2021-01-01"))$sm_mean_ct, na.rm=TRUE) - .*20, 
                                       name="Smoothed 7-day rolling mean Ct value")) +
  facet_wrap(vars(name), nrow=3) +
  theme_main +
  theme(axis.text.x=element_blank()) +
  xlab("Time (Jan-Dec 2021)")

p_ccf_7day / p_ccf_scatter / p_ccf_lines

save_bak((p_ccf_7day + labs(tag="A")) / (p_ccf_scatter + labs(tag="B")) / (p_ccf_lines + labs(tag="C")), 
         str_glue("./figures/{key}_cross_corrs.{figext}"), 
         width=12, height=12, units="in", dpi=300)

#===============================================================================
# PLOT SUPPLEMENTARY CT DISTRIBUTION OVER TIME FIGURE
#===============================================================================

## Faceted line plot of Ct mean and skewness over time
p_ct_dist_week <- dat_pos %>% 
  group_by(coll_wk) %>%
  summarise(Mean=mean(Ct_value), Skewness=moments::skewness(Ct_value), StDev = sd(Ct_value)) %>% 
  pivot_longer((-coll_wk)) %>% 
  ggplot() + 
  geom_tile(data=dat_shade %>% select(-id) %>% pivot_longer((-c("coll_date", "var_era"))), 
            aes(x=coll_date, y=value, width=1, height=Inf, fill=var_era), alpha=0.1) +
  geom_line(aes(x=coll_wk,y=value)) + 
  geom_vline(xintercept=as_date(split_date), linewidth=2, linetype="longdash", alpha=0.5) + 
  facet_wrap(~name,scales="free_y",ncol=1) + 
  theme_main +
  theme(axis.title.x=element_blank(), legend.position="none") +
  scale_x_date(limits=c(xlim_1, xlim_2)) +
  scale_fill_manual(name="Variant era", 
                    values=c("WT"="black", "Alpha"="orange","Omicron"="red","Delta"="blue"), 
                    breaks=c("WT", "Alpha", "Delta", "Omicron")) +
  ylab("Ct value")
p_ct_dist_week

save_bak(p_ct_dist_week, 
         str_glue("./figures/{key}_ct_dist_week.{figext}"), 
         width=8, height=5, units="in", dpi=300)

#===============================================================================
# CREATE SUPPLEMENTARY DATA CHARACTERISTICS TABLE
#===============================================================================

dat_summ <- list(dataset = key, 
                 n_pos = nrow(dat_pos), 
                 days_total = nrow(comb_dat_daily), 
                 days_mdl = nrow(comb_dat_daily %>% filter(!is.na(sm_mean_ct), !is.na(sm_skew_ct), !is.na(gr_roll))), 
                 days_excl = nrow(comb_dat_daily %>% filter(is.na(N))), 
                 spear_mean = spear_func(comb_dat_daily$sm_mean_ct, comb_dat_daily$gr_roll), 
                 spear_skew = spear_func(comb_dat_daily$sm_skew_ct, comb_dat_daily$gr_roll), 
                 min_gr = min(comb_dat_daily$gr_roll), 
                 max_gr = max(comb_dat_daily$gr_roll), 
                 inf_days = nrow(comb_dat_daily %>% filter(!is.na(sm_mean_ct), !is.na(sm_skew_ct), !is.na(gr_roll), inflection==TRUE)),
                 inf_perc = mean((comb_dat_daily %>% filter(!is.na(sm_mean_ct), !is.na(sm_skew_ct), !is.na(gr_roll)))$inflection))

save_bak(dat_summ, str_glue("./outputs/{key}_data_summary.rds"))
