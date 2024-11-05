
color_palette <- c("#288BE4", "#E6A300", "#FB4D42", "#1F2C5C")

theme_main <- envalysis::theme_publish() + 
  theme(
    # axis.text.x=element_text(size=8, angle=45, hjust=1), 
    axis.text.y=element_text(size=8), 
    axis.title=element_text(size=10), 
    legend.text=element_text(size=8), 
    # text=element_text(family="Arial"), 
    plot.title=element_text(size=12), 
    axis.line.x = element_line(linewidth=0.75, color="grey10"), 
    axis.line.y = element_line(linewidth=0.5, color="grey10"), 
    plot.background = element_rect(fill="white"), 
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_line(colour = "grey80"), 
    panel.grid.minor.x = element_line(colour = "grey80"))



plot_pred_gr <- function(.data, y_var=Estimate, datevar=coll_date, observed=comb_dat_daily, inf=TRUE){
  p <- ggplot(.data) +
    geom_tile(data=observed, aes(x={{ datevar }}, y=0, width=1, height=Inf, fill=var_era), alpha=0.1) +
    # geom_tile(data=observed %>% filter(inflection==TRUE), aes(x={{ datevar }}, y=gr_roll, width=1, height=Inf), alpha=0.1) +
    geom_hline(yintercept=0,linetype="dashed") +
    # scale_x_date(breaks="1 month", date_labels="%y-%m") + 
    geom_line(data=observed, aes(x={{ datevar }}, y=gr_roll, col="Observed", group=id), alpha=0.5) +
    # geom_line(data=observed, aes(x={{ datevar }},y=sm_gr_roll, group=id)) +
    geom_ribbon(aes(x={{ datevar }},ymin=Q2.5,ymax=Q97.5, group=id), alpha=0.3) +
    geom_ribbon(aes(x={{ datevar }},ymin=Q2.5_ns,ymax=Q97.5_ns, group=id), alpha=0.15) +
    # geom_errorbar(aes(x={{ datevar }},ymin=Q2.5_ns,ymax=Q97.5_ns,col=var_era), alpha=0.3) +
    geom_line(aes(x={{ datevar }},y={{ y_var }},col="Predicted", group=id), linewidth=1) +
    # geom_point(data=observed %>% filter(inflection==TRUE, growing==0), aes(x={{ datevar }},y=-0.215,shape=inflection), col="blue") +
    scale_color_manual(name="Growth rate", 
                       values=c("Observed"="blue", "Predicted"="black"), 
                       breaks=c("Observed", "Predicted")) +
    scale_fill_manual(name="Variant era", 
                      values=c("WT"="black", "Alpha"="Orange","Omicron"="Red","Delta"="blue"), 
                      breaks=c("WT", "Alpha", "Delta", "Omicron")) +
    ylab("7-day rolling average growth rate") + 
    scale_x_date(breaks=as_date(c("2020-07-01", "2021-01-01", "2021-07-01", "2022-01-01", "2022-07-01", "2023-01-01")), 
                 labels=c("Jul 2020", "Jan 2021", "Jul 2021", "Jan 2022", "Jul 2022", "Jan 2023")) +
    coord_cartesian(ylim = c(-0.2, 0.2)) +
    theme_main +
    theme(legend.position="right", 
          legend.justification="top", 
          legend.box.spacing=unit(0, "pt"), 
          legend.box="vertical", 
          axis.title.x=element_blank()) + 
    guides(colour=guide_legend(order=1))
  
  if (inf==TRUE) {
    p <- p +
      geom_point(data=observed %>% filter(inflection==TRUE), aes(x={{ datevar }},y=0.217,shape=inflection), col="black") +
      scale_shape_manual(name=NULL,
                         values=c(16),
                         breaks=c(TRUE),
                         labels=c("Inflection period")) +
      guides(shape=guide_legend(order=2), colour=guide_legend(order=1))
  }
  p
}

##### OLD VERSION RESULTS PLOTTING FUNCTION #####
# plot_pred_gr <- function(.data, y_var=Estimate, datevar=coll_date, observed=comb_dat_daily){
#   p <- ggplot(.data) +
#     geom_tile(data=observed %>% filter(inflection==TRUE), aes(x={{ datevar }}, y=gr_roll, width=1, height=Inf), alpha=0.1) +
#     geom_hline(yintercept=0,linetype="dashed") +
#     # scale_x_date(breaks="1 month", date_labels="%y-%m") + 
#     geom_line(data=observed, aes(x={{ datevar }}, y=gr_roll, group=id), linetype="dashed", alpha=0.5) +
#     # geom_line(data=observed, aes(x={{ datevar }},y=sm_gr_roll, group=id)) +
#     geom_ribbon(aes(x={{ datevar }},ymin=Q2.5,ymax=Q97.5,fill=var_era), alpha=0.3) +
#     geom_ribbon(aes(x={{ datevar }},ymin=Q2.5_ns,ymax=Q97.5_ns,fill=var_era), alpha=0.1) +
#     # geom_errorbar(aes(x={{ datevar }},ymin=Q2.5_ns,ymax=Q97.5_ns,col=var_era), alpha=0.3) +
#     geom_line(aes(x={{ datevar }},y={{ y_var }},col=var_era, group=id)) +
#     scale_fill_manual(name="Variant era", 
#                       values=c("WT"="black", "Alpha"="Orange","Omicron"="Red","Delta"="blue"), 
#                       breaks=c("WT", "Alpha", "Delta", "Omicron")) +
#     scale_color_manual(name="Variant era", 
#                        values=c("WT"="black", "Alpha"="Orange","Omicron"="Red","Delta"="blue"), 
#                        breaks=c("WT", "Alpha", "Delta", "Omicron")) +
#     ylab("Log 7-day rolling average growth rate") + 
#     coord_cartesian(ylim = c(-0.2, 0.2)) +
#     theme_main +
#     theme(legend.position="right", 
#           legend.justification="top", 
#           legend.box.spacing=unit(0, "pt"), 
#           axis.title.x=element_blank())
# }

annotate_perf <- function(plot, .data, obs, perf) {
  ## Pull performance measures, using out-of-sample where available and in-sample otherwise
  rmse <- perf[['out_RMSE']]
  if (is.na(rmse)) rmse <- perf[['in_RMSE']]
  spear <- perf[['out_spear']]
  if (is.na(spear)) spear <- perf[['in_spear']]
  
  plot + 
    annotate("text", x=max(.data[["coll_date"]], na.rm=TRUE), 
             y=min(min(.data[["Q2.5_ns"]], na.rm=TRUE), min(obs[["gr_roll"]], na.rm=TRUE)), 
             hjust=1, vjust=-0.1, 
             label=str_glue("RMSE: {signif(rmse, 3)}\n", 
                            "Spearman's Rho: {signif(spear, 3)}\n", 
                            # "Coverage (95%): {signif(perf[['gr_cover_out']] * 100, 3)}%",
                            )
             )
}

## Create and plot ROC curve for epidemic direction predictions
plot_ROC <- function(.data, x_var="growing", y_var="Estimate", axis_labels=TRUE) {
  direct_ROC <- pROC::roc(.data[[x_var]], .data[[y_var]])
  p <- pROC::ggroc(direct_ROC) + 
    geom_abline(intercept=1, slope=1, linetype="dashed", alpha=0.5) +
    annotate("text", x=0.2, y=0.2, label=str_glue("AUC: {round(direct_ROC$auc, 3)}"))
  if (axis_labels==FALSE) {
    p <- p + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  }
  p
}

eval_mdl <- function(model_str) {
  eval(parse(text=model_str))
}

plot_xsec_ct_gr <- function(.data, i, lab) {
  sub_data <- bind_cols(.data %>% 
                          slice(i) %>% 
                          select(idx:gr_t) %>% 
                          pivot_longer(gr:gr_t, names_to="gr_col", values_to="gr_val") %>% 
                          slice(rep(row_number(), each=2)),  # Repeat gr values twice
                        .data %>% 
                          slice(i) %>% 
                          select(mean_ct:skew_ct_t) %>% 
                          pivot_longer(mean_ct:skew_ct_t, values_to="ct_val")) %>% 
    select(-gr_col) %>% 
    unnest_longer(c(gr_val, ct_val)) %>% 
    mutate(statistic = case_when(name %in% c("mean_ct", "mean_ct_t") ~ "Mean", 
                                 name %in% c("skew_ct", "skew_ct_t") ~ "Skewness"))
  
  p <- ggplot(sub_data) +
    # geom_point(data=test %>% filter(name %in% c("mean_ct", "mean_ct_t")), aes(x=ct_val, y=gr_val, col=name, shape=name, alpha=name)) +
    # geom_point(data=test %>% filter(name %in% c("skew_ct", "skew_ct_t")), aes(x=ct_val*10+30, y=gr_val, col=name, shape=name, alpha=name)) + 
    # geom_smooth(data=test %>% filter(name %in% c("mean_ct", "mean_ct_t")), aes(x=ct_val, y=gr_val, col=name, linetype=name), method="lm") +
    # geom_smooth(data=test %>% filter(name %in% c("skew_ct", "skew_ct_t")), aes(x=ct_val*10+30, y=gr_val, col=name, linetype=name), method="lm") +
    geom_point(aes(x=ct_val, y=gr_val, col=name, shape=name, alpha=name)) +
    geom_smooth(aes(x=ct_val, y=gr_val, col=name, linetype=name), method="lm") +
    geom_hline(yintercept=0,linetype="dashed") + 
    # scale_x_continuous(name="Ct value Mean", sec.axis=sec_axis(~(.-30)/10, name="Ct value Skewness")) + 
    scale_y_continuous(limits=c(-0.2,0.2)) +
    scale_shape_manual(labels=c("Mean, training", "Mean, testing", "Skewness, training", "Skewness, testing"),
                       values=c(0, 15, 2, 17)) +
    scale_colour_manual(labels=c("Mean, training", "Mean, testing", "Skewness, training", "Skewness, testing"),
                        values=c("red", "blue", "red", "blue")) +
    scale_alpha_manual(labels=c("Mean, training", "Mean, testing", "Skewness, training", "Skewness, testing"),
                       values=c(0.5, 1, 0.5, 1)) +
    scale_linetype_manual(labels=c("Mean, training", "Mean, testing", "Skewness, training", "Skewness, testing"),
                          values=c(1, 1, 2, 2)) +
    facet_wrap(~statistic,scales="free_x",ncol=2) + 
    ylab("Growth rate") +
    theme(legend.position="bottom") + 
    xlab("Statistic (cycle threshold value)")
  # labs(shape = "Statistic", colour = "Statistic", alpha = "Statistic", linetype = "Statistic")
  p
  ggsave(str_glue("{lab}_idx_{i}_ct_gr.png"), p,
         width=8, height=5, units="in", dpi=300)
}
