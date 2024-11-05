## If specified, downsample dataset for sample size analyses
if (downsample == TRUE) {
  if (is.character(samp_frac)) {
    cutoff <- as.numeric(str_sub(samp_frac, 2))
    cat("Trimming daily distribution ends by", (cutoff * 100), "%\n")
    dat_pos <- dat_pos %>% 
      group_by(coll_date) %>% 
      mutate(lower = quantile(Ct_value, cutoff), 
             upper = quantile(Ct_value, (1-cutoff))) %>% 
      filter(Ct_value > lower, Ct_value < upper) %>%  # Trim daily samples to specified cutoff
      select(-lower, -upper)
  } else if (samp_frac > 1) {  # Restrict to max samples per day
    cat("Restricing to maximum of", samp_frac, "samples per day\n")
    dat_pos <- dat_pos %>% 
      group_by(coll_date) %>% 
      slice_sample(n=samp_frac)
  } else if (samp_frac <= 1) {  # Or to specified proportion of total data available
    cat("subsampling", (samp_frac*100), "% of data\n")
    dat_pos <- dat_pos %>% 
      slice_sample(prop=samp_frac)
  }
  cat("Final total Ct values used:", nrow(dat_pos), "\n")
}


