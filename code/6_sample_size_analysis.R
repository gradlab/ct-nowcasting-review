
## Read in analysis-specific settings
key <- "SMP"
source("./code/0_code_settings.R")

## Load and clean incidence data
source("./code/data-cleaning/mass_case_data_cleaning.R")

## Load and clean Ct data
source("./code/data-cleaning/mgb_data_cleaning.R")
dat_pos_main <- dat_pos # Copy cleaned dataset for later downsampling

for (frac in c(samp_fracs, 1)) {
  samp_frac <- frac
  if (!startsWith(samp_frac, "t")) samp_frac <- as.numeric(samp_frac)
  key <- str_glue("{key}_{samp_frac}")  # Set key for output labels
  
  ## Create tibble of model settings to iterate over
  source("./code/model-lists/generate_models_SMP.R")
  
  models <- models %>% mutate(plot_label = str_c(plot_label, samp_frac, "_"))
  
  if (samp_frac == 1 | is.character(samp_frac)) {
    ## Randomly downsample Ct data
    dat_pos <- dat_pos_main # Reset dataset to downsample to full cleaned dataset
    source("./code/data-cleaning/data_downsampling.R")
    
    ## Iterate over model settings for basic model fitting
    source("./code/auxiliary/run_basic_models.R")
    
    ## Iterate over model settings for nowcast model fitting
    source("./code/auxiliary/run_nowcast_models.R")
  } else {
    for (i in 1:draws) {
      key <- str_c(key, '_', sprintf("%03d", i))
      set.seed(i) # Change random number seed
      
      ## Randomly downsample Ct data
      dat_pos <- dat_pos_main # Reset dataset to downsample to full cleaned dataset
      source("./code/data-cleaning/data_downsampling.R")
      
      ## Iterate over model settings for basic model fitting
      source("./code/auxiliary/run_basic_models.R")
      
      ## Iterate over model settings for nowcast model fitting
      source("./code/auxiliary/run_nowcast_models.R")
      
      key <- str_sub(key, 1, -5)
    }
    set.seed(1) # Reset seed for reproducibility
  }
  key <- "SMP"  # Reset key
}

## Iterate over model settings for nowcast model fitting
source("./code/auxiliary/analyse_sample_size_results.R")

## Create sample size performance comparison plots & tables
source("./code/graphing/plot_sample_size_results.R")


