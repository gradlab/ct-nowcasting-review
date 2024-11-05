
## Read in analysis-specific settings
key <- "SEL"
source("./code/0_code_settings.R")

## Load and clean Ct data
source("./code/data-cleaning/sim_data_cleaning.R")

dat_pos <- dat_pos %>% filter(set == "base")
case_dat_daily <- case_dat_daily %>% filter(set == "base")

## Create tibble of model settings to iterate over
source("./code/model-lists/generate_models_SEL.R")

## Iterate over model settings for basic model fitting
source("./code/auxiliary/run_basic_models.R")

## Iterate over model settings for nowcast model fitting
source("./code/auxiliary/run_nowcast_models.R")

## Compile nowcast model results
source("./code/auxiliary/analyse_nowcast_results.R")

## Create summary figures and tables
source("./code/graphing/plot_model_selection_summaries.R")