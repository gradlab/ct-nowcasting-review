
## Read in analysis-specific settings
key <- "SIM"
source("./code/0_code_settings.R")

## Load and clean Ct data
source("./code/data-cleaning/sim_data_cleaning.R")

## Create basic data visualisation plots
source("./code/graphing/plot_sim_data_summaries.R")

## Create tibble of model settings to iterate over
source("./code/model-lists/generate_models_SIM.R")

## Iterate over model settings for basic model fitting
source("./code/auxiliary/run_basic_models.R")

## Iterate over model settings for nowcast model fitting
source("./code/auxiliary/run_nowcast_models.R")

## Compile nowcast model results
source("./code/auxiliary/analyse_nowcast_results.R")

## Create main and supplementary plots of model fit-to-data
source("./code/graphing/plot_sim_model_results.R")
