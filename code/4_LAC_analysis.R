
## Read in analysis-specific settings
key <- "LAC"
source("./code/0_code_settings.R")

## Load and clean Ct data
source("./code/data-cleaning/lac_data_cleaning.R")

## Load and clean incidence data
source("./code/data-cleaning/lac_case_data_cleaning.R")

## Create basic data visualisation plots
source("./code/graphing/plot_data_summaries.R")

## Create data supplement
if (update_data == TRUE) {
  rmarkdown::render("./code/data-cleaning/LAC_data_supplement.Rmd", output_dir="./outputs", knit_root_dir=getwd(), envir=new.env())
}

## Create tibble of model settings to iterate over
source("./code/model-lists/generate_models_LAC.R")

## Iterate over model settings for basic model fitting
source("./code/auxiliary/run_basic_models.R")

## Iterate over model settings for nowcast model fitting
source("./code/auxiliary/run_nowcast_models.R")

## Compile nowcast model results
source("./code/auxiliary/analyse_nowcast_results.R")

## Create main and supplementary plots of model fit-to-data
source("./code/graphing/plot_main_model_results.R")

## LAC only - create stratified sensitivity analysis figures
source("./code/graphing/plot_LAC_sens_results.R")
