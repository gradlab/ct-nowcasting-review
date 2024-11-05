# Code
Contains all `R` scripts used for data cleaning, analysis, and graphing.

Scripts for main analyses are in the main subdirectory, in numbered order; most can be run independently without running the others except `6_sample_size_analysis.R` (by default, requires having previously run `5_TFT_analysis.R`) and `7_combined_outputs.R` (requires having previously run all other scripts).

## Code settings
The file `0_code_settings.R` specifies settings for running all other analyses. **This should be the only file you need to modify before replicating the analyses.** Change settings here before running the other main analysis scripts.

`0_code_settings.R` contains the following global settings:
- `dat_root` is the root directory for raw data folders, when access to raw data is needed and available
- `rollmean_window` specifies the smoothing window for rolling average or smoothed data (7 days by default)
- `inf_settings` defines the window (specified as half-width, 3 days by default) and growth rate threshold (0.025 by default) for identifying inflection points / periods of rapid change, per article section 'Nowcasting performance during time periods of rapid change in growth rate'
- `min_samples` specifies the threshold for minimum daily samples for inclusion in the analysis (10 by default)
- `variant_eras` defines date ranges for the variant eras used in the analysis
- `split_date` specifies the default end date for the training period used in the analysis
- `nowcast_window` is the length of window for rolling nowcasts (14 days by default)
- `figext` specifies the filetype extension for saving figures
- `main_mdl` is the plot label identifier for the 'main' model (of all model specifications tested) to use for the analysis
- `iter_chunk_size` specifies the frequency of saving results when running the iterative analysis, to balance memory usage and computing time

It also contains the following settings specific to each analysis (note that not all settings are used or included for every analysis):
- `dat_folder` specifies the subdirectory for the specific raw data folder for each analysis
- `update_data` (boolean) if `TRUE`, clean and process raw data; if `FALSE`, read cleaned de-identified data instead
- `hosp` and `ww` (boolean) specify whether hospitalization or wastewater data are available alongside case counts (not used in analysis)
- `update_basic_analysis` (boolean) if `TRUE` will rerun basic model fitting; if `FALSE` will read existing results
- `update_nowcast` (boolean) if `TRUE` will rerun nowcast model fitting (warning: this can take a while); if `FALSE` will read existing fitted models
- `process_nowcast` (boolean) if `TRUE` will reprocess fitted nowcast models to calculate performance metrics; if `FALSE` will read existing results
- `save_basic` (boolean) if `TRUE` (the default) will save basic model results; turn to `FALSE` to skip saving step (used for sample size analysis)
- `nc_slice` specifies the subset of models (defined in the respective script in [model-lists](code/model-lists)) to use for nowcasting analysis
- `nc_seq` specifies the series of weeks to use for rolling nowcast analysis windows
- `downsample` (boolean) if `TRUE` will downsample Ct value data according to `samp_fracs` (used only for sample size analysis); `FALSE` by default
- `samp_fracs` specifies downsampling settings to use in sample size analysis. Values between 0 and 1 will be parsed as proportions of daily data to include; values >1 will be parsed as maximum daily samples to include; values specified starting with 't' will be parsed as truncation thresholds for trimming daily data, e.g. "t0.05" excludes the highest and lowest 5% of reported Ct values for each day
- `draws` specifies the number of repeated draws to perform for sample size analyes
- `dat_folder_2` specifies the data folder for the comparison dataset (used only for sample size analysis)


## Other subdirectories
### auxiliary
Scripts for function definitions, data manipulation, model analysis, and workflow control, called by the main analysis scripts

### data-cleaning
Scripts for case and Ct value data cleaning for all datasets, including synthetic data. For reference, Ct value data cleaning scripts include code used for cleaning raw data (which are not shared here); if raw data are not accessible, de-identified data will automatically be read instead.

### graphing
Figure plotting scripts for all summary and results figures

### model-lists
Specifications for models used in each analysis, including model equations, labels, data groupings, training and testing data
