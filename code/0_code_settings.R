library(tidyverse)
library(lubridate)
library(patchwork)
library(broom)
# library(brms)
library(performance)
library(mgcv)
library(gratia)
# library(envalysis)
# library(tidybayes)
# library(EpiNow2)
# library(ggrepel)
# library(ggpubr)

## Set constants for entire analysis
dat_root <- "C:/Data"  # Root directory for raw data folders
rollmean_window <- 7  # Smoothing window for rolling average or smoothed data
inf_settings <- c(3, 0.025)  # Settings for inflection point identification: half-window width and threshold
min_samples <- 10  # Minimum daily samples for days to include
variant_eras <- c("WT" = "2020-12-01", "Alpha" = "2021-06-01", "Delta" = "2021-12-04", "Omicron" = "2023-03-31")
split_date <- "2021-12-31"  # Split date for single train-test split
nowcast_window <- 14  # Length of window for rolling nowcasts
figext <- "png"  # Filetype extension for saving figures
set.seed(1)  # Set seed for reproducibility
main_mdl <- "dmskw_cr_"  # Plot label identifier for 'main' model to use for results

iter_chunk_size <- 20  # Chunk size for saving models in iterative analysis
datestring <- format(Sys.time(), "%Y%m%d")

## Set defaults to be overridden for specific analyses
hosp <- FALSE
ww <- FALSE
grouping_vars <- c()
id_cols <- c("coll_date", "var_era", "gr", "gr_roll", "sm_gr", "sm_gr_roll", "inflection", "growing")  # Default ID columns for data grouping
save_basic <- TRUE

## Load shared functions
cat("\nLoading shared functions\n\n")
source("./code/auxiliary/ct_analysis_functions.R")
source("./code/auxiliary/ct_graphing_functions.R")

if (key == "SEL") {
  dat_folder <- str_glue("{dat_root}/SimData/")
  update_data <- FALSE
  update_basic_analysis <- TRUE
  update_nowcast <- TRUE
  process_nowcast <- TRUE
  nc_slice <- seq(1,24)
  nc_seq <- seq(16, 150, 2)  # Weeks for nowcast
  # id_cols <- c("coll_date", "var_era")  # For simulated data grouping
} else if (key == "SIM") {
  dat_folder <- str_glue("{dat_root}/SimData/")
  update_data <- FALSE
  update_basic_analysis <- FALSE
  update_nowcast <- FALSE
  process_nowcast <- FALSE
  nc_slice <- seq(1,18)
  nc_seq <- seq(16, 150, 2)
  # nc_seq <- seq(22, 26, 2)
  id_cols <- c("coll_date", "var_era")  # For simulated data grouping
} else if (key == "MGB") {
  dat_folder <- str_glue("{dat_root}/MGB/")
  update_data <- FALSE
  hosp <- TRUE
  ww <- TRUE
  update_basic_analysis <- FALSE
  update_nowcast <- FALSE
  process_nowcast <- FALSE
  nc_slice <- seq(1, 20)
  nc_seq <- seq(16, 150, 2)
  # nc_seq <- seq(16, 20, 2)
} else if (key == "LAC") {
  dat_folder <- "C:/Users/c249912/Desktop/Ct"
  update_data <- TRUE
  update_basic_analysis <- FALSE
  update_nowcast <- FALSE
  process_nowcast <- FALSE
  nc_slice <- seq(1, 18)
  nc_seq <- c(seq(16, 63, 2), seq(69, 103, 2))
} else if (key == "TFT") {
  dat_folder <- str_glue("{dat_root}/Tufts/")
  update_data <- TRUE
  hosp <- TRUE
  ww <- TRUE
  update_basic_analysis <- TRUE
  update_nowcast <- TRUE
  process_nowcast <- TRUE
  nc_slice <- seq(1, 14)
  nc_seq <- c(seq(16, 52, 2), seq(54, 84, 2))
} else if (key == "SMP") {
  dat_folder <- str_glue("{dat_root}/MGB/")  # MGB data used for main analysis
  dat_folder_2 <- str_glue("{dat_root}/Tufts/")  # Tufts data used for comparison
  update_data <- FALSE
  hosp <- TRUE
  ww <- TRUE
  update_basic_analysis <- FALSE
  save_basic <- FALSE
  update_nowcast <- FALSE
  process_nowcast <- TRUE
  nc_slice <- seq(1)
  nc_seq <- seq(16, 150, 2)
  # nc_seq <- seq(16, 36, 2)
  downsample <- TRUE
  # samp_fracs <- c(0.1, 0.5, 50)
  # draws <- 2
  # samp_fracs <- c("t0.025", "t0.05", "t0.1")
  samp_fracs <- c(0.1, 0.25, 0.5, 0.75, 25, 50, 100, "t0.025", "t0.05", "t0.1")
  draws <- 100
  # samp_fracs <- c(0.5, 0.75, 50, 100)
}




