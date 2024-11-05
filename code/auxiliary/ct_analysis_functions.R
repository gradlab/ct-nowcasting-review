
#===============================================================================
# FUNCTION DEFINITIONS
#===============================================================================

## Generic function for saving output files (e.g. plots, tables, objects) while backing up existing versions
save_bak <- function(obj, path, savecmd=NULL, ...) {
  ## Extract components of specified file path
  ext <- path %>% str_split_i("\\.", -1)  # Get extension
  fdir <- path %>% str_remove(path %>% str_split_i("/", -1)) # Get file path
  fname <- path %>% str_split_i("/", -1) %>% str_remove(ext) %>% str_sub(1, -2)
  
  if (file.exists(path)) {  # Back up existing file, if any
    dir.create(str_glue("{fdir}/bak"), showWarnings=FALSE)  # Create backup directory if needed
    datestamp <- format(file.mtime(path), "%Y%m%d")  # Get existing file last modified date
    file.rename(path, str_glue("{fdir}/bak/{fname}_{datestamp}.{ext}"))  # Copy to backup and rename with modified date
  }
  
  ## Save file with appropriate command
  if (ext == "rds") {
    saveRDS(obj, file=path)
  } else if (ext == "csv") {
    write_csv(obj, path)
  } else if (ext %in% c("jpg", "png", "svg", "jpeg", "pdf")) {
    ggsave(path, obj, ...)
  } else if (!is.null(savecmd)) {  # Or use custom command if filetype not recognised
    tryCatch(savecmd(obj, path, ...), error=function(e) cat("Invalid save command specified\n"))
  } else {  # Or give error message
    cat("ERROR: File not saved\nPlease specify valid file extension or explicit save command\n")
  }
}


## Fill NA values for missing dates/group combos
## Needed to correctly calculate rolling averages
complete_dates <- function(data, datevar, grouping_vars, period=1) {
  data %>%
    ungroup() %>%
    complete({{ datevar }} := full_seq({{ datevar }}, period=period), !!!rlang::syms(grouping_vars))
}

## Summarise Ct distribution features
summarise_cts <- function(data, ct) {
  data %>% 
    summarise(mean_ct=mean({{ ct }}), 
              median_ct=median({{ ct }}), 
              skew_ct=moments::skewness({{ ct }}), 
              sd_ct=sd({{ ct }}), 
              N=n()) %>% 
    mutate(across(mean_ct:sd_ct, ~ ifelse(is.nan(.), NA, .)), # Replace NaN values (na_if() not working)
           across(mean_ct:sd_ct, na_if, Inf), 
           across(mean_ct:sd_ct, na_if, -Inf))
}

## Summarise test positivity
## UPDATE THIS WITH RUNNER FUNCTION TO CORRECTLY CALCULATE MOVING AVERAGE
summarise_prop_pos <- function(data, result, datevar, grouping_vars, pos_value="Positive") {
  data %>% 
    group_by({{ datevar }}, across(all_of(grouping_vars))) %>% 
    summarise(N = n(), 
              N_pos = sum({{ result }} == pos_value), 
              positivity = N_pos/N) %>% 
    group_by(across(all_of(grouping_vars))) %>% 
    mutate(sm_positivity = zoo::rollmean(positivity, rollmean_window, fill=NA), 
           positivity_change = positivity - lag(positivity, 1)) %>% 
    select(-N, -N_pos)
}

## Add variant eras from list
add_variant_eras <- function(.data, variant_eras, datevar) {
  variant_breaks <- data.frame(
    var_era = names(variant_eras), 
    end_date = as_date(unname(variant_eras))
  )
  .data %>% 
    left_join(variant_breaks, join_by(closest({{ datevar }} < end_date))) %>% 
    select(!end_date) %>% 
    mutate(var_era = factor(var_era, levels = names(variant_eras))) %>% 
    mutate(id = consecutive_id(var_era))
}

## Multiple correlation function
## Calculates correlations using `cor.test` between multiple x-variables and a 
## single y-variable, returning tibble of `cor.test` outputs with one row per x-var
list_corrs <- function(.data, x_list, y_var, method=c("pearson", "kendall", "spearman")) {
  iter_out <- vector("list", length(x_list))
  
  for (i in 1:length(x_list)) {
    corrs <- cor.test(.data[[x_list[[i]]]], .data[[y_var]], method=method) %>% 
      tidy() %>% 
      mutate(x_var = x_list[[i]]) %>% 
      select(x_var, everything())
    iter_out[[i]] <- corrs
  }
  iter_out <- bind_rows(iter_out)
  iter_out
}

## Simple Spearman rank correlation function
spear_func <- function(x_var, y_var, val="estimate", digits=2) {
  cor.test(x_var, y_var, method="spearman") %>% 
    tidy() %>% 
    pull({{ val }}) %>% 
    round(digits)
}


## Lag functions
## Create multiple lagged version of `var` as additional columns
calculate_lags <- function(df, var, lags) {
  map_lag <- lags %>% map(~partial(lag, n = .x))
  names(map_lag) <- lags
  return(df %>% mutate(across(.cols = {{ var }}, .fns = map_lag, .names = "{.col}_lag{.fn}")))
}

## Create list of names corresponding to lagged columns from `calculate_lags`
get_lagged_names <- function(vars, lags) {
  exp_vars <- rep(vars, each=length(lags))
  exp_lags <- rep(lags, length(vars))
  str_glue("{exp_vars}_lag{exp_lags}")
}



## Regression formula modification function to avoid single-level factor error
## Automatically modifies formula to exclude variables with only single level
reformulate_zv <- function(.data, dep_var, model) {
  # Extract model variables
  model_vars <- str_split_1(model, '\\+|\\*|:') %>% str_trim()
  
  model_terms <- str_split_1(model, '\\+') %>% str_trim()
  
  # Get the number of levels for all factor variables
  lev_leng <- .data %>% 
    # select(where(is.factor) & any_of(model_vars)) %>% 
    # map_int(~ length(levels(droplevels(.x))))
    select(any_of(model_vars)) %>% 
    map_int(~ length(unique(.x)))
  
  # Check if length is one
  invalid <- names(which(lev_leng == 1))
  
  # If any is invalid, it will have length > 0
  if (length(invalid) != 0) {
    for (i in invalid) {
      if (any(str_detect(model_terms, paste0('\\*[:space:]?', i)))) {
        # Remove invalid interaction terms from formula
        model_terms <- str_remove(model_terms,
          paste0('\\*[:space:]?',i))
      } else{
        # Remove entire invalid terms from formula
        model_terms <- model_terms[str_detect(model_terms, i, T)]
      }
      print(str_glue("{i} has no valid contrasts, removing {i}"))
    }
    model <- str_flatten(model_terms, '+')
    print(str_glue("New model is {model}"))
  }
  # Define the new formula
  as.formula(glue::glue('{dep_var} ~ {model}'))
}

## Data modification function to convert test data factor levels missing 
## from model data to `NA` to avoid 'factor has new levels' error
drop_new_levels <- function(newdata, fit) {
  ## Extract factor variables and levels included from fitted model
  xlevels <- broom.helpers::model_get_xlevels(fit)
  
  for (fac in names(xlevels)) {  # Loop over factor names
    new_levels <- which(!newdata[[fac]] %in% xlevels[[fac]])  # Identify indices of new factor levels
    newdata[[fac]][new_levels] <- NA  # Replace new factor levels with NA
    if (all(is.na(newdata[[fac]]))) {  # If factor levels entirely replaced with NA...
      newdata[[fac]] <- xlevels[[fac]][length(xlevels[[fac]])]
    }  # ...then replace with last known level for that factor
  }
  newdata %>% droplevels()
}


## Data prep function
## Create subset pivot table with which to run model
pivot_model_data <- function(.data, dep_vars, grouping_vars, values_from, datevar=coll_date, idcols=c()) {
  if (!is.null(grouping_vars)) {
    model_data <- .data %>% 
      pivot_wider(id_cols=c({{ datevar }}, all_of(dep_vars), all_of(idcols)), 
                  names_from=all_of(grouping_vars), 
                  values_from=all_of(values_from))
    # %>% 
      # fill(everything())  # Fill missing values
  } else {
    model_data <- .data %>% 
      select({{ datevar }}, all_of(dep_vars), all_of(idcols), all_of(values_from))
  }
  model_data
}


## Identify outliers from BRMS predictions or fits and replace with specified value
## Replaces entire set of predictions with `NA` by default if mean of predicted 
## values is too high
clear_outliers <- function(preds, threshold=2, rep_val=NA) {
  if (mean(simplify(abs(preds[, 1])), na.rm=TRUE) > threshold) {
    preds <- preds %>% replace(!is.na(preds), rep_val)
  }
  preds
}


## Return median and 95% CrI values from posterior samples
quantile_fun <- function(x, probs = c(0.025, 0.5, 0.975), ...) {
  tibble::tibble(
    .value = quantile(x, probs = probs, ...),
    .q = probs * 100
  )
}

## Main modeling function
## Fit model and make predictions to compare to observed data
## Returns a list of [1] fitted model, [2] Spearman rank correlations of 
## dependent variable against each numeric predictor variable, separated into 
## training & testing periods if applicable, [3-4] in-/out-of-sample model predictions, 
## and if available, [5-6] performance summaries for in-/out-of-sample predictions 
## against observed data generated using the `performance` package


# pred_gr <- model_data %>% 
#   model_predict(dep_var=gr_var, 
#                 model=prediction_model, 
#                 family="gaussian", 
#                 ncores=12, 
#                 test_data=test_data)
# 
# ## Fit epidemic direction to specified model
# pred_direct <- model_data %>% 
#   model_predict(dep_var=direct_var, 
#                 model=prediction_model, 
#                 family="binomial", 
#                 ncores=12,
#                 test_data=test_data)

model_predict <- function(.data, dep_var, model, family="gaussian", ncores=8, test_data=NULL, pr_in=TRUE,
                          id_cols=c("coll_date", "coll_wk", "var_era", "sm_gr_roll"), 
                          test_fill=c("var_era")) {
  ## Compile regression formula
  form <- as.formula(str_glue("{dep_var} ~ {model}"))
  form_vars <- all.vars(form)

  ## Subset data only to needed vars
  .data <- .data %>% 
    filter(!if_any(form_vars, is.na)) %>%  # Filter out rows with NA in needed vars
    select(any_of(id_cols), all_of(form_vars)) %>% 
    droplevels()  # Drop unused factor levels for all vars

  ## Calculate Spearman correlations of numeric predictors with dependent var
  form_vars_num <- .data[form_vars] %>% select(where(is.numeric)) %>% colnames()
  spear_data <- list_corrs(.data, form_vars_num[-1], form_vars_num[1], method='spearman') %>% 
    mutate(training = TRUE)

  ## Clean single-level variables from formula
  form <- reformulate_zv(.data, dep_var, model)

  ## Fit regression model to data
  fit <- gam(form, .data, family=family, method="REML", select=TRUE, cores=ncores, save_pars=save_pars(all=TRUE))

  ## Merge fitted-model-predicted values back with original data for comparison
  print("Fitting in-sample")
  preds_in <- fit %>% 
    fitted_values(.data) %>%  # Get basic fitted values and expected value CrI
    rename(Estimate=.fitted, Est.Error=.se, Q2.5=.lower_ci, Q97.5=.upper_ci) %>% 
    mutate(coverage = (.[[dep_var]] < Q97.5 & .[[dep_var]] > Q2.5), 
           id = consecutive_id(var_era)) 
    
  ## Get observed vs. predicted linear fit
  spear_preds_in <- cor.test(preds_in[[dep_var]], preds_in$Estimate, method='spearman') %>% tidy()
  
  perf_in <- NULL
  if (family == "gaussian") {  # If feasible, use `performance` to assess model fit
    perf_in <- model_performance(fit) %>% 
      tidy() %>% 
      select(column, mean) %>% 
      pivot_wider(names_from=column, values_from=mean)
    
    ## Calculate nRMSE
    perf_in <- perf_in %>% 
      mutate(nRMSEq = RMSE / IQR(model_data[[dep_var]], na.rm=TRUE), 
             nRMSEd = RMSE / as.numeric(quantile(model_data[[dep_var]], 0.9, na.rm=TRUE) - 
                                          quantile(model_data[[dep_var]], 0.1, na.rm=TRUE)))
    
    if (isTRUE(pr_in)) {
      ## Add posterior sample prediction intervals & calculate coverage
      preds_in <- preds_in %>% 
        left_join(fit %>%  # Draw posterior sample
                    posterior_samples(.data, n=10000, n_cores=12) %>% 
                    group_by(.row) %>% 
                    reframe(quantile_fun(.response)) %>%  # And calculate prediction intervals
                    pivot_wider(id_cols = .row, names_from = .q, values_from = .value, names_prefix = "Q") %>% 
                    select(.row, Q50, Q2.5, Q97.5) %>% 
                    rename(Estimate_ns=Q50, Q2.5_ns=Q2.5, Q97.5_ns=Q97.5), 
                  by=join_by(.row==.row)) %>% 
        mutate(coverage = (.[[dep_var]] < Q97.5_ns & .[[dep_var]] > Q2.5_ns))  # Overwrite with true PrI
    }
  }
  preds_in <- preds_in %>% select(-.row)
  perf_out <- perf_in

  ## If test data specified, get fit test data for comparison
  if (is.null(test_data)) {
    preds_out <- preds_in
    spear_preds_out <- spear_preds_in
    preds_rej <- NA
  } else {
    test_data <- test_data %>% 
      filter(!if_any(form_vars, is.na)) %>%  # Filter to match predictions
      select(any_of(id_cols), all_of(form_vars)) %>% 
      droplevels() %>%  # Drop unused factor levels for all vars
      drop_new_levels(fit) %>% 
      fill(all_of(test_fill))
    print("Fitting out-of-sample")
    
    ## Check for outlier predictions
    ## If predictions are outliers, replace entirety with NA
    # fits <- as_tibble(fit %>% predict.gam(test_data, se.fit=TRUE, type="response")) %>% magrittr::set_colnames(c("Estimate", "Est.Error")) %>% clear_outliers()
    # preds <- fit %>% predict(test_data) %>% clear_outliers() %>% 
    #   magrittr::set_colnames(c("Estimate_ns", "Est.Error_ns", "Q2.5_ns", "Q97.5_ns"))
    # preds_out <- test_data %>% 
    #   select(-var_era) %>%  # Remove and re-add variant eras in case
    #   add_variant_eras(variant_eras, coll_date) %>%  # they got dropped for model fitting
    #   select(coll_date, var_era, everything()) %>% 
    #   bind_cols(fits) %>% 
    #   # bind_cols(preds) %>% 
    #   mutate(coverage = (.[[dep_var]] < Estimate + 1.96 * `Est.Error` & .[[dep_var]] > Estimate - 1.96 * `Est.Error`), 
    #          id = consecutive_id(var_era))
    preds_out <- fit %>% 
      fitted_values(test_data) %>%  # Get basic fitted values and expected value CrI
      rename(Estimate=.fitted, Est.Error=.se, Q2.5=.lower_ci, Q97.5=.upper_ci) %>% 
      mutate(coverage = (.[[dep_var]] < Q97.5 & .[[dep_var]] > Q2.5), 
             id = consecutive_id(var_era)) 
    
    ## Log whether outliers replaced  ## DEPRECATED
    preds_rej <- all(is.na(preds_out))
    
    if (family == "gaussian") {  # Add posterior sample prediction intervals & calculate coverage
      preds_out <- preds_out %>% 
        left_join(fit %>%  # Draw posterior sample
                    posterior_samples(test_data, n=10000, n_cores=12) %>% 
                    group_by(.row) %>% 
                    reframe(quantile_fun(.response)) %>%  # And calculate prediction intervals
                    pivot_wider(id_cols = .row, names_from = .q, values_from = .value, names_prefix = "Q") %>% 
                    select(.row, Q50, Q2.5, Q97.5) %>% 
                    rename(Estimate_ns=Q50, Q2.5_ns=Q2.5, Q97.5_ns=Q97.5), 
                  by=join_by(.row==.row)) %>% 
        mutate(coverage = (.[[dep_var]] < Q97.5_ns & .[[dep_var]] > Q2.5_ns))
      
      perf_out <- calc_gof(preds_out, dep_var, "Estimate", "coverage")
      if (all(is.na(preds_out))) {
        perf_out <- perf_out %>% replace(!is.na(perf_out), NA)
      }
    }
    preds_out <- preds_out %>% select(-.row)
    
    spear_data <- spear_data %>% 
      bind_rows(list_corrs(
        test_data, form_vars_num[-1], form_vars_num[1], method='spearman') %>% 
          mutate(training = FALSE))
    
    ## Get observed vs. predicted Spearman rank correlations & predictive performance summary
    spear_preds_out <- cor.test(preds_out[[dep_var]], preds_out$Estimate, method='spearman') %>% tidy()
  }
  
  ## Create named list for output
  output <- list(fit=fit, 
                 spear_data=spear_data, 
                 preds_in=preds_in, 
                 preds_out=preds_out, 
                 preds_rej=preds_rej, 
                 spear_preds_in=spear_preds_in, 
                 spear_preds_out=spear_preds_out, 
                 perf_in=perf_in, 
                 perf_out=perf_out)
  output
}


## Organize output of iterative testing of multiple models, 
## compiling single dataframe w/ models tested and outputs for archiving
unnest_multi_models <- function(.models) {
  .models <- .models %>%  # Extract items from results columns to their own columns
    hoist(pred_gr,  # `unnest_wider` not working with objects, so use `hoist` instead
          pred_gr_fit = "fit", 
          pred_gr_preds_in = "preds_in", 
          pred_gr_preds_out = "preds_out", 
          pred_gr_preds_rej = "preds_rej") %>% 
    unnest_wider(pred_gr, names_sep="_", simplify=FALSE) %>% 
    hoist(pred_direct, 
          pred_direct_fit = "fit", 
          pred_direct_preds_in = "preds_in",
          pred_direct_preds_out = "preds_out") %>% 
    unnest_wider(pred_direct, names_sep="_", simplify=FALSE) 
  .models
}

## Summarises compiled output of multiple models, extracting key GOF measures 
## along with model information for easy comparison of model quality

summarise_multi_models <- function(.models, daily=TRUE) {
  .summary <- .models %>% 
    mutate(in_spear = map_dbl(pred_gr_spear_preds_in, "estimate"),  # Extract prediction performance measures
           cover_in = map(pred_gr_preds_in, "coverage"), 
           in_cover = map_dbl(cover_in, mean), 
           # gr_r2_out = map_dbl(pred_gr_perf_out, "R2"), 
           # gr_rmse_out = map_dbl(pred_gr_perf_out, "RMSE"),
           # gr_nrmseq_out = map_dbl(pred_gr_perf_out, "nRMSEq"),
           # gr_nrmsed_out = map_dbl(pred_gr_perf_out, "nRMSEd"),
           # gr_rho_out = map_dbl(pred_gr_spear_preds_out, "estimate"), 
           # cover_out = map(pred_gr_preds_out, "coverage"),
           # out_cover = map_dbl(cover_out, mean),
           gr_rej = pred_gr_preds_rej,
           dir_data_in = map2(pred_direct_preds_in, direct_var, ~.x %>% pull(.y)),  # Extract data needed to compute ROCs
           dir_preds_in = map2(pred_direct_preds_in, "Estimate", ~.x %>% pull(.y)),
           dir_data_out = map2(pred_direct_preds_out, direct_var, ~.x %>% pull(.y)), 
           dir_preds_out = map2(pred_direct_preds_out, "Estimate", ~.x %>% pull(.y))) %>% 
    mutate(in_ROC = map2(dir_data_in, dir_preds_in, function(x, y) tryCatch(pROC::roc(x, y, quiet=TRUE), error=function(e) NA)), # Create ROCs and extract AUC values
           in_AUC = map(in_ROC, function(x) tryCatch(as.numeric(pluck(x, "auc")), error=function(e) NA)), 
           out_ROC = map2(dir_data_out, dir_preds_out, function(x, y) tryCatch(pROC::roc(x, y, quiet=TRUE), error=function(e) NA)),
           out_AUC = map(out_ROC, function(x) tryCatch(as.numeric(pluck(x, "auc")), error=function(e) NA))) %>% 
    rename(`in` = pred_gr_perf_in, 
           out = pred_gr_perf_out) %>% 
    unnest_wider(`in`, simplify=TRUE, names_sep='_') %>%  # Extract performance measures from `performance` summary
    unnest_wider(out, simplify=TRUE, names_sep='_') %>%
    unnest(cols=c(in_AUC, out_AUC), keep_empty=TRUE) %>% 
    filter(daily=={{ daily }}) %>% 
    replace(is.null(.), NA) %>%
    select(plot_label:test_data, in_AIC:in_RMSE, in_nRMSEq:in_nRMSEd, out_R2:out_RMSE, 
           in_spear, out_spear, in_cover, out_cover, in_AUC, out_AUC, gr_rej)
  #   select(plot_label:test_data, ELPD:nRMSEd, in_spear:out_AUC,
  #          -c("cover_in", "cover_out", "dir_data_in", "dir_preds_in", "in_ROC", "dir_data_out", "dir_preds_out", "out_ROC"))
     
  .summary
}



## Takes tibble of multiple models for iterative fitting and returns longer tibble 
## with each model split into multiple training and testing periods, training up 
## to dates in `nowcast_dates` and testing over further `nowcast_window` days
prep_nowcast_models <- function(.models, nowcast_dates, nowcast_window) {
  iter_out <- vector("list", nrow(.models))  # Container for output
  
  for (i in 1:nrow(.models)) {
    rowdf <- .models %>% slice(i)
    nowcast_out <- vector("list", length(nowcast_dates))  # Sub-container for output
    
    for (j in 1:length(nowcast_dates)) {
      train_end <- nowcast_dates[[j]]  # Set training and testing cutoff dates
      test_end <- as_date(train_end) + days(nowcast_window)
      
      nowcast_out[[j]] <- rowdf %>%  # Modify iterative fitting specification with new model and test data
        mutate(plot_label = str_glue("{plot_label}{j}_"), 
               test_data = str_glue("{model_data} %>% filter(coll_date > '{train_end}' & coll_date <= '{test_end}')"), 
               model_data = str_glue("{model_data} %>% filter(coll_date <= '{train_end}')"), 
               mdl = i,  # Add model and window indices for easy grouping
               idx = j) %>% 
        select(mdl, idx, everything())
    }
    iter_out[[i]] <- bind_rows(nowcast_out)
  }
  nowcast_models <- bind_rows(iter_out)
  nowcast_models
}

## Summarises tibble of nowcast model results across multiple nowcast windows 
## into one entry per model, taking the mean or median of each relevant statistic
summarise_nc_models <- function(.models, models_base, summ_full, group_var=mdl){
  indexed <- .models %>% select(1:3) %>% left_join(summ_full)
  
  summ <- indexed %>% 
    group_by({{ group_var }}) %>% 
    # summarise(across(ELPD:dir_auc_out, ~ median(.x, na.rm=TRUE)))
    summarise(
      # across(AIC:R2_adjusted, ~ median(.x, na.rm=TRUE)),
              across(in_AIC:out_AUC, ~ mean(.x, na.rm=TRUE))) 
              # gr_r2_out = median(gr_r2_out, na.rm=TRUE), 
              # across(gr_rmse_out:dir_auc_out, ~ mean(.x, na.rm=TRUE)))
  # summarise(across(ELPD:dir_auc_out, ~ mean(.x, na.rm=TRUE)))) %>% 
  
  tryCatch(summ <- bind_cols(models_base, summ) %>% select({{ group_var }}, everything()), error=function(e) summ)
  
  summ
}

## Calculate goodness-of-fit metrics (MSE, RMSE, R2, Spearman coeff, coverage) from observed & predicted values
calc_gof <- function(.data, resp, pred, cover) {
  resp <- .data %>% pull({{ resp }})
  pred <- .data %>% pull({{ pred }})
  cover <- .data %>% pull({{ cover }})
  
  resids <- resp - pred
  # resids <- {{ resp }} - {{ pred }}
  MSE <- mean(resids^2, na.rm = TRUE)
  RMSE <- sqrt(MSE)
  R2 <- 1 - MSE / mean((resp - mean(resp, na.rm = TRUE))^2, na.rm = TRUE)
  spear <- spear_func(resp, pred, digits=3)
  cover <- mean(cover, na.rm=TRUE)
  # R2 <- 1 - MSE / mean(({{ resp }} - mean({{ resp }}, na.rm = TRUE))^2, na.rm = TRUE)
  
  return(list(MSE=MSE, RMSE=RMSE, R2=R2, spear=spear, cover=cover))
}

## Calculate ROC and AUC from observed & predicted directional values
calc_roc <- function(.data, resp, pred) {
  resp <- .data %>% pull({{ resp }})
  pred <- .data %>% pull({{ pred }})
  
  roc <- tryCatch(pROC::roc(resp, pred, quiet=TRUE), error=function(e) NA)
  auc <- tryCatch(as.numeric(pluck(roc, "auc")), error=function(e) NA)
  
  return(list(ROC=roc, AUC=auc))
}

