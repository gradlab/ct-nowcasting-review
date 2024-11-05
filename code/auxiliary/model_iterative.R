## Run iterative fitting over tibble of model settings
## Takes `.models` as input and returns updated `.models`

## Break up model indices into chunks
chunks <- split(seq(nrow(.models)), seq(nrow(.models)) %/% iter_chunk_size)

## Initialise new container for output if it doesn't already exist & back up output of incomplete runs
if (exists("iter_out")) iter_out_bak <- iter_out
if (!exists("iter_out")) iter_out <- vector("list", length(chunks))
error_tracker <- c()

## Find first empty element of container
first_idx <- length(iter_out[!sapply(iter_out, is.null)]) + 1

for (i in first_idx:length(chunks)) {
  
  ## Create sub-container
  cat("INITIATING CHUNK", i, "\n")
  
  #####
  iter_out_mini <- tibble(pred_gr=NA, pred_direct=NA, .rows=iter_chunk_size)
  first_idx_mini <- length(iter_out_mini[!sapply(iter_out_mini, is.na)]) + 1
  
  
  # if (!exists("iter_out_mini")) iter_out_mini <- vector("list", length(chunks[[i]]))
  # first_idx_mini <- length(iter_out_mini[!sapply(iter_out_mini, is.null)]) + 1
  
  for (j in first_idx_mini:length(chunks[[i]])) {
    ## Pull settings for model to analyse from current row
    rowdf <- .models %>% slice(chunks[[i]][[j]])
    
    cat("Time is:", format(Sys.time(), "%a %d %b %Y %X"), "\n")
    cat("Grouping data for chunk", i, "row number", chunks[[i]][[j]], "\n")
    
    ## Check if data already grouped by same grouping vars, if so, skip grouping
    # if ((is.null(grouping_vars) == is.null(pull(rowdf, group_vars) %>% unlist()))
    if ((chunks[[i]][[j]] != 1 & is.null(grouping_vars) == is.null(pull(rowdf, group_vars) %>% unlist()))
        & all(grouping_vars == pull(rowdf, group_vars) %>% unlist())) {
      cat("This row's grouping vars", (pull(rowdf, group_vars) %>% unlist()), "\n")
      cat("Grouping vars still", grouping_vars, ", skipping grouping\n")
    } else {  # Otherwise, group data by grouping vars
      grouping_vars <- pull(rowdf, group_vars) %>% unlist()
      cat("Grouping data by:", grouping_vars, "\n")
      source("./code/auxiliary/ct_data_grouping.R")
    }
    
    ## Set environmental variables based on specified values
    gr_var <- pull(rowdf, gr_var) 
    direct_var <- pull(rowdf, direct_var)
    prediction_model <- pull(rowdf, model)
    model_data <- eval(parse(text=pull(rowdf, model_data)))
    test_data <- eval(parse(text=pull(rowdf, test_data)))
    plot_label <- pull(rowdf, plot_label)
    cat("Running row", chunks[[i]][[j]], "\n")
    # print(rowdf)
    # print(list(class(grouping_vars), class(gr_var), class(direct_var), class(prediction_model), class(model_data), class(test_data), class(plot_label)))
    
    ## Note: custom function needed for error handling
    model_gr_direct <- function() {
      tryCatch({
        ## Fit epidemic growth rate to specified model
        pred_gr <- model_data %>% 
          model_predict(dep_var=gr_var, 
                        model=prediction_model, 
                        family="gaussian", 
                        ncores=12, 
                        test_data=test_data, 
                        pr_in=!nc)
        
        ## Fit epidemic direction to specified model
        pred_direct <- model_data %>% 
          model_predict(dep_var=direct_var, 
                        model=prediction_model, 
                        family="binomial", 
                        ncores=12,
                        test_data=test_data)
        
        return(list(pred_gr=list(pred_gr), pred_direct=list(pred_direct)))
      }, 
      error = function(e) {
        cat("ERROR!!! in row", chunks[[i]][[j]], "\n")
        return(NULL)
      })
    }
    res <- model_gr_direct()
    if (!is.null(res)) {
      iter_out_mini[j,] <- res
    } else {
      error_tracker <- error_tracker %>% append(-chunks[[i]][[j]])
    }
    
    
    # 
    # ## Attempt to fit models, catching any errors
    # tryCatch({
    #   ## Fit epidemic growth rate to specified model
    #   pred_gr <- model_data %>% 
    #     model_predict(dep_var=gr_var, 
    #                   model=prediction_model, 
    #                   family="gaussian", 
    #                   ncores=12, 
    #                   test_data=test_data)
    #   
    #   ## Fit epidemic direction to specified model
    #   pred_direct <- model_data %>% 
    #     model_predict(dep_var=direct_var, 
    #                   model=prediction_model, 
    #                   family="bernoulli", 
    #                   ncores=12,
    #                   test_data=test_data)
    # 
    #   # iter_out_mini[j,] <- list(pred_gr=list(pred_gr), pred_direct=list(pred_direct))
    #   res <- list(pred_gr=list(pred_gr), pred_direct=list(pred_direct))
    #   
    #   # res <- (tibble(
    #   #   pred_gr = list(pred_gr),
    #   #   pred_direct = list(pred_direct)
    #   #   ))
    #   }, 
    #   error = function(e) {
    #     cat("ERROR!!! in row", chunks[[i]][[j]], "\n")
    #     # error_tracker <- error_tracker %>% append(-chunks[[i]][[j]])
    #     res <- "TESTING"
    #   }
    # )
    # if (!is.null(res)) {
    #   iter_out_mini[[j]] <- res  # Add model output to container
    # }
  }
  
  ## Save raw chunk results into backup RDS file and clear memory
  cat("SAVING CHUNK", i, "\n")
  iter_out_mini <- bind_rows(iter_out_mini)
  saveRDS(iter_out_mini, file=str_glue("{dat_folder}/temp/iter_out_mini_{i}.rds"))
  remove(iter_out_mini, first_idx_mini)
}

## Reload chunk RDS files and concatenate
for (i in first_idx:length(chunks)) {
  cat("RELOADING CHUNK", i, "\n")
  iter_out[[i]] <- readRDS(file=str_glue("{dat_folder}/temp/iter_out_mini_{i}.rds"))
}

if (!is.null(error_tracker)) .models <- .models %>% slice(error_tracker)
iter_out <- bind_rows(iter_out) %>% drop_na()
.models <- bind_cols(.models, iter_out)

## Back up and clean up
iter_out_bak <- iter_out
remove(iter_out, first_idx, error_tracker)
unlink(str_glue("{dat_folder}/temp/iter_out_mini_?.rds"))
