# =========================================================================== #
# ===================== Run the Monte Carlo Simulation ====================== #
# =========================================================================== #

# Clear data.
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")

# No scientific notation. 
options(scipen = 999)

# Clean console history.
cat("\f")

# Parallel option.
do_parallel <- TRUE




# =========================================================================== #
# ========================= File paths and packages ========================= #
# =========================================================================== #

# Set seed. 
set.seed(42)

# Load packages.
library(broom)
library(data.table)
library(doParallel)
library(doRNG)
library(ggplot2)
library(Matrix)
library(matrixStats)
library(np)
library(quantreg)
library(readxl)
library(tidyr)

# Check users to set directory.
#  (NOTE TO MIKE, add something unique to your base working directory to detect
#   when it is your computer)
my_wd <- getwd()
if (my_wd %like% "Nmath_000") {
  # Base directory. 
  base_path <- "c:/Users/Nmath_000/Documents/Research/"
  
  # Path for data to save.
  out_data <- "c:/Users/Nmath_000/Documents/data/Value Added/mc_data/"
  
} else {
  # Base directory. 
  base_path <- "/home/tanner/Documents/Research/HeterogenousTeacherVA/Git/"
  
  # Path for data to save.
  out_data <- "/home/tanner/Documents/Research/HeterogenousTeacherVA/Output/"
  
}

# Load model_xwalk.
model_xwalk <- data.table(read_excel(paste0(base_path,
                  "Heterogeneous-Teacher-Value-Added/R_code/model_xwalk.xlsx")))

# Load our functions now that we have a file path.
func_path <- "Heterogeneous-Teacher-Value-Added/R_code/functions/"
source(paste0(base_path, func_path, "binned_va_function.R"))
source(paste0(base_path, func_path, "qtile_aggregation.R"))
source(paste0(base_path, func_path, "qtile_va_function.R"))
source(paste0(base_path, func_path, "simulate_test_data.R"))
source(paste0(base_path, func_path, "teacher_impact.R"))
source(paste0(base_path, func_path, "true_ww_impact.R"))
source(paste0(base_path, func_path, "weighting_functions.R"))
source(paste0(base_path, func_path, "welfare_statistic.R"))

# Get a time stamp. 
date_time <- gsub("-", "_", Sys.time())
date_time <- gsub(":", "_", date_time)
date_time <- gsub(" ", "__", date_time)




# =========================================================================== #
# =============================== Set options =============================== #
# =========================================================================== #

# Set parallel options.
if (my_wd %like% "Nmath_000") {
  myCluster <- makeCluster(4, # number of cores to use
                           type = "PSOCK") # type of cluster
                                           # (Must be "PSOCK" on Windows)
} else {
  myCluster <- makeCluster(20, # number of cores to use
                           type = "FORK") # type of cluster
                                          # (Must be "PSOCK" on Windows)
}

if (do_parallel) {
  registerDoParallel(myCluster)
  registerDoRNG()
}




# =========================================================================== #
# ======================== Single Iteration Function ======================== #
# =========================================================================== #

# Run inside of MC function up until this single_iteration_fun to get debug
#  parms.

# in_dt              = r_dt
# weight_type        = p_weight_type
# method             = p_method
# lin_alpha          = p_lin_alpha
# pctile             = p_pctile
# weight_above       = p_weight_above
# weight_below       = p_weight_below
# v_alpha            = p_v_alpha
# mrpctile           = p_mrpctile
# mrdist             = p_mrpctile
# npoints            = p_npoints
# n_teacher          = p_n_teacher
# n_stud_per_teacher = p_n_stud_per_teacher
# test_SEM           = p_test_SEM
# teacher_va_epsilon = p_teacher_va_epsilon
# impact_type        = p_impact_type
# impact_function    = p_impact_function
# max_diff           = p_max_diff
# covariates         = p_covariates
# peer_effects       = p_peer_effects
# stud_sorting       = p_stud_sorting
# rho                = p_rho
# ta_sd              = p_ta_sd
# sa_sd              = p_sa_sd

# Define the function for a single iteration of the Monte Carlo.
single_iteration_fun <- function(in_dt              = NULL,
                                 weight_type        = NULL,
                                 method             = NULL, 
                                 lin_alpha          = NULL,
                                 pctile             = NULL,
                                 weight_below       = NULL,
                                 weight_above       = NULL,
                                 v_alpha            = NULL,
                                 mrpctile           = NULL, 
                                 mrdist             = NULL,
                                 npoints            = NULL,
                                 n_teacher          = NULL,
                                 n_stud_per_teacher = NULL,
                                 test_SEM           = NULL,
                                 teacher_va_epsilon = NULL,
                                 impact_type        = NULL,
                                 impact_function    = NULL,
                                 max_diff           = NULL,
                                 covariates         = NULL,
                                 peer_effects       = NULL,
                                 stud_sorting       = NULL,
                                 rho                = NULL,
                                 ta_sd              = NULL,
                                 sa_sd              = NULL){

  # I need this for it to work on windows clusters since libraries are not
  #  loaded on every cluster.
  require(data.table)
  
  # Resample the student data.
  in_dt <- simulate_test_data(n_teacher          = n_teacher,
                              n_stud_per_teacher = n_stud_per_teacher,
                              test_SEM           = test_SEM,
                              teacher_va_epsilon = teacher_va_epsilon,
                              impact_type        = impact_type,
                              impact_function    = impact_function,
                              max_diff           = max_diff,
                              teacher_dt         = in_dt[, c("teacher_id",
                                                             "teacher_ability",
                                                             "teacher_center",
                                                             "teacher_max")],
                              covariates         = covariates,
                              peer_effects       = peer_effects,
                              stud_sorting       = stud_sorting,
                              rho                = rho,
                              ta_sd              = ta_sd,
                              sa_sd              = sa_sd)
  
  
  # Run the standard VA.
  if (covariates == 0) {
    va_out1 <- lm(test_2 ~ test_1 + teacher_id - 1, data = in_dt)
  } else {
    va_out1 <- lm(test_2 ~ test_1 + teacher_id + school_av_test + stud_sex +
                    stud_frpl + stud_att - 1, data = in_dt) ###### Check this
  }
  
  # Clean results.
  va_tab1 <- data.table(broom::tidy(va_out1))
  va_tab1[, teacher_id := gsub("teacher_id", "", term)]
  
  # Return just the estimates
  va_tab1 <- va_tab1[term %like% "teacher_id", c("teacher_id", "estimate")]
  
  # Get the welfare statistic for the standard VA.
  va_tab1 <- welfare_statistic(in_dt           = in_dt,
                               output          = va_tab1,
                               type            = 'standard', 
                               npoints         = npoints,
                               weight_type     = weight_type,
                               in_test_1       = in_dt$test_1,
                               pctile          = pctile,
                               weight_below    = weight_above,
                               weight_above    = weight_below,
                               v_alpha         = v_alpha,
                               mrpctile        = mrpctile, 
                               mrdist          = mrpctile)

  
  # Check method option.
  if (method=="bin") {
    # Estimate the binned VA.
    if (covariates == 0) {
      output <- binned_va(in_data = in_dt)
    } else {
      output <- binned_va(in_data = in_dt,
                          formula = paste0('test_2 ~ test_1 + teacher_id + ',
                                           'categories + teacher_id*categories',
                                           ' + school_av_test + stud_sex + ',
                                           'stud_frpl + stud_att - 1'))
    }
    
    # Fill in missing estimates with 0 (so that we end up with teacher_ability).
    output <- complete(output, teacher_id, category)
    for (i in seq_along(output)) set(output, i=which(is.na(output[[i]])), j=i,
                                     value=0)
    
    # Calculate the welfare statistic for each teacher.
    output <- welfare_statistic(in_dt           = in_dt,
                                output          = output,
                                type            = 'bin', 
                                npoints         = npoints,
                                weight_type     = weight_type,
                                in_test_1       = in_dt$test_1,
                                pctile          = pctile,
                                weight_below    = weight_above,
                                weight_above    = weight_below,
                                v_alpha         = v_alpha,
                                mrpctile        = mrpctile, 
                                mrdist          = mrpctile)
  }

  
  if (method=="semip") {
    # put implementation here. Call output or rename that object everywhere 
    # not really a good name anyway 
    
    output <- semip_va(in_data = in_dt )
  }
  
  
  if (method=="qtle") {
    # Run quantile regression and get estimates for a grid of tau values.
    qtile_res <- qtilep_va(in_data       = in_dt,
                           in_teacher_id = "teacher_id",
                           in_pre_test   = "test_1",
                           in_post_test  = "test_2",
                           ptle          = seq(.02, .98, by=.04))
    
    
    # Calculate the welfare statistic for each teacher.
    output <- welfare_statistic(in_dt           = in_dt,
                                output          = qtile_res,
                                type            = 'quant', 
                                npoints         = npoints,
                                weight_type     = weight_type,
                                in_test_1       = in_dt$test_1,
                                pctile          = pctile,
                                weight_below    = weight_above,
                                weight_above    = weight_below,
                                v_alpha         = v_alpha,
                                mrpctile        = mrpctile, 
                                mrdist          = mrpctile)
 
  }
  
  
  # Merge on the standard VA
  va_tab2 <- merge(va_tab1, output, "teacher_id")
  
  # Return the estimates.
  return(va_tab2)

}




# =========================================================================== #
# ============================= run Monte Carlo ============================= #
# =========================================================================== #

# Loop over xwalk to run this. 
for(i in 1:nrow(model_xwalk)){

  # Set parameters for this Monte Carlo run.
  # Run parameters.
  run_id                     <- model_xwalk[i, run_id]             # keep track of what run it is 
  nsims                      <- model_xwalk[i, nsims]              # how many simulations to do 
  p_npoints                  <- model_xwalk[i, npoints]            # number of grid points over which to calculate welfare added
  
  # Simulated data parameters.
  p_n_teacher                <- model_xwalk[i, n_teacher]          # number of teachers 
  p_n_stud_per_teacher       <- model_xwalk[i, n_stud_per_teacher] # students per teacher
  p_test_SEM                 <- model_xwalk[i, test_SEM]           # SEM of test
  p_teacher_va_epsilon       <- model_xwalk[i, teacher_va_epsilon] # SD of noise on teacher impact 
  p_impact_type              <- model_xwalk[i, impact_type]        # one of 'MLRN', 'MLR', 'MNoR', 'MNo', 'No'
  p_impact_function          <- model_xwalk[i, impact_function]    # which teacher impact function to use, and integer
  p_max_diff                 <- model_xwalk[i, max_diff]           # maximum impact difference between best and worst matched students
  p_covariates               <- model_xwalk[i, covariates]         # whether or not to include covariates
  p_peer_effects             <- model_xwalk[i, peer_effects]       # whether or not to include peer effects
  p_stud_sorting             <- model_xwalk[i, stud_sorting]       # whether or not to include student sorting
  p_rho                      <- model_xwalk[i, rho]                # correlation between teacher and student ability
  p_ta_sd                    <- model_xwalk[i, ta_sd]              # teacher ability standard deviation
  p_sa_sd                    <- model_xwalk[i, sa_sd]              # student ability standard deviation
  
  # Weight and estimation parameters.
  p_weight_type              <- model_xwalk[i, weight_type]        # style of social planner pareto weights
  p_method                   <- model_xwalk[i, method]             # method of estimation used 
  p_lin_alpha                <- model_xwalk[i, lin_alpha]          # For linear weights
  p_pctile                   <- model_xwalk[i, pctile]             # for rawlsian 
  p_weight_below             <- model_xwalk[i, weight_below ]      # for rawlsian 
  p_weight_above             <- model_xwalk[i, weight_above]       # for rawlsian 
  p_v_alpha                  <- model_xwalk[i, v_alpha]            # For v weights
  p_mrpctile                 <- model_xwalk[i, mrpctile]           # For mr weights
  p_mrdist                   <- model_xwalk[i, mrdist]             # for mr weights
  
  
  # Simulate initial data.
  r_dt <- simulate_test_data(n_teacher          = p_n_teacher,
                             n_stud_per_teacher = p_n_stud_per_teacher,
                             test_SEM           = p_test_SEM,
                             teacher_va_epsilon = p_teacher_va_epsilon,
                             impact_type        = p_impact_type,
                             impact_function    = p_impact_function,
                             max_diff           = p_max_diff,
                             covariates         = p_covariates,
                             peer_effects       = p_peer_effects,
                             stud_sorting       = p_stud_sorting,
                             rho                = p_rho,
                             ta_sd              = p_ta_sd,
                             sa_sd              = p_sa_sd)

  
  # Get true WW impact.
  teacher_info <- welfare_statistic(in_dt           = r_dt,
                                    type            = 'true', 
                                    npoints         = p_npoints,
                                    weight_type     = p_weight_type,
                                    in_test_1       = r_dt$test_1,
                                    pctile          = p_pctile,
                                    weight_below    = p_weight_below,
                                    weight_above    = p_weight_above,
                                    v_alpha         = p_v_alpha,
                                    mrpctile        = p_mrpctile, 
                                    mrdist          = p_mrpctile,
                                    impact_type     = p_impact_type,
                                    impact_function = p_impact_function)

  
  # Run a Monte Carlo with the specified parameters. 
  if (do_parallel) {
    mc_res <- foreach(j = 1:nsims) %dopar% single_iteration_fun(in_dt              = r_dt,
                                                                weight_type        = p_weight_type, # Weighting parameters
                                                                method             = p_method,
                                                                lin_alpha          = p_lin_alpha,
                                                                pctile             = p_pctile,
                                                                weight_above       = p_weight_above,
                                                                weight_below       = p_weight_below,
                                                                v_alpha            = p_v_alpha,
                                                                mrpctile           = p_mrpctile, 
                                                                mrdist             = p_mrpctile,
                                                                npoints            = p_npoints,
                                                                n_teacher          = p_n_teacher, # Simulated data parameters
                                                                n_stud_per_teacher = p_n_stud_per_teacher,
                                                                test_SEM           = p_test_SEM,
                                                                teacher_va_epsilon = p_teacher_va_epsilon,
                                                                impact_type        = p_impact_type,
                                                                impact_function    = p_impact_function,
                                                                max_diff           = p_max_diff,
                                                                covariates         = p_covariates,
                                                                peer_effects       = p_peer_effects,
                                                                stud_sorting       = p_stud_sorting,
                                                                rho                = p_rho,
                                                                ta_sd              = p_ta_sd,
                                                                sa_sd              = p_sa_sd)
    
  } else {
    mc_res <- foreach(j = 1:nsims) %do% single_iteration_fun(in_dt              = r_dt,
                                                             weight_type        = p_weight_type, # Weighting parameters
                                                             method             = p_method,
                                                             lin_alpha          = p_lin_alpha,
                                                             pctile             = p_pctile,
                                                             weight_above       = p_weight_above,
                                                             weight_below       = p_weight_below,
                                                             v_alpha            = p_v_alpha,
                                                             mrpctile           = p_mrpctile, 
                                                             mrdist             = p_mrpctile,
                                                             npoints            = p_npoints,
                                                             n_teacher          = p_n_teacher, # Simulated data parameters
                                                             n_stud_per_teacher = p_n_stud_per_teacher,
                                                             test_SEM           = p_test_SEM,
                                                             teacher_va_epsilon = p_teacher_va_epsilon,
                                                             impact_type        = p_impact_type,
                                                             impact_function    = p_impact_function,
                                                             max_diff           = p_max_diff,
                                                             covariates         = p_covariates,
                                                             peer_effects       = p_peer_effects,
                                                             stud_sorting       = p_stud_sorting,
                                                             rho                = p_rho,
                                                             ta_sd              = p_ta_sd,
                                                             sa_sd              = p_sa_sd)
        
  }

  
# Stack the results for this run.
mc_res <- rbindlist(mc_res)

# Get the mean estimates for each teacher. The by groups are all descriptive
#  variables.
mean_tab <- mc_res[, list(mean_standard = mean(standard_welfare),
                          sd_standard   = sd(standard_welfare),
                          mean_ww = mean(alternative_welfare),
                          sd_ww   = sd(alternative_welfare)),
                   by = teacher_id]

# Add some more indicators.
mean_tab[, run_id := run_id]
mean_tab[, nsims := nsims]

# Merge on teacher info.
mean_tab <- merge(mean_tab, teacher_info, "teacher_id")

# Write to the file.
print(paste0('Finished with simulation ', i))
write.table(mean_tab, paste0(out_data, '/', "mc_results_", date_time, ".csv"), 
            sep = ",", col.names = !file.exists(paste0(out_data, '/',
                                                       "mc_results_",
                                                       date_time, ".csv")), 
            append = T, row.names = FALSE)

} # Close Monte Carlo loop.


# Let go of the processors.
if(do_parallel){
  stopCluster(myCluster)
}




# =========================================================================== #
# =============================== save xwalk ================================ #
# =========================================================================== #

# Save a copy of the most recent xwalk so there are no mixups.
write.csv(model_xwalk, paste0(out_data, '/', "mc_xwalk_", date_time,".csv" ), row.names = FALSE)
