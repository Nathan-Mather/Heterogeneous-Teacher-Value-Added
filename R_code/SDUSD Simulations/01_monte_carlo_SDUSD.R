# =========================================================================== #
# ===================== Run the Monte Carlo Simulation ====================== #
# =========================================================================== #

# Clear data.
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")

# No scientific notation.
options(scipen = 999)

# Clean console history.
cat("\f")

# Define a notin function.
`%notin%` <- Negate(`%in%`)

# Parallel options.
do_parallel <- TRUE
ncores <- 20




# =========================================================================== #
# ========================= File paths and packages ========================= #
# =========================================================================== #

# Load packages.
library(boot)
library(broom)
library(data.table)
library(doParallel)
library(doRNG)
library(ggplot2)
library(Kendall)
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
  
}  else if (my_wd %like% "ricksmi") {
  # Base directory. 
  base_path <- "c:/Users/ricksmi/Desktop/vam/"
  
  # Path for data to save.
  out_data <- "c:/Users/ricksmi/Desktop/vam/data/mc/"
  
}  else {
  # Base directory.
  base_path <- "/home/tanner/Documents/Research/HeterogenousTeacherVA/Git/"
  
  # Path for data to save.
  out_data <- "/home/tanner/Documents/Research/HeterogenousTeacherVA/Output/"
  
}

# Load model_xwalk.
model_xwalk <- data.table(read_excel(paste0(base_path,
                                            "Heterogeneous-Teacher-Value-Added/R_code/model_xwalk_SDUSD.xlsx")))

# load teacher student xwalk
teacher_student_xwalk <- fread(paste0(base_path, "/Heterogeneous-Teacher-Value-Added/R_code/teacher_student_xwalk_fake.csv"))

# Load past crosswalk to get run number.
if (file.exists(paste0(out_data,'xwalk.csv'))) {
  old_xwalk <- data.table(read.csv(file=paste0(out_data,'xwalk.csv')))
  run_id <- max(old_xwalk$run_id) + 1
} else {
  run_id <- 1
}

# Load our functions now that we have a file path.
func_path <- "Heterogeneous-Teacher-Value-Added/R_code/functions/"
source(paste0(base_path, func_path, "mc_functions.R"))
source(paste0(base_path, func_path, "binned_va_function.R"))
source(paste0(base_path, func_path, "qtile_va_function.R"))
source(paste0(base_path, func_path, "np_hack_va_function.R"))
source(paste0(base_path, func_path, "simulate_test_data.R"))
source(paste0(base_path, func_path, "teacher_impact.R"))
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
  myCluster <- makeCluster(ncores, # number of cores to use
                           type = "FORK") # type of cluster
  # (Must be "PSOCK" on Windows)
}

if (do_parallel) {
  registerDoParallel(myCluster)
  registerDoRNG()
}




# =========================================================================== #
# ============================= Run Monte Carlo ============================= #
# =========================================================================== #

# Loop over xwalk to run this.
for(i in 1:nrow(model_xwalk)){
  
  # Set seed.
  set.seed(42)
  
  # Set parameters for this Monte Carlo run.
  # Run parameters.
  nsims                      <- model_xwalk[i, nsims]              # how many simulations to do
  single_run                 <- model_xwalk[i, single_run]         # whether or not to run just one draw and calculate bootstrap SE's
  p_npoints                  <- model_xwalk[i, npoints]            # number of grid points over which to calculate welfare added
  
  # Simulated data parameters.
  p_test_SEM                 <- model_xwalk[i, test_SEM]           # SEM of test
  p_impact_type              <- model_xwalk[i, impact_type]        # one of 'MLRN', 'MLR', 'MNoR', 'MNo', 'No'
  p_impact_function          <- model_xwalk[i, impact_function]    # which teacher impact function to use, and integer
  p_min_diff                 <- model_xwalk[i, min_diff]           # minimum impact difference between best and worst matched students
  p_max_diff                 <- model_xwalk[i, max_diff]           # maximum impact difference between best and worst matched students
  p_covariates               <- model_xwalk[i, covariates]         # whether or not to include covariates
  p_peer_effects             <- model_xwalk[i, peer_effects]       # whether or not to include peer effects
  p_stud_sorting             <- model_xwalk[i, stud_sorting]       # whether or not to include student sorting
  p_rho                      <- model_xwalk[i, rho]                # correlation between teacher and student ability
  p_ta_sd                    <- model_xwalk[i, ta_sd]              # teacher ability standard deviation
  p_sa_sd                    <- model_xwalk[i, sa_sd]              # student ability standard deviation
  p_tc_sd                    <- model_xwalk[i, tc_sd]              # teacher center standard deviation
  
  # Weight and estimation parameters.
  p_weight_type              <- model_xwalk[i, weight_type]        # style of social planner pareto weights
  p_method                   <- model_xwalk[i, method]             # method of estimation used
  p_lin_alpha                <- model_xwalk[i, lin_alpha]          # for linear weights
  p_pctile                   <- model_xwalk[i, pctile]             # for rawlsian
  p_weight_below             <- model_xwalk[i, weight_below ]      # for rawlsian
  p_weight_above             <- model_xwalk[i, weight_above]       # for rawlsian
  p_v_alpha                  <- model_xwalk[i, v_alpha]            # for v weights
  p_mrpctile                 <- model_xwalk[i, mrpctile]           # for mr weights
  p_mrdist                   <- model_xwalk[i, mrdist]             # for mr weights
  p_weighted_average         <- model_xwalk[i, weighted_average]   # whether or not to calculate a weighted average of standard and NP
  
  # Add in the run id.
  model_xwalk[i, run_id := .GlobalEnv$run_id]
  
  # Simulate teacher data 
  teacher_ability_xwalk <- simulate_teacher_ability(teacher_student_xwalk,
                                                    ta_sd                   = p_ta_sd,
                                                    school_cor              = 0,
                                                    tc_sd                   = p_tc_sd,
                                                    min_diff                = 0,
                                                    max_diff                = 0.1)
  
  
  
  
  # Get true WW impact.
  teacher_info <- welfare_statistic(in_dt           = teacher_ability_xwalk,
                                    type            = 'true',
                                    npoints         = p_npoints,
                                    weight_type     = p_weight_type,
                                    in_test_1       = r_dt$test_1,
                                    lin_alpha       = p_lin_alpha,
                                    pctile          = p_pctile,
                                    weight_below    = p_weight_below,
                                    weight_above    = p_weight_above,
                                    v_alpha         = p_v_alpha,
                                    mrpctile        = p_mrpctile,
                                    mrdist          = p_mrdist,
                                    impact_type     = p_impact_type,
                                    impact_function = p_impact_function)
  
  standard_info <- welfare_statistic(in_dt           = r_dt,
                                     type            = 'true',
                                     npoints         = p_npoints,
                                     weight_type     = 'equal',
                                     in_test_1       = r_dt$test_1,
                                     lin_alpha       = p_lin_alpha,
                                     pctile          = p_pctile,
                                     weight_below    = p_weight_below,
                                     weight_above    = p_weight_above,
                                     v_alpha         = p_v_alpha,
                                     mrpctile        = p_mrpctile,
                                     mrdist          = p_mrdist,
                                     impact_type     = p_impact_type,
                                     impact_function = p_impact_function)
  
  setnames(standard_info, old=c('true_welfare'), new=c('true_standard'))
  
  # Merge on the teacher center.
  teacher_info <- merge(teacher_info, unique(r_dt[, c('teacher_id',
                                                      'teacher_center')]),
                        'teacher_id')
  
  
  # Run just a single draw if specified, otherwise run the Monte Carlo.
  if (single_run == 1) {
    
    # Run the standard VA.
    va_tab1 <- standard_va_stat(in_dt               = r_dt,
                                single_run          = single_run)
    
    # Run the alternative VA and bootstrap standard errors.
    if (p_method %like% 'bin') {
      
      # Run the bootstrap.
      binned <- binned_va_stat(in_dt               = r_dt,
                               lin_alpha           = p_lin_alpha,
                               pctile              = p_pctile,
                               weight_above        = p_weight_above,
                               weight_below        = p_weight_below,
                               v_alpha             = p_v_alpha,
                               mrpctile            = p_mrpctile,
                               mrdist              = p_mrdist,
                               npoints             = p_npoints,
                               covariates          = p_covariates)
      
    } 
    
    if (p_method %like% 'quantile') {
      
      # Run the bootstrap.
      quantile <- quantile_va_stat(in_dt               = r_dt,
                                   weight_type         = p_weight_type, # Weighting parameters
                                   lin_alpha           = p_lin_alpha,
                                   pctile              = p_pctile,
                                   weight_above        = p_weight_above,
                                   weight_below        = p_weight_below,
                                   v_alpha             = p_v_alpha,
                                   mrpctile            = p_mrpctile,
                                   mrdist              = p_mrdist,
                                   npoints             = p_npoints,
                                   covariates          = p_covariates)
      
    } 
    
    if (p_method %like% 'np') {
      
      # Run the bootstrap.
      np <- np_hack_va_stat(in_dt               = r_dt,
                            weight_type         = p_weight_type, # Weighting parameters
                            lin_alpha           = p_lin_alpha,
                            pctile              = p_pctile,
                            weight_above        = p_weight_above,
                            weight_below        = p_weight_below,
                            v_alpha             = p_v_alpha,
                            mrpctile            = p_mrpctile,
                            mrdist              = p_mrdist,
                            npoints             = p_npoints,
                            covariates          = p_covariates,
                            weighted_average    = p_weighted_average)
    }
    
    # Add in quantile later, for now just use these.
    st_bin <- round(Kendall(va_tab1$mean_standard,
                            binned$alternative_welfare)$tau, digits=2)
    st_np <- round(Kendall(va_tab1$mean_standard,
                           np$alternative_welfare)$tau, digits=2)
    bin_np <- round(Kendall(binned$alternative_welfare,
                            np$alternative_welfare)$tau, digits=2)
    
    # Save as a data table.
    dt <- data.table(run_id = .GlobalEnv$run_id,
                     st_bin = .GlobalEnv$st_bin,
                     st_np  = .GlobalEnv$st_np,
                     bin_np = .GlobalEnv$bin_np)
    
    # Write to the file.
    write.table(dt,
                paste0(out_data, 'single_results.csv'),
                sep = ",",
                col.names = !file.exists(paste0(out_data,
                                                'single_results.csv')), 
                append = T,
                row.names = FALSE)
    
    next
    
  } else {
    # Run a Monte Carlo with the specified parameters. 
    if (do_parallel) {
      mc_res <- foreach(j = 1:nsims) %dopar% single_iteration_fun(in_dt               = r_dt,
                                                                  weight_type         = p_weight_type, # Weighting parameters
                                                                  method              = p_method,
                                                                  lin_alpha           = p_lin_alpha,
                                                                  pctile              = p_pctile,
                                                                  weight_above        = p_weight_above,
                                                                  weight_below        = p_weight_below,
                                                                  v_alpha             = p_v_alpha,
                                                                  mrpctile            = p_mrpctile, 
                                                                  mrdist              = p_mrdist,
                                                                  npoints             = p_npoints,
                                                                  n_teacher           = p_n_teacher, # Simulated data parameters
                                                                  n_stud_per_teacher  = p_n_stud_per_teacher,
                                                                  test_SEM            = p_test_SEM,
                                                                  teacher_va_epsilon  = p_teacher_va_epsilon,
                                                                  impact_type         = p_impact_type,
                                                                  impact_function     = p_impact_function,
                                                                  min_diff            = p_min_diff,
                                                                  max_diff            = p_max_diff,
                                                                  covariates          = p_covariates,
                                                                  peer_effects        = p_peer_effects,
                                                                  stud_sorting        = p_stud_sorting,
                                                                  rho                 = p_rho,
                                                                  ta_sd               = p_ta_sd,
                                                                  sa_sd               = p_sa_sd,
                                                                  tc_sd               = p_tc_sd,
                                                                  weighted_average    = p_weighted_average)
      
    } else {
      mc_res <- foreach(j = 1:nsims) %do% single_iteration_fun(in_dt               = r_dt,
                                                               weight_type         = p_weight_type, # Weighting parameters
                                                               method              = p_method,
                                                               lin_alpha           = p_lin_alpha,
                                                               pctile              = p_pctile,
                                                               weight_above        = p_weight_above,
                                                               weight_below        = p_weight_below,
                                                               v_alpha             = p_v_alpha,
                                                               mrpctile            = p_mrpctile, 
                                                               mrdist              = p_mrdist,
                                                               npoints             = p_npoints,
                                                               n_teacher           = p_n_teacher, # Simulated data parameters
                                                               n_stud_per_teacher  = p_n_stud_per_teacher,
                                                               test_SEM            = p_test_SEM,
                                                               teacher_va_epsilon  = p_teacher_va_epsilon,
                                                               impact_type         = p_impact_type,
                                                               impact_function     = p_impact_function,
                                                               min_diff            = p_min_diff,
                                                               max_diff            = p_max_diff,
                                                               covariates          = p_covariates,
                                                               peer_effects        = p_peer_effects,
                                                               stud_sorting        = p_stud_sorting,
                                                               rho                 = p_rho,
                                                               ta_sd               = p_ta_sd,
                                                               sa_sd               = p_sa_sd,
                                                               tc_sd               = p_tc_sd,
                                                               weighted_average    = p_weighted_average)
      
    }
    
    
    # Stack the results for this run.
    mc_res <- rbindlist(mc_res)
    
    # Get the mean estimates for each teacher. The by groups are all descriptive
    #  variables.
    mean_tab <- mc_res[, list(mean_standard = mean(standard_welfare),
                              sd_standard   = sd(standard_welfare)), teacher_id]
    
    if (p_method %like% 'bin') {
      mean_tab <- merge(mean_tab,
                        mc_res[, list(mean_bin = mean(binned_welfare),
                                      sd_bin   = sd(binned_welfare)), teacher_id],
                        'teacher_id')
    }
    
    if (p_method %like% 'np') {
      mean_tab <- merge(mean_tab,
                        mc_res[, list(mean_np = mean(np_welfare),
                                      sd_np   = sd(np_welfare)), teacher_id],
                        'teacher_id')
    }
    
    if (p_method %like% 'quantile') {
      mean_tab <- merge(mean_tab,
                        mc_res[, list(mean_quantile = mean(quantile_welfare),
                                      sd_quantile   = sd(quantile_welfare)),
                               teacher_id],
                        'teacher_id')
    }
    
    # Merge on teacher info.
    mean_tab <- merge(mean_tab, teacher_info, "teacher_id")
    mean_tab <- merge(mean_tab, standard_info, "teacher_id")
    
    # Add some more indicators.
    mean_tab[, run_id := run_id]
    mean_tab[, nsims := nsims]
    mean_tab[, date_time := date_time]
    
    # Ensure the output file has all columns and is setup correctly.
    print(paste0('Finished with simulation ', i))
    
    for (name in c('teacher_id',	'mean_standard',	'sd_standard',	'mean_bin',
                   'sd_bin',	'mean_quantile', 'sd_quantile', 'mean_np',
                   'sd_np',	'true_welfare',	'true_standard', 'teacher_center',
                   'run_id', 'nsims',	'date_time')) {
      if (name %notin% colnames(mean_tab)) {
        mean_tab[, .GlobalEnv$name := numeric()]
      }
    }
    
    mean_tab <- mean_tab[, c('teacher_id',	'mean_standard',	'sd_standard',
                             'mean_bin', 'sd_bin','mean_np', 'sd_np',
                             'mean_quantile', 'sd_quantile', 'true_welfare',
                             'true_standard', 'teacher_center',	'run_id', 'nsims',
                             'date_time')]
    
    # Write to the file.
    write.table(mean_tab,
                paste0(out_data, 'results.csv'), 
                sep = ",",
                col.names = !file.exists(paste0(out_data, 'results.csv')), 
                append = T,
                row.names = FALSE)
    
  } # Close the single run else clause.
  
  # Increment the run id.
  run_id <- run_id + 1
  
} # Close Monte Carlo loop.


# Let go of the processors.
if(do_parallel){
  stopCluster(myCluster)
}




# =========================================================================== #
# =============================== Save xwalk ================================ #
# =========================================================================== #

# Save a copy of the most recent xwalk so there are no mixups.
write.table(model_xwalk,
            paste0(out_data, 'xwalk.csv'),
            sep = ",",
            col.names = !file.exists(paste0(out_data, 'xwalk.csv')), 
            append = T,
            row.names = FALSE)
