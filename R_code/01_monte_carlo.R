# =========================================================================== #
# ===================== Run the Monte Carlo Simulation ====================== #
# =========================================================================== #

# Clear data.
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")

# No scientific notation. 
options(scipen = 999)

# Clean console history.
cat("\f")

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
                  "Heterogeneous-Teacher-Value-Added/R_code/model_xwalk.xlsx")))

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
  run_id                     <- model_xwalk[i, run_id]             # keep track of what run it is 
  nsims                      <- model_xwalk[i, nsims]              # how many simulations to do
  single_run                 <- model_xwalk[i, single_run]         # whether or not to run just one draw and calculate bootstrap SE's
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
  p_lin_alpha                <- model_xwalk[i, lin_alpha]          # for linear weights
  p_pctile                   <- model_xwalk[i, pctile]             # for rawlsian 
  p_weight_below             <- model_xwalk[i, weight_below ]      # for rawlsian 
  p_weight_above             <- model_xwalk[i, weight_above]       # for rawlsian 
  p_v_alpha                  <- model_xwalk[i, v_alpha]            # for v weights
  p_mrpctile                 <- model_xwalk[i, mrpctile]           # for mr weights
  p_mrdist                   <- model_xwalk[i, mrdist]             # for mr weights
  p_weighted_average          <- model_xwalk[i, weighted_average]   # whether or not to calculate a weighted average of standard and NP
  
  
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
                                    mrdist          = p_mrdist,
                                    impact_type     = p_impact_type,
                                    impact_function = p_impact_function)

  
  # Run just a single draw if specified, otherwise run the Monte Carlo.
  if (single_run == 1) {

    # Run the standard VA.
    va_tab1 <- standard_va_stat(in_dt              = r_dt,
                                single_run         = single_run,
                                weight_type        = p_weight_type, # Weighting parameters
                                method             = p_method,
                                lin_alpha          = p_lin_alpha,
                                pctile             = p_pctile,
                                weight_above       = p_weight_above,
                                weight_below       = p_weight_below,
                                v_alpha            = p_v_alpha,
                                mrpctile           = p_mrpctile, 
                                mrdist             = p_mrdist,
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
    
    # Run the alternative VA and bootstrap standard errors.
    if (p_method == 'bin') {

      # Run the bootstrap.
      boot_res <- boot(data               = r_dt,
                       statistic          = binned_va_stat,
                       R                  = 99,
                       boot               = single_run,
                       weight_type        = p_weight_type, # Weighting parameters
                       method             = p_method,
                       lin_alpha          = p_lin_alpha,
                       pctile             = p_pctile,
                       weight_above       = p_weight_above,
                       weight_below       = p_weight_below,
                       v_alpha            = p_v_alpha,
                       mrpctile           = p_mrpctile, 
                       mrdist             = p_mrdist,
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
      
    } else if (p_method == 'qtle') {
      
      # Run the bootstrap.
      boot_res <- boot(data               = r_dt,
                       statistic          = quantile_va_stat,
                       R                  = 99,
                       boot               = single_run,
                       weight_type        = p_weight_type, # Weighting parameters
                       method             = p_method,
                       lin_alpha          = p_lin_alpha,
                       pctile             = p_pctile,
                       weight_above       = p_weight_above,
                       weight_below       = p_weight_below,
                       v_alpha            = p_v_alpha,
                       mrpctile           = p_mrpctile, 
                       mrdist             = p_mrdist,
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
      
    } else if (p_method == 'semip') {
      
    }
    
  } else {
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
                                                                  mrdist             = p_mrdist,
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
                                                                  sa_sd              = p_sa_sd,
                                                                  weighted_average   = p_weighted_average)
      
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
                                                               mrdist             = p_mrdist,
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
                                                               sa_sd              = p_sa_sd,
                                                               weighted_average   = p_weighted_average)
          
    }
  
    
  # Stack the results for this run.
  mc_res <- rbindlist(mc_res)
  
  # Get the mean estimates for each teacher. The by groups are all descriptive
  #  variables.
  mean_tab <- mc_res[, list(mean_standard = mean(standard_welfare),
                            sd_standard   = sd(standard_welfare),
                            mean_ww       = mean(alternative_welfare),
                            sd_ww         = sd(alternative_welfare)),
                            by            = teacher_id]
  
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

  } # Close the single run else clause.
} # Close Monte Carlo loop.


# Let go of the processors.
if(do_parallel){
  stopCluster(myCluster)
}




# =========================================================================== #
# =============================== Save xwalk ================================ #
# =========================================================================== #

# Save a copy of the most recent xwalk so there are no mixups.
write.csv(model_xwalk, paste0(out_data, '/', "mc_xwalk_", date_time,".csv" ), row.names = FALSE)
