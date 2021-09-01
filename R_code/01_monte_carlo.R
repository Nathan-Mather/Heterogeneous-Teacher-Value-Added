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
  out_data <- "C:/Users/Nmath_000/Documents/Research/Value_added_local/results/"
  
  # path to save qc 
  out_qc <- paste0(out_data, "/qc/")
  
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


# create int directory if it idoesnt exist already 
if(!file.exists(paste0(out_data, "/int_results"))){
  dir.create(paste0(out_data, "/int_results"))
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
source(paste0(base_path, func_path, "simulate_sdusd_data.R"))
source(paste0(base_path, func_path, "quality_control_functions.R"))


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

#=====================================#
# ==== check for existing results ====
#=====================================#

if (file.exists(paste0(out_data,'xwalk.csv'))) {
  
  old_xwalk <- fread( paste0(out_data, 'xwalk.csv'))
  setnames(old_xwalk, "done_flag", "done_flag_old")
  
  # subset to only done runs
  old_xwalk <- old_xwalk[done_flag_old == 1]
  
  # mark old and new xwalks 
  model_xwalk[, flag_new := 1]
  old_xwalk[, flag_old := 1]
  
  old_xwalk[, impact_function := as.character(impact_function)]
  
  # merge old and new
  merged_cols <- setdiff(colnames(model_xwalk), c("done_flag", "flag_new"))
  model_xwalk <- merge(model_xwalk, old_xwalk,merged_cols, all = TRUE)
  
  # check for completed runs we don't need to duplicate 
  model_xwalk[done_flag_old == 1, done_flag := 1]
  model_xwalk[, done_flag_old := NULL, ]
  
  
  # check for runs we no longer want and should remorve 
  to_remove <- model_xwalk[flag_old == 1 & is.na(flag_new), run_id]
  
  # remove runs that are no longer in xwalk 
  for(k in to_remove){
    
    # remove results 
    unlink(paste0(out_data, "/int_results/results_", k, ".csv"))
    
    # remove qc 
    unlink(paste0(out_data, "/qc/run_", k ),
           recursive = TRUE)
  }
  
  # Now subset to only the ones from the new xwalk 
  model_xwalk <- model_xwalk[flag_new == 1]
  
  # get rid of extra variables I created 
  model_xwalk[,c("flag_new", "flag_old") := NULL]
  

  # get list of run id's to use 
  run_id_list <- 1:nrow(model_xwalk)
  run_id_list <- setdiff(run_id_list, model_xwalk$run_id)
  
  # initialize run id counter 
  run_id_counter <- 1
  
  
} else {
  run_id_list <- 1:nrow(model_xwalk)
  run_id_counter <- 1
}
colnames(model_xwalk)
# set model xwalk order 
setorder(model_xwalk, "teacher_student_xwalk", "impact_type", "impact_function", "weight_type" ,"max_diff")

# =========================================================================== #
# ============================= Run Monte Carlo ============================= #
# =========================================================================== #

# Loop over xwalk to run this.
for(i in 1:nrow(model_xwalk)){
  
  # see if it has been run already 
  done_flag <- model_xwalk[i, done_flag]  
  
  if(done_flag == 1){
    next()
  }
  
  # get run id. There is a list of unused ID's and a a counter
  # for which ID we are on (does not correspond to I because of skipped rows)
  run_id_i <- run_id_list[[run_id_counter]]
  run_id_counter <- run_id_counter + 1
  
  
  #====================#
  # ==== get parms ====
  #====================#
  
  # Set seed.
  set.seed(42)
  
  # Set parameters for this Monte Carlo run.
  # Run parameters.
  qc_flag                    <- model_xwalk[i, qc_flag]            # should we create quality control output? (runs slower)
  nsims                      <- model_xwalk[i, nsims]              # how many simulations to do
  ts_xwalk_name              <- model_xwalk[i, teacher_student_xwalk]  # which teacher stuent xwalk file to use 
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
  p_tc_sd                    <- model_xwalk[i, tc_sd]              # teacher center standard deviation
  p_n_cohorts                <- model_xwalk[i, n_cohorts]          # number of cohorts per teacher 
  p_pretest_coef             <- model_xwalk[i, pretest_coef]       #coefficent on student pretest/ability 
  
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
  p_num_cats                 <- model_xwalk[i, num_cats]          # number of bins for binned estimator 
  
  
  
  # Add in the run id.
  model_xwalk[i, run_id := run_id_i]
  
  

  # load teacher student xwalk 
  teacher_student_xwalk <- fread(paste0(base_path, "Heterogeneous-Teacher-Value-Added/R_code/", ts_xwalk_name, ".csv"))
  
  #============================#
  # ==== simulate teachers ====
  #============================#
  
  # Simulate teacher data #note: loacted in funcitons/simulate_sdusd_data 
  teacher_ability_xwalk <- simulate_teacher_ability(teacher_student_xwalk = teacher_student_xwalk,
                                                    ta_sd                   = p_ta_sd,
                                                    school_cor              = 0,
                                                    tc_sd                   = p_tc_sd,
                                                    min_diff                = p_min_diff,
                                                    max_diff                = p_max_diff)
  
    #=================#
    # ==== run qc ====
    #=================#
    if(qc_flag == 1){
      teacher_xwalk_qc(in_teacher_ability_xwalk = teacher_ability_xwalk,
                       run_id                   = run_id_i,
                       out_path                 =out_qc)
    }
  #====================#
  # ==== get truth ====
  #====================#
  
  # Get true WW impact.
  teacher_info <- welfare_statistic(in_dt           = teacher_ability_xwalk,
                                    type            = 'true',
                                    npoints         = p_npoints,
                                    weight_type     = p_weight_type,
                                    in_test_1       = NULL,
                                    lin_alpha       = p_lin_alpha,
                                    pctile          = p_pctile,
                                    weight_below    = p_weight_below,
                                    weight_above    = p_weight_above,
                                    v_alpha         = p_v_alpha,
                                    mrpctile        = p_mrpctile,
                                    mrdist          = p_mrdist,
                                    impact_type     = p_impact_type,
                                    impact_function = p_impact_function,
                                    qc_flag         = qc_flag,
                                    out_qc_path     = out_qc,
                                    run_id_qc       = run_id_i)
  
  standard_info <- welfare_statistic(in_dt           = teacher_ability_xwalk,
                                     type            = 'true',
                                     npoints         = p_npoints,
                                     weight_type     = 'equal',
                                     in_test_1       = NULL,
                                     lin_alpha       = p_lin_alpha,
                                     pctile          = p_pctile,
                                     weight_below    = p_weight_below,
                                     weight_above    = p_weight_above,
                                     v_alpha         = p_v_alpha,
                                     mrpctile        = p_mrpctile,
                                     mrdist          = p_mrdist,
                                     impact_type     = p_impact_type,
                                     impact_function = p_impact_function,
                                     qc_flag         = 0)
  

  setnames(standard_info, old=c('true_welfare'), new=c('true_standard'))
  
  # Merge on the teacher center.
  teacher_info <- merge(teacher_info, unique(teacher_ability_xwalk[, c('teacher_id',
                                                                       'teacher_center')]),
                        'teacher_id')
  
  
  #==========================#
  # ==== Run simulations ====
  #==========================#
  
  
  
  # Run a Monte Carlo with the specified parameters. 
  if (do_parallel) {
    mc_res <- foreach(j = 1:nsims) %dopar%  single_iteration_fun( teacher_ability_xwalk   = teacher_ability_xwalk,
                                                                        n_cohorts               = p_n_cohorts,
                                                                        pretest_coef            = p_pretest_coef,
                                                                        weight_type             = p_weight_type,
                                                                        method                  = p_method,
                                                                        num_cats                = p_num_cats,
                                                                        lin_alpha               = p_lin_alpha,
                                                                        pctile                  = p_pctile,
                                                                        weight_below            = p_weight_below,
                                                                        weight_above            = p_weight_above,
                                                                        v_alpha                 = p_v_alpha,
                                                                        mrpctile                = p_mrpctile, 
                                                                        mrdist                  = p_mrdist,
                                                                        npoints                 = p_npoints,
                                                                        test_SEM                = p_test_SEM,
                                                                        impact_type             = p_impact_type,
                                                                        impact_function         = p_impact_function,
                                                                        covariates              = p_covariates,
                                                                        peer_effects            = p_peer_effects,
                                                                        stud_sorting            = p_stud_sorting,
                                                                        rho                     = p_rho,
                                                                        weighted_average        = p_weighted_average)

  } else {
    mc_res <- foreach(j = 1:nsims) %do%  single_iteration_fun( teacher_ability_xwalk   = teacher_ability_xwalk,
                                                                     n_cohorts               = p_n_cohorts,
                                                                     pretest_coef            = p_pretest_coef,
                                                                     weight_type             = p_weight_type,
                                                                     method                  = p_method,
                                                                     num_cats                = p_num_cats,
                                                                     lin_alpha               = p_lin_alpha,
                                                                     pctile                  = p_pctile,
                                                                     weight_below            = p_weight_below,
                                                                     weight_above            = p_weight_above,
                                                                     v_alpha                 = p_v_alpha,
                                                                     mrpctile                = p_mrpctile, 
                                                                     mrdist                  = p_mrdist,
                                                                     npoints                 = p_npoints,
                                                                     test_SEM                = p_test_SEM,
                                                                     impact_type             = p_impact_type,
                                                                     impact_function         = p_impact_function,
                                                                     covariates              = p_covariates,
                                                                     peer_effects            = p_peer_effects,
                                                                     stud_sorting            = p_stud_sorting,
                                                                     rho                     = p_rho,
                                                                     weighted_average        = p_weighted_average)
    
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
  mean_tab[, run_id := run_id_i]
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
  write.csv(mean_tab, 
       file = paste0(out_data, "int_results/", "results_", run_id_i, ".csv"),
       row.names = FALSE)
  
  
  # mark this row as done 
  model_xwalk[i, done_flag :=1]
  
  
  # Save a copy of the most recent xwalk so there are no mixups.
  write.csv(model_xwalk,
            paste0(out_data, '/xwalk.csv'),
            row.names = FALSE)
  
 
  
} # Close Monte Carlo loop.


# Let go of the processors.
if(do_parallel){
  stopCluster(myCluster)
}


#========================#
# ==== stack results ====
#========================#

# load in results, stack them, save as one 
file_paths <- list.files(paste0(out_data, "int_results"),
                         full.names = TRUE)

results <- rbindlist(lapply(file_paths, fread))

# save results 
write.csv(results,
          paste0(out_data, 
                 "results.csv"))


# =========================================================================== #
# =============================== Save xwalk ================================ #
# =========================================================================== #

# Save a copy of the most recent xwalk so there are no mixups.
write.csv(model_xwalk,
          paste0(out_data, '/xwalk.csv'),
          row.names = FALSE)



