# ==================================== #
# == Run the Monte Carlo Simulation == #
# ==================================== #

# clear data.
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
# no scientific notation 
options(scipen = 999)
# clean console history 
cat("\f")

#==================================#
# ==== File paths and packages ====
#==================================#

# set seeds 
set.seed(42)

# load packages 
library(data.table)
library(broom)
library(Matrix)
library(ggplot2)
library(doParallel)
library(matrixStats)
library(doRNG)


# check users. (NOTE TO MIKE, add something unique to your base working directory to detect when it is your computer)
my_wd <- getwd()
if(my_wd %like% "Nmath_000"){
  # base directory 
  base_path <- "c:/Users/Nmath_000/Documents/Research/"
  
  # path for data to save
  out_data <- "c:/Users/Nmath_000/Documents/data/Value Added/"
  
}else{
  # base directory 
  base_path <- "~/Documents/Research/HeterogenousTeacherVA/Git/"
  
  # path for data to save 
  out_data <- "~/Documents/Research/HeterogenousTeacherVA/Git/Heterogeneous-Teacher-Value-Added/R_code"
  
}

# load our functions now that we have a file path 
func_path <- "Heterogeneous-Teacher-Value-Added/R_code/functions/"
source(paste0(base_path, func_path, "simulate_test_data.R"))
source(paste0(base_path, func_path, "ww_va_function.R"))
source(paste0(base_path, func_path, "weighting_functions.R"))


#======================#
# ==== set options ====
#======================#

# Set parallel options
if(my_wd %like% "Nmath_000"){
  myCluster <- makeCluster(4, # number of cores to use
                           type = "PSOCK") # type of cluster (Must be "PSOCK" on Windows)
}else{
  myCluster <- makeCluster(20, # number of cores to use
                           type = "FORK") # type of cluster (Must be "PSOCK" on Windows)
}
registerDoParallel(myCluster)
registerDoRNG()

#====================================#
# ==== Single Iteration Function ====
#====================================#


# # Single iteration funciton inputs for debug
# in_dt = r_dt # go down and run the function to get r_dt so you can debug
# weight_type <-"rawlsian" # "linear" or "rawlsian" or "equal" or "v" or "mr"
# lin_alpha = 2 # For linear weights
# pctile = .4 # For rawlsian weights
# v_alpha = 1 # For v weights
# mrpctile = .3 # For mr weights
# mrdist = .2 # for mr weights
# method =  "wls" # "wls" or "semip" or "qtle"

# this is what a single run of the monte carlo will do 
single_iteration_fun <- function(in_dt        = NULL,
                                 weight_type  = NULL,
                                 method       = NULL, 
                                 lin_alpha    = NULL,
                                 pctile       = NULL,
                                 v_alpha      = NULL,
                                 mrpctile     = NULL, 
                                 mrdist       = NULL){
  
  # I need this for it to work on windows clusters since libraries arent loaded  on every cluseter
  require(data.table)
  
  # Resample the student data
  in_dt <- simulate_test_data(teacher_dt = in_dt[, c( "teacher_id", "teacher_ability", "teacher_center")])
  
  # First run the standard VA 
  # run regression 
  va_out1 <- lm(test_2 ~ test_1 + teacher_id - 1, data = in_dt)
  
  # clean results 
  va_tab1 <- data.table(broom::tidy(va_out1))
  va_tab1[, teacher_id := gsub("teacher_id", "", term)]
  
  # Return just the estimates
  va_tab1 <- va_tab1[term %like% "teacher_id", c("teacher_id", "estimate")]
  
  # Now run the weighted VA
  # Generate the weights
  in_dt[, (weight_type) := ww_general_fun(weight_type  = weight_type,
                                          in_test_1    = test_1,
                                          lin_alpha    = lin_alpha,
                                          pctile       = pctile,
                                          v_alpha      = v_alpha,
                                          mrpctile     = mrpctile, 
                                          mrdist       = mrdist)]
  
  # check method option
  if(method=="wls"){
    # Estimate the weighted VA
    ww_tab1 <- ww_va(in_data = in_dt, in_weights = weight_type)
  }
  
  if(method=="semip"){
    # put implimentation here. Call output ww_tab1 or rename that object everywhere 
    # not really a good name anyway 
  }
  
  if(method=="qtle"){
    # put implimentation here. Call output ww_tab1 or rename that object everywhere 
    # not really a good name anyway 
  }
  
  
  # Merge on the standard VA
  va_tab1 <- merge(va_tab1, ww_tab1, "teacher_id")
  
  # mark up the weight type and options involved so we can do stuff by group
  va_tab1[, weight_type := weight_type]
  va_tab1[, lin_alpha := lin_alpha]
  va_tab1[, pctile := pctile]
  va_tab1[, v_alpha := v_alpha]
  va_tab1[, mrpctile := mrpctile]
  va_tab1[, mrdist := mrdist]
}


#==========================#
# ==== run monte carlo ====
#==========================#

# set parameters for this monte carlo run 
# CODEING NOTE: We could just fill this stuff into every function but some of it gets used twice. 
# that is an opportunity to miss somthing and make a mistake. It also just makes it annoying
# to change parameters so lets just set everything here. Might be a way to write the functions 
# to avoid this but I couldn't figure it out and I dont mind this global parameter type approach. 
nsims = 3
p_n_teacher  = 140
p_num_students = 100
p_teacher_va_epsilon = .05
p_teacher_ability_drop_off = 0.15
p_test_SEM = .07
p_weight_type = "rawlsian" # "linear" or "rawlsian" or "equal" or "v" or "mr"
p_method =  "wls" # "wls" or "semip" or "qtle"
p_pctile = .4
p_lin_alpha = NULL # For linear weights
p_v_alpha = NULL # For v weights
p_mrpctile = NULL # For mr weights
p_mrdist = NULL # for mr weights


# simulate initial data 
r_dt <- simulate_test_data(n_teacher               = p_n_teacher,
                           n_stud_per_teacher      = p_num_students,
                           test_SEM                = p_test_SEM,
                           teacher_va_epsilon      = p_teacher_va_epsilon,
                           teacher_ability_drop_off = p_teacher_ability_drop_off)

# Get true WW impact 
teacher_info <- true_ww_fun(in_dt                    = r_dt,
                            teacher_ability_drop_off = p_teacher_va_epsilon,
                            grid_size                = 10000,
                            weight_type              = p_weight_type,
                            lin_alpha                = p_lin_alpha,
                            pctile                   = p_pctile,
                            v_alpha                  = p_v_alpha,
                            mrpctile                 = p_mrpctile, 
                            mrdist                   = p_mrdist)
  


# run a monte carlo with whatever parameters you want 
mc_res <- foreach(i = 1:nsims) %dopar% single_iteration_fun(in_dt        = r_dt,
                                                            weight_type  = p_weight_type,
                                                            method       = p_method,
                                                            lin_alpha    = p_lin_alpha,
                                                            pctile       = p_pctile,
                                                            v_alpha      = p_v_alpha,
                                                            mrpctile     = p_mrpctile, 
                                                            mrdist       = p_mrpctile)

# here we could run it with other parameters and just stack the results 

#=========================#
# ==== combine results ====
#=========================#

# stack the results 
mc_res <- rbindlist(mc_res)

# Get the mean estiamtes for each teacher.The by groups are all descriptive variables 
by_vars <- setdiff(colnames(mc_res), c("estimate", "ww_va"))
mean_tab <- mc_res[, list(mean_standard = mean(estimate),
                          sd_standard   = sd(estimate),
                          mean_weighted = mean(ww_va),
                          sd_weighted   = sd(ww_va)), 
                   by = by_vars]

# merge on teacher info 
mean_tab <- merge(mean_tab, teacher_info, "teacher_id")



#===============#
# ==== save ====
#===============#


# dependign on what parameters we change and stuff we can change the name of this 
date_time <- gsub("-", "_", Sys.time())
date_time <- gsub(":", "_", date_time)
date_time <- gsub(" ", "__", date_time)
write.csv(mean_tab, paste0(out_plot, '/', "mc_results_", date_time,".csv" ))




