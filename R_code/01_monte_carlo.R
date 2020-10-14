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
library(readxl)
library(doRNG)
library(np) # non parametric library
library(quantreg)

# check users. (NOTE TO MIKE, add something unique to your base working directory to detect when it is your computer)
my_wd <- getwd()
if(my_wd %like% "Nmath_000"){
  # base directory 
  base_path <- "c:/Users/Nmath_000/Documents/Research/"
  
  # path for data to save
  out_data <- "c:/Users/Nmath_000/Documents/data/Value Added/mc_data/"
  
}else{
  # base directory 
  base_path <- "~/Documents/Research/HeterogenousTeacherVA/Git/"
  
  # path for data to save 
  out_data <- "~/Documents/Research/HeterogenousTeacherVA/Git/Heterogeneous-Teacher-Value-Added/R_code"
  
}

# load model_xwalk 
model_xwalk <- data.table(read_excel(paste0(base_path, "Heterogeneous-Teacher-Value-Added/R_code/model_xwalk.xlsx")))

# load our functions now that we have a file path 
func_path <- "Heterogeneous-Teacher-Value-Added/R_code/functions/"
source(paste0(base_path, func_path, "simulate_test_data.R"))
source(paste0(base_path, func_path, "binned_va_function.R"))
source(paste0(base_path, func_path, "weighting_functions.R"))
source(paste0(base_path, func_path, "true_ww_impact.R"))
source(paste0(base_path, func_path, "teacher_impact.R"))
source(paste0(base_path, func_path, "qtile_aggregation.R"))
source(paste0(base_path, func_path, "qtile_va_function.R"))
source(paste0(base_path, func_path, "welfare_statistic.R"))


# get a time stamp 
date_time <- gsub("-", "_", Sys.time())
date_time <- gsub(":", "_", date_time)
date_time <- gsub(" ", "__", date_time)


#======================#
# ==== set options ====
#======================#

# parallel option 
do_parallel <- TRUE

# Set parallel options
if(my_wd %like% "Nmath_000"){
  myCluster <- makeCluster(4, # number of cores to use
                           type = "PSOCK") # type of cluster (Must be "PSOCK" on Windows)
}else{
  myCluster <- makeCluster(20, # number of cores to use
                           type = "FORK") # type of cluster (Must be "PSOCK" on Windows)
}
if(do_parallel){
  registerDoParallel(myCluster)
  registerDoRNG()
}

#====================================#
# ==== Single Iteration Function ====
#====================================#

# # run inside of MC function up until this single_iteration_fun to get debug parms 
# in_dt        = r_dt
# weight_type  = p_weight_type
# method       = p_method
# lin_alpha    = p_lin_alpha
# pctile       = p_pctile
# weight_below = p_weight_below
# weight_above = p_weight_above
# v_alpha      = p_v_alpha
# mrpctile     = p_mrpctile
# mrdist       = p_mrdist



# this is what a single run of the monte carlo will do 
single_iteration_fun <- function(in_dt        = NULL,
                                 weight_type  = NULL,
                                 method       = NULL, 
                                 lin_alpha    = NULL,
                                 pctile       = NULL,
                                 weight_below = NULL,
                                 weight_above = NULL,
                                 v_alpha      = NULL,
                                 mrpctile     = NULL, 
                                 mrdist       = NULL,
                                 npoints      = NULL){
  
  # I need this for it to work on windows clusters since libraries are not loaded  on every cluster
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

    
  # check method option
  if(method=="bin"){
    # Estimate the binned VA
    output <- binned_va(in_data = in_dt)
    
    # Fix the first teacher. ###################### Check this later.
    for(row in unique(output$category)) {
      if (row != '') {
        output <- rbindlist(list(output, list(1, row, 0)))
      }
    }
    
    # Get the estimated welfare.
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

  if(method=="semip"){
    # put implementation here. Call output or rename that object everywhere 
    # not really a good name anyway 
    
    output <- semip_va(in_data = in_dt )
  }
  
  if(method=="qtle"){
    # run qtile regression and get estimates for a grid of tau values 
    qtile_res <- qtilep_va(in_data = in_dt,
                           in_teacher_id = "teacher_id",
                           in_pre_test   = "test_1",
                           in_post_test  = "test_2",
                           ptle = seq(.02,.98,by=.04))
    
    
    # Now aggregate them 
    output <- welfare_statistic(in_dt           = in_dt,
                                output          = output,
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
  
  return(va_tab2[])

}


#==========================#
# ==== run monte carlo ====
#==========================#

# i <- 1

# initialize list for MC runs 
mc_res_list <- vector("list", length = nrow(model_xwalk))

# loop over xwalk to run this 
for(i in 1:nrow(model_xwalk)){

  # set parameters for this monte carlo run
  # run parameters 
  run_id                     <- model_xwalk[i, run_id]  # keep track of what run it is 
  nsims                      <- model_xwalk[i, nsims]   # how many simulations to do 
  p_npoints                  <- model_xwalk[i, npoints] # number of grid points over which to calculate welfare added
  # teacher parms 
  p_n_teacher                <- model_xwalk[i, n_teacher] # number of teachers 
  p_n_stud_per_teacher       <- model_xwalk[i, n_stud_per_teacher] # students per teaher
  p_teacher_va_epsilon       <- model_xwalk[i, teacher_va_epsilon] # SD of noise on teacher impacy 
  p_test_SEM                 <- model_xwalk[i, test_SEM]  # SEM of test 
  p_impact_type              <- model_xwalk[i, impact_type]  
  p_impact_function          <- model_xwalk[i, impact_function]  
  p_max_diff                 <- model_xwalk[i, max_diff]  
  # weight and estimation parameters 
  p_weight_type              <- model_xwalk[i, weight_type] # style of social planner pareto weights
  p_method                   <- model_xwalk[i, method] # method of estimation used 
  p_lin_alpha                <- model_xwalk[i, lin_alpha] # For linear weights
  p_pctile                   <- model_xwalk[i, pctile] # for rawlsian 
  p_weight_below             <- model_xwalk[i, weight_below ] # for rawlsian 
  p_weight_above             <- model_xwalk[i, weight_above] # for rawlsian 
  p_v_alpha                  <- model_xwalk[i, v_alpha]# For v weights
  p_mrpctile                 <- model_xwalk[i, mrpctile] # For mr weights
  p_mrdist                   <- model_xwalk[i, mrdist] # for mr weights
  
  # simulate initial data 
  r_dt <- simulate_test_data(n_teacher                = p_n_teacher,
                             n_stud_per_teacher       = p_n_stud_per_teacher,
                             test_SEM                 = p_test_SEM,
                             teacher_va_epsilon       = p_teacher_va_epsilon,
                             impact_type              = p_impact_type,
                             impact_function          = p_impact_function,
                             max_diff                 = p_max_diff)
  
  
  # Get true WW impact 
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
    
  # run a monte carlo with whatever parameters you want 
  if(do_parallel){
    mc_res <- foreach(j = 1:nsims) %dopar% single_iteration_fun(in_dt        = r_dt,
                                                                weight_type  = p_weight_type,
                                                                method       = p_method,
                                                                lin_alpha    = p_lin_alpha,
                                                                pctile       = p_pctile,
                                                                weight_above = p_weight_above,
                                                                weight_below = p_weight_below,
                                                                v_alpha      = p_v_alpha,
                                                                mrpctile     = p_mrpctile, 
                                                                mrdist       = p_mrpctile,
                                                                npoints      = p_npoints)
    
  } else {
    mc_res <- foreach(j = 1:nsims) %do% single_iteration_fun(in_dt        = r_dt,
                                                             weight_type  = p_weight_type,
                                                             method       = p_method,
                                                             lin_alpha    = p_lin_alpha,
                                                             pctile       = p_pctile,
                                                             weight_above = p_weight_above,
                                                             weight_below = p_weight_below,
                                                             v_alpha      = p_v_alpha,
                                                             mrpctile     = p_mrpctile, 
                                                             mrdist       = p_mrpctile,
                                                             npoints      = p_npoints)
    
  }

# stack the results for this run 
mc_res <- rbindlist(mc_res)

# Get the mean estimates for each teacher.The by groups are all descriptive variables 
mean_tab <- mc_res[, list(mean_standard = mean(estimate),
                          sd_standard   = sd(estimate),
                          mean_ww = mean(ww_va),
                          sd_ww   = sd(ww_va)),
                   by = teacher_id]

# add some more indicators 
mean_tab[, run_id := run_id]
mean_tab[, nsims := nsims]

# merge on teacher info 
mean_tab <- merge(mean_tab, teacher_info, "teacher_id")

# put results in a list 
mc_res_list[[i]] <- mean_tab

# close loop over model 
}

# Let go of the processors
if(do_parallel){
  stopCluster(myCluster)
}

#=========================#
# ==== combine results ====
#=========================#

  # stack up all the runs 
  mc_res_full <- rbindlist(mc_res_list)

#===============#
# ==== save ====
#===============#


# depending on what parameters we change and stuff we can change the name of this 
write.csv(mc_res_full, paste0(out_data, '/', "mc_results_", date_time,".csv" ), row.names = FALSE)

# save a copy of the most recent xwalk also so there are no mixups 
write.csv(model_xwalk, paste0(out_data, '/', "mc_xwalk_", date_time,".csv" ), row.names = FALSE)



