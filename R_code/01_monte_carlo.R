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

# geta time stamp 
date_time <- gsub("-", "_", Sys.time())
date_time <- gsub(":", "_", date_time)
date_time <- gsub(" ", "__", date_time)


#======================#
# ==== set options ====
#======================#

# paralell option 
do_parallel <- FALSE

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
                                 mrdist       = NULL){
  
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
  
  # Now run the weighted VA
  # Generate the weights
  in_dt[, (weight_type) := ww_general_fun(weight_type  = weight_type,
                                          in_test_1    = test_1,
                                          lin_alpha    = lin_alpha,
                                          pctile       = pctile,
                                          weight_below = weight_below,
                                          weight_above = weight_above,
                                          v_alpha      = v_alpha,
                                          mrpctile     = mrpctile, 
                                          mrdist       = mrdist)]
  
  # Generate a grid over which we can get the true welfare added.
  grid <- unlist(lapply(seq(-3, 3, length.out = npoints), rep, times =  length(unique(r_dt$teacher_id))))
  
  # Attach teacher ids.
  welfare <- unique(in_dt[, c('teacher_id', 'teacher_ability', 'teacher_center', 'teacher_max')])
  welfare <- do.call('rbind', replicate(npoints, welfare, simplify=FALSE))
  welfare <- welfare[, grid := grid]
  
  # Get the weights for each place in the grid. ################ Need to update weights other than Rawlsian.
  welfare[, weight := ww_general_fun(weight_type  = weight_type,
                                     in_test_1    = grid,
                                     weight_below = weight_below,
                                     weight_above = weight_above,
                                     pctile_val   = quantile(in_dt$test_1, pctile))]
  
  # Get the teacher impact for the grid.
  welfare[, true_impact := teacher_impact(teacher_ability  = teacher_ability,
                                          teacher_center   = teacher_center,
                                          teacher_max      = teacher_max,
                                          stud_ability_1   = in_dt$stud_ability_1,
                                          other_data       = grid,
                                          type             = impact_type,
                                          func_num         = impact_function)]
  
  # Calculate the true welfare.
  welfare[, true_welfare := sum(weight*true_impact), by='teacher_id']
  welfare <- unique(welfare[, c('teacher_id', 'grid', 'weight', 'true_welfare', 'true_impact')])
    
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
    
    # Calculate the estimated teacher impact.
    output[, range_low := as.numeric(sub('\\(', '', sapply(strsplit(category, ','), '[', 1)))]
    output[, range_high := as.numeric(sub('\\]', '', sapply(strsplit(category, ','), '[', 2)))]
    output[, baseline := .SD[1, estimate], by='teacher_id']
    
    output[category != '', temp1 := min(range_low), by='teacher_id']
    output[category != '', temp2 := max(range_high), by='teacher_id']
    output[range_low == temp1, range_low := -100]
    output[range_high == temp2, range_high := 100]

    welfare[, estimate := mapply((function(x, y) output[output$teacher_id == x & 
                                                        output$range_low < y &
                                                        output$range_high >= y, estimate] + 
                                                 output[output$teacher_id == x & 
                                                        output$range_low < y &
                                                        output$range_high >= y, baseline]), teacher_id, grid)]
    
    #ggplot() + geom_point(data=welfare[teacher_id == 6], aes(x=grid, y=true_impact), color='red') +
    #  geom_point(data=welfare[teacher_id == 6], aes(x=grid, y=estimate))
    
    output <- welfare[, c('teacher_id', 'true_welfare', 'estimate')]
    
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
    output    <- qtile_agg(in_test_1   = in_dt$test_1,
                          in_coefs     = qtile_res,
                          weight_type  = weight_type,
                          lin_alpha    = lin_alpha,
                          pctile       = pctile,
                          weight_below = weight_below,
                          weight_above = weight_above,
                          v_alpha      = v_alpha,
                          mrpctile     = mrpctile,
                          mrdist       = mrdist)
 
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
  # teacger parms 
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
  #NOTE: NEED TO UPDATE THIS WITH TANNERS THING AND ALSO UPDATE THE PARAMETERS IN THE XWALK ...........................
  r_dt <- simulate_test_data(n_teacher                = p_n_teacher,
                             n_stud_per_teacher       = p_n_stud_per_teacher,
                             test_SEM                 = p_test_SEM,
                             teacher_va_epsilon       = p_teacher_va_epsilon,
                             impact_type              = p_impact_type,
                             impact_function          = p_impact_function,
                             max_diff                 = p_max_diff)
  
  
  # Get true WW impact 
  # NOTE: THIS ALSO NEEDS TO BE REPLACED, it does not work the way it should since the trth chagnes with the sample 
  # will need to put in the new rawlsian parameters also 
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
                                                                mrdist       = p_mrpctile)
  }else{
    mc_res <- foreach(j = 1:nsims) %do% single_iteration_fun(in_dt        = r_dt,
                                                             weight_type  = p_weight_type,
                                                             method       = p_method,
                                                             lin_alpha    = p_lin_alpha,
                                                             pctile       = p_pctile,
                                                             weight_above = p_weight_above,
                                                             weight_below = p_weight_below,
                                                             v_alpha      = p_v_alpha,
                                                             mrpctile     = p_mrpctile, 
                                                             mrdist       = p_mrpctile)
    
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



