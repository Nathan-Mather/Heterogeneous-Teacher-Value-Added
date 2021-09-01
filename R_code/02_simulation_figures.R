# =========================================================================== #
# ============================ Simulation Figures =========================== #
# =========================================================================== #
# - Purpose of code:
#  - Make a few figures for the simulation primitives and assumptions.

# Clear data.
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")

# No scientific notation. 
options(scipen = 999)

# Clean console history.
cat("\f")

# Set plot attributes.
plot_attributes <- theme_classic() + 
  theme(text = element_text(size= 20),
        plot.title = element_text(vjust=0,
                                  hjust = 0.5,
                                  colour = "black",
                                  face = "bold",
                                  size =25),
        plot.subtitle = element_text(color = "black",
                                     hjust = 0.5,
                                     size =25))

# Set various graph options.
truth_color <- '#000000'
truth_alpha <- 0.5

standard_color <- '#0072B2'
standard_alpha <- 0.5

binned_color <- '#E69F00'
binned_alpha <- 0.5

quantile_color <- 'darkgreen'
quantile_alpha <- 0.5

np_color <- '#009E73'
np_alpha <- 0.5

np1_color <- 'gold'
np1_alpha <- 0.5

av_color <- '#ffaabb'
av_alpha <- 0.5




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
library(scales)

# Check users to set directory.
#  (NOTE TO MIKE, add something unique to your base working directory to detect
#   when it is your computer)
my_wd <- getwd()
if (my_wd %like% "Nmath_000") {
  # Base directory. 
  base_path <- "c:/Users/Nmath_000/Documents/Research/"
  
  # Path for data to save.
  out_data <- "C:/Users/Nmath_000/Documents/Research/Value_added_local/results"
  
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
model_xwalk <- fread(paste0(out_data,
                           "/xwalk.csv"))


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


#==============================================#
# ==== make teacher xwalk specific figures ====
#==============================================#

# get all the unique teacher xwalks 

ts_xwalk_sub <- unique(model_xwalk, by = c("teacher_student_xwalk","n_cohorts" ))

for(k in 1:nrow(ts_xwalk_sub)){
  
  xwalk_name_k <- ts_xwalk_sub[k, teacher_student_xwalk]
  p_n_cohorts <- ts_xwalk_sub[k, n_cohorts]
  
  # load teacher student xwalk 
  teacher_student_xwalk <- fread(paste0(base_path, "Heterogeneous-Teacher-Value-Added/R_code/", xwalk_name_k, ".csv"))
  
  #=====================#
  # ==== run models ====
  #=====================#
  
  
  # Simulate teacher data 
  # we can fill out teacher 
  teacher_ability_xwalk <- simulate_teacher_ability(teacher_student_xwalk,
                                                    ta_sd                   = .6,
                                                    school_cor              = 0,
                                                    tc_sd                   = 1,
                                                    min_diff                = 0,
                                                    max_diff                = 1.2)
  
  # simulate student data 
  r_student_dt <- simulate_sdusd_data(teacher_ability_xwalk   = teacher_ability_xwalk,
                                      n_cohorts               = p_n_cohorts,
                                      pretest_coef            = .9,
                                      impact_type             = "NO",
                                      impact_function         = 1
                                      # test_SEM                 = 0.07,
                                      # covariates               = 0,
                                      # peer_effects             = 0,
                                      # stud_sorting             = 0,
                                      # rho                      = 0.2
  )
  
  # make a plot of teacher' class size 's number of students 
  stud_count <- r_student_dt[, list(n_studs = .N), teacher_id]
  
  stud_count_plot <- ggplot(data = stud_count) +
    geom_histogram(aes(x = n_studs), 
                   fill=I(standard_color), 
                   col=I("black") ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    ggtitle(paste0("Example Student Counts ", xwalk_name_k)) +
    plot_attributes + 
    theme(plot.title = element_text(size =15))
  
  # great place to save this 
  dir.create( paste0(out_qc, "teacher_student_xwalk/",  "interactive_data/"),
             recursive = TRUE)
  
  
  # Save the figures.
  ggsave(filename =  paste0(out_qc, "teacher_student_xwalk/", xwalk_name_k, "_counts.png"), 
         plot     = stud_count_plot, 
         width    = 9, 
         height   = 4)
  
  save(stud_count_plot,
       file = paste0(out_qc, "teacher_student_xwalk/","interactive_data/", xwalk_name_k, "_counts.Rdata"))
  
}

# generate student data 

# plot distrubutiin 

# =========================================================================== #
# =============== Make Figures for each Different Simulation ================ #
# =========================================================================== #

# Loop over xwalk to run this. 
for(i in 1:nrow(model_xwalk)){
  
  # Set seed.
  set.seed(42)
  
  run_id <- model_xwalk[i, run_id]
  
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
  
  
  
  # load teacher student xwalk 
  teacher_student_xwalk <- fread(paste0(base_path, "Heterogeneous-Teacher-Value-Added/R_code/", ts_xwalk_name, ".csv"))
  
  #=====================#
  # ==== run models ====
  #=====================#

  
  # Simulate teacher data 
  teacher_ability_xwalk <- simulate_teacher_ability(teacher_student_xwalk,
                                                    ta_sd                   = p_ta_sd,
                                                    school_cor              = 0,
                                                    tc_sd                   = p_tc_sd,
                                                    min_diff                = p_min_diff,
                                                    max_diff                = p_max_diff)
  
  # simulate student data 
  r_student_dt <- simulate_sdusd_data(teacher_ability_xwalk   = teacher_ability_xwalk,
                                      n_cohorts               = p_n_cohorts,
                                      pretest_coef            = p_pretest_coef,
                                      impact_type             = p_impact_type,
                                      impact_function         = p_impact_function
                                      # test_SEM                 = 0.07,
                                      # covariates               = 0,
                                      # peer_effects             = 0,
                                      # stud_sorting             = 0,
                                      # rho                      = 0.2
  )
  
  # Estimate standard value added.
  va_stand <- standard_va_stat(in_dt              = r_student_dt,
                              covariates         = p_covariates,
                              se_flag           = TRUE)
  
  # Estimate the binned VA.
  va_outputs <-  binned_va_stat(in_dt              = r_student_dt,
                             weight_type        = p_weight_type,
                             lin_alpha          = p_lin_alpha,
                             pctile             = p_pctile,
                             weight_below       = p_weight_below,
                             weight_above       = p_weight_above,
                             v_alpha            = p_v_alpha,
                             mrpctile           = p_mrpctile, 
                             mrdist             = p_mrdist,
                             npoints            = p_npoints,
                             covariates         = p_covariates,
                             num_cats           = p_num_cats,
                             qc_flag            = 1)
  # extract peices 
  bin_welfare <- va_outputs$welfare_est
  bin_full_ests <- va_outputs$full_est
  bin_xwalk    <- va_outputs$bin_xwalk
  
  # organize the estimates 
  bin_full_ests <- merge(bin_full_ests, bin_xwalk, "bin")
  
  # # Run quantile regression and get estimates for a grid of tau values.
  # qtile_res <- qtilep_va(in_data       = teacher_ex,
  #                        in_teacher_id = "teacher_id",
  #                        in_pre_test   = "test_1",
  #                        in_post_test  = "test_2",
  #                        ptle          = seq(.02, .98, by=.04))
  # 
  # qtile_constants <- qtile_res[teacher_id == 'test_1']
  # qtile_res <- qtile_res[teacher_id != 'test_1', ]
  # 
  # qtile_res[, qtile_est := (qtile_est - mean(qtile_est))/sd(qtile_est), tau]
  # qtile_res[, qtile_est := mapply((function(x, y)  y + qtile_constants[tau == x, qtile_est]), tau, qtile_est)]
  # 
  # # Calculate the estimated teacher impact.
  # l1 <- teacher_ex$test_1
  # vals <- seq(.02, .98, by=.04)
  # teacher_ex[, pct_val := mapply((function(xo)  length(l1[l1 <= xo])/length(l1)), test_1)]
  # teacher_ex[, pct_val := mapply((function(xo)  vals[which.min(abs(vals - xo))]), pct_val)]
  # 
  # teacher_ex[, quantile := mapply((function(x,y)  qtile_res[teacher_id == x &
  #                                                             abs(tau - y) < .02, qtile_est]), teacher_id, pct_val)]
  # 
  # 
  # # Run the NP regression.
  # np_res <- np_hack_va(in_data       = teacher_ex,
  #                      in_teacher_id = "teacher_id",
  #                      in_pre_test   = "test_1",
  #                      in_post_test  = "test_2",
  #                      npoints       = p_npoints,
  #                      weighted_av   = FALSE)
  # 
  # 
  # # Get the coefficients.
  # teacher_ex[, np := mapply((function(x,y)  as.matrix(np_res$results[, 1:p_npoints],
  #                                                     ncol(1))[x, which.min(abs(np_res$points - y))]),
  #                           teacher_id, test_1)]
  # 
  # # Run the NP regression weighted estimate.
  # np_res <- np_hack_va(in_data       = teacher_ex,
  #                      in_teacher_id = "teacher_id",
  #                      in_pre_test   = "test_1",
  #                      in_post_test  = "test_2",
  #                      npoints       = p_npoints,
  #                      weighted_av   = TRUE)
  # 
  # 
  # # Get the coefficients and subtract off the pre-test score.
  # teacher_ex[, np1 := mapply((function(x,y)  as.matrix(np_res$results[, 1:p_npoints],
  #                                                      ncol(1))[x, which.min(abs(np_res$points - y))]),
  #                            teacher_id, test_1)]
  # 
  # # Get the ymin and ymax for consistency across the teacher example plots.
  # ymin <- min(teacher_ex[, c('teacher_impact', 'standard', 'binned', 'np')])
  # ymax <- max(teacher_ex[, c('teacher_impact', 'standard', 'binned', 'np')])
  # 
  
  #================================#
  # ==== Example teacher plots ====
  #================================#
  
  # initialize a folder to save the stuff 
  # clearn and then create folder to save these in 
  
  full_out_path <- paste0(out_qc, "run_", run_id, "/simulation_qc/")
  
  
  files_to_clear <- list.files(full_out_path, full.names = TRUE)
  z <- unlink(files_to_clear,
          recursive = TRUE)
  
  
  dir.create(paste0(full_out_path, "/interactive_data/"),
             recursive = TRUE)
  
  # merge and organize all the data we need 
  welfare_dt <- merge(r_student_dt, va_stand, "teacher_id")
  
  # mamke bin categories 
  for(k in 1:nrow(bin_xwalk)){
    lower_i <- bin_xwalk[k, range_low]
    upper_i <- bin_xwalk[k, range_high]
    bin_i   <- bin_xwalk[k, bin]
    
    welfare_dt[ stud_ability_1 >= lower_i & stud_ability_1 <= upper_i, bin := bin_i]
    
  }
  
  # merge on binned ests 
  setnames(bin_full_ests, "estimate", "bin_estiamte")
  welfare_dt <- merge(welfare_dt, bin_full_ests, c("teacher_id", "bin"))

  # pick 10 random teachers and graph their imapct 
  rand_teachers <- sample(teacher_ability_xwalk[, teacher_id], 10)
  
  # find min and max accross all teachers estimates 
  ymin <- min(welfare_dt[, c('teacher_impact', 'standard_welfare', 'bin_estiamte')])
  ymax <- max(welfare_dt[, c('teacher_impact', 'standard_welfare', 'bin_estiamte')])
  
  # delete all old examples   
  to_delete <- list.files( paste0(full_out_path), full.names = TRUE)
  to_delete <- grep("_ests_", to_delete, value = TRUE)
  unlink(to_delete)
  
  # loop over random selection of teachers 
  for(t_j in rand_teachers){
    
    # subset data points to this specific teacher 
    welfare_sub <- welfare_dt[teacher_id == t_j]
    
    teacher_est_plot <- ggplot(data =welfare_sub) +
      geom_point(aes(x = stud_ability_1, y = teacher_impact, color = 'True Teacher Impact'), size = 2, alpha = truth_alpha) +
      geom_point(aes(x = stud_ability_1, y = standard_welfare, color = 'Standard'), size = 2, alpha = standard_alpha) +
      geom_point(aes(x = stud_ability_1, y = bin_estiamte, color = 'Binned'), size = 2, alpha = binned_alpha) + 
      ylab("Teacher Impact") + 
      xlab("Student Ability") +
      ylim(ymin, ymax) +
      plot_attributes + 
      scale_color_manual(values = c(binned_color, standard_color, truth_color)) +
      theme(legend.title = element_blank(),
            legend.position = 'bottom')
    
    teacher_est_plot2 <- ggplot(data =welfare_sub) +
      geom_point(aes(x = stud_ability_1, y = (stud_ability_2-stud_ability_1), color = 'Student growth'), size = 2, alpha = truth_alpha) +
      geom_point(aes(x = stud_ability_1, y = standard_welfare, color = 'Standard'), size = 2, alpha = standard_alpha) +
      geom_point(aes(x = stud_ability_1, y = bin_estiamte, color = 'Binned'), size = 2, alpha = binned_alpha) + 
      ylab("Teacher Impact") + 
      xlab("Student Ability") +
      ylim(ymin, ymax) +
      plot_attributes + 
      scale_color_manual(values = c(binned_color, standard_color, truth_color)) +
      theme(legend.title = element_blank(),
            legend.position = 'bottom')
    
    
    # Save the figures.
    ggsave(filename = paste0(full_out_path, "teacher_", t_j, "_ests_truth_run_",  run_id, ".png"), 
           plot     = teacher_est_plot, 
           width    = 9, 
           height   = 4)
    
    ggsave(filename = paste0(full_out_path, "teacher_", t_j, "_ests_growth_run_",  run_id, ".png"), 
           plot     = teacher_est_plot2, 
           width    = 9, 
           height   = 4)
    
    # save r data for shiny app 
    save(teacher_est_plot,
         file = paste0(full_out_path, "/interactive_data/", "teacher_", t_j, "_ests_truth_run_",  run_id, ".Rdata"))
    
    save(teacher_est_plot2,
         file = paste0(full_out_path, "/interactive_data/", "teacher_", t_j, "_ests_growth_run_",  run_id, ".Rdata"))
  }

  
  
}
