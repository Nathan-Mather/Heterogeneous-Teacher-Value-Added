# =========================================================================== #
# ============================ Simulation Figures =========================== #
# =========================================================================== #
# - Purpose of code:
#  - Make a few figures for simulation primitives and assumptions.

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
truth_color <- 'black'
truth_alpha <- 0.25

standard_color <- '#ffaabb'
standard_alpha <- 0.5

binned_color <- 'deepskyblue3'
binned_alpha <- 0.5

quantile_color <- 'darkgreen'
quantile_alpha <- 0.5




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
if(my_wd %like% "Nmath_000"){
  # base directory 
  base_path <- "c:/Users/Nmath_000/Documents/Research/"
  
  # path for data to save
  in_data <- "c:/Users/Nmath_000/Documents/data/Value Added/mc_data/"
  
  # path for plots
  out_plot <- "c:/Users/Nmath_000/Documents/data/Value Added/mc_plots/"
  
}else{
  # base directory 
  base_path <- "/home/tanner/Documents/Research/HeterogenousTeacherVA/Git/"
  
  # path for data to save 
  in_data <- "/home/tanner/Documents/Research/HeterogenousTeacherVA/Output/"
  
  # path for plots
  out_plot <- "/home/tanner/Documents/Research/HeterogenousTeacherVA/Output/Figures/"
}

# Load our functions now that we have a file path.
func_path <- "Heterogeneous-Teacher-Value-Added/R_code/functions/"
source(paste0(base_path, func_path, "binned_va_function.R"))
source(paste0(base_path, func_path, "mc_functions.R"))
source(paste0(base_path, func_path, "qtile_va_function.R"))
source(paste0(base_path, func_path, "simulate_test_data.R"))
source(paste0(base_path, func_path, "teacher_impact.R"))
source(paste0(base_path, func_path, "weighting_functions.R"))
source(paste0(base_path, func_path, "welfare_statistic.R"))

# Sort the runs to get the most recent.
files <- list.files(in_data)
xwalk_files <- grep("mc_xwalk_", files, value=TRUE)
xwalk_files <- sort(xwalk_files, decreasing=TRUE)

# Load most recent xwalk.
model_xwalk <- fread(paste0(in_data, xwalk_files[[1]]))




# =========================================================================== #
# =============== Make Figures for each Different Simulation ================ #
# =========================================================================== #

# Loop over xwalk to run this. 
for(i in 1:1) { #nrow(model_xwalk)){
  
  # Set seed. 
  set.seed(43)
  
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
  p_lin_alpha                <- model_xwalk[i, lin_alpha]          # For linear weights
  p_pctile                   <- model_xwalk[i, pctile]             # for rawlsian 
  p_weight_below             <- model_xwalk[i, weight_below ]      # for rawlsian 
  p_weight_above             <- model_xwalk[i, weight_above]       # for rawlsian 
  p_v_alpha                  <- model_xwalk[i, v_alpha]            # For v weights
  p_mrpctile                 <- model_xwalk[i, mrpctile]           # For mr weights
  p_mrdist                   <- model_xwalk[i, mrdist]             # for mr weights
  
  
  # ========================================================================= #
  # ========================= Example Teacher Figure ======================== #
  # ========================================================================= #
  
  # Simulate necessary data.
  teacher_ex <- simulate_test_data(n_teacher          = 2,
                                   n_stud_per_teacher = 1000,
                                   test_SEM           = p_test_SEM,
                                   teacher_va_epsilon = .05, #p_teacher_va_epsilon,
                                   impact_type        = p_impact_type,
                                   impact_function    = 1, #p_impact_function,
                                   max_diff           = p_max_diff,
                                   covariates         = p_covariates,
                                   peer_effects       = p_peer_effects,
                                   stud_sorting       = p_stud_sorting,
                                   rho                = p_rho,
                                   ta_sd              = p_ta_sd,
                                   sa_sd              = p_sa_sd)
  
  
  # Add in the noise.
  #teacher_ex[, teacher_impact := teacher_impact + rnorm(nrow(teacher_ex), sd = .05)] #p_teacher_va_epsilon)]
  
  # Estimate standard value added.
  if (p_covariates == 0) {
    va_out1 <- lm(test_2 ~ test_1 + teacher_id - 1, data = teacher_ex)
  } else {
    va_out1 <- lm(test_2 ~ test_1 + teacher_id + school_av_test + stud_sex +
                    stud_frpl + stud_att - 1, data = teacher_ex)
  }
  
  # Clean results.
  va_tab1 <- data.table(broom::tidy(va_out1))
  va_tab1[, teacher_id := gsub("teacher_id", "", term)]
  teacher_ex <- merge(teacher_ex, va_tab1[, c('teacher_id', 'estimate')], 'teacher_id')
  setnames(teacher_ex, c('estimate'), c('standard'))
  
  # Estimate the binned VA.
  if (p_covariates == 0) {
    output <- binned_va(in_data = teacher_ex)
  } else {
    output <- binned_va(in_data = teacher_ex,
                        reg_formula = paste0('test_2 ~ test_1 + teacher_id + ',
                                             'categories + teacher_id*categories',
                                             ' + school_av_test + stud_sex + ',
                                             'stud_frpl + stud_att - 1'))
  }
  
  # Fill in missing estimates with 0 (so that we end up with teacher_ability).
  output <- complete(output, teacher_id, category)
  for (i in seq_along(output)) set(output, i=which(is.na(output[[i]])), j=i,
                                   value=0)
  
  # Get the numeric range for each category.
  output <- as.data.table(output)
  output[, range_low := as.numeric(sub('\\(', '', sapply(strsplit(category, ','), '[', 1)))]
  output[, range_high := as.numeric(sub('\\]', '', sapply(strsplit(category, ','), '[', 2)))]
  
  # Get the baseline estimate for each teacher.
  output[, baseline := .SD[1, estimate], by='teacher_id']
  
  
  # Make the overall minimum very low and the overall maximum very high to capture all.
  output[category != '', temp1 := min(range_low), by='teacher_id']
  output[category != '', temp2 := max(range_high), by='teacher_id']
  output[range_low == temp1, range_low := -100]
  output[range_high == temp2, range_high := 100]
  
  # Calculate the estimated teacher impact.
  teacher_ex[, binned := mapply((function(x, y) output[output$teacher_id == x & 
                                                        output$range_low < y &
                                                        output$range_high >= y, estimate] + 
                                  output[output$teacher_id == x & 
                                           output$range_low < y &
                                           output$range_high >= y, baseline]), teacher_id, test_1)]
  
  # Run quantile regression and get estimates for a grid of tau values.
  qtile_res <- qtilep_va(in_data       = teacher_ex,
                         in_teacher_id = "teacher_id",
                         in_pre_test   = "test_1",
                         in_post_test  = "test_2",
                         ptle          = seq(.02, .98, by=.04))
  
  # Calculate the estimated teacher impact.
  l1 <- teacher_ex$test_1
  vals <- seq(.02, .98, by=.04)
  teacher_ex[, pct_val := mapply((function(xo)  length(l1[l1 <= xo])/length(l1)), test_1)]
  teacher_ex[, pct_val := mapply((function(xo)  vals[which.min(abs(vals - xo))]), pct_val)]
  
  teacher_ex[, quantile := mapply((function(x,y)  qtile_res[teacher_id == x &
                                                            abs(tau - y) < .02, qtile_est]), teacher_id, pct_val)]
  
  plot <- ggplot(data = teacher_ex[teacher_id == 2]) +
    geom_point(aes(x = stud_ability_1, y = teacher_impact), size = 2, color = truth_color, alpha = truth_alpha) +
    geom_point(aes(x = stud_ability_1, y = standard), size = 2, color = standard_color, alpha = standard_alpha) +
    geom_point(aes(x = stud_ability_1, y = binned), size = 2, color = binned_color, alpha = binned_alpha) + 
    #geom_point(aes(x = stud_ability_1, y = quantile), size = 2, color = quantile_color, alpha = quantile_alpha) + 
    ylab("Teacher's Impact") + 
    xlab("Student Ability") +
    plot_attributes
  print(plot)
} # Close overall loop.