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
library(Matrix)
library(matrixStats)
library(np)
library(OneR)
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
  in_data <- "c:/Users/Nmath_000/Documents/Research/Heterogeneous-Teacher-Value-Added/R_code/"
  
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
source(paste0(base_path, func_path, "np_hack_va_function.R"))
source(paste0(base_path, func_path, "qtile_va_function.R"))
source(paste0(base_path, func_path, "simulate_test_data.R"))
source(paste0(base_path, func_path, "teacher_impact.R"))
source(paste0(base_path, func_path, "weighting_functions.R"))
source(paste0(base_path, func_path, "welfare_statistic.R"))
source(paste0(base_path, "Heterogeneous-Teacher-Value-Added/R_code/SDUSD Simulations/simulate_sdusd_data.R"))

# Load the xwalk.
model_xwalk <- data.table(read_excel(paste0(in_data, 'model_xwalk_SDUSD.xlsx')))

# load teacher student xwalk 
teacher_student_xwalk <- fread("c:/Users/Nmath_000/Documents/Research/Value added local/simulation_inputs/teacher_student_xwalk_realish.csv")


# Figure out which figures already exist.
figs <- list.files(path=out_plot)
figs <- tail(figs[figs %like% 'weight_example_'], n=1)

if (length(figs) == 0) {
  last_num <- 0
} else {
  last_num <- as.numeric(gsub('^.*([0-9]+).*$', '\\1', figs))
}





# =========================================================================== #
# =============== Make Figures for each Different Simulation ================ #
# =========================================================================== #

# Loop over xwalk to run this. 
for(i in 1:nrow(model_xwalk)){
  
  # Set seed.
  set.seed(42)
  
  # Set parameters for this Monte Carlo run.
  # Run parameters.
  run_id                     <- i           # keep track of what run it is 
  nsims                      <- model_xwalk[i, nsims]              # how many simulations to do
  # single_run                 <- model_xwalk[i, single_run]         # whether or not to run just one draw and calculate bootstrap SE's
  p_npoints                  <- model_xwalk[i, npoints]            # number of grid points over which to calculate welfare added
  
  # Simulated data parameters.
  # p_n_teacher                <- model_xwalk[i, n_teacher]          # number of teachers 
  # p_n_stud_per_teacher       <- model_xwalk[i, n_stud_per_teacher] # students per teacher
  p_test_SEM                 <- model_xwalk[i, test_SEM]           # SEM of test
  # p_teacher_va_epsilon       <- model_xwalk[i, teacher_va_epsilon] # SD of noise on teacher impact 
  p_impact_type              <- model_xwalk[i, impact_type]        # one of 'MLRN', 'MLR', 'MNoR', 'MNo', 'No'
  p_impact_function          <- model_xwalk[i, impact_function]    # which teacher impact function to use, and integer
  p_min_diff                 <- model_xwalk[i, min_diff]           # minimum impact difference between best and worst matched students
  p_max_diff                 <- model_xwalk[i, max_diff]           # maximum impact difference between best and worst matched students
  p_covariates               <- model_xwalk[i, covariates]         # whether or not to include covariates
  p_peer_effects             <- model_xwalk[i, peer_effects]       # whether or not to include peer effects
  p_stud_sorting             <- model_xwalk[i, stud_sorting]       # whether or not to include student sorting
  p_rho                      <- model_xwalk[i, rho]                # correlation between teacher and student ability
  p_ta_sd                    <- model_xwalk[i, ta_sd]              # teacher ability standard deviation
  # p_sa_sd                    <- model_xwalk[i, sa_sd]              # student ability standard deviation
  p_tc_sd                    <- model_xwalk[i, tc_sd]              # teacher center standard deviation
  p_n_cohorts                <- model_xwalk[i, n_cohorts]          # number of cohorts per teacher 
  p_impact_type              <- model_xwalk[i, impact_type]        # one of 'MLRN', 'MLR', 'MNoR', 'MNo', 'No'
  p_impact_function          <- model_xwalk[i, impact_function]    # which teacher impact function to use, and integer
  p_pretest_coef             <- model_xwalk[i, pretest_coef]       #coefficent on student pretest/ability 
  
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
  
  if ((as.numeric(run_id) <= last_num) | (as.numeric(single_run) == 1)) {
    next
  }
  
  
  # ========================================================================= #
  # ========================= Example Teacher Figure ======================== #
  # ========================================================================= #
  
  # Simulate teacher data 
  teacher_ability_xwalk <- simulate_teacher_ability(teacher_student_xwalk,
                                                    ta_sd                   = p_ta_sd,
                                                    school_cor              = 0,
                                                    tc_sd                   = p_tc_sd,
                                                    min_diff                = p_min_diff,
                                                    max_diff                = p_max_diff)
  
  # Simulate necessary data.
  # generate student data 
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
  if (p_covariates == 0) {
    va_out1 <- lm(test_2 ~ test_1 + teacher_id - 1, data = r_student_dt)
  } else {
    va_out1 <- lm(test_2 ~ test_1 + teacher_id + school_av_test + stud_sex +
                    stud_frpl + stud_att - 1, data = r_student_dt)
  }
  
  # Clean results.
  va_tab1 <- data.table(broom::tidy(va_out1))
  va_tab1[, teacher_id := gsub("teacher_id", "", term)]
  teacher_ex <- merge(teacher_ex, va_tab1[, c('teacher_id', 'estimate')], 'teacher_id')
  setnames(teacher_ex, c('estimate'), c('standard'))
  
  
  # Estimate the binned VA.
  if (p_covariates == 0) {
    output <- binned_va(in_data = teacher_ex, num_cats = 5)
  } else {
    output <- binned_va(in_data = teacher_ex,
                        reg_formula = paste0('test_2 ~ test_1',
                                             ' + teacher_id:categories',
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
                                                         output$range_high >= y, estimate]), teacher_id, test_1)]
  
  
  # Run quantile regression and get estimates for a grid of tau values.
  qtile_res <- qtilep_va(in_data       = teacher_ex,
                         in_teacher_id = "teacher_id",
                         in_pre_test   = "test_1",
                         in_post_test  = "test_2",
                         ptle          = seq(.02, .98, by=.04))
  
  qtile_constants <- qtile_res[teacher_id == 'test_1']
  qtile_res <- qtile_res[teacher_id != 'test_1', ]
  
  qtile_res[, qtile_est := (qtile_est - mean(qtile_est))/sd(qtile_est), tau]
  qtile_res[, qtile_est := mapply((function(x, y)  y + qtile_constants[tau == x, qtile_est]), tau, qtile_est)]
  
  # Calculate the estimated teacher impact.
  l1 <- teacher_ex$test_1
  vals <- seq(.02, .98, by=.04)
  teacher_ex[, pct_val := mapply((function(xo)  length(l1[l1 <= xo])/length(l1)), test_1)]
  teacher_ex[, pct_val := mapply((function(xo)  vals[which.min(abs(vals - xo))]), pct_val)]
  
  teacher_ex[, quantile := mapply((function(x,y)  qtile_res[teacher_id == x &
                                                              abs(tau - y) < .02, qtile_est]), teacher_id, pct_val)]
  
  
  # Run the NP regression.
  np_res <- np_hack_va(in_data       = teacher_ex,
                       in_teacher_id = "teacher_id",
                       in_pre_test   = "test_1",
                       in_post_test  = "test_2",
                       npoints       = p_npoints,
                       weighted_av   = FALSE)
  
  
  # Get the coefficients.
  teacher_ex[, np := mapply((function(x,y)  as.matrix(np_res$results[, 1:p_npoints],
                                                      ncol(1))[x, which.min(abs(np_res$points - y))]),
                            teacher_id, test_1)]
  
  # Run the NP regression weighted estimate.
  np_res <- np_hack_va(in_data       = teacher_ex,
                       in_teacher_id = "teacher_id",
                       in_pre_test   = "test_1",
                       in_post_test  = "test_2",
                       npoints       = p_npoints,
                       weighted_av   = TRUE)
  
  
  # Get the coefficients and subtract off the pre-test score.
  teacher_ex[, np1 := mapply((function(x,y)  as.matrix(np_res$results[, 1:p_npoints],
                                                       ncol(1))[x, which.min(abs(np_res$points - y))]),
                             teacher_id, test_1)]
  
  # Get the ymin and ymax for consistency across the teacher example plots.
  ymin <- min(teacher_ex[, c('teacher_impact', 'standard', 'binned', 'np')])
  ymax <- max(teacher_ex[, c('teacher_impact', 'standard', 'binned', 'np')])
  
  # Put together the example teacher plot.
  teacher_example_just_truth <- ggplot(data = teacher_ex[teacher_id == 1]) +
    geom_point(aes(x = stud_ability_1, y = teacher_impact, color = 'True Teacher Impact'), size = 2, alpha = truth_alpha) +
    ylab("Teacher Impact") + 
    xlab("Student Ability") +
    ylim(ymin, ymax) +
    plot_attributes + 
    scale_color_manual(values = c(truth_color)) +
    theme(legend.position = "none")
  
  teacher_example_truth_standard <- ggplot(data = teacher_ex[teacher_id == 1]) +
    geom_point(aes(x = stud_ability_1, y = teacher_impact, color = 'True Teacher Impact'), size = 2, alpha = truth_alpha) +
    geom_point(aes(x = stud_ability_1, y = standard, color = 'Standard'), size = 2, alpha = standard_alpha) +
    ylab("Teacher Impact") + 
    xlab("Student Ability") +
    ylim(ymin, ymax) +
    plot_attributes + 
    scale_color_manual(values = c(standard_color, truth_color)) +
    theme(legend.title = element_blank(),
          legend.position = 'bottom')
  
  teacher_example_truth_bin <- ggplot(data = teacher_ex[teacher_id == 1]) +
    geom_point(aes(x = stud_ability_1, y = teacher_impact, color = 'True Teacher Impact'), size = 2, alpha = truth_alpha) +
    geom_point(aes(x = stud_ability_1, y = standard, color = 'Standard'), size = 2, alpha = standard_alpha) +
    geom_point(aes(x = stud_ability_1, y = binned, color = 'Binned'), size = 2, alpha = binned_alpha) + 
    ylab("Teacher Impact") + 
    xlab("Student Ability") +
    ylim(ymin, ymax) +
    plot_attributes + 
    scale_color_manual(values = c(binned_color, standard_color, truth_color)) +
    theme(legend.title = element_blank(),
          legend.position = 'bottom')
  
  teacher_example_truth_np <- ggplot(data = teacher_ex[teacher_id == 1]) +
    geom_point(aes(x = stud_ability_1, y = teacher_impact, color = 'True Teacher Impact'), size = 2, alpha = truth_alpha) +
    geom_point(aes(x = stud_ability_1, y = standard, color = 'Standard'), size = 2, alpha = standard_alpha) +
    geom_point(aes(x = stud_ability_1, y = np, color = 'NP'), size = 2, alpha = np_alpha) + 
    ylab("Teacher Impact") + 
    xlab("Student Ability") +
    ylim(ymin, ymax) +
    plot_attributes + 
    scale_color_manual(values = c(np_color, standard_color, truth_color)) +
    theme(legend.title = element_blank(),
          legend.position = 'bottom')
  
  teacher_example <- ggplot(data = teacher_ex[teacher_id == 1]) +
    geom_point(aes(x = stud_ability_1, y = teacher_impact, color = 'True Teacher Impact'), size = 2, alpha = truth_alpha) +
    geom_point(aes(x = stud_ability_1, y = standard, color = 'Standard'), size = 2, alpha = standard_alpha) +
    geom_point(aes(x = stud_ability_1, y = binned, color = 'Binned'), size = 2, alpha = binned_alpha) + 
    #geom_point(aes(x = stud_ability_1, y = quantile), size = 2, color = quantile_color, alpha = quantile_alpha) +
    geom_point(aes(x = stud_ability_1, y = np, color = 'NP'), size = 2, alpha = np_alpha) + 
    #geom_point(aes(x = stud_ability_1, y = np1, color = 'Weighted Av.'), size = 2, alpha = np1_alpha) + 
    ylab("Teacher Impact") + 
    xlab("Student Ability") +
    ylim(ymin, ymax) +
    plot_attributes + 
    scale_color_manual(values = c(binned_color, np_color, standard_color, truth_color)) +
    theme(legend.title = element_blank(),
          legend.position = 'bottom')
  
  
  # Get the differences with each estimation method.
  teacher_ex[, st_diff := (teacher_impact - standard)]
  teacher_ex[, bin_diff := (teacher_impact - binned)]
  teacher_ex[, quant_diff := (teacher_impact - quantile)]
  teacher_ex[, np_diff := (teacher_impact - np)]
  teacher_ex[, np1_diff := (teacher_impact - np1)]
  
  # Make bins and get the average.
  teacher_ex[, bins := bin(stud_ability_1, nbins=20)]
  teacher_ex[, av_st_diff := mean(st_diff), bins]
  teacher_ex[, av_bin_diff := mean(bin_diff), bins]
  teacher_ex[, av_quant_diff := mean(quant_diff), bins]
  teacher_ex[, av_np_diff := mean(np_diff), bins]
  teacher_ex[, av_np1_diff := mean(np1_diff), bins]
  
  # Plot the raw differences.
  teacher_example_diffs <- ggplot(data = teacher_ex[teacher_id == 12]) +
    geom_point(aes(x = stud_ability_1, y = av_st_diff, color = 'Standard'), size = 2, alpha = standard_alpha) +
    geom_point(aes(x = stud_ability_1, y = av_bin_diff, color = 'Binned'), size = 2, alpha = binned_alpha) + 
    #geom_point(aes(x = stud_ability_1, y = av_quant_diff), size = 2, color = quantile_color, alpha = quantile_alpha) +
    geom_point(aes(x = stud_ability_1, y = av_np_diff, color = 'NP'), size = 2, alpha = np_alpha) + 
    geom_point(aes(x = stud_ability_1, y = av_np1_diff, color = 'Weighted Av.'), size = 2, alpha = np1_alpha) + 
    ylab("Teacher Impact") + 
    xlab("Student Ability") +
    plot_attributes + 
    scale_color_manual(values = c(binned_color, np_color, standard_color, np1_color)) +
    theme(legend.title = element_blank(),
          legend.position = 'bottom')
  
  
  # Get the weights.
  teacher_ex[, weight := ww_general_fun(weight_type  = p_weight_type,
                                        in_test_1    = stud_ability_1,
                                        lin_alpha    = p_lin_alpha,
                                        quant_val_l  = quantile(teacher_ex$test_1, probs = 0.1),
                                        quant_val_h  = quantile(teacher_ex$test_1, probs = 0.9),
                                        pctile       = NULL,
                                        weight_below = p_weight_below,
                                        weight_above = p_weight_above,
                                        v_alpha      = p_v_alpha,
                                        median_va    = median(teacher_ex$test_1),
                                        mrpctile     = p_mrpctile, 
                                        mrdist       = p_mrdist,
                                        min_score    = quantile(teacher_ex$test_1, max(p_pctile - p_mrdist, 0)),
                                        max_score    = quantile(teacher_ex$test_1, min(p_pctile + p_mrdist, 100)),
                                        pctile_val   = quantile(teacher_ex$test_1, p_pctile))]
  
  teacher_ex[, gweight := dnorm(stud_ability_1)]
  
  # Get the weighted differences.
  teacher_ex[, w_st_diff := gweight*weight*st_diff]
  teacher_ex[, w_bin_diff := gweight*weight*bin_diff]
  teacher_ex[, w_quant_diff := gweight*weight*quant_diff]
  teacher_ex[, w_np_diff := gweight*weight*np_diff]
  teacher_ex[, w_np1_diff := gweight*weight*np1_diff]
  
  # Average over bins.
  teacher_ex[, w_st_diff := mean(w_st_diff), bins]
  teacher_ex[, w_bin_diff := mean(w_bin_diff), bins]
  teacher_ex[, w_quant_diff := mean(w_quant_diff), bins]
  teacher_ex[, w_np_diff := mean(w_np_diff), bins]
  teacher_ex[, w_np1_diff := mean(w_np1_diff), bins]
  
  # Plot the weighted differences.
  teacher_example_diffsw <- ggplot(data = teacher_ex[teacher_id == 1]) +
    geom_point(aes(x = stud_ability_1, y = w_st_diff, color = 'Standard'), size = 2, alpha = standard_alpha) +
    geom_point(aes(x = stud_ability_1, y = w_bin_diff, color = 'Binned'), size = 2, alpha = binned_alpha) + 
    #geom_point(aes(x = stud_ability_1, y = w_quant_diff), size = 2, color = quantile_color, alpha = quantile_alpha) +
    geom_point(aes(x = stud_ability_1, y = w_np_diff, color = 'NP'), size = 2, alpha = np_alpha) + 
    geom_point(aes(x = stud_ability_1, y = w_np1_diff, color = 'Weighted Av.'), size = 2, alpha = np1_alpha) + 
    ylab("Teacher Impact") + 
    xlab("Student Ability") +
    plot_attributes + 
    scale_color_manual(values = c(binned_color, np_color, standard_color, np1_color)) +
    theme(legend.title = element_blank(),
          legend.position = 'bottom')
  
  
  # Get the sum of the weighted differences.
  teacher_ex[, tot_w_st_diff := sum(w_st_diff), teacher_id]
  teacher_ex[, tot_w_bin_diff := sum(w_bin_diff), teacher_id]
  teacher_ex[, tot_w_quant_diff := sum(w_quant_diff), teacher_id]
  teacher_ex[, tot_w_np_diff := sum(w_np_diff), teacher_id]
  teacher_ex[, tot_w_np1_diff := sum(w_np1_diff), teacher_id]
  
  teacher_ex[, tot_true := sum(weight*teacher_impact), teacher_id]
  teacher_ex[, tot_st := sum(weight*standard), teacher_id]
  teacher_ex[, tot_bin := sum(weight*binned), teacher_id]
  teacher_ex[, tot_quant := sum(weight*quantile), teacher_id]
  teacher_ex[, tot_np := sum(weight*np), teacher_id]
  teacher_ex[, tot_np1 := sum(weight*np1), teacher_id]
  
  # Melt the data.
  teacher_ex_collapsed <- unique(melt(data = teacher_ex[, c('teacher_id', 'tot_w_st_diff', 
                                                            'tot_w_bin_diff',
                                                            'tot_w_np_diff',
                                                            'tot_w_np1_diff')],
                                      id.vars = c('teacher_id'),
                                      measure_vars = c('tot_w_st_diff', 
                                                       'tot_w_bin_diff',
                                                       'tot_w_np_diff',
                                                       'tot_w_np1_diff')))
  
  va_collapsed <- unique(melt(data = teacher_ex[, c('teacher_id', 'tot_true',
                                                    'tot_st', 'tot_bin',
                                                    'tot_np',
                                                    'tot_np1')],
                              id.vars = c('teacher_id'),
                              measure_vars = c('tot_true',
                                               'tot_st', 'tot_bin',
                                               'tot_np',
                                               'tot_np1')))
  
  # Rename values.
  teacher_ex_collapsed[, variable := gsub('tot_w_st_diff', 'Standard', variable)]
  teacher_ex_collapsed[, variable := gsub('tot_w_bin_diff', 'Binned', variable)]
  teacher_ex_collapsed[, variable := gsub('tot_w_quant_diff', 'Quantile', variable)]
  teacher_ex_collapsed[, variable := gsub('tot_w_np_diff', 'NP', variable)]
  teacher_ex_collapsed[, variable := gsub('tot_w_np1_diff', 'Weighted Av.', variable)]
  
  teacher_ex_collapsed[, variable := gsub('tot_true', 'Truth', variable)]
  teacher_ex_collapsed[, variable := gsub('tot_st', 'Standard', variable)]
  teacher_ex_collapsed[, variable := gsub('tot_bin', 'Binned', variable)]
  teacher_ex_collapsed[, variable := gsub('tot_quant', 'Quantile', variable)]
  teacher_ex_collapsed[, variable := gsub('tot_np', 'NP', variable)]
  teacher_ex_collapsed[, variable := gsub('tot_np1', 'Weighted Av.', variable)]
  
  # Plot the sums by teacher.
  teacher_total_estimates <- ggplot(data = teacher_ex_collapsed, 
                                    aes(y=value, x=teacher_id, fill=variable, color=variable)) + 
    geom_bar(position="dodge", stat="identity") + 
    scale_color_manual(values = c(binned_color, np_color, standard_color, np1_color)) +
    scale_fill_manual(values = c(binned_color, np_color, standard_color, np1_color))
  
  # Plot the sums to see the rankings for teachers. reorder(repayment_interval, -loan_amount), loan_amount)
  ranks <- ggplot(data = va_collapsed, 
                  aes(y=value, x=variable, fill=reorder(teacher_id, -value), color=reorder(teacher_id, -value))) + 
    geom_bar(position="dodge", stat="identity") + 
    scale_color_manual(values = c(binned_color, np_color, standard_color, np1_color, 'red', 'blue', 'black', 'purple', 'pink', 'brown')) +
    scale_fill_manual(values = c(binned_color, np_color, standard_color, np1_color, 'red', 'blue', 'black', 'purple', 'pink', 'brown'))
  
  
  # Save the figures.
  ggsave(filename = paste0(out_plot, "teacher_example_just_truth_",  run_id, ".png"), 
         plot     = teacher_example_just_truth, 
         width    = 9, 
         height   = 4)
  
  ggsave(filename = paste0(out_plot, "teacher_example_truth_standard_",  run_id, ".png"), 
         plot     = teacher_example_truth_standard, 
         width    = 9, 
         height   = 4)
  
  ggsave(filename = paste0(out_plot, "teacher_example_truth_bin_",  run_id, ".png"), 
         plot     = teacher_example_truth_bin, 
         width    = 9, 
         height   = 4)
  
  ggsave(filename = paste0(out_plot, "teacher_example_truth_np_",  run_id, ".png"), 
         plot     = teacher_example_truth_np, 
         width    = 9, 
         height   = 4)
  
  ggsave(filename = paste0(out_plot, "teacher_example_",  run_id, ".png"), 
         plot     = teacher_example, 
         width    = 9, 
         height   = 4)
  
  # ggsave(filename = paste0(out_plot, "teacher_example_diff_",  run_id, ".png"), 
  #        plot     = teacher_example_diffs, 
  #        width    = 9, 
  #        height   = 4)
  # 
  # ggsave(filename = paste0(out_plot, "teacher_example_diffw_",  run_id, ".png"), 
  #        plot     = teacher_example_diffsw, 
  #        width    = 9, 
  #        height   = 4)
  # 
  # ggsave(filename = paste0(out_plot, "teacher_example_diffw_sum_",  run_id, ".png"), 
  #        plot     = teacher_total_estimates, 
  #        width    = 9, 
  #        height   = 4)
  # 
  # ggsave(filename = paste0(out_plot, "teacher_ranks_",  run_id, ".png"), 
  #        plot     = ranks,
  #        width    = 9, 
  #        height   = 4)  
  
  
  # ========================================================================= #
  # ========================= Average Teacher Figure ======================== #
  # ========================================================================= #
  
  # Simulate necessary data.
  teacher_av <- simulate_test_data(n_teacher           = p_n_teacher,
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
                                   tc_sd               = p_tc_sd)
  
  # Generate a grid over which we can get the true welfare added.
  grid <- unlist(lapply(seq(-3, 3, length.out = p_npoints), rep,
                        times =length(unique(teacher_av$teacher_id))))
  
  # Attach teacher ids.
  teacher_av <- unique(teacher_av[, c('teacher_id', 'teacher_ability',
                                      'teacher_center', 'teacher_max')])
  teacher_av <- do.call('rbind', replicate(p_npoints, teacher_av, simplify=FALSE))
  teacher_av[, grid := grid]
  
  # Get the teacher impact over that grid.
  teacher_av[, teacher_impact :=
               teacher_impact(teacher_ability  = teacher_av$teacher_ability,
                              teacher_center   = teacher_av$teacher_center,
                              teacher_max      = teacher_av$teacher_max,
                              stud_ability_1   = teacher_av$grid,
                              type             = p_impact_type,
                              func_num         = 1)] #p_impact_function)]
  
  # Get the average true impact per teacher across a grid.
  teacher_av[, av_impact := mean(teacher_impact), grid]
  
  # Save unique grid values.
  grid1 <- unique(teacher_av[, c('grid', 'av_impact')])
  
  # Put together the example teacher plot.
  teacher_average <- ggplot(data = grid1) +
    geom_point(aes(x = grid, y = av_impact), size = 2, color = av_color, alpha = av_alpha) +
    ylab("Average Teacher Impact") + 
    xlab("Student Ability") +
    ylim(min(-0.5, min(grid1$av_impact)), max(0.5, max(grid1$av_impact))) + 
    plot_attributes
  
  # Save the figure.
  ggsave(filename = paste0(out_plot, "average_teacher_example_",  run_id, ".png"), 
         plot     = teacher_average, 
         width    = 9, 
         height   = 4)
  
  
  
  # ========================================================================= #
  # ========================= Example Weight Figure ========================= #
  # ========================================================================= #
  
  # Make a data table.
  weight_ex <- as.data.table(grid)
  
  
  # Get the weights for each place in the grid.
  weight_ex[, weight := ww_general_fun(weight_type  = p_weight_type,
                                       in_test_1    = grid,
                                       lin_alpha    = p_lin_alpha,
                                       quant_val_l  = quantile(teacher_av$grid, probs = 0.1),
                                       quant_val_h  = quantile(teacher_av$grid, probs = 0.9),
                                       pctile       = NULL,
                                       weight_below = p_weight_below,
                                       weight_above = p_weight_above,
                                       v_alpha      = p_v_alpha,
                                       median_va    = median(teacher_av$grid),
                                       mrpctile     = p_mrpctile, 
                                       mrdist       = p_mrdist,
                                       min_score    = quantile(teacher_av$grid, max(p_pctile - p_mrdist, 0)),
                                       max_score    = quantile(teacher_av$grid, min(p_pctile + p_mrdist, 100)),
                                       pctile_val   = quantile(teacher_av$grid, p_pctile))]
  
  # Put together the example teacher plot.
  weight_example <- ggplot(data = weight_ex) +
    geom_point(aes(x = grid, y = weight*100000), size = 2, color = av_color, alpha = av_alpha) +
    ylab("Welfare Weight") + 
    xlab("Ex Ante Expected Performance") +
    ylim(0,4.5) + 
    plot_attributes
  
  # Save the figure.
  ggsave(filename = paste0(out_plot, "weight_example_",  run_id, ".png"), 
         plot     = weight_example, 
         width    = 9, 
         height   = 4)
  
} # Close overall loop.
