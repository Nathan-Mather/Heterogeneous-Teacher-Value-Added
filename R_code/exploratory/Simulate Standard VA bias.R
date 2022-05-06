#======================================#
# ==== Value added Bias simulation ====
#======================================#

rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")

# load packages 
library(data.table)
library(readxl)

# set WD and file paths 
# Check users to set directory.
#  (NOTE TO MIKE, add something unique to your base working directory to detect
#   when it is your computer)
my_wd <- getwd()
if (my_wd %like% "Nmath_000") {
  # Base directory. 
  base_path <- "c:/Users/Nmath_000/Documents/Research/"
  
  # Path for data to save.
  out_data <- "C:/Users/Nmath_000/Documents/Research/Value_added_local/VA_bais_sim/"
  
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
                                            "Heterogeneous-Teacher-Value-Added/R_code/model_xwalk_VA_bias_sim.xlsx")))


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



# plot attributes 
plot_attributes <- theme_classic() + 
  theme(text = element_text(size= 15),
        plot.title = element_text(vjust=0,
                                  hjust = 0.5,
                                  colour = "black",
                                  face = "bold",
                                  size =12),
        plot.subtitle = element_text(color = "black",
                                     hjust = 0.5,
                                     size =12))

#=========================#
# ==== set parameters ====
#=========================#

#eventualy will be a loop. For now just grab the base case 
i <- 1
run_id_i <- 1

# Set parameters for this Monte Carlo run.
# Run parameters.
nsims                      <- model_xwalk[i, nsims]              # how many simulations to do
p_npoints                  <- model_xwalk[i, npoints]            # number of grid points over which to calculate welfare added

# Simulated data parameters.
p_impact_type              <- model_xwalk[i, impact_type]        # one of 'MLRN', 'MLR', 'MNoR', 'MNo', 'No'
p_impact_function          <- model_xwalk[i, impact_function]    # which teacher impact function to use, and integer
p_min_diff                 <- model_xwalk[i, min_diff]           # minimum impact difference between best and worst matched students
p_max_diff                 <- model_xwalk[i, max_diff]           # maximum impact difference between best and worst matched students
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
p_weighted_average         <- model_xwalk[i, weighted_average]   # whether or not to calculate a weighted average of standard and NP
p_num_cats                 <- model_xwalk[i, num_cats]          # number of bins for binned estimator 



#========================#
# ==== generate data ====
#========================#

  # parameters used here, can add to xwalk 
  n_schools <- 10
  n_teacer_per_school <- 9
  n_studs_per_class <- 30
  n_grdes           <- 3
  max_comp_adv      <- 0
  qc_flag <- 1
  
  #=================================#
  # ==== generate teacher xwalk ====
  #=================================#
    # generate school ids 
    school_id <- c(school_id = 1:n_schools)
    
    # make teacher id's per school 
    teacher_id <- c(school_id = 1:n_teacer_per_school)
    
    teacher_student_xwalk <- data.table(expand.grid(school_id = school_id,teacher_id = teacher_id))
    
    
    # assign gades 
    bin_breaks <- seq(0,n_teacer_per_school, length.out = n_grdes+1)
   
    teacher_student_xwalk[, grade := cut(teacher_id, breaks = bin_breaks, labels = FALSE)]
    
    # sanity check 
    teacher_student_xwalk[, .N, c("grade", "teacher_id")]
    
    # asign studs 
    teacher_student_xwalk[, n_studs := n_studs_per_class]
    
    
    # create a unique teacher id 
    teacher_student_xwalk[, teacher_id := paste0(school_id, teacher_id)]

    #================================#
    # ==== teacher ability xwalk ====
    #================================#
    
    # Simulate teacher data #note: loacted in funcitons/simulate_sdusd_data 
    teacher_ability_xwalk <- simulate_teacher_ability(teacher_student_xwalk = teacher_student_xwalk,
                                                      ta_sd                   = p_ta_sd,
                                                      school_cor              = 0,
                                                      tc_sd                   = p_tc_sd,
                                                      min_diff                = p_min_diff,
                                                      max_diff                = p_max_diff)
 
    
    
    
#======================================#
# ==== generate class/student data ====
#======================================#

  # set sd of distribution of class averages to do
    class_ave_sd_list <- seq(0, 2, .2)
    
  # get a list of student data sets 
  stud_data_list <- vector("list", length = length(class_ave_sd_list) + 1)
  
  names(stud_data_list) <- c("identical", class_ave_sd_list)

    #============================#
    # ==== identical classes ====
    #============================#

    # simulate students equal sized identical classes 
    sample_class <-  data.table(stud_id = 1:40)
    sample_class[, stud_ability_1 := rnorm(.N, mean = 0, sd = 1)]
    sample_class[, m_id := 1]
    teacher_ability_xwalk[, m_id := 1]
    
    # merge sample class to every teacher 
    r_dt <- merge(teacher_ability_xwalk, sample_class, "m_id", allow.cartesian = TRUE)
    r_dt[, stud_id := paste0(teacher_id, "_", stud_id)]
    
    r_dt[, class_ave := mean(stud_ability_1), teacher_id]
    
    # put it in list 
    stud_data_list[["identical"]] <- r_dt

    #=========================#
    # ==== random classes ====
    #=========================#

    # loop over class sds 
    for(class_ave_sd in class_ave_sd_list){
    
    # get a distribution of classroom averages based on how much variance we want 
      class_dist <-  rnorm(nrow(teacher_ability_xwalk), mean = 0, sd = class_ave_sd)
    
      # now generate students based on that 
      students <- lapply(class_dist, rnorm, n = 40, sd = 1)
      
      # put it into a data table with a loop I guess 
      data_list <- vector("list", length = length(students))
      for(i in 1:length(students)){
        teach_i <- teacher_ability_xwalk$teacher_id[[i]]
        
        data_list[[i]] <- data.table(stud_ability_1 = students[[i]], teacher_id = teach_i)
      }

      stud_ability <- rbindlist(data_list)
      stud_ability[, stud_id := 1:.N]
      
      r_dt <- merge(teacher_ability_xwalk, stud_ability, "teacher_id")
      
      # put observed class overages in there 
      r_dt[, class_ave := mean(stud_ability_1), teacher_id]
      
      # put dataset in list 
      stud_data_list[[as.character(class_ave_sd)]] <- r_dt
    }

    
    
#=====================#
# ==== unweighted ====
#=====================#
    unweighted_data_list <- vector("list", length = length(stud_data_list) + 1)
  
  #==========================#
  # ==== true unweighted ====
  #==========================#
    # get teacher unweighted welfare, on mean classroom 
    standard_true_ave_class <- welfare_statistic(in_dt           = teacher_ability_xwalk,
                                                 type            = 'true',
                                                 npoints         = 10000,
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
    
    
    setnames(standard_true_ave_class, "true_welfare", "value")
    standard_true_ave_class[, statistic := "standard_avaerage_class"]
    unweighted_data_list[[1]] <- standard_true_ave_class
  
  #===============================#
  # ==== classroom unweighted ====
  #===============================#
    
    # now loop over class types 
    for(i in 1:length(stud_data_list)){
      
      r_dt <- stud_data_list[[i]]
      class_ave_sd <- names(stud_data_list[i])
     #  now get teacher impact 
      r_dt[, teacher_impact :=
             teacher_impact(teacher_ability  = r_dt$teacher_ability,
                            teacher_center   = r_dt$teacher_center,
                            teacher_max      = r_dt$teacher_max,
                            stud_ability_1   = r_dt$stud_ability_1,
                            type             = p_impact_type,
                            func_num         = p_impact_function)]
      
      standard_true_actual_class <- r_dt[, .("value" = mean(teacher_impact),
                                             "class_ability_ave" = unique(class_ave)), teacher_id]
      standard_true_actual_class[, statistic := "standard_actual_class"]
      standard_true_actual_class[, class_ave_sd := class_ave_sd ]
    

      unweighted_data_list[[i + 1]] <- standard_true_actual_class
      
      
    }
    
    
    

    
    
#=======================#
# ==== weights loop ====
#=======================#
    
  # get a set of weights to try 
  weight_above_list <- seq(0,1,.1)
  weight_below_list <- rev(weight_above_list)
  
  # list for  results 
  long_res_list <- vector("list", length= length(weight_above_list) + length(weight_above_list) * length(stud_data_list))

# loop over this for a bunch of wekght values to make charts 
for(i in 1:length(weight_below_list)){
  p_weight_above <- weight_above_list[[i]]
  p_weight_below <- weight_below_list[[i]]  
    
    # get teachers true welfare impact with weights, on mean classroom 
    ww_true_ave_class <- welfare_statistic(in_dt           = teacher_ability_xwalk,
                                            type            = 'true',
                                            npoints         = 10000,
                                            weight_type     = p_weight_type,
                                            in_test_1       = NULL,
                                            lin_alpha       = p_lin_alpha,
                                            pctile          = p_pctile,
                                            weight_below    = p_weight_below,
                                            weight_above    = p_weight_above,
                                            impact_type     = p_impact_type,
                                            impact_function = p_impact_function,
                                            qc_flag         = 0,
                                            out_qc_path     = 0,
                                            run_id_qc       = run_id_i)
   

      
      # make long output 
      setnames(ww_true_ave_class, "true_welfare", "value")
      ww_true_ave_class[, statistic := "weighted_average_class"]

      ww_true_ave_class[, weight_above := as.character(p_weight_above)]
      ww_true_ave_class[, weight_below := as.character(p_weight_below)]
    
      long_res_list[[ 1 + (length(stud_data_list) + 1)*(i-1) ]] <- ww_true_ave_class
      
    
      #===============================================#
    # ==== loop through class composition types ====
    #===============================================#
      for(j in 1:length(stud_data_list)){
        
         #grab student data 
        r_dt <- stud_data_list[[j]]
        class_ave_sd <- names(stud_data_list[j])
        
        # now get their true weighted impact on their actual class
        r_dt[, weight := ww_general_fun(weight_type  = p_weight_type,
                                        in_test_1    = r_dt$stud_ability_1,
                                        lin_alpha    = p_lin_alpha,
                                        quant_val_l  = quantile(r_dt$stud_ability_1, probs = 0.1),
                                        quant_val_h  = quantile(r_dt$stud_ability_1, probs = 0.9),
                                        pctile       = NULL,
                                        weight_below = p_weight_below,
                                        weight_above = p_weight_above,
                                        v_alpha      = p_v_alpha,
                                        median_va    = median(r_dt$stud_ability_1),
                                        mrpctile     = p_mrpctile, 
                                        mrdist       = p_mrdist,
                                        min_score    = quantile(r_dt$stud_ability_1, max(p_pctile - mrdist, 0)),
                                        max_score    = quantile(r_dt$stud_ability_1, min(p_pctile + mrdist, 100)),
                                        pctile_val   = quantile(r_dt$stud_ability_1, p_pctile))]
        
        
        # now get their true weighted impact on their acutal class 
        weighted_true_actual_class <- r_dt[, .("value" = stats::weighted.mean(teacher_impact, weight),
                                               "class_ability_ave" = unique(class_ave)), teacher_id]

        weighted_true_actual_class[, statistic := "weighted_actual_class"]
        weighted_true_actual_class[, class_ave_sd := class_ave_sd ]
        
        weighted_true_actual_class[, weight_above := as.character(p_weight_above)]
        weighted_true_actual_class[, weight_below := as.character(p_weight_below)]
        long_res_list[[ 1 + (length(stud_data_list) + 1)*(i-1) + j]] <- weighted_true_actual_class
        
        
        
      }

   

}

  
  #============================#
  # ==== bind all the data ====
  #============================#

  long_results <- rbindlist(unweighted_data_list, fill = TRUE)
  long_results2 <- rbindlist(long_res_list, fill = TRUE)
  long_results <- rbind(long_results, long_results2, fill = TRUE)
  
  
  #=========================#
  # ==== make new plots ====
  #=========================#

  # Unweighted vs weigted average class, different weights 
  #this is to show the impact of the weights on identical classes 
  
  # unweighted average class vs random classes 
  # this is to show what happens when we introduce random classes to standard VA. 
  # should just crank up noise 
  
  # weighted average class vs random classes 
  # This is to show what happens to weignted VA with random classes. 
  # shuold crank up noise even more 
  
  # weighted average class vs unweighted normal class
  # this shows what is actually being estimated (without noise) in each case 
  # should show that standard is biased in this situation 
    plot_dt <- long_results[statistic %chin% c("weighted_average_class", "standard_actual_class")]
    plot_dt_w <- long_results[statistic == "weighted_average_class"]
    plot_dt_s <- long_results[statistic == "standard_actual_class"]

    # start with .50-.50 wights and look over class distributions 
    plot_dt_w1 <- plot_dt_w[weight_above == .5]
    plot_dt_w1 <- plot_dt_w1[, c("teacher_id", "value")]
    setnames(plot_dt_w1, "value", "weighted_average_class")
    plot_dt_s <- plot_dt_s[class_ave_sd != "identical"]
    
    plot_dt <- merge(plot_dt_s, plot_dt_w1, "teacher_id")
    
    
  
    weights_plot <- ggplot(plot_dt, aes(x=weighted_average_class, y=value, color = class_ave_sd, group = class_ave_sd)) + 
      geom_point() +
      geom_abline(intercept = 0, slope = 1) +
      geom_smooth(method = "lm", se = FALSE) +
      facet_wrap(~class_ave_sd) +
      xlab("Equal Weight Ave class Welfare Impact") + 
      ylab("Standard Class Impact") +
      labs(color = "Sd of Class Mean") + 
      plot_attributes +
      ggtitle("Ave Welfare weighted vs Class Standard VA Over Class Variance")
    
    weights_plot
    
    # now for a high variance example, lets see the difference in scores over class mean 
    plot_dt2 <- plot_dt[class_ave_sd == 2]
    plot_dt2[, standard_va_bias := weighted_average_class - value]
    
    bias_plot <- ggplot(plot_dt2, aes(x = class_ability_ave, y = standard_va_bias )) + 
      geom_point() +
      plot_attributes
    
    bias_plot
    
      # unweighted vs weighted, random classes, different weights 
  # this is to show 
  
  
  
  
  
  
 #========================#
 # ==== make old plot ====
 #========================#


  # get what you need 
  plot_dt <- long_results[is.na(class_ave_sd)]
  standard <- plot_dt[statistic == "standard_avaerage_class"]
  weighted <- plot_dt[statistic != "standard_avaerage_class"]
  setnames(weighted)
  
  # not done yet   

  # impact of weights when there is hetorogeniety in teacher impacts 
  weights_plot <- ggplot(res_w, aes(x=true_weighted_welfare, y=true_unweightd_welfare, color = weight_above, group = weight_above)) + 
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~weight_above) +
    xlab("Rawlsian Weighted Welfare Impact") + 
    ylab("Unweight Welfare Impact") +
    labs(color = "Weight Above Mean") + 
    ggtitle("Vary WW, heterogenious impact, Truth (average class)")
   
  weights_plot
  
  
  ggsave(filename = paste0(out_data,"Changing_ww_hetero_basline.png"), 
         plot     = weights_plot, 
         width    = 9, 
         height   = 4)    
  