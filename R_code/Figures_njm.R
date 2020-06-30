#=======================#
# ==== Nate figures ====
#=======================#


#====================================#
# ==== set dir and load packages ====
#====================================#

# clear data.
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")

# check users
my_wd <- getwd()

# set paths based on users 
if(my_wd %like% "Nmath_000"){
  
  # source simulated data path 
  source("c:/Users/Nmath_000/Documents/Research/Heterogeneous-Teacher-Value-Added/R_code/simulate_test_data.R") 

  # source weighting funciton 
  source("c:/Users/Nmath_000/Documents/Research/Heterogeneous-Teacher-Value-Added/R_code/weighting_functions.R")
  
  # set path for monte carlo data 
  mc_path <- "c:/Users/Nmath_000/Documents/data/Value Added/"
  
  #set path for plots to save 
  out_plot <- "c:/Users/Nmath_000/Documents/data/Value Added/"
  

# tanner  
}else{
  
  source("~/Documents/Research/HeterogenousTeacherVA/Git/Heterogeneous-Teacher-Value-Added/R_code/simulate_test_data.R")
  
}

# load mc data 
mc_linear <- fread(paste0(mc_path, "linear_MC.csv"))
mc_kernal <- fread(paste0(mc_path, "mr_MC.csv"))
mc_rawlsian <- fread(paste0(mc_path, "rawlsian_MC.csv"))

# load packages and our functions 
library(data.table)
library(ggplot2)
library(xtable)

#====================#
# ==== set parms ====
#====================#

lin_alpha = 2 # For linear weights
pctile = .4 # For rawlsian weights
v_alpha = 1 # For v weights
mrpctile = .4 # For mr weights
mrdist = .2 # for mr weights
teacher_ability_drop_off = .15
teacher_va_epsilon = .1

#=========================#
# ==== set plot style ====
#=========================#

plot_attributes <- theme_classic() + 
  theme(text = element_text(size= 65),
        plot.title = element_text(vjust=0, hjust = 0.5, colour = "black",face = "bold", size =80),
        plot.subtitle = element_text(color = "black",hjust = 0.5, size =80))
  

#==================================#
# ==== Single simulation plots ====
#==================================#
set.seed(123)
r_dt <- simulate_test_data(n_schools               = 20,
                           min_stud                = 200,
                           max_stud                = 200, 
                           n_stud_per_teacher      = 30,
                           test_SEM                = .07,
                           teacher_va_epsilon      = teacher_va_epsilon,
                           teacher_ability_drop_off = teacher_ability_drop_off)
  
  #=================================#
  # ==== example teacher impact ====
  #=================================#
  # take the expected mean teacher ability and center and map their benefit to the draw of students
  E_teacher_ability <- 0
  E_Teacher_center <- 0
  n_row_dt <- nrow(r_dt)
  r_dt[, diog_teacher_impact := E_teacher_ability - abs(stud_ability_1 - E_Teacher_center)* teacher_ability_drop_off + rnorm(n_row_dt, sd = teacher_va_epsilon)]
  diog_plot1 <- ggplot(data = r_dt, aes(x= test_1, y = diog_teacher_impact)) +
    geom_point(size = 6, color = "#db7093", alpha = .5) +
    ggtitle("Example Teacher's Impact:", subtitle =  "Mean and Center = 0") +
    ylab("Expected Teacher Impact") + 
    xlab("Student Test 1") +
    plot_attributes
  
  print(diog_plot1)

  #==============================#
  # ==== mean teacher impact ====
  #==============================#
  
  # now get the mean teacher impact for each student 
  # start by getting teacher data 
  teach_data <- unique(r_dt[,c("teacher_id", "teacher_ability", "teacher_center")])
  
  # now for each student get the average teacher's impact on that student 
  in_student_ability <- .1
  ave_teach_impact_fun <- function(in_student_ability){
    
    n_rows <- length( teach_data$teacher_ability)
    
    teacher_impact_v <- teach_data$teacher_ability - abs(rep(in_student_ability,n_rows) - teach_data$teacher_center)* teacher_ability_drop_off + rnorm(n_rows, sd = teacher_va_epsilon)
    
    # get the mean 
    mean_impact <- mean(teacher_impact_v)
  }
  
  # now run it on each student to get average teacher's impact on every studnet 
  r_dt[, ave_teacher_impact := ave_teach_impact_fun(stud_ability_1), stud_id]
  
  # now plot it 
  diog_plot2 <- ggplot(data = r_dt, aes(x= test_1, y = ave_teacher_impact)) +
    geom_point(size = 6, color = "#db7093", alpha = .5) +
    ggtitle("Average Teacher Impact") +
    ylab("Average Expected teacher Impact") + 
    xlab("Student Test 1") +
    plot_attributes
  
  print(diog_plot2)

  #=============================#
  # ==== linear weight plot ====
  #=============================#

  r_dt[, linear_weights := linear_weight_fun(alpha = lin_alpha, in_test_1 = test_1)]
  
  lin_w_plot <- ggplot(data = r_dt, aes(x= test_1, y = linear_weights)) + 
    geom_point(size = 6, color = "#db7093", alpha = .5) + 
    ggtitle("Example Linear Weight") +
    ylab("Student Weight") + 
    xlab("Student Test 1") +
    plot_attributes
        
  print(lin_w_plot)      
                 
  
  
  #===============================#
  # ==== rawlsian weight plot ====
  #===============================#
  r_dt[, rawlsian_weight := rawlsian_weight_fun(pctile, test_1)]
  
  rawlsian_w_plot <- ggplot(data = r_dt, aes(x= test_1, y = rawlsian_weight)) + 
    geom_point(size = 6, color = "#db7093", alpha = .5) + 
    ggtitle("Example Rawlsian Weight") +
    ylab("Student Weight") + 
    xlab("Student Test 1") +
    plot_attributes
  
  print(rawlsian_w_plot)
  #==================================#
  # ==== kernal "mr" weight plot ====
  #==================================#
  r_dt[,  mr_weight := mr_weight_fun(mrpctile, mrdist, test_1)]

  mr_w_plot <- ggplot(data = r_dt, aes(x= test_1, y = mr_weight)) + 
    geom_point(size = 6, color = "#db7093", alpha = .5) + 
    ggtitle("Example Kernal Weight") +
    ylab("Student Weight") + 
    xlab("Student Test 1") +
    plot_attributes
  
  print(mr_w_plot)
  
  #=======================================#
  # ==== save single simulation plots ====
  #=======================================#
  
  # save plot 1 
  png(paste0(paste0(out_plot,"teacher_impact_kernal_noise.png")),
      height = 1200, width = 2100, type = "cairo")
  print(diog_plot1)
  dev.off()  
  
  # save plot 2
  png(paste0(paste0(out_plot,"Average_teacher_impact_kernal.png")),
      height = 1200, width = 2100, type = "cairo")
  print(diog_plot2)
  dev.off()  
  
  # save plot 3 
  png(paste0(paste0(out_plot,"linear_weight.png")),
      height = 1200, width = 2100, type = "cairo")
  print(lin_w_plot)
  dev.off()  
  
  png(paste0(paste0(out_plot,"rawlsian_weight.png")),
      height = 1200, width = 2100, type = "cairo")
  print(rawlsian_w_plot)
  dev.off()  
  
  png(paste0(paste0(out_plot,"Kernal_weight.png")),
      height = 1200, width = 2100, type = "cairo")
  print(mr_w_plot)
  dev.off()  
  
#===================#
# ==== MC plots ====
#===================#
  
  # fill in all the weight types 
  mc_linear[, weight_type := "Linear"]
  mc_kernal[, weight_type := "Kernal"]
  mc_rawlsian[, weight_type := "rawlsian"]
  
  for( in_data in list(mc_linear,mc_kernal, mc_rawlsian)){
    # Renormalize everything so they have the same mean and variance
    in_data[, mean_weighted_norm := (mean_weighted - mean(mean_weighted))/sd(mean_weighted)]
    in_data[, mean_standard_norm := (mean_standard - mean(mean_standard))/sd(mean_standard)]
    in_data[, true_ww := (true_ww - mean(true_ww))/sd(true_ww)]
    
    # now renormalize standard deviations 
    in_data[, sd_weighted_norm := sd_weighted/sd(mean_weighted)]
    in_data[, sd_standard_norm := sd_standard/sd(mean_standard)]
  }
  
  #===============================#
  # ==== standard caterpillar ====
  #===============================#

  # function to make standard caterpillar plots and save them 
  cat_plottR_st <- function(in_data){
    setorder(in_data, mean_standard_norm)
    in_data[, standard_id :=.I]
    in_data[, standard_lc := mean_standard_norm - 1.96*sd_standard_norm]
    in_data[, standard_uc := mean_standard_norm + 1.96*sd_standard_norm]
    weight_type <- unique(in_data$weight_type)
    
    standard_cat_plot <- ggplot(in_data, aes(x = standard_id, y = mean_standard_norm)) +
      geom_point(size = 3, color = "#db7093", alpha = 1) + 
      geom_errorbar(aes(ymin=standard_lc, ymax=standard_uc), width=.2, color = "#db7093") +
      ggtitle("Standard VA Results", subtitle = weight_type) +
      ylab("Value Added") + 
      xlab("Teacher Order") +
      ylim(-6,5)+
      plot_attributes
    
    png(paste0(paste0(out_plot,"standard_", weight_type, "_caterpillar.png")),
        height = 1500, width = 1500, type = "cairo")
    print(standard_cat_plot)
    dev.off()  
    
    return(standard_cat_plot)
    
  }

  # run this. Would be easier to lapply over a list but already set up with way...
  cat_plottR_st(mc_linear)
  cat_plottR_st(mc_kernal)
  cat_plottR_st(mc_rawlsian)
  
  
  #=========================#
  # ==== ww caterpillar ====
  #=========================#
  # function to make WW caterpillar plots and save them 
  cat_plottR_st <- function(in_data){
    setorder(in_data, mean_weighted_norm)
    in_data[, ww_id :=.I]
    in_data[, ww_lc := mean_weighted_norm - 1.96*sd_weighted_norm]
    in_data[, ww_uc := mean_weighted_norm + 1.96*sd_weighted_norm]
    weight_type <- unique(in_data$weight_type)
    
    ww_cat_plot <- ggplot(in_data, aes(x = ww_id, y = mean_weighted_norm)) +
      geom_point(size = 3, color = "#db7093", alpha = 1) + 
      geom_errorbar(aes(ymin=ww_lc, ymax=ww_uc), width=.2, color = "#db7093") +
      ggtitle("Welfare Weighted VA Results", subtitle = weight_type) +
      ylab("Weighted Value Added") + 
      xlab("Teacher Order") +
      ylim(-6,5)+
      plot_attributes
    
    png(paste0(paste0(out_plot,"ww_", weight_type, "_caterpillar.png")),
        height = 1500, width = 1500, type = "cairo")
    print(ww_cat_plot)
    dev.off()  
    
    return(ww_cat_plot)
    
  }
  
  # run this. Would be easier to lapply over a list but already set up with way...
  cat_plottR_st(mc_linear)
  cat_plottR_st(mc_kernal)
  cat_plottR_st(mc_rawlsian)
  
  #======================================#
  # ==== histogram and summary stats ====
  #======================================#
  
  histogram_sum_Stat_fun <- function(in_data){
  
    # grab weight type 
    weight_type <- unique(in_data$weight_type)
    
    # Calculate the mean squared distance from the rank of the truth
    setorder(in_data, mean_standard_norm)
    in_data[, baseline := (.I - tid)^2]
    in_data[, baseline_count := (.I - tid != 0)]
    in_data[, baseline_count_num := abs(.I - tid)]
    
    setorder(in_data, mean_weighted_norm)
    in_data[, weighted := (.I - tid)^2]
    in_data[, weighted_count := (.I - tid != 0)]
    in_data[, weighted_count_num := abs(.I - tid)]
    
    # start a data.table of results 
    sum_stats <- list()
    sum_stats[[1]] <- data.table(Statistic = "Mean Squared Distance", 
                      Standard = mean(in_data$baseline),
                      Weighted = mean(in_data$weighted))
    
    sum_stats[[2]] <- data.table(Statistic = "Mean Absolute Distance", 
                                 Standard =  mean(in_data$baseline_count_num),
                                 Weighted =  mean(in_data$weighted_count_num))
    
    sum_stats[[3]] <- data.table(Statistic = "# of Rank Inversions", 
                                 Standard =  sum(in_data$baseline_count),
                                 Weighted =sum(in_data$weighted_count))
    
    sum_stats[[4]] <- data.table(Statistic = "Correlation to Truth", 
                                 Standard =  cor(in_data$mean_standard_norm, in_data$true_ww),
                                 Weighted = cor(in_data$mean_weighted_norm, in_data$true_ww))
    
    
    out_sum_stats <- rbindlist(sum_stats)
    # save summ stats as latex table 
    print(xtable(out_sum_stats, type = "latex"),
          file = paste0(out_plot,"sum_stats_", weight_type, ".tex"),
          include.rownames = FALSE,
          floating = FALSE)

    # Histogram of the density and distance of rank inversions
    # set binwidth parm
    b_width <- 3
    
    out_histogram <- ggplot(in_data) + 
      geom_histogram( aes(baseline_count_num, fill = "Standard"), alpha = .4, colour="black", binwidth = b_width) +
      geom_histogram( aes(weighted_count_num, fill = "Weighted"), alpha = .4, colour="black", binwidth = b_width) +
      ggtitle("Deviation From True Rank",
              subtitle = weight_type) + 
      xlab("Difference in Rank From Truth")+
      scale_fill_manual(values= c("#56B4E9", "#D55E00")) +
      plot_attributes + 
      theme(legend.title = element_blank(),
            legend.position = c(0.8, 0.8),
            legend.key.size = unit(4, "cm"))
    
    # save plot and table 
    png(paste0(paste0(out_plot,"Histrogram_", weight_type, ".png")),
        height = 1100, width = 2300, type = "cairo")
    print(out_histogram)
    dev.off()  
    
    # LIST OF OUTPUT 
    out_list <- list()
    out_list[["hist"]] <- out_histogram
    out_list[["sum_stats"]] <- out_sum_stats
  }

  
  # run on different simulations 
  histogram_sum_Stat_fun(mc_linear)
  histogram_sum_Stat_fun(mc_kernal)
  histogram_sum_Stat_fun(mc_rawlsian)
  
    