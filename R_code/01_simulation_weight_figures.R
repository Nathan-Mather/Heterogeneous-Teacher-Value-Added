#====================================#
# ==== single simulation figures ====
#====================================#

# to do here is to set this up to run with a xwalk and save in a more progromatic way. 

# clear data.
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
# no scientific notation 
options(scipen = 999)
# clean console history 
cat("\f")


#====================================#
# ==== set dir and load packages ====
#====================================#

# load packages and our functions 
library(data.table)
library(ggplot2)
library(xtable)

# check users. (NOTE TO MIKE, add something unique to your base working directory to detect when it is your computer)
my_wd <- getwd()
if(my_wd %like% "Nmath_000"){
  # base directory 
  base_path <- "c:/Users/Nmath_000/Documents/Research/"
  
  # set path for monte carlo data 
  mc_path <- "c:/Users/Nmath_000/Documents/data/Value Added/mc_data/"
  
  # set path for stress test data 
  stress_path <- "c:/Users/Nmath_000/Documents/data/Value Added/MC_stress_test/"
  
  #set path for plots to save 
  out_plot <- "c:/Users/Nmath_000/Documents/data/Value Added/"
  
  
}else{
  # base directory 
  base_path <- "~/Documents/Research/HeterogenousTeacherVA/Git/"
  
  # set path for monte carlo data 
  mc_path <- ""
  
  # set path for stress test data 
  stress_path <- ""
  
  #set path for plots to save 
  out_plot <- ""
  
  
}

# load our functions now that we have a file path 
func_path <- "Heterogeneous-Teacher-Value-Added/R_code/functions/"
source(paste0(base_path, func_path, "simulate_test_data.R"))
source(paste0(base_path, func_path, "weighting_functions.R"))

# load mc data 
mc_linear <- fread(paste0(mc_path, "linear100Student_MC.csv"))
mc_kernal <- fread(paste0(mc_path, "mr100Student_MC.csv"))
mc_rawlsian <- fread(paste0(mc_path, "rawlsian100Students_MC.csv"))

# load model_xwalk 
model_xwalk <- data.table(read_excel(paste0(base_path, "Heterogeneous-Teacher-Value-Added/R_code/model_xwalk.xlsx")))

# set seed if we want to 
set.seed(123)

#=========================#
# ==== set plot style ====
#=========================#

plot_attributes <- theme_classic() + 
  theme(text = element_text(size= 65),
        plot.title = element_text(vjust=0, hjust = 0.5, colour = "black",face = "bold", size =80),
        plot.subtitle = element_text(color = "black",hjust = 0.5, size =80))



#========================#
# ==== Teacher plots ====
#========================#

# get parms for teacher plot 
teach_parms <- c("n_teacher", "n_stud_per_teacher", "teacher_va_epsilon", "teacher_ability_drop_off", "test_SEM")

# start by subsetting to parms for this section and removing duplicates 
teach_plot_xwalk <- model_xwalk[,teach_parms, with = FALSE]
teach_plot_xwalk <- unique(teach_plot_xwalk)

# now loop  over unique combinations of these variables 
for(i in 1:nrow(teach_plot_xwalk)){
  
  p_n_teacher                <- teach_plot_xwalk[i, n_teacher]
  p_n_stud_per_teacher       <- teach_plot_xwalk[i, n_stud_per_teacher]
  p_teacher_va_epsilon       <- teach_plot_xwalk[i, teacher_va_epsilon]
  p_teacher_ability_drop_off <- teach_plot_xwalk[i, teacher_ability_drop_off]
  p_test_SEM                 <- teach_plot_xwalk[i, test_SEM]
  
    
    #========================#
    # ==== Simulate data ====
    #========================#
    r_dt <- simulate_test_data(n_teacher                = p_n_teacher,
                               n_stud_per_teacher       = p_n_stud_per_teacher,
                               test_SEM                 = p_test_SEM,
                               teacher_va_epsilon       = p_teacher_va_epsilon,
                               teacher_ability_drop_off = p_teacher_ability_drop_off)
    
    #=================================#
    # ==== example teacher impact ====
    #=================================#
    # take the expected mean teacher ability and center and map their benefit to the draw of students
    E_teacher_ability <- 0
    E_Teacher_center <- 0
    n_row_dt <- nrow(r_dt)
    
    r_dt[, diog_teacher_impact := E_teacher_ability -pmin(abs(stud_ability_1 - E_Teacher_center), 2)* p_teacher_ability_drop_off + rnorm(n_row_dt, sd = p_teacher_va_epsilon)]
    diog_plot1 <- ggplot(data = r_dt, aes(x= test_1, y = diog_teacher_impact)) +
      geom_point(size = 6, color = "#ffaabb", alpha = .75) +
      ylab("Example Teacher's Impact") + 
      xlab("Ex Ante Expected Performance") +
      plot_attributes

    
    #==============================#
    # ==== mean teacher impact ====
    #==============================#
    
    # now get the mean teacher impact for each student 
    # start by getting teacher data 
    teach_data <- unique(r_dt[,c("teacher_id", "teacher_ability", "teacher_center")])
    
    # now for each student get the average teacher's impact on that student 
    ave_teach_impact_fun <- function(in_student_ability){
      
      n_rows <- length( teach_data$teacher_ability)
      
      teacher_impact_v <- teach_data$teacher_ability -  pmin(abs(rep(in_student_ability,n_rows) - teach_data$teacher_center), 2)* p_teacher_ability_drop_off + rnorm(n_rows, sd = p_teacher_va_epsilon)
      
      # get the mean 
      mean_impact <- mean(teacher_impact_v)
    }
    
    # now run it on each student to get average teacher's impact on every studnet 
    r_dt[, ave_teacher_impact := ave_teach_impact_fun(stud_ability_1), stud_id]
    
    # now plot it 
    diog_plot2 <- ggplot(data = r_dt, aes(x= test_1, y = ave_teacher_impact)) +
      geom_point(size = 6, color = "#ffaabb", alpha = .75) +
      ylab("Average Teacher Impact") + 
      xlab("Ex Ante Expected Performance") +
      plot_attributes
  
} # close loop over teacher xwalk 
#=======================#
# ==== weight plots ====
#=======================#
  
  # get parms for teacher plot 
  
  p_pctile = 2 # For linear weights
  p_pctile = .4 # For rawlsian weights
  p_v_alpha = 1 # For v weights
  p_mrpctile = .3 # For mr weights
  p_mrdist = .2 # for mr weights
  
  p_weight_type              <- model_xwalk[i, weight_type]
  p_lin_alpha                <- model_xwalk[i, lin_alpha] # For linear weights
  p_pctile                   <- model_xwalk[i, pctile] # for rawlsian 
  p_v_alpha                  <- model_xwalk[i, v_alpha]# For v weights
  p_mrpctile                 <- model_xwalk[i, mrpctile] # For mr weights
  p_mrdist                   <- model_xwalk[i, mrdist] # for mr weights
  
  
  
  
  #=============================#
  # ==== linear weight plot ====
  #=============================#
  
  r_dt[, linear_weights := linear_weight_fun(alpha = p_pctile, in_test_1 = test_1)]
  
  lin_w_plot <- ggplot(data = r_dt, aes(x= test_1, y = linear_weights)) + 
    geom_point(size = 6, color = "#ffaabb", alpha = .75) + 
    ylab("Welfare Weight") +
    xlab("Ex Ante Expected Performance") + xlim(-4,4) +
    plot_attributes
  
  print(lin_w_plot)      
  
  
  #===============================#
  # ==== rawlsian weight plot ====
  #===============================#
  r_dt[, rawlsian_weight := rawlsian_weight_fun(p_pctile, test_1)]
  
  rawlsian_w_plot <- ggplot(data = r_dt, aes(x= test_1, y = rawlsian_weight)) + 
    geom_point(size = 6, color = "#ffaabb", alpha = .75) + 
    ylab("Welfare Weight") + 
    scale_y_continuous(breaks = seq(0, 1, by = .25), limits = c(0,1.02)) +
    xlab("Ex Ante Expected Performance") + xlim(-4,4) +
    plot_attributes
  
  print(rawlsian_w_plot)
  #==================================#
  # ==== kernal "mr" weight plot ====
  #==================================#
  r_dt[,  mr_weight := mr_weight_fun(p_mrpctile, p_mrdist, test_1)]
  
  mr_w_plot <- ggplot(data = r_dt, aes(x= test_1, y = mr_weight)) + 
    geom_point(size = 6, color = "#ffaabb", alpha = .75) + 
    ylab("Welfare Weight") + ylim(0,1) +
    xlab("Ex Ante Expected Performance") + xlim(-4,4) +
    plot_attributes
  
  print(mr_w_plot)

#=======================================#
# ==== save single simulation plots ====
#=======================================#
# # save plot 1 
# png(paste0(paste0(out_plot,"teacher_impact_kernal_noise.png")),
#     height = 1200, width = 2100, type = "cairo")
# print(diog_plot1)
# dev.off()  
# # save plot 2
# 
# png(paste0(paste0(out_plot,"Average_teacher_impact_kernal.png")),
#     height = 1200, width = 2100, type = "cairo")
# print(diog_plot2)
# dev.off()  
# 
# # save plot 3 
# png(paste0(paste0(out_plot,"linear_weight.png")),
#     height = 1200, width = 2100, type = "cairo")
# print(lin_w_plot)
# dev.off() 
# 
# # save plot 6
# png(paste0(paste0(out_plot,"rawlsian_weight.png")),
#     height = 1200, width = 2100, type = "cairo")
# print(rawlsian_w_plot)
# dev.off()  
# 
# png(paste0(paste0(out_plot,"Kernal_weight.png")),
#     height = 1200, width = 2100, type = "cairo")
# print(mr_w_plot)
# dev.off() 