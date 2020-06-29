#=======================#
# ==== Nate figures ====
#=======================#


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

  # set path for monte carlo data 
  
  #set path for plots to save 
  out_plot <- "c:/Users/Nmath_000/Documents/data/Value Added/"
  

# tanner  
}else{
  
  source("~/Documents/Research/HeterogenousTeacherVA/Git/Heterogeneous-Teacher-Value-Added/R_code/simulate_test_data.R")
  
}

# load packages and our functions 
library(data.table)
library(ggplot2)


#=========================#
# ==== set plot style ====
#=========================#

plot_attributes <- theme_classic() + 
  theme(text = element_text(size= 65),
        plot.title = element_text(vjust=0, hjust = 0.5, colour = "black",face = "bold", size =80),
        plot.subtitle = element_text(color = "black",hjust = 0.5, size =80),
        legend.title = element_text( colour = "black", size = 65))

#==================================#
# ==== Single simulation plots ====
#==================================#
teacher_ability_drop_off = .15
teacher_va_epsilon = .1
r_dt <- simulate_test_data(n_schools               = 20,
                           min_stud                = 200,
                           max_stud                = 200, 
                           n_stud_per_teacher      = 30,
                           test_SEM                = .07,
                           teacher_va_epsilon      = .1,
                           teacher_ability_drop_off = .15)
  
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

  
  #===============================#
  # ==== rawlsian weight plot ====
  #===============================#

  #==================================#
  # ==== kernal "mr" weight plot ====
  #==================================#


  #=======================================#
  # ==== save single simulation plots ====
  #=======================================#
  
  # ggsave("c:/Users/Nmath_000/Documents/data/Value Added/teacher_impact_kernal_noise.png")
  png(paste0(paste0(out_plot,"teacher_impact_kernal_noise.png")),
      height = 1200, width = 2100, type = "cairo")
  print(diog_plot1)
  dev.off()  
  
  png(paste0(paste0(out_plot,"Average_teacher_impact_kernal.png")),
      height = 1200, width = 2100, type = "cairo")
  print(diog_plot2)
  dev.off()  
  
  