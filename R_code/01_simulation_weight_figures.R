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


# NOTE!!!!!!!! make sure you have these directories created in your out_plot
# example_teacher_plots
# Average_teacher_plots
# weight_plots
#====================================#
# ==== set dir and load packages ====
#====================================#

# load packages and our functions 
library(data.table)
library(ggplot2)
library(xtable)
library(gridExtra)

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
  
  #set path for plots to save. MAKE SURE YOU HAVE DIRECTORIES CREATED!!!!!!!!!
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
  theme(text = element_text(size= 20),
        plot.title = element_text(vjust=0, hjust = 0.5, colour = "black",face = "bold", size =25),
        plot.subtitle = element_text(color = "black",hjust = 0.5, size =25))



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
    
    # make plot 
    r_dt[, diog_teacher_impact := E_teacher_ability -pmin(abs(stud_ability_1 - E_Teacher_center), 2)* p_teacher_ability_drop_off + rnorm(n_row_dt, sd = p_teacher_va_epsilon)]
    
    et_impact <- ggplot(data = r_dt, aes(x= test_1, y = diog_teacher_impact)) +
      geom_point(size = 2, color = "#ffaabb", alpha = .5) +
      ylab("Teacher's Impact") + 
      xlab("Ex Ante Expected Performance") +
      ggtitle("Example Teacher's Impact") +
      plot_attributes

    # make table of parameters 
    parms_tab <- melt.data.table(teach_plot_xwalk[i], measure.vars = colnames(teach_plot_xwalk))
    
    # put parameters in with plot
    parms_tbl <- tableGrob(parms_tab, rows=NULL, cols = NULL, theme = ttheme_default(base_size = 9))
    et_impact2 <- grid.arrange(et_impact, parms_tbl,
                            layout_matrix = rbind(c(1, 1, NA),
                                                  c(1, 1, 2),
                                                  c(1, 1, NA)))
    # Make file name 
    file_name <- paste0(out_plot,
                        "example_teacher_plots/ET_",
                        p_n_teacher, "_",
                        p_n_stud_per_teacher, "_",
                        p_teacher_va_epsilon, "_",
                        p_teacher_ability_drop_off, "_",
                        p_test_SEM)
    file_name <- gsub("\\.", "", file_name)
    file_name <- paste0(file_name, ".png")
    # save it 
    ggsave(filename = file_name, plot = et_impact2, width = 9, height = 4)
    
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
    at_impact <- ggplot(data = r_dt, aes(x= test_1, y = ave_teacher_impact)) +
      geom_point(size = 2, color = "#ffaabb", alpha = .75) +
      ylab("Average Impact") + 
      xlab("Ex Ante Expected Performance") +
      ggtitle("Average Teacher Impact") +
      plot_attributes
    
    # put it in table with parameters 
    at_impact2 <- grid.arrange(at_impact, parms_tbl,
                               layout_matrix = rbind(c(1, 1, NA),
                                                     c(1, 1, 2),
                                                     c(1, 1, NA)))
    # Make file name 
    file_name <- paste0(out_plot,
                        "Average_teacher_plots/AT_",
                        p_n_teacher, "_",
                        p_n_stud_per_teacher, "_",
                        p_teacher_va_epsilon, "_",
                        p_teacher_ability_drop_off, "_",
                        p_test_SEM)
    file_name <- gsub("\\.", "", file_name)
    file_name <- paste0(file_name, ".png")
    
    # save it 
    ggsave(filename = file_name, plot = at_impact2, width = 9, height = 4)
    
    
  
} # close loop over teacher xwalk 

#=======================#
# ==== weight plots ====
#=======================#

# simulate data for the weights 
#NOTE: None of the teacher related inputs are going to matter here 
# we just want a good discribution of students and student abilities 
r_dt <- simulate_test_data(n_teacher                = 140,
                           n_stud_per_teacher       = 100,
                           test_SEM                 = .07,
                           teacher_va_epsilon       = .05,
                           teacher_ability_drop_off = .15)

# get parms for weight plots
weight_parms <- c("weight_type","lin_alpha", "pctile", "pctile", "v_alpha", "mrpctile", "mrdist")

# start by subsetting to parms for this section and removing duplicates 
weight_plot_xwalk <- model_xwalk[,weight_parms, with = FALSE]
weight_plot_xwalk <- unique(weight_plot_xwalk)

# now loop over all unique combos in the xwalk 
for(i in 1:nrow(weight_plot_xwalk)){
  
  p_weight_type              <- weight_plot_xwalk[i, weight_type]
  p_lin_alpha                <- weight_plot_xwalk[i, lin_alpha] # For linear weights
  p_pctile                   <- weight_plot_xwalk[i, pctile] # for rawlsian 
  p_v_alpha                  <- weight_plot_xwalk[i, v_alpha]# For v weights
  p_mrpctile                 <- weight_plot_xwalk[i, mrpctile] # For mr weights
  p_mrdist                   <- weight_plot_xwalk[i, mrdist] # for mr weights
  
  #=============================#
  # ==== linear weight plot ====
  #=============================#
  if(p_weight_type == "linear"){
    
    r_dt[, linear_weights := linear_weight_fun(alpha = p_lin_alpha, in_test_1 = test_1)]
    
    weight_plot <- ggplot(data = r_dt, aes(x= test_1, y = linear_weights)) + 
      geom_point(size = 2, color = "#ffaabb", alpha = .75) + 
      ylab("Welfare Weight") +
      xlab("Ex Ante Expected Performance") + xlim(-4,4) +
      ggtitle(paste0("Linear Weight with ", p_lin_alpha, " slope")) +
      plot_attributes
    
    file_name <- paste0(out_plot,
                        "weight_plots/linear_",
                        p_lin_alpha)
    file_name <- gsub("\\.", "", file_name)
    file_name <- paste0(file_name, ".png")
    ggsave(filename = file_name, plot = weight_plot, width = 9, height = 4)
    
  }
  
  #===============================#
  # ==== rawlsian weight plot ====
  #===============================#
  if(p_weight_type == "rawlsian"){
    r_dt[, rawlsian_weight := rawlsian_weight_fun(p_pctile, test_1)]
    
    weight_plot <- ggplot(data = r_dt, aes(x= test_1, y = rawlsian_weight)) + 
      geom_point(size = 2, color = "#ffaabb", alpha = .75) + 
      ylab("Welfare Weight") + 
      scale_y_continuous(breaks = seq(0, 1, by = .25), limits = c(0,1.02)) +
      xlab("Ex Ante Expected Performance") + xlim(-4,4) +
      ggtitle(paste0("Rawlsian Weight at ", p_pctile, " Percentile")) +
      plot_attributes
    
    file_name <- paste0(out_plot,
                        "weight_plots/rawlsian_",
                        p_pctile)
    file_name <- gsub("\\.", "", file_name)
    file_name <- paste0(file_name, ".png")
    ggsave(filename = file_name, plot = weight_plot, width = 9, height = 4)
    
  }
  #==================================#
  # ==== kernal "mr" weight plot ====
  #==================================#
  if(p_weight_type == "mr"){
    
    r_dt[,  mr_weight := mr_weight_fun(p_mrpctile, p_mrdist, test_1)]
    
    weight_plot <- ggplot(data = r_dt, aes(x= test_1, y = mr_weight)) + 
      geom_point(size = 2, color = "#ffaabb", alpha = .75) + 
      ylab("Welfare Weight") + ylim(0,1) +
      xlab("Ex Ante Expected Performance") + xlim(-4,4) +
      ggtitle(paste0("MR Weight at ", p_mrpctile, " Percenitle and ", p_mrdist, " dist")) +
      plot_attributes
    
    file_name <- paste0(out_plot,
                        "weight_plots/mr_",
                        p_mrpctile, "_",
                        p_mrdist)
    file_name <- gsub("\\.", "", file_name)
    file_name <- paste0(file_name, ".png")
    ggsave(filename = file_name, plot = weight_plot, width = 9, height = 4)
    
  }
  
  #========================#
  # ==== V weight plot ====
  #========================#

  if(p_weight_type == "v"){
    
    r_dt[,  v_weight := v_weight_fun(p_v_alpha, test_1)]
    
    weight_plot <- ggplot(data = r_dt, aes(x= test_1, y = v_weight)) + 
      geom_point(size = 2, color = "#ffaabb", alpha = .75) + 
      ylab("Welfare Weight")  +
      xlab("Ex Ante Expected Performance") + xlim(-4,4) +
      ggtitle(paste0("V Weight with Alpha of ", p_v_alpha)) +
      plot_attributes
    
    file_name <- paste0(out_plot,
                        "weight_plots/v_",
                        p_v_alpha)
    file_name <- gsub("\\.", "", file_name)
    file_name <- paste0(file_name, ".png")
    ggsave(filename = file_name, plot = weight_plot, width = 9, height = 4)
    
  }
  
  
} # close the loop 
