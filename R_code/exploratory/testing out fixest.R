#=====================#
# ==== test feols ====
#=====================#

library(data.table)
library(fixest)
library(readxl)
library(plm)
library(lfe)

# Check users to set directory.
#  (NOTE TO MIKE, add something unique to your base working directory to detect
#   when it is your computer)
my_wd <- getwd()
if (my_wd %like% "Nmath_000") {
  # Base directory. 
  base_path <- "c:/Users/Nmath_000/Documents/Research/"
  
  # Path for data to save.
  out_data <- "c:/Users/Nmath_000/Documents/data/Value Added/mc_data/"
  
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
                                            "Heterogeneous-Teacher-Value-Added/R_code/model_xwalk_SDUSD.xlsx")))

# load teacher student xwalk 
teacher_student_xwalk <- fread("c:/Users/Nmath_000/Documents/Research/Value added local/simulation_inputs/teacher_student_xwalk_realish.csv")

# Load past crosswalk to get run number.
if (file.exists(paste0(out_data,'xwalk.csv'))) {
  old_xwalk <- data.table(read.csv(file=paste0(out_data,'xwalk.csv')))
  run_id <- max(old_xwalk$run_id) + 1
} else {
  run_id <- 1
}

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
source(paste0(base_path, "Heterogeneous-Teacher-Value-Added/R_code/SDUSD Simulations/simulate_sdusd_data.R"))

#========================================#
# ==== generate some small fake data ====
#========================================#

in_dt <- simulate_test_data(n_teacher           = 100,
                            n_stud_per_teacher  = 150,
                            test_SEM            = 0,
                            teacher_va_epsilon  = 1,
                            impact_type         = "No",
                            impact_function     = 1,
                            min_diff            = 0,
                            max_diff            = .4,
                            teacher_dt          = NULL,
                            covariates          = 0,
                            peer_effects        = 0,
                            stud_sorting        = 0,
                            rho                 = .3,
                            ta_sd               = .1,
                            sa_sd               = 1,
                            tc_sd               = 1)



# regular lm
va_out1 <- lm(test_2 ~ test_1 + teacher_id - 1, data = in_dt)


# now try fixest
feols_out <- feols(fml = test_2 ~ test_1 - 1 | teacher_id, 
                   data = in_dt,
                    se = "standard")

fixedEffects = fixef(feols_out)
summary(fixedEffects)

# looks like it works 
round(fixedEffects$teacher_id - va_out1$coefficients[-1], 6)


#=================================#
# ==== time it on larger data ====
#=================================#

in_dt_big <- simulate_test_data(n_teacher           = 1800,
                                n_stud_per_teacher  = 200,
                                test_SEM            = 0,
                                teacher_va_epsilon  = 1,
                                impact_type         = "No",
                                impact_function     = 1,
                                min_diff            = 0,
                                max_diff            = .4,
                                teacher_dt          = NULL,
                                covariates          = 0,
                                peer_effects        = 0,
                                stud_sorting        = 0,
                                rho                 = .3,
                                ta_sd               = .1,
                                sa_sd               = 1,
                                tc_sd               = 1)

  #================#
  # ==== feols ====
  #================#
  
  # start time feols 
  start_time_fe <- Sys.time()
  # now try fixest
  feols_out <- feols(fml = test_2 ~ test_1 - 1 | teacher_id, 
                     data = in_dt_big,
                     se = "standard")
  fixedEffects = fixef(feols_out)
  
  # grab variance covariance matrix 
  vcov_feols <- vcov(feols_out)
  
  # end time feols 
  end_time_fe <- Sys.time()
  
  # total time feols 
  end_time_fe - start_time_fe
  
  #===============#
  # ==== felm ====
  #===============#
  
  # start time feols 
  start_time_felm <- Sys.time()
  felm_out1 <- felm(test_2 ~ test_1 - 1| teacher_id ,
                  data = in_dt_big)
  
  estimates <- getfe(felm_out1,
                     se = TRUE)
  
  # end time feols 
  end_time_felm <- Sys.time()
  
  
  
  # total time feols 
  end_time_felm - start_time_felm
  


  #=============#
  # ==== lm ====
  #=============#
  
  # start lm 
  start_time_lm <- Sys.time()
  
  # regular lm
  va_out1 <- lm(test_2 ~ test_1 + teacher_id - 1, data = in_dt_big)
  
  # end lm 
  end_time_lm <- Sys.time()
  
  # lm total 
  end_time_lm - start_time_lm
  
  
  
  
  #=============#
  # ==== plm ====
  #=============#
  
  # start lm 
  start_time_plm <- Sys.time()
  
  # regular lm
  # fixed effects model 
  plm_out1 <- plm(test_2 ~ test_1 + teacher_id - 1,
                  data = in_dt_big,
                  model = "within")
  
  # end lm 
  end_time_plm <- Sys.time()
  
  # lm total 
  end_time_plm - start_time_plm
  

