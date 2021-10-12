

#####################################
# measurement error correction code # 
#####################################

# libraries 
library(arm)
library(data.table)
library(Matrix)

base_path <- "c:/Users/Nmath_000/Documents/Research/"

code_path <- "Heterogeneous-Teacher-Value-Added/R_code/"
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

# source me correction functions 
source(paste0(base_path, func_path, "make_dummies.R"))
source(paste0(base_path, func_path, "me_correction.R"))


##########################################
# simulate data and check residual plots #
##########################################

# laod in a smal easy techer xwalk 
teacher_xwalk <- fread(paste0(base_path, code_path, "teacher_student_xwalk_fake.csv"))


# Simulate teacher data #note: loacted in funcitons/simulate_sdusd_data 
teacher_ability_xwalk <- simulate_teacher_ability(teacher_student_xwalk = teacher_xwalk,
                                                  ta_sd                   = .6,
                                                  school_cor              = 0,
                                                  tc_sd                   = 1,
                                                  min_diff                = 0,
                                                  max_diff                = 2.4)

# with masurement error 
me_dt <-   simulate_sdusd_data(teacher_ability_xwalk   = teacher_ability_xwalk,
                                n_cohorts               = 8,
                                pretest_coef            = 1.1,
                                impact_type             = "NO",
                                impact_function         = 1,
                                test_SEM                = .34)


# Subset to a specific grade #NOTE 
# for now the measurement error I am simulating is not corresponding to the reliability I wanted 
# because of the serial correlation or something. 
me_dt <- me_dt[grade == 3]

#NOTE: I need to do the following checks by grade because the variance and SD increases when pooled accross grades 
# check that this is a good sem, should give reliability of like .9 
var(me_dt$stud_ability_1) / var(me_dt$test_1)

# are return the SEM
sd(me_dt$test_1) * sqrt( 1 - var(me_dt$stud_ability_1) / var(me_dt$test_1))


#######################################
# first make residual plot with no ME #

# Now run the value added regression so we can check the residuals 
reg_res <- lm(stud_ability_2 ~  stud_ability_1 + teacher_id - 1, data = me_dt)


# now check the residuals 
me_dt[, resids_1 := reg_res$residuals]


# now plot residuals over ability 
plot(me_dt$stud_ability_1, me_dt$resids_1)

binnedplot(me_dt$stud_ability_1 ,me_dt$resids_1)

######################################
# Now do it with ME in the pretest  #
reg_res2 <- lm(test_2 ~  test_1 + teacher_id - 1, data = me_dt)


# now check the residuals 
me_dt[, resids_2 := reg_res2$residuals]


# now plot residuals over prettest 
binnedplot(me_dt$test_1 ,me_dt$resids_2)


# Now do it with student ability though 
binnedplot(me_dt$stud_ability_1 ,me_dt$resids_2)

###########################
# ME correction fucntion #
##########################


# make the dummes 
make_dummies(in_data       = me_dt,
          to_dummy_var = "teacher_id")

# run ME corrected regression 
me_results <- me_correction(in_data = me_dt,
                            lhs_var = "stud_ability_2",
                            pretest_vars = "test_1",
                            control_vars = NULL,
                            teacher_vars = grep("d_teacher_id_", colnames(me_dt), value = TRUE),
                            stud_id_var  = "stud_id",
                            reliability  = .9 )


# Now dothe binnedplot 
me_resids <- me_results$residuals


binnedplot(me_dt$test_1, me_resids$resid_model)

binnedplot(me_dt$stud_ability_1 ,  me_resids$resid_model)


