

#####################################
# measurement error correction code # 
#####################################

# libraries 
library(arm)
library(data.table)


base_path <- "c:/Users/Nmath_000/Documents/Research/"
func_path <- "Heterogeneous-Teacher-Value-Added/R_code/functions/"
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


##########################################
# simulate data and check residual plots #
##########################################

# laod in a smal easy techer xwalk 
teacher_xwalk <- fread("C:/Users/Nmath_000/Documents/Research/Heterogeneous-Teacher-Value-Added/R_code/teacher_student_xwalk_fake.csv")


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
                                pretest_coef            = .9,
                                impact_type             = "NO",
                                impact_function         = 1,
                                test_SEM                = .34)

#NOTE: I need to do the following checks by grade because the variance and SD increases when pooled accross grades 
# check that this is a good sem, should give reliability of like .9 
me_dt_sub <- me_dt[grade == 3]
var(me_dt_sub$stud_ability_1) / var(me_dt_sub$test_1)

# are return the SEM
sd(me_dt_sub$test_1) * sqrt( 1 - var(me_dt_sub$stud_ability_1) / var(me_dt_sub$test_1))


#######################################
# first make residual plot with no ME #

# Now run the value added regression so we can check the residuals 
reg_res <- lm(stud_ability_2 ~  stud_ability_1 + teacher_id - 1, data = me_dt)


# now check the residuals 
me_dt[, resids_1 := reg_res$residuals]


# now plot residuals over ability 
plot(me_dt$stud_ability_1, me_dt$resids)

binnedplot(me_dt$stud_ability_1 ,me_dt$resids)

######################################
# Now do it with ME in the pretest  #
reg_res2 <- lm(stud_ability_2 ~  test_1 + teacher_id - 1, data = me_dt)


# now check the residuals 
me_dt[, resids_2 := reg_res2$residuals]


# now plot residuals over ability 
plot(me_dt$test_1, me_dt$resids_2)

binnedplot(me_dt$test_1 ,me_dt$resids_2)



