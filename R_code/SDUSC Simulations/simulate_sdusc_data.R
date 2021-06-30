# =========================================================================== #
# ======================= Simulate SDUSD data functions ======================= #
# =========================================================================== #
# - Keywords:
#  - #note I will use the tags #note for things I think are important.
#  - #set will be used for things that need to be set for your specific
#         computer, usually just directories.

# - Purpose of code:
#  - Create simulated student test data that matches SDUSD. This will be an update 
#  to our simulation code to be more realistic but also be helpful for working on code
# without data access. 


#================#
# ==== notes ====
#================#

# need to figure out how to correlate teacher ability within simulate_teacher_ability
# will need to add a teacher center to simulate_teacher_ability

# dependency for this 


# ========================================================================= #
# ========================= Roxygen documentation ========================= #
# ========================================================================= #



# ========================================================================= #
# ============================ debug parameters =========================== #
# ========================================================================= #

# load in needed inputs 
teacher_student_xwalk <- fread("C:/Users/Nmath_000/Documents/Research/Value added local/simulation_inputs/teacher_student_xwalk_fake.csv")

# load packages and functions for test runs 
base_path <- "c:/Users/Nmath_000/Documents/Research/"
func_path <- "Heterogeneous-Teacher-Value-Added/R_code/functions/"
source(paste0(base_path, func_path, "teacher_impact.R"))
library(data.table)


# create a simple version for visualizing steps 
teacher_student_xwalk <- teacher_student_xwalk[ school_id <= 2]
teacher_student_xwalk <- teacher_student_xwalk[, n_studs := 2]


n_cohorts                  = 3
pretest_coef            = .9
tc_sd                    = 1
min_diff                 = 0
max_diff                 = 0.1
impact_type              = "MLRN"
impact_function          = 1
# from wooldridge 
sa_sd                    = 1

# teacher_student_xwalk    = teacher_student_xwalk
# test_SEM                 = 0.07
# teacher_va_epsilon       = 0.1
# impact_type              = "MLRN"
# impact_function          = 1
# max_diff                 = 0.1
# teacher_dt               = NULL
# teacher_id               = "teacher_id"
# teacher_ability          = "teacher_ability"
# teacher_center           = "teacher_center"
# teacher_max              = "teacher_max"
# covariates               = 0
# peer_effects             = 0
# stud_sorting             = 0
# rho                      = 0.2




# debug parms for simulate_teacher_ability
# from wooldridge .6 
ta_sd                   = .6
school_cor              = 0


#==================================================#
# ==== Function for simulating teacher ability ====
#==================================================#


# function to add teacher ability 
simulate_teacher_ability <- function(teacher_student_xwalk   = NULL,
                                     ta_sd                   = .6,
                                     school_cor              = 0,
                                     tc_sd                   = 1,
                                     min_diff                = 0,
                                     max_diff                = 0.1){
  
  # need to figure out how to add in school correlation 
  teacher_student_xwalk[, teacher_ability := rnorm(.N, mean = 0, sd = ta_sd)]
  
  
  # Assign teachers a "center" for which students they best match.
  teacher_student_xwalk[, teacher_center := min(2,
                               max(-2,
                                   rnorm(1,
                                         mean = 0,
                                         sd = tc_sd))),
       teacher_id]
  
  # Assign teachers a "max" which is the difference in impact between their
  #  best and worst matched students.
  teacher_student_xwalk[, teacher_max := runif(1, min=min_diff, max=max_diff), teacher_id]
  
  # return 
  return(teacher_student_xwalk[])
}

teacher_student_xwalk <- simulate_teacher_ability(teacher_student_xwalk)

#============================================================#
# ==== function for simluating student ability and tests ====
#============================================================#


# Start of the function.
simulate_sdusd_data <- function(teacher_student_xwalk   = NULL,
                                n_cohorts               = 3,
                                pretest_coef            = .9,
                                sa_sd                    = 1,
                               # test_SEM                 = 0.07,
                               impact_type              = "MLRN",
                               impact_function          = 1
                               # covariates               = 0,
                               # peer_effects             = 0,
                               # stud_sorting             = 0,
                               # rho                      = 0.2,
                               # center_ability_corr      = 0
                               ) {
  
 
    # create a list for all cohort years of data to combine at the end 
    # note, should predetermine length of this list 
    r_dt_list <- list()
    
    # get minimum grade 
    min_gade <- teacher_student_xwalk[, min(grade)]
    
    # get total number of grades 
    n_gades <- teacher_student_xwalk[, length(unique(grade))]
    
    # get teacher school xwalk 
    teacher_school_xwalk <- teacher_student_xwalk[, c("school_id", "teacher_id", "teacher_ability",  "teacher_center", "teacher_max")]
    
    # get school count xwalk 
    school_xwalk <- teacher_student_xwalk[grade == min_gade, .(n_studs_school = sum(n_studs)), school_id ]
  
  
    for(k in 1:n_cohorts){
    
      # generate a cohort of students 
      n_studs <- teacher_student_xwalk[grade == min_gade, sum(n_studs)]
      
      # generate students, give them a base grade and starting year. This is pretest year without teachers 
      r_dt <- data.table(stud_id = 1:n_studs)
      r_dt[, grade := min_gade-1]
      r_dt[, year := k]
      
      # but, lets assign then to their school. Make a list of schoosl to assign
      school_list <- school_xwalk[, rep(school_id, n_studs_school)]
      
      # randomly assign list to students 
      r_dt[, school_id := sample(school_list, replace = FALSE)]
      r_dt[, teacher_id := NA]
      r_dt[, teacher_ability := NA]
      r_dt[, teacher_center := NA]
      r_dt[, teacher_max := NA]
      r_dt[, teacher_impact := NA]
      r_dt[, test_1 := NA]
      
      # give students their pretests 
      #note, will need to add possiblity of correlated effects here 
      #note will need to add measurment error here. 
      r_dt[, test_2 := rnorm(.N, mean = 0, sd = sa_sd)]
      
      # give students their time invariant effect like in wooldridge
      r_dt[, stud_fe := rnorm(.N, mean = 0, sd = .6)]
      
      # good place to give them invariant covariates as well
      
      # save this cohort year 
      index <- (k-1)*(n_gades + 1) + 1
      r_dt_list[[index]] <- copy(r_dt)
      
      # now create all the years with teacher assignments 
      for(i in 1:n_gades){
        
        # Now remove teachers, teacher ability, pre test, add a year and grade 
        r_dt[, teacher_id := NULL]
        r_dt[, teacher_ability := NULL]
        r_dt[, teacher_center := NULL]
        r_dt[, teacher_max := NULL]
        r_dt[, teacher_impact := NULL]
        r_dt[, test_1 := NULL]
        r_dt[, grade := grade + 1]
        r_dt[, year := year + 1]

        # rename post_Test to pre-test 
        setnames(r_dt, "test_2", "test_1")
        
        # get a list of teachers 
        teacher_list <- teacher_student_xwalk[grade == min_gade+i-1, .(teacher_id = rep(teacher_id, n_studs)), school_id]
        
        # sort teacher list and data to match 
        setorder(r_dt, school_id)
        setorder(teacher_list, school_id)
        
        # shuffle rows 
        teacher_list[, teacher_id := sample(teacher_id,replace = FALSE), school_id]
        setnames(teacher_list, "school_id", "school_id2")
        
        # bind on teacher ids 
        r_dt <- cbind(r_dt, teacher_list)
        
        # double check merge was okay 
        if(!all(r_dt$school_id == r_dt$school_id2)) stop( "merge failed ")
        
        # drop extar col 
        r_dt[, school_id2 := NULL]
        
        # merge on teacher ability 
        r_dt <- merge(r_dt, teacher_school_xwalk, c("school_id", "teacher_id"))
        
        # Get true teacher impact on students without noise.
        r_dt[, teacher_impact :=
               teacher_impact(teacher_ability  = r_dt$teacher_ability,
                              teacher_center   = r_dt$teacher_center,
                              teacher_max      = r_dt$teacher_max,
                              stud_ability_1   = r_dt$test_1,
                              type             = impact_type,
                              func_num         = impact_function)]
        
        #note add covariates and peer effects here as well 
        
        # make new test via wooldridge with no measurment error
        r_dt[, test_2 := pretest_coef*test_1 + teacher_impact + stud_fe + rnorm(.N, mean = 0, sd = 1)]
        
  
        # save this year cohort 
        r_dt_list[[index + i]] <- copy(r_dt)
        
        
      }
    
    }
    
    # now bind together whole list of student teacher assignments 
    r_dt <- rbindlist(r_dt_list, use.names = TRUE)
    
    

  # Return the simulated data.
  return(r_dt[])
  
} # End function. 
