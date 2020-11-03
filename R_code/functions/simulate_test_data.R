# =========================================================================== #
# ======================= Simulate test data function ======================= #
# =========================================================================== #
# - Keywords:
#  - #note I will use the tags #note for things I think are important.
#  - #set will be used for things that need to be set for your specific
#         computer, usually just directories.

# - Purpose of code:
#  - Create simulated student test data to test value added code.


# - notes for future work 
# teacher's per school is hard coded in at 20 which also means number of teachers
# needs to be a multiple of 20 

  # ========================================================================= #
  # ========================= Roxygen documentation ========================= #
  # ========================================================================= #

  #'@param n_teacher Number of teachers. 
  #'@param n_stud_per_teacher Number of students per teacher.
  #'@param test_SEM The test's Standard Error of Measure.
  #'@param teacher_va_epsilon SE of normal distribution adding noise to how 
  #'teacher ability translates into student growth.
  #'@param impact_type This specifies the type of impact function that teachers
  #'will have for their students. Must by one of "MLRN" (Monotone, Linear,
  #'Rank Similar, No Heterogeneity), "MLR" (Monotone, Linear, Rank Similar),
  #'"ML" (Monotone, Linear, Not Rank Similar), "MNoR" (Monotone, Non-linear,
  #'Rank Similar), "MNo" (Monotone, Non-linear, Not Rank Similar), or "No" (Not
  #'Monotone, Non-linear, Not Rank Similar).
  #'@param impact_function Which function from the specified type we want for
  #'the true teacher impact.
  #'@param max_diff This sets the maximum teacher impact difference between the
  #'best and worst matched students for that teacher.
  #'@param teacher_dt This is null if no data table of teachers is supplied, but
  #'just resamples students instead of re-generating the entire data table if
  #'supplied. If this is specified, the data table must contain teacher
  #'identifiers, teacher ability, teacher center, and teacher max.
  #'@param teacher_id This is the name of the column in teacher_dt containing
  #'teacher identifiers.
  #'@param teacher_ability This is the name of the column in teacher_dt
  #'containing teacher ability.
  #'@param teacher_center This is the name of the column in teacher_dt
  #'containing teacher center.
  #'@param teacher_max This is the name of the column in teacher_dt containing
  #'the maximum allowable teacher impact difference.
  #'@param covariates 1 to include covariates, 0 otherwise.
  #'@param peer_effects 1 to include peer effects, 0 otherwise.
  #'@param stud_sorting 1 to include student sorting, 0 otherwise.
  #'@param rho Correlation between teacher and student ability.
  #'@param sa_sd Standard deviation of student ability.
  #'@param ta_sd Standard deviation of teacher ability.
  #'@details 
  #'@examples 


  # ========================================================================= #
  # ============================ debug parameters =========================== #
  # ========================================================================= #

  # n_teacher                = 140
  # n_stud_per_teacher       = 30
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
  # ta_sd                    = 0.1
  # sa_sd                    = 1


  # ========================================================================= #
  # ============================ Define Function ============================ #
  # ========================================================================= #

  # Start of the function.
  simulate_test_data <- function(n_teacher                = 140,
                                 n_stud_per_teacher       = 30,
                                 test_SEM                 = 0.07,
                                 teacher_va_epsilon       = 0.1,
                                 impact_type              = "MLRN",
                                 impact_function          = 1,
                                 max_diff                 = 0.1,
                                 teacher_dt               = NULL,
                                 teacher_id               = "teacher_id",
                                 teacher_ability          = "teacher_ability",
                                 teacher_center           = "teacher_center",
                                 teacher_max              = "teacher_max",
                                 covariates               = 0,
                                 peer_effects             = 0,
                                 stud_sorting             = 0,
                                 rho                      = 0.2,
                                 ta_sd                    = 0.1,
                                 sa_sd                    = 1,
                                 center_ability_corr      = 0) {
    
    # Generate the teacher information if not provided.
    if (is.null(teacher_dt)) {
      
      # Generate data of proper length with students. 
      r_dt <- data.table(stud_id = 1:(n_teacher*n_stud_per_teacher))
      
      # Assign teachers to classes. 
      teach_vector <- unlist(lapply(1:n_teacher, rep,
                                    times = n_stud_per_teacher))
      r_dt[, teacher_id := as.factor(teach_vector)]
  
      # Assign teachers an overall ability.
      r_dt[, teacher_ability := rnorm(1, mean = 0, sd = ta_sd), teacher_id]
      
      # Assign teachers a "center" for which students they best match.
      r_dt[, teacher_center := min(2,
                                     max(-2,
                                         rnorm(1,
                                               mean = 0,
                                               sd = 1))),
               teacher_id]

      # Assign teachers a "max" which is the difference in impact between their
      #  best and worst matched students.
      r_dt[, teacher_max := max_diff]
      
    } else {
      # Create the data table.
      r_dt <- copy(teacher_dt)
      
      # Rename columns as needed.
      setnames(r_dt, 
               old = c(teacher_id, teacher_ability, teacher_center,
                       teacher_max), 
               new = c( "teacher_id", "teacher_ability", "teacher_center",
                        "teacher_max"))

      # Grab the needed variables.
      r_dt <- r_dt[, c( "teacher_id", "teacher_ability", "teacher_center",
                        "teacher_max")]
      
      # add students 
      r_dt[, stud_id := .I]
    }
    
    
    # Ensure that we have a correct 1 or 0 for student sorting and a value for
    #  rho.
    stopifnot(stud_sorting == 0 | (stud_sorting == 1 & !is.null(rho)))
    
    # Get the total number of students.
    n_row_dt <- nrow(r_dt)
    
    # Generate the student data.
    if (covariates == 1) {
      # Assign schools.
      school_vector <- unlist(lapply(1:(n_teacher/min(20, n_teacher)), rep,
                                     times = n_stud_per_teacher*min(20, n_teacher)))
      r_dt[, school_id := as.factor(school_vector)]
      
      # Assign school characteristics.
      r_dt[, school_av_test := rnorm(1, mean = 0, sd = 0.25), school_id]
      
      # Assign student characteristics, including sex, free and reduced
      #  price lunch status, and attendance.
      r_dt[, stud_sex := runif(n_row_dt, min = 0, max = 1) > 0.5]
      r_dt[, stud_frpl := runif(n_row_dt, min = 0, max = 1) > 0.6]
      r_dt[, stud_att := 180 - (8 - 4*stud_frpl)*abs(rnorm(nrow(r_dt), mean = 0
                                                           ,sd = 1))]
      
      # Create partially unobserved ability (higher mean re-centers test_1 in
      #  expectation).
      r_dt[, stud_ability_1 := rnorm(n_row_dt,
                                     mean = stud_sorting*
                                       (sa_sd*rho*teacher_ability/ta_sd) +
                                       school_av_test + stud_sex*0.1 -
                                       stud_frpl*0.6 + (stud_att - 170)*0.008 +
                                       0.14, sd = sa_sd*(1 - stud_sorting) + 
                                       stud_sorting*sqrt(1 - rho^2))]

    } else if (covariates == 0) {
      # Now create unobserved ability.
      r_dt[, stud_ability_1 := rnorm(n_row_dt, mean = 0, sd = sa_sd)]
      
    } else {
      stop("Error covariates must be 0 or 1")
    }
    
    
    # Assign each student a test score.
    r_dt[, test_1 := rnorm(n_row_dt, mean = stud_ability_1, sd = test_SEM) ]

    
    # Get true teacher impact on students without noise.
    r_dt[, teacher_impact :=
           teacher_impact(teacher_ability  = r_dt$teacher_ability,
                          teacher_center   = r_dt$teacher_center,
                          teacher_max      = r_dt$teacher_max,
                          stud_ability_1   = r_dt$stud_ability_1,
                          type             = impact_type,
                          func_num         = impact_function)]
    
    
    # Generate the student ability and test from the second year.
    if (peer_effects == 1){
      # Calculate peer ability for linear in mean peer effects.
      r_dt[, peer_ability := (mean(stud_ability_1) -
                                stud_ability_1/n_stud_per_teacher)*
                                n_stud_per_teacher/(n_stud_per_teacher - 1),
           teacher_id]
      
      # Calculate second year student ability with peer effects. 
      r_dt[, stud_ability_2 := stud_ability_1 + 0.5*peer_ability +
                                 teacher_impact +
                                 rnorm(n_row_dt, sd = teacher_va_epsilon)]
      
      # Get second year student test.
      r_dt[, test_2 :=  mapply(rnorm, n = 1, mean = stud_ability_2,
                               sd = test_SEM)]    
      
    } else if (peer_effects==0){
      # Calculate second year student ability without peer effects. 
      r_dt[, stud_ability_2 := stud_ability_1 + teacher_impact +
                                 rnorm(n_row_dt, sd = teacher_va_epsilon)]
      
      # Get second year student test.
      r_dt[, test_2 :=  mapply(rnorm, n = 1, mean = stud_ability_2,
                               sd = test_SEM)]    
      
    } else{
      stop("peer_effects must be 0 or 1")
    }
    
    
    # Return the simulated data.
    return(r_dt[])
  
  } # End function. 
