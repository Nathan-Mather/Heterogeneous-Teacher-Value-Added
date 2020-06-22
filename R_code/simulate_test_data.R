#======================================#
# ==== Simulated student test data ====
#======================================#
# - Keywords  
#  - #note I will use the tags #note for things I think are important
#  - #set will be used for things that need to be set for your specific computer. 
#  Usually this is just directories 

# - purpose of code:
# create simulated student test data to test value added code 

# notes for future versions 
# 1) The school sizes are just uniform but that's probably not accurate. Just need to find a better distribution 
# 2) students have standard normal distributions of underlying ability. seems reasonable to me 
# 3) student ability and test scores are all in std normal distributions. I thought 
# this was good, but I am not sure how to relate that to SEMs. Need to figure it out how to properly simulate test data 
# 4) teacher to student ratios are pretty much set for now but we probably want some variation in this.
# I also don't like how I coded this, but that will change with the update anyway 
# 5) How the teacher ability translates into student growth is entirely arbitrary for now 
# 6) there is effectively only one grade 
# 7) each student is assigned exactly one teacher 

# clear data. This is commented out so we can source this from another script 
# rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
# options(scipen = 999)
# cat("\f")

library(data.table)

#======================================#
# ==== Simulate test data function ====
#======================================#
  
  #================================#
  # ==== Roxygen documentation ====
  #================================#
  # dont need this now, but eventually it would be in the help file 
  # I like to do it right away in shared projects just to keep track of what functions do 
  #' AS_IW
  #' 
  #' implement methods from "Estimating Dynamic Treatment Effects in Event Studies
  #' with Heterogeneous Treatment Effects" by Abraham and Sun.
  #'
  #'@param n_schools number of schools 
  #'@param min_stud minimum students per school 
  #'@param max_stud maximum students per school
  #'@param n_stud_per_teacher goal number of students per teacher 
  #'@param test_SEM The test's Standard Error of Measure. Maybe eventually want condition SEMS too 
  #'@param teacher_va_epsilon SE of normal distribution adding noise to how teacher ability translates into student growth 
  #'@param teacher_ability_drop_off This dictates how quickly teacher ability falls as a student moves away from a
  #'teacher's ideal "center". Higher numbers mean a teachers ability drops off faster. 
  #'@param teacher_dt This is null if no data table of teachers is supplied, but just
  #'resamples students instead of re-generating the entire data table if supplied. If this is specified,
  #'the data table must contain school identifiers, must contain teacher identifiers, teacher ability,
  #'and teacher center.
  #'@param school_id This is the name of the column in teacher_dt containing school identifiers.
  #'@param teacher_id This is the name of the column in teacher_dt containing teacher identifiers.
  #'@param teacher_ability This is the name of the column in teacher_dt containing teacher ability.
  #'@param teacher_center This is the name of the column in teacher_dt containing teacher center.
  #'@details 
  #'@examples 

  #==========================#
  # ==== debug parameters ====
  #==========================#
  # It can be helpful for code debugging to define all the function inputs in the global environment
  # and then run the function line by line.
  # uncomment the chunk by highlighting and hitting ctrl+shift+c
  #
  #   # parameters for school, teacher sizes
    #note: for now just doing one grade
  #   n_schools = 100             # number of schools
  #   min_stud  = 25           # minimum students per school
  #   max_stud  = 600       # maximum number of students
  #   n_stud_per_teacher = 25      # goal number of students per teacher
  # 
  # #   # parameters for test
  #   test_SEM = .07 # this is a complete guess at this point. Need to brush up on psychometrics
  # 
  #   # uncertainty for teacher ability translating into student growth
  # teacher_va_epsilon  = .1
  # teacher_ability_drop_off     = .25
  #==========================#
  # ==== Define Function ====
  #==========================#
  
  # start of the function
  simulate_test_data <- function(n_schools                = 20,
                                 min_stud                 = 25,
                                 max_stud                 = 600, 
                                 n_stud_per_teacher       = 25,
                                 test_SEM                 = .07,
                                 teacher_va_epsilon       = .1,
                                 teacher_ability_drop_off = .25,
                                 teacher_dt               = NULL,
                                 school_id                = "school",
                                 teacher_id               = "teacher_id",
                                 teacher_ability          = "teacher_ability",
                                 teacher_center           = "teacher_center"){
    
    # Generate the teacher information if not provided.
    if (is.null(teacher_dt)) {
      # generate an empty list to fill in with schools 
      r_dt <- vector("list", length = n_schools)
      
      # fill in schools data.tables with random numbers of rows (students)
      for(i in 1:n_schools){
        
        r_dt[[i]] <- data.table(school = rep(i,  round(runif(1,  min_stud, max_stud))))
        
      }
      
      # stack up lists into one data.table 
      r_dt <- rbindlist(r_dt)
      
      # add students 
      r_dt[, stud_id := .I]
      
      # now assign teachers within school 
      # start by getting the number of teachers needed per school 
      r_dt[, teach_needed := ceiling(.N/n_stud_per_teacher) , school]
      
      # now get the number of students per teacher 
      r_dt[, stud_per_teach := ceiling(.N/teach_needed) , school]
      
      # now fill in teacher ID by repeating the number 1 to teach_needed, stud_per_teach times and cut off the vector at .N in each school
      r_dt[, teacher_id := unlist(lapply(1:unique(teach_needed), rep, unique(stud_per_teach)))[1:.N], school]
      
      # update teacher Id to be unique combo of teacher number and school number 
      r_dt[, teacher_id := paste0(school, "_", teacher_id)]
      
      # drop extra vars
      r_dt[, c("teach_needed", "stud_per_teach") := NULL]
      
      # assign teacher's an overall ability. and a "center" where they perform best 
      r_dt[, teacher_ability := rnorm(1, mean =0, sd = .1), teacher_id]
      r_dt[, teacher_center := rnorm(1, mean =0, sd = 1), teacher_id]
      
    } else {
      # Create the data table
      r_dt = teacher_dt
      
      # Rename columns as needed
      names(r_dt)[names(r_dt) == school_id] <- "school"
      names(r_dt)[names(r_dt) == teacher_id] <- "teacher_id"
      names(r_dt)[names(r_dt) == teacher_ability] <- "teacher_ability"
      names(r_dt)[names(r_dt) == teacher_center] <- "teacher_center"
      
      # add students 
      r_dt[, stud_id := .I]
    }
    
    # Now assign students random ability for first test 
    #note this is where we would do random assignment since up till now all studs are identical 
    r_dt[, stud_ability_1 := rnorm(nrow(r_dt))]
    
    # assign them a test score 
    #note not 100% sure this is how this works with the SEM but I think this is close to right 
    # mapply applies rnorm to each row using stud_ability_1 and test_SEM from each row
    # we need to do this because the arguments mean and sd are not vectorized 
    r_dt[, test_1 := mapply(rnorm, n= 1, mean = stud_ability_1, sd = test_SEM) ]
    
    # Now let teachers influence students and grow their ability over time base on how good the teacher is 
    #note I have no idea what this distribution should look like. keep in mind all the scores are in theory normalized 
    # so a student going down just means relative score is going down 
    n_row_dt <- nrow(r_dt)
    # get teacher impact on student without noise 
    r_dt[, teacher_impact := teacher_ability - abs(stud_ability_1 - teacher_center)* teacher_ability_drop_off]
    
    # as impact to student with noise 
    r_dt[, stud_ability_2 := stud_ability_1 + teacher_impact + rnorm(n_row_dt, sd = teacher_va_epsilon)]
    
    # Now that they have a new ability give them another test 
    r_dt[, test_2 := mapply(rnorm, n= 1, mean = stud_ability_2, sd = test_SEM) ]
    
    
    return(r_dt[])
  
  } # end function 
  
  #========================#
  # ==== Test function ====
  #========================#
  
  # # just leave all the defaults in there 
  # sim_dt <- simulate_test_data()

  # #======================#
  # # ==== diognostics ====
  # #======================#
  # 
    # take the expected mean teacher ability and center and map their benefit to the draw of students
  #   E_teacher_ability <- 0
  #   E_Teacher_center <- 0
  #   r_dt[, diog_teacher_impact := E_teacher_ability - abs(stud_ability_1 - E_Teacher_center)* teacher_abiliy_drop_off]
  #   diog_plot1 <- ggplot(data = r_dt, aes(x= test_1, y = diog_teacher_impact)) +
  #     geom_point() +
  #     ggtitle("Expected teacher impact without Noise") +
  #     ylab("Theoretical average teacher impact")
  #   print(diog_plot1)
  # ggsave("c:/Users/Nmath_000/Documents/data/Value Added/teacher_impact_kernal_no_noise.png")
  # #   
  #   # now do it with the noise
  #   r_dt[, diog_teacher_impact := E_teacher_ability - abs(stud_ability_1 - E_Teacher_center)* teacher_abiliy_drop_off + rnorm(n_row_dt, sd = teacher_va_epsilon)]
  #   diog_plot1 <- ggplot(data = r_dt, aes(x= test_1, y = diog_teacher_impact)) +
  #     geom_point() +
  #     ggtitle("Expected teacher impact with Noise") +
  #     ylab("Theoretical average teacher impact with added noise ")
  #   print(diog_plot1)
  # 
  # ggsave("c:/Users/Nmath_000/Documents/data/Value Added/teacher_impact_kernal_noise.png")
  # 

