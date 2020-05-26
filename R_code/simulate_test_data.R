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
# 4) teacher to student ratios are pretty mush set for now but we probaby want some variation in this.
# I also don't like how I coded this, but that will change with the update anyway 
# 5) How the teacher ability translates into student growth is entirely arbitrary for now 
# 6) there is effectivly only one grade 
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
  #' impliment methods from "Estimating Dynamic Treatment Effects in Event Studies
  #' with Heterogeneous Treatment Effects" by Abraham and Sun.
  #'
  #'@param n_schools number of schools 
  #'@param min_stud minimum students per school 
  #'@param max_stud maximum students per school
  #'@param n_stud_per_teacher goal number of students per teacher 
  #'@param test_SEM The test's Standard Error of Measure. Maybe eventually want condition SEMS too 
  #'@details See the working paper http://economics.mit.edu/files/14964
  #'@examples 

  #==========================#
  # ==== debug parameters ====
  #==========================#
  # It can be helpful for code debugging to define all the function inputs in the global enviorment 
  # and then run the funciton line by line.
  # uncomment the chunk by highlighting and hitting ctrl+shift+c
  # 
  #   # parameters for school, teacher sizes 
  #   #note: for now just doing one grade 
    # n_schools = 100             # number of schools
    # min_stud  = 25           # minimum students per school
    # max_stud  = 600       # maximum number of students
    # n_stud_per_teacher = 25      # goal number of students per teacher

  #   # parameters for test 
    # test_SEM = .07 # this is a complete guess at this point. Need to brush up on psychometrics
  #   

  #==========================#
  # ==== Define Function ====
  #==========================#
  
  
  simulate_test_data <- function(n_schools          = 100,
                                 min_stud           = 25,
                                 max_stud           = 600, 
                                 n_stud_per_teacher = 25,
                                 test_SEM           = .07){
    # generate vectors 
    r_dt <- vector("list", length = n_schools)
    
    # just do this with a loop because its easy 
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
    
    # Now assign students random ability for first test 
    #note this is where we would do random assignment since up till now all studs are identical 
    r_dt[, stud_ability_1 := rnorm(nrow(r_dt))]
    
    # assign them a test score 
    #note not 100% sure this is how this works with the SEM but I think this is close to right 
    r_dt[, test_1 := mapply(rnorm, n= 1, mean = stud_ability_1, sd = test_SEM) ]
    
    # assign teacher's an underlying value added 
    r_dt[, teacher_ability := .1*rnorm(1), teacher_id]
    
    # Now let teachers influence students and grow their ability over time base on how good the teacher is 
    #note I have no idea what this distribution shold look like. keep in mind all the scores are in theory normalized 
    # so a student going down just means relative score is going down 
    r_dt[, stud_ability_2 := stud_ability_1 + teacher_ability]
    
    # Now that they have a new ability give them another test 
    r_dt[, test_2 := mapply(rnorm, n= 1, mean = stud_ability_2, sd = test_SEM) ]
    
    return(r_dt[])
  
  } # end fucntion 
  
  #========================#
  # ==== Test function ====
  #========================#
  
  # # just leave all the defaults in there 
  # sim_dt <- simulate_test_data()




