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
# n_schools                = 20
# n_teacher                = 140
# n_stud_per_teacher       = 30
# test_SEM                 = .07
# teacher_va_epsilon       = .1
# teacher_ability_drop_off = .15
# teacher_dt               = NULL
# school_id                = "school"
# teacher_id               = "teacher_id"
# teacher_ability          = "teacher_ability"
# teacher_center           = "teacher_center"
#==========================#
# ==== Define Function ====
#==========================#

# start of the function
simulate_test_data <- function(n_teacher                = 140,
                               n_stud_per_teacher       = 30,
                               test_SEM                 = .07,
                               teacher_va_epsilon       = .1,
                               teacher_ability_drop_off = .15,
                               teacher_dt               = NULL,
                               teacher_id               = "teacher_id",
                               teacher_ability          = "teacher_ability",
                               teacher_center           = "teacher_center",
                               covariates               = 1,
                               peer_effects             = 1,
                               stud_sorting             = 0,
                               rho                      = .2,
                               ta_sd                    =.1,
                               sa_sd                    = 1){
  
  # Generate the teacher information if not provided.
  if (is.null(teacher_dt)) {
    
    # generate data of proper length with students 
    r_dt <- data.table(stud_id = 1:(n_teacher*n_stud_per_teacher))
    
    # assign teachers 
    teach_vector <- unlist(lapply(1:n_teacher, rep, times =  n_stud_per_teacher))
    r_dt[, teacher_id := as.factor(teach_vector)]
  
    
    # assign teacher's an overall ability. and a "center" where they perform best 
    r_dt[, teacher_ability := rnorm(1, mean =0, sd = ta_sd), teacher_id]
    r_dt[, teacher_center := rnorm(1, mean =0, sd = 1), teacher_id]

    
  } else {
    # Create the data table
    r_dt <- copy(teacher_dt)
    
    # Rename columns as needed
    setnames(r_dt, 
             old = c(teacher_id, teacher_ability, teacher_center), 
             new = c( "teacher_id", "teacher_ability", "teacher_center"))
    
    # grab needed vars 
    r_dt <- r_dt[, c( "teacher_id", "teacher_ability", "teacher_center")]
    
    # add students 
    r_dt[, stud_id := .I]
  }

  stopifnot(stud_sorting==0|(stud_sorting==1& !is.null(rho)))
  
  
  if (covariates==1) {
    # assign schools
    school_vector <- unlist(lapply(1:(n_teacher/20), rep, times =  n_stud_per_teacher*20))
    r_dt[, school_id := as.factor(school_vector)]
    
    # Assign school characteristic:
    r_dt[ , school_av_test := rnorm(1, mean =0, sd = .25), school_id]
    
    # Now assign student characteristics
    r_dt[,stud_sex := runif(nrow(r_dt), min = 0, max = 1)>.5]
    r_dt[,stud_frpl := runif(nrow(r_dt), min = 0, max = 1)>.6]
    r_dt[,stud_att := 180-(8-4*stud_frpl)*abs(rnorm(nrow(r_dt), mean =0 ,sd=1 ))]
    
    # Now create partially unobserved ability (higher mean recenters test_1 in expectation)
    r_dt[, stud_ability_1 := rnorm(nrow(r_dt),mean= stud_sorting*(sa_sd*rho*teacher_ability/ta_sd)   + school_av_test+ stud_sex*.1 - stud_frpl *.6 + (stud_att-170)*.008  +.14,sd=sa_sd(1-stud_sorting)+stud_sorting*sqrt(1-rho^2))]
    
  } else if (covariates == 0) {
    # Now create unobserved ability 
    r_dt[, stud_ability_1 := rnorm(nrow(r_dt),mean= 0,sd=sa_sd)]
    
  } else {
    stop("Error covariates must be 0 or 1")
  }
  # assign them a test score 
  # mapply applies rnorm to each row using stud_ability_1 and test_SEM from each row
  # we need to do this because the arguments mean and sd are not vectorized 
  r_dt[, test_1 :=   mapply(rnorm, n= 1, mean = stud_ability_1, sd = test_SEM) ]
  
  # Now let teachers and school influence students and grow their ability over time base on how good the teacher is 
  #note I have no idea what this distribution should look like. keep in mind all the scores are in theory normalized 
  # so a student going down just means relative score is going down 
  n_row_dt <- nrow(r_dt)
  
  # get teacher impact on student without noise 
  r_dt[, teacher_impact := teacher_ability - pmin(abs(stud_ability_1 - teacher_center), 2) * teacher_ability_drop_off]
  
  if (peer_effects==1){
    # calculate peer ability for linear in mean peer effects
    r_dt[ ,peer_ability :=(mean(stud_ability_1)-stud_ability_1/n_stud_per_teacher)*n_stud_per_teacher/(n_stud_per_teacher-1) , teacher_id]

    # as impact to student with noise 
    r_dt[, stud_ability_2 := stud_ability_1 +.5*peer_ability + teacher_impact + rnorm(n_row_dt, sd = teacher_va_epsilon)]
    
    # Now that they have a new ability give them another test 
    r_dt[, test_2 :=  mapply(rnorm, n= 1, mean = stud_ability_2, sd = test_SEM) ]    
    
  } else if (peer_effects==0){
    # as impact to student with noise 
    r_dt[, stud_ability_2 := stud_ability_1 + teacher_impact + rnorm(n_row_dt, sd = teacher_va_epsilon)]
    
    # Now that they have a new ability give them another test 
    r_dt[, test_2 :=  mapply(rnorm, n= 1, mean = stud_ability_2, sd = test_SEM) ]    
    
  } else{
    stop("peer_effects must be 0 or 1")
  }
  

  
  
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
