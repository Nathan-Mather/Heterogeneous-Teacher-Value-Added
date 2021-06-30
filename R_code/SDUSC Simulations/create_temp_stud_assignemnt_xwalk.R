#==========================================#
# ==== Make test inputs for simulation ====
#==========================================#

library(data.table)

base_path <- "C:/Users/Nmath_000/Documents/Research/Value added local/simulation_inputs/"

# make a fake class/school size xwalk 
school_id <- c(school_id = 1:240)

teacher_id <- c(school_id = 1:9)



teacher_student_xwalk <- data.table(expand.grid(school_id = school_id,teacher_id = teacher_id))


# assign gades 
teacher_student_xwalk[teacher_id <= 3, grade := 3]
teacher_student_xwalk[teacher_id > 3 & teacher_id <= 6, grade := 4]
teacher_student_xwalk[teacher_id > 6, grade := 5]


# asign studs 
teacher_student_xwalk[, n_studs := 25]


# create a unique teacher id 
teacher_student_xwalk[, teacher_id := paste0(school_id, teacher_id)]



# save this out for practice until I get real thing from tanner 
write.csv(teacher_student_xwalk, paste0(base_path, "teacher_student_xwalk_fake.csv"), row.names = FALSE)
