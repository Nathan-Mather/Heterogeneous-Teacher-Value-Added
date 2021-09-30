#==========================================#
# ==== Make test inputs for simulation ====
#==========================================#

library(data.table)

base_path <- "C:/Users/Nmath_000/Documents/Research/Heterogeneous-Teacher-Value-Added/R_code/"


# load in real data to get something cose 
real_data <- fread(paste0("C:/Users/Nmath_000/Documents/Research/Value_added_local/simulation_inputs/SimulationAnalysis.csv"))


#==============================#
# ==== make a temp realish data ====
#==============================#
# this is not perfect or the final thing I will do, but it is a reasonable and quick 
# way to get started 

# greab a single year of data 
one_year <- real_data[yearsp == 2008]

#  remove duplicates 
one_year <- unique(one_year, by = "teacher_id")

# drop schools with less than 6 teachers 
n_teacher_tab <- one_year[, length(unique(teacher_id)), school_id]
to_drop <- n_teacher_tab[V1 < 6, school_id]
one_year <- one_year[!school_id %in% to_drop]



# assing a grade 
one_year[, grade := rep(c(3:5), ceiling(.N/3))[1:.N], school_id]

# keep inly third grade 
one_year_grade <- one_year[grade == 3]
one_year_grade_4 <- copy(one_year_grade)
one_year_grade_4[, grade := 4]
one_year_grade_4[, teacher_id := 1:.N]
one_year_grade_5 <- copy(one_year_grade)
one_year_grade_5[, grade := 5]
one_year_grade_5[, teacher_id := (.N+1):(.N*2)]

# stack them 
realish_data <- rbind(one_year_grade, one_year_grade_4, one_year_grade_5)

# rename/remove columnts 
realish_data[, yearsp := NULL]
setnames(realish_data, "class_size","n_studs")
write.csv(realish_data, paste0(base_path, "teacher_student_xwalk_realish.csv"), row.names = FALSE)


#=============================#
# ==== generate fake data ====
#=============================#

# generate school ids 
school_id <- c(school_id = 1:10)

# make teacher id's per school 
teacher_id <- c(school_id = 1:9)



teacher_student_xwalk <- data.table(expand.grid(school_id = school_id,teacher_id = teacher_id))


# assign gades 
teacher_student_xwalk[teacher_id <= 3, grade := 3]
teacher_student_xwalk[teacher_id > 3 & teacher_id <= 6, grade := 4]
teacher_student_xwalk[teacher_id > 6, grade := 5]


# asign studs 
teacher_student_xwalk[, n_studs := 30]


# create a unique teacher id 
teacher_student_xwalk[, teacher_id := paste0(school_id, teacher_id)]


#########
# save #
########

# subset this to have  simple small sample 

# save this out for practice until I get real thing from tanner 
write.csv(teacher_student_xwalk, paste0(base_path, "teacher_student_xwalk_fake.csv"), row.names = FALSE)


