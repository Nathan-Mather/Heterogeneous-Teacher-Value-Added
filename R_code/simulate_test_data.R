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


library(data.table)

#=========================#
# ==== set parameters ====
#=========================#

n_schools <- 100             # number of schools 
min_stud    <- 50            # minimum students per school 
max_stud    <- 3000         # maximum number of students 
stud_sd   <- 300             # standard deviation of number of students per school 
n_stud_per_teacher <- 25      # mean numver of students per teacher 



# generate vectors 
r_dt <- vector("list", length = n_schools)

# just do this with a loop because its easy 
for(i in 1:n_schools){
  
  r_dt[[i]] <- data.table(school = rep(i,  max(round(rnorm(1,  m_stud, stud_sd)), 50)))
  
}

#testing github 

