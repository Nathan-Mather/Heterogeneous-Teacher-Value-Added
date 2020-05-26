#================================#
# ==== Simulated Value added ====
#================================#
# - Keywords  
#  - #note I will use the tags #note for things I think are important
#  - #set will be used for things that need to be set for your specific computer. 
#  Usually this is just directories 

# - purpose of code:
#  Run a basic value added on simulated data 

# notes for future versions 
# 1) Measurment error correction 
# 2) shrinkage 
# 3) centering and scaling and all that 
# 4) aggrergation if teachers teach multiple grade levels from mutliple models 



#=================#
# ==== set up ====
#=================#


# clear data. This is commented out so we can source this from another script 
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")

# load packages and our functions 
library(data.table)
library(broom)
source("c:/Users/Nmath_000/Documents/Research/Heterogeneous-Teacher-Value-Added/R_code/simulate_test_data.R")  #set this path 


#===================#
# ==== sim data ====
#===================#

# generate simluated data. just use default inputs for now 
r_dt <- simulate_test_data()


#=================#
# ==== run VA ====
#=================#

# for now I am just going to use built in commands. If we eventually want to do measurment error correction and stuff 
# then we will have to write this more from scratch and can rely on the VA code I have from EA. 

# convert teacher_id to a factor so we can treat it as a dummy 
r_dt[, teacher_id := as.factor(teacher_id)]

# run regression 
va_out1 <- lm(test_2 ~ test_1 + teacher_id - 1, data = r_dt)

# clean results 
va_tab1 <- data.table(tidy(va_out1))

va_tab1[, teacher_id := gsub("teacher_id", "", term)]

va_res <- va_tab1[term %like% "teacher_id", c("estimate", "teacher_id")]

# grab teacher data 
teach_dt <- unique(r_dt[, c("teacher_id", "teacher_ability")])

# merge it on to data '
va_res <- merge(teach_dt, va_res, "teacher_id")

# get correlation 
correlation <- va_res[, cor(estimate, teacher_ability)]


#==========================#
# ==== welfare weights ====
#==========================#


