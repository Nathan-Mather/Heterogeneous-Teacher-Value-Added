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
# then we will have to write this more from scratch and can rely on the VA code I have from EA. We will also 
# have to create a dummy matrix once students have multiple teacher assignments 

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

# run first regression to partial out controlls 
ww_partial <- lm(test_2 ~ test_1 , data = r_dt)

# get the residuals 
ww_resids <- data.table(stud_id = r_dt$stud_id, residual = residuals(ww_partial) )

# merge on residuals 
r_dt <- merge(r_dt, ww_resids, "stud_id")

# make weights 
# lets do a linear weight where the lowest student counts twice as much as the highest 
max_score <- r_dt[, max(test_1)]
min_score <- r_dt[,min(test_1)]
r_dt[, weight := 2-(test_1-min_score)*(1/(max_score-min_score))]
summary(r_dt$weight)

# Now run weighted regression 
ww_va <- lm(residual ~ teacher_id - 1, weights = weight, data = r_dt)

# clean results 
ww_tab <- data.table(tidy(ww_va))

ww_tab[, teacher_id := gsub("teacher_id", "", term)]

ww_res <- ww_tab[term %like% "teacher_id", c("estimate", "teacher_id")]

# merge it on to data 
ww_res <- merge(teach_dt, ww_res, "teacher_id")

# get correlation 
correlation <- ww_res[, cor(estimate, teacher_ability)]

#============================#
# ==== rescaling weights ====
#============================#
# I'm not sure what happens if I rescale the weights so I am going to try that 
r_dt[, weight_2 := weight*.5]
summary(r_dt$weight_2)

# Now run weighted regression 
ww_va2 <- lm(residual ~ teacher_id - 1, weights = weight_2, data = r_dt)

# clean results 
ww_tab2 <- data.table(tidy(ww_va2))

ww_tab2[, teacher_id := gsub("teacher_id", "", term)]


# compare, looks like nothing happens... interesting 
all.equal(ww_tab2$estimate, ww_tab$estimate)
all.equal(ww_tab2$std.error, ww_tab$std.error)
all.equal(r_dt$weight, r_dt$weight_2)


#==============================#
# ==== try extreme weights ====
#==============================#

r_dt[, weight_3 := (1/(test_1 + abs(min_score) +.01))]
summary(r_dt$weight_3)

# Now run weighted regression 
ww_va3 <- lm(residual ~ teacher_id - 1, weights = weight_3, data = r_dt)

# clean results 
ww_tab3 <- data.table(tidy(ww_va3))

ww_tab3[, teacher_id := gsub("teacher_id", "", term)]


# compare:
all.equal(ww_tab3$estimate, ww_tab$estimate)
all.equal(ww_tab3$std.error, ww_tab$std.error)
summary(ww_tab3$std.error)
summary( ww_tab$std.error)
=all.equal(r_dt$weight, r_dt$weight_3)
