#================================#
# ==== Make (v Cool) Figures ====
#================================#


#=================#
# ==== set up ====
#=================#
# tbh I hvae no idea what this is doing but I copied it from Nate's code

# clear data.
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")

# check users (can do something here eventually to automatically pick a user)

# option for weight type options: linear, mr, .... 
opt_weight_type <- "linear"

# load packages and our functions 
library(data.table)
library(broom)
source("c:/Users/Nmath_000/Documents/Research/Heterogeneous-Teacher-Value-Added/R_code/functions/simulate_test_data.R")  #set this path
# source("~/Documents/Research/HeterogenousTeacherVA/Git/Heterogeneous-Teacher-Value-Added/R_code/functions/simulate_test_data.R")
library(Matrix)
library(ggplot2)

#set path for plots to save 
out_plot <- "c:/Users/Nmath_000/Documents/data/Value Added/"



#===================#
# ==== Figure 1: Corr(VA,VA_est) with teacher_va_epsilon ====
#===================#

# This isn't quite right because I'm not doing the MC for students. Update once we get Tanners MC code

eps_steps <- seq(0,2,by=.01)
counter <- 0
correlation <- rep(0,length(eps_steps))
for (var_i in eps_steps)
{
  counter <- counter + 1
  
  # === generate simulated data ===
  r_dt <- simulate_test_data(n_teacher               = 160,
                           n_stud_per_teacher      = 25,
                           test_SEM                = .07,
                           teacher_va_epsilon      = var_i,
                           teacher_ability_drop_off = .25)


  # ==== run VA ====
  # convert teacher_id to a factor
  r_dt[, teacher_id := as.factor(teacher_id)]
  
  # run regression 
  va_out1 <- lm(test_2 ~ test_1 + teacher_id - 1, data = r_dt)
  
  # clean results 
  va_tab1 <- data.table(tidy(va_out1))
  va_tab1[, teacher_id := gsub("teacher_id", "", term)]
  
  # grab estimates and teacher id 
  va_res <- va_tab1[term %like% "teacher_id", c("estimate", "teacher_id")]
  
  # calculate true teacher ability
  r_dt[ , va_true:= mean(teacher_impact), teacher_id]
  temp <- unique(r_dt[, c("teacher_id", "va_true")])
  
  # merge ability to the value added for comparison 
  va_res <- merge(temp, va_res, "teacher_id")
  
  # get correlation 
  correlation[counter] <- va_res[, cor(estimate, va_true)]
}

out <- data.frame(eps_steps,correlation)
diog_plot1 <- ggplot(data = out, aes(x= eps_steps, y = correlation)) +
       geom_point() +
       ggtitle("Correlation between VA estimates and truth") +
       ylab("Correlation Coef") + xlab("Variance of unobserved shock")
print(diog_plot1)





#===================#
# ==== Figure 2: % out of CI? ====
#===================#


# Again because we aren't doing the MC here there is going to be noise there "shouldn't" be in the graph
#.. Or maybe for this one there should be Part of what is interesting is the increase in spread...


eps_steps <- seq(0,2,by=.01)
counter <- 0
pct_above <- rep(0,length(eps_steps))
for (var_i in eps_steps)
{
  counter <- counter + 1
  
  # === generate simulated data ===
  r_dt <- simulate_test_data(n_teacher               = 160,
                             n_stud_per_teacher      = 25,
                             test_SEM                = .07,
                             teacher_va_epsilon      = var_i,
                             teacher_ability_drop_off = .25)
  
  
  # ==== run VA ====
  # convert teacher_id to a factor
  r_dt[, teacher_id := as.factor(teacher_id)]
  
  # run regression 
  va_out1 <- lm(test_2 ~ test_1 + teacher_id - 1, data = r_dt)
  
  # clean results 
  va_tab1 <- data.table(tidy(va_out1))
  va_tab1[, teacher_id := gsub("teacher_id", "", term)]
  
  # grab estimates and teacher id 
  va_res <- va_tab1[term %like% "teacher_id", c("estimate", "std.error", "teacher_id")]
  va_res[ , worst_ci :=  (min(estimate)==estimate)*(estimate + 1.96*std.error)]
  va_res[worst_ci==0, worst_ci := NA]
  va_res[ , flag := estimate>max(worst_ci, na.rm= TRUE)]
  
  pct_above[counter]<- mean(va_res[,flag])
}

out <- data.frame(eps_steps,pct_above)
diog_plot1 <- ggplot(data = out, aes(x= eps_steps, y = pct_above)) +
  geom_point() +
  ggtitle("Percent of VA measures different from worst (95% CI)") +
  ylab("Correlation Coef") + xlab("Variance of unobserved shock")
print(diog_plot1)




