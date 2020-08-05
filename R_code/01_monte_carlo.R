# ==================================== #
# == Run the Monte Carlo Simulation == #
# ==================================== #

# clear data.
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
# no scientific notation 
options(scipen = 999)
# clean console history 
cat("\f")

# ================ #
# ==== set up ==== #
# ================ #


# check users. (NOTE TO MIKE, add something unique to your base working directory to detect when it is your computer)
my_wd <- getwd()
if(my_wd %like% "Nmath_000"){
  # base directory 
  base_path <- "c:/Users/Nmath_000/Documents/Research/"
  
  # path for plots to save
  out_plot <- "c:/Users/Nmath_000/Documents/data/Value Added/"
  
}else{
  # base directory 
  base_path <- "~/Documents/Research/HeterogenousTeacherVA/Git/"
  
  # path for plots to save 
  out_plot <- "~/Documents/Research/HeterogenousTeacherVA/Git/Heterogeneous-Teacher-Value-Added/R_code"
  
}

# load packages and our functions 
func_path <- "Heterogeneous-Teacher-Value-Added/R_code/functions/"
source(paste0(base_path, func_path, "simulate_test_data.R"))
source(paste0(base_path, func_path, "ww_va_function.R"))
source(paste0(base_path, func_path, "weighting_functions.R"))

library(data.table)
library(broom)
library(Matrix)
library(ggplot2)
library(doParallel)
library(matrixStats)
library(doRNG)


# Set parallel options
if(my_wd %like% "Nmath_000"){
  myCluster <- makeCluster(4, # number of cores to use
                           type = "PSOCK") # type of cluster (Must be "PSOCK" on Windows)
}else{
myCluster <- makeCluster(20, # number of cores to use
                         type = "FORK") # type of cluster (Must be "PSOCK" on Windows)
}
registerDoParallel(myCluster)
registerDoRNG()

#=======================#
# ==== script parms ====
#=======================#
set.seed(42)

nsims = 3
opt_weight_type <-"rawlsian" # "linear" or "rawlsian" or "equal" or "v" or "mr"
teacher_ability_drop_off = 0.15
lin_alpha = 2 # For linear weights
pctile = .4 # For rawlsian weights
v_alpha = 1 # For v weights
mrpctile = .3 # For mr weights
mrdist = .2 # for mr weights
num_students = 100
teacher_va_epsilon = .05


# ================== #
# ==== sim data ==== #
# ================== #

# generate simulated data. do a very small sample so stuff runs quickly 
r_dt <- simulate_test_data(n_teacher               = 140,
                           n_stud_per_teacher      = num_students,
                           test_SEM                = .07,
                           teacher_va_epsilon      = teacher_va_epsilon,
                           teacher_ability_drop_off = teacher_ability_drop_off)

# convert teacher_id to a factor so we can treat it as a dummy 
r_dt[, teacher_id := as.factor(teacher_id)]




# ============================= #
# ==== run the Monte Carlo ==== #
# ============================= #

# Define a function to combine the results
comb <- function(...){
  # Combine the lists
  args <- do.call(cbind, list(...))
  
  # Get all estimates from the standard VA
  args1 <- args[, c(grepl("est", names(args))), with=FALSE]
  
  # Make a data table with means and standard deviations for the standard case
  out <- data.table(mean_standard = rowMeans(args1))
  out[, sd_standard := round(rowSds(matrix(unlist(args1), nr=lengths(out)[1])), digits=10)]
  
  # Get all estimates from the weighted case
  args2 <- args[, c(grepl("ww", names(args))), with=FALSE]
  
  # Add to the data table
  out[, mean_weighted := rowMeans(args2)]
  out[, sd_weighted := round(rowSds(matrix(unlist(args2), nr=lengths(out)[1])), digits=10)]
  
  # Get the teacher id
  out[, teacher_id := args[, c("teacher_id")]]
  
  return(out)
}


# Run the simulation
out <- foreach(i = 1:nsims, .combine = 'comb', .multicombine = TRUE) %dopar% { # Change %dopar% to %do% if you don't want to run it parallel

  # I need this for it to work on windows clusters since libraries arent recognized on every cluseter
  require(data.table)
  # Resample the student data
  r_dt <- simulate_test_data(teacher_dt = r_dt[, c( "teacher_id", "teacher_ability", "teacher_center")])

  # First run the standard VA 
  # run regression 
  va_out1 <- lm(test_2 ~ test_1 + teacher_id - 1, data = r_dt)
  
  # clean results 
  va_tab1 <- data.table(broom::tidy(va_out1))
  va_tab1[, teacher_id := gsub("teacher_id", "", term)]
  
  # Return just the estimates
  va_tab1 <- va_tab1[term %like% "teacher_id", c("teacher_id", "estimate")]
  
  
  # Now run the weighted VA
  # Generate the weights
  if(opt_weight_type == "linear"){
    
    r_dt[, c(opt_weight_type) := linear_weight_fun(alpha = lin_alpha, in_test_1 = test_1)]
    
  } else if(opt_weight_type == "rawlsian"){
    
    r_dt[, c(opt_weight_type) := rawlsian_weight_fun(pctile, test_1)]
    
  } else if(opt_weight_type == "equal"){
    
    r_dt[, c(opt_weight_type) := equal_weight_fun(test_1)]
    
  } else if(opt_weight_type == "v"){
    
    r_dt[, c(opt_weight_type) := v_weight_fun(v_alpha, test_1)]
    
  } else if(opt_weight_type == "mr"){
    
    r_dt[, c(opt_weight_type) := mr_weight_fun(mrpctile, mrdist, test_1)]
    
  }
  
  # Estimate the weighted VA
  ww_tab1 <- ww_va(in_data = r_dt, in_weights = opt_weight_type)
  
  # Merge on the standard VA
  va_tab1 <- merge(va_tab1, ww_tab1, "teacher_id")

}




# ======================================= #
# ==== Generate the Caterpillar Plot ==== #
# ======================================= #

# Make a column with the "true" welfare-weighted effect on scores.
if(opt_weight_type == "linear"){
  
  # make weights based on true ex ante ability rather than test one 
  r_dt[, weights_true := linear_weight_fun(alpha = lin_alpha, in_test_1 = stud_ability_1)]
  
} else if(opt_weight_type == "rawlsian"){
  
  # make weights based on true ex ante ability rather than test one 
  r_dt[, weights_true := rawlsian_weight_fun(pctile, stud_ability_1)]
  
} else if(opt_weight_type == "equal"){
  
  # make weights based on true ex ante ability rather than test one 
  r_dt[, weights_true := equal_weight_fun(stud_ability_1)]
  
} else if(opt_weight_type == "v"){
  
  # make weights based on true ex ante ability rather than test one 
  r_dt[, weights_true := v_weight_fun(v_alpha, stud_ability_1)]
  
} else if(opt_weight_type == "mr"){
  
  # make weights based on true ex ante ability rather than test one 
  r_dt[, weights_true := mr_weight_fun(mrpctile, mrdist, stud_ability_1)]
  
}

r_dt[, true_ww := sum(teacher_impact * weights_true), "teacher_id"]


# Keep just the teacher id and true welfare-weighted effect
new <- r_dt[, .(teacher_id, true_ww)]
new <- unique(new)

# Sort the teachers by true_ww and label them 1-Number Teachers.
new <- new[order(true_ww)]
new[, tid := .I]

# Merge on the "true" welfare-weighted effect
out <- merge(out, new, "teacher_id")

# Write the csv
write.csv(out, paste0(out_plot, '/', opt_weight_type, teacher_va_epsilon*100, 'Epsilon_MC.csv'))

# Renormalize everything so they have the same mean and variance
out[, mean_weighted := (mean_weighted - mean(mean_weighted))/sd(mean_weighted)]
out[, mean_standard := (mean_standard - mean(mean_standard))/sd(mean_standard)]
out[, true_ww := (true_ww - mean(true_ww))/sd(true_ww)]

# Make a Caterpillar plot (currently includes truth, baseline, and weighted estimates)
test <- melt(out, id.vars="tid", measure.vars = c("mean_standard", "mean_weighted", "true_ww"))

cat_plot_baseline <- ggplot(test, aes(x = tid, y = value, color = variable)) + geom_point()
cat_plot_baseline

# Calculate the mean squared distance from the rank of the truth
out <- out[order(mean_standard)]
out[, baseline := (.I - tid)^2]
out[, baseline_count := (.I - tid != 0)]
out[, baseline_count_num := abs(.I - tid)]

out <- out[order(mean_weighted)]
out[, weighted := (.I - tid)^2]
out[, weighted_count := (.I - tid != 0)]
out[, weighted_count_num := abs(.I - tid)]

# Display the mean squared distance for both
sum(out$baseline)
sum(out$weighted)

# Display the number of rank inversions for both
sum(out$baseline_count)
sum(out$weighted_count)

# Histogram of the density and distance of rank inversions
c1 <- rgb(0, 0, 255,max = 255, alpha = 80, names = "blue")
c2 <- rgb(0, 255, 0, max = 255, alpha = 80, names = "green")

h1 <- hist(out$baseline_count_num, breaks=seq(0,max(out$baseline_count_num),l=15))
h2 <- hist(out$weighted_count_num, breaks=seq(0,max(out$weighted_count_num),l=15))

plot(h1, col = c1)
plot(h2, col = c2, add = TRUE) # Blue is the baseline, green the weighted

#plot(r_dt$stud_ability_1, r_dt$weights_true)

# differences in correlation 
out[, cor(mean_standard, true_ww)]
out[, cor(mean_weighted, true_ww)]

# Let go of the processors
stopCluster(myCluster)
