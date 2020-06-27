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
# 1) Measurement error correction 
# 2) shrinkage 
# 3) centering and scaling and all that 
# 4) aggregation if teachers teach multiple grade levels from multiple models 



#=================#
# ==== set up ====
#=================#

# clear data.
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")

# check users
my_wd <- getwd()

# option for weight type options: linear, mr, .... 
opt_weight_type <- "linear"

# load packages and our functions 
library(data.table)
library(broom)
library(Matrix)
library(ggplot2)
# check user and set paths accordingly 
if(my_wd %like% "Nmath_000"){
  source("c:/Users/Nmath_000/Documents/Research/Heterogeneous-Teacher-Value-Added/R_code/simulate_test_data.R")  #set this path
  
  #set path for plots to save 
  out_plot <- "c:/Users/Nmath_000/Documents/data/Value Added/"
  
}else{
  
source("~/Documents/Research/HeterogenousTeacherVA/Git/Heterogeneous-Teacher-Value-Added/R_code/simulate_test_data.R")

  }



#===================#
# ==== sim data ====
#===================#

# generate simulated data. do a very small sample so stuff runs quickly 
r_dt <- simulate_test_data(n_schools               = 20,
                           min_stud                = 200,
                           max_stud                = 200, 
                           n_stud_per_teacher      = 30,
                           test_SEM                = .07,
                           teacher_va_epsilon      = .1,
                           teacher_ability_drop_off = .25)



#===========================#
# ==== weight functions ====
#===========================#

# function for linear weights. lowest student is weighted alpha times more than highest ]
# in_test_1 <- r_dt$test_1
linear_weight_fun <- function(alpha, in_test_1){
  
  quntile_lh <- quantile(in_test_1, probs = c(.1,.9))
  low_score <- quntile_lh["10%"]
  high_score <- quntile_lh["90%"]
  weight <-  alpha-(in_test_1-low_score)*(1/(high_score-low_score))*(alpha-1)
  weight <- weight/sum(weight) # I think it is more helpful if the weights sum to one in talking about them, though it does not affect the estimates at all.
}

# use the function to make the weights 
lin_w_alpha <- 2
r_dt[, linear_weights := linear_weight_fun(lin_w_alpha,test_1)]
lin_w_plot <- ggplot(data = r_dt, aes(x= test_1, y = linear_weights)) + geom_point()

# Mike Ricks weights 
w_i <- median(r_dt$test_1)
  max_score <-  max(r_dt$test_1)
  min_score <-  min(r_dt$test_1)
  r_dt[test_1<=w_i, mr_weights := (test_1-min_score)/(w_i-min_score) ]
  r_dt[test_1>w_i, mr_weights := 1 - (test_1-w_i)/(max_score-w_i)]
  mr_w_plot <- ggplot(data = r_dt, aes(x= test_1, y = mr_weights)) + geom_point()

  # save plots 
  ggsave(paste0(out_plot, "linear_wight.png"), plot = lin_w_plot)
  ggsave(paste0(out_plot, "mr_weight.png"), plot = mr_w_plot)
  

  
#=================#
# ==== run VA ====
#=================#

# for now I am just going to use built in commands. If we eventually want to do measurement error correction and stuff 
# then we will have to write this more from scratch and can rely on the VA code I have from EA. We will also 
# have to create a dummy matrix, rather than using factors, once students have multiple teacher assignments 

# convert teacher_id to a factor so we can treat it as a dummy 
r_dt[, teacher_id := as.factor(teacher_id)]

# run regression 
va_out1 <- lm(test_2 ~ test_1 + teacher_id - 1, data = r_dt)

# clean results 
va_tab1 <- data.table(tidy(va_out1))

va_tab1[, teacher_id := gsub("teacher_id", "", term)]

# grab out just the estimates for teachers and the the teacher id 
va_res <- va_tab1[term %like% "teacher_id", c("estimate", "teacher_id")]

# grab each teachers ability so we can compare it to the value added 
teach_dt <- unique(r_dt[, c("teacher_id", "teacher_ability")])

# merge ability to the value added for comparison 
va_res <- merge(teach_dt, va_res, "teacher_id")

# get correlation 
correlation <- va_res[, cor(estimate, teacher_ability)]
correlation


#============================================#
# ==== partial out regression no weights ====
#============================================#

# make data into dose matrix so I can do regression with matrices  
# start by grabbing stud_id and teacher_id
dose_dt <- r_dt[, c("stud_id", "teacher_id")]

# now loop through and make a new dummy column for each teacher 
for(teach_i in unique(r_dt$teacher_id)){
  
  # create category column in dataset
  dose_dt[, paste0("d_teacher_",teach_i) := 0]
  
  # add 1 in column for rows where category applies
  dose_dt[ teacher_id == teach_i, paste0("d_teacher_",teach_i) := .25]
  
}

# drop off the extra columns and make it a matrix 
dose_mat <- as.matrix(dose_dt[, -c("stud_id", "teacher_id")])

# get controls matrix 
cont_dt <- r_dt[, c("test_1")]
# cont_dt[, constant := 1]
cont_mat <- as.matrix(cont_dt)

# partial out the effect of the pretest (and other controls once we get them) on the dose matrix. 
B_mat <- solve(crossprod(cont_mat, cont_mat)) %*% (crossprod(cont_mat, dose_mat))

# get residuals 
residuals_1 <- dose_mat - cont_mat%*%B_mat

# make outcome matrix 
Y_mat <-  as.matrix(r_dt[, c("test_2")])

# now do a\second step of regression 
p_out_va_coef <- solve(crossprod(residuals_1, residuals_1)) %*% (crossprod(residuals_1, Y_mat))

# reorganize them for easier comparison
p_out_va_coef_dt <- data.table(p_out_va_coef, keep.rownames = TRUE)
colnames(p_out_va_coef_dt) <- c("teacher_id", "p_out_va1")
p_out_va_coef_dt[, teacher_id := gsub("d_teacher_", "", teacher_id)]
p_out_va_coef_dt <- merge(teach_dt, p_out_va_coef_dt, "teacher_id")

# check correlation 
p_out_va_coef_dt[, cor(teacher_ability, p_out_va1)]

# check that it is identical to lm regression 
comparison_1 <- merge(va_res, p_out_va_coef_dt, "teacher_id")
all.equal(comparison_1$estimate, comparison_1$p_out_va1)



#==========================#
# ==== welfare weights ====
#==========================#
  #==============================================#
  # ==== partial out regression with weights ====
  #==============================================#


  # put the weights on the diagonal of a matrix, pick weight using option 
  if(opt_weight_type == "linear"){
    W_mat <- as.matrix(diag(r_dt$linear_weights))
  }
  if(opt_weight_type == "mr"){
    W_mat <- as.matrix(diag(r_dt$mr_weights))
  }
  if(!exists("W_mat")){
    stop("You need to pick one of the specified opt_weight_type options")
  }
 

  # make outcome matrix 
  Y_mat <-  as.matrix(r_dt[, c("test_2")])
  
  # now do a weighted regression of the residuals on the posttest 
  ww_va_coef <- solve(crossprod(residuals_1, W_mat) %*% residuals_1) %*% (crossprod(residuals_1, W_mat) %*% Y_mat)
  
  # reorganize them for easier comparison
  ww_va_coef_dt <- data.table(ww_va_coef, keep.rownames = TRUE)
  colnames(ww_va_coef_dt) <- c("teacher_id", "ww_va1")
  ww_va_coef_dt[, teacher_id := gsub("d_teacher_", "", teacher_id)]
  ww_va_coef_dt <- merge(teach_dt, ww_va_coef_dt, "teacher_id")
  
  # check correlation 
  print("welfare weighted cor:")
  ww_va_coef_dt[, cor(teacher_ability, ww_va1)]
  print("standard cor: ")
  correlation
  
  # =========================================== #
  # ==== Calculate the "Truth" and Compare ==== #
  # =========================================== #
  
  # I assume that the true ranking is given by the density of the difference between
  #   student ability and the teacher center multiplied by the teacher's ability 
  #   and the correct weight. Is this the right way to do it?
  
  # Make a column with the "true" welfare-weighted effect on scores.
  if(opt_weight_type == "linear"){
    
    # make weights based on true ex ante ability rather than test one 
    r_dt[,linear_weights_true := linear_weight_fun(alpha = lin_w_alpha, in_test_1 = stud_ability_1)]
    r_dt[, true_ww := sum(dnorm(stud_ability_1 - teacher_center)*teacher_ability*linear_weights_true), "teacher_id"]
  }
  if(opt_weight_type == "mr"){
    r_dt[, true_ww := sum(dnorm(stud_ability_1 - teacher_center)*teacher_ability*mr_weights), "teacher_id"]
    
  }
  # Keep just the teacher id and true welfare-weighted effect
  new <- r_dt[, .(teacher_id, true_ww)]
  new <- unique(new)
  
  # Sort the teachers by true_ww and label them 1-Number Teachers.
  new <- new[order(true_ww)]
  new[, tid := .I]
  
  # Combine the weighted and the unweighted estimates into one dataframe
  ww_va_coef_dt <- merge(ww_va_coef_dt, va_res[, c("teacher_ability") := NULL], "teacher_id")
  
  # Merge on the "true" welfare-weighted effect
  ww_va_coef_dt <- merge(ww_va_coef_dt, new, "teacher_id")
  
  # Renormalize everything so they have the same mean and variance
  ww_va_coef_dt[, ww_va1 := ((ww_va1 - mean(ww_va1))/sd(ww_va1) + mean(true_ww))*sd(true_ww)]
  ww_va_coef_dt[, estimate := ((estimate - mean(estimate))/sd(estimate) + mean(true_ww))*sd(true_ww)]
  
  # Make a Caterpillar plot (currently includes truth, baseline, and weighted estimates)
  test <- melt(ww_va_coef_dt, id.vars="tid", measure.vars = c("estimate", "ww_va1", "true_ww"))
  
  cat_plot_baseline <- ggplot(test, aes(x = tid, y = value, color = variable)) + geom_point()
  cat_plot_baseline
  
  # Calculate the mean squared distance from the rank of the truth
  ww_va_coef_dt <- ww_va_coef_dt[order(estimate)]
  ww_va_coef_dt[, baseline := (.I - tid)^2]
  
  ww_va_coef_dt <- ww_va_coef_dt[order(ww_va1)]
  ww_va_coef_dt[, weighted := (.I - tid)^2]
  
  # Display the mean squared distance for both
  sum(ww_va_coef_dt$baseline)
  sum(ww_va_coef_dt$weighted)
  
  # differences in correlation 
  ww_va_coef_dt[, cor(ww_va1, true_ww)]
  ww_va_coef_dt[, cor(estimate, true_ww)]

  
  
  
  
  
  
  
  
  
  
  # work in progress###########################################################################################
  #==============================#
  # ==== get standard errors ====
  #==============================#
  # this is just code I copied from online I am tring to work through to maybe 
  # help understand what I need to do. 
  # https://stats.stackexchange.com/questions/283764/standard-errors-with-weighted-least-squares-regression
  
  # get weights as a vector 
  W_vec <- diag(W_mat)
  
  
  (resid_var2 <- sqrt(sum(W_vec*(residuals^2))/(100 - 2)))
  sig_i <- resid_var2 / sqrt(W_vec)
  var_betas2 <- solve(t(X) %*% W %*% X) %*% (t(X) %*% W %*% diag(sig_i^2) %*% t(W) %*% X) %*% solve(t(X) %*% W %*% X)
  sqrt(diag(var_betas2)) # SEs
  
  ###########################################################
  ### DATA GENERATION #######################################
  ###########################################################
  set.seed(1234)
  # Generate a covariate
  x <- rnorm(100)
  # Generate the propensity score
  ps <- (1 + exp(-(-.5 + .8*x)))^-1
  # Generate the exposure (i.e., treatment) variable
  z <- rbinom(n = 100, size = 1, prob = ps)
  # Generate the outcome
  y <- 1.1*x - .6*z + rnorm(100, sd = .5)
  
   ###########################################################
  ### INVERSE PROPENSITY SCORE WEIGHTING ####################
  ###########################################################
  ### Estimate propensity scores
  glm1 <- glm(z ~ x, family = "binomial")
  ps_est <- predict(glm1, type = "response")
  ### Create inverse ps weights
  wts <- (z/ps_est) + (1-z)/(1-ps_est)
  ### Estimate average treatment effect via WLS
  lm_wls <- lm(formula = y ~ z, weights = wts)
  summary(lm_wls)
  
  ### Verify WLS
  X <- cbind(1, z)
  W <- diag(wts)

    (resid_var2 <- sqrt(sum(wts*(lm_wls$residuals^2))/(100 - 2)))
  sig_i <- resid_var2 / sqrt(wts)
  var_betas2 <- solve(t(X) %*% W %*% X) %*% (t(X) %*% W %*% diag(sig_i^2) %*% t(W) %*% X) %*% solve(t(X) %*% W %*% X)
  sqrt(diag(var_betas2)) # SEs
  


  
  #================================================#
  # ==== Save example data for mike and tanner ====
  #================================================#
  write.csv(r_dt, "C:/Users/Nmath_000/Documents/data/Value Added/Simulated_data_6_2_2020.csv", row.names = FALSE)
