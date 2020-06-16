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


# clear data.
rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")

# load packages and our functions 
library(data.table)
library(broom)
source("c:/Users/Nmath_000/Documents/Research/Heterogeneous-Teacher-Value-Added/R_code/simulate_test_data.R")  #set this path 
library(Matrix)

#===================#
# ==== sim data ====
#===================#

# generate simluated data. do a very small smaple so stuff runs quickly 
r_dt <- simulate_test_data(n_schools          = 10,
                           min_stud           = 200,
                           max_stud           = 200, 
                           n_stud_per_teacher = 25,
                           test_SEM           = .07)


#=================#
# ==== run VA ====
#=================#

# for now I am just going to use built in commands. If we eventually want to do measurment error correction and stuff 
# then we will have to write this more from scratch and can rely on the VA code I have from EA. We will also 
# have to create a dummy matrix, rather than using factors, once students have multiple teacher assignments 

# convert teacher_id to a factor so we can treat it as a dummy 
r_dt[, teacher_id := as.factor(teacher_id)]

# run regression 
va_out1 <- lm(test_2 ~ test_1 + teacher_id - 1, data = r_dt)

# clean results 
va_tab1 <- data.table(tidy(va_out1))

va_tab1[, teacher_id := gsub("teacher_id", "", term)]

# grab out just the esimates for teachers and the the teacher id 
va_res <- va_tab1[term %like% "teacher_id", c("estimate", "teacher_id")]

# grab each teachers ability so we can compare it to the value added 
teach_dt <- unique(r_dt[, c("teacher_id", "teacher_ability")])

# merge ability to the value added for comparison 
va_res <- merge(teach_dt, va_res, "teacher_id")

# get correlation 
correlation <- va_res[, cor(estimate, teacher_ability)]




#============================================#
# ==== partial out regression no weights ====
#============================================#

# make data into dose matrix so I can do regression with matrices  
# start by grabing stud_id and teacher_id
dose_dt <- r_dt[, c("stud_id", "teacher_id")]

# now loop through and make a new dummy column for each teacer 
for(teach_i in unique(r_dt$teacher_id)){
  
  # create category column in dataset
  dose_dt[, paste0("d_teacher_",teach_i) := 0]
  
  # add 1 in column for rows where category applies
  dose_dt[ teacher_id == teach_i, paste0("d_teacher_",teach_i) := 1]
  
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

# check correclation 
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

  # Make wieghts 
  # lots of different ways to do this. For now lets make weights where 
  #lowest student is weighted twice what the highest student is
  max_score <- r_dt[, max(test_1)]
  min_score <- r_dt[,min(test_1)]
  r_dt[, weight := 2-(test_1-min_score)*(1/(max_score-min_score))]
  summary(r_dt$weight)
  # put the weights on the diagonal of a matrix 
  W_mat <- as.matrix(diag(r_dt$weight))
  W_mat <- as.matrix(diag(rep(1, length(r_dt$weight))))
  
  # make outcome matrix 
  Y_mat <-  as.matrix(r_dt[, c("test_2")])
  
  # now do a weighted regression of the residuals on the posttest 
  ww_va_coef <- solve(crossprod(residuals_1, W_mat) %*% residuals_1) %*% (crossprod(residuals_1, W_mat) %*% Y_mat)
  
  # reorganize them for easier comparison
  ww_va_coef_dt <- data.table(ww_va_coef, keep.rownames = TRUE)
  colnames(ww_va_coef_dt) <- c("teacher_id", "ww_va1")
  ww_va_coef_dt[, teacher_id := gsub("d_teacher_", "", teacher_id)]
  ww_va_coef_dt <- merge(teach_dt, ww_va_coef_dt, "teacher_id")
  
  # check correclation 
  ww_va_coef_dt[, cor(teacher_ability, ww_va1)]
  
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
  
  #============================================#
  # ==== residualized outcome regression ====
  #============================================#
  # this is WRRRRRRRRRRRROOOOOOOOOOOOOOONGGGGGGGGGGGGGGGGG  but I am just going to keep it down here for now as a reference 
  
  # run first regression to partial out controlls 
  ww_partial <- lm(test_2 ~ test_1 , data = r_dt)
  
  # get the residuals 
  ww_resids <- data.table(stud_id = r_dt$stud_id, residual = residuals(ww_partial) )
  
  # merge on residuals 
  r_dt <- merge(r_dt, ww_resids, "stud_id")

  
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
  all.equal(r_dt$weight, r_dt$weight_3)
  
  
  
  #================================================#
  # ==== Save example data for mike and tanner ====
  #================================================#
  write.csv(r_dt, "C:/Users/Nmath_000/Documents/data/Value Added/Simulated_data_6_2_2020.csv", row.names = FALSE)
