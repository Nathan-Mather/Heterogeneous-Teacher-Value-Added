

#########################################
# Measurement error correction funciton #
#########################################

# impliments measurement error correction 
# Most of this is borrowed from EA code. I still need 
# to check out this book to really understand the SE process 
# https://www.amazon.com/Measurement-Error-Models-Wayne-Fuller/dp/0470095717


# # If you want to run it line by line, open up the me_correction_example script, run until the function 
# then define these terms and run line by line 
# # define terms for debug 
# in_data = me_dt
# lhs_var = "stud_ability_2"
# pretest_vars    = c("test_1" )
# control_vars = NULL
# teacher_vars = grep("d_teacher_id_", colnames(me_dt), value = TRUE)
# stud_id_var  = "stud_id"
# reliability  = .9

# inputs for function 

#' VA Regression
#' 
#' \code{ME_correction_regression} This will run a regression with measurement error correction. It returns the coefficient tables 
#' # can add other things as well if need be. No intercept is included. You'll need to create a constant column and include 
#' it in rhs_var if you want one 
#'
#'@param in_data         name of the data set containing the above columns 
#'@param lhs_var column name of the outcome variable, left hand side variable. 
#'@param pretest_vars column name of the pretest variables measured with error 
#'@param control_vars column names of independent variables, right hand side variabes 
#'@param teacher_vars column ames of teacher controls. These need to be dummies and not a factor variable 
#'
#'#'@return Returns 3 outputs 
#'\cr \bold{va_ests} is a data.table containing a va estimates for each teacher 
#'\cr
#'\cr \bold{coefs} is a data.table containing coefficients for all pretests and control variables 
#'\cr
#'\cr \bold{residuals} is a data.table containing student residuals both with and without the teaecher dummies 




# Fucniton 
me_correction <- function(in_data = NULL,
                             lhs_var = NULL,
                             pretest_vars = NULL,
                             control_vars = NULL,
                             teacher_vars = NULL,
                             stud_id_var  = "stud_id",
                             reliability  = .9){
  
  # make Y and X matrices 
  Y <- as.matrix(in_data[, lhs_var, with = FALSE])
  
  X <- as.matrix(in_data[, c(pretest_vars, control_vars, teacher_vars), with = FALSE])
  
  # make pretest set for sum2sem calculation 
  pretests_set <- in_data[, pretest_vars, with = FALSE]
  
  # make a controls/pretest only matrix for residual calculation 
  fe_pretest_controls_matrix <- as.matrix(in_data[, c(pretest_vars, control_vars), with = FALSE])
  
  # get student Id's 
  student_ids_set <- in_data[, stud_id_var, with = FALSE] 
  
  # save number of pretest 
  n_pre <- length(pretest_vars)
  
  # get some parameters we need for standard errors 
  n_students <- nrow(in_data)
  n_rhs_vars <- length(c(pretest_vars, control_vars, teacher_vars))
  
  #===========================================#
  # ==== ** calc sum2sem with reliability ====
  #===========================================#
  
  # define function for sum2sem calculation
  fe_sum2sem_function <- function(x_pre, rely) {
    
    # step 1. square de-meaned test score (x)
    step1 <- (x_pre - mean(x_pre)) ^ 2
    
    # step 2. multiply squared score by (1 - reliability (y))
    step2 <- step1 * (1 - rely)
    
    # step 3. sum values
    step3 <- sum(step2)
    
    # return sum2sem
    return (step3)
  }
  
  # compute sum2sems with set of pretests
  fe_sum2sem_list <- mapply(fe_sum2sem_function, pretests_set, reliability)
  
  # create matrix with sum2sems on the diagonals and 0's on the off-diagonals
  sum2sem_matrix <- diag(fe_sum2sem_list, n_pre, n_pre)
  
  
  
  # make x'x matrix
  sscp_matrix <- t(X)%*%X
  
  # do ME adjustment 
  # alter the sscp matrix by subtracting the sum2sem matrix from the pretest section
  sscp_matrix[pretest_vars, pretest_vars] <- sscp_matrix[pretest_vars, pretest_vars] - sum2sem_matrix
  
  
  #====================================================#
  # ==== * test if sscp matrix is positivedefinite ====
  #====================================================#
  
  # calculate eigenvalues of the sscp matrix
  sscp_eigenvalues <- eigen(sscp_matrix, symmetric = TRUE)$values
  
  # round eigenvalues to 8 decimal places for logic tests
  sscp_eigenvalues <- round(sscp_eigenvalues, 8)
  
  # test if any eigenvalues are equal to 0 and error if try 
  if(any(sscp_eigenvalues == 0)){
    stop("SSCP matrix is not invertible. Check for perfectly collinear RHS variables.")
  }
  
  # initialize object to store in output 
  is_positive_definite <- TRUE
  
  # test if any eigenvalues are less than 
  if(any(sscp_eigenvalues < 0)){
    
    # throw warning 
    warning("SSCP matrix is not positive-definite. Check measurement error correction and collinearity of pretest variables.")
    
    # overwrite object if matrix is not positive-definite
    is_positive_definite <- FALSE
    
  }
  
  
  #==========================#
  # ==== * create coeffs ====
  #==========================#
  
  # invert sscp matrix using solve
  sscpinv_matrix <- solve(sscp_matrix) 
  
  # create X'Y #mmult
  fe_leftprime_right <- t(X)%*% Y
  
  # solve for beta in the OLS equation X'X * beta = X'Y ( more efficient than beta = inv(X'X) X'Y ) #mmult
  fe_coeffs <- sscpinv_matrix %*% fe_leftprime_right
  
  # convert to data.table version of coefficients for output
  fe_coeffs_dt <- data.table::as.data.table(fe_coeffs, keep.rownames = TRUE)
  
  # rename data.table variables
  data.table::setnames(fe_coeffs_dt, c("rn", lhs_var), c("variable", "est"))
  
  #remove: used matrices
  rm(sscp_matrix, fe_leftprime_right)
  
  
  
  #========================================#
  # ==== * calculate student residuals ====
  #========================================#
  
  # calculate pred score #mmult
  fe_pred_score <- X %*% fe_coeffs
  
  # create matrix of va estimates without alphas (alpha are the estimates for the unit of va analysis [teacher, schools, etc.])
  fe_coeffs_noalpha <- as.matrix(fe_coeffs_dt[!(variable %chin% teacher_vars), "est"])
  
  # calculate pred score without alphas #mmult
  fe_pred_score_noalpha_uncentered <- fe_pretest_controls_matrix %*% fe_coeffs_noalpha
  
  # create fe_resid_model (post test score - pred score)
  fe_resid_model <- Y - fe_pred_score
  
  # create va_resids (post test score - pred score without alphas)
  fe_resid_va <- Y - fe_pred_score_noalpha_uncentered
  
  # combine resid vars in data.table
  fe_student_resids_dt <- data.table::data.table(act_score           = as.numeric(Y),
                                                 pred_score_model    = as.numeric(fe_pred_score),
                                                 resid_model         = as.numeric(fe_resid_model),
                                                 resid_va_uncentered = as.numeric(fe_resid_va))
  
  # de-mean va_resid
  fe_student_resids_dt[, resid_va := resid_va_uncentered - mean(resid_va_uncentered)]
  
  # add intercept to pred_score_noalpha, NOt sure if we need/want all this stuff 
  fe_student_resids_dt[, pred_score_noalpha := act_score - resid_va]
  
  # make it a data.table 
  va_studresids_out <- data.table::data.table(student_ids_set, fe_student_resids_dt)
  
  #remove: used matrices
  rm( fe_pred_score, fe_pretest_controls_matrix, fe_coeffs_noalpha, fe_pred_score_noalpha_uncentered, Y, 
      fe_resid_va)
  
  #=======================#
  # ==== * create SEs ====
  #=======================#
  
  # compute residual sum of squares divided by degrees of freedom (n - k)
  cssr <- sum(fe_resid_model ^ 2) / (n_students - n_rhs_vars)
  
  # create variance-covariance matrix
  fe_varcov <- sscpinv_matrix * cssr
  
  #=============================================================#
  # ==== ** adjust varcov matrix using fuller ME adjustment ====
  #=============================================================#
  
  # create fuller adjustment for measurement error corrected standard errors: divide sum2sem matrix by number of students
  sum2sem_fuller <- sum2sem_matrix / n_students
  
  # create 0 matrices to fill in rest of adjusted matrix
  rhs_rows <- mat.or.vec(n_rhs_vars - n_pre, n_pre)
  rhs_cols <- mat.or.vec(n_rhs_vars, n_rhs_vars - n_pre)
  
  # combine fuller adjustment matrix with additional 0 rows (use "c()" if only one pretest)
  if (n_pre == 1) { sum2sem_rhs_rows <- c(sum2sem_fuller, rhs_rows) }
  if (n_pre != 1) { sum2sem_rhs_rows <- rbind(sum2sem_fuller, rhs_rows) }
  
  # combine with additional 0 columns
  suu_matrix <- cbind(sum2sem_rhs_rows, rhs_cols)
  
  #remove: 0 matrices used to create suu_matrix
  rm(rhs_rows, rhs_cols, sum2sem_rhs_rows)
  
  #====================================#
  # ==== *** adjust var-cov matrix ====
  #====================================#
  
  # step 1. multiply Suu pretest correction * pretest coeffs, for each pretest #mmult
  step1 <- suu_matrix %*% fe_coeffs
  
  # step 2. transpose step 1
  step2 <- t(step1)
  
  # step 3. multiply step 1 and step 2 - every combo of pretest result from step 1 and 2, including squaring them for same pretest #mmult
  step3 <- step1 %*% step2
  
  # step 4. multiply Suu by cssr, and add step 3 
  step4 <- (suu_matrix * cssr) + step3
  
  # step 5. make adjustment based on number of students (still only changing pretest parts of matrix)
  step5 <- step4 * n_students
  
  # step 6. multiply sscpinv_matrix and step 5 #mmult
  step6 <- sscpinv_matrix %*% step5
  
  # step 7. multiply by sscpinv_matrix again #mmult
  step7 <- step6 %*% sscpinv_matrix
  
  # add step 7 to varcov matrix for fuller adjustment
  fe_varcov <- fe_varcov + step7
  
  #remove: used matrices
  rm(suu_matrix, step1, step2, step3, step4, step5, step6, step7)
  
  
  
  # extract diagonal from varcov matrix and take sqrt to find SE
  fe_se <- sqrt(diag(fe_varcov))
  
  # convert to data.table for output
  fe_varcov_dt <- data.table::as.data.table(fe_varcov, keep.rownames = TRUE)
  fe_se_dt     <- data.table::as.data.table(fe_se, keep.rownames = TRUE)
  
  # rename columns
  data.table::setnames(fe_varcov_dt, "rn", "variable")
  data.table::setnames(fe_se_dt, c("rn", "fe_se"), c("variable", "se"))
  
  #remove: used matrices
  rm(fe_resid_model, sscpinv_matrix, fe_coeffs, fe_varcov, fe_se)
  
  #================================#
  # ==== * format coefficients ====
  #================================#
  
  # merge est and se by variable
  fe_coeffs_dt <- merge(fe_coeffs_dt, fe_se_dt, by = "variable")
  
  # subset to only pretest and control coeffs (drop dose coeffs)
  va_coeffs_out <- fe_coeffs_dt[!(variable %chin% teacher_vars)]
  
  
  #============================#
  # ==== ** format va_ests ====
  #============================#
  
  # create set of ests and ses
  va_ests_out	<- fe_coeffs_dt[variable %chin% teacher_vars ]
  
  # rename dummy id column
  data.table::setnames(va_ests_out, "variable", "teacher_id")
  
  # strip out teacher ids 
  va_ests_out[, teacher_id := gsub("d_teacher_id_", "", teacher_id)]
  
  # order ests by dose id
  data.table::setkeyv(va_ests_out, "teacher_id")
  
  
  #===================================#
  # ==== ** format student_resids ====
  #===================================#
  
  # add student id to student resids
  va_studresids_out <- data.table::data.table(in_data[, stud_id_var, with = FALSE], fe_student_resids_dt)
  
  
  ########################
  # put it all in a list #
  ########################
  
  results_list <- list(va_ests = va_ests_out,
                       coefs   = va_coeffs_out,
                       residuals = va_studresids_out)
  
  return(results_list)
  
  
} # close ME fucniton 


