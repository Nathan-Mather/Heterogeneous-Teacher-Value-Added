
#================================#
# ==== Roxygen documentation ====
#================================#
# dont need this now, but eventually it would be in the help file 
# I like to do it right away in shared projects just to keep track of what functions do 
#' AS_IW
#' 
#' Run our weighted value added function 
#'
#'@param in_data data.table with all the needed data 
#'@param in_teacher_id variable name of teacher id entered as character string 
#'@param in_stud_id variable name of the student id entered as character string 
#'@param in_pre_test variable name of pre-test entered as character string 
#'@param in_post_test variable name of post-test entered as character string 
#'@param in_weights variable name of policy maker's welfare weights entered as character string 
#'@param weightvar variable name of basis of the weights (e.g. ability for the infeasible weights)
#'@param alpha the alpha value for the linear weights
#'@param 
#'@details 
#'@examples 


# Sample data to debug 
#in_data       = r_dt
#in_stud_id    = "stud_id"
#in_teacher_id = "teacher_id"
#in_pre_test   = "test_1"
#in_post_test  = "test_2"
#in_weights    = "linear"

# Linear weighting function
linear_weight_fun <- function(alpha, in_test_1){
  max_score <- max(in_test_1)
  min_score <- min(in_test_1)
  weight <-  alpha-(in_test_1-min_score)*(1/(max_score-min_score))*(alpha-1)
  weight <- weight/sum(weight) # I think it is more helpful if the weights sum to one in talking about them, though it does not affect the estimates at all.
}


# define function and set default column names to what we have been using 
ww_va <- function(in_data = NULL,
                  in_teacher_id = "teacher_id",
                  in_stud_id    = "stud_id",
                  in_pre_test   = "test_1",
                  in_post_test  = "test_2",
                  in_weights    = "linear",
                  weightvar     = "student_ability_1",
                  alpha         = 2){

  #=======================#
  # ==== error checks ====
  #=======================#

  
  # check that in_data is a data.table 
  if(!any(class(in_data) == "data.table")){
    stop("in_data needs to be a data.table")
  }
  
  # check that all the columns exist 
  
  #========================#
  # ==== make matrices ====
  #========================#

  # make data into dose matrix so I can do regression with matrices  
  # start by grabbing stud_id and teacher_id
  dose_dt <- in_data[, c(in_stud_id, in_teacher_id), with = FALSE]
  
  # now loop through and make a new dummy column for each teacher 
  u_teachers <- unique(in_data[[in_teacher_id]])
  for(teach_i in u_teachers){
    
    # create category column in dataset
    dose_dt[, paste0("d_",teach_i) := 0]
    
    # add 1 in column for rows where category applies
    dose_dt[ get(in_teacher_id) == teach_i, paste0("d_",teach_i) := 1]
    
  }
  
  # drop off the extra columns and make it a matrix 
  dose_mat <- as.matrix(dose_dt[, -c(in_stud_id, in_teacher_id), with = FALSE])
  
  # get controls matrix 
  cont_dt <- in_data[, in_pre_test, with = FALSE]
  cont_mat <- as.matrix(cont_dt)
  
  # put the weights on the diagonal of a matrix, pick weight using option 
  if (in_weights == "linear") {
    in_data[, weights := linear_weight_fun(alpha, stud_ability_1)] # Come back to this, replace stud_ability_1 with weightvar
  }
  
  W_mat <- as.matrix(diag(in_data$weights))
  
  # make outcome matrix 
  Y_mat <-  as.matrix(in_data[, in_post_test, with = FALSE])
  
  #=========================#
  # ==== do value added ====
  #=========================#
  
  # partial out the effect of the pretest (and other controls once we get them) on the dose matrix. 
  B_mat <- solve(crossprod(cont_mat, cont_mat)) %*% (crossprod(cont_mat, dose_mat))
  
  # get residuals 
  residuals_1 <- dose_mat - cont_mat%*%B_mat

  # now do a weighted regression of the residuals on the posttest 
  ww_va_coef <- solve(crossprod(residuals_1, W_mat) %*% residuals_1) %*% (crossprod(residuals_1, W_mat) %*% Y_mat)
  
  #===========================#
  # ==== clean up results ====
  #===========================#
  # reorganize them for easier comparison
  ww_va_coef_dt <- data.table(ww_va_coef, keep.rownames = TRUE)
  colnames(ww_va_coef_dt) <- c(in_teacher_id, "ww_va1")
  ww_va_coef_dt[, (in_teacher_id) := gsub("^d_", "", get(in_teacher_id))]

  return(ww_va_coef_dt)
  
}
