# =========================================================================== #
# ======================= Binned Value Added Function ======================= #
# =========================================================================== #
# - Purpose of code:
#  - Run the estimation for the binned value added.


  # ========================================================================= #
  # ========================= Roxygen documentation ========================= #
  # ========================================================================= #
  
  #'@param in_data The data table for the estimation.
  #'@param in_teacher_id Name of the teacher id column in in_data.
  #'@param in_stud_id Name of the student id column in in_data.
  #'@param in_pre_test Name of the pre test column in in_data.
  #'@param in_post_test Name of the post test column in in_data.
  #'@param num_cats Number of bins to estimate per teacher.
  #'@param reg_formula The regression formula to estimate.
  #'@details 
  #'@examples 
  
  
  # ========================================================================= #
  # ============================ debug parameters =========================== #
  # ========================================================================= #

  # in_data       = r_dt
  # in_teacher_id = "teacher_id"
  # in_stud_id    = "stud_id"
  # in_pre_test   = "test_1"
  # in_post_test  = "test_2"
  # num_cats      = 10
  # reg_formula   = "test_2 ~ test_1 + teacher_id + categories + teacher_id*categories - 1"
  
  
  # ========================================================================= #
  # ============================ Define Function ============================ #
  # ========================================================================= #
  
  # Start of the function.
  binned_va <- function(in_data       = NULL,
                        in_teacher_id = "teacher_id",
                        in_stud_id    = "stud_id",
                        in_pre_test   = "test_1",
                        in_post_test  = "test_2",
                        num_cats      = 5,
                        reg_formula   = paste0("test_2 ~ test_1",
                                               "+ teacher_id:categories - 1")) {
    
    # Check that in_data is a data.table.
    if(!any(class(in_data) == "data.table")){
      stop("in_data needs to be a data.table")
    }
    
    # Identify the cutpoints based on how many we specified and make the factor.
    pctile_cutpoints <- seq(0, 1, length.out=num_cats+1)
    cutpoints <- quantile(in_data[[in_pre_test]], pctile_cutpoints)
    
    # Define the categorical variable for bins.
    in_data[, categories := as.factor(cut(in_data[[in_pre_test]], cutpoints))]
    
    # Run the regression. 
    output <- lm(reg_formula, data = in_data)
    
    # Clean results. 
    coefs <- data.table(broom::tidy(output))
    coefs[, teacher_id := gsub(paste0(in_teacher_id, "|:categories.*"), "",
                                term)]
    coefs[, category := gsub(paste0(in_teacher_id, "[0-9]+|:categories"), "",
                              term)]
    
    # Return just the estimates.
    coefs <- coefs[term %like% in_teacher_id, c("teacher_id", "category",
                                                  "estimate")]
    return(coefs)
    
  } # End function.
