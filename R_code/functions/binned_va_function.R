# define function and set default column names to what we have been using 
binned_va <- function(in_data = NULL,
                     in_teacher_id = "teacher_id",
                     in_stud_id    = "stud_id",
                     in_pre_test   = "test_1",
                     in_post_test  = "test_2",
                     num_cats      = 5,
                     reg_formula   = "test_2 ~ test_1 + teacher_id + categories + teacher_id*categories - 1"){
  
  #======================#
  #==== error checks ====#
  #======================#
  
  
  # check that in_data is a data.table 
  if(!any(class(in_data) == "data.table")){
    stop("in_data needs to be a data.table")
  }
  
  
  #====================================#
  #==== make categorical variables ====#
  #====================================#
  
  
  # identify the cutpoints based on how many we specified and make the factor
  pctile_cutpoints <- seq(0, 1, length.out=num_cats+1)
  cutpoints <- quantile(in_data[[in_pre_test]], pctile_cutpoints)
  in_data[, categories := as.factor(cut(in_data[[in_pre_test]], cutpoints))]
  
  
  #============================#
  #==== run the regression ====#
  #============================#
  
  
  # run regression 
  output <- lm(reg_formula, data = in_data)
  
  # clean results 
  output <- data.table(broom::tidy(output))
  output[, teacher_id := gsub(paste0(in_teacher_id, "|:categories.*"), "", term)]
  output[, category := gsub(paste0(in_teacher_id, "[0-9]+|:categories"), "", term)]
  
  # Return just the estimates
  output <- output[term %like% in_teacher_id, c("teacher_id", "category", "estimate")]
  return(output)
}
