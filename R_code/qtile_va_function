# define function and set default column names to what we have been using 
semip_va <- function(in_data = NULL,
                     in_teacher_id = "teacher_id",
                     in_stud_id    = "stud_id",
                     in_pre_test   = "test_1",
                     in_post_test  = "test_2",
                     ptle = seq(.01,.99,by=.02)){
  
  #=======================#
  # ==== error checks ====
  #=======================#
  
  
  # check that in_data is a data.table 
  if(!any(class(in_data) == "data.table")){
    stop("in_data needs to be a data.table")
  }
  
  
  #=========================#
  # ==== do value added ====
  #=========================#
  
  # convert teacher_id to a factor so we can treat it as a dummy 
  r_dt[, teacher_id := as.factor(teacher_id)]
  
  # Run quantie regressions
  rqfit <- rq(test_2 ~ test_1 + teacher_id -1, data = r_dt, tau = ptle)
  
  # clean results for teachers
  coefs <- rqfit[["coefficients"]]
  coefs <- coefs[1:length(unique(in_data[[in_teacher_id]]))+1,]
  
  # Standardize coefs (whne should I do this? If I don't at some point things look really wrong...)
  std_coefs <- matrix(0,length(unique(in_data[[in_teacher_id]])),length(ptle))
  for (x in seq(1,length(ptle),by=1))
  {
    std_coefs[,x] = (coefs[,x]-mean(coefs[,x]))/sd(coefs[,x])
  }
  

  
  #===========================#
  # ==== clean up results ====
  #===========================#
  # reorganize them for easier comparison
  #std_coefs[ ,teacher_id:= u_teachers]
  # Add row for ptile?
  
  return(std_coefs)
  
}
