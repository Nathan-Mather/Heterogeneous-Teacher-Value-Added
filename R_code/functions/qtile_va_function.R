# define function and set default column names to what we have been using 
qtilep_va <- function(in_data = NULL,
                     in_teacher_id = "teacher_id",
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
  in_data[, (in_teacher_id) := as.factor(get(in_teacher_id))]
  
  # Run quantie regressions
  my_formula <- as.formula(paste0(in_post_test, " ~ ", in_pre_test, " + ", in_teacher_id, " - 1"))
  rqfit <- rq(my_formula, data = in_data, tau = ptle)
  rqfit_coefs <- rqfit$coefficients

  # make coefs long 
  coefs_dt <- data.table(rqfit_coefs, keep.rownames = TRUE)
  setnames(coefs_dt, colnames(coefs_dt), gsub("tau= ", "", colnames(coefs_dt)))
  coefs_dt <-  melt.data.table(coefs_dt, id.vars = "rn")
  setnames(coefs_dt, c("rn", "variable", "value"), c("teacher_id", "tau", "qtile_est"))
 
  ## I (NATE) think this should come after aggregation. Not 100% sure thouogh 
  # # Standardize coefs (whne should I do this? If I don't at some point things look really wrong...)
  # std_coefs <- matrix(0,length(unique(in_data[[in_teacher_id]])),length(ptle))
  # for (x in seq(1,length(ptle),by=1))
  # {
  #   std_coefs[,x] = (coefs[,x]-mean(coefs[,x]))/sd(coefs[,x])
  # }
  # 
  
  # need var cov matrix and standard errors 
  
  #===========================#
  # ==== clean up results ====
  #===========================#
  # reorganize them for easier comparison
  #std_coefs[ ,teacher_id:= u_teachers]
  # Add row for ptile?
  
  return(coefs_dt)
  
}
