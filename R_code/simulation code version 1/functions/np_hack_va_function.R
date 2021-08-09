# define function and set default column names to what we have been using 
np_hack_va <- function(in_data       = NULL,
                  in_teacher_id      = "teacher_id",
                  in_stud_id         = "stud_id",
                  in_pre_test        = "test_1",
                  in_post_test       = "test_2",
                  npoints            = 1000,
                  weighted_average   = FALSE,
                  smoothing          = 1){
  # in_data       = r_dt
  # in_teacher_id      = "teacher_id"
  # in_stud_id         = "stud_id"
  # in_pre_test        = "test_1"
  # in_post_test       = "test_2"
  # npoints            = p_npoints
  # weighted_average   = p_weighted_average
  # smoothing          = 1
  #=======================#
  # ==== error checks ====
  #=======================#
  
  
  # check that in_data is a data.table 
  if(!any(class(in_data) == "data.table")){
    stop("in_data needs to be a data.table")
  }
  
  
  # =============================== #
  # ==== calculate standard va ==== #
  # =============================== #
  if (weighted_average == TRUE) {
    standard <- summary(lm(test_2 ~ test_1 + teacher_id - 1,
                           data=in_data))$coefficients
  }
  
  
  #=========================#
  # ==== do value added ====
  #=========================#

  # Initialize values
  points = seq(-3, 3, length.out = npoints)
  u_teachers <- unique(in_data[[in_teacher_id]])
  npresults <- matrix( rep( 0, len=npoints*length(u_teachers)), nrow = length(u_teachers)) 
  counter = 0

  # Estimate relationship of test1 and test2 for each teacher
  for(teach_i in u_teachers){
    counter <- counter + 1
    # Once we have controls I think we want to include them all in this index model
    # m <- npindex(test_2~test_1+ ... , data = r_dt[in_teacher_id==teach_i])
    
    # First calculate the the least-squares cross-validated bandwidth.
    bw <- npregbw(test_2~test_1, data=in_data[get(in_teacher_id)==teach_i], exdat=points, na.action = na.omit)
    
    while (TRUE) {
      # For now just estimate the relationship between test1 and test2 nonparametrically
      m <- npreg(test_2~test_1, data=in_data[get(in_teacher_id)==teach_i], exdat=points, bws = bw$bw*smoothing,  na.action = na.omit) # Check if we want bwtype = "adaptive_nn"  , bws = bw

      # Extract the values and the standard errors.
      vals <- fitted(m) - points
      sds <- m$merr

      if (0 %in% sds) {
        bw$bw <- bw$bw*1.05 # Arbitrary, check later.
      } else {
        break
      }
      
    }
    
    #
    if (weighted_average == TRUE) {
      npresults[counter,] <- (vals/sds^2 + standard[counter + 1, 1]/standard[counter + 1, 2]^2)/(1/sds^2 + 1/standard[counter + 1, 2]^2)
    } else {
      npresults[counter,] <- vals
    }

    # desired end result 
    # Matrix of fitted values: rows are teachers columns are evaluation points.
   }
  
  #===========================#
  # ==== clean up results ====#
  #===========================#
  
  # reorganize them for easier comparison
  npresults <- data.table(npresults)
  npresults[ ,teacher_id:= u_teachers]
  
  # put it in a list with the points 
  res_list <- list(results = npresults,
                   points = points)
  
  return(res_list)
  
}
