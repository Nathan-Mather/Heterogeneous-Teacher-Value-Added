
#=====================================#
# ==== get True WW teacher impact ====
#=====================================#

# this is meant to be done with data from the simulate_test_data funciton and so it 
# assumes the column names match that output 

#================================#
# ==== Roxygen documentation ====
#================================#
# dont need this now, but eventually it would be in the help file 
# I like to do it right away in shared projects just to keep track of what functions do 
#' AS_IW
#' 
#' Run our weighted value added function 
#'
#'@param in_dt
#'@param 
#'@details 
#'@examples 
grid_size <- 10000
true_ww_fun <- function(in_dt                    = NULL,
                    teacher_ability_drop_off = NULL,
                    grid_size                = NULL,
                    weight_type              = NULL,
                    lin_alpha                = NULL,
                    pctile                   = NULL,
                    v_alpha                  = NULL,
                    mrpctile                 = NULL, 
                    mrdist                   = NULL){
  
  # grab unique teacher info 
  teacher_dt <- unique( in_dt[, c( "teacher_id", "teacher_ability", "teacher_center")])
  
  # get a grid of representative students 
  stud_grid <- data.table( stud_ability = rnorm(grid_size))
  stud_grid[, ww_weight := ww_general_fun(weight_type  = weight_type,
                                          in_test_1    = stud_ability,
                                          lin_alpha    = lin_alpha,
                                          pctile       = pctile,
                                          v_alpha      = v_alpha,
                                          mrpctile     = mrpctile, 
                                          mrdist       = mrdist)]
  
  
  # write a functin to get weighted average teacher impact over simulated ability 
  ave_teacher_imapct <- function(teacher_ability, teacher_center){
    
    ww_teacher_impact_v <- stud_grid[, teacher_ability - pmin(abs(stud_ability - teacher_center), 2) * teacher_ability_drop_off * ww_weight]
    ww_teacher_impact_mean <-mean(ww_teacher_impact_v)
    return(ww_teacher_impact_mean)
  }

  teacher_dt[, true_ww_impact := ave_teacher_imapct(teacher_ability, teacher_center), teacher_id]
  
  return(teacher_dt[])
}
