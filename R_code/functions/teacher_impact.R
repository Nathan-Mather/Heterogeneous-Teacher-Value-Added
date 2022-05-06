# =========================================================================== #
# ========================= Teacher Impact Function ========================= #
# =========================================================================== #
# - Purpose of code:
#  - Calculate the impact for each teacher.


  # ========================================================================= #
  # ========================= Roxygen documentation ========================= #
  # ========================================================================= #
  
  #'@param teacher_ability Vector of teacher abilities.
  #'@param teacher_center Vector of teacher center values.
  #'@param teacher_max Vector of teacher max values. the difference in impact between their best and worst matched students.
  #'@param stud_ability_1 Vector of all student abilities.
  #'@param studmean Optional (we need either this and studsd or stud_ability_1)
  #'parameter giving the mean of student ability from our population.
  #'@param studsd Optional parameter giving the standard deviation of student
  #'ability from our population.
  #'@param other_data Another group of student ability values over which to
  #'calculate the teacher impact (e.g. an even grid of points).
  #'@param type The specified type of function, i.e. which assumptions to use in
  #'the teacher impact function.
  #'@param func_num The number of the function to use where there are multiple
  #'options with a specified type.
  #'@details 
  #'@examples 
  
  
  # ========================================================================= #
  # ============================ debug parameters =========================== #
  # ========================================================================= #
  
  # teacher_ability  = r_dt$teacher_ability
  # teacher_center   = r_dt$teacher_center
  # teacher_max      = r_dt$teacher_max
  # stud_ability_1   = r_dt$stud_ability_1
  # studmean         = NULL
  # studsd           = NULL
  # other_data       = NULL
  # type             = 'No'
  # func_num         = 1
  
  
  # ========================================================================= #
  # ============================ Define Function ============================ #
  # ========================================================================= #
  
  # Start of the function.
  teacher_impact <- function(teacher_ability  = NULL,
                             teacher_center   = NULL,
                             teacher_max      = NULL,
                             stud_ability_1   = NULL,
                             studmean         = NULL,
                             studsd           = NULL,
                             other_data       = NULL,
                             type             = 'MLRN',
                             func_num         = 1){
  
    # Define magic numbers here so we can modify them in later as needed.
    exp_num <- 6 # How sharply does it jump for students in exponential cases (choose even number).
    width <- .2 # Width of the spike.
    temp_center <- runif(1) # Location of second discontinuity with three discontinuity case CHECK LATER.
    period <- 10 # 2pi/period is the period in cosine.
    
    
    # Get the student ability mean and standard deviation.
    if (is.null(studmean)) {
      studmean <- mean(stud_ability_1)
      studsd <- sd(stud_ability_1)
    }
    
    
    # Generate a normalized variable based on sd for the various impacts.
    if (is.null(other_data)) {
      studpct <- (abs(stud_ability_1) < studmean + 2*studsd)*
                 ((stud_ability_1 - (studmean - 2*studsd))/
                 (2*(studmean + 2*studsd))) + 
                 (stud_ability_1 >= studmean + 2*studsd)
    }
    else {
      studpct <- (abs(other_data) < studmean + 2*studsd)*
        ((other_data - (studmean - 2*studsd))/(2*(studmean + 2*studsd))) + 
        (other_data >= studmean + 2*studsd)
    }
    
    teachpct <- (abs(teacher_center) <= 2)*
      ((teacher_center + 2)/4) + 
      (teacher_center >= 2)

    
    # Check which teacher impact function is specified.
    if (type == 'MLR') {
      # MLR - Monotone, Linear, Rank Similar.
      # Upward sloping line.
      return(teacher_ability + 
               teacher_max*studpct - 
               teacher_max/2)
    
    } else if (type == 'ML') {
      # ML - Monotone, Linear, Not Rank Similar.
      # Downward sloping line.
      return(teacher_ability -
               teacher_max*studpct +
               teacher_max/2)
      
      
    } else if (type == 'MNoR') {
      # MNoR - Monotone, Non-linear, Rank Similar.
      if (func_num == 1) {
        # Parabolic jump.
        return(teacher_ability + 
                 teacher_max*studpct^exp_num - 
                 teacher_max/(exp_num + 1))
      
      } else if (func_num == 2) {
        # Parabolic jump then plateau.
        return(teacher_ability + 
                 (studpct < teachpct)*teacher_max*((1/teachpct)*studpct)^exp_num + 
                 (studpct >= teachpct)*teacher_max - 
                 teachpct*teacher_max/(exp_num + 1) - 
                 (1 - teachpct)*teacher_max)
        
      } else {
        # Two discontinuous pieces.
        return(teacher_ability + 
                 (studpct >= teachpct)*(teacher_max) - 
                 (1 - teachpct)*teacher_max)
      }
      
    
    } else if (type == 'MNo') {
      # MNo - Monotone, Non-linear, Not Rank Similar.
      if (func_num == 1) {
        # Parabolic drop.
        return(teacher_ability + 
                 teacher_max*(studpct-1)^exp_num - 
                 teacher_max/(exp_num + 1))
        
      } else if (func_num == 2) {
        # Plateau then parabolic drop.
        return(teacher_ability + 
                 (studpct >= teachpct)*teacher_max*((1/(1-teachpct))*(studpct-1))^exp_num +
                 (studpct < teachpct)*teacher_max -
                 (1 - teachpct)*teacher_max/(exp_num + 1) -
                 (teachpct)*teacher_max)
        
      } else {
        # Two discontinuous pieces.
        return(teacher_ability + 
                 (studpct < teachpct)*(teacher_max) - 
                 (1 - teachpct)*teacher_max)
      }
      
      
    } else if (type == 'No') {
      # No - Not Monotone, Non-linear, Not Rank Similar.
      if (func_num == 1) {
        # Spike in middle.
        return(teacher_ability + 
                 (studpct >= teachpct - width)*(studpct < teachpct + width)*
                 (teacher_max - (teacher_max/width)*abs(teachpct - studpct)) -
                 width*teacher_max)
        
      } else if (func_num == 2) {
        # Three discontinuous pieces.
        return(teacher_ability +
                 (studpct >= pmin(temp_center, teachpct))*
                 (studpct <= pmax(temp_center, teachpct))*teacher_max +
                 (studpct > pmax(temp_center, teachpct))*(1/2)*teacher_max - 
                 (pmax(temp_center, teachpct) - pmin(temp_center, teachpct))*
                 teacher_max -
                 (1 - pmax(temp_center, teachpct))*(1/2)*teacher_max)
        
      } else if (func_num == 3) {
        # V-shaped.
        return(teacher_ability +
                 (studpct < teachpct)*(-teacher_max/teachpct)*studpct +
                 (studpct >= teachpct)*((teacher_max/(1-teachpct))*studpct -
                 teacher_max - teacher_max*teachpct/(1 - teachpct)) +
                 teachpct*teacher_max/2 - 
                 ((1 - teachpct^2)*teacher_max)/(2*(1 - teachpct)) +
                 (1 - teachpct)*(teacher_max + teacher_max*teachpct/
                 (1 - teachpct)))
        
      } else {
        # Cosine.
        return(teacher_ability + 
                 teacher_max*cos(period*(studpct - teachpct)))
        
      }
      
    }else if(type == "Bin"|type == "bin"){
      
      #  teacher center determines comparative advnatage. Max diff is differnce between high and lowe 
      # centered on their impact. 4 cases H,h, h,l, l,h , l,l
      t_comp_adv <- as.numeric((teacher_center >= 0))
      s_type <- as.numeric((studpct >= 0.5))
      
      # types matches HH, LL
      matched <- (t_comp_adv*s_type)+((1-t_comp_adv)*(1-s_type))
      
      # types not matched HL, LH
      not_matched <- (t_comp_adv*(1-s_type))+((1-t_comp_adv)*s_type)
      
      return(matched*(teacher_ability + teacher_max/2) + not_matched*(teacher_ability - teacher_max/2))
      
      
    } else {
      # MLRN - Monotone, Linear, Rank Similar, No Heterogeneity.
      return(teacher_ability)
    }
    
  } # End function.
    