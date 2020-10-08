# Notes:
#  - I tried to make each function integrate to teacher ability, so that the
#      impact will always be mean 0 and teacher_ability will be the average
#      impact that a teacher has on their students.

teacher_impact <- function(teacher_ability  = NULL,
                           teacher_center   = NULL,
                           teacher_max      = NULL,
                           stud_ability_1   = NULL,
                           type             = 'MLRN',
                           func_num         = 1){

  # Define magic numbers here so we can build them in later when needed.
  max_diff <- .1 # Maximum difference in impact between best and worst matched student.
  lin_slope <- 1 # For linear functions if we want teachers to have different slopes.
  exp_num <- 6 # How sharply does it jump for students in exponential cases (choose even number).
  height <- 1 # How high does the exponential function go.
  jump1 <- .5 # Where the first kink/discontinuity is in functions.
  diff_size1 <- 1 # Size of the first jump in discontinuous functions.
  jump2 <- .7 # Where the second jump/discontinuity is in functions.
  diff_size2 <- .5 # Size of the second jump in discontinuous functions.
  amp <- 1 # Amplitude in cosine.
  period <- 10 # 2pi/period is the period in cosine.
  phase <- .1 # Is the right phase shift in cosine. 
  width <- .2 # Width of the spike.
  
  
  # Get the student ability mean and standard deviation.
  studmean <- mean(stud_ability_1)
  studsd <- sd(stud_ability_1)
  
  
  # Generate a normalized variable based on sd to plug into the various impacts.
  studpct <- (abs(stud_ability_1) < studmean + 2*studsd)*
             ((stud_ability_1 - (studmean - 2*studsd))/(2*(studmean + 2*studsd))) + 
             (stud_ability_1 >= studmean + 2*studsd)
  
  teachpct <- (abs(teacher_center) < 2)*
    ((teacher_center + 2)/4) + 
    (teacher_center >= 2)
  
  impact <- teacher_ability +
    (studpct >= teachpct - width)*(studpct < teachpct + width)*
    (teacher_max - (teacher_max/width)*abs(teachpct - studpct)) -
    width*teacher_max
  
  # Check which teacher impact function is specified.
  if (type == 'MLR') {
    # MLR - Monotone, Linear, Rank Similar.
    # Upward sloping line
    return(teacher_ability + 
             teacher_max*studpct - 
             teacher_max/2)
  
  } else if (type == 'ML') {
    # ML - Monotone, Linear, Not Rank Similar.
    # Downward sloping line
    return(teacher_ability -
             teacher_max*studpct +
             teacher_max/2)
    
    
  } else if (type == 'MNoR') {
    # MNoR - Monotone, Non-linear, Rank Similar.
    if (func_num == 1) {
      # Parabolic jump
      return(teacher_ability + 
               teacher_max*studpct^exp_num - 
               teacher_max/(exp_num + 1))
    
    } else if (func_num == 2) {
      # Parabolic jump then plateau
      return(teacher_ability + 
               (studpct < teachpct)*teacher_max*((1/teachpct)*studpct)^exp_num + 
               (studpct >= teachpct)*teacher_max - 
               teachpct*teacher_max/(exp_num + 1) - 
               (1 - teachpct)*teacher_max)
      
    } else {
      # Two discontinuous pieces
      return(teacher_ability + 
               (studpct >= teachpct)*(teacher_max) - 
               (1 - teachpct)*teacher_max)
    }
    
  
  } else if (type == 'MNo') {
    # MNo - Monotone, Non-linear, Not Rank Similar.
    if (func_num == 1) {
      # Parabolic drop
      return(teacher_ability + 
               teacher_max*(studpct-1)^exp_num - 
               teacher_max/(exp_num + 1))
      
    } else if (func_num == 2) {
      # Plateau then parabolic drop ## DOESNT WORK YET
      return(teacher_ability + 
               (studpct >= teachpct)*teacher_max*((1/teachpct)*(studpct-1))^exp_num + 
               (studpct < teachpct)*teacher_max - 
               teachpct*teacher_max/(exp_num + 1) - 
               (1 - teachpct)*teacher_max)
      
    } else {
      # Two discontinuous pieces ## DOESNT WORK YET
      return(teacher_ability + 
               (studpct < jump1)*(diff_size1*max_diff) - 
               (1 - jump1)*diff_size1*max_diff)
    }
    
    
  } else if (type == 'No') {
    # No - Not Monotone, Non-linear, Not Rank Similar.
    if (func_num == 1) {
      # Spike in middle
      return(teacher_ability +
               (studpct >= teachpct - width)*(studpct < teachpct + width)*
               (teacher_max - (teacher_max/width)*abs(teachpct - studpct)) -
               width*teacher_max)
      
    } else if (func_num == 2) {
      # Three discontinuous pieces ## DOESNT WORK YET
      return(teacher_ability + 
               (studpct >= jump1)*(studpct <= jump2)*diff_size1*max_diff +
               (studpct > jump2)*diff_size2*max_diff - 
               (jump2 - jump2)*diff_size1*max_diff -
               (1 - jump2)*diff_size2*max_diff)
      
    } else if (func_num == 3) {
      # V-shaped ## DOESNT WORK YET
      return(teacher_ability +
               (studpct < jump1)*(max_diff/jump1)*studpct +
               (studpct >= jump1)*((max_diff/(1-jump1))*studpct - max_diff - max_diff*jump1/(1 - jump1)) +
               jump1*max_diff/2 - 
               ((1 - jump1^2)*max_diff)/(2*(1 - jump1)) +
               (1 - jump1)*(max_diff + max_diff*jump1/(1 - jump1)))
      
    } else {
      # Cosine ## DOESNT WORK YET
      return(teacher_ability + 
               amp*max_diff*cos(period*(studpct - phase)))
      
    }
    
  } else {
    # MLRN - Monotone, Linear, Rank Similar, No Heterogeneity.
    return(teacher_ability)
  }
}
  