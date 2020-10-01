# Notes:
#  - I tried to make each function integrate to teacher ability, so that the
#      impact will always be mean 0 and teacher_ability will be the average
#      impact that a teacher has on their students.

teacher_impact <- function(teacher_ability,
                           stud_ability_1,
                           type     = 'MLRN',
                           func_num = 1)
  
  # Define magic numbers here so we can build them in later when needed.
  max_diff = .1 # Maximum difference in impact between best and worst matched student.
  lin_slope = 1 # For linear functions if we want teachers to have different slopes.
  exp_num = 6 # How sharply does it jump for students in exponential cases (choose even number).
  height = 1 # How high does the exponential function go.
  jump1 = .5 # Where the first kink/discontinuity is in functions.
  diff_size1 = 1 # Size of the first jump in discontinuous functions.
  jump2 = .7 # Where the second jump/discontinuity is in functions.
  diff_size2 = .5 # Size of the second jump in discontinuous functions.
  amp = 1 # Amplitude in cosine.
  period = 10 # 2pi/period is the period in cosine.
  phase = .1 # Is the right phase shift in cosine. 
  width = .1 # Width of the spike.
  
  
  # Get the student percentiles.
  studpct = trunc(rank(stud_ability_1))/len(stud_ability_1)
  
  
  # Check which teacher impact function is specified.
  if (type == 'MLR') {
    # MLR - Monotone, Linear, Rank Similar.
    # Upward sloping line
    return(teacher_ability + 
             lin_slope*max_diff*studpct - 
             lin_slope*max_diff/2)
  
  } else if (type == 'ML') {
    # ML - Monotone, Linear, Not Rank Similar.
    # Downward sloping line
    return(teacher_ability - 
             lin_slope*max_diff*studpct + 
             lin_slope*max_diff/2)
    
    
  } else if (type == 'MNoR') {
    # MNoR - Monotone, Non-linear, Rank Similar.
    if (func_num == 1) {
      # Parabolic jump
      return(teacher_ability + 
               height*max_diff*studpct^exp_num - 
               height*max_diff/(exp_num + 1))
    
    } else if (func_num == 2) {
      # Parabolic jump then plateau
      return(teacher_ability + 
               (studpct < jump1)*height*max_diff*((1/jump1)*studpct)^exp_num + 
               (studpct >= jump1)*height*max_diff - 
               jump1*height*max_diff/(exp_num + 1) - 
               (1 - jump1)*height*max_diff)
      
    } else {
      # Two discontinuous pieces
      return(teacher_ability + 
               (studpct >= jump1)*(diff_size1*max_diff) - 
               (1 - jump1)*diff_size1*max_diff)
    }
    
  
  } else if (type == 'MNo') {
    # MNo - Monotone, Non-linear, Not Rank Similar.
    if (func_num == 1) {
      # Parabolic drop
      return(teacher_ability + 
               height*max_diff*(studpct-1)^exp_num - 
               height*max_diff/(exp_num + 1))
      
    } else if (func_num == 2) {
      # Plateau then parabolic drop
      return(teacher_ability + 
               (studpct >= jump1)*height*max_diff*((1/jump1)*(studpct-1))^exp_num + 
               (studpct < jump1)*height*max_diff - 
               jump1*height*max_diff/(exp_num + 1) - 
               (1 - jump1)*height*max_diff)
      
    } else {
      # Two discontinuous pieces
      return(teacher_ability + 
               (studpct < jump1)*(diff_size1*max_diff) - 
               (1 - jump1)*diff_size1*max_diff)
    }
    
    
  } else if (type == 'No') {
    # No - Not Monotone, Non-linear, Not Rank Similar.
    if (func_num == 1) {
      # Spike in middle
      return(teacher_ability +
               (studpct >= jump1 - width)*(studpct < jump1 + width)*
               (max_diff + (max_diff/width)*abs(jump1 - studpct)) -
               width*max_diff)
      
    } else if (func_num == 2) {
      # Three discontinuous pieces
      return(teacher_ability + 
               (studpct >= jump1)*(studpct <= jump2)*diff_size1*max_diff +
               (studpct > jump2)*diff_size2*max_diff - 
               (jump2 - jump2)*diff_size1*max_diff -
               (1 - jump2)*diff_size2*max_diff)
      
    } else if (func_num == 3) {
      # V-shaped
      return(teacher_ability +
               (studpct < jump1)*(max_diff/jump1)*studpct +
               (studpct >= jump1)*((max_diff/(1-jump1))*studpct - max_diff - max_diff*jump1/(1 - jump1)) +
               jump1*max_diff/2 - 
               ((1 - jump1^2)*max_diff)/(2*(1 - jump1)) +
               (1 - jump1)*(max_diff + max_diff*jump1/(1 - jump1)))
      
    } else {
      # Cosine
      return(teacher_ability + 
               amp*max_diff*cos(period*(studpct - phase)))
      
    }
    
  } else {
    # MLRN - Monotone, Linear, Rank Similar, No Heterogeneity.
    return(teacher_ability)
  }
  